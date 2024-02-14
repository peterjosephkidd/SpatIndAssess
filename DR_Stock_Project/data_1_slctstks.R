
# Setup
library(icesSAG)
library(icesSD)
library(stringr)
library(writexl)
library(dplyr)
library(ggplot2)

save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/"

# 1. Stock Descriptions ####
stks22 <- getSD(year = 2022) # N=272

# Data-rich stocks 
stks22.dr <- stks22 %>%
  mutate(DataCategory = as.numeric(DataCategory)) %>%
  filter(DataCategory < 2,
         SpeciesCommonName != "Norway lobster") # N=95

# Remove stocks without assessment keys
stks22.dr <- stks22.dr[!is.na(stks22.dr$AssessmentKey),] # N=84
any(is.na(stks22.dr$AssessmentKey))

# Explore 
length(unique(stks22.dr$SpeciesCommonName)) 
stks22.dr %>% 
  group_by(SpeciesCommonName) %>%
  summarise(Freq = length(SpeciesCommonName)) %>%
  arrange(-Freq)

length(unique(stks22.dr$StockKeyLabel))     # n.stocks
length(unique(stks22.dr$AssessmentKey))     # n.asskeys

table(stks22.dr$ExpertGroup, stks22.dr$FisheriesGuild)
table(stks22.dr$FisheriesGuild)
table(stks22.dr$ExpertGroup)

# 2. Reference Points ####
refpts <- getFishStockReferencePoints(unique(stks22.dr$AssessmentKey))
refpts <- do.call(bind_rows, refpts) # bind_rows better here due to different dimensions among lists
head(refpts)

# Stocks with MSY Btrigger estimated 
refpts.msyb <- filter(refpts, !is.na(MSYBtrigger)) # N=69

# Link to Advice
advice <- icesSAG::getListStocks(2022) %>% 
  filter(AssessmentKey %in% unique(refpts.msyb$AssessmentKey)) %>% 
  select(AssessmentKey, LinkToAdvice)

refpts.msyb <- full_join(refpts.msyb, advice, by = c("AssessmentKey"))

head(refpts.msyb)

# 3. Stock Assessment Estimates ####
sumtbl <- getSummaryTable(unique(refpts.msyb$AssessmentKey))
sumtbl <- do.call(rbind.data.frame, sumtbl) # bind_rows doesn't work here due to columns not being of consistent class
sumtbl <- rename(sumtbl, StockKeyLabel = fishstock)
head(sumtbl)

# 4. Final dataframe ####
# Combine summary table and reference points ###
finaldata <- full_join(sumtbl, refpts.msyb, 
                       by = "StockKeyLabel", 
                       suffix = c("", ".refpts"))
nrow(sumtbl) == nrow(finaldata)
head(finaldata)

# check AY matches then remove one of the AY cols ###
if(all(finaldata$AssessmentYear == finaldata$AssessmentYear.refpts)){
  finaldata <- select(finaldata, -AssessmentYear.refpts)
}

# Add Stock Descriptions ###
finaldata <- right_join(stks22, finaldata, by = na.omit(janitor::compare_df_cols(stks22, finaldata))$column_name)

head(finaldata)
length(unique(finaldata$StockKeyLabel))

# Check for NAs
any(is.na(finaldata[, c("FisheriesGuild", "TrophicGuild", "SizeGuild")]))
unique(finaldata[is.na(finaldata[, c("FisheriesGuild", "TrophicGuild", "SizeGuild")]),]$StockKeyLabel) # Atlantic Wolffish (AK 3208) # check https://sid.ices.dk/ViewStock.aspx?key=3208

# Subset stock info so that we have one stock per row
stkinfo <- finaldata %>% 
  select(StockKeyLabel, DataCategory, SpeciesCommonName, SpeciesScientificName, AssessmentYear, 
         AssessmentKey, StockDatabaseID, MSYBtrigger, ExpertGroup, EcoRegion, AdviceCategory, 
         TrophicGuild, FisheriesGuild, SizeGuild, ModelName, LinkToAdvice) %>%
  distinct()

head(stkinfo)

## Save data ####
write_xlsx(finaldata, paste0(save.path, "icesData-", 
                           length(unique(finaldata$StockKeyLabel)), 
                           "stks-AY", 
                           unique(finaldata$AssessmentYear),
                           "-SA-data.xlsx"))

write_xlsx(stkinfo,  paste0(save.path, "icesData-", 
                           length(unique(finaldata$StockKeyLabel)), 
                           "stks-AY", 
                           unique(finaldata$AssessmentYear),
                           "-stkdescrptn.xlsx"))

# 5. Optimise stock selection ####
#> There are 69 stocks that we can use to run spatial indicators on
#> But to assess the full range will take a lot of time
#> Instead, we should select a range of stocks that represent the range of 
#> ecoregions, size guilds, trophic guilds, and fisheries guilds.

# Select data, one row per stock
t <- finaldata %>%
  select(StockKeyLabel, EcoRegion, ExpertGroup, TrophicGuild, FisheriesGuild, SizeGuild) %>%
  distinct() 

# List stocks and their EcoRegions in long fomrat
regns <- str_split(t$EcoRegion, ", ") %>%
  setNames(t$StockKeyLabel) %>%
  lapply(function(x) c(x, "NA")) %>%
  unlist() %>%
  as.data.frame() %>%
  rename(EcoRegion = ".") %>%
  tibble::rownames_to_column(var = "StockKeyLabel") %>%
  filter(EcoRegion != "NA") %>%
  mutate(StockKeyLabel = str_sub(StockKeyLabel, end = -2),
         edit = StockKeyLabel %in% t$StockKeyLabel,
         StockKeyLabel = if_else(edit == F, str_sub(StockKeyLabel, end = -2), StockKeyLabel),
         EcoRegion = str_remove(EcoRegion, " Ecoregion")) %>%
  select(-edit)

# Create UID of all available combination of guilds & ecoregions
regns_long <- left_join(regns, select(t, -EcoRegion), by = "StockKeyLabel") %>%
  mutate(occ = 1,
         SFT = (paste0(SizeGuild, ", ", FisheriesGuild, ", ", TrophicGuild)),
         SFT_eco = paste0(SFT, "---", EcoRegion)) %>%
  mutate_all(~ replace(., . == "na", NA))

regns_long %>%
  group_by(SFT, EcoRegion) %>%
  summarise(Freq = sum(occ))%>%
  arrange(-Freq)

## Visual Explorations ####
# Explore breakdown of stocks 

p <- ggplot() +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("ICES Eco Regions")

# Number of stocks within each ecoregion
p + geom_histogram(data = regns_long, aes(x = EcoRegion, fill = FisheriesGuild), stat = "count") 
p + geom_histogram(data = regns_long, aes(x = EcoRegion, fill = SizeGuild),      stat = "count")
p + geom_histogram(data = regns_long, aes(x = EcoRegion, fill = TrophicGuild),   stat = "count")
p + geom_histogram(data = regns_long, aes(x = EcoRegion, fill = SFT),            stat = "count")
p + geom_histogram(data = regns_long, aes(x = SFT_eco),                          stat = "count") +
  xlab("SFT-Eco UID") +
  theme(axis.text.y = element_text(size = 7))

# Look at actual number of stocks 
sumdata <- regns_long %>%
  select(-EcoRegion, -SFT_eco) %>%
  distinct()

p + geom_histogram(data = sumdata, aes(x = FisheriesGuild, fill = SizeGuild), stat = "count") +
  xlab("Fisheries Guild")
p + geom_histogram(data = sumdata, aes(x = SFT, fill = SFT), stat = "count") +
  guides(fill = "none") +
  xlab("Size, Fisheries, Trophic Guild")

# What is the medium pelagic demersal species (Golden redfish)
stks22.dr[stks22.dr$StockKeyLabel ==  filter(sumdata, FisheriesGuild == "Demersal", SizeGuild == "medium pelagic")$StockKeyLabel,][c(1:3,7:9)]

##  Integer Linear Programming (ILP) ####
# Out of the 69 DR stocks we want to select a subset to take forward
# The subset should represent a range of fisheries, trophic, and size guilds
# And also cover all regions
# This can be treated as an Integer Linear Programming (ILP) problem
# Each unique combination of size guild, fisheries guild, trophic guild ecoregion 
# should be chosen at least once whilst minimising the number of stocks chosen

# Set-up
stocks    <- unique(regns_long$StockKeyLabel)
regions   <- unique(regns_long$EcoRegion)
SFTguilds <- unique(regns_long$SFT_eco)
groups <- str_split(SFTguilds, "---")

# Unique guild ID column 
fishdata <- stkinfo %>%
  mutate(SFT_eco = paste0(SizeGuild, ", ", FisheriesGuild, ", ", TrophicGuild, ", ", EcoRegion))

# Objective function coefficients - objective is to minimize the total number of chosen stocks
objective.fn <- rep(1, length(stocks)) 

# Constraint matrix (coefficients of decision variables)
const.mat <- matrix(0, nrow = length(SFTguilds), ncol = length(stocks))

# if the unique guild ID column matches SFTguilds[i] then give 1, else 0
for (i in 1:length(SFTguilds)) {
  message(SFTguilds[i])
  f1 <- fishdata %>%
    mutate(cond1 = if_else(stringr::str_detect(SFT_eco, groups[[i]][1]),1,0)) %>%
    mutate(cond2 = if_else(stringr::str_detect(EcoRegion, groups[[i]][2]), cond1+1, cond1+0))
  const.mat[i, which(f1$cond2 == 2)] <- 1
}

# Constraint direction ###
const.dir <- rep(">=", length(SFTguilds)) 

# Right-hand side of constraints ###
const.rhs <- rep(1, length(SFTguilds))  

## Solution ####
lp.solution <- lp("min", objective.fn, const.mat, const.dir, const.rhs)
lp.solution

selected_stocks           <- stocks[which(lp.solution$solution == 1)]
selected_stocks_info      <- stkinfo[stkinfo$StockKeyLabel %in% selected_stocks,]
selected_stocks_finaldata <- finaldata[finaldata$StockKeyLabel %in% selected_stocks,]

## Save Data ####
# SSB and stock assessment info
write_xlsx(selected_stocks_finaldata, paste0(save.path, "icesData-", 
                                         length(unique(selected_stocks_finaldata$StockKeyLabel)), 
                                         "stks-AY", 
                                         unique(selected_stocks_finaldata$AssessmentYear),
                                         "-SA-data-optim.xlsx"))

# Reference points and metadata 
write_xlsx(selected_stocks_info,  paste0(save.path, "icesData-", 
                                         length(unique(selected_stocks_info$StockKeyLabel)), 
                                         "stks-AY", 
                                         unique(selected_stocks_info$AssessmentYear),
                                         "-stkdescrptn-optim.xlsx"))

## Plots ####
selected_stocks_sumdata   <- sumdata[sumdata$StockKeyLabel %in% selected_stocks,]
ggplot(data = selected_stocks_sumdata) +
  geom_histogram(aes(x = FisheriesGuild, fill = SizeGuild), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  xlab("Fisheries Guilds") +
  ylab("No. Stocks") +
  ggtitle("Distribution of size and fisheries guilds across selected stocks")
  
selected_stock_ecoregions <- regns_long[regns_long$StockKeyLabel %in% selected_stocks,]
ggplot(data = selected_stock_ecoregions) +
  geom_histogram(aes(x = EcoRegion, fill = StockKeyLabel), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("ICES Eco Regions") +
  ggtitle("Distriubiton of fisheries guilds across ICES Ecoregions")

# Tables
table(selected_stock_ecoregions$EcoRegion, selected_stock_ecoregions$FisheriesGuild)
table(selected_stocks_sumdata$FisheriesGuild)

selected_stocks_info %>%
  select(StockKeyLabel, SpeciesCommonName, FisheriesGuild) %>%
  arrange (FisheriesGuild, StockKeyLabel)

# Indicator species provided by Laurie
indstks <- c("her.27.25-2932",
"her.27.nirs",
"ane.27.8",
"ane.27.9a",
"cap.27.1-2",
"her.27.28",
"her.27.5a",
"her.27.6aN",
"hom.27.9a",
"nop.27.6a",
"pil.27.8abd",
"san.sa.1r",
"san.sa.4r",
"san.sa.5r",
"san.sa.7r",
"aru.27.6b7-1012",
"boc.27.6-8",
"her.27.irls",
"hom.27.2a4a5b6a7a-ce-k8",
"hom27.3a4bc7d",
"mac.27.nea",
"nop.27.3a4",
"pil.27.7",
"san.sa.2r",
"san.sa.3r",
"spr.27.22-32",
"spr.27.3a4",
"whb.27.1-91214",
"aru.27.123a4",
"cap.27.2a514",
"her.27.1-24a514a",
"her.27.20-24",
"her.27.3a47d",
"her.27.6aS7bc",
"pil.27.8c9a",
"san.sa.6r",
"spr.27.7de",
"aru.27.5a14",
"aru.27.5b6a",
"her.27.3031")
 
# check they are in data
all(indstks %in% stks22$StockKeyLabel)
# Not in any data
indstks[!indstks %in% stks22$StockKeyLabel]
indstks2 <- indstks[indstks %in% stks22$StockKeyLabel]

# Stocks that arent data rich
indstks2[!indstks2 %in% stks22.dr$StockKeyLabel]
indstks3 <- indstks2[indstks2 %in% stks22$StockKeyLabel]

# Stocks that do not have MSY btr
indstks3[!indstks3 %in% refpts.msyb$StockKeyLabel]
indstks4 <- indstks3[indstks3 %in% refpts.msyb$StockKeyLabel]

# Which reminign stocks are/are not in my selected stoks 
indstks4[!indstks4 %in% selected_stocks]

# are there any stocks that I have selected that are not in the inidctaor list
selected_stocks[!selected_stocks %in% indstks] 
selected_stocks[selected_stocks %in% indstks]

