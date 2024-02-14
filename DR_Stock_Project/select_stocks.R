
# Setup
library(icesSAG)
library(icesSD)
library(stringr)
library(writexl)
library(dplyr)
library(ggplot2)

# 1. AY 2022 Stocks ####
stks22 <- getSD(year = 2022)

## 1.1. Data-rich stocks ####
stks22.flt <- stks22 %>%
  mutate(DataCategory = as.numeric(DataCategory)) %>%
  filter(DataCategory < 2,
         SpeciesCommonName != "Norway lobster") 

## 1.2. Remove stocks without assessment keys ####
stks22.flt <- stks22.flt[!is.na(stks22.flt$AssessmentKey),]
any(is.na(stks22.flt$AssessmentKey))

## 1.3. Explore ####
length(unique(stks22.flt$SpeciesCommonName)) # n.species
stks22.flt %>% 
  group_by(SpeciesCommonName) %>%
  summarise(Freq = length(SpeciesCommonName)) %>%
  arrange(-Freq)# species

length(unique(stks22.flt$StockKeyLabel))     # n.stocks
length(unique(stks22.flt$AssessmentKey))     # n.asskeys

table(stks22.flt$ExpertGroup, stks22.flt$FisheriesGuild)
table(stks22.flt$FisheriesGuild)
table(stks22.flt$ExpertGroup)

## 1.4. Refine columns ####
stks22.flt.min <- stks22.flt %>%
  select(StockDatabaseID, StockKey, StockKeyLabel, ActiveYear, SpeciesScientificName, SpeciesCommonName,
         , ExpertGroup, DataCategory, , AdviceCategory, AdviceType, TrophicGuild,
         FisheriesGuild, SizeGuild, AssessmentKey, ModelName)
head(stks22.flt.min)

# 2. Reference Points ####
refpts <- getFishStockReferencePoints(unique(stks22.flt.min$AssessmentKey))
refpts <- do.call(bind_rows, refpts) # bind_rows better here due to different dimensions among lists
head(refpts)

## 2.1. Stocks with MSY Btrigger estimated ####
refpts.flt <- filter(refpts, !is.na(MSYBtrigger)) # refines to 69 stocks
stks       <- unique(refpts.flt$StockKeyLabel)
asskey     <- unique(refpts.flt$AssessmentKey)

## 2.2. Link to Advice ####
advice <- icesSAG::getListStocks(2022) %>% 
  filter(AssessmentKey %in% asskey) %>% 
  select(AssessmentKey, LinkToAdvice)

refpts.flt <- full_join(refpts.flt, advice, by = c("AssessmentKey"))

head(refpts.flt)
dim(refpts.flt)
  
# 3. Summary Table ####
sumtbl <- getSummaryTable(asskey)
sumtbl <- do.call(rbind.data.frame, sumtbl) # bind_rows doesnt work here due to columns not being of consistent class

head(sumtbl)

# 4. Final df ####
## 4.1. Combine summary table and reference points ####
finaldata <- full_join(sumtbl, refpts.flt, 
                       by = join_by(fishstock == StockKeyLabel), 
                       suffix = c("", ".refpts"))

## 4.2. check AY matches then remove one of the cols ####
if(all(finaldata$AssessmentYear == finaldata$AssessmentYear.refpts)){
  finaldata <- select(finaldata, -AssessmentYear.refpts)
}

head(finaldata)
## 4.3. Add with stock metadata ####
finaldata <- right_join(stks22, finaldata, 
                        by = na.omit(janitor::compare_df_cols(stks22, finaldata))$column_name) %>%
  mutate(StockID = fishstock) %>%
  select(-fishstock)

head(finaldata)

## 4.4. Check for NAs ####
any(is.na(finaldata[, c("FisheriesGuild", "TrophicGuild", "SizeGuild")]))
unique(finaldata[is.na(finaldata[, c("FisheriesGuild", "TrophicGuild", "SizeGuild")]),]$StockKeyLabel)
# Atlantic Wolffish (AK 3208)
# check https://sid.ices.dk/ViewStock.aspx?key=3208

View(finaldata[is.na(finaldata[, c("FisheriesGuild", "TrophicGuild", "SizeGuild")]),])
## 4.5. Save ####
### All data ####
write.csv(finaldata, file = paste0("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesData-", 
                                   length(unique(finaldata$StockID)), 
                                   "stks-AY", 
                                   unique(finaldata$AssessmentYear)))
### Stock metadata ####
stkinfo <- finaldata %>% 
  select(StockID, DataCategory, SpeciesCommonName, SpeciesScientificName, AssessmentYear, 
         AssessmentKey, StockDatabaseID, MSYBtrigger, ExpertGroup, EcoRegion, AdviceCategory, 
         TrophicGuild, FisheriesGuild, SizeGuild, ModelName, LinkToAdvice) %>%
  distinct()

write_xlsx(stkinfo,  paste0("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/stkinfo-", 
                           length(unique(finaldata$StockID)), 
                           "stks-AY", 
                           unique(finaldata$AssessmentYear),
                           ".xlsx"))


# 5. regns ####
t <- stkinfo %>%
  select(StockID, EcoRegion, ExpertGroup, TrophicGuild, FisheriesGuild, SizeGuild)

# List of stocks and their EcoRegions
regns <- str_split(t$EcoRegion, ", ") %>%
  setNames(t$StockID) %>%
  lapply(function(x) c(x, "NA")) %>%
  unlist() %>%
  as.data.frame() %>%
  rename(EcoRegion = ".") %>%
  tibble::rownames_to_column(var = "StockID") %>%
  filter(EcoRegion != "NA") %>%
  mutate(StockID = str_sub(StockID, end = -2),
         edit = StockID %in% t$StockID,
         StockID = if_else(edit == F, str_sub(StockID, end = -2), StockID),
         EcoRegion = str_remove(EcoRegion, " Ecoregion")) %>%
  select(-edit)

## 5.1. Long format ####
# We combine the stocks guild and each ecoregion wihtin it stock area
# to create a unique combination

regns_long <- left_join(regns, select(t, -EcoRegion), by = "StockID") %>%
  mutate(occ = 1,
         SFT = (paste0(SizeGuild, ", ", FisheriesGuild, ", ", TrophicGuild)),
         SFT_eco = paste0(SFT, "---", EcoRegion)) %>%
  mutate_all(~ replace(., . == "na", NA))

regns_long %>%
  group_by(SFT, EcoRegion) %>%
  summarise(Freq = sum(occ))%>%
  arrange(-Freq)

## 5.2. Wide format ####
regns_wide <- tidyr::pivot_wider(regns_long, names_from = EcoRegion, values_from = occ, values_fill = 0) %>%
  mutate(TotRegns = rowSums(across(where(is.numeric))))

# 6. Plots ####
# How many stocks are in each each ecoregion, split by...
## 6.1. Fisheries ####
ggplot() +
  geom_histogram(data = regns_long, aes(x = EcoRegion, fill = FisheriesGuild), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("ICES Eco Regions") +
  ggtitle("Distriubiton of fisheries guilds across ICES Ecoregions")

## 6.2. Size ####
ggplot() +
  geom_histogram(data = regns_long, aes(x = EcoRegion, fill = SizeGuild), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("ICES Eco Regions") +
  ggtitle("Distriubiton of size guilds across ICES Ecoregions")

## 6.3. Trophic ####
ggplot() +
  geom_histogram(data = regns_long, aes(x = EcoRegion, fill = TrophicGuild), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("ICES Eco Regions") +
  ggtitle("Distriubiton of trophic guilds across ICES Ecoregions")

## 6.4. SFT ####
ggplot() +
  geom_histogram(data = regns_long, aes(x = EcoRegion, fill = SFT), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("Size Guild, Fisheries Guild, Trophic Guild") +
  ggtitle("Unique combiantions of size, fisheries, trophic guilds across ecoregions")

## 6.5. SFT_eco ####
ggplot() +
  geom_histogram(data = regns_long, aes(x = SFT_eco, fill = SFT_eco), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  guides(fill = "none")

## 6.5. sumdata ####
# using the regns_long dataset doe snot give true number of total stocks
# stock names are replicated per eco region
# look at raw data
sumdata <- regns_long %>%
  select(-EcoRegion, -SFT_eco) %>%
  distinct()

ggplot() +
  geom_histogram(data = sumdata, aes(x = FisheriesGuild, fill = SizeGuild), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("Fisheries Guild")

ggplot() +
  geom_histogram(data = sumdata, aes(x = SFT, fill = SFT), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  guides(fill = "none")

# What is the medium pelagic demersal species (Golden redfish)
stks22.flt.min[stks22.flt.min$StockKeyLabel ==  filter(sumdata, FisheriesGuild == "Demersal", SizeGuild == "medium pelagic")$StockID,]



# OPTIMISE #####################################
# Out of the 69 DR stocks we want to select a subset to take forward
# The subset should represent a range of fisheries, trophic, and size guilds
# And also cover all regions

## 1. Set-up ####
stocks    <- unique(regns_long$StockID)
regions   <- unique(regns_long$EcoRegion)
SFTguilds <- unique(regns_long$SFT_eco)
groups <- str_split(SFTguilds, "---")

## 1.1. Unique guild ID column ####
fishdata <- stkinfo %>%
  mutate(SFT_eco = paste0(SizeGuild, ", ", FisheriesGuild, ", ", TrophicGuild, ", ", EcoRegion))

## 1.2. Objective function coefficients ####
# Our objective is to minimize the total number of chosen stocks
objective.fn <- rep(1, length(stocks)) 

## 1.3. Constraint matrix ####
#(coefficients of decision variables)
const.mat <- matrix(0, nrow = length(SFTguilds), ncol = length(stocks))

# if the unique guild ID column matches SFTguilds[i] then give 1, else 0
for (i in 1:length(SFTguilds)) {
  message(SFTguilds[i])
  f1 <- fishdata %>%
    mutate(cond1 = if_else(stringr::str_detect(SFT_eco, groups[[i]][1]),1,0)) %>%
    mutate(cond2 = if_else(stringr::str_detect(EcoRegion, groups[[i]][2]), cond1+1, cond1+0))
  #print(fishdata[which(f1$cond2 == 2),])
  const.mat[i, which(f1$cond2 == 2)] <- 1
  #print(const.mat[i,])
}

## 1.4. Constraint direction ####
const.dir <- rep(">=", length(SFTguilds)) 

## 1.5. Right-hand side of constraints ####
const.rhs <- rep(1, length(SFTguilds))  

# 2. ILP ####
# Each unique combination of size guild, fisheries guild, trophic guild ecoregion 
# should be chosen at least once whilst minimising number of stocks chosen
lp.solution <- lp("min", objective.fn, const.mat, const.dir, const.rhs)
lp.solution

## 2.1 Solution ####
selected_stocks <- stocks[which(lp.solution$solution == 1)]
selected_stocks_sumdata <- sumdata[sumdata$StockID %in% selected_stocks,]
selected_stocks_info <- stkinfo[stkinfo$StockID %in% selected_stocks,]
selected_stock_ecoregions <- regns_long[regns_long$StockID %in% selected_stocks,]
selected_stocks_finaldata <- finaldata[finaldata$StockKeyLabel %in% selected_stocks,]

## 2.2. Plots ####
ggplot(data = selected_stocks_sumdata) +
  geom_histogram(aes(x = FisheriesGuild, fill = SizeGuild), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  xlab("Fisheries Guilds") +
  ylab("No. Stocks") +
  ggtitle("Distribution of size and fisheries guilds across selected stocks")
  

ggplot(data = selected_stock_ecoregions) +
  geom_histogram(aes(x = EcoRegion, fill = StockID), stat = "count") +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("No. Stocks") +
  xlab("ICES Eco Regions") +
  ggtitle("Distriubiton of fisheries guilds across ICES Ecoregions")

table(selected_stock_ecoregions$EcoRegion, selected_stock_ecoregions$FisheriesGuild)
table(selected_stocks_sumdata$FisheriesGuild)

selected_stocks_info %>%
  select(StockID, SpeciesCommonName, FisheriesGuild) %>%
  arrange (FisheriesGuild, StockID)

## 2.3. Save Data ####
### SSB and stock assessment info
write.csv(selected_stocks_finaldata, file = paste0("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesData-", 
                                   length(unique(selected_stocks_finaldata$StockID)), 
                                   "stks-AY", 
                                   unique(selected_stocks_finaldata$AssessmentYear),
                                   "-opt"))

### Stock metadata reference points and metadata 
write_xlsx(selected_stocks_info,  paste0("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/stkinfo-", 
                            length(unique(selected_stocks_info$StockID)), 
                            "stks-AY", 
                            unique(selected_stocks_info$AssessmentYear),
                            "-opt",
                            ".xlsx"))

