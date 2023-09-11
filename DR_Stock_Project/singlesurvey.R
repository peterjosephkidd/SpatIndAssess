#> Script to calculate and plot spatial indicators for surveys 1 by 1
#> This can be used to check individual surveys 

### A. Load in requirements ####
# load packages
pckgs <- c("FLCore", "FLBRP", "dplyr", "ggplot2", "ggplotFL", "rgdal", 
           "DataExplorer", "rgeos", "sf", "mapplots", "maptools", "mapproj", 
           "beepr", "patchwork", "ineq", "icesDatras", "icesVocab", "scales", 
           "readxl", "data.table", "zoo", "cowplot")
for(pkg in pckgs){
  library(pkg, character.only = T, quietly = T)
}
rm(pckgs, pkg)

# Source user functions
source(paste0(getwd(), "/Functions/dataprep_funs.R")) # for dealing with datras data
source(paste0(getwd(), "/Functions/spatinds_funs.R")) # for computing spatial indicators
source(paste0(getwd(), "/Functions/ROC_funs.R"))      # for ROC and TSS

# Load ICES rectangles and divisions
load(paste0(getwd(), "/Data/ICES Rect/ices_rect.rds"))
load(paste0(getwd(), "/Data/ICES Divs/ices_divs.rds"))

# Load stock metainformation
allstk_metadata <- read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice Sheets/2022/stock_metadata_refpts.xlsx"), sheet = "stk_metadata")
allstk_refpts <- read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice Sheets/2022/stock_metadata_refpts.xlsx"), sheet = "stk_refpts")

# Load Stock Objects
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock Objects/2022/")
for(stockfile in list.files(stockobj.path)){load(paste0(stockobj.path, stockfile))}
stocklist <- list(cod.27.47d20_nov, had.27.46a20, ple.27.420, ple.27.7d, 
                  pok.27.3a46, sol.27.4, tur.27.4, whg.27.47d, wit.27.3a47d) # ignore so.27.7d for now, need to find YFS survey data
stocklist.chr <- list("cod.27.47d20_nov", "had.27.46a20", "ple.27.420", "ple.27.7d", 
                      "pok.27.3a46", "sol.27.4", "tur.27.4", "whg.27.47d", "wit.27.3a47d") # ignore so.27.7d for now, need to find YFS survey data

### B. Choose survey ####
stk <- had.27.46a20
stk.chr <- "had.27.46a20"
SurveyName <- "NS-IBTS" # name of the survey that feeds into the stock assessment
IndexFolder <- "Q1" # name of survey index in advice sheet
SurveyFolder <- "NS-IBTS" # name fo survey in advice sheet

### B. Calculate Spatial Indicators ####

message(paste0("\n",
         paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = ""),"\n# ", 
         stk.chr, " #", "\n",
         paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = "")))
  
##### 1.1 Species Name ####
# Get the stock species name
species <- allstk_refpts %>% filter(
  stk_name == stk.chr) %>%
  select(spcs_name, latin_name)
head(species)
# Get Valid Aphia ID using Latin name
species_aphia <- findAphia(species$latin_name, latin = TRUE)

##### 1.2 Stock Divisions ####
# Get the ICES divisions for the stock of interest
stk_divs2 <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(stk_divs) %>%
  unique()
stk_divs <- strsplit(as.character(stk_divs2), split = ", ")[[1]]
rm(stk_divs2)
# Print selection
writeLines(paste0("ICES Divisions for ", species$spcs_name, " (", stk.chr, ") : ", paste0(stk_divs, collapse = ", ")))
# check stk_divs are in the ices_rect and ices_divs
all(stk_divs %in% ices_rect$Area_27)
all(stk_divs %in% ices_divs$Area_27)

##### 1.3 Stock Reference Points ####
# Get the advice sheet reference points for the stock
stk_refpts <- allstk_refpts %>% filter(
  stk_name == stk.chr)
print(stk_refpts)

##### 1.4 Survey Information ####
###### 1.4.1 `stk_surveys` ####
# Get survey information for the current stock we are looking at
stk_surveys <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(-description, -date_published)
print(stk_surveys)

###### 1.4.2 `data.list` ####
### Select the surveys used within the stock assessment
## we will load in the survey data from local drive 
surveydata.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/Survey Data/", SurveyName)
survey_data <- list.files(paste0(surveydata.path), pattern = "*.data.rds*")
survey_data <- load(paste0(surveydata.path, "/", survey_data))
data.list <- get(survey_data)

# Check that surveys loaded are what we expect
summary(data.list)
unique(data.list$hlhh$Survey.x)

###### 1.4.3 `stkindx_surveys` (might not need) ####
stkindx_surveys <- allstk_metadata %>% filter(
  stk_id == stk.chr,
  survey_index == IndexFolder) 
print(stkindx_surveys)

##### 1.5 Filter Survey Data ####
###### 1.5.1 `stk_data_filtered` ####
  
# Get the survey data used in each index and filter it so that it matches
# the data used in the stock assessment e.g. by Species, Years, and Quarters. 
#IndexFolder == stkindx_surveys$survey_index
print(paste0("Survey Index: ", IndexFolder))

#SurveyName == stkindx_surveys$survey_name
print(paste0("Survey: ", SurveyName))

yr_strt <- stkindx_surveys$survey_yrs_start[stkindx_surveys$survey_name==SurveyName]
yr_end <- stkindx_surveys$survey_yrs_end[stkindx_surveys$survey_name==SurveyName] 
# assumptions made if start or end year is not given in the advice sheet
if(is.na(yr_end)){yr_end <- 2022} # assume most recent data used
if(is.na(yr_strt)){yr_strt <- 1900} # assume oldest data used
print(paste0("Years: ", yr_strt, "-", yr_end))

# get the quarters used in stock index for this survey
qrs <- stkindx_surveys$survey_qrs[stkindx_surveys$survey_name==SurveyName]
# sometimes the quarters used is not available in the advice sheets, so 
# assume all quarters were used. This isn't explicitly stated probably because some 
# surveys are only run in certain quarters so there is no need to say.
# Setting `qrs` to c(1:4) will retain whatever data the survey has. 
if(is.na(qrs)){qrs <- c(1:4)}
print(paste0("Quarters: ", paste(qrs, collapse = ", ")))

# filter the years and quarters of survey data, for each survey, within each survey index
# CHECK: do I need to do c(qrs). Will this code work if there are multiple quartes that need to be selected
hlhh <- data.list$hlhh %>%
          filter(Year %in% c(yr_strt:yr_end),
                 Quarter %in% qrs)
hl <- data.list$hl %>%
        filter(Year %in% c(yr_strt:yr_end),
               Quarter %in% qrs)
hh <- data.list$hh %>%
        filter(Year %in% c(yr_strt:yr_end),
               Quarter %in% qrs)
ca <- data.list$ca %>%
        filter(Year %in% c(yr_strt:yr_end),
               Quarter %in% qrs)

stk_data_filtered <- list(hlhh = hlhh, hl = hl, hh = hh, ca = ca)

###### 1.5.2 Check filtered data ####
stk_surveys[stk_surveys$survey_index==IndexFolder,]
range(stk_data_filtered$hlhh$Year)
unique(stk_data_filtered$hlhh$Year)
unique(data.list$hlhh$Year)
unique(stk_data_filtered$hlhh$Quarter)
  
#### 2. Calculate Spatial Indicators ####
# Output path to save data and plots
# Keeping this to my one drive, files may be too large for GitHub
si.data.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/10Stock_Outputs/"

# get filtered survey data
hlhh <- stk_data_filtered$hlhh
hh <- stk_data_filtered$hh
yrs <- unique(hlhh$Year)[unique(hlhh$Year) < 2022] # make sure max does not exceed max year in 2022 stock assessment 
qrs <- unique(hlhh$Quarter)
writeLines(noquote(paste0("Survey years:       ", min(yrs), "-", max(yrs))))
writeLines(noquote(paste0("Survey quarters:    ", paste(sort(unique(qrs)), collapse = ", "))))

##### 2.2 Positive Area (by ICES rectangle) ####
writeLines(noquote("Positive Area (Rectangle)"))
np.rects <- pa_rect( 
  hlhh = hlhh, 
  yrs = yrs, 
  qrs = qrs, 
  species_aphia = species_aphia, 
  stk_divs = stk_divs)
plot(np.rects$Year, np.rects$PosAreaR, type = "l")

##### 2.3 Positive Area (by Haul) ####
writeLines(noquote("Positive Area (Haul)"))
np.hauls <- pa_haul(
  hlhh = hlhh, 
  yrs = yrs, 
  qrs = qrs, 
  species_aphia = species_aphia, 
  stk_divs = stk_divs)
plot(np.hauls$Year, np.hauls$PosAreaH, type = "l")

##### 2.4 Lorenz curve data ####
writeLines(noquote("Lorenz Curve"))
lorenz <- lorenz_data(hlhh = hlhh, 
                      yrs = yrs, 
                      qrs = qrs, 
                      species_aphia = species_aphia, 
                      stk_divs = stk_divs)
lorenz_plot(lorenz)
###### 2.4.1 Gini index ####
writeLines(noquote("Gini Index"))
Gini.index <- Gini(lorenz)
plot(Gini.index$Year, Gini.index$`Gini Index`, type = "l")

###### 2.4.2 D95 ####
writeLines(noquote("D95"))
D95 <- d95(lorenz)
plot(D95$Year, D95$D95, type = "l")

##### 2.5 Spread of Participation Index (SPI) ####
#### Prep data
writeLines(noquote("Spread of Participation Index"))
SPI <- spi(
  hlhh = hlhh,
  yrs = 1991,
  qrs = qrs,
  species_aphia = species_aphia,
  stk_divs = stk_divs)
plot(SPI$Year, SPI$SPI, type = "l")

##### 2.6 Spreading Area ####
writeLines(noquote("Spreading Area"))
sa_data <- spreadingarea_data(
  hlhh = hlhh,
  yrs = c(yrs),
  qrs = c(qrs),
  species_aphia = species_aphia,
  stk_divs = stk_divs)
sa <- sa_data %>%
  group_by(Year) %>%
  summarise("Spreading Area" = spreadingarea_calc(TotalNo_Dur)) %>%
  mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% # add quarters
  relocate(Year, Quarter)
plot(sa$Year, sa$`Spreading Area`, type= "l")

##### 2.7 Equivalent Area ####
writeLines(noquote("Equivalent Area"))
ea <- sa_data %>%
  group_by(Year) %>%
  summarise("Equivalent Area" = equivalentarea(TotalNo_Dur)) %>%
  mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% # add quarters
  relocate(Year, Quarter)
plot(ea$Year, ea$`Equivalent Area`, type = "l")

##### 2.8 Centre of Gravity, Inertia, & 95% CI Ellipse ####
writeLines(noquote("Centre of Gravity (x and y)"))
writeLines(noquote("Inertia"))
writeLines(noquote("95% CI Ellipse"))

cog <- coginert(
  hlhh = hlhh,
  yrs = yrs,
  qrs = qrs,
  species_aphia = species_aphia,
  stk_divs = stk_divs)

plot(cog$Year, cog$cg_x, type = "l")
plot(cog$Year, cog$cg_y, type = "l")
plot(cog$Year, scale(cog$inertia), type = "l")
plot(cog$Year, cog$area_of_ellipse, type = "l")

coginertMap(hlhh[hlhh$Year==2020 & hlhh$Area_27 %in% stk_divs & hlhh$Valid_Aphia == species_aphia,])

##### 2.9 Convex Hull Area ####
writeLines(noquote("Convex Hull Area"))
cha <- chullarea(
  hlhh = hlhh,
  yrs = yrs,
  qrs = qrs,
  species_aphia = species_aphia,
  stk_divs = stk_divs)
plot(cha$Year, cha$areaoccupied, type = "l")


range(data.list$hlhh$Year)

data.list$hlhh %>%
  filter(Valid_Aphia == species_aphia,
         Area_27 %in% stk_divs,
         Year == "1993")
stk_data_filtered$hlhh %>%
  filter(Area_27 %in% stk_divs,
         Year == "1993") %>%
  summarise(unique(StatRec))


#### 3. Merge Data ####
#Merge all spatial indicator values into a single data frame
message("\nMerging and saving spatial indicator data...")
## select the columns that we need
Gini.index <- Gini.index[c("Year", "Quarter", "Gini Index")]
D95 <- D95[c("Year", "Quarter", "D95")]
np.hauls <- np.hauls[c("Year", "Quarter", "PosAreaH")]
np.rects <- np.rects[c("Year", "Quarter", "PosAreaR")]
SPI <- SPI[c("Year", "Quarter", "SPI")]
sa <- sa
ea <- ea
cog <- cog
cha <- cha[c("Year", "Quarter", "areaoccupied")]
## merge
si <- Reduce(function(x, y) full_join(x, y, by = c("Year", "Quarter")), list(Gini.index, D95, np.hauls, np.rects, SPI, sa, ea, cog, cha))
## rename some columns
si <- si %>%
  rename("Positive Area (Haul)" = PosAreaH,
         "Positive Area (Rectangle)" = PosAreaR,
         "CoG (x)" = cg_x,
         "CoG (y)" = cg_y, 
         "Inertia" = inertia,
         "Ellipse Area" = area_of_ellipse,
         "Convex Hull Area" = areaoccupied) %>%
  mutate(SurveyIndex = IndexFolder,
         Survey = SurveyName,
         StockID = stk.chr) %>%
  relocate(StockID, SurveyIndex, Survey, Year, Quarter, `Gini Index`) #, D95, `Positive Area (Haul)`, `Positive Area (Rectangle)`, SPI) these might also have to be ordered to match roc order
# Gini must be the first spat ind column for the roc and tss funs


### C. Plot ####
si$Inertia <- si$Inertia/1000000
colnames(si)[colnames(si)=="Inertia"] <- "Inertia (million)"
#  Convert wide to long 
si_long <- si %>% tidyr::pivot_longer(cols = c("Gini Index", "D95", "Positive Area (Rectangle)", "Positive Area (Haul)", "SPI", "Spreading Area", "Equivalent Area",
                                               "CoG (x)","CoG (y)", "Inertia (million)", "Ellipse Area", "Convex Hull Area"), 
                                      names_to = "Spatial Indicator",
                                      values_to = "Spatial Indicator Value")
si_long$`Spatial Indicator` <- factor(si_long$`Spatial Indicator`, levels = c("Gini Index", "D95", "Positive Area (Haul)", "Positive Area (Rectangle)", "SPI", 
                                                                              "Spreading Area", "Equivalent Area", "CoG (x)","CoG (y)", "Inertia (million)", 
                                                                              "Ellipse Area", "Convex Hull Area")) # factor & relocate
si_long$`Survey Index, Survey Name` <- paste0(si_long$SurveyIndex, ", ", si_long$Survey)
si$`Survey Index, Survey Name` <- paste0(si$SurveyIndex, ", ", si$Survey)


sdi_plot1 <- ggplot() + 
  geom_line(data = si_long, aes(x = Year, y = `Spatial Indicator Value`, colour = `Survey Index, Survey Name`), key_glyph = "rect") + 
  #geom_smooth(data = sdistk_long, aes(x = Year, y = `Spatial Indicator Value`, colour = `Survey Index, Survey Name`)) + 
  facet_wrap(vars(`Spatial Indicator`), scales = "free") +
  labs(title = paste0("Spatial Indicator Time Series (", stk.chr, ")"))+
  ylab("Indicator Value") + 
  # Theme
  theme(
    # Panels
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black"),
    # Legend
    #legend.justification = "top",
    legend.position = "right",
    legend.key = element_rect(colour="black", linewidth = 0.5), # border around glyphs
    legend.key.width = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"), # distance between each key glyph
    #legend.position = c(1.164,.69),
    #legend.background = element_rect(colour = "black"),
    # Axis
    axis.text = element_text(size = 8),
    #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
    axis.title = element_text(size = 10),
    aspect.ratio = 1,
    plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
  ) +
  guides(colour = guide_legend(byrow = TRUE)) # to enable vertical spacing between key glyphs


stkssb <- as.data.frame(ssb(stk))[c("year", "data")] %>%
  rename(Year = year, SSB = data)
#### 2.2 SSB/MSY Btrigger ####
# get ref point
msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
# calc SSB/MSY B trigger
stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt
#### 2.3 Merge data ####
sdistk_wide <- merge(si, stkssb, by = "Year") %>%
  relocate(Year, Quarter, StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, SSB, ssb.msybtrig)
# remove year where there isn't SSB data ir where there isnt SI data
#sdistk_wide <- na.omit(sdistk_wide)
sdistk_wide[is.na(sdistk_wide)] <- NA

### 3. ROC & TSS ####
#### 3.1 Set Parameters ####
state <- "ssb.msybtrig"
inds <- c("Gini Index", "D95", "Positive Area (Haul)", "Positive Area (Rectangle)", 
          "SPI", "Spreading Area", "Equivalent Area", "CoG (x)", "CoG (y)", "Inertia (million)",
          "Ellipse Area", "Convex Hull Area")

roc_long <- roc_fun3(sdistk_wide, state, inds, return = 'long')
roc_long$TSS <- as.numeric(roc_long$TSS)
roc_long$`Spatial Indicator Value` <- as.numeric(roc_long$`Spatial Indicator Value`)
roc_long2 <- roc_long[!apply(is.na(roc_long[,c("Spatial Indicator Value", "status")]), 1, all),]

#### 3. ROC Plot ####
roc_plot <- ggplot() +
  geom_path(data = roc_long, aes(x = FPR, y = TPR, colour = `Survey Index, Survey Name`), key_glyph = "rect") +
  #geom_point(data = optthresh, aes(x = FPR, y = TPR, colour = `Survey Index, Survey Name`)) +
  geom_abline(slope = c(0,1)) +
  facet_wrap(vars(`Spatial Indicator`)) +
  labs(title = paste0("ROC Curve (", stk.chr, ")"))+
  ylab("True Positive Rate") + 
  xlab("False Positive Rate") +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  # Theme
  theme(
    # Panels
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black"),
    # Legend
    legend.position = "right",
    legend.key = element_rect(colour="black", linewidth = 0.5), # border around glyphs
    legend.key.width = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"), # distance between each key glyph
    # Axis
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    axis.line.x = element_blank(),
    aspect.ratio = 1,
  ) +
  guides(colour = guide_legend(byrow = TRUE)) # to enable vertical spacing between key glyphs

#### 3. TSS Plot ####
tss_plot <- ggplot() +
  geom_line(data = roc_long, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`), key_glyph = "rect") +
  #geom_point(data = optthresh, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`)) +
  geom_abline(slope = c(0,0)) +
  facet_wrap(vars(`Spatial Indicator`), scales = "free") +
  labs(title = paste0("ROC Curve (", stk.chr, ")"))+
  ylab("True Skill Score") + 
  xlab("Index Value") +
  coord_cartesian(ylim = c(-1,1)) +
  #scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  # Theme
  theme(
    # Panels
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black"),
    # Legend
    legend.position = "right",
    legend.key = element_rect(colour="black", linewidth = 0.5), # border around glyphs
    legend.key.width = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"), # distance between each key glyph
    # Axis
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    axis.line.x = element_blank(),
    aspect.ratio = 1,
  ) +
  guides(colour = guide_legend(byrow = TRUE)) # to enable vertical spacing between key glyphs
