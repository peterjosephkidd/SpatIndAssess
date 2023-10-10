# Single stock run through of full analysis
# 1. Prep ####
rm(list = ls())
# install FLR:
#source("http://flr-project.org/R/instFLR.R")
# load packages
pckgs <- c("FLCore", "FLBRP", "ggplotFL", "dplyr", "ggplot2", "rgdal", 
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

## 1.1 Select the stock to analyse ####
i <- 3 # ple.27.420
stk <- stocklist[[i]]
stk.chr <- stocklist.chr[[i]]
message(paste0("\n",
               paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = ""),"\n# ", 
               stk.chr, " #", "\n",
               paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = "")))

# 2. Get Data ####
## 2.1 Species Name ####
# Get the stock species name
species <- allstk_refpts %>% filter(
  stk_name == stk.chr) %>%
  select(spcs_name, latin_name)
head(species)
# Get Valid Aphia ID using Latin name
species_aphia <- findAphia(species$latin_name, latin = TRUE)

## 2.2 Stock Divisions ####
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
stk_divs %in% ices_rect$Area_27
stk_divs %in% ices_divs$Area_27

## 2.3 Stock Reference Points ####
# Get the advice sheet reference points for the stock
stk_refpts <- allstk_refpts %>% filter(
  stk_name == stk.chr)
print(stk_refpts)

## 2.4 Survey Information ####
### 2.4.1 `stk_surveys` ####
# Get survey information for the current stock we are looking at
stk_surveys <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(-description, -date_published)
print(stk_surveys)

### 2.4.2 `data.list` ####
# Select the surveys used within the stock assessment
# we will load in the survey data from local drive 
surveydata.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/Survey Data/"

data.list <- list()
for(survey in unique(stk_surveys$survey_name)){
  survey_data <- list.files(paste0(surveydata.path, survey), pattern = "*.data.rds*")
  survey_data <- try(load(paste0(surveydata.path, survey, "/", survey_data)))
  survey_data <- try(get(survey_data))
  data.list[[survey]] <- survey_data
}

# Check that surveys loaded are what we expect
summary(data.list)
row.names(summary(data.list)) %in% unique(stk_surveys$survey_name) # should be TRUE

### 2.4.3 `stk_surveys_indices` (might not need) ####
stk_surveys_indices <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(survey_index) %>%
  unique()
print(stk_surveys_indices)

## 2.5 Filter Survey Data ####
### 2.5.1 `stk_data_filtered` ####
# Get the survey data used in each index and filter it so that it matches
# the data used in the stock assessment e.g. by Species, Years, and Quarters. 
stk_data_filtered <- list()
for(indx in stk_surveys_indices$survey_index){
  # for each survey index in the stock assessment...
  print(paste0("Survey Index: ", indx))
  # get the row of data in allstk_metadata for this survey index
  stkindx_surveys <- allstk_metadata %>%
    filter(stk_id == stk.chr,
           survey_index == indx)
  print(stkindx_surveys)
  #stk_data_filtered[indx] <- indx
  
  for(survey in stkindx_surveys$survey_name){
    print(paste0("Survey: ", survey))
    # for each survey within each survey index...
    # get the start and end years of the survey data used
    yr_strt <- stkindx_surveys$survey_yrs_start[stkindx_surveys$survey_name==survey]
    yr_end <- stkindx_surveys$survey_yrs_end[stkindx_surveys$survey_name==survey] 
    # assumptions made if start or end year is not given in the stock assessment
    if(is.na(yr_end)==TRUE){yr_end <- 2022} # assume most recent data used
    if(is.na(yr_strt)==TRUE){yr_strt <- 1900} # assume oldest data used
    # get the quarters used in stock stock index for this survey
    qrs <- stkindx_surveys$survey_qrs[stkindx_surveys$survey_name==survey]
    
    # sometimes the quarters used is not available in the advice sheets, so 
    # assume all quarters were used. This isn't explicitly stated probably because some 
    # surveys are only run in certain quarters so there is no need to say.
    # Setting `qrs` to c(1:4) will retain whatever data the survey has. 
    if(is.na(qrs)){
      qrs <- c(1:4)
    }
    
    # filter the years and quarters of survey data, for each survey, within each survey index
    # CHECK: do I need to do c(qrs). Will this code work if there are multiple quartes that need to be selected
    hlhh <- try(data.list[[survey]]$hlhh %>%
                  filter(Year %in% c(yr_strt:yr_end),
                         Quarter %in% qrs)) 
    hl <- try(data.list[[survey]]$hl %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    hh <- try(data.list[[survey]]$hh %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    ca <- try(data.list[[survey]]$ca %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    
    stk_data_filtered[[indx]][[survey]] <- list(hlhh = hlhh, hl = hl, hh = hh, ca = ca)
  }
}

### 2.5.2 Check filtered data ####
for(indx in 1:length(stk_data_filtered)){
  # get survey index name from data
  message(paste0("Survey Index:             ", names(stk_data_filtered[indx])))
  # does this match with the excel doc
  print(noquote(paste0("Survey index matches? ", names(stk_data_filtered[indx]) %in% stk_surveys$survey_index)))
  for(survey in 1:length(stk_data_filtered[[indx]])){
    # get survey name within this survey index
    print(noquote(paste0("  Survey:             ", names(stk_data_filtered[[indx]][survey]))))
    # does this survey match with exceld doc
    print(noquote(paste0("  Survey matches?     ", names(stk_data_filtered[[indx]][survey]) ==
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_name"])))
    # get survey data years from the dataset
    print(noquote(paste0("  Survey years:       ", min(stk_data_filtered[[indx]][[survey]]$hlhh$Year), "-", max(stk_data_filtered[[indx]][[survey]]$hlhh$Year))))
    # does this match with the start and end years in the excel doc
    print(noquote(paste0("  Start year matches? ", 
                         min(stk_data_filtered[[indx]][[survey]]$hlhh$Year) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_yrs_start"])))
    print(noquote(paste0("  End year matches?   ", 
                         max(stk_data_filtered[[indx]][[survey]]$hlhh$Year) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_yrs_end"])))
    # get survey quarters from the dataset
    print(noquote(paste0("  Survey quarters:    ", unique(stk_data_filtered[[indx]][[survey]]$hlhh$Quarter))))
    # does this match with the survey quarters in the excel doc
    print(noquote(paste0("  Quarters match?     ", 
                         unique(stk_data_filtered[[indx]][[survey]]$hlhh$Quarter) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_qrs"])))
    print(noquote("---------------------------"))
  }
}

# Plotting params ####
## Set type ####
type <- "AllSurveys" # name added to signify filter for best surveys

## Order surveys by coverage ####
# Calculate the number of rectangles within the stock boundary
# use ICES shapefiles and stk_divs from earlier
totrec <- ices_rect %>% 
  filter(Area_27 %in% stk_divs) %>%
  summarise(N = length(unique(ICESNAME)))

# the data for each survey is stored in `stk_data_filtered`
# calculate the number of rectangles surveyed in the stock boundary for each survey
meanrects <- data.frame()
for(SurveyFolder in 1:length(stk_data_filtered)){
  for(IndexFolder in 1:length((stk_data_filtered[[SurveyFolder]]))){
    survrec <- stk_data_filtered[[SurveyFolder]][[IndexFolder]]$hlhh %>%
      filter(Area_27 %in% stk_divs) %>%
      group_by(Year) %>%
      summarise(ny = length(unique(StatRec)))
    n <- mean(survrec$ny)
    output <- c(stk.chr, 
                paste0(stk_divs, collapse = ", "), 
                min(stk_data_filtered[[SurveyFolder]][[IndexFolder]]$hlhh$Year),
                max(stk_data_filtered[[SurveyFolder]][[IndexFolder]]$hlhh$Year),
                names(stk_data_filtered[SurveyFolder]), 
                names(stk_data_filtered[[SurveyFolder]][IndexFolder]), 
                n)
    meanrects <- rbind(meanrects, output)
  }
}
colnames(meanrects) <- c("StockID", "StkDivs", "YrStrt", "YrEnd", "SurveyIndex", "SurveyName", "MeanRects")
meanrects$MeanRects <- as.numeric(meanrects$MeanRects)
meanrects$TotalRects <- totrec$N
meanrects$SurvCoverage <- round((meanrects$MeanRects/meanrects$TotalRects)*100, 2)
meanrects$`Survey Index, Survey Name` <- paste0(meanrects$SurveyIndex, ", ", meanrects$SurveyName)
survorder <- arrange(meanrects, desc(SurvCoverage))$`Survey Index, Survey Name`


## Colours for consistency ####
library(RColorBrewer)
library(paletteer)

colrs <- c("blue4", "#00695C", "#8BC34A", "gold2", "#E78100FF", "#F5191CFF", "#9C27B0") # "#FFD320", 
names(colrs) <- survorder

# 3. Calculate Spatial Indicators ####
# Output path to save data and plots
# Keeping this to my one drive, files may be too large for GitHub
## 3.1 Start Loop ####
for(indx in 1:length(stk_data_filtered)){
  for(survey in 1:length(stk_data_filtered[[indx]])){
    si.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/SpatInd/", type, "/")
    
    # get survey name within this survey index
    message("---------------------------\n", names(stk_data_filtered[indx]))
    writeLines(noquote(paste0("Survey Index:       ", names(stk_data_filtered[indx]))))
    writeLines(noquote(paste0("Survey:             ", names(stk_data_filtered[[indx]][survey]))))
    # get filtered survey data
    hlhh <- stk_data_filtered[[indx]][[survey]]$hlhh
    hh <- stk_data_filtered[[indx]][[survey]]$hh
    yrs <- unique(hlhh$Year)[unique(hlhh$Year) < 2022] # make sure max does not exceed max year in 2022 stock assessment 
    qrs <- unique(hlhh$Quarter)
    writeLines(noquote(paste0("Survey years:       ", min(yrs), "-", max(yrs))))
    writeLines(noquote(paste0("Survey quarters:    ", paste(sort(unique(qrs)), collapse = ", "))))
    
    ### Calculate spatial indicators for each survey
    message("\nCalculating spatial indicators")
    ### 3.2 Positive Area (by ICES rectangle) ####
    writeLines(noquote("Positive Area (Rectangle)"))
    np.rects <- pa_rect( 
      hlhh = hlhh, 
      yrs = yrs, 
      qrs = qrs, 
      species_aphia = species_aphia, 
      stk_divs = stk_divs)
    
    ## 3.3 Positive Area (by Haul) ####
    writeLines(noquote("Positive Area (Haul)"))
    np.hauls <- pa_haul(
      hlhh = hlhh, 
      yrs = yrs, 
      qrs = qrs, 
      species_aphia = species_aphia, 
      stk_divs = stk_divs)
    
    ## 3.4 Lorenz curve data ####
    writeLines(noquote("Lorenz Curve"))
    lorenz <- lorenz_data(hlhh = hlhh, 
                          yrs = yrs, 
                          qrs = qrs, 
                          species_aphia = species_aphia, 
                          stk_divs = stk_divs)
    
    ### 3.4.1 Gini index ####
    writeLines(noquote("Gini Index"))
    Gini.index <- Gini(lorenz)
    
    ### 3.4.2 D95 ####
    writeLines(noquote("D95"))
    D95 <- d95(lorenz)
    
    ## 3.5 Spread of Participation Index (SPI) ####
    #### Prep data
    writeLines(noquote("Spread of Participation Index"))
    #spi_data <- spi_prep(hlhh = hlhh, 
    #        yrs = yrs, 
    #        qrs = qrs, 
    #        species_aphia = species_aphia, 
    #        stk_divs = stk_divs)
    #### Calculate
    #spi <- spi_data %>% 
    #  group_by(Year) %>% # Year only, not quarter
    #  filter(Area_27 %in% stk_divs) %>%
    #  summarise(SPI = spi_calc(TotalNo_TotalDur, area = 1)) %>% # area=1 = all rects equal weight
    #  mutate(Quarter = paste(as.character(sort(unique(spi_data$Quarter))), collapse = ", ")) %>% # add quarters
    #  relocate(Year, Quarter)
    #### Take inverse of SPI, so high numbers = good
    #spi$SPI <- 1-spi$SPI 
    #### Then change NaNs to 0
    #spi$SPI[is.nan(spi$SPI)] <- 0 
    SPI <- spi(
      hlhh = hlhh,
      yrs = yrs,
      qrs = qrs,
      species_aphia = species_aphia,
      stk_divs = stk_divs
    )
    
    ## 3.6 Spreading Area ####
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
    
    ## 3.7 Equivalent Area ####
    writeLines(noquote("Equivalent Area"))
    ea <- sa_data %>%
      group_by(Year) %>%
      summarise("Equivalent Area" = equivalentarea(TotalNo_Dur)) %>%
      mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% # add quarters
      relocate(Year, Quarter)
    
    ## 3.8 Centre of Gravity, Inertia, & 95% CI Ellipse ####
    writeLines(noquote("Centre of Gravity (x and y)"))
    writeLines(noquote("Inertia"))
    writeLines(noquote("95% CI Ellipse"))
    
    cog <- coginert(
      hlhh = hlhh,
      yrs = yrs,
      qrs = qrs,
      species_aphia = species_aphia,
      stk_divs = stk_divs)
    
    ## 3.9 Convex Hull Area ####
    writeLines(noquote("Convex Hull Area"))
    cha <- chullarea(
      hlhh = hlhh,
      yrs = yrs,
      qrs = qrs,
      species_aphia = species_aphia,
      stk_divs = stk_divs)
    
    ## 3.10 Merge Data ####
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
    # `full_join` keeps rows where certain spatinds could not be calculated but others are available. `merge` removes these rows
    ## rename some columns
    si <- si %>%
      rename("Positive Area (Haul)" = PosAreaH,
             "Positive Area (Rectangle)" = PosAreaR,
             "CoG (x)" = cg_x,
             "CoG (y)" = cg_y, 
             "Inertia" = inertia,
             "Ellipse Area" = area_of_ellipse,
             "Convex Hull Area" = areaoccupied) %>%
      mutate(SurveyIndex = names(stk_data_filtered[indx]),
             Survey = names(stk_data_filtered[[indx]][survey]),
             StockID = stk.chr) %>%
      relocate(StockID, SurveyIndex, Survey, Year, Quarter, `Gini Index`) #, D95, `Positive Area (Haul)`, `Positive Area (Rectangle)`, SPI) these might also have to be ordered to match roc order
    ## 3.11 Save Data ####
    ## Create directories
    if(dir.exists(paste0(si.data.path, stk.chr)) == FALSE){
      dir.create(paste0(si.data.path, stk.chr))
    }
    if(dir.exists(paste0(si.data.path, stk.chr, "/", names(stk_data_filtered[indx]))) == FALSE){
      dir.create(paste0(si.data.path, stk.chr, "/", names(stk_data_filtered[indx])))
    }
    if(dir.exists(paste0(si.data.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey]))) == FALSE){
      dir.create(paste0(si.data.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey])))
    }
    si.data.path <- paste0(si.data.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey]))
    print(si.data.path)
    
    if(dir.exists(paste0(si.data.path, "/SpatIndData")) == FALSE){
      dir.create(paste0(si.data.path, "/SpatIndData"))
    }  
    save(si, file = paste0(si.data.path, "/SpatIndData/SpatIndData - ", stk.chr, " - ", names(stk_data_filtered[indx]), " - ", names(stk_data_filtered[[indx]][survey]), ".rda"))
  }
}

# 4. Plot Spatial Indicators ####
## Consistent colours ####
if(type == "BestSurveys"){
  b <- paste0(bestsurveys$SurveyIndex, ", ", bestsurveys$SurveyName)
  colrs <- colrs[b]
}

## 4.1 Load and Prepare Data ####
# Get SDI data and convert into long format
si.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/SpatInd/", type, "/", stk.chr, "/")
si.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/SpatInd/", type, "/", stk.chr, "/")

sdi_longlist <- list()
sdi_widelist <- list()
i <- 1

for(IndexFolder in names(stk_data_filtered)){
  message(paste0("Survey Index: ", IndexFolder))
  for(SurveyFolder in names(stk_data_filtered[[IndexFolder]])){
    print(noquote(paste0("Survey: ", SurveyFolder)))
    #Load  Data
    si_file <- list.files(paste0(si.data.path, "/", IndexFolder, "/", SurveyFolder, "/SpatIndData/"), pattern = "*SpatIndData*")
    load(paste0(si.data.path, "/", IndexFolder, "/", SurveyFolder, "/SpatIndData/", si_file))
    si$Inertia <- si$Inertia/1000000
    #colnames(si)[colnames(si)=="Inertia"] <- "Inertia (million)"
    sdi_widelist[[i]] <- si
    #  Convert wide to long 
    si_long <- si %>% tidyr::pivot_longer(cols = sort(c("Gini Index", "D95", "Positive Area (Rectangle)", "Positive Area (Haul)", "SPI", "Spreading Area", "Equivalent Area",
                                                        "CoG (x)","CoG (y)", "Inertia", "Ellipse Area", "Convex Hull Area")), 
                                          names_to = "Spatial Indicator",
                                          values_to = "Spatial Indicator Value")
    si_long$`Spatial Indicator` <- factor(si_long$`Spatial Indicator`, levels = sort(c("Gini Index", "D95", "Positive Area (Haul)", "Positive Area (Rectangle)", "SPI", 
                                                                                       "Spreading Area", "Equivalent Area", "CoG (x)","CoG (y)", "Inertia", 
                                                                                       "Ellipse Area", "Convex Hull Area"))) # factor & relocate
    sdi_longlist[[i]] <- si_long
    i <- i + 1
  }
}

## 4.2 Merge Data ####
sdistk_long <- do.call(rbind, sdi_longlist)
sdistk_wide <- do.call(rbind, sdi_widelist)
# Create new column for line plot
sdistk_long$`Survey Index, Survey Name` <- paste0(sdistk_long$SurveyIndex, ", ", sdistk_long$Survey)
sdistk_wide$`Survey Index, Survey Name` <- paste0(sdistk_wide$SurveyIndex, ", ", sdistk_wide$Survey)

### Order Indicators ####
indorder <- c("CoG (x)", "CoG (y)", "Inertia", 
              "Gini Index", "D95", "Spreading Area", 
              "Equivalent Area", "SPI", "Positive Area (Rectangle)", 
              "Positive Area (Haul)", "Convex Hull Area", "Ellipse Area")

## 4.3 Get SSB for survey years ####
strtyr <- min(sdistk_wide$Year)
if(strtyr < range(stk)["minyear"]){
  warning("First year of survey data provided preceeds first year of data in the stock object. Using minyear of the stock object instead.", immediate. = TRUE)
  strtyr <- range(stk)["minyear"]}

endyr <- max(sdistk_wide$Year)
if(endyr > range(stk)["maxyear"]){
  warning("Last year of survey data provided exceeds available last year of data in the stock object. Using maxyear of the stock object instead.", immediate. = TRUE)
  endyr <- range(stk)["maxyear"]}  

stkssb <- as.data.frame(ssb(stk)[,ac(strtyr:endyr)])[c("year", "data")] %>%
  rename(Year = year, SSB = data) %>%
  mutate(type = "SSB") # for facet plot

## 4.4 Plot SSB ####
ssb_plot <- ggplot() + geom_line(data = stkssb, aes(x = Year, y = SSB/1000000), colour = "black") +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black", fill = "grey20"),
        strip.text = element_text(colour = "white", face = "bold"), 
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("SSB (million tons)") +
  facet_wrap(vars(`type`))

## 4.5 Plot SDIs ####
### remove CoG data ####
if(type == "BestSurveys"){
  sdistk_long[sdistk_long$`Spatial Indicator` == "CoG (x)" | 
                sdistk_long$`Spatial Indicator` == "CoG (y)",]$`Spatial Indicator Value` <- NaN
}

## Order surveys by coverage ####
sdistk_long$`Survey Index, Survey Name` <- factor(sdistk_long$`Survey Index, Survey Name`, levels=survorder)

sdi_plot1 <- ggplot() + 
  geom_line(data = sdistk_long, aes(x = Year, y = `Spatial Indicator Value`, colour = `Survey Index, Survey Name`), linewidth = 0.6, key_glyph = "rect") + 
  scale_colour_manual(values = colrs) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder)), scales = "free") +
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
    legend.position = "right",
    legend.key = element_rect(colour="black", linewidth = 0.5), # border around glyphs
    legend.key.width = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"), # distance between each key glyph
    # Axis
    axis.text = element_text(size = 8),
    #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
    axis.title = element_text(size = 10),
    aspect.ratio = 1,
    plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
  ) +
  guides(colour = guide_legend(byrow = TRUE)) # to enable vertical spacing between key glyphs

## 4.6 Overlay SSB plot SDI plot ####
sdissb_plot <- ggdraw()+ draw_plot(sdi_plot1)+ draw_plot(ssb_plot, x= .76, y= 0.2977, width=.206, height = .206)
## 4.7 Save ####
cowplot::save_plot(plot = sdissb_plot, filename = paste0(si.plot.path, "SpatIndPlot-", stk.chr, ".png"), base_height = 25, base_width = 12)

# 5. ROC & TSS ####
roc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/ROC/", type, "/", stk.chr, "/")
roc.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/ROC/", type, "/", stk.chr, "/")

# Check if there are any NAs in the spatial indicator series
if(any(is.na(sdistk_wide))){
  writeLines("sdistk_wide contains NAs")
  which(is.na(sdistk_wide), arr.ind=TRUE)
}

## 5.1 Add SSB ####
stkssb <- as.data.frame(ssb(stk))[c("year", "data")] %>%
  rename(Year = year, SSB = data)
## 5.2 SSB/MSY Btrigger ####
# get ref point
msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
# calc SSB/MSY B trigger
stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt
## 5.3 Merge data ####
sdistk_wide <- merge(sdistk_wide, stkssb, by = "Year") %>%
  relocate(Year, Quarter, StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, SSB, ssb.msybtrig)
# remove year where there isn't SSB data ir where there isnt SI data
#sdistk_wide <- na.omit(sdistk_wide)

## 5.4 ROC & TSS ####
### 5.4.1 Set Parameters ####
state <- "ssb.msybtrig"
inds <- c("Gini Index", "D95", "Positive Area (Haul)", "Positive Area (Rectangle)", 
          "SPI", "Spreading Area", "Equivalent Area", "CoG (x)", "CoG (y)", "Inertia",
          "Ellipse Area", "Convex Hull Area")
roc_longlist <- list()
### 5.4.2 For each survey... ####
for(i in 1:length(unique(sdistk_wide$`Survey Index, Survey Name`))){
  surveyROC <- sdistk_wide %>% filter(`Survey Index, Survey Name` == unique(sdistk_wide$`Survey Index, Survey Name`)[i])
  ##### 3.2.1 Compute data ####
  # roc_long <- roc_fun3(surveyROC, state, inds, return = 'long')
  roc_long <- roc_fun4(data = surveyROC, obs = state, preds = inds, return = 'long')
  
  if(all(unique(roc_long$`Spatial Indicator`) != inds)){warning("Not all spatial indicators ended up in the ROC output df", immediate. = T)}
  roc_longlist[[i]] <- roc_long
}
#### 3.3 Merge lists of surveys ####
rocAll_long <- do.call(rbind, roc_longlist)
rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
head(rocAll_long)
#### 3.4 Save Data ####
if(dir.exists(roc.data.path) == FALSE){
  dir.create(roc.data.path)
}
save(rocAll_long, file = paste0(roc.data.path, stk.chr, " - ROC_long.rda"))

# 6. Plot ROC ####
# remove COG data for best surveys
if(type == "BestSurveys"){
  rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                rocAll_long$`Spatial Indicator` == "CoG (y)",]$FPR <- NaN
  rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                rocAll_long$`Spatial Indicator` == "CoG (y)",]$TPR <- NaN
  rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                rocAll_long$`Spatial Indicator` == "CoG (y)",]$TSS <- NaN
  rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                rocAll_long$`Spatial Indicator` == "CoG (y)",]$AUC <- NaN
}

## 6.1 Optimum Threshold #### 
# Get the optimum threshold for each survey for each spatial ind
optthresh <- rocAll_long %>%
  group_by(`Survey Index, Survey Name`, `Spatial Indicator`) %>%
  filter(TSS == max(TSS))

# order surveys by coverage
rocAll_long$`Survey Index, Survey Name` <- factor(rocAll_long$`Survey Index, Survey Name`, levels=survorder)

## 6.2 Plot ####
roc_plot <- ggplot() +
  geom_path(data = rocAll_long, aes(x = FPR, y = TPR, colour = `Survey Index, Survey Name`), size = 1, alpha = 0.9, key_glyph = "rect") +
  geom_point(data = optthresh, aes(x = FPR, y = TPR, colour = `Survey Index, Survey Name`), size = 2) +
  scale_colour_manual(values = colrs) + 
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  facet_wrap(vars(factor(`Spatial Indicator`, 
                         levels = indorder))) +
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

## 6.3 Save ####
cowplot::save_plot(plot = roc_plot, filename = paste0(roc.plot.path, "rocPlot-", stk.chr, ".png"), base_height = 8, base_width = 12)

# 7. Plot TSS ####
# remove COG data for best surveys
if(type == "BestSurveys"){
  
}

## 7.1 Optimum Threshold #### 
tss.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/TSS/", type, "/", stk.chr, "/")

# Get the optimum threshold for each survey for each spatial ind
optthresh <- rocAll_long %>%
  group_by(`Survey Index, Survey Name`, `Spatial Indicator`) %>%
  filter(TSS == max(TSS))

## 7.2 Plot ####
tss_plot <- ggplot() +
  geom_line(data = rocAll_long, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`), linewidth = 0.8, alpha = 0.9, key_glyph = "rect") +
  geom_point(data = optthresh, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`), size = 2) +
  scale_colour_manual(values = colrs) + 
  geom_abline(slope = c(0,0)) +
  facet_wrap(vars(factor(`Spatial Indicator`, 
                                    levels = indorder)), scales = "free_x") +
  labs(title = paste0("True Skill Score (", stk.chr, ")"))+
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

## 7.3 Save ####
cowplot::save_plot(plot = tss_plot, filename = paste0(tss.plot.path, "tssPlot-", stk.chr, ".png"), base_height = 8, base_width = 12)

# 8. Plot AUC ####
## 8.1. Plot ####
auc.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/AUC/", type, "/", stk.chr, "/")
auc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/AUC/", type, "/", stk.chr, "/")

AUC_plot <- ggplot() +
  geom_col(data = rocAll_long, aes(x = `Survey Index, Survey Name`, y = AUC, fill = `Survey Index, Survey Name`), position = position_dodge(width = 2), width = 0.8) +
  scale_fill_manual(values = colrs) +
  guides(alpha = "none") +
  #scale_colour_identity() +
  #geom_text(data = auc_df, aes(x = `Survey Index, Survey Name`, y = AUC, label = sig), vjust = -.0001) + # add astericks where AUC > 0.75
  #scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0.5, colour = "grey20", lty = 2) +
  facet_wrap(vars(factor(`Spatial Indicator`, 
                         levels = indorder))) +
  xlab("") +
  ylab("Area Under ROC Curve") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  theme(axis.text.x = element_blank(),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black"),
        # Legend
        legend.position = "right")

## 8.1 Save AUC Plot #### 
cowplot::save_plot(plot = AUC_plot, filename = paste0(auc.plot.path, "aucPlot.png"), base_height = 8, base_width = 12)

# 9. Plot TSS Summary ####
tssSum.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/TSS_Sum/", type, "/", stk.chr, "/")
tssSum.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/TSS_Sum/", type, "/", stk.chr, "/")

tssSum_df <- data.frame()
tssSum_df2 <- data.frame()
## 9.1 Get TSS #### 
for(j in unique(rocAll_long$`Survey Index, Survey Name`)){
  rocSpcs_long <- filter(rocAll_long, `Survey Index, Survey Name` == j)
  for(w in unique(rocAll_long$`Spatial Indicator`)){
    rocSpcsIndx <- filter(rocSpcs_long, `Spatial Indicator` == w)
    rocSpcsIndx_long <- filter(rocSpcsIndx, is.na(TSS) == F) %>%
      filter(TSS == max(TSS))
    tssSum_df <- rbind(tssSum_df, rocSpcsIndx_long)
  }
}
tssSum_df <- rocAll_long

## 9.2 Save TSS Summary Data ####
save(tssSum_df, file = paste0(tssSum.data.path, "tssSum_longdata.rda"))

## 9.2 Plot ####
tssSum_df3 <- tssSum_df %>%
  group_by(`Survey Index, Survey Name`, `Spatial Indicator`) %>%
  summarise(y0 = min(TSS),
            y25 = quantile(TSS, 0.25, na.rm = T),
            y50 = median(TSS),
            y75 = quantile(TSS, 0.75, na.rm = T),
            y100 = max(TSS))
# order surveys by coverage
tssSum_df3$`Survey Index, Survey Name` <- factor(tssSum_df3$`Survey Index, Survey Name`, levels=survorder)

tssSum_plot3 <- ggplot() +
  geom_errorbar(data = tssSum_df3, aes(x = `Survey Index, Survey Name`, ymin = y0, ymax = y100, colour = `Survey Index, Survey Name`), linewidth = 1, width = 0.5, key_glyph = "rect") +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder))) +
  scale_colour_manual(values = colrs) +
  geom_hline(yintercept = 0, colour = "grey20", lty = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  xlab("") +
  ylab("True Skill Score (TSS)") +
  # Theme
  theme(
    axis.text.x = element_blank(),
    # Legend
    legend.position = "right",
    legend.key = element_rect(colour="black", linewidth = 0.5), # border around glyphs
    legend.key.width = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"), # distance between each key glyph
    # Panels
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black")
  ) +
  guides(colour = guide_legend(byrow = TRUE, title = "Survey Index, Survey Name")) # to enable vertical spacing between key glyphs

cowplot::save_plot(plot = tssSum_plot3, filename = paste0(tssSum.plot.path, "tssSumPlot3.png"), base_height = 8, base_width = 12)

# 10. Spatial Indicators with Optimal Threshold ####
## 10.1 Add Optimum Threshold ####
# Get the spat ind value where TSS was maximised 
optthreshdata <- optthresh %>%
  select(Year, StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, `Spatial Indicator`, `Spatial Indicator Value`, `TSS`) %>%
  rename("OptThresh" = `Spatial Indicator Value`, "MaxTSS" = TSS, "OptYear" = Year) %>%
  filter(OptYear != 998)

# There should be no duplicates in this check:
# Sometimes there are multiple optimum thresholds. In these cases, take the one with
# highest TNR i.e. identifies more cases corrctl where stock status is impaired
# TNR is 1-FPR. Therfreo just minimi
if(any(duplicated(optthreshdata[2:6]))){
  duprowno <- unique(sort(c(which(duplicated(optthreshdata[2:6])), which(duplicated(optthreshdata[2:6]))-1)))
  duprows <- optthreshdata[duprowno,] %>%
    rename("Year" = OptYear, "Spatial Indicator Value" = OptThresh, "TSS" = MaxTSS) %>%
    mutate(Duplicate = "TRUE")
  tssdup <- merge(tssSum_df, duprows, by = c("Year", "StockID", "SurveyIndex", "Survey", "Survey Index, Survey Name", "Spatial Indicator", "Spatial Indicator Value", "TSS"))
  tssdup$TNR <- 1-tssdup$FPR
  
  duprows <- tssdup[tssdup$Duplicate == "TRUE",] %>% 
    arrange(StockID, `Survey Index, Survey Name`, `Spatial Indicator`) %>%
    group_by(StockID, `Survey Index, Survey Name`, `Spatial Indicator`) %>%
    mutate(maxTNR = if_else(TNR == max(TNR), "TRUE", "FALSE")) %>%
    relocate(maxTNR) %>%
    select(maxTNR, Year, StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, `Spatial Indicator`, `Spatial Indicator Value`, TSS) %>%
    rename("OptYear" = Year, "OptThresh" = `Spatial Indicator Value`, "MaxTSS" = TSS)
  optthreshdata <- full_join(optthreshdata, duprows, by = c("OptYear", "StockID", "SurveyIndex", "Survey", "Survey Index, Survey Name", "Spatial Indicator", "OptThresh", "MaxTSS")) %>%
    mutate(maxTNR = if_else(is.na(maxTNR), "ignore", maxTNR)) %>%
    filter(maxTNR != "FALSE") %>%
    select(-maxTNR, -OptYear) %>%
    group_by(StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, `Spatial Indicator`) %>%
    distinct()
}
# check again
if(any(duplicated(optthreshdata[2:6]))){print("Still not okay")}

#> We have duplicates when multiple spatial indicator threshold values, give the
#> same TSS, which also happens to be the max TSS. So there are multiple points
#> where the TSS is maximized. How do we decide which threshold to choose?
#> The TSS is the sum of the true positive rate and the true negative rate.
#> We could select the threshold by looking at these individual values. It may 
#> be more important to us to correctly identify when the stock is in a negative
#> state as rather than when it is healthy. Therefore we would select the 
#> threshold with the highest TNR. If the TNR and TPR are also identical, then 
#> we could decide on which threshold to use by looking at the TSS of the 
#> neighboring thresholds. If the neighboring thresholds have only a slightly
#> lower TSS, then this is better than having neighboring thresholds that have 
#> very low or negative TSS. 

# Merge with spat ind data and divde spat inds by the optimal threshold
# No everything above 1 = healthy, below 1 = unhealthy
sdistk_stndrd <- left_join(sdistk_long, optthreshdata, by = c("StockID", "SurveyIndex", "Survey", "Survey Index, Survey Name", "Spatial Indicator"))
sdistk_stndrd$`Spatial Indicator Value/OptThresh` <- sdistk_stndrd$`Spatial Indicator Value`/sdistk_stndrd$OptThresh
head(sdistk_stndrd)

## 10.2 Get SSB/MSY Btrigger ####
strtyr <- min(sdistk_long$Year)
if(strtyr < range(stk)["minyear"]){
  warning("First year of survey data provided preceeds first year of data in the stock object. Using minyear of the stock object instead.", immediate. = TRUE)
  strtyr <- range(stk)["minyear"]}

endyr <- max(sdistk_wide$Year)
if(endyr > range(stk)["maxyear"]){
  warning("Last year of survey data provided exceeds available last year of data in the stock object. Using maxyear of the stock object instead.", immediate. = TRUE)
  endyr <- range(stk)["maxyear"]}  

msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
stkssb <- as.data.frame(ssb(stk)[,ac(strtyr:endyr)])[c("year", "data")] %>%
  rename(Year = year, SSB = data) %>%
  mutate(type = "SSB")
stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt

## 10.3 Plot SSB/MSY Btrigger ####
ssb_plot <- ggplot() + geom_line(data = stkssb, aes(x = Year, y = ssb.msybtrig), colour = "black") +
  geom_hline(yintercept = 1, colour = "grey20", lty = 2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black", fill = "grey20"),
        strip.text = element_text(colour = "white", face = "bold"), 
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("SSB/MSY Btrigger") +
  facet_wrap(vars(`type`))

# order surveys by coverage
sdistk_stndrd$`Survey Index, Survey Name` <- factor(sdistk_stndrd$`Survey Index, Survey Name`, levels=survorder)

## 10.0 Remove Inds < 0.5 TSS ####
# for best surveys
if(type == "BestSurveys"){
  delinds <- rocAll_long %>%
    group_by(`StockID`, `Spatial Indicator`, `Survey Index, Survey Name`) %>%
    summarise(MaxTSS = max(TSS), bin = max(TSS) < 0.5) %>%
    filter(bin == TRUE)
  sdistk_stndrd[sdistk_stndrd$StockID %in% delinds$StockID & 
                  sdistk_stndrd$`Spatial Indicator` %in% delinds$`Spatial Indicator` &
                  sdistk_stndrd$`Survey Index, Survey Name` %in% delinds$`Survey Index, Survey Name`,]$`Spatial Indicator Value/OptThresh` <- NaN
}

## 10.4 Plot SIs with OptThresh ####
optthresh_plot2 <- ggplot() + 
  geom_line(data = sdistk_stndrd, aes(x = Year, y = `Spatial Indicator Value/OptThresh`, colour = `Survey Index, Survey Name`), linewidth = 0.6, key_glyph = "rect") + 
  geom_hline(yintercept = 1, colour = "grey20", lty = 2) +
  scale_colour_manual(values = colrs) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder)), scales = "free") +
  labs(title = paste0("Spatial Indicator Time Series/Optimum Threshold (", stk.chr, ")"))+
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

## 10.5 Overlay SSB & SDI ####
optthresh_plot <- ggdraw()+ draw_plot(optthresh_plot2)+ draw_plot(ssb_plot, x= .76, y= 0.2977, width=.206, height = .206)
## 10.6 Save ####
cowplot::save_plot(plot = optthresh_plot, filename = paste0(si.plot.path, "/", "OptSpatIndPlot-", stk.chr, ".png"), base_height = 25, base_width = 12)

### View all plots ####
sdissb_plot; roc_plot; tss_plot; AUC_plot; tssSum_plot3; optthresh_plot

# 11. Best Surveys ####
#> the 'best' surveys are considered those which cover most of the stock boundary
#> first we need to objectively identify which ones this is
#> then we will remove the surveys which only cover a small percentage of the stock boundary
#> we will also remove surveys which have no cstock status contast in the those survey years
#> if there is no contrast, the ROC curve cannot be calculated and this analysis is void

## 11.1 Stock Boundary Area ####
meanrects
#> this shows the mean percentage of rectangles in the stock boundary surveyed 
#> across the years of data used in the stock assessment 
#> Now we need to use this to filter the surveys out that are not 
#> representative of the whole stock boundary
#> E.g. those with survey coverage of less than 25%
bestsurveys <- meanrects[meanrects$SurvCoverage >= 25,]

## 11.2 Surveys without contrast ####
#> these are surveys where the ROC could not be computed 
#> and therefore the classification ability could not tested using this survye data
#> data does not show up on full plots, but the survey will still be in the legend
#> lets remove these surveys
#> here i identify the surveys where AUC == NaN, indictaing ROC could not calculated
t <- rocAll_long %>% filter(AUC == "NaN")
# remove factoring
t$`Survey Index, Survey Name` <- as.character(t$`Survey Index, Survey Name`)
# find survey(s)
nocontrast <- unique(t$`Survey Index, Survey Name`)
# remove from best surveys
bestsurveys <- bestsurveys[bestsurveys$`Survey Index, Survey Name` != nocontrast,]

## 11.3 Filter stk_data_filtered ####
# remove surveys from stk_data_filtered and re-run whole analysis from section 3.
srvsfilt <- list()
for(i in 1:nrow(bestsurveys)){
  d <- stk_data_filtered[[bestsurveys$SurveyIndex[i]]][[bestsurveys$SurveyName[i]]]
  srvsfilt[[bestsurveys$SurveyIndex[i]]][[bestsurveys$SurveyName[i]]] <- d
}

type <- "BestSurveys" # this changes where the filtered analysis is saved so analysis with allsurveys is not overwritten
#stk_data_filtered <- srvsfilt # now go through section 3 to 10
# NOTE: Analysis must be run with all surveys first


