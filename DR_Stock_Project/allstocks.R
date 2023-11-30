#> Calculate Spatial Indicators
#> Loop through all stocks at once
rm(list = ls())

### A. Load in requirements ----------------------------------------------------
# load packages
pckgs <- c("FLCore", "FLBRP", "dplyr", "ggplot2", "ggplotFL", "rgdal", 
           "DataExplorer", "rgeos", "sf", "mapplots", "maptools", "mapproj", 
           "beepr", "patchwork", "ineq", "icesDatras", "icesVocab", "scales", 
           "readxl", "data.table", "zoo", "cowplot", "pracma")
for(pkg in pckgs){
  library(pkg, character.only = T, quietly = T)
}
rm(pckgs, pkg)

#### 1. Source user functions ####
source(paste0(getwd(), "/Functions/dataprep_funs.R")) # for dealing with datras data
source(paste0(getwd(), "/Functions/spatinds_funs.R")) # for computing spatial indicators
source(paste0(getwd(), "/Functions/ROC_funs.R"))      # for ROC and TSS

#### 2. Load ICES rectangles and divisions ####
load(paste0(getwd(), "/Data/ICES Rect/ices_rect.rds"))
load(paste0(getwd(), "/Data/ICES Divs/ices_divs.rds"))

#### 3. Load stock metainformation ####
allstk_metadata <- read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice Sheets/2022/stock_metadata_refpts.xlsx"), sheet = "stk_metadata")
allstk_refpts <- read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice Sheets/2022/stock_metadata_refpts.xlsx"), sheet = "stk_refpts")
allstk_spcsinfo <- read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice Sheets/2022/stock_metadata_refpts.xlsx"), sheet = "spcs_info")

#### 4. Load Stock Objects ####
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock Objects/2022/")
for(stockfile in list.files(stockobj.path)){load(paste0(stockobj.path, stockfile))}
stocklist <- list(cod.27.47d20_nov, had.27.46a20, ple.27.420, ple.27.7d, 
                  pok.27.3a46, sol.27.4, tur.27.4, whg.27.47d, wit.27.3a47d) # ignore so.27.7d for now, need to find YFS survey data
stocklist.chr <- list("cod.27.47d20_nov", "had.27.46a20", "ple.27.420", "ple.27.7d", 
                      "pok.27.3a46", "sol.27.4", "tur.27.4", "whg.27.47d", "wit.27.3a47d") # ignore so.27.7d for now, need to find YFS survey data
#### 5. Toggle type ####
type <- "AllSurveys" # `AllSurveys` or `BestSurveys`

#### 6. Toggle stock ####
# Toggle to specific stocks or leave to run for all stocks
#stocklist <- list(ple.27.420)
#stocklist.chr <- list("ple.27.420")

### B. Calculate Spatial Indicators --------------------------------------------
meanrects <- data.frame() # storage for ordering surveys by coverage

for(i in 1:length(stocklist)){
  tic()
  #### 1. Run Loop ####
  # Select the stock to analyse
  stk <- stocklist[[i]]
  stk.chr <- stocklist.chr[[i]]
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
  stk_divs %in% ices_rect$Area_27
  stk_divs %in% ices_divs$Area_27
  
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
  
  ###### 1.4.3 `stk_surveys_indices` (might not need) ####
  stk_surveys_indices <- allstk_metadata %>% filter(
    stk_id == stk.chr) %>%
    select(survey_index) %>%
    unique()
  print(stk_surveys_indices)
  
  ##### 1.5 Filter Survey Data ####
  ###### 1.5.1 `stk_data_filtered` ####
  
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
  
  ###### 1.5.2 Check filtered data ####
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
  
  #### 2. Calculate Spatial Indicators ####
  ##### 2.1 Start Loop ####
  # Calculate the number of rectangles within the stock boundary
  # use ICES shapefiles and stk_divs from earlier
  totrec <- ices_rect %>% 
    filter(Area_27 %in% stk_divs) %>%
    summarise(N = length(unique(ICESNAME)))
  
  for(indx in 1:length(stk_data_filtered)){
    for(survey in 1:length(stk_data_filtered[[indx]])){
      # save path
      si.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/SpatInd/", type, "/", stk.chr, "/")
      # survey coverage
      survrec <- stk_data_filtered[[indx]][[survey]]$hlhh %>%
        filter(Area_27 %in% stk_divs) %>%
        group_by(Year) %>%
        summarise(ny = length(unique(StatRec)))
      n <- mean(survrec$ny)
      output <- c(stk.chr, 
                  paste0(stk_divs, collapse = ", "), 
                  min(stk_data_filtered[[indx]][[survey]]$hlhh$Year),
                  max(stk_data_filtered[[indx]][[survey]]$hlhh$Year),
                  names(stk_data_filtered[indx]), 
                  names(stk_data_filtered[[indx]][survey]), 
                  n)
      meanrects <- rbind(meanrects, output)
      
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
      ##### 2.2 Positive Area (by ICES rectangle) ####
      writeLines(noquote("Positive Area (Rectangle)"))
      np.rects <- pa_rect( 
        hlhh = hlhh, 
        yrs = yrs, 
        qrs = qrs, 
        species_aphia = species_aphia, 
        stk_divs = stk_divs)
      
      ##### 2.3 Positive Area (by Haul) ####
      writeLines(noquote("Positive Area (Haul)"))
      np.hauls <- pa_haul(
        hlhh = hlhh, 
        yrs = yrs, 
        qrs = qrs, 
        species_aphia = species_aphia, 
        stk_divs = stk_divs)
      
      ##### 2.4 Lorenz curve data ####
      writeLines(noquote("Lorenz Curve"))
      lorenz <- lorenz_data(hlhh = hlhh, 
                            yrs = yrs, 
                            qrs = qrs, 
                            species_aphia = species_aphia, 
                            stk_divs = stk_divs)
      
      ###### 2.4.1 Gini index ####
      writeLines(noquote("Gini Index"))
      Gini.index <- Gini(lorenz)
      
      ###### 2.4.2 D95 ####
      writeLines(noquote("D95"))
      D95 <- d95(lorenz)
      
      ##### 2.5 Spread of Participation Index (SPI) ####
      #### Prep data
      writeLines(noquote("Spread of Participation Index"))
      SPI <- spi(
        hlhh = hlhh,
        yrs = yrs,
        qrs = qrs,
        species_aphia = species_aphia,
        stk_divs = stk_divs
      )
      
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
      
      ##### 2.7 Equivalent Area ####
      writeLines(noquote("Equivalent Area"))
      ea <- sa_data %>%
        group_by(Year) %>%
        summarise("Equivalent Area" = equivalentarea(TotalNo_Dur)) %>%
        mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% # add quarters
        relocate(Year, Quarter)
      
      ##### 2.8 Centre of Gravity, Inertia, & 95% CI Ellipse ####
      writeLines(noquote("CoG and Inertia"))
      cog <- coginis(hlhh = hlhh,
              yrs = yrs,
              qrs = qrs,
              species_aphia = species_aphia,
              stk_divs = stk_divs,
              iso = F, plot = F, density = F)
      
      #cog <- coginert(
      #  hlhh = hlhh,
      #  yrs = yrs,
      #  qrs = qrs,
      #  species_aphia = species_aphia,
      #  stk_divs = stk_divs)
      
      ##### 2.9 Convex Hull Area ####
      writeLines(noquote("Convex Hull Area"))
      cha <- chullarea(
        hlhh = hlhh,
        yrs = yrs,
        qrs = qrs,
        species_aphia = species_aphia,
        stk_divs = stk_divs)
      
      ##### 2.10 Ellipse Area ####
      writeLines(noquote("95% CI Ellipse"))
      ela <- ellarea(
        hlhh = hlhh,
        yrs = yrs,
        qrs = qrs,
        species_aphia = species_aphia,
        stk_divs = stk_divs)
      
      #### 3. Merge Data ####
      #Merge all spatial indicator values into a single data frame
      message("\nMerging and saving spatial indicator data...")
      ## select the columns that we need
      Gini.index <- Gini.index[c("Year", "Quarter", "Gini Index")]
      D95 <- D95
      np.hauls <- np.hauls[c("Year", "Quarter", "PosAreaH")]
      np.rects <- np.rects[c("Year", "Quarter", "PosAreaR")]
      SPI <- SPI[c("Year", "Quarter", "SPI.dur")]
      sa <- sa
      ea <- ea
      cog <- cog
      cha <- cha[c("Year", "Quarter", "areaoccupied")]
      ela <- ela
      
      ## merge
      si <- Reduce(function(x, y) full_join(x, y, by = c("Year", "Quarter")), list(Gini.index, D95, np.hauls, np.rects, SPI, sa, ea, cog, cha, ela)) 
      # `full_join` keeps rows where certain spatinds could not be calculated but others are available. `merge` removes these rows
      ## rename some columns
      si <- si %>%
        rename("Positive Area (Haul)" = PosAreaH,
               "Positive Area (Rectangle)" = PosAreaR,
               "Convex Hull Area" = areaoccupied,
               "SPI" = SPI.dur) %>%
        mutate(SurveyIndex = names(stk_data_filtered[indx]),
               Survey = names(stk_data_filtered[[indx]][survey]),
               StockID = stk.chr) %>%
        relocate(StockID, SurveyIndex, Survey, Year, Quarter, `Gini Index`) #, D95, `Positive Area (Haul)`, `Positive Area (Rectangle)`, SPI) these might also have to be ordered to match roc order
      #### 3.1 Save Data ####
      path <- paste0(si.data.path, names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey]))
      dir.create(path, recursive = TRUE)
      save(si, file = paste0(path, "/SpatIndData - ", stk.chr, " - ", names(stk_data_filtered[indx]), " - ", names(stk_data_filtered[[indx]][survey]), ".rda"))
      #### 3.2 Mean rects ####
      }
  }
  message("Complete")
  toc()
}
# Mean rects dataframe
# Organise survey coverage dataframe
colnames(meanrects) <- c("StockID", "StkDivs", "YrStrt", "YrEnd", "SurveyIndex", "SurveyName", "MeanRects")
meanrects$MeanRects <- as.numeric(meanrects$MeanRects)
meanrects$TotalRects <- totrec$N
meanrects$SurvCoverage <- round((meanrects$MeanRects/meanrects$TotalRects)*100, 2)
meanrects$`Survey Index, Survey Name` <- paste0(meanrects$SurveyIndex, ", ", meanrects$SurveyName)
# save meanrects 
head(meanrects)
save(meanrects, file = paste0(getwd(), "/Data/meanrects.rda"))

### C. Plot Spatial Indicators ####
#### 1. Load and Prepare Data ####
# Order indicators for facet_wrap
indorder <- c("CoG (x)", "CoG (y)", # Location
              "Inertia", "Convex Hull Area", "Ellipse Area", # Dispersion
              "Positive Area (Rectangle)", "Positive Area (Haul)", # Occupancy
              "Gini Index", "D95", "Spreading Area", # Aggregation 
              "Equivalent Area", "SPI")
# load meanrects
load(paste0(getwd(), "/Data/meanrects.rda"))

# Get all SDI data and convert into long format
si.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/SpatInd/AllSurveys/")
si.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/SpatInd/", type, "/")

sdi_longlist <- list()
sdi_widelist <- list()
i <- 1

for(StockFolder in stocklist.chr){
  message(paste0("Stock: ", StockFolder))
  for(IndexFolder in list.files(paste0(si.data.path, StockFolder))){
    message(paste0("Survey Index: ", IndexFolder))
    for(SurveyFolder in list.files(paste0(si.data.path, StockFolder, "/", IndexFolder))){
      print(noquote(paste0("Survey: ", SurveyFolder)))
      #Load  Data
      si_file <- list.files(paste0(si.data.path, StockFolder, "/", IndexFolder, "/", SurveyFolder, "/"), pattern = "*SpatIndData*")
      load(paste0(si.data.path, StockFolder, "/", IndexFolder, "/", SurveyFolder, "/", si_file))
      #si$Inertia <- si$Inertia/1000000
      #colnames(si)[colnames(si)=="Inertia"] <- "Inertia (million)"
      sdi_widelist[[i]] <- si
      #  Convert wide to long 
      si_long <- si %>% tidyr::pivot_longer(cols = indorder, 
                                            names_to = "Spatial Indicator",
                                            values_to = "Spatial Indicator Value")
      si_long$`Spatial Indicator` <- factor(si_long$`Spatial Indicator`, levels = indorder) # factor & relocate
      sdi_longlist[[i]] <- si_long
      i <- i + 1
    }
  }
}

#### 2. Merge Data ####
sdiall_long <- do.call(rbind, sdi_longlist)
sdiall_wide <- do.call(rbind, sdi_widelist)
# Create new column for line plot
sdiall_long$`Survey Index, Survey Name` <- paste0(sdiall_long$SurveyIndex, ", ", sdiall_long$Survey)
sdiall_wide$`Survey Index, Survey Name` <- paste0(sdiall_wide$SurveyIndex, ", ", sdiall_wide$Survey)

for(i in 1:length(stocklist)){
  tic()
  #### 4. Plot ####
  # Select the stock to analyse
  stk <- stocklist[[i]]
  stk.chr <- stocklist.chr[[i]]
  message(paste0("\n",
                 paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = ""),"\n# ", 
                 stk.chr, " #", "\n",
                 paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = "")))
  
  writeLines("Filter SI data to stock")
  sdistk_long <- filter(sdiall_long, StockID == stk.chr) 
  sdistk_wide <- filter(sdiall_wide, StockID == stk.chr)
  
  # filter survey coverage data to stock
  scov <- filter(meanrects, StockID == stk.chr)
  # order surveys by coverage for plot legend
  survorder <- arrange(scov, desc(SurvCoverage))$`Survey Index, Survey Name`
  sdistk_long$`Survey Index, Survey Name` <- factor(sdistk_long$`Survey Index, Survey Name`, levels=survorder)
  # colours for surveys (cool = higher survey coverage)
  colrs <- c("blue4", "#00695C", "#8BC34A", "gold2", "#E78100FF", "#F5191CFF", "#9C27B0") # "#FFD320", 
  names(colrs) <- survorder
  
  ##### 4.1 Best Surveys ####
  if(type == "BestSurveys"){
    stkbestsurveys <- filter(bestsurveys, StockID == stk.chr)
    # Filter out surveys that do not have at least 25% coverage
    sdistk_long <- filter(sdistk_long, 
                          StockID %in% stkbestsurveys$StockID,
                          SurveyIndex %in% stkbestsurveys$SurveyIndex,
                          Survey %in% stkbestsurveys$SurveyName,
                          `Survey Index, Survey Name` %in% stkbestsurveys$`Survey Index, Survey Name`)
    # Remove CoG (add other inds if desired)
    sdistk_long[sdistk_long$`Spatial Indicator` == "CoG (x)" | 
                  sdistk_long$`Spatial Indicator` == "CoG (y)",]$`Spatial Indicator Value` <- NaN
    # Keep colours consistent between allsurvey plots and bestsurevy plots
    b <- paste0(stkbestsurveys$SurveyIndex, ", ", stkbestsurveys$SurveyName)
    colrs <- colrs[b]
  }
  if(nrow(sdistk_long) == 0){
    next
  }
  
  # Get SSB for survey years
  writeLines("Get SSB for survey years")
  
  strtyr <- min(sdistk_long$Year)
  if(strtyr < range(stk)["minyear"]){
    warning("First year of survey data provided preceeds first year of data in the stock object. Using minyear of the stock object instead.", immediate. = TRUE)
    strtyr <- range(stk)["minyear"]}
  
  endyr <- max(sdistk_long$Year)
  if(endyr > range(stk)["maxyear"]){
    warning("Last year of survey data provided exceeds available last year of data in the stock object. Using maxyear of the stock object instead.", immediate. = TRUE)
    endyr <- range(stk)["maxyear"]}  
  
  msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
  stkssb <- as.data.frame(ssb(stk)[,ac(strtyr:endyr)])[c("year", "data")] %>%
    rename(Year = year, SSB = data) %>%
    mutate(type = "SSB")
  stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt
  
  # Plot SSB
  writeLines("Plot SSB")
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
  
  # Plot SDIs
  writeLines("Plot Spatial Indicators")
  # Filter years so that the years in the spatind plot donot predate the ssb plot
  sdistk_long <- sdistk_long %>% filter(Year %in% stkssb$Year)
  sdi_plot1 <- ggplot() + 
    geom_line(data = sdistk_long, aes(x = Year, y = `Spatial Indicator Value`, colour = `Survey Index, Survey Name`), key_glyph = "rect") + 
    scale_colour_manual(values = colrs) +
    facet_wrap(vars(factor(`Spatial Indicator`, 
                           levels = indorder)), scales = "free") +
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
  
  # Overlay SSB plot SDI plot
  writeLines("Plot layout")
  sdissb_plot <- ggdraw()+ draw_plot(sdi_plot1)+ draw_plot(ssb_plot, x= .76, y= 0.2977, width=.206, height = .206)
  #### 5. Save ####
  writeLines("Save")
  dir.create(paste0(si.plot.path, stk.chr), recursive = TRUE)
  cowplot::save_plot(plot = sdissb_plot, filename = paste0(si.plot.path, stk.chr, "/", "SpatIndPlot-", stk.chr, ".png"), base_height = 25, base_width = 12)
  toc()
}

### D. Calculate ROC and TSS ####
# Run C.1. and C.2. to get all spatial indicator data in both long and wide format
roc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/ROC/", type, "/")

#### 2. Get stock specific data ####
for(i in 1:length(stocklist)){
  tic()
  # Select the stock to analyse
  stk <- stocklist[[i]]
  stk.chr <- stocklist.chr[[i]]
  message(paste0("\n",
                 paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = ""),"\n# ", 
                 stk.chr, " #", "\n",
                 paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = "")))
  writeLines("Filter SI data to stock")
  # Filter data from section C to specific stock
  sdistk_long <- filter(sdiall_long, StockID == stk.chr) 
  sdistk_wide2 <- filter(sdiall_wide, StockID == stk.chr)
  # Check if there are any NAs in the spatial indicator series
  if(any(is.na(sdistk_wide))){
    writeLines("sdistk_wide contains NAs")
    which(is.na(sdistk_wide), arr.ind=TRUE)
  }

  ##### 2.1 Add SSB ####
  writeLines("Add SSB to dataframe")
  stkssb <- as.data.frame(ssb(stk))[c("year", "data")] %>%
    rename(Year = year, SSB = data)
  #### 2.2 SSB/MSY Btrigger ####
  # get ref point
  msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
  # calc SSB/MSY B trigger
  stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt
  #### 2.3 Merge data ####
  sdistk_wide <- merge(sdistk_wide2, stkssb, by = "Year") %>%
    relocate(Year, Quarter, StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, SSB, ssb.msybtrig)
  # remove year where there isn't SSB data ir where there isnt SI data
  sdistk_wide <- na.omit(sdistk_wide)
  
  ### 3. ROC & TSS ####
  #### 3.1 Set Parameters ####
  state <- "ssb.msybtrig"
  inds <- c("Gini Index", "D95", "Positive Area (Haul)", "Positive Area (Rectangle)", 
            "SPI", "Spreading Area", "Equivalent Area", "CoG (x)", "CoG (y)", "Inertia",
            "Ellipse Area", "Convex Hull Area")
  roc_longlist <- list()
  #### 3.2 For each survey... ####
  for(i in 1:length(unique(sdistk_wide$`Survey Index, Survey Name`))){
    surveyROC <- sdistk_wide %>% filter(`Survey Index, Survey Name` == unique(sdistk_wide$`Survey Index, Survey Name`)[i])
    ##### 3.2.1 Compute data ####
    roc_long <- roc_fun4(surveyROC, state, inds, return = 'long', p = FALSE)
    if(all(unique(roc_long$`Spatial Indicator`) != inds)){warning("Not all spatial indicators ended up in the ROC output df", immediate. = T)}
    roc_longlist[[i]] <- roc_long
  }
  #### 3.3 Merge lists ####
  rocAll_long <- do.call(rbind, roc_longlist)
  #### 3.4 Save Data ####
  dir.create(paste0(roc.data.path, stk.chr), recursive = TRUE)
  save(rocAll_long, file = paste0(roc.data.path, stk.chr, "/", stk.chr, " - ROC_long.rda"))
  toc()
}

### E. Plot ROC ####
#### 1. Load Data ####
roc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/ROC/", type, "/")
roc.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/ROC/", type, "/")

for(i in 1:length(list.files(roc.data.path))){
  tic()
  roc_file <- list.files(paste0(roc.data.path, list.files(roc.data.path)[i], "/"), pattern = "*ROC_long*")
  load(paste0(roc.data.path, list.files(roc.data.path)[i], "/", roc_file)) # loads as rocAll_long

  stk.chr <- unique(rocAll_long$StockID)
  
  # filter survey coverage data to stock
  scov <- filter(meanrects, StockID == stk.chr)
  # order surveys by coverage for plot legend
  survorder <- arrange(scov, desc(SurvCoverage))$`Survey Index, Survey Name`
  rocAll_long$`Survey Index, Survey Name` <- factor(rocAll_long$`Survey Index, Survey Name`, levels=survorder)
  # colours for surveys (cool = higher survey coverage)
  colrs <- c("blue4", "#00695C", "#8BC34A", "gold2", "#E78100FF", "#F5191CFF", "#9C27B0") # "#FFD320", 
  names(colrs) <- survorder
  
  ##### 4.1 Best Surveys ####
  if(type == "BestSurveys"){
    stkbestsurveys <- filter(bestsurveys, StockID == stk.chr)
    # Filter out surveys that do not have at least 25% coverage
    rocAll_long <- filter(rocAll_long, 
                          StockID %in% stkbestsurveys$StockID,
                          SurveyIndex %in% stkbestsurveys$SurveyIndex,
                          Survey %in% stkbestsurveys$SurveyName,
                          `Survey Index, Survey Name` %in% stkbestsurveys$`Survey Index, Survey Name`)
    if(nrow(rocAll_long) > 0){
    # Remove CoG (add other inds if desired)
    rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                  rocAll_long$`Spatial Indicator` == "CoG (y)",]$FPR <- NaN
    rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                  rocAll_long$`Spatial Indicator` == "CoG (y)",]$TPR <- NaN
    rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                  rocAll_long$`Spatial Indicator` == "CoG (y)",]$TSS <- NaN
    rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                  rocAll_long$`Spatial Indicator` == "CoG (y)",]$AUC <- NaN
    # Keep colours consistent between allsurvey plots and bestsurevy plots
    b <- paste0(stkbestsurveys$SurveyIndex, ", ", stkbestsurveys$SurveyName)
    colrs <- colrs[b]
    }
  }
  if(nrow(rocAll_long) == 0){
    next
  }
  
  #### 2. Optimum Threshold #### 
  # Get the optimum threshold for each survey for each spatial ind
  optthresh <- rocAll_long %>%
    group_by(`Survey Index, Survey Name`, `Spatial Indicator`) %>%
    filter(TSS == max(TSS))
  
  #### 3. Plot ####
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
  
  #### 4. Save ####
  dir.create(paste0(roc.plot.path, stk.chr), recursive = TRUE)
  cowplot::save_plot(plot = roc_plot, filename = paste0(roc.plot.path, stk.chr, "/", "rocPlot-", stk.chr, ".png"), base_height = 8, base_width = 12)
  toc()
}

### F. Plot TSS ####
#### 1. Load Data ####
roc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/ROC/", type, "/")
tss.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/TSS/", type, "/")

for(i in 1:length(list.files(roc.data.path))){
  tic()
  roc_file <- list.files(paste0(roc.data.path, list.files(roc.data.path)[i], "/"), pattern = "*ROC_long*")
  load(paste0(roc.data.path, list.files(roc.data.path)[i], "/", roc_file)) # loads as rocAll_long
  
  stk.chr <- unique(rocAll_long$StockID)
  rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
  
  # filter survey coverage data to stock
  scov <- filter(meanrects, StockID == stk.chr)
  # order surveys by coverage for plot legend
  survorder <- arrange(scov, desc(SurvCoverage))$`Survey Index, Survey Name`
  rocAll_long$`Survey Index, Survey Name` <- factor(rocAll_long$`Survey Index, Survey Name`, levels=survorder)
  # colours for surveys (cool = higher survey coverage)
  colrs <- c("blue4", "#00695C", "#8BC34A", "gold2", "#E78100FF", "#F5191CFF", "#9C27B0") # "#FFD320", 
  names(colrs) <- survorder
  
  if(type == "BestSurveys"){
    stkbestsurveys <- filter(bestsurveys, StockID == stk.chr)
    # Filter out surveys that do not have at least 25% coverage
    rocAll_long <- filter(rocAll_long, 
                          StockID %in% stkbestsurveys$StockID,
                          SurveyIndex %in% stkbestsurveys$SurveyIndex,
                          Survey %in% stkbestsurveys$SurveyName,
                          `Survey Index, Survey Name` %in% stkbestsurveys$`Survey Index, Survey Name`)
    if(nrow(rocAll_long) > 0){
      # Remove CoG (add other inds if desired)
      rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                    rocAll_long$`Spatial Indicator` == "CoG (y)",]$FPR <- NaN
      rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                    rocAll_long$`Spatial Indicator` == "CoG (y)",]$TPR <- NaN
      rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                    rocAll_long$`Spatial Indicator` == "CoG (y)",]$TSS <- NaN
      rocAll_long[rocAll_long$`Spatial Indicator` == "CoG (x)" | 
                    rocAll_long$`Spatial Indicator` == "CoG (y)",]$AUC <- NaN
      # Keep colours consistent between allsurvey plots and bestsurevy plots
      b <- paste0(stkbestsurveys$SurveyIndex, ", ", stkbestsurveys$SurveyName)
      colrs <- colrs[b]
    }
  }
  if(nrow(rocAll_long) == 0){
    next
  }
  
  #### 2. Optimum Threshold #### 
  # Get the optimum threshold for each survey for each spatial ind
  optthresh <- rocAll_long %>%
    group_by(`Survey Index, Survey Name`, `Spatial Indicator`) %>%
    filter(TSS == max(TSS))
  
  #### 3. Plot ####
  tss_plot <- ggplot() +
    geom_line(data = rocAll_long, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`), key_glyph = "rect") +
    geom_point(data = optthresh, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`)) +
    scale_colour_manual(values = colrs) + 
    geom_hline(yintercept = 0) +
    facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder)), scales = "free_x") +
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
  
  #### 4. Save ####
  dir.create(paste0(tss.plot.path, stk.chr), recursive = TRUE)
  cowplot::save_plot(plot = tss_plot, filename = paste0(tss.plot.path, stk.chr, "/", "tssPlot-", stk.chr, ".png"), base_height = 8, base_width = 12)
  toc()
}

### G. Plot AUC ####
#### 1. Load Data ####
roc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/ROC/", type, "/")
auc.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/AUC/", type, "/")

auc_df <- list()
for(i in 1:length(list.files(roc.data.path))){
  roc_file <- list.files(paste0(roc.data.path, list.files(roc.data.path)[i], "/"), pattern = "*ROC_long*")
  load(paste0(roc.data.path, list.files(roc.data.path)[i], "/", roc_file)) # loads as rocAll_long
  stk.chr <- unique(rocAll_long$StockID)
  rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
  auc_df <- rbind(auc_df, rocAll_long)
}

#### 2. Best Surveys ####
if(type == "BestSurveys"){
  # Filter out surveys that do not have at least 25% coverage
  auc_df <- filter(auc_df, 
                   StockID %in% bestsurveys$StockID,
                   SurveyIndex %in% bestsurveys$SurveyIndex,
                   Survey %in% bestsurveys$SurveyName,
                   `Survey Index, Survey Name` %in% bestsurveys$`Survey Index, Survey Name`)
  if(nrow(auc_df) > 0){
    # Remove CoG data (add other inds if desired)
    auc_df[auc_df$`Spatial Indicator` == "CoG (x)" | 
             auc_df$`Spatial Indicator` == "CoG (y)",]$AUC <- NaN
    auc_df[auc_df$`Spatial Indicator` == "CoG (x)" | 
             auc_df$`Spatial Indicator` == "CoG (y)",]$TSS <- NaN
  }
}

#### 2. x-axis labels ####
auc_df$names <- paste(auc_df$StockID, auc_df$`Survey Index, Survey Name`)

##### 2.1 Order stocks by fisheries guild, growth rate, & survey coverage ####
meanrects2 <- merge(meanrects, allstk_spcsinfo[,c("StockID", "k", "FisheriesGuild")], by = "StockID")
meanrects2 <- meanrects2 %>%
  mutate(names = paste(meanrects$StockID, meanrects$`Survey Index, Survey Name`)) %>%
  arrange(FisheriesGuild, k, StockID, desc(SurvCoverage)) %>% # descending survcov within each stock
  select(names, StockID, FisheriesGuild, k, SurvCoverage)
auc_df2 <- merge(auc_df, meanrects2, by = c("names", "StockID"))
auc_df2$names <- factor(auc_df2$names, levels=meanrects2$names) # this reorders the plotting of bars

scale <- paletteer_c("grDevices::Zissou 1", n = 30)
nbenth <- length(unique(auc_df2$StockID[auc_df2$FisheriesGuild == "Benthic"]))
ndem <- length(unique(auc_df2$StockID[auc_df2$FisheriesGuild == "Demersal"]))

#colrsbenth <- scale[seq(1, nbenth*3)][c(rep(FALSE, 2), TRUE)]
#colrsdem <- rev(rev(scale)[seq(1, ndem*3)])[c(rep(FALSE, 2), TRUE)]
#colrs <- c("#584B9F","#0086B3", "#0BC0B3", "lightblue", "#A9E9AD",  "#FDE38D", "#F8B84E", "#E97302", "#B72E48")
colrs <- c("#584B9F","#0086B3", "#0BC0B3", "lightblue", "#A9E9AD","#B72E48", "#E97302", "#F8B84E", "#FDE38D")
#colrs <- c(colrsbenth, colrsdem)
names(colrs) <- unique(meanrects2$StockID)

if(type == "BestSurveys"){
  bestsurveys2 <- merge(bestsurveys, allstk_spcsinfo[,c("StockID", "k", "FisheriesGuild")], by = "StockID")
  meanrects2 <- bestsurveys2 %>%
    mutate(names = paste(bestsurveys2$StockID, bestsurveys2$`Survey Index, Survey Name`)) %>%
    arrange(FisheriesGuild, k, StockID, desc(SurvCoverage)) %>% # descending survcov within each stock
    select(names, StockID, FisheriesGuild, k, SurvCoverage)
  auc_df2 <- merge(auc_df, meanrects2, by = c("names", "StockID"))
  auc_df2$names <- factor(auc_df2$names, levels=meanrects2$names) # this reorders the plotting of bars
  colrs <- colrs[c(unique(as.character(arrange(auc_df2, FisheriesGuild, k, StockID, desc(SurvCoverage))$StockID)))]
}

# Here we get the names of the surveys in the correct order but without the stock appended to it
# which is what would happen if we just used `names`
# we have to use `names` because some stocks have identical `Survey Index, Survey Name`s
lablist <- list()
for(i in 1:length(unique(auc_df2$names))){
  namesorder <- unique(as.character(arrange(auc_df2, FisheriesGuild, k, StockID, desc(SurvCoverage))$names))
  splitname <- strsplit(namesorder, " ")[[i]] # split name into segments
  remID <- strsplit(namesorder, " ")[[i]][2:length(splitname)] # remove stock ID from name
  lab <- paste(remID, collapse = " ") # concat back together
  lablist[i] <- lab
}
labels <- unlist(lablist)

# NOTE: Must check that bars match up with correct labels
# Check these values against the plot
auc_df2 %>% 
  select(names, `Spatial Indicator`, AUC, SurvCoverage) %>%
  group_by(names, `Spatial Indicator`) %>% 
  distinct() %>%
  filter(`Spatial Indicator` == "Positive Area (Rectangle)")

# Add proportion of AUC > 0.75 per indicator
Nsurvs <- length(unique(as.character(auc_df2$names[!is.na(auc_df2$AUC)]))) # where AUC is available. 
# Where AUC isnt available means ROC could not be conducted and should not be included when calcing the porportion
# as.cchar removes factoring
aucprops <- auc_df2 %>% 
  select(`Spatial Indicator`, AUC) %>%
  na.omit() %>%
  group_by(`Spatial Indicator`) %>% 
  distinct() %>%
  mutate(prop75auc = paste0(round(length(AUC[AUC>=0.75])/Nsurvs*100,1), "%"),
         prop50auc = paste0(round(length(AUC[AUC>0.5])/Nsurvs*100,1), "% > 0.5")) %>%
  select(-AUC) %>%
  distinct()

#### 3. Plot AUC ####
AUC_plot <- ggplot(data = auc_df2) +
  geom_col(data = auc_df2, aes(x = names, y = AUC, fill = StockID), position = position_dodge(width = 2), width = 0.8) +
  scale_fill_manual(breaks = names(colrs), values = colrs) + 
  geom_text(data = aucprops, aes(x = length(unique(auc_df2$names))-2, y = 1, label = c(aucprops$prop75auc)), size = 3.3) +
  #geom_text(data = props, aes(x = length(unique(auc_df2$names))-5, y = 0.9, label = c(props$prop50auc)), size = 3.3) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0.5, colour = "grey20", lty = 1) +
  geom_hline(yintercept = 0.75, colour = "grey20", lty = 2) +
  
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder))) +
  xlab("Survey Index, Survey Name") +
  ylab("Area Under ROC Curve") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black"),
        # Legend
        legend.position = "right")

#### 4. Save AUC Plot #### 
dir.create(paste0(auc.plot.path, "allstocks"), recursive = TRUE)
cowplot::save_plot(plot = AUC_plot, filename = paste0(auc.plot.path, "allstocks/aucPlot.png"), base_height = 8, base_width = 12)

### H. Plot TSS Summary ####
roc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/ROC/", type, "/")
tssSum.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/TSS_Sum/", type, "/")

#### 1. Max TSS ####
tssSum_df <- list()
for(i in 1:length(list.files(roc.data.path))){
  roc_file <- list.files(paste0(roc.data.path, list.files(roc.data.path)[i], "/"), pattern = "*ROC_long*")
  load(paste0(roc.data.path, list.files(roc.data.path)[i], "/", roc_file)) # loads as rocAll_long
  stk.chr <- unique(rocAll_long$StockID)
  rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
  tssSum_df <- rbind(tssSum_df, rocAll_long)
}

#### 2. Best Surveys ####
if(type == "BestSurveys"){
  # Filter out surveys that do not have at least 25% coverage
  tssSum_df <- filter(tssSum_df, 
                   StockID %in% bestsurveys$StockID,
                   SurveyIndex %in% bestsurveys$SurveyIndex,
                   Survey %in% bestsurveys$SurveyName,
                   `Survey Index, Survey Name` %in% bestsurveys$`Survey Index, Survey Name`)
  if(nrow(tssSum_df) > 0){
    # Remove CoG data (add other inds if desired)
    tssSum_df[tssSum_df$`Spatial Indicator` == "CoG (x)" | 
                tssSum_df$`Spatial Indicator` == "CoG (y)",]$AUC <- NaN
    tssSum_df[tssSum_df$`Spatial Indicator` == "CoG (x)" | 
                tssSum_df$`Spatial Indicator` == "CoG (y)",]$TSS <- NaN
    # Keep colours consistent between allsurvey plots and bestsurvey plots
    #b <- paste0(bestsurveys$SurveyIndex, ", ", bestsurveys$SurveyName)
    #colrs <- colrs[b]
  }
}

#### 2. x-axis labels ####
tssSum_df$names <- paste(tssSum_df$StockID, tssSum_df$`Survey Index, Survey Name`)
##### 2.1 Order by survey coverage ####
if(type == "AllSurveys"){
  meanrects2 <- meanrects %>%
    mutate(names = paste(meanrects$StockID, meanrects$`Survey Index, Survey Name`)) %>%
    arrange(StockID, desc(SurvCoverage)) %>% # descending survcov within each stock
    select(names, SurvCoverage)
  tssSum_df2 <- merge(tssSum_df, meanrects2, by = "names")
  tssSum_df2$names <- factor(tssSum_df2$names, levels=meanrects2$names) # this reorders the plotting of bars
}
if(type == "BestSurveys"){
  meanrects2 <- bestsurveys %>%
    mutate(names = paste(bestsurveys$StockID, bestsurveys$`Survey Index, Survey Name`)) %>%
    arrange(StockID, desc(SurvCoverage)) %>% # descending survcov within each stock
    select(names, SurvCoverage)
  tssSum_df2 <- merge(tssSum_df, meanrects2, by = "names")
  tssSum_df2$names <- factor(tssSum_df2$names, levels=meanrects2$names) # this reorders the plotting of bars
}

##### 2.1 Order stocks by fisheries guild, growth rate, & survey coverage ####
meanrects2 <- merge(meanrects, allstk_spcsinfo[,c("StockID", "k", "FisheriesGuild")], by = "StockID")
meanrects2 <- meanrects2 %>%
  mutate(names = paste(meanrects$StockID, meanrects$`Survey Index, Survey Name`)) %>%
  arrange(FisheriesGuild, k, StockID, desc(SurvCoverage)) %>% # descending survcov within each stock
  select(names, StockID, FisheriesGuild, k, SurvCoverage)
tssSum_df2 <- merge(tssSum_df, meanrects2, by = c("names", "StockID"))
tssSum_df2$names <- factor(tssSum_df2$names, levels=meanrects2$names) # this reorders the plotting of bars

scale <- paletteer_c("grDevices::Zissou 1", n = 30)
nbenth <- length(unique(tssSum_df2$StockID[tssSum_df2$FisheriesGuild == "Benthic"]))
ndem <- length(unique(tssSum_df2$StockID[tssSum_df2$FisheriesGuild == "Demersal"]))

#colrsbenth <- scale[seq(1, nbenth*3)][c(rep(FALSE, 2), TRUE)]
#colrsdem <- rev(rev(scale)[seq(1, ndem*3)])[c(rep(FALSE, 2), TRUE)]
#colrs <- c("#584B9F","#0086B3", "#0BC0B3", "lightblue", "#A9E9AD",  "#FDE38D", "#F8B84E", "#E97302", "#B72E48")
colrs <- c("#584B9F","#0086B3", "#0BC0B3", "lightblue", "#A9E9AD","#B72E48", "#E97302", "#F8B84E", "#FDE38D")
#colrs <- c(colrsbenth, colrsdem)
names(colrs) <- unique(meanrects2$StockID)

if(type == "BestSurveys"){
  bestsurveys2 <- merge(bestsurveys, allstk_spcsinfo[,c("StockID", "k", "FisheriesGuild")], by = "StockID")
  meanrects2 <- bestsurveys2 %>%
    mutate(names = paste(bestsurveys2$StockID, bestsurveys2$`Survey Index, Survey Name`)) %>%
    arrange(FisheriesGuild, k, StockID, desc(SurvCoverage)) %>% # descending survcov within each stock
    select(names, StockID, FisheriesGuild, k, SurvCoverage)
  tssSum_df2 <- merge(tssSum_df, meanrects2, by = c("names", "StockID"))
  tssSum_df2$names <- factor(tssSum_df2$names, levels=meanrects2$names) # this reorders the plotting of bars
  colrs <- colrs[c(unique(as.character(arrange(tssSum_df2, FisheriesGuild, k, StockID, desc(SurvCoverage))$StockID)))]
}

# Here we get the names of the surveys in the correct order but without the stock appended to it
# which is what would happen if we just used `names`
# we have to use `names` because some stocks have identical `Survey Index, Survey Name`s
lablist <- list()
for(i in 1:length(unique(tssSum_df2$names))){
  namesorder <- unique(as.character(arrange(tssSum_df2, FisheriesGuild, k, StockID, desc(SurvCoverage))$names))
  splitname <- strsplit(namesorder, " ")[[i]] # split name into segments
  remID <- strsplit(namesorder, " ")[[i]][2:length(splitname)] # remove stock ID from name
  lab <- paste(remID, collapse = " ") # concat back together
  lablist[i] <- lab
}
labels <- unlist(lablist)

# NOTE: Must check that bars match up with correct labels
# Check these values against the plot
tssSum_df2 %>% 
  select(names, `Spatial Indicator`, AUC, SurvCoverage) %>%
  group_by(names, `Spatial Indicator`) %>% 
  distinct() %>%
  filter(`Spatial Indicator` == "Positive Area (Rectangle)")
# Find the max TSS
tssSum_df3 <- tssSum_df2 %>%
  select(StockID, `Survey Index, Survey Name`, `Spatial Indicator`, names, TSS) %>%
  group_by(StockID, `Survey Index, Survey Name`, `Spatial Indicator`, names) %>%
  summarise(maxTSS = max(TSS))

# Add proportion of TSS > 0.5 per indicator
Nsurvs <- length(unique(as.character(tssSum_df2$names[!is.na(tssSum_df2$AUC)]))) # where AUC is available. 
# Where AUC isnt available means ROC could not be conducted and should not be included when calcing the porportion
# as.cchar removes factoring
tssprops <- tssSum_df3 %>% 
  select(`Spatial Indicator`, maxTSS) %>%
  na.omit() %>%
  group_by(`Spatial Indicator`) %>% 
  distinct() %>%
  mutate(prop05tss = paste0(round(length(maxTSS[maxTSS>=0.5])/Nsurvs*100,1), "%"),
         prop00tss = paste0(round(length(maxTSS[maxTSS>0])/Nsurvs*100,1), "% > 0")) %>%
  select(-maxTSS) %>%
  distinct()

#### 3. Plot TSS Summary ####
tssSum_plot <- ggplot() +
  geom_col(data = tssSum_df3, aes(x = names, y = maxTSS, fill = StockID), position = position_dodge(width = 2), width = 0.8) +
  geom_text(data = tssprops, aes(x = length(unique(tssSum_df2$names))-2, y = 1, label = c(tssprops$prop05tss)), size = 3.3) +
  scale_fill_manual(breaks = names(colrs), values = colrs) + 
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = c(0, 0.5), colour = "grey20", lty = 2) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder))) +
  xlab("Survey Index, Survey Name") +
  ylab("True Skill Score of Optimised Threshold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black"),
        # Legend
        legend.position = "right")

#### 4. Save TSS Summary Plot #### 
dir.create(paste0(tssSum.plot.path, "allstocks"), recursive = TRUE)
cowplot::save_plot(plot = tssSum_plot, filename = paste0(tssSum.plot.path, "allstocks/tssSumPlot.png"), base_height = 8, base_width = 12)

#### 5. Alternative TSS Summary Plots ####
tssSum_df4 <- tssSum_df2 %>%
  group_by(StockID, `Survey Index, Survey Name`, `Spatial Indicator`, names) %>%
  summarise(y0 = min(TSS),
            y25 = quantile(TSS, 0.25, na.rm = T),
            y50 = median(TSS),
            y75 = quantile(TSS, 0.75, na.rm = T),
            y100 = max(TSS))

tssSum_plot2 <- ggplot() +
  geom_errorbar(data = tssSum_df4, aes(x = names, ymin = y0, ymax = y100), colour = "black", width = 0.5, key_glyph = "rect") +
  geom_boxplot(data = tssSum_df4, aes(x = names, ymin = y0, ymax = y100, lower = y25, upper = y75, middle = y50, fill = StockID),colour = "grey20", stat="identity", key_glyph = "rect") +
  scale_fill_manual(breaks = names(colrs), values = colrs) + 
  geom_text(data = tssprops, aes(x = length(unique(tssSum_df2$names))-2, y = 1, label = c(tssprops$prop05tss)), size = 3.3) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder))) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0, colour = "grey20", lty = 1) +
  geom_hline(yintercept = 0.5, colour = "grey20", lty = 2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  xlab("Survey Index, Survey Name") +
  ylab("True Skill Score (TSS)") +
  # Theme
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
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
  guides(fill = guide_legend(byrow = TRUE, title = "Stock ID")) # to enable vertical spacing between key glyphs
dir.create(paste0(tssSum.plot.path, "allstocks"), recursive = TRUE)
cowplot::save_plot(plot = tssSum_plot2, filename = paste0(tssSum.plot.path, "allstocks/tssSumPlot2.png"), base_height = 8, base_width = 12)

tssSum_plot3 <- ggplot() +
  geom_errorbar(data = tssSum_df4, aes(x = names, ymin = y0, ymax = y100, colour = StockID), width = 0.9, key_glyph = "rect", linewidth = 1) +
  scale_colour_manual(breaks = names(colrs), values = colrs) + 
  geom_text(data = tssprops, aes(x = length(unique(tssSum_df2$names))-2, y = 1, label = c(tssprops$prop05tss)), size = 3.3) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder))) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0, colour = "grey20", lty = 1) +
  geom_hline(yintercept = 0.5, colour = "grey20", lty = 2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  xlab("Survey Index, Survey Name") +
  ylab("True Skill Score (TSS)") +
  # Theme
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
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
  guides(colour = guide_legend(byrow = TRUE, title = "Stock ID")) # to enable vertical spacing between key glyphs
dir.create(paste0(tssSum.plot.path, "allstocks"), recursive = TRUE)
cowplot::save_plot(plot = tssSum_plot3, filename = paste0(tssSum.plot.path, "allstocks/tssSumPlot3.png"), base_height = 8, base_width = 12)

### I. Plot Spatial Indicators with Optimal Threshold ####
#### 1. Load and Prepare Data ####
# Get all SDI data and convert into long format
si.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/SpatInd/", type, "/")
si.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Plots/SpatInd/", type, "/")

##### 1.2 Add Optimum Threshold ####
# Load data from section H
# Get the spat ind value where TSS was maximised 
optthreshdata <- tssSum_df %>%
  select(StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, `Spatial Indicator`, `Spatial Indicator Value`, TSS, FPR) %>%
  group_by(StockID, `Survey Index, Survey Name`, `Spatial Indicator`) %>%
  mutate(maxTSS = if_else(TSS == max(TSS), TRUE, FALSE), TNR = 1-FPR) %>%
  filter(maxTSS == TRUE) %>%
  distinct()

# Are there multiple spatial indictaor values that score the same TSS?
if(any(duplicated(optthreshdata[c(1:5)]))){
  message("Multiple thresholds maximising TSS. Selecting threshold that maxmises TNR")
  # get row numbers
  duprowno <- unique(sort(c(which(duplicated(optthreshdata[c(1:5)])), which(duplicated(optthreshdata[c(1:5)]))-1)))
  # if there are mutliple thresholds that maximise classifcation
  # we want to take the threshold that gets the negatives correct
  # It is more important to identify a stock as unhealthy than it is healthy
  optthreshdata[duprowno,]
  pess <- optthreshdata[duprowno,] %>%
    group_by(StockID, `Survey Index, Survey Name`, `Spatial Indicator`) %>%
    mutate(maxTNR = if_else(TNR == max(TNR), TRUE, FALSE)) %>%
    filter(maxTNR == TRUE) %>%
    distinct()
  # check to see if there are still duplicates 
  if(any(duplicated(pess[c(1:5)]))){
    message("Still duplciates after first clean. Selecting first instances")
  duprowno2 <- unique(sort(c(which(duplicated(pess[c(1:5)])), which(duplicated(pess[c(1:5)]))-1)))
  pess[duprowno2,]
  pess2 <- pess[duprowno2,] %>%
    group_by(StockID, `Survey Index, Survey Name`, `Spatial Indicator`) %>%
    mutate(First = if_else(`Spatial Indicator Value` == min(`Spatial Indicator Value`), TRUE, FALSE)) %>%
    filter(First == TRUE) %>%
    distinct()
  any(duplicated(pess2[c(1:5)]))
  pess2
  # remove duplicate rows from pess
  pess <- anti_join(pess, pess[duprowno2,])
  # join back rows with duplicates removed
  pess <- rbind(pess, pess2)
  }
  # remove duplicated rows from optthreshdata  
  optthreshdata2 <- anti_join(optthreshdata, optthreshdata[duprowno,])
  # join back the rows which maximise TNR as well as TSS
  optthreshdata3 <- rbind(optthreshdata2, pess)
  # check if duplicates still exist
  if(any(duplicated(optthreshdata3[c(1:5)]))){
    message("Still some duplicates. Look at `pess3` data and resolve manually")
  duprowno3 <- unique(sort(c(which(duplicated(optthreshdata3[c(1:5)])), which(duplicated(optthreshdata3[c(1:5)]))-1)))
  pess3 <- optthreshdata3[duprowno3,]} else{message("All duplicates removed")}
}
optthreshdata <- optthreshdata3 %>% 
  select(-maxTNR, -TNR, -maxTSS, -FPR) %>%
  rename(OptThresh = `Spatial Indicator Value`, maxTSS = TSS)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Merge with spat ind data and divde spat inds by the optimal threshold
# Now everything above 1 = healthy, below 1 = unhealthy
sdiall_stndrd <- left_join(sdiall_long, optthreshdata, by = c("StockID", "SurveyIndex", "Survey", "Survey Index, Survey Name", "Spatial Indicator"))
sdiall_stndrd$`Spatial Indicator Value/OptThresh` <- sdiall_stndrd$`Spatial Indicator Value`/sdiall_stndrd$OptThresh

#### 2. Load Stock Objects ####
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock Objects/2022/")
# Put into a list to iterate through later
for(stockfile in list.files(stockobj.path)){load(paste0(stockobj.path, stockfile))}
stocklist <- list(cod.27.47d20_nov, had.27.46a20, ple.27.420, ple.27.7d, 
                  pok.27.3a46, sol.27.4, tur.27.4, whg.27.47d, wit.27.3a47d) # ignore so.27.7d for now, need to find YFS survey data
# Order here must match order in stocklist
stocklist.chr <- list("cod.27.47d20_nov", "had.27.46a20", "ple.27.420", "ple.27.7d", 
                      "pok.27.3a46", "sol.27.4", "tur.27.4", "whg.27.47d", "wit.27.3a47d") # ignore so.27.7d for now, need to find YFS survey data

#### 3. Plot ####
for(i in 1:length(stocklist)){
  tic()
  ##### 3.1 Filter to Stock ####
  # Select the stock to analyse
  stk <- stocklist[[i]]
  stk.chr <- stocklist.chr[[i]]
  message(paste0("\n",
                 paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = ""),"\n# ", 
                 stk.chr, " #", "\n",
                 paste0(c(rep("#", times = stringr::str_count(stk.chr)+4)), collapse = "")))
  
  writeLines("Filter SI data to stock")
  sdistk_long <- filter(sdiall_long, StockID == stk.chr) 
  sdistk_wide <- filter(sdiall_wide, StockID == stk.chr)
  sdistk_stndrd <- filter(sdiall_stndrd, StockID == stk.chr)
  
  # filter survey coverage data to stock
  scov <- filter(meanrects, StockID == stk.chr)
  # order surveys by coverage for plot legend
  survorder <- arrange(scov, desc(SurvCoverage))$`Survey Index, Survey Name`
  sdistk_stndrd$`Survey Index, Survey Name` <- factor(sdistk_stndrd$`Survey Index, Survey Name`, levels=survorder)
  #rocAll_long$`Survey Index, Survey Name` <- factor(rocAll_long$`Survey Index, Survey Name`, levels=survorder)
  # colours for surveys (cool = higher survey coverage)
  colrs <- c("blue4", "#00695C", "#8BC34A", "gold2", "#E78100FF", "#F5191CFF", "#9C27B0") # "#FFD320", 
  names(colrs) <- survorder
  # order surveys by coverage
  #sdistk_stndrd$`Survey Index, Survey Name` <- factor(sdistk_stndrd$`Survey Index, Survey Name`, levels=survorder)
  
  ####3.3 Best Surveys ####
  if(type == "BestSurveys"){
    stkbestsurveys <- filter(bestsurveys, StockID == stk.chr)
    # Filter out surveys that do not have at least 25% coverage
    sdistk_stndrd <- filter(sdistk_stndrd, 
                            StockID %in% stkbestsurveys$StockID,
                            SurveyIndex %in% stkbestsurveys$SurveyIndex,
                            Survey %in% stkbestsurveys$SurveyName,
                            `Survey Index, Survey Name` %in% stkbestsurveys$`Survey Index, Survey Name`)
    # Remove CoG (add other inds if desired)
    sdistk_stndrd[sdistk_stndrd$`Spatial Indicator` == "CoG (x)" | 
                    sdistk_stndrd$`Spatial Indicator` == "CoG (y)",]$`Spatial Indicator Value` <- NaN
    ## 10.0 Remove Inds < 0.5 TSS ####
    sdistk_stndrd[sdistk_stndrd$maxTSS < 0.5 & !is.na(sdistk_stndrd$maxTSS),]$`Spatial Indicator Value/OptThresh` <- NaN
    # Keep colours consistent between allsurvey plots and bestsurevy plots
    b <- paste0(stkbestsurveys$SurveyIndex, ", ", stkbestsurveys$SurveyName)
    colrs <- colrs[b]
  }
  if(nrow(sdistk_stndrd) == 0){
    next
  }
  
  # Filter years so that the years in the spatind plot do not predate the ssb plot
  sdistk_stndrd <- sdistk_stndrd %>% filter(Year %in% stkssb$Year)
  
  #### 3.2 Get SSB/MSY Btrigger ####
  writeLines("Get SSB for survey years")
  
  strtyr <- min(sdistk_stndrd$Year)
  if(strtyr < range(stk)["minyear"]){
    warning("First year of survey data provided preceeds first year of data in the stock object. Using minyear of the stock object instead.", immediate. = TRUE)
    strtyr <- range(stk)["minyear"]}
  
  endyr <- max(sdistk_stndrd$Year)
  if(endyr > range(stk)["maxyear"]){
    warning("Last year of survey data provided exceeds available last year of data in the stock object. Using maxyear of the stock object instead.", immediate. = TRUE)
    endyr <- range(stk)["maxyear"]}  
  
  writeLines("Add SSB to dataframe")
  msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
  stkssb <- as.data.frame(ssb(stk)[,ac(strtyr:endyr)])[c("year", "data")] %>%
    rename(Year = year, SSB = data) %>%
    mutate(type = "SSB")
  stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt
  
  #### 3.2 Plot SSB/MSY Btrigger ####
  writeLines("Plot SSB")
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
  
  #### 3.3 Plot SIs with OptThresh####
  writeLines("Plot Spatial Indicators with Optimal Threshold")
  optthresh_plot2 <- ggplot() + 
    geom_line(data = sdistk_stndrd, aes(x = Year, y = `Spatial Indicator Value/OptThresh`, colour = `Survey Index, Survey Name`), key_glyph = "rect") + 
    scale_colour_manual(values = colrs) + 
    geom_hline(yintercept = 1, colour = "grey20", lty = 2) +
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
  
  #### 3.4 Overlay SSB & SDI ####
  writeLines("Plot layout")
  optthresh_plot <- ggdraw()+ draw_plot(optthresh_plot2)+ draw_plot(ssb_plot, x= .76, y= 0.2977, width=.206, height = .206)
  #### 3.5 Save ####
  writeLines("Save")
  dir.create(paste0(si.plot.path, stk.chr))
  cowplot::save_plot(plot = optthresh_plot, filename = paste0(si.plot.path, stk.chr, "/OptSpatIndPlot-", stk.chr, ".png"), base_height = 25, base_width = 12)
  toc()
}

### J. Best Surveys ####
#> the 'best' surveys are considered those which cover most of the stock boundary
#> first we need to objectively identify which ones this is
#> then we will remove the surveys which only cover a small percentage of the stock boundary
#> we will also remove surveys which have no stock status contrast in the those survey years
#> if there is no contrast, the ROC curve cannot be calculated and this analysis is void

#### 1. Stock Boundary Area ####
meanrects
#> this shows the mean percentage of rectangles in the stock boundary surveyed 
#> across the years of data used in the stock assessment 
#> Now we need to use this to filter the surveys out that are not 
#> representative of the whole stock boundary
#> E.g. those with survey coverage of less than 25%
bestsurveys <- meanrects[meanrects$SurvCoverage >= 25,]
bestsurveys

#### 2. Surveys without contrast ####
#> these are surveys where the ROC could not be computed 
#> and therefore the classification ability could not tested using this survye data
#> data does not show up on full plots, but the survey will still be in the legend
#> lets remove these surveys
#> here i identify the surveys where AUC == NaN, indictaing ROC could not calculated
roc.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/ROC/", type, "/")

d <- list()
for(i in 1:length(list.files(roc.data.path))){
  roc_file <- list.files(paste0(roc.data.path, list.files(roc.data.path)[i], "/"), pattern = "*ROC_long*")
  load(paste0(roc.data.path, list.files(roc.data.path)[i], "/", roc_file)) # loads as rocAll_long
  stk.chr <- unique(rocAll_long$StockID)
  rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
  d <- rbind(d, rocAll_long)
}

t <- d %>% filter(AUC == "NaN")
setnames(t, "Survey", "SurveyName")

# remove factoring
t$`Survey Index, Survey Name` <- as.character(t$`Survey Index, Survey Name`)
# find survey(s) with no contrast
nocontrast <- unique(t[, 3:6])
# remove from bests surveys
bestsurveys <- anti_join(bestsurveys, nocontrast, by = c("StockID", "SurveyIndex", "SurveyName", "Survey Index, Survey Name"))
# check removal
nocontrast %in% bestsurveys[c("StockID", "SurveyIndex", "SurveyName", "Survey Index, Survey Name")]

# Did we lose any stocks completely?
writeLines(paste0("Stocks lost: ", length(unique(meanrects$StockID)) - length(unique(bestsurveys$StockID))))
writeLines(setdiff(unique(meanrects$StockID), unique(bestsurveys$StockID)))

type <- "BestSurveys" # this changes where the filtered analysis is saved so analysis with allsurveys is not overwritten
# run from section C

# NOTE: Analysis must be run with all surveys first
