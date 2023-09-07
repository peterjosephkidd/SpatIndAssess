#> Calculate Spatial Indicators
#> Loop through all stocks at once

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

### B. Calculate Spatial Indicators ####
for(i in 1:length(stocklist)){
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
  # Output path to save data and plots
  # Keeping this to my one drive, files may be too large for GitHub
  
  ##### 2.1 Start Loop ####
  for(indx in 1:length(stk_data_filtered)){
    for(survey in 1:length(stk_data_filtered[[indx]])){
      si.data.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/10Stock_Outputs/"
      
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
      writeLines(noquote("Centre of Gravity (x and y)"))
      writeLines(noquote("Inertia"))
      writeLines(noquote("95% CI Ellipse"))
      
      cog <- coginert(
        hlhh = hlhh,
        yrs = yrs,
        qrs = qrs,
        species_aphia = species_aphia,
        stk_divs = stk_divs)
      
      ##### 2.9 Convex Hull Area ####
      writeLines(noquote("Convex Hull Area"))
      cha <- chullarea(
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
      ##### 3.1 Save Data ####
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
}

### C. Plot Spatial Indicators ####
#### 1. Load and Prepare Data ####
# Get all SDI data and convert into long format
si.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/10Stock_Outputs/")
si.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/SpatIndPlots/")

sdi_longlist <- list()
sdi_widelist <- list()
i <- 1

for(StockFolder in stocklist.chr){
  message(paste0("Stock: ", StockFolder))
  for(IndexFolder in list.files(paste0(si.data.path, StockFolder))){
    message(paste0("Survey Index: ", IndexFolder))
    for(SurveyFolder in list.files(paste0(si.data.path, StockFolder, "/", IndexFolder, "/"))){
      print(noquote(paste0("Survey: ", SurveyFolder)))
      #Load  Data
      si_file <- list.files(paste0(si.data.path, StockFolder, "/", IndexFolder, "/", SurveyFolder, "/SpatIndData/"), pattern = "*SpatIndData*")
      load(paste0(si.data.path, StockFolder, "/", IndexFolder, "/", SurveyFolder, "/SpatIndData/", si_file))
      si$Inertia <- si$Inertia/1000000
      colnames(si)[colnames(si)=="Inertia"] <- "Inertia (million)"
      sdi_widelist[[i]] <- si
      #  Convert wide to long 
      si_long <- si %>% tidyr::pivot_longer(cols = sort(c("Gini Index", "D95", "Positive Area (Rectangle)", "Positive Area (Haul)", "SPI", "Spreading Area", "Equivalent Area",
                                                     "CoG (x)","CoG (y)", "Inertia (million)", "Ellipse Area", "Convex Hull Area")), 
                                            names_to = "Spatial Indicator",
                                            values_to = "Spatial Indicator Value")
      si_long$`Spatial Indicator` <- factor(si_long$`Spatial Indicator`, levels = sort(c("Gini Index", "D95", "Positive Area (Haul)", "Positive Area (Rectangle)", "SPI", 
                                                                                    "Spreading Area", "Equivalent Area", "CoG (x)","CoG (y)", "Inertia (million)", 
                                                                                    "Ellipse Area", "Convex Hull Area"))) # factor & relocate
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

#### 3. Load Stock Objects ####
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock Objects/2022/")
for(stockfile in list.files(stockobj.path)){load(paste0(stockobj.path, stockfile))}
stocklist <- list(cod.27.47d20_nov, had.27.46a20, ple.27.420, ple.27.7d, 
                  pok.27.3a46, sol.27.4, tur.27.4, whg.27.47d, wit.27.3a47d) # ignore so.27.7d for now, need to find YFS survey data
stocklist.chr <- list("cod.27.47d20_nov", "had.27.46a20", "ple.27.420", "ple.27.7d", 
                      "pok.27.3a46", "sol.27.4", "tur.27.4", "whg.27.47d", "wit.27.3a47d") # ignore so.27.7d for now, need to find YFS survey data

for(i in 1:length(stocklist)){
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
  
  # Get SSB for survey years
  writeLines("Get SSB for survey years")
  
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
    mutate(type = "SSB")
  
  # Plot SSB
  writeLines("Plot SSB")
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
  
  # Plot SDIs
  writeLines("Plot Spatial Indicators")
  sdi_plot1 <- ggplot() + 
    geom_line(data = sdistk_long, aes(x = Year, y = `Spatial Indicator Value`, colour = `Survey Index, Survey Name`), key_glyph = "rect") + 
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
  cowplot::save_plot(plot = sdissb_plot, filename = paste0(si.plot.path, "SpatIndPlot-", stk.chr, ".png"), base_height = 25, base_width = 12)
}


### D. Calculate ROC and TSS ####
# Run C.1. and C.2. to get all spatial indicator data in both long and wide format
roc.data.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Data/"

#### 1. Load Stock Objects ####
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock Objects/2022/")
for(stockfile in list.files(stockobj.path)){load(paste0(stockobj.path, stockfile))}
stocklist <- list(cod.27.47d20_nov, had.27.46a20, ple.27.420, ple.27.7d, 
                  pok.27.3a46, sol.27.4, tur.27.4, whg.27.47d, wit.27.3a47d) # ignore so.27.7d for now, need to find YFS survey data
stocklist.chr <- list("cod.27.47d20_nov", "had.27.46a20", "ple.27.420", "ple.27.7d", 
                      "pok.27.3a46", "sol.27.4", "tur.27.4", "whg.27.47d", "wit.27.3a47d") # ignore so.27.7d for now, need to find YFS survey data

#### 2. Get stock specific data ####
for(i in 1:length(stocklist)){
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
            "SPI", "Spreading Area", "Equivalent Area", "CoG (x)", "CoG (y)", "Inertia (million)",
            "Ellipse Area", "Convex Hull Area")
  roc_longlist <- list()
  #### 3.2 For each survey... ####
  for(i in 1:length(unique(sdistk_wide$`Survey Index, Survey Name`))){
    surveyROC <- sdistk_wide %>% filter(`Survey Index, Survey Name` == unique(sdistk_wide$`Survey Index, Survey Name`)[i])
    ##### 3.2.1 Compute data ####
    roc_long <- roc_fun3(surveyROC, state, inds, return = 'long')
    if(all(unique(roc_long$`Spatial Indicator`) != inds)){warning("Not all spatial indicators ended up in the ROC output df", immediate. = T)}
    roc_longlist[[i]] <- roc_long
  }
  #### 3.3 Merge lists ####
  rocAll_long <- do.call(rbind, roc_longlist)
  #### 3.4 Save Data ####
  save(rocAll_long, file = paste0(roc.data.path, stk.chr, " - ROC_long.rda"))
}

### E. Plot ROC ####
#### 1. Load Data ####
roc.data.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Data/"
roc.plot.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/ROC_plot/"

for(i in 1:length(list.files(roc.data.path))){
  load(paste0(roc.data.path, list.files(roc.data.path)[i])) # loads as rocAll_long
  stk.chr <- unique(rocAll_long$StockID)
  
  #### 2. Optimum Threshold #### 
  # Get the optimum threshold for each survey for each spatial ind
  optthresh <- rocAll_long %>%
    group_by(`Survey Index, Survey Name`, `Spatial Indicator`) %>%
    filter(TSS == max(TSS))
  
  #### 3. Plot ####
  roc_plot <- ggplot() +
    geom_path(data = rocAll_long, aes(x = FPR, y = TPR, colour = `Survey Index, Survey Name`), key_glyph = "rect") +
    geom_point(data = optthresh, aes(x = FPR, y = TPR, colour = `Survey Index, Survey Name`)) +
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
  
  #### 4. Save ####
  cowplot::save_plot(plot = roc_plot, filename = paste0(roc.plot.path, "rocPlot-", stk.chr, ".png"), base_height = 8, base_width = 12)
}

### F. Plot TSS ####
#### 1. Load Data ####
roc.data.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Data/"
tss.plot.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/TSS_plot/"

for(i in 1:length(list.files(roc.data.path))){
  load(paste0(roc.data.path, list.files(roc.data.path)[i])) # loads as rocAll_long
  stk.chr <- unique(rocAll_long$StockID)
  rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
  #### 2. Optimum Threshold #### 
  # Get the optimum threshold for each survey for each spatial ind
  optthresh <- rocAll_long %>%
    group_by(`Survey Index, Survey Name`, `Spatial Indicator`) %>%
    filter(TSS == max(TSS))
  
  #### 3. Plot ####
  tss_plot <- ggplot() +
    geom_line(data = rocAll_long, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`), key_glyph = "rect") +
    geom_point(data = optthresh, aes(x = `Spatial Indicator Value`, y = TSS, colour = `Survey Index, Survey Name`)) +
    geom_abline(slope = c(0,0)) +
    facet_wrap(vars(`Spatial Indicator`), scales = "free_x") +
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
  
  #### 4. Save ####
  cowplot::save_plot(plot = tss_plot, filename = paste0(tss.plot.path, "tssPlot-", stk.chr, ".png"), base_height = 8, base_width = 12)
}

### G. Plot AUC ####
#### 1. Load Data ####
roc.data.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Data/"
auc.plot.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/AUC_plot/"

columns <- c("StockID", "Survey Index, Survey Name", "Spatial Indicator", "AUC")
auc_df <- data.frame(data.frame(matrix(nrow = 1, ncol = length(columns))))
colnames(auc_df) <- columns 
for(i in 1:length(list.files(roc.data.path))){
  load(paste0(roc.data.path, list.files(roc.data.path)[i])) # loads as rocAll_long
  stk.chr <- unique(rocAll_long$StockID)
  rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
  for(j in unique(rocAll_long$`Survey Index, Survey Name`)){
    rocSpcs_long <- filter(rocAll_long, `Survey Index, Survey Name` == j)
    for(w in unique(rocAll_long$`Spatial Indicator`)){
      #### 2. Calculate AUC #### 
      rocSpcsIndx_long <- filter(rocSpcs_long, `Spatial Indicator` == w)
      x <- 1-rocSpcsIndx_long$FPR
      y <- rocSpcsIndx_long$TPR
      AUC <- sum(diff(x)*rollmean(y,2)) 
      output <- c(stk.chr, j, w, AUC)
      auc_df <- rbind(auc_df, output)
    }
  }
}
#### 3. Add Columns for Plots ####
auc_df <- auc_df[2:nrow(auc_df),1:4] # remove first row of NAs used init col names
auc_df$AUC <- as.numeric(auc_df$AUC)
auc_df$names <- paste(auc_df$StockID, auc_df$`Survey Index, Survey Name`)
auc_df$rand <- ifelse(auc_df$AUC > 0.5, "TRUE", "FALSE")
auc_df$sig <- ifelse(auc_df$AUC >= 0.75, "*", NA)

#auc_df <- auc_df %>%
#  mutate(rand = if_else(AUC > 0.5, "TRUE", "FALSE"))
#auc_df$colour <- ifelse(auc_df$AUC >= 0.75, "black", NA)

lablist <- list()
for(i in 1:length(unique(auc_df$names))){
  test <- sort(unique(auc_df$names))[i]
  split <- strsplit(test, " ")[[1]]
  k <- strsplit(test, " ")[[1]][2:length(split)]
  lab <- paste(k, collapse = " ")
  lablist[i] <- lab
}
labels <- unlist(lablist)
  
#### 4. Save AUC Data ####
save(auc_df, file = paste0(auc.plot.path, "AUC_longdata.rda"))

#### 5. Plot AUC ####
AUC_plot <- ggplot() +
  geom_col(data = auc_df, aes(x = names, y = AUC, fill = StockID, alpha = rand), position = position_dodge(width = 2), width = 0.8) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  guides(alpha = "none") +
  #scale_colour_identity() +
  geom_text(data = auc_df, aes(x = names, y = AUC, label = sig), vjust = -.0001) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0.5, colour = "grey20", lty = 2) +
  facet_wrap(vars(`Spatial Indicator`)) +
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
  
#### 5. Save AUC Plot #### 
cowplot::save_plot(plot = AUC_plot, filename = paste0(auc.plot.path, "aucPlot.png"), base_height = 8, base_width = 12)

### H. Plot TSS Summary ####
roc.data.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Data/"
tssSum.plot.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/TSSsummary_plot/"

tssSum_df <- data.frame()
tssSum_df2 <- data.frame()

#### 2. Get TSS #### 
for(i in 1:length(list.files(roc.data.path))){
  load(paste0(roc.data.path, list.files(roc.data.path)[i])) # loads as rocAll_long
  stk.chr <- unique(rocAll_long$StockID)
  rocAll_long$`Spatial Indicator Value` <- as.numeric(rocAll_long$`Spatial Indicator Value`)
  tssSum_df2 <- rbind(tssSum_df2, rocAll_long)
  for(j in unique(rocAll_long$`Survey Index, Survey Name`)){
    rocSpcs_long <- filter(rocAll_long, `Survey Index, Survey Name` == j)
    for(w in unique(rocAll_long$`Spatial Indicator`)){
      rocSpcsIndx <- filter(rocSpcs_long, `Spatial Indicator` == w)
      rocSpcsIndx_long <- filter(rocSpcsIndx, is.na(TSS) == F) %>%
        filter(TSS == max(TSS))
      tssSum_df <- rbind(tssSum_df, rocSpcsIndx_long)

    }
  }
}

#### 3. Add Columns for Plots ####
tssSum_df$names <- paste(tssSum_df$StockID, tssSum_df$`Survey Index, Survey Name`)
tssSum_df$rand <- ifelse(tssSum_df$TSS > 0, "TRUE", "FALSE")
tssSum_df$sig <- ifelse(tssSum_df$TSS >= 0.5, "*", NA)

lablist <- list()
for(i in 1:length(unique(tssSum_df$names))){
  test <- sort(unique(tssSum_df$names))[i] # sorting alphabetically puts in same order that ggplot plots data
  split <- strsplit(test, " ")[[1]]
  k <- strsplit(test, " ")[[1]][2:length(split)]
  lab <- paste(k, collapse = " ")
  lablist[i] <- lab
}
labels <- unlist(lablist)
  
#### 4. Save TSS Summary Data ####
save(tssSum_df, file = paste0(tssSum.plot.path, "tssSum_longdata.rda"))

#### 5. Plot TSS Summary ####
tssSum_plot <- ggplot() +
  geom_col(data = tssSum_df, aes(x = names, y = TSS, fill = StockID, alpha = rand), position = position_dodge(width = 2), width = 0.8) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  guides(alpha = "none") +
  #scale_colour_identity() +
  geom_text(data = tssSum_df, aes(x = names, y = TSS, label = sig), vjust = -.0001) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0, colour = "grey20", lty = 2) +
  facet_wrap(vars(`Spatial Indicator`)) +
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
  
#### 6. Save TSS Summary Plot #### 
cowplot::save_plot(plot = tssSum_plot, filename = paste0(tssSum.plot.path, "tssSumPlot.png"), base_height = 8, base_width = 12)

#### 7. Alternative TSS Summary Plots ####
tssSum_df2$names <- paste(tssSum_df2$StockID, tssSum_df2$`Survey Index, Survey Name`)
tssSum_df2$rand <- ifelse(tssSum_df2$TSS > 0, "TRUE", "FALSE")
tssSum_df2$sig <- ifelse(tssSum_df2$TSS >= 0.5, "*", NA)

tssSum_df3 <- tssSum_df2 %>%
  group_by(StockID, `Survey Index, Survey Name`, `Spatial Indicator`, names) %>%
  summarise(y0 = min(TSS),
            y25 = quantile(TSS, 0.25, na.rm = T),
            y50 = median(TSS),
            y75 = quantile(TSS, 0.75, na.rm = T),
            y100 = max(TSS))

tssSum_plot2 <- ggplot() +
  geom_errorbar(data = tssSum_df3, aes(x = names, ymin = y0, ymax = y100), colour = "black", width = 0.5, key_glyph = "rect") +
  geom_boxplot(data = tssSum_df3, aes(x = names, ymin = y0, ymax = y100, lower = y25, upper = y75, middle = y50, fill = StockID),colour = "grey20", stat="identity", key_glyph = "rect") +
  facet_wrap(vars(`Spatial Indicator`)) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0, colour = "grey20", lty = 2) +
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
cowplot::save_plot(plot = tssSum_plot2, filename = paste0(tssSum.plot.path, "tssSumPlot2.png"), base_height = 8, base_width = 12)

tssSum_plot3 <- ggplot() +
  geom_errorbar(data = tssSum_df3, aes(x = names, ymin = y0, ymax = y100, colour = StockID), width = 0.9, key_glyph = "rect", linewidth = 0.5) +
  facet_wrap(vars(`Spatial Indicator`)) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0, colour = "grey20", lty = 2) +
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
cowplot::save_plot(plot = tssSum_plot3, filename = paste0(tssSum.plot.path, "tssSumPlot3.png"), base_height = 8, base_width = 12)

  
        
  
  
  #scale_alpha_discrete(range = c(0.5, 1)) +
  #guides(alpha = "none") +
  #scale_colour_identity() +
  #geom_text(data = tssSum_df, aes(x = names, y = TSS, label = sig), vjust = -.0001) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0, colour = "grey20", lty = 2) +
  facet_wrap(vars(`Spatial Indicator`)) +
  xlab("Survey Index, Survey Name") +
  ylab("True Skill Score of Optimised Threshold") +
  #coord_cartesian(ylim = c(-1,1.1)) +
  #scale_y_continuous(breaks = seq(-1,1, 0.2), expand = c(0,0)) + 
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

### I. Plot Spatial Indicators with Optimal Threshold ####
#### 1. Load and Prepare Data ####
# Get all SDI data and convert into long format
si.data.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/10Stock_Outputs/")
si.plot.path <- paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/SpatIndPlots/")
sdi_longlist <- list()
sdi_widelist <- list()
i <- 1

for(StockFolder in list.files(si.data.path)){
  message(paste0("Stock: ", StockFolder))
  for(IndexFolder in list.files(paste0(si.data.path, StockFolder))){
    message(paste0("Survey Index: ", IndexFolder))
    for(SurveyFolder in list.files(paste0(si.data.path, StockFolder, "/", IndexFolder, "/"))){
      print(noquote(paste0("Survey: ", SurveyFolder)))
      #Load  Data
      si_file <- list.files(paste0(si.data.path, StockFolder, "/", IndexFolder, "/", SurveyFolder, "/SpatIndData/"), pattern = "*SpatIndData*")
      load(paste0(si.data.path, StockFolder, "/", IndexFolder, "/", SurveyFolder, "/SpatIndData/", si_file))
      si$Inertia <- si$Inertia/1000000
      colnames(si)[colnames(si)=="Inertia"] <- "Inertia (million)"
      sdi_widelist[[i]] <- si
      #  Convert wide to long 
      si_long <- si %>% tidyr::pivot_longer(cols = sort(c("Gini Index", "D95", "Positive Area (Rectangle)", "Positive Area (Haul)", "SPI", "Spreading Area", "Equivalent Area",
                                                     "CoG (x)","CoG (y)", "Inertia (million)", "Ellipse Area", "Convex Hull Area")), 
                                            names_to = "Spatial Indicator",
                                            values_to = "Spatial Indicator Value")
      si_long$`Spatial Indicator` <- factor(si_long$`Spatial Indicator`, levels = sort(c("Gini Index", "D95", "Positive Area (Haul)", "Positive Area (Rectangle)", "SPI", 
                                                                                    "Spreading Area", "Equivalent Area", "CoG (x)","CoG (y)", "Inertia (million)", 
                                                                                    "Ellipse Area", "Convex Hull Area"))) # factor & relocate
      sdi_longlist[[i]] <- si_long
      i <- i + 1
    }
  }
}

##### 1.1 Merge Data ####
sdiall_long <- do.call(rbind, sdi_longlist)
sdiall_wide <- do.call(rbind, sdi_widelist)
# Create new column for line plot
sdiall_long$`Survey Index, Survey Name` <- paste0(sdiall_long$SurveyIndex, ", ", sdiall_long$Survey)
sdiall_wide$`Survey Index, Survey Name` <- paste0(sdiall_wide$SurveyIndex, ", ", sdiall_wide$Survey)

##### 1.2 Add Optimum Threshold ####
# Load data from section H
tssSum.plot.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/TSSsummary_plot/"
# All columns of data where TSS is maximised 
load(file = paste0(tssSum.plot.path, "tssSum_longdata.rda")) # tssSum_df 
# Get the spat ind value where TSS was maximised 
optthreshdata <- tssSum_df %>%
  select(Year, StockID, SurveyIndex, Survey, `Survey Index, Survey Name`, `Spatial Indicator`, `Spatial Indicator Value`, `TSS`) %>%
  rename("OptThresh" = `Spatial Indicator Value`, "MaxTSS" = TSS, "OptYear" = Year) %>%
  filter(OptYear != 998)
# There should be no duplicates in this check:
if(any(duplicated(optthreshdata[2:6]))){
  duprowno <- unique(sort(c(which(duplicated(optthreshdata[2:6])), which(duplicated(optthreshdata[2:6]))-1)))
  duprows <- optthreshdata[duprowno,] %>%
    rename("Year" = OptYear, "Spatial Indicator Value" = OptThresh, "TSS" = MaxTSS) %>%
    mutate(Duplicate = "TRUE")
  tssdup <- merge(tssSum_df, duprows, by = c("Year", "StockID", "SurveyIndex", "Survey", "Survey Index, Survey Name", "Spatial Indicator", "Spatial Indicator Value", "TSS"))
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
    select(-maxTNR)
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
sdiall_Stndrd <- merge(sdiall_long, optthreshdata, by = c("StockID", "SurveyIndex", "Survey", "Survey Index, Survey Name", "Spatial Indicator"))
sdiall_Stndrd$`Spatial Indicator Value/OptThresh` <- sdiall_Stndrd$`Spatial Indicator Value`/sdiall_Stndrd$OptThresh

#### 2. Load Stock Objects ####
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock Objects/2022/")
# Put into a lsit to iterate through later
for(stockfile in list.files(stockobj.path)){load(paste0(stockobj.path, stockfile))}
stocklist <- list(cod.27.47d20_nov, had.27.46a20, ple.27.420, ple.27.7d, 
                  pok.27.3a46, sol.27.4, tur.27.4, whg.27.47d, wit.27.3a47d) # ignore so.27.7d for now, need to find YFS survey data
# Order here must match order in stocklist
stocklist.chr <- list("cod.27.47d20_nov", "had.27.46a20", "ple.27.420", "ple.27.7d", 
                      "pok.27.3a46", "sol.27.4", "tur.27.4", "whg.27.47d", "wit.27.3a47d") # ignore so.27.7d for now, need to find YFS survey data

#### 3. Plot ####
for(i in 1:length(stocklist)){
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
  sdistk_stndrd <-filter(sdiall_Stndrd, StockID == stk.chr)
  
  #### 3.2 Get SSB/MSY Btrigger ####
  writeLines("Get SSB for survey years")
  
  strtyr <- min(sdistk_wide$Year)
  if(strtyr < range(stk)["minyear"]){
    warning("First year of survey data provided preceeds first year of data in the stock object. Using minyear of the stock object instead.", immediate. = TRUE)
    strtyr <- range(stk)["minyear"]}
  
  endyr <- max(sdistk_wide$Year)
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
    geom_hline(yintercept = 1, colour = "grey20", lty = 2) +
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
  
  #### 3.4 Overlay SSB & SDI ####
  writeLines("Plot layout")
  optthresh_plot <- ggdraw()+ draw_plot(optthresh_plot2)+ draw_plot(ssb_plot, x= .76, y= 0.2977, width=.206, height = .206)
  ### 4. Save ####
  writeLines("Save")
  cowplot::save_plot(plot = optthresh_plot, filename = paste0(si.plot.path, "OptSpatIndPlot-", stk.chr, ".png"), base_height = 25, base_width = 12)
}
