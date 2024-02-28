#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                      3. Process DATRAS Survey data
#>                           
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(writexl)
library(readxl)
library(icesDatras)
library(icesVocab)
library(dplyr)
library(stringr)

rm(list = ls())

# Set-up ####
load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"

stksurveys <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stksurveys-optim.xlsx"), sheet = "Surveys")
refpts     <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stkdescrptn-optim.xlsx"))
sa_data    <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-SA-data-optim.xlsx"))

load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Divs/ices_divs.rds")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Rect/ices_rect.rds")
source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/dataprep_funs.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
load(paste0(save.path, "/stks.rds")) # saved stks from data_2_DownloadDATRAS
stks <- "ank.27.78abd"               # method 1, to individual stocks
stks <- "all.stocks"                 # method 2

# Clean data ####
for (i in 1:length(stks)) {
  stk <- stks[i]
  message(stk)
  
  
  if(stk == "all.stocks") {
    srvy.list <- unique(str_split_i(list.files(paste0(load.path, "SurveyData/", stk, "/raw/")), pattern = ".Yr", i=1))
  } else {
    srvys <- stksurveys %>%
      filter(StockKeyLabel == stk) %>%
      select(SpeciesCommonName, SurveyAcronymn, SurveyRefNo, SurveyIndex, YearStart, YearEnd, Quarter)
    srvy.list <- unique(srvys$SurveyAcronymn)
  }
  
  suppressWarnings(dir.create(paste0(save.path, stk, "/clean"), recursive = T))

  for (srvy in 1:length(srvy.list)) {
    
    srv <- srvy.list[srvy]
    message(srv)
    # Load survey data 
    files <- list.files(paste0(load.path, "SurveyData/", stk, "/raw/"), pattern = paste0("*", srv, "*"), full.names = TRUE)

    load(files[str_detect(files, ".HH--")])
    load(files[str_detect(files, ".HL--")])
    load(files[str_detect(files, ".CA--")])
    
    # Remove duplicates
    hh <- unique(hh)
    hl <- unique(hl)
    ca <- unique(ca)
    
    # Add ICES Divisions
    area_div <- dplyr::distinct(ices_rect[c("ICESNAME", "Area_27", "Shape_Area")])
    hh <- merge.data.frame(hh, area_div, by.x = "StatRec", by.y = "ICESNAME", all.x = TRUE)
    ca <- merge.data.frame(ca, area_div, by.x = "AreaCode", by.y = "ICESNAME", all.x = TRUE)
    
    # Edit -9 NA placeholder
    hl$TotalNo[hl$TotalNo == -9] <- NA
    hl$HLNoAtLngt[hl$HLNoAtLngt == -9] <- NA
    
    # Remove invalid hauls
    hh <- filter(hh, !HaulVal %in% c("I", "P")) # p = partly valid, it is deprecated 
    
    # Create haul.id
    hh$haul.id <- as.character(
      paste(hh$Year, hh$Quarter, hh$Country, hh$Ship, hh$Gear, hh$StNo, hh$HaulNo, 
            sep = ":"))
    hl$haul.id <- as.character(
      paste(hl$Year, hl$Quarter, hl$Country, hl$Ship, hl$Gear, hl$StNo, hl$HaulNo, 
            sep = ":"))
    ca$haul.id <- as.character(
      paste(ca$Year, ca$Quarter, ca$Country, ca$Ship, ca$Gear, ca$StNo, ca$HaulNo, 
            sep = ":"))
    
    # Merge HL HH 
    m <- hh[c("haul.id", "Year", "Quarter", "Month", "Survey","Country", "Ship", 
              "Gear", "GearEx", "DoorType", "HaulDur", "HaulNo", "StNo", "SweepLngt", 
              "StatRec", "Area_27", "ShootLong", "ShootLat", "HaulVal", "Depth")]
    hlhh <- merge(hl, dplyr::distinct(m),
                  c("haul.id", "Year", "Quarter", "HaulNo", "StNo", "Gear", "GearEx", 
                    "DoorType","Ship", "SweepLngt", "Country", "Survey"))
    
    hlhh <- hlhh %>% 
      group_by(haul.id, Valid_Aphia) %>%
      mutate(SumHLNoAtLngt = sum(HLNoAtLngt),
             "SumHlNoAtLngt - TotalNo" = SumHLNoAtLngt - TotalNo) # should = 0    
    
    any(hlhh$`SumHlNoAtLngt - TotalNo` != 0)
    
    save(hh,   file = paste0(save.path, stk, "/clean/", namefile(stk, hh)))
    save(hl,   file = paste0(save.path, stk, "/clean/", namefile(stk, hl)))
    save(ca,   file = paste0(save.path, stk, "/clean/", namefile(stk, ca)))
    save(hlhh, file = paste0(save.path, stk, "/clean/", namefile(stk, hlhh, r = "HLHH")))
  
  }
}
