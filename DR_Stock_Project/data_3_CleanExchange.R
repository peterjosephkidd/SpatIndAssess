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

rm(list = ls())

# Set-up ####
load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"

stksurveys <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stksurveys-optim.xlsx"), sheet = "Surveys")
refpts     <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stkdescrptn-optim.xlsx"))
sa_data    <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-SA-data-optim.xlsx"))

load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Divs/ices_divs.rds")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Rect/ices_rect.rds")

namefile <- function(stk = NA, data, f = NA, r = NA, ext = ".rds"){
  if(is.na(f)){
    f <- unique(data$Survey)
  }
  if(is.na(r)){
    r <- unique(data$RecordType)
  }
  y <- paste0(min(data$Year), "-", max(data$Year))
  q <- paste0("Q", unique(data$Quarter), collapse = ".")
  n <- paste0(f,".Yr",y,".",q,".",r, "--", stk, ext)
  return(n)
}

dummy <- data.frame(StockKeyLabel = rep("dummy.stock", 2), 
                    SpeciesCommonName = rep("Dummy",2), 
                    SpeciesScientificName = rep("Dummus",2), 
                    SurveyAcronymn = c("NS-IBTS", "SNS"),
                    YearStart = c(2000, 2003),
                    YearEnd = c(2005, 2006),
                    Quarter = c(1, 3))

stksurveys <- full_join(stksurveys, dummy, by = c(colnames(dummy)))

# Loop through all
stks <- unique(refpts$StockKeyLabel)
# ... or just one stock
#stks <- "dummy.stock"

# Valid_Aphia ####
va <- c()
for(i in 1:length(stks)){
  stk <- stks[i]
  species <- refpts %>% filter(
    StockKeyLabel == stk) %>%
    select(SpeciesCommonName, SpeciesScientificName)
  Valid_Aphia <- findAphia(species$SpeciesScientificName, latin = TRUE) # get valid aphia
  output <- cbind(StockKeyLabel = stk, species, Valid_Aphia)
  va <- rbind(va, output)
  rm(species, stk, Valid_Aphia, output)
}

stksurveys <- merge(stksurveys,  va, by = c("StockKeyLabel", "SpeciesCommonName", "SpeciesScientificName"))
refpts     <- merge(refpts,      va, by = c("StockKeyLabel", "SpeciesCommonName", "SpeciesScientificName"))
sa_data    <- merge(sa_data,     va, by = c("StockKeyLabel", "SpeciesCommonName", "SpeciesScientificName"))


# ... or just one stock
stks <- "dummy.stock"

# Clean data ####
for(i in 1:length(stks)){
  stk <- stks[i]
  message(stk)
  
  suppressWarnings(dir.create(paste0(save.path, stk, "/clean"), recursive = T))
  
  srvys <- stksurveys %>%
    filter(StockKeyLabel == stk) %>%
    select(SpeciesCommonName, SurveyAcronymn, SurveyRefNo, SurveyIndex, YearStart, YearEnd, Quarter)
  
  srvy.list <- unique(srvys$SurveyAcronymn)
  message(paste0(srvy.list, collapse = ", "))  
  
  for(srvy in srvy.list){
    
    # Load survey data 
    files <- list.files(paste0(load.path, "SurveyData/", stk, "/raw/"), pattern = paste0("*", srvy, "*"))
    file.paths <- paste0(load.path, "SurveyData/", stk, "/raw/", files)
    
    load(file.paths[stringr::str_detect(file.paths, ".HH--")])
    load(file.paths[stringr::str_detect(file.paths, ".HL--")])
    load(file.paths[stringr::str_detect(file.paths, ".CA--")])
    
    # Remove duplicates
    hh <- unique(hh)
    hl <- unique(hl)
    ca <- unique(ca)
    
    # Add ICES Divisions
    area_div <- dplyr::distinct(ices_rect[c("ICESNAME", "Area_27", "Shape_Area")])
    hh <- merge.data.frame(hh, area_div, by.x = "StatRec", by.y = "ICESNAME")
    ca <- merge.data.frame(ca, area_div, by.x = "AreaCode", by.y = "ICESNAME")
    
    # Edit -9 NA placeholder
    hl$TotalNo[hl$TotalNo == -9] <- NA
    
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
    
    
    save(hh, file = paste0(save.path, stk, "/clean/", namefile(stk, hh)))
    save(hl, file = paste0(save.path, stk, "/clean/", namefile(stk, hl)))
    save(ca, file = paste0(save.path, stk, "/clean/", namefile(stk, ca)))
    save(hlhh, file = paste0(save.path, stk, "/clean/", namefile(stk, hlhh, r = "HLHH")))
  
  }
}
