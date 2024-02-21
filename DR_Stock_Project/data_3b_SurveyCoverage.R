#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               3.b. Filter to surveys with 'good' coverage
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(writexl)
library(readxl)
library(dplyr)

rm(list = ls())

load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/"
stksurveys <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stksurveys-optim.xlsx"), sheet = "Surveys")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Divs/ices_divs.rds")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Rect/ices_rect.rds")

stks <- unique(stksurveys$StockKeyLabel)
stks <- "cod.27.47d20"
stksurveys$AvgSurveyCoverage <- rep(NA, nrow(stksurveys))

srvys.coverage <- data.frame()

for(i in 1:length(stks)){
  stk <- stks[i]
  #stk <- "cod.27.47d20"
  message(stk)
  
  divs <- unique(stksurveys[stksurveys$StockKeyLabel == stk,]$Divisions)
  stkdivs <- unlist(strsplit(divs, ", "))

  # No. ICES Rectangles in Stock Area
  TotRects <- ices_rect %>%
    filter(Area_27 %in% stkdivs) %>%
    select(Area_27, ICESNAME) %>%
    distinct() %>%
    nrow()

  srvys <- stksurveys[stksurveys$StockKeyLabel == stk,]

  for(j in 1:nrow(srvys)){
    srv <- srvys[j,]$SurveyAcronymn
    indx <- srvys[j,]$SurveyIndex
    tryCatch({
      
      yrs <- srvys[j,]$YearStart:srvys[j,]$YearEnd
      qrs <- srvys[j,]$Quarter

      message(paste0(srv, ", ", indx))
      files <- list.files(paste0(load.path, "SurveyData/", stk, "/matures/"), pattern = paste0("*", srv, "*"), full.names = T)
      do.call(list, lapply(files, load, envir = .GlobalEnv))
      
      # Average across years and quarters
      avg <- hlhh %>%
        filter(Area_27 %in% stkdivs,
               Year %in% yrs,
               Quarter %in% qrs) %>%
        group_by(Year) %>%
        select(StatRec) %>%
        distinct() %>%
        summarise(N.SampledRects = length(StatRec)) %>%
        mutate(N.RectsInStkDiv = TotRects,
               SurveyCoverage = (N.SampledRects/N.RectsInStkDiv)*100,
               AvgSurveyCoverage = ((sum(N.SampledRects)/nrow(.))/N.RectsInStkDiv)*100) %>%
        select(AvgSurveyCoverage) %>%
        distinct()
      
      srvys[j,]$AvgSurveyCoverage <- avg$AvgSurveyCoverage
      srvys.coverage <- rbind(srvys.coverage, srvys[j,])
      
      
    }, error = function(e){
      message(paste0(stk, ": ", srv, ", ", indx, ". ", "Data missing. Skipping this survey index."))
    })

    next
      
  }
}

write_xlsx(srvys.coverage, path = paste0(save.path, "/icesData-", length(unique(srvys.coverage$StockKeyLabel)), "stks-AY", unique(srvys.coverage$AssessmentYear) ,"-stksurveys-optim-survcoverage.xlsx"))


# Need to get area list of each stock
#> For each survey...
#> filter to correct years & quarters, and to the stock area
#> Then for each year within the survey...
#> Work out how many rectangles within the stock area were sampled
#> calculate averge over the years 
#> 

areas <- unique(ices_rect$Area_27)
paste0(sort(areas[stringr::str_detect(areas, "7.")]), collapse = ", ")
getDATRAS(record = "HH", survey = "SP-PORC", years = 2020, quarters = 0)
