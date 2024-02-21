#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               3.a. Filter survey data to mature individuals
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#There are compatability problems with `FishBase`, so install an earlier
#version. These have been fixed now. see
#<https://james-thorson-noaa.github.io/FishLife/>

#remotes::install_github("ropensci/rfishbase",ref="d150f2e0f5")
#remotes::install_github("james-thorson/FishLife")
#devtools::install_github("henning-winker/SPMpriors")

library(writexl)
library(readxl)
library(icesDatras)
library(icesVocab)
library(dplyr)
library(remotes)
library(rfishbase)
library(SPMpriors)
library(FishLife)

rm(list = ls())

load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"

refpts     <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stkdescrptn-optim.xlsx"))
stksurveys <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stksurveys-optim.xlsx"), sheet = "Surveys")



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

# Get L50 from FishBase
Lmat <- data.frame()
for(i in 1:nrow(refpts)){
  stk <- refpts[i,]
  Genus <- strsplit(stk$SpeciesScientificName, " ")[[1]][1]
  Species <- strsplit(stk$SpeciesScientificName, " ")[[1]][2]
  # Use tryCatch to handle errors
  tryCatch({
    par <- flmvn_traits(Genus, Species, Plot = FALSE)
    L50 <- par$traits[3,]
    output <- cbind(select(stk, StockKeyLabel, SpeciesCommonName, SpeciesScientificName), L50)
    Lmat <- rbind(Lmat, output)
  }, error = function(e) {
    # Handle the error gracefully, e.g., print a message
    cat("Error occurred for:", stk$SpeciesCommonName, "\nError message:", conditionMessage(e), "\n")
  })
  next
  rm(output, par, stk, L50, Genus, Species)
}

# L50 not retrieved for
refpts[!refpts$StockKeyLabel %in% Lmat$StockKeyLabel,]

# Save
suppressWarnings(dir.create(paste0(load.path, "/FishBaseMaturity"), recursive = T))
save(Lmat, file = paste0(load.path, "/FishBaseMaturity/", nrow(Lmat), "Stks-FishBase-L50.rds"))

# Loop all
stks <- Lmat$StockKeyLabel

# Or one stock at a time
stks <- "cod.27.47d20"

# TotalNo Mature
for(i in 1:length(stks)){
  stk <- stks[i]
  message(stk)
  
  stkLmat <- Lmat[Lmat$StockKeyLabel == stk,]
  va <-  findAphia(stkLmat$SpeciesScientificName, latin = TRUE)
  
  srvys <- unique(stksurveys[stksurveys$StockKeyLabel == stk,]$SurveyAcronymn)
  
  suppressWarnings(dir.create(paste0(save.path, stk, "/matures"), recursive = T))
  
  for(j in 1:length(srvys)){
    srv <- srvys[j]
    message(srv)
    
    files <- list.files(paste0(load.path, "SurveyData/", stk, "/clean/"), pattern = paste0("*", srv, "*"), full.names = T)
    do.call(list, lapply(files, load, envir = .GlobalEnv))
    
    hlhh$TrgtSpcs <- stkLmat$SpeciesCommonName
    ca$TrgtSpcs   <- stkLmat$SpeciesCommonName
    
    hlhh$TrgtSpcsL50 <- if_else(hlhh$Valid_Aphia == va, L50$mu.sp, NA)
    ca$TrgtSpcsL50   <- if_else(ca$Valid_Aphia   == va, L50$mu.sp, NA)
    
    hlhh$TrgtSpcsMature <- if_else(hlhh$LngtClass >= hlhh$TrgtSpcsL50 & hlhh$Valid_Aphia == va, 1, if_else(hlhh$LngtClass < hlhh$TrgtSpcsL50 & hlhh$Valid_Aphia == va, 0, NA))
    ca$TrgtSpcsMature   <- if_else(ca$LngtClass   >= ca$TrgtSpcsL50   & ca$Valid_Aphia   == va, 1, if_else(ca$LngtClass   < ca$TrgtSpcsL50   & ca$Valid_Aphia   == va, 0, NA))
    
    hlhh <- hlhh %>% 
      group_by(haul.id, Valid_Aphia) %>%
      mutate(TotalNoMature = sum(HLNoAtLngt[TrgtSpcsMature == 1]),
             PropMature = TotalNoMature/TotalNo)
    
    save(hh,   file = paste0(save.path, stk, "/matures/", namefile(stk, hh)))
    save(hl,   file = paste0(save.path, stk, "/matures/", namefile(stk, hl)))
    save(ca,   file = paste0(save.path, stk, "/matures/", namefile(stk, ca)))
    save(hlhh, file = paste0(save.path, stk, "/matures/", namefile(stk, hlhh, r = "HLHH")))
  }
}







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>       THIS IS SOMETHING TO PUT INTO THE DATA_3_CLEANEXCHNG.R SCRIPT
#>        
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#> I noticed that some of the summations of HLNoAtLength does not equal TotalNo
#> Sometimes it is greater than TotalNo
#> Sometimes by a large amount e.g. 3000 but usually by 1-10
#> This is going to require some manual intervention to look at fix for
#> every survey data I download
#> It appears to me the error is in the TotalNo, not in my code. 
#> For most datapoints HLNoAtLngt is equal to TotalNo
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
any(hlhh$`SumHlNoAtLngt - TotalNo` > 0)

fails <- hlhh[hlhh$`SumHlNoAtLngt - TotalNo` > 0,]
nrow(fails)
nrow(hlhh[hlhh$`SumHlNoAtLngt - TotalNo` == 0,])
nrow(hlhh[hlhh$`SumHlNoAtLngt - TotalNo` < 0,])

fails$YearPlot <- as.character(fails$Year)
  
ggplot(data = fails) +
  geom_histogram(aes(x = `SumHlNoAtLngt - TotalNo`, fill = YearPlot), bins = 200) +
  geom_vline(xintercept = 10)

topfails <- hlhh[hlhh$`SumHlNoAtLngt - TotalNo` > 10,] 

ggplot(data = topfails) +
  geom_histogram(aes(x = `SumHlNoAtLngt - TotalNo`, fill = YearPlot), bins = 200)

  tst <- hlhh %>% 
    #filter(Valid_Aphia == va) %>%
    select(Valid_Aphia, haul.id, Year, Quarter, HaulNo, TotalNo, HLNoAtLngt, 
           LngtClass, TrgtSpcsL50, TrgtSpcsMature, TotalNoMature, PropMature, 
           SumHLNoAtLngt, `TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`,
           `SumHlNoAtLngt - TotalNo`) %>%
    arrange(haul.id)
  
  fails <- tst[tst$`SumHLNoAtLngt <= TotalNo` == "Fail",]
  
  # In which years and quarters
  table(fails$Quarter, fails$Year)
  # Which species
  failspecies <- unique(topfails$Valid_Aphia)
  # Any target species?
  va %in% failspecies
  # Which year and quarters for target species
  table(fails[fails$Valid_Aphia == va,]$Quarter, fails[fails$Valid_Aphia == va,]$Year)
  # Have a look
  TrgtSpcsFails <- fails[fails$Valid_Aphia == va,]
  # Biggest difference
  max(TrgtSpcsFails$`SumHlNoAtLngt - TotalNo`, na.rm = T)
  # have a look
  TrgtSpcsFails  %>%
    select(Valid_Aphia, haul.id, Year, Quarter, HaulNo, TotalNo, HLNoAtLngt, 
           LngtClass, TrgtSpcsL50, TrgtSpcsMature, TotalNoMature, PropMature, 
           SumHLNoAtLngt, 
           #`TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`,
           `SumHlNoAtLngt - TotalNo`) %>%
    arrange(haul.id) %>%
    print(n = 150)
  # Are there case where TotalNo within a single haul.id is not consistent 
  checkTotNo <- TrgtSpcsFails %>%
    ungroup() %>%
    select(haul.id, TotalNo) %>%
    distinct()
  any(duplicated(checkTotNo$haul.id))
  # What about for all species
  checkTotNo_all <- fails %>%
    ungroup() %>%
    select(haul.id, TotalNo) %>%
    distinct()
  any(duplicated(checkTotNo$haul.id))
  duphauls <- checkTotNo_all[duplicated(checkTotNo_all$haul.id),]
  
  fails %>%
    filter(haul.id == duphauls$haul.id[3]) %>%
    arrange(haul.id, Valid_Aphia) %>%
    print(n = nrow(.))
  

  fails[fails$Year == "NA",]
  max(fails$SumHLNoAtLngt - fails$TotalNo, na.rm = T)
  
  fails %>%
   arrange(-`SumHlNoAtLngt - TotalNo`) %>%
    select(- c(`TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`))
  
  topfails <- fails %>%
    select(Valid_Aphia, haul.id) %>%
    distinct()
  
  failspecies <- unique(topfails$Valid_Aphia)
  
  
  
  
  hlhh %>%
    filter(haul.id == "2001:1:FR:35HT:GOV:F0023:23",
           Valid_Aphia == 105865) %>%
    select(Valid_Aphia, haul.id, Year, Quarter, HaulNo, TotalNo, HLNoAtLngt, 
           LngtClass, TrgtSpcsL50, TrgtSpcsMature, TotalNoMature, PropMature, 
           SumHLNoAtLngt, 
           #`TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`,
           `SumHlNoAtLngt - TotalNo`)
  
  

  
  tst <- hlhh %>%
    filter(Valid_Aphia ==  va,
           haul.id == "2000:1:DE:06NI:GOV:116:34") 
  sum(tst$HLNoAtLngt)
  tst %>%
    #group_by(haul.id) %>%
    mutate(TotalNoMature = sum(HLNoAtLngt[TrgtSpcsMature == 1]),
           PropMature = TotalNoMature/TotalNo)

  


