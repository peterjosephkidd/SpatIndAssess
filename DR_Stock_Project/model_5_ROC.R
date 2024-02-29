#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               5. Assess Spatial Indicators
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(icesVocab)
library(readxl)
library(ggplot2) 

rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/"
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/"

# Stock Assessment Data 
sa_data    <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-31stks-AY2022-SA-data-optim.xlsx"))

# Spatial Indicator Data
si.files <- list.files(paste0(load.path, "Data/DR_Stocks/Outputs/Spatinds/"), full.names = T)
load(si.files[2])

# ROC Functions
source(paste0(load.path, "Functions/ROC_funs.R")) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ssb.ref <- sa_data %>% 
  select(StockKeyLabel, Year, SSB, MSYBtrigger) %>%
  filter(StockKeyLabel %in% unique(spatinds$StockKeyLabel)) %>%
  mutate(`SSB/MSYBtrigger` = SSB/MSYBtrigger) %>%
  full_join(., spatinds, by = c("StockKeyLabel", "Year")) %>%
  mutate(SurveyNameIndex = paste0(SurveyName, ", ", SurveyIndex))

rbind(head(ssb.ref), tail(ssb.ref))

all(unique(spatinds$StockKeyLabel) %in% unique(ssb.ref$StockKeyLabel))

# Parameters
state <- "SSB/MSYBtrigger"
inds <- c("CoG (x)", "CoG (y)", "Inertia", "EOO", "ELA", "POPR", "POPH", "Gini Index", "D95", "SA", "EA", "SPI")
stks <- unique(ssb.ref$StockKeyLabel)
roc_longlist <- list()
iter <- 1

# Loop
for (i in 1:length(stks)) {
  
  stk <- stks[i]
  #message(paste0("#>>>>>>> ", stk, " >>>>>>>#"))
  ssb.ref.stk <- filter(ssb.ref, StockKeyLabel == stk)
  srvys <- unique(ssb.ref.stk$SurveyNameIndex)
  
  for (j in 1:length(srvys)) {
    
    srvindx <- srvys[j]
    #message(paste0("#>>>>>>> ", srvindx, " >>>>>>>#"))
    ssb.ref.stk.srvindx <- filter(ssb.ref.stk, SurveyNameIndex == srvindx)
    qrs <- sort(unique(ssb.ref.stk.srvindx$Quarter))
    
    for (w in 1:length(quarters)){
      qr <- qrs[w]
      message(paste0("#>>>>>> ", stk, ": ", srvindx, ", Quarter: ", qr, " >>>>>>#"))
      
      if (is.na(qr)) {
        ssb.ref.stk.srvindx.qr <- ssb.ref.stk.srvindx
      } else{
      ssb.ref.stk.srvindx.qr <- filter(ssb.ref.stk.srvindx, Quarter == qr)
      }
      
      roc_long <- rocR(ssb.ref.stk.srvindx.qr, state, inds, return = "long", p = F)
      roc_longlist[[iter]] <- roc_long
      iter <- iter + 1
    }
  }
}

# Output
rocAll_long <- do.call(rbind, roc_longlist)

# Explore
roc_summary <- rocAll_long %>%
  select(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`, AUC) %>%
  distinct() %>%
  group_by(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`) %>%
  na.omit() %>%
  summarise(AUC) %>%
  print(n = nrow(.))

table(roc_summary$`Spatial Indicator`)

roc_summary %>%
  group_by(`Spatial Indicator`) %>%
  summarise(n.good = length(which(AUC >= 0.75)),
            n.avg  = length(which(AUC < 0.75 & AUC >= 0.35)),
            n.bad  = length(which(AUC < 0.35)))

roc_summary %>%
  group_by(StockKeyLabel) %>%
  summarise(n.good = length(which(AUC >= 0.75)),
            n.avg  = length(which(AUC < 0.75 & AUC >= 0.35)),
            n.bad  = length(which(AUC < 0.35))) %>%
  print(n = nrow(.))

# Save
suppressWarnings(dir.create(paste0(save.path, "/ROC"), recursive = T))
save(rocAll_long, file = paste0(save.path, "ROC/ROCdata-", length(unique(rocAll_long$StockKeyLabel)), "Stks.rda"))

