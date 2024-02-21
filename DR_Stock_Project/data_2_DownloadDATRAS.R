#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                        2. Download DATRAS Survey data
#>                         
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(writexl)
library(readxl)
library(icesDatras)
library(icesVocab)
library(dplyr)

rm(list = ls())

load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"

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

stksurveys <- read_xlsx(paste0(load.path, "icesData-31stks-AY2022-stksurveys-optim.xlsx"), sheet = "Surveys")

dummy <- data.frame(StockKeyLabel = rep("dummy.stock", 2), 
                    SpeciesCommonName = rep("Dummy",2), 
                    SpeciesScientificName = rep("Dummus",2), 
                    SurveyAcronymn = c("NS-IBTS", "SNS"),
                    YearStart = c(2000, 2004),
                    YearEnd = c(2001, 2005),
                    Quarter = c(1, 3))

stksurveys <- full_join(stksurveys, dummy, by = c(colnames(dummy)))

# Loop through all
stks <- unique(stksurveys$StockKeyLabel)
# One stock at a time otherwise function stalls
stk <- "dummy.stock"

allsrvys <- stksurveys %>%
  select(SurveyAcronymn, SurveyRefNo, YearStart, YearEnd, Quarter) %>%
  group_by(SurveyAcronymn, Quarter) %>%
  mutate(minYr = min(YearStart), maxYr = max(YearEnd)) %>%
  distinct()
allsrvys %>% arrange(SurveyAcronymn) %>%
  print(n = nrow(allsrvys))

srvys <- stksurveys %>%
  filter(StockKeyLabel == stk) %>%
  select(SurveyAcronymn, SurveyRefNo, SurveyIndex, YearStart, YearEnd, Quarter)

srvy.list <- unique(srvys$SurveyAcronymn)

suppressWarnings(dir.create(paste0(save.path, stk, "/raw"), recursive = T))

# One survey at a time
srvy <- srvy.list[2] # have manually change, messes up in for loop

srvys.filt <- srvys[srvys$SurveyAcronymn == srvy,]

qrs <- unique(srvys.filt$Quarter)
yrs <- min(srvys.filt$YearStart, na.rm = T):max(srvys.filt$YearEnd, na.rm = T)

hh <- data.frame()
hl <- data.frame()
ca <- data.frame()

hh.df <- try(getDATRAS(record = "HH", srvy, years = yrs, quarters = c(qrs)))
hl.df <- try(getDATRAS(record = "HL", srvy, years = yrs, quarters = c(qrs)))
ca.df <- try(getDATRAS(record = "CA", srvy, years = yrs, quarters = c(qrs)))

hh <- rbind(hh, hh.df)
hl <- rbind(hl, hl.df)
ca <- rbind(ca, ca.df)
  
save(hh, file = paste0(save.path, stk, "/raw/", namefile(stk, hh)))
save(hl, file = paste0(save.path, stk, "/raw/", namefile(stk, hl)))
save(ca, file = paste0(save.path, stk, "/raw/", namefile(stk, ca)))

  
  
###########
for(j in 1:length(stks)){
  
  stk <- stks[j]
  message(stk)
  srvys <- stksurveys %>%
    filter(StockKeyLabel == stk) %>%
    select(SurveyAcronymn, SurveyRefNo, SurveyIndex, YearStart, YearEnd, Quarter)
  
  srvy.list <- unique(srvys$SurveyAcronymn)
  message(paste0(srvy.list, collapse = ", "))  
  
  
  for(i in 1:length(srvy.list)){
    srvy <- srvy.list[i]
    
    srvys.filt <- srvys[srvys$SurveyAcronymn == srvy,]
    
    qrs <- unique(srvys.filt$Quarter)
    yrs <- min(srvys.filt$YearStart, na.rm = T):max(srvys.filt$YearEnd, na.rm = T)
    
    message(paste0("Downloading ", srvy, ", ", min(yrs), "-", max(yrs), ", Qrs ", paste0(qrs, collapse = ", ")))
    
    hh.df <- data.frame()
    hl.df <- data.frame()
    ca.df <- data.frame()
    
    for(yr in yrs){
      
      print(yr)
      
      message("\nHH")
      hh <- try(getDATRAS(record = "HH", srvy, years = yr, quarters = c(qrs)))
      
      message("\nHL")
      hl <- try(getDATRAS(record = "HL", srvy, years = yr, quarters = c(qrs)))
      
      message("\nCA")
      ca <- try(getDATRAS(record = "CA", srvy, years = yr, quarters = c(qrs)))
      
      if(!is.null(dim(hh))){
        hh.df <- rbind(hh.df, hh)
      }
      
      if(!is.null(dim(hl))){
        hl.df <- rbind(hl.df, hl)
      }
      
      if(!is.null(dim(ca))){
        ca.df <- rbind(ca.df, ca)
      }
      
      
    }
    
    # Retry years that did not get downloaded
    hh.yrs <- yrs[!yrs %in% unique(hh.df$Year)]
    hl.yrs <- yrs[!yrs %in% unique(hl.df$Year)]
    ca.yrs <- yrs[!yrs %in% unique(ca.df$Year)]
    
    if(!is.null(dim(hh.yrs))){
      message("\nHH retry")
      hh2 <- try(getDATRAS(record = "HH", srvy, years = hh.yrs, quarters = c(qrs)))
    }
    
    if(!is.null(dim(hl.yrs))){
      message("\nHL retry")
      hl2 <- try(getDATRAS(record = "HL", srvy, years = hl.yrs, quarters = c(qrs)))
    }
    
    if(!is.null(dim(ca.yrs))){
      message("\nCA retry")
      ca2 <- try(getDATRAS(record = "CA", srvy, years = ca.yrs, quarters = c(qrs)))
    }
    
    if(hh2 != F){
      hh.df <- rbind(hh.df, hh2)
    }
    if(hl2 != F){
      hl.df <- rbind(hl.df, hl2)
    }
    if(ca2 != F){
      ca.df <- rbind(ca.df, ca2)
    }
    
    # Check years
    if(any(!yrs %in% unique(hh.df$Year))){
      warning(paste0("Years not downloaded for HH: \n"), paste0(yrs[!yrs %in% unique(hh.df$Year)], collpase = " "))
    }
    
    if(any(!yrs %in% unique(hl.df$Year))){
      warning(paste0("Years not downloaded for HL: \n"), paste0(yrs[!yrs %in% unique(hl.df$Year)], collpase = " "))
    }
    
    if(any(!yrs %in% unique(ca.df$Year))){
      warning(paste0("Years not downloaded for HH: \n"), paste0(yrs[!yrs %in% unique(ca.df$Year)], collpase = " "))
    }
    
    save(hh.df, file = paste0(save.path, stk, "/raw/", namefile(stk, hh.df)))
    save(hl.df, file = paste0(save.path, stk, "/raw/", namefile(stk, hl.df)))
    save(ca.df, file = paste0(save.path, stk, "/raw/", namefile(stk, ca.df)))
    
  }
}

yrs[!yrs %in% unique(hh.df$Year)]
yrs[!yrs %in% unique(hl.df$Year)]
yrs[!yrs %in% unique(ca.df$Year)]

hh3 <- try(getDATRAS(record = "HH", srvy, years = yrs[!yrs %in% unique(hh.df$Year)], quarters = c(qrs)))
hl3 <- try(getDATRAS(record = "HL", srvy, years = yrs[!yrs %in% unique(hl.df$Year)], quarters = c(qrs)))
ca3 <- try(getDATRAS(record = "CA", srvy, years = yrs[!yrs %in% unique(ca.df$Year)], quarters = c(qrs)))
