#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               4. Calculate Spatial Indicators
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(icesVocab)
library(readxl)
library(ggplot2) 

rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/"
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"

# Load data (numbers might change e.g. 31stks)
stksurveys <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-31stks-AY2022-stksurveys-optim.xlsx"), sheet = "Surveys")
refpts     <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-31stks-AY2022-stkdescrptn-optim.xlsx"))
sa_data    <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-31stks-AY2022-SA-data-optim.xlsx"))
survcoverage <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-1stks-AY2022-stksurveys-optim-survcoverage.xlsx"))

load(paste0(load.path, "Data/DR_Stocks/FishBaseMaturity/30Stks-FishBase-L50.rds")) # Lmat
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Divs/ices_divs.rds")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Rect/ices_rect.rds")

# Load functions
source(paste0(load.path, "Functions/spatinds_funs.R")) # for computing spatial indicators


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                          1. Matures, all surveys 
#>                    
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>                    

load(paste0(save.path, "SurveyData/icesData-stksurveys_full.rds"))
load(paste0(save.path, "SurveyData/stks.rds")) # saved stks from data_2_DownloadDATRAS
#stks <- unique(stksurveys$StockKeyLabel) # this will need to change
#stks <- "ank.27.78abd"

spatinds <- data.frame()

# Categorise indicators
loc <- c("CoG (x)", "CoG (y)")
ran <- c("Inertia", "EOO", "ELA")
occ <- c("POPR", "POPH")
agg <- c("Gini Index", "D95", "SA", "EA", "SPI")

# Toggle maturity
mtr <- TRUE

for(i in 1:length(stks)){
  stk <- stks[i]
  species_aphia <- findAphia(unique(stksurveys_full[stksurveys_full$StockKeyLabel == stk,]$SpeciesScientificName), latin = TRUE)

  message(paste0("\n", stk))
  
  srvys <- stksurveys_full[stksurveys_full$StockKeyLabel == stk,]
  srvys.list <- unique(srvys$SurveyAcronymn)
  
  for(j in 1:length(srvys.list)){
    srv <- srvys.list[j]
    message(paste0("\n##########", "\n", srv, "\n", "##########\n"))
    
    files <- list.files(paste0(load.path, "Data/DR_Stocks/SurveyData/", stk, "/matures/"), pattern = paste0("*", srv, "*"), full.names = T)
    do.call(list, lapply(files, load, envir = .GlobalEnv))
    indices <- srvys[srvys$SurveyAcronymn == srv,]
    
    for(ind in 1:nrow(indices)){
      srvindx <- indices[ind,]
      index <- srvindx$SurveyIndex
      writeLines(paste0("Survey Index: ", index))
      
      yrs <- srvindx$YearStart:srvindx$YearEnd
      qrs <- srvindx$Quarter
      stk_divs <- unlist(strsplit(srvindx$Divisions, ", "))
      
      if(paste(stk_divs, collapse = ", ") == "NEA"){stk_divs = unique(ices_rect$Area_27)[!is.na(unique(ices_rect$Area_27))]}
      
      check <- hlhh %>%
        ungroup() %>%
        filter(Area_27 %in% stk_divs,
               Year %in% c(yrs),
               Quarter %in% c(qrs),
               HaulVal != "I") 
      
      n.data <- check %>%
        group_by(Year, Quarter) %>%
        count() %>%
        mutate(Quarter = as.character(Quarter),
               StockKeyLabel = stk,
               SurveyName = srv, 
               SurveyIndex = index,
               ValidAphia = species_aphia,
               AreaList = paste0(stk_divs, collapse = ", "))
      
      if (nrow(check) > 0) {
        # Centre of Gravity (CoG) and Inertia
        writeLines("CoG & Inertia")
        cginert <- coginis(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr,
                 cog = T, inertia = T, iso = F, density = T)
        
        # Extent of Occurrence (EOO)
        writeLines("EOO")
        
        eoo <- chullarea(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)[1:3]
        
        # Ellipse Area (ELA)
        writeLines("ELA")
        
        ela <- ellarea(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
        
        # Proportion of Presence
        # Rectangle (POPR)
        writeLines("POPR")
        
        popr <- pa_rect(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
        
        # Haul (POPH)
        writeLines("POPH")
        
        poph <- pa_haul(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
        
        # Gini index
        writeLines("Lorenz")
        
        lorenz <- lorenz_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
        writeLines("Gini")
        
        gni <- Gini(lorenz, matures = mtr)
        
        # D95
        writeLines("D95")
        
        D95 <- d95(lorenz)
        
        # Spreading Area (SA) & Equivalent Area (EA)
        writeLines("SA & EA")
        
        sa_data <- spreadingarea_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
        sa <- sa_data %>%
          group_by(Year) %>%
          summarise("Spreading Area" = spreadingarea_calc(TotalNoMature_Dur),
                    "Equivalent Area" = equivalentarea(TotalNoMature_Dur)) %>%
          mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% 
          relocate(Year, Quarter) 
        
        # Spread of Participation Index
        writeLines("SPI")
        
        SPI <- spi(hlhh, yrs, qrs, species_aphia, stk_divs)[c(1,2,4)]
        
        # Combine ouptuts
        df_list <- list(cginert, eoo, ela, popr, poph, gni, D95, sa, SPI) 
        
        sidf <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) %>%
                select(-c(nrects, nrects_p, no_haul.ids, pr_hauls)) %>% # remove some cols
                rename(EOO = convex_hull_area,
                       POPR = PosAreaR,
                       POPH = PosAreaH,
                       ELA = `Ellipse Area`,
                       SPI = SPI.dur,
                       SA = `Spreading Area`,
                       EA = `Equivalent Area`) %>%
          mutate(StockKeyLabel = stk,
                 SurveyName    = srv,
                 SurveyIndex   = index,
                 ValidAphia    = species_aphia,
                 AreaList      = paste0(stk_divs, collapse = ", ")) 
        
        # Add numer of data points in each yr/qr combo
        sidf <- full_join(sidf, n.data, by = c("Year", "Quarter", "StockKeyLabel", "SurveyName", "SurveyIndex", "ValidAphia", "AreaList")) %>%
          relocate(StockKeyLabel, SurveyName, SurveyIndex, ValidAphia, Year, Quarter, n)
        
        spatinds <- rbind(spatinds, sidf)
        
      } else {
        message(paste0("No data - filling row with NAs\n", stk, ": ", srv, "; ", index, "; Yrs ", paste0(range(yrs), collapse = "-"), ", Qrs ", paste0(qrs, collapse = ", ")))
        sidf <- data.frame("StockKeyLabel" = stk, "SurveyName" = srv, "SurveyIndex" = index, 
         "ValidAphia" = species_aphia, "Year" = NA, "Quarter" = qrs, "n" = NA,
         "CoG (x)" = NA, `CoG (y)` = NA, "Inertia" = NA, "EOO" = NA, "ELA" = NA,
         "POPR" = NA, "POPH" = NA, "Gini Index" = NA, "D95" = NA, 
         "SA" = NA, "EA" = NA, "SPI" = NA, "AreaList" = paste0(stk_divs, collapse = ", "), check.names = F)
        spatinds <- rbind(spatinds, sidf)
        print("###################2")
      }
    }
  }
}







View(spatinds)
suppressWarnings(dir.create(paste0(save.path, "Outputs/SpatInds"), recursive = TRUE))
save(spatinds, file = paste0(save.path, "Outputs/SpatInds/SpatialIndicators-", length(unique(spatinds$StockKeyLabel)), "-Stks.rds"))














# Merge all outputs into a single df
sidf <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) %>%
  select(-c(nrects, nrects_p, no_haul.ids, pr_hauls)) %>% # remove some cols
  rename(EOO = convex_hull_area,
         POPR = PosAreaR,
         POPH = PosAreaH,
         ELA = `Ellipse Area`,
         SPI = SPI.dur,
         SA = `Spreading Area`,
         EA = `Equivalent Area`) %>%
  na.omit() %>%
  tidyr::pivot_longer(cols = 3:14, names_to = "Indicator", values_to = "Value") %>%
  mutate(Type = case_when(
    Indicator %in% loc  ~ "Location",
    Indicator %in% ran  ~ "Range",
    Indicator %in% occ  ~ "Occupancy",
    Indicator %in% agg  ~ "Aggregation")) %>%
  mutate(Indicator = factor(Indicator, levels = c(loc, ran, occ, agg)))

ttl <- function(species, stk_divs, srv, qrs, yrs){
  paste0(species[1], " (", species[2], ") in ", paste0(stk_divs, collapse = ", "), "\n", srv, " (Q", qrs, ") ", min(yrs), " - ", max(yrs))}

indplot <- ggplot(data = sidf, aes(x = Year, y = Value)) +
  geom_line(aes(colour = Type)) +
  scale_x_continuous(breaks = seq(from = min(unique(sidf$Year)), to = max(unique(sidf$Year)), by = 2)) +
  facet_wrap(vars(Indicator), scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)
  ) +
  labs(title = "Spatial Indicator Timeseries",
       subtitle = ttl(species = stk, stk_divs, srv, qrs, yrs)) +
  ylab("Indicator Value")
indplot
