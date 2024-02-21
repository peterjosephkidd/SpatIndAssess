#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               4. Calculate Spatial Indicators
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)

rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess"
source(paste0(load.path, "/Functions/dataprep_funs.R")) # for dealing with datras data
source(paste0(load.path, "/Functions/spatinds_funs.R")) # for computing spatial indicators
source(paste0(load.path, "/Functions/ROC_funs.R"))      # for ROC and TSS