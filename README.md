# Empirical Spatial Indicator Assessment
Project to develop an empirical stock assessment using spatial indicators of species distribution.

### DR_Stock_Project
Initial project looking at the ability of spatial indicators to classificy stock status of 10 data-rich stocks, using survey data.
* **1_Download_Survey_Data**: Script for downloading and handling Datras survey data
* **2_ROC_Assessment**:       Script for loading in survey data, computing ROC curves and TSS plots

### Data
* **ICES Rect, ICES Divs**:   ICES shapefiles for rectangles and divisions
* **DR_Stocks**:              Advice sheets and stock assessment objects for 10 data-rich stocks

### Functions
* **ROC_funs**:               Functions for clauclating and plotting ROC curves and true skill score (TSS)
* **dataprep_funs**:          Functions for handling ICES Datras survey data
* **spatinds_funs**:          Functions for calculating and plotting spatial indicators 
