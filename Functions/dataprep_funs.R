################################################################################
######################## Prepare DATRAS Survey Data ############################
################################################################################
prepsurveydata <- function(hh, hl, ca, ices_rect){
  tictoc::tic()
  message("Running unique() to get rid of duplicate rows")
  hh2 <- unique(hh)
  hl <- unique(hl)
  ca <- unique(ca)
  tictoc::toc()
  # we dont wat to do this for ca data 
  # ca data can be duplicated if a species of the same length is sampled more than once
  
  ### Subset DATRAS by Region
  # We need to make the code so that we can subset the later maps and P-A 
  # calculation by areas. To do this, we need to work out which rectangles belong 
  # to which division and merge this with the DATRAS `HH` data. We will use the 
  # ICES shapefiles and the ices data frames  created above
  # record type.
  tictoc::tic()
  message("Getting location information from ICES shp")
  
  area_div <- dplyr::distinct(ices_rect[c("ICESNAME", "Area_27", "Shape_Area")])
  
  hh <- merge.data.frame(hh2,     
                         area_div,
                         by.x = "StatRec",
                         by.y = "ICESNAME")
  message("Checking merge")
  ### check that the merge did not result in multiplication of rows
  if(!(nrow(hh) == nrow(hh2))){
    warning("Merge not correct. Number of rows between old and new dataset are not identical")
  }
  rm(hh2)
  tictoc::toc()
  
  # Create lon_lat to see unique combinations later
  ## using Shoot
  hh$lon_lat <- paste0(hh$ShootLong, "_", hh$ShootLat)
  
  ## remove rows of data where TotalNo < 0
  # identified this issue after funky Lorenz plots and TSS for D95 and Gini in turbot
  # where many years of data had -9 TotalNo
  # was there a reason that it was always -9??? (SNS survey)
  message("Removing rows in hl where TotalNo < 0")
  hl <- subset(hl, TotalNo >= 0)
  
  tictoc::tic()
  message("Merging hl and hh data into hlhh")
  # Create a Haul ID
  hh$haul.id <- as.character(paste(hh$Year, 
                                   hh$Quarter, 
                                   hh$Country, 
                                   hh$Ship, 
                                   hh$Gear, 
                                   hh$StNo, 
                                   hh$HaulNo, 
                                   sep = ":"))
  hl$haul.id <- as.character(paste(hl$Year, 
                                   hl$Quarter, 
                                   hl$Country, 
                                   hl$Ship, 
                                   hl$Gear, 
                                   hl$StNo, 
                                   hl$HaulNo, 
                                   sep = ":"))
  
  ## merge location data from HH to HL species data
  # check which columns are identical 
  cols <- janitor::compare_df_cols(hh, hl)
  na.omit(cols[cols$hh == cols$hl,])
  m <- hh[c("haul.id", "Year", "Quarter", "Month", "Survey","Country", 
            "Ship", "Gear", "GearEx", "DoorType", "HaulDur", "HaulNo", 
            "StNo", "SweepLngt", "StatRec", "Area_27", "ShootLong", "ShootLat", "lon_lat", "HaulVal", "Depth")]
  hlhh <- merge(hl,
                dplyr::distinct(m),
                c("haul.id", "Year", "Quarter", "HaulNo", "StNo", "Gear", "GearEx", "DoorType","Ship", 
                  "SweepLngt", "Country"))
  
  message("Some data might be removed after merging hh and hl. This is typically due to haul.id's in HL not being in HH. Check which rows were removed, if any, by using the following code:")
  print("anti_join(SURVEY_NAME.data$hl, SURVEY_NAME.data$hlhh, by = c('haul.id','Year','Quarter','Country','Ship','Gear','SweepLngt','GearEx','DoorType','StNo','HaulNo','TotalNo','HaulDur'))",
        quote = FALSE)
  
  ## create list of datasets
  data.list <- list(hh = hh, 
                    hl = hl, 
                    ca = ca, 
                    hlhh = hlhh)
  tictoc::toc()
  return(data.list)
}


################################################################################
############################## Spatial fortify #################################
################################################################################

## custom function to convert shapefiles to dataframe retaining all important information including coordinates
# (function from Zach Radford)
makeReadableFortify <- function(shapefile) {
  shapefile@data$id <- rownames(shapefile@data)
  shapefile.points <- fortify(shapefile)
  shapefile.df <- left_join(shapefile.points, shapefile@data, by = "id")
  return(shapefile.df)
}
