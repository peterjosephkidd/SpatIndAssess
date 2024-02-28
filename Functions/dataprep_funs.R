#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
prepsurveydata <- function(hh, hl, ca, ices_rect){
#>>>>>>>>>>>>>>>>>>>>>>>>>>>> prepsurveydata >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> Basic cleaning of datras exchange data. Outputs a list of datasets.
#> Includes hh,hl,ca, hlhh. hlhh dataset combines hl and hh data. 
#> Also adds ICES Area_27 to dataset based on ICES rectangles. 
#> Useful for filtering data to species stock division (e.g.ple.27.7d)
#> ICES rectangles come from ICES shapefile.
#> hlhh data is the datset used in most later funs. Other datasets kept for 
#> for ease of access
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

  message("Running unique() to get rid of duplicate rows")
  hh2 <- unique(hh)
  hl <- unique(hl)
  ca <- unique(ca)
  
  # Subset DATRAS by Region
  # Which ICES rectangles belong to which ICES division 
  message("Identifying Divisions for ICES Rectangles")
  area_div <- dplyr::distinct(ices_rect[c("ICESNAME", "Area_27", "Shape_Area")])
  # merge this with the DATRAS `HH` data. We will use the 
  hh <- merge.data.frame(hh2,     
                         area_div,
                         by.x = "StatRec",
                         by.y = "ICESNAME")
  # check that the merge did not result in multiplication of rows
  if(!(nrow(hh) == nrow(hh2))){
    warning("Merge not correct. Number of rows between old and new dataset are not identical")
  }
  rm(hh2)
  
  # Create lon_lat to see unique combinations using Shoot
  hh$lon_lat <- paste0(hh$ShootLong, "_", hh$ShootLat)
  
  # Remove rows of data where TotalNo < 0
  # -9 is used as a placeholder for NA. Remove these.
  message("Removing rows in hl where TotalNo < 0")
  hl <- subset(hl, TotalNo >= 0)
  
  # HLHH
  message("Creating hlhh data")
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
  
  # Merge location data from HH to HL species data
  ## check which columns are identical 
  cols <- janitor::compare_df_cols(hh, hl)
  na.omit(cols[cols$hh == cols$hl,])
  ## Subset HH data to what we want
  m <- hh[c("haul.id", "Year", "Quarter", "Month", "Survey","Country", 
            "Ship", "Gear", "GearEx", "DoorType", "HaulDur", "HaulNo", 
            "StNo", "SweepLngt", "StatRec", "Area_27", "ShootLong", "ShootLat", "lon_lat", "HaulVal", "Depth")]
  ## Merge	
  hlhh <- merge(hl,
                dplyr::distinct(m),
                c("haul.id", "Year", "Quarter", "HaulNo", "StNo", "Gear", "GearEx", "DoorType","Ship", 
                  "SweepLngt", "Country"))
  
  #message("Some data might be removed after merging hh and hl. This is typically due to haul.id's in HL not being in HH. Check which rows were removed, if any, by using the following code:")
  #print("anti_join(SURVEY_NAME.data$hl, SURVEY_NAME.data$hlhh, by = c('haul.id','Year','Quarter','Country','Ship','Gear','SweepLngt','GearEx','DoorType','StNo','HaulNo','TotalNo','HaulDur'))",
  #      quote = FALSE)
  
  ## create list of datasets
  data.list <- list(hh = hh, 
                    hl = hl, 
                    ca = ca, 
                    hlhh = hlhh)
  return(data.list)
}

?setdiff

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
filtersurveydata <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
#>>>>>>>>>>>>>>>>>>>>>>>>>>>> filtersurveydata >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> Filter data to the years, quarters, species, and stock divisions desired
#> Resulting dataset can then be passed to spatial indicator functions in 
#> spatinds_funs.R.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  ##Get all hauls sampled in the given years, quarters, and stock divisions
  d <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") %>% # remove invalid hauls
	distinct()
  # Check years and quarters provided are available
  if(!identical(as.numeric(unique(d$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(d$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(d$Quarter)), setdiff(unique(d$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(d$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(d$Year)), setdiff(unique(d$Year), yrs)), ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(d$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
 return(d)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
makeReadableFortify <- function(shapefile) {
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Spatial fortify >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> custom function to convert shapefiles to dataframe retaining all important 
#> information including coordinates
#>(function from Zach Radford)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  shapefile@data$id <- rownames(shapefile@data)
  shapefile.points <- fortify(shapefile)
  shapefile.df <- left_join(shapefile.points, shapefile@data, by = "id")
  return(shapefile.df)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
file_metadata <- function(url) {
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Spatial fortify >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> Gets metadata from GitHub 
#> Code from: 
#> https://stackoverflow.com/questions/73952017/get-metadata-on-csv-file-in-a-github-repo-with-r-i-e-file-info-but-for-onlin
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  
  page <- read_html(url)
  
  file <- tail(strsplit(url, "/")[[1]], 1)
  div1 <- "text-mono f6 flex-auto pr-3 flex-order-2 flex-md-order-1"
  
  size <- page %>%
    html_elements(xpath = paste0("//div[@class='", div1, "']")) %>%
    html_text() %>%
    strsplit("\n") %>%
    sapply(trimws) %>%
    getElement(5)
  
  last_commit <- page %>% 
    html_elements("relative-time") %>% 
    html_attr("datetime") %>%
    as.POSIXct()
  
  data.frame(file, size, last_commit)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
filter_to_matures <- function(data, species = "Target", species_aphia, L50, is.hlhh = TRUE){
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> data: can be CA, HL or HLHH. Intended for HLHH
#> species: character string (can be anything, is just used for reference)
#> species_aphia: Valid_Aphia of the species
#> L50: the length below which are masked out, to identify matures
#> is.hlhh: boolean
#> 
#> Note: data is not actually filtered, but produces a mask to allow filtering
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  
  data$TrgtSpcs <- species
  data$TrgtSpcsL50 <- if_else(data$Valid_Aphia == species_aphia, L50, NA)
  data$TrgtSpcsMature <- if_else(data$LngtClass >= data$TrgtSpcsL50 & data$Valid_Aphia == species_aphia, 1, if_else(data$LngtClass < data$TrgtSpcsL50 & data$Valid_Aphia == species_aphia, 0, NA))
  
  if(is.hlhh == TRUE){
    data <- data %>% 
      group_by(haul.id, Valid_Aphia) %>%
      mutate(TotalNoMature = sum(HLNoAtLngt[TrgtSpcsMature == 1]),
             PropMature = TotalNoMature/TotalNo)
  }
  
  return(data)
  
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
namefile <- function(stk = NA, data, f = NA, r = NA, ext = ".rds"){
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
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