#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                    Spatial indicator functions
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                                   SPI 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> This function calculate SPI based on the observed catches. The are argument
#> can be used to weight areas in circumstances of unequal sized areas
#> Since ICES rectangles have the same area, this should be set to 1 so that all
#> rectangles are given equal weight
#> 
#> Index from Plowman, 2003:
#> https://doi.org/10.1016/S0168-1591(03)00142-4
#> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

spi <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  # Filter data
  s1 <- hlhh %>%
    filter(Area_27 %in% stk_divs, 
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") %>% # remove invalid hauls %>%
    distinct() # remove duplicates 
  
  # Check what data is available
  if(!identical(as.numeric(unique(s1$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(s1$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(s1$Quarter)), setdiff(unique(s1$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(s1$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(s1$Year)), setdiff(unique(s1$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(s1$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  
  # All rectangles sampled each year and the total duration of hauls within them
  rectsamp <- s1 %>% 
    select(Year,StatRec,HaulDur) %>%
    group_by(Year, StatRec) %>%
    summarise(TotalHaulDur = sum(HaulDur))
  
  # Fo: observed frequency -- catches in each rectangle/area
  observed <- s1 %>% 
    filter(Valid_Aphia == species_aphia) %>%
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, Area_27, TotalNo, HaulVal, HaulDur) %>%
    distinct() %>%
    group_by(Year, StatRec) %>%
    summarise(Fo = sum(TotalNo),
              TotSpcsHaulDur = sum(HaulDur),
              Fo.Dur = sum(TotalNo)/sum(HaulDur))
  
  # Merge and divide the total number of catches by total duration hauled in each rectangle
  observed <- left_join(rectsamp, observed, by = c("Year", "StatRec")) # left_join keeps the rectangles where there were no species observations as NAs
  observed$Fo[is.na(observed$Fo)] <- 0 # change NAs in Fo to 0
  observed$TotSpcsHaulDur[is.na(observed$TotSpcsHaulDur)] <- 0 # change NAs in Fo to 0
  observed$Fo.Dur[is.na(observed$Fo.Dur)] <- 0 # change NAs in Fo to 0
  
  observed <- observed %>%
    mutate(Fo.Dur2 = Fo/TotalHaulDur) # standardise 
  
  # Total recorded observations 
  TotalObs <- observed %>%
    group_by(Year) %>%
    summarise(TotRecObs = sum(Fo),
              TotRecObs.Dur = sum(Fo.Dur),
              TotRecObs.Dur2 = sum(Fo.Dur2))
  TotalObs <- left_join(observed, TotalObs, by = "Year")
  
  # What percentage of the total surveyd area do each rectangle take up?
  # Assumming equal areas of rectangles, by the total number of rectangles
  AreaPerRect <- s1 %>%
    select(Year, StatRec) %>%
    group_by(Year) %>%
    summarise(N.rects = length(unique(StatRec))) %>%
    mutate(PercArea = 1/N.rects*100) # Percetnage area of each rectangle 
  
  # Fe = total recorded obs x (%area/100) -- (for equal areas)
  fefo <- left_join(TotalObs, AreaPerRect, by = "Year") %>%
    mutate(Fe = TotRecObs*(PercArea/100),
           Fe.Dur = (TotRecObs.Dur)*(PercArea/100),
           Fe.Dur2 = (TotRecObs.Dur2)*(PercArea/100),
           `fo-fe` = abs(Fo.Dur - Fe.Dur),
           `fo-fe2` = abs(Fo.Dur2 - Fe.Dur2))
  
  # fo-fe
  spi1 <- fefo %>%
    group_by(Year) %>%
    summarise(sumFoFe = sum(`fo-fe`),
              sumFoFe2 = sum(`fo-fe2`))
  spi2 <- left_join(TotalObs, spi1)
  
  # fe min
  femin <- fefo %>%
    group_by(Year) %>%
    select(Year, Fe.Dur, Fe.Dur2) %>%
    summarise(Femin = min(Fe.Dur),
              Femin2 = min(Fe.Dur2))
  
  # SPI
  spi3 <- left_join(spi2, femin) %>%
    mutate(twoN.femin = 2*(TotRecObs.Dur - Femin),
           twoN.femin2 = 2*(TotRecObs.Dur2 - Femin2),
           SPI = 1-sumFoFe/twoN.femin,
           SPI2 = 1-sumFoFe2/twoN.femin2) %>%
    select(Year, SPI, SPI2) %>%
    distinct() %>%
    mutate(Quarter = paste(sort(unique(s1$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter)
  
  return(spi3)
}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                              PosArea_Rect                                
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> The proportion of ICES rectangles where species were fond at least once.
#> Calculated per year to give annual time series. Value of 1 indicates that 
#> all rectangles were occupied. Value of 0 indicates that no rectangles were
#> occupied. Indicator is bounded by 0 and 1
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

pa_rect <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  ### Filter data
  n.rects2 <- hlhh %>%
    filter(Area_27 %in% stk_divs, 
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")# remove invalid hauls
  
  # Check what data is available
  if(!identical(as.numeric(unique(n.rects2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(n.rects2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(n.rects2$Quarter)), setdiff(unique(n.rects2$Quarter), qrs)), collapse = ", "), "\n")
  }
  if(!identical(as.numeric(sort(unique(n.rects2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(n.rects2$Year)), setdiff(unique(n.rects2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(n.rects2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"))
  }
  
  ### How many rectangles sampled per year?
  n.rects <- n.rects2 %>%
    group_by(Year) %>%
    summarise(nrects = length(unique(StatRec))) %>%
    mutate(Quarter = paste(sort(unique(n.rects2$Quarter)), collapse = ", "))
  
  ### How many rectangles sampled per year where species were present?
  p.rects <- n.rects2 %>%
    filter(Valid_Aphia == species_aphia) %>% # remove invalid hauls
    group_by(Year) %>%
    filter() %>%
    summarise(nrects_p = length(unique(StatRec))) %>%
    mutate(Quarter = paste(sort(unique(n.rects2$Quarter)), collapse = ", "))
  
  ### Merge but keep years where no species were found
  np.rects <- left_join(n.rects, p.rects, by = c("Year", "Quarter"))
  np.rects$nrects_p[is.na(np.rects$nrects_p)] <- 0 # change NA to 0 
  np.rects$Year <- as.numeric(as.character(np.rects$Year))
  np.rects$PosAreaR <- np.rects$nrects_p/np.rects$nrects
  
  ### Return
  return(np.rects)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
###                              PosArea_Haul                                ###
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

pa_haul <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  ### Filter data
  n.hauls2 <- hlhh %>% 
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") # remove invalid hauls
  
  # Check what data is available
  if(!identical(as.numeric(unique(n.hauls2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(n.hauls2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(n.hauls2$Quarter)), setdiff(unique(n.hauls2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(n.hauls2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(n.hauls2$Year)), setdiff(unique(n.hauls2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(n.hauls2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  
  ### How many hauls in each year?
  n.hauls <- n.hauls2 %>%
    group_by(Year) %>%
    summarise(no_haul.ids = length(unique(haul.id))) %>%
    mutate(Quarter = paste(sort(unique(n.hauls2$Quarter)), collapse = ", "))
  
  ### How many hauls in each year where species were present?
  p.hauls <- n.hauls2 %>% 
    filter(Valid_Aphia == species_aphia) %>% # remove invalid hauls
    group_by(Year) %>%
    summarise(pr_hauls = length(unique(haul.id))) %>%
    mutate(Quarter = paste(sort(unique(n.hauls2$Quarter)), collapse = ", "))
  
  ### Merge but keep years where no species were found
  np.hauls <- left_join(n.hauls, p.hauls, by = c("Year", "Quarter"))
  np.hauls$pr_hauls[is.na(np.hauls$pr_hauls)] <- 0 # change NA to 0 
  np.hauls$Year <- as.numeric(as.character(np.hauls$Year))
  np.hauls$PosAreaH <- np.hauls$pr_hauls/np.hauls$no_haul.ids
  
  ### return
  return(np.hauls)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#                                Create Lorenz Data                        
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> To calculate Gini Index and D95 we must calculate data needed to form a 
#> Lorenz curve. Then we can plot this curve and derive our spatial indicators
#> 
#> Edits needed:
#>  Lorenz outputs and data is actually calculated per haul not per rectangles
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

lorenz_data <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  ### Get all hauls sampled in the given years, quarters, and stock divisions
  allspcs_lor2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I") # remove invalid hauls
  # Checks
  if(!identical(as.numeric(unique(allspcs_lor2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(allspcs_lor2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(allspcs_lor2$Quarter)), setdiff(unique(allspcs_lor2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(allspcs_lor2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(allspcs_lor2$Year)), setdiff(unique(allspcs_lor2$Year), yrs)), ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(allspcs_lor2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  
  allspcs_lor <- allspcs_lor2 %>%
    select(haul.id, Year, Quarter, StatRec, HaulDur) %>%
    distinct() %>%
    na.omit() %>%
    # Create some artificial variables. We want to include all hauls conducted, 
    # not just the ones where target species were found
    mutate(TotalNo = as.numeric(0), # add 0 TotalNo 
           Valid_Aphia = species_aphia) %>% # add species aphia to all data
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur) # rearrange cols
  # Checks
  if(all(unique(allspcs_lor$Year), unique(hlhh$Year)) == FALSE){
    warning("Not all survey years were retained in `allspcs_lor`\n", immediate. = TRUE)
  }
  if(any(duplicated(allspcs_lor$haul.id))==TRUE){
    warning("Some rows of haul.id are duplicated in `allspcs_lor`\n", immediate. = TRUE)
  }
  
  ### Filter to hauls where our target species were found 
  spcs_lor2 <- allspcs_lor2 %>%
    filter(Valid_Aphia == species_aphia)

  # Restrict dataset to what we need   
  spcs_lor <- spcs_lor2 %>%
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur) %>%
    distinct() %>%
    na.omit() %>%
    mutate(TotalNo = as.numeric(TotalNo)) %>% #, 
           #Quarter = paste(sort(unique(spcs_lor2$Quarter)), collapse = ", ")) %>%
    relocate(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur)

  
  if(janitor::compare_df_cols_same(allspcs_lor, spcs_lor)==FALSE){
    warning("Columns in `allspcs_lor` and `spcs_lor` do not match.\n", immediate. = TRUE)
  }
  # Join the zero dataset to the presence dataset, but only the combinations of
  # haul.id, year, quarter, and statrec that are not already in the present dataset
  # essentially, this creates a full dataframe of all survey hauls, both where 
  # target species were found and not found. 
  # bind hauls where species were not found to data where species present data
  # hauls where species were not found will now have the haul.id with TotalNo = 0
  spcsmissing <- anti_join(allspcs_lor[1:4], spcs_lor) # the rows not in the species dataset
  spcsmissing <- filter(allspcs_lor, allspcs_lor$haul.id %in% spcsmissing$haul.id == TRUE)

  lorenz <- bind_rows(spcs_lor, spcsmissing) %>%
    #select(-Quarter, -haul.id) %>%
    distinct() %>%
    arrange(Year) %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur) %>% # standardise by haul duration
    arrange(Year, TotalNo_Dur) %>% # order desc
    group_by(Year) %>%
    mutate(CumSum = cumsum(TotalNo_Dur),
           rect_num = row_number(),
           cumsum_prop = CumSum/max(CumSum),
           rect_num_prop = row_number()/max(row_number()))
  lorenz$Year <- as.numeric(as.character(lorenz$Year))
  
  if(suppressWarnings(all(unique(lorenz$Year), unique(hlhh$Year)) == FALSE)){
    warning("Not all survey years were retained in `lorenz`.\n", immediate. = TRUE)
  }
  return(lorenz)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                              Plot Lorenz Curve
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> Feed the output of lorenz_data into this function to get lorenz curves 
#> for each survey year
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
lorenz_plot <- function(lorenz_data){
  lplot <- ggplot(data = lorenz_data, aes(x = rect_num_prop, y = cumsum_prop)) + 
  geom_line(aes(group = Year, colour = Year), alpha = 0.3, linewidth = 2) +
  geom_abline() +
  geom_vline(xintercept = 0.95, lty = 2, colour = "black", linewidth = 1) +
  coord_cartesian(ylim= c(0,1), xlim = c(0,1), expand = FALSE) +
    labs(title = paste0("Lorenz Curve: Distribution of ", species, " (", min(lorenz_data$Year),":", max(lorenz_data$Year), ", Q", paste(sort(unique(lorenz$Quarter)), collapse = ", "), ")"), 
         subtitle = "Dashed line = Proportion of population observed within 95% of rectangles (D95)",
x = "Culmuative Number of Rectangles (%/100)", 
y = "Culmuative Sum of Species Counts (%/100)") +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))+
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none")

  return(lplot)
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                              Gini Index 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> Gini Index is the area between the Lorenz curve and the linear "line of 
#> equality". Typically a high score means that there is high inequality. In 
#> other words, only a few ICES rectangles are repsonsible for the majority
#> of the total catch. On the other hand, low values indicate that the total 
#> catch is spread across ICES rectangles equally. 
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
Gini <- function(lorenz){
    G <- lorenz %>%
      group_by(Year) %>%
      summarise('Gini Index' = ineq(TotalNo_Dur, type = "Gini")) %>%
      mutate(Quarter = paste(sort(unique(lorenz$Quarter)), collapse = ", ")) %>%
      relocate(Year, Quarter)
    G$'Gini Index' <- 1- G$'Gini Index' # take inverse, higher = more distributed
    G$'Gini Index'[is.nan(G$'Gini Index')] <- 0 # change NaNs to 0
    G$Year <- as.numeric(as.character(G$Year))
    return(G)
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                                 D95
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> The proportion of the population observed within 95% of rectangles
#> The point where the solid red line in Lorenz plot 
#> intersects the Lorenz curve for each year
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

d95 <- function(lorenz){
  D95_d <- data.frame()
  for(yr in unique(lorenz$Year)){
    lorenz_t3 <- subset(lorenz, lorenz$Year == yr)
    L <- Lc(lorenz_t3$TotalNo_Dur)
    # Get the index  of where cumsum of total pop is above or equal to 95%
    D95_index <- which(L$p >= 0.95)[1]
    D95_cumsum <- L$p[D95_index]
    # Get D95 value of proportion of rectangles supporting 95% of population
    D95 <- L$L[D95_index]
    output <- c(yr, D95_index, D95_cumsum, D95)
    D95_d <- rbind(D95_d, output)
  }
  colnames(D95_d) <- c("Year", "Index_loc", "D95_cumsum", "D95")
  D95_d$D95[is.nan(D95_d$D95)] <- 0 # change NaNs to 0
  D95_d$Year <- as.numeric(as.character(D95_d$Year)) 
  D95_d <- D95_d %>%
    mutate(Quarter = paste(sort(unique(lorenz$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter, Index_loc, D95_cumsum, D95)
  return(D95_d)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                             Spreading Area                               
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> Similar to Lorenz curve an Gini index but has no influence from zeros


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                          Spreading Area Data Prep
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> Function to get the total number of catches divided by the haul duration
#> for each haul
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
spreadingarea_data <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  sa2 <- hlhh %>%
      filter(Area_27 %in% stk_divs,
             Year %in% c(yrs),
             Quarter %in% c(qrs),
             HaulVal != "I") # remove invalid hauls
  
  # Check what data is available
  if(!identical(as.numeric(unique(sa2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(sa2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(sa2$Quarter)), setdiff(unique(sa2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(sa2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(sa2$Year)), setdiff(unique(sa2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(sa2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  
  sa <- sa2 %>%
      select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>%
      mutate(TotalNo = as.numeric(TotalNo), 
             Quarter = paste(sort(unique(sa2$Quarter)), collapse = ", ")) %>%
      filter(Valid_Aphia == species_aphia) %>%
      relocate(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur) %>%
      distinct() %>%
      na.omit() %>%
      mutate(TotalNo_Dur = TotalNo/HaulDur) %>% #standardise by haul duration
      arrange(Year, desc(TotalNo_Dur))
  return(sa)
}
    
# old code ---  
#  sa <- sa2 %>%
#    arrange(Year) %>%
#    mutate(TotalNo_Dur = TotalNo/HaulDur) %>% # standardise by haul duration
#    arrange(Year, desc(TotalNo_Dur)) %>% # order asce
#    group_by(Year) %>%
#    mutate("Q(T)" = cumsum(TotalNo_Dur),
#           "T" = row_number(),
#           "Tprop" = row_number()/max(row_number()),
#           "Q" = sum(TotalNo_Dur),
#           "(Q-Q(T)/Q" = (Q-`Q(T)`)/Q,
#           "n" = length(Q))
# old code ---  


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                          Spreading Area Calculation
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> Routine from EU program Fisboat, DG-Fish, STREP n° 502572
#> Authors : M.Woillez and J.Rivoirard (Mines-ParisTech)
#> Last update : 01 march 2008 
#>
#> Arguments:
#> z     variable of interest (i.e. fish density)
#> w     appropriate areas of influence set as weighted factors
#> plot  if TRUE, the curve expressing (Q-Q(T))/Q as a function of T is 
#>       plotted with T the cumulated area occupied by the density values,
#>       ranked in decreasing order, Q(T) the corresponding cumulated abundance,
#>       and Q the overall abundance. The spreading area SA (expressed in square
#>       nautical miles) is then simply defined as twice the area below this 
#>       curve
#>       
#>                              !!!NOT MY CODE!!!
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
spreadingarea_calc <- function(z, w = NA, plot = F){
  # extract data
  nb <-length(z)
  
  # sort data in increasing order
  #zi <- sort(z,index.return=T)
  z <- sort(z)
  
  if(is.na(w)){
    w <- rep(1, length(z))
  } else{
    w <- w[order(sort(z))]
  }
  
  # computation of the spreading area 
  Q <- sum(z*w)
  QT <- c(0,cumsum(z*w))
  SA <- sum((QT[1:nb]+QT[2:(nb+1)])*w)/Q
  
  # computation of (Q-Q(T))/Q as a function of T
  T <- c(0,cumsum(w))
  T <- T[nb+1] - T
  T <- rev(T)
  Tprop <- T/max(T)
  QT <- QT[nb+1] - QT
  QT <- rev(QT)
  
  # display
  if(plot)
    plot(T, (Q-QT)/Q, main="Curve (Q-Q(T))/Q", type="o", pch="+")
  
  
  x <- Tprop
  y <- (Q-QT)/Q
  id <- order(x)
  auc <- sum(diff(x[id])*rollmean(y[id],2))
  
  # outputs
  return(SA)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                             Equivalent Area                                
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> Routine from EU program Fisboat, DG-Fish, STREP n° 502572
#> Authors : M.Woillez and J.Rivoirard (Mines-ParisTech)
#> Last update : 01 march 2008 
#>
#> Arguments:
#> z     variable of interest (i.e. fish density)
#> w     appropriate areas of influence set as weighted factors
#>
#>                              !!!NOT MY CODE!!!
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
equivalentarea <- function(z, w = 1){
  EA <- sum(z*w,na.rm=T)^2 / sum(z^2*w,na.rm=T)
    return(EA)
  }


## for me, the areas of influence accounts for biased smapling, i.e. if some rectangles are sampled more than others.
## The AOI is then used as a weighting factor
## But i have already resolved this by dividing catch by haul duration 
## However i have not resolved this for the binary indicators

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                             Convex Hull Area                                 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                                                                             
#>This function creates a polygon (the onvex hull) around data points and then 
#> calculates the area of this polygon. This is an index of range extent        
#>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

# Load the grDevices and rgeos packages
library(grDevices)
library(rgeos)
library(sp)
library(sf)

chullarea <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  chull_data2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")
  
  # Check what data is available
  if(!identical(as.numeric(unique(chull_data2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(chull_data2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(chull_data2$Quarter)), setdiff(unique(chull_data2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(chull_data2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(chull_data2$Year)), setdiff(unique(chull_data2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(chull_data2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  
  chull_data <- chull_data2 %>%
    filter(Valid_Aphia == species_aphia) 
  
  # get world map
  world <- map_data("world")
  worldsf <- sfheaders::sf_polygon(
    obj = world
    , x = "long"
    , y = "lat"
    , polygon_id = "group"
  ) 
  
  chull <- data.frame()
  # Pseudo-dataset (latitude and longitude coordinates)
  for(yr in yrs){
    chull_yearly <- chull_data %>%
      filter(Year == yr)
    longitude <- chull_yearly$ShootLong
    latitude <- chull_yearly$ShootLat
    
    # Combine latitude and longitude into a matrix
    coordinates <- cbind(longitude, latitude)
    # Geometries - closed shapes must have at least 4 rows
    if(nrow(coordinates) <= 4){
      convex_hull_area <- 0
      areaoccupied <- 0
    } else{
      # Calculate the convex hull
      convex_hull <- chull(coordinates)
      # Extract the convex hull points
      convex_hull_points <- as.data.frame(coordinates[convex_hull, ])
      
      # Create a SpatialPolygons object from the convex hull points
      convex_hull_sf <- sfheaders::sf_polygon(
        obj = convex_hull_points
        , x = "longitude"
        , y = "latitude") 
      # Calculate area of the convex hull
      convex_hull_area <- st_area(convex_hull_sf)
      
      # get area of convex hull minus area of intersecting land
      land <- st_intersection(convex_hull_sf, worldsf)
      landarea <- sum(st_area(land))
      areaoccupied <- convex_hull_area - landarea
    }
    outputs <- cbind(yr, convex_hull_area, areaoccupied)
    chull <- rbind(chull, outputs)
  }
  # currently scaled by diising by max value. 
  # But could be scaled by dividing by the area of the whole stock region
  # i.e. the true possible maximum. That way if the indictaor = 1 then know that 
  # the stock is dispersed across the whole region. 
  StkArea <- ices_rect %>% 
    filter(Area_27 %in% stk_divs)
  StkArea <- length(unique(StkArea$ICESNAME))*0.5
  
  chull <- chull %>% 
    mutate(convex_hull_area_scaled = convex_hull_area/StkArea,
           areaoccupied_scaled = areaoccupied/StkArea,
           Quarter = paste(sort(unique(chull_data2$Quarter)), collapse = ", ")) %>%
    relocate(yr, Quarter) %>%
    rename("Year" = yr)
  return(chull)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>              Centre of Gravity and Inertia                                     
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                                                                              
#> Function for calculating centre of gravity (x and y), inertia, and the area  
#> of the inertia ellipse around the centre of gravity                          
#>                                                                              
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

#>>>>>>>>>>>>>>>>>>> Centre of Gravity
cog <- function(longitude, latitude){
  cog_xy <- c(mean(longitude), mean(latitude))
  return(cog_xy)
}

#>>>>>>>>>>>>>>>>>>> Inertia
inertia <- function(longitude, latitude){
  ### Centre of gravity
  cg_x <- mean(longitude)
  cg_y <- mean(latitude)
  ### Inertia
  inert <- sum(abs(longitude)-cg_x)^2 + sum(abs(latitude)-cg_y)^2
  return(inert)
}

#>>>>>>>>>>>>>>>>>>> Centre of Gravity, Inertia, Area of Ellipse
coginert <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  cog_data2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")
  # Check what data is available
  if(!identical(as.numeric(unique(cog_data2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
", "Only data for quarters ", paste(sort(unique(cog_data2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
", "No data for quarter ", paste(c(setdiff(qrs, unique(cog_data2$Quarter)), setdiff(unique(cog_data2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(cog_data2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
", "No data for years ", paste(c(setdiff(yrs, unique(cog_data2$Year)), setdiff(unique(cog_data2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(cog_data2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  cog_data <- cog_data2 %>% 
    filter(Valid_Aphia == species_aphia) # remove invalid hauls
  
  cgi <- data.frame()
  # Pseudo-dataset (latitude and longitude coordinates)
  for(yr in yrs){
    cog_yearly <- cog_data %>%
      filter(Year == yr)
    longitude <- cog_yearly$ShootLong
    latitude <- cog_yearly$ShootLat
    ### Centre of gravity
    cg_x <- mean(longitude)
    cg_y <- mean(latitude)
    ### Inertia
    inertia <- sum(abs(longitude)-cg_x)^2 + sum(abs(latitude)-cg_y)^2
    ### Area of Inertia
    # Create a 2x2 covariance matrix
    cov_matrix <- cov(cbind(longitude, latitude))
    if(any(is.na(cov_matrix))){
      area_of_ellipse <- NA
    } else{
      # Calculate the eigenvalues and eigenvectors of the covariance matrix
      eigen_result <- eigen(cov_matrix)
      eigenvalues <- eigen_result$values
      # Calculate the semi-major and semi-minor axes (2 standard deviations)
      # The factor 5.991 represents the Chi-square likelihood for the 95% confidence of the data.
      # https://www.visiondummy.com/2014/04/draw-error-ellipse-representing-covariance-matrix/
      semi_major_axis <- sqrt(5.991 * eigenvalues[1]) # 5.991 corresponds to 95% confidence interval for a 2D Gaussian distribution
      semi_minor_axis <- sqrt(5.991 * eigenvalues[2])
      # Calculate the area of the ellipse
      area_of_ellipse <- pi * semi_major_axis * semi_minor_axis
    }
    
    ### Output
    outputs <- data.frame(yr, cg_x, cg_y, inertia, area_of_ellipse)
    outputs <- outputs %>%
      relocate(yr, cg_x, cg_y, inertia) %>%
      rename("Year"= yr)
    cgi <- rbind(cgi, outputs)
  }
  cgi <- cgi %>%
    mutate(Quarter = paste(sort(unique(cog_data2$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter)
  return(cgi)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                      Map Spatial Distribution Indicators                                   
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> Function for visualising surveys points where species were found, convex     
#> hull area, centre of gravity, inertia ellipse with world map. Filter hlhh    
#> before providing it to the function      
#>                                     
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
coginertMap <- function(hlhh){
  # world sf 
  world <- map_data("world")
  worldsf <- sfheaders::sf_polygon(
    obj = world
    , x = "long"
    , y = "lat"
    , polygon_id = "group"
  ) 
  # convex_hull_sf
  longitude <- hlhh$ShootLong
  latitude <- hlhh$ShootLat
  
  # Combine latitude and longitude into a matrix
  coordinates <- cbind(longitude, latitude)
  # Calculate the convex hull
  convex_hull <- chull(coordinates)
  # Extract the convex hull points
  convex_hull_points <- as.data.frame(coordinates[convex_hull, ])
  
  # Create a SpatialPolygons object from the convex hull points
  convex_hull_sf <- sfheaders::sf_polygon(
    obj = convex_hull_points
    , x = "longitude"
    , y = "latitude"
  ) 
  # Centre of gravity
  centre_xy <- as.data.frame(cbind("longitude" = mean(coordinates[,"longitude"]), "latitude" = mean(coordinates[,"latitude"])))
  # and the inertia around the centre
  inertia <- sum((coordinates[, "longitude"]-centre_xy$longitude)^2 + (coordinates[, "latitude"]-centre_xy$latitude)^2)
  
  # plot area
  xlim <- c(min(convex_hull_points[1:nrow(convex_hull_points),1]), max(convex_hull_points[1:nrow(convex_hull_points),1]))
  ylim <- c(min(convex_hull_points[1:nrow(convex_hull_points),2]), max(convex_hull_points[1:nrow(convex_hull_points),2]))
  cord <- as.data.frame(coordinates)
  
  ggplot() +
    geom_sf(data = worldsf, size = 0.1) + # world map
    geom_sf(data = convex_hull_sf, colour = "red", fill = "red", alpha = 0.1) + # convex hull 
    coord_sf(xlim, ylim) +
    geom_point(data = cord, aes(x = longitude, y = latitude), colour = "grey30") +  # scatter plot
    geom_point(data = centre_xy, aes(x= longitude, y = latitude), colour = "blue", shape = 4, size = 3) + # centre of gravity
    #annotate("rect", xmin = min(xlim)-0.8, xmax = min(xlim) + 5, ymin = min(ylim)-0.4, ymax = min(ylim)+0.4, fill = "white", colour = "black") +
    #annotate("text", label = paste0("Area = ", as.character(round(areaoccupied,2)), " sq. deg."), x = min(x)+2, y = min(y), size = unit(3.5, "pt")) +
    stat_ellipse(data = cord, aes(x = longitude, y = latitude), type = "t") + # inertia
    theme_minimal()
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                            Shift Legend                
#>                          function by Z.Lin      
#>                        
#> https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> 
#> When creating a facet_wrap plot, you are often left with empty space. The 
#> legend makes this white space even larger by appending to one of the four
#> sides of the plot. This function moves the legend into one of the empty 
#> spaces created by facetting, reducing overall plot size and empty space
#> 
#>                              NOT MY CODE!
#>                              
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

shift_legend <- function(p){
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.\n")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.\n")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.\n")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}
