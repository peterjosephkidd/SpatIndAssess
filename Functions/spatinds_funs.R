################################################################################
###                    Spatial indicator functions                           ###
################################################################################
################################################################################
###                 Spread of Participation Index (SPI)                      ###
################################################################################

################################ SPI DATA PREP #################################
spi_prep <- function(hlhh, yrs, qrs, species_aphia, stk_regions){
  #### TotalNo for each species in each year and quarter
  s1 <- hlhh %>%
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, Area_27, TotalNo, HaulVal, HaulDur) %>%
    filter(HaulVal != "I", 
           Year %in% yrs,
           Quarter %in% qrs) %>% # remove invalid hauls 
    distinct() %>%
    na.omit() %>% 
    group_by(Valid_Aphia, Year, Quarter, StatRec, Area_27) %>%
    summarise(TotalNo = sum(TotalNo), .groups = "drop_last") %>%
    arrange(Year, Quarter)
  
  ### All rectangles sampled by survey 
  ### and number of hauls in each rectangle p.year p.quarter
  
  ### How long was each rectangle sampled for p.year p.quarter?
  s2 <- hlhh %>%
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, Area_27, TotalNo, HaulVal, HaulDur) %>%
    filter(HaulVal != "I",
           Year %in% yrs,
           Quarter %in% qrs) %>% # remove invalid hauls 
    distinct() %>%
    na.omit() %>% 
    group_by(Year, Quarter, StatRec, Area_27) %>%
    summarise(TotalHaulDur = sum(HaulDur), .groups = "drop_last")
  
  ### merge Total Haul duration of each rectangle to s1
  ### Then divide TotalNo by how long that rectangle was sampled for
  ### This standardizes TotalNo by sampling effort
  s3 <- s1 %>%
    left_join(s2, by = c("Year", "Quarter", "StatRec", "Area_27")) %>%
    mutate(TotalNo_TotalDur = TotalNo/TotalHaulDur)
  
  ### all survey rectangles
  s4 <- hlhh %>%
    select(haul.id, Valid_Aphia, Year, Quarter, StatRec, Area_27, TotalNo, HaulVal, HaulDur) %>%
    filter(HaulVal != "I",
           Year %in% yrs,
           Quarter %in% qrs) %>% # remove invalid hauls 
    distinct() %>%
    na.omit() %>% # remove invalid hauls
    select(Year, Quarter, StatRec, Area_27) %>%
    distinct()
  
  ### Rectangles where species were found
  s5 <- s3 %>% 
    group_by(Valid_Aphia, Year, Quarter, StatRec, Area_27) %>%
    filter(Valid_Aphia == species_aphia) %>%
    mutate(Present = 1)
  
  ### Rectangles where species were not found
  #### Survey rectangles (s4) minus present rectangles (s5)
  s6 <- s4 %>% 
    anti_join(s5, by = c("Year", "Quarter", "StatRec", "Area_27")) %>%
    mutate(Valid_Aphia = species_aphia, Absent = 1) %>%
    relocate(Valid_Aphia, Year, Quarter, StatRec, Area_27, Absent)
  
  ### Join presence with absence
  s7 <- bind_rows(s5, s6) %>%
    arrange(StatRec, .by_group = TRUE) %>%
    mutate(TotalNo = ifelse(is.na(TotalNo), 0, TotalNo),
           TotalNo_TotalDur  = ifelse(is.na(TotalNo_TotalDur ), 0, TotalNo_TotalDur),
           Present = ifelse(is.na(Present), 0, Present),
           Absent = ifelse(is.na(Absent), 0, Absent))
  
  s7 <- subset(s7, select = -TotalHaulDur)
  s7 <- merge(s7, s2, by = c("Year", "Quarter", "StatRec", "Area_27"))
  s7 <- s7 %>% 
    relocate(Valid_Aphia, Year, Quarter, StatRec, Area_27, TotalNo, TotalHaulDur, TotalNo_TotalDur, Present, Absent)
  
  # change year and quarter from discrete to continuous
  s7$Year <- as.numeric(as.character(s7$Year))
  s7$Quarter <- as.numeric(as.character(s7$Quarter))
  
  s7 <- s7 %>%
    mutate(Quarter.d = ifelse(Quarter == 1, 0, ifelse(Quarter == 3, 50, NA)),
           Year_Quart = paste0(Year,".", Quarter.d))
  
  s7$Year_Quart <- as.numeric(as.character(s7$Year_Quart))
  return(s7)
}

################################ SPI CALCULATE #################################

spi_calc <- function(observed, area){
  # default area = 1 means all zones are of equal size i.e. equal weighting
  #if(length(unique(area)) == 1|NA){
  # area <- rep(1, length(observed))
  #}
  
  spi <- as.data.frame(cbind(observed, area))
  # remove rows of data with NAs
  spi <- spi[complete.cases(spi), ]
  
  perc <- data.frame()
  N <- sum(spi$observed)
  
  for(a in spi$area){
    
    
    area_perc <- a/sum(spi$area)*100
    fe <- N*(area_perc/100) # expected occurence if random distributed
    o_minus_e <- 
      output <- c(area_perc, fe)
    perc <- rbind(perc, output) # area of each zone in perc
    
  }
  perc <- cbind(spi$observed, spi$area, perc)
  colnames(perc) <- c("Observations", "Area", "Area_Percentage", "Expected")
  fe_min <- min(perc$Expected)
  
  for(b in 1:nrow(perc)){
    fe_fo <- abs(perc$Observations[b] - perc$Expected[b])
    perc$fe_fo[b] <- cbind(fe_fo)
  }
  #print(perc)
  spi <- (sum(perc$fe_fo))/(2*(N-fe_min))
  #message("SPI value of 0 indicates homogenous use of zones. A value of 1 indicates strong biased use of one zone: ")
  return(spi)
}

################################################################################
###                              PosArea_Rect                                ###
################################################################################

pa_rect <- function(hh, hlhh, yrs, qrs, species_aphia, stk_regions){
  #### How many rectangles were sampled in each year?
  n.rects <- hh %>%
    filter(Area_27 %in% stk_regions, 
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    summarise(nrects = length(unique(StatRec)))
  
  #### How many rectangles sampled where species were present?
  p.rects <- hlhh %>%
    filter(Area_27 %in% stk_regions,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    filter(Valid_Aphia == species_aphia) %>%
    summarise(nrects_p = length(unique(StatRec)))
  
  # sometimes p.rects has less rows because some years the species isnt found.
  # we need to add those years into the df so we can bind the two dfs
  if(nrow(n.rects)!= nrow(p.rects)){
    missing_yrs <- setdiff(n.rects$Year, p.rects$Year)
    missing_vals <- rep(0, length(missing_yrs))
    missing_rows <- as.data.frame(cbind("Year" = missing_yrs, "nrects_p" = missing_vals))
    p.rects <- p.rects %>%
      rows_append(missing_rows) %>%
      arrange(Year)
  }
  
  ### Join together and calculate proportion of hauls present
  np.rects <- suppressMessages(bind_cols(n.rects, p.rects) %>%
    mutate(PosArea = nrects_p/nrects))
  
  ### formatting for plots
  #### change some colnames
  colnames(np.rects) <- c("Year", "nrects", "Year...3", "nrects_p", "PosArea")
  np.rects <- np.rects[c("Year", "nrects", "nrects_p", "PosArea")]
  #### Convert year and quarter to discrete values while maintain their original value
  np.rects$Year <- as.numeric(as.character(np.rects$Year))

  ### Return
  return(np.rects)
}

################################################################################
###                              PosArea_Haul                                ###
################################################################################

pa_haul <- function(hh, hlhh, yrs, qrs, species_aphia, stk_regions){
  ### How many hauls in each year in each quarter?
  n.hauls <- hh %>% 
    filter(Area_27 %in% stk_regions,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    summarise(no_haul.ids = length(unique(haul.id)))
  
  ### How many hauls in each quarter where species were present?
  p.hauls <- hlhh %>% 
    filter(Area_27 %in% stk_regions,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    filter(Valid_Aphia == species_aphia) %>%
    summarise(pr_hauls = length(unique(haul.id)))

  # sometimes p.rects has less rows because some years the species isnt found.
  # we need to add those years into the df so we can bind the two dfs
  if(nrow(n.hauls)!= nrow(p.hauls)){
    missing_yrs <- setdiff(n.hauls$Year, p.hauls$Year)
    missing_vals <- rep(0, length(missing_yrs))
    missing_rows <- as.data.frame(cbind("Year" = missing_yrs, "pr_hauls" = missing_vals))
    p.hauls <- p.hauls %>%
      rows_append(missing_rows) %>%
      arrange(Year)
  }
  
  ### Join together
  np.hauls <- suppressMessages(bind_cols(n.hauls, p.hauls) %>%
    mutate(PosArea = pr_hauls/no_haul.ids))
  
  ### formatting for plots
  colnames(np.hauls) <- c("Year", "no_haul.ids", "Year...3", "pr_hauls", "PosArea")
  np.hauls <- np.hauls[c("Year", "no_haul.ids", "pr_hauls", "PosArea")]
  np.hauls$Year <- as.numeric(as.character(np.hauls$Year))
  
  ### return
  return(np.hauls)
}

################################################################################
###                                 Lorenz                                   ###
################################################################################

################################ Lorenz Data ###################################
lorenz_data <- function(hlhh, yrs, qrs, species_aphia, stk_regions){
  lorenz <- hlhh %>%
    filter(Area_27 %in% stk_regions,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>%
    filter(Valid_Aphia == species_aphia) %>%
    distinct() %>%
    na.omit() %>% 
    mutate(TotalNo_Dur = TotalNo/HaulDur) %>% # standardise by haul duration
    arrange(Year, TotalNo_Dur) %>% # order desc
    group_by(Year) %>%
    mutate(cumsum = cumsum(TotalNo_Dur),
           rect_num = row_number(),
           cumsum_prop = cumsum(TotalNo_Dur)/max(cumsum(TotalNo_Dur)),
           rect_num_prop = row_number()/max(row_number()))
  lorenz$Year <- as.numeric(as.character(lorenz$Year))
  return(lorenz)
}
################################ Lorenz Plot ###################################
lorenz_plot <- function(lorenz_data){
  lplot <- ggplot(data = lorenz_data, aes(x = rect_num_prop, y = cumsum_prop)) + 
  geom_line(aes(group = Year, colour = Year), alpha = 0.3, linewidth = 2) +
  geom_abline() +
  geom_vline(xintercept = 0.95, lty = 2, colour = "black", linewidth = 1) +
  coord_cartesian(ylim= c(0,1), xlim = c(0,1), expand = FALSE) +
    labs(title = paste0("Lorenz Curve: Distribution of ", species), 
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

################################ Gini Index ###################################
Gini <- function(lorenz_data){
    G <- lorenz_data %>%
    group_by(Year) %>%
    summarise(G = ineq(TotalNo_Dur, type = "Gini"))
  G$Year <- as.numeric(as.character(G$Year))
  return(G)
}

################################### D95 #######################################
# The proportion of the population observed within 95% of rectangles
## The point where the solid red line in Lorenz plot 
## intersects the Lorenz curve for each year

d95 <- function(lorenz_data){
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
  D95_d$Year <- as.numeric(as.character(D95_d$Year))
  return(D95_d)
}
