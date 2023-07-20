#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                    Spatial indicator functions
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                Spread of Participation Index (SPI)                      
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> 
#> Function to prepare data so that we can calculate SPI
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
spi_prep <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
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
  
  return(s7)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                               SPI Calculation
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> This function calculate SPI based on the observed catches. The are argument
#> can be used to weight areas in circumstances of unequal sized areas
#> Since ICES rectangles have the same area, this should be set to 1 so that all
#> rectangles are given equal weight
#> 
#> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

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
  
  return(spi)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                              PosArea_Rect                                
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> The proportion of ICES rectangles where species were fond at least once.
#> Calculated per year to give annual time series. Value of 1 indicates that 
#> all rectangles were occupied. Value of 0 indicates that no rectangles were
#> occupied. Indicator is bounded by 0 and 1
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

pa_rect <- function(hh, hlhh, yrs, qrs, species_aphia, stk_divs){
  #### How many rectangles were sampled in each year?
  n.rects <- hh %>%
    filter(Area_27 %in% stk_divs, 
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    summarise(nrects = length(unique(StatRec))) %>%
    mutate(Quarter = paste(as.character(sort(qrs)), collapse = ", "))
  
  #### How many rectangles sampled where species were present?
  p.rects <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    filter(Valid_Aphia == species_aphia) %>%
    summarise(nrects_p = length(unique(StatRec))) %>%
    mutate(Quarter = paste(as.character(sort(qrs)), collapse = ", "))
  
  # sometimes p.rects has less rows because some years the species isnt found.
  # we need to add those years into the df so we can bind the two dfs
  if(nrow(n.rects)!= nrow(p.rects)){
    missing_yrs <- as.integer(setdiff(n.rects$Year, p.rects$Year))
    missing_vals <- rep(0, length(missing_yrs))
    missing_rows <- as.data.frame(cbind("Year" = missing_yrs, 
                                        "nrects_p" = missing_vals, 
                                        "Quarter" = paste(as.character(sort(qrs)), collapse = ", ")))
    missing_rows$Year <- as.integer(missing_rows$Year)
    missing_rows$nrects_p <- as.integer(missing_rows$nrects_p)

    p.rects <- p.rects %>%
      rows_append(missing_rows) %>%
      arrange(Year)
  }
  
  ### Join together and calculate proportion of hauls present
  np.rects <- suppressMessages(bind_cols(n.rects, p.rects) %>%
    mutate(PosAreaR = nrects_p/nrects))
  
  ### formatting for plots
  #### change some colnames
  colnames(np.rects) <- c("Year", "nrects", "Quarter", "Year...4", "nrects_p", "Quarter...6", "PosAreaR")
  np.rects <- np.rects[c("Year", "Quarter", "nrects", "nrects_p", "PosAreaR")]
  #### Convert year to discrete values while maintain their original value
  np.rects$Year <- as.numeric(as.character(np.rects$Year))

  ### Return
  return(np.rects)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
###                              PosArea_Haul                                ###
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

pa_haul <- function(hh, hlhh, yrs, qrs, species_aphia, stk_divs){
  ### How many hauls in each year in each quarter?
  n.hauls <- hh %>% 
    filter(Area_27 %in% stk_divs,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    summarise(no_haul.ids = length(unique(haul.id))) %>%
    mutate(Quarter = paste(as.character(sort(qrs)), collapse = ", "))
  
  ### How many hauls in each quarter where species were present?
  p.hauls <- hlhh %>% 
    filter(Area_27 %in% stk_divs,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    group_by(Year) %>%
    filter(Valid_Aphia == species_aphia) %>%
    summarise(pr_hauls = length(unique(haul.id))) %>%
    mutate(Quarter = paste(as.character(sort(qrs)), collapse = ", "))

  # sometimes p.rects has less rows because some years the species isnt found.
  # we need to add those years into the df so we can bind the two dfs
  if(nrow(n.hauls)!= nrow(p.hauls)){
    missing_yrs <- setdiff(n.hauls$Year, p.hauls$Year)
    missing_vals <- rep(0, length(missing_yrs))
    missing_rows <- as.data.frame(cbind("Year" = missing_yrs, 
                                        "pr_hauls" = missing_vals,
                                        "Quarter" = paste(as.character(sort(qrs)), collapse = ", ")))
    missing_rows$Year <- as.integer(missing_rows$Year)
    missing_rows$pr_hauls <- as.integer(missing_rows$pr_hauls)
    
    p.hauls <- p.hauls %>%
      rows_append(missing_rows) %>%
      arrange(Year)
  }
  
  ### Join together
  np.hauls <- suppressMessages(bind_cols(n.hauls, p.hauls) %>%
    mutate(PosAreaH = pr_hauls/no_haul.ids))
  
  ### formatting for plots
  colnames(np.hauls) <- c("Year", "no_haul.ids", "Quarter", "Year...4", "pr_hauls", "Quarter...6", "PosAreaH")
  np.hauls <- np.hauls[c("Year", "Quarter", "no_haul.ids", "pr_hauls", "PosAreaH")]
  np.hauls$Year <- as.numeric(as.character(np.hauls$Year))
  
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
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

lorenz_data <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  # get each individual haul. We dont care about Valid_Aphia or TotalNo here 
  allspcs_lor <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    select(haul.id, Year, StatRec, HaulDur) %>%
    distinct() %>%
    na.omit() %>%
    mutate(TotalNo = as.numeric(0), # add 0 TotalNo 
           Valid_Aphia = species_aphia) %>% # add species aphia
    select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>% # rearrange cols
    mutate(Quarter = paste(as.character(sort(qrs)), collapse = ", "))
  
  if(all(unique(allspcs_lor$Year), unique(hlhh$Year)) == FALSE){
    warning("Not all survey years were retained in `allspcs_lor`.")
  }
  if(any(duplicated(allspcs_lor$haul.id))==TRUE){
    warning("Some rows of haul.id are duplicated in `allspcs_lor`.")
  }
  # get the data filtered to where species were found 
  spcs_lor <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I") %>% # remove invalid hauls
    select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>%
    mutate(TotalNo = as.numeric(TotalNo), 
           Quarter = paste(as.character(sort(qrs)), collapse = ", ")) %>%
    filter(Valid_Aphia == species_aphia) %>%
    relocate(haul.id, Valid_Aphia, Year, Quarter, StatRec, TotalNo, HaulDur) %>%
    distinct() %>%
    na.omit() 
  
  if(janitor::compare_df_cols_same(allspcs_lor, spcs_lor)==FALSE){
    warning("Columns in `allspcs_lor` and `spcs_lor` do not match.")
  }
  # bind hauls where species were not found to data where species present data
  # hauls where species were not found will now have the haul.id with TotalNo = 0
  lorenz <- bind_rows(spcs_lor, allspcs_lor) %>%
    arrange(Year) %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur) %>% # standardise by haul duration
    arrange(Year, TotalNo_Dur) %>% # order desc
    group_by(Year) %>%
    mutate(cumsum = cumsum(TotalNo_Dur),
           rect_num = row_number(),
           cumsum_prop = cumsum(TotalNo_Dur)/max(cumsum(TotalNo_Dur)),
           rect_num_prop = row_number()/max(row_number()))
  lorenz$Year <- as.numeric(as.character(lorenz$Year))
  
  if(suppressWarnings(all(unique(lorenz$Year), unique(hlhh$Year)) == FALSE)){
    warning("Not all survey years were retained in `lorenz`.")
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
    labs(title = paste0("Lorenz Curve: Distribution of ", species, " (", min(lorenz_data$Year),":", max(lorenz_data$Year), ", Q", unique(lorenz_data$Quarter), ")"), 
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
      mutate(Quarter = unique(lorenz$Quarter)) %>%
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
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

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
  D95_d$D95[is.nan(D95_d$D95)] <- 0 # change NaNs to 0
  D95_d$Year <- as.numeric(as.character(D95_d$Year)) 
  D95_d <- D95_d %>%
    mutate(Quarter = unique(lorenz_data$Quarter)) %>%
    relocate(Year, Quarter, Index_loc, D95_cumsum, D95)
  return(D95_d)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                             Spreading Area                               
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> Similar to Lorenz curve an Gini index but has no influence from zeros

###### test example ######
stk.chr <- "ple.27.420"

species <- allstk_refpts %>% filter(
  stk_name == stk.chr) %>%
  select(spcs_name, latin_name)
head(species)
species_aphia <- findAphia(species$latin_name, latin = TRUE)

stk_divs2 <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(stk_divs) %>%
  unique()
stk_divs <- strsplit(as.character(stk_divs2), split = ", ")[[1]]

hlhh <- ns_ibts.data$hlhh

yrs <- c(1991)
qrs <- 3

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                          Spreading Area Data Prep
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>
#> Function to get the total number of catches divided by the haul duration
#> for each haul
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
spreadingarea_data <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  sa <- hlhh %>%
      filter(Area_27 %in% stk_divs,
             Year %in% yrs,
             Quarter %in% qrs,
             HaulVal != "I") %>% # remove invalid hauls
      select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>%
      mutate(TotalNo = as.numeric(TotalNo), 
             Quarter = paste(as.character(sort(qrs)), collapse = ", ")) %>%
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
spreadingarea(z = sa$TotalNo_Dur, w = sa$T, plot = TRUE)
spreadingarea(z = sa$TotalNo_Dur, w = rep(1, length(sa$TotalNo_Dur)), plot = TRUE) # scaled 0 to 1
spreadingarea(z = sa$TotalNo_Dur, w = rep(1, length(sa$TotalNo_Dur))/length(sa$TotalNo_Dur), plot = TRUE) # scaled 0 to 1

plot(sa$T, sa$`(Q-Q(T)/Q`, type = "o", pch = "+")



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

chullAOC <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  chull_data <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I",
           Valid_Aphia == species_aphia) # remove invalid hauls
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
    # Calculate area of the convex hull
    convex_hull_area <- st_area(convex_hull_sf)
    
    # get area of convex hull minus area of intersecting land
    land <- st_intersection(convex_hull_sf, worldsf)
    landarea <- sum(st_area(land))
    areaoccupied <- convex_hull_area - landarea
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
           areaoccupied_scaled = areaoccupied/StkArea)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                                Centre of Gravity                                      
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                                                                              
#> Function for calculating centre of gravity (x and y), inertia, and the area  
#> of the inertia ellipse around the centre of gravity                          
#>                                                                              
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
coginert <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  cog_data <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% yrs,
           Quarter %in% qrs,
           HaulVal != "I",
           Valid_Aphia == species_aphia) # remove invalid hauls
  
  cgi <- data.frame()
  # Pseudo-dataset (latitude and longitude coordinates)
  for(yr in yrs){
    cog_yearly <- chull_data %>%
      filter(Year == yr)
    longitude <- cog_yearly$ShootLong
    latitude <- cog_yearly$ShootLat
    ### Centre of gravity
    cg_x <- mean(longitude)
    cg_y <- mean(latitude)
    ### Inertia
    inertia <- sum(longitude-cg_x)^2 + sum(latitude-cg_y)^2
    ### Area of Inertia
    # Create a 2x2 covariance matrix
    cov_matrix <- cov(cbind(longitude, latitude))
    # Calculate the eigenvalues and eigenvectors of the covariance matrix
    eigen_result <- eigen(cov_matrix)
    eigenvalues <- eigen_result$values
    # Calculate the semi-major and semi-minor axes (2 standard deviations)
    # The factor 5.991 represents the Chi-square likelihood for the 95% confidence of the data.
    # https://www.visiondummy.com/2014/04/draw-error-ellipse-representing-covariance-matrix/
    semi_major_axis <- sqrt(5.991 * eigenvalues[1]) # 5.991 corresponds to 95% confidence interval for a 2D Gaussian distribution
    semi_minor_axis <- sqrt(5.991 * eigenvalues[2])
    # Calculate the area of the ellipse
    area_of_intertia <- pi * semi_major_axis * semi_minor_axis
    
    ### Output
    outputs <- cbind(yr, cg_x, cg_y, inertia, area_of_intertia)
    cgi <- rbind(cgi, outputs)
  }
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
mapSDI <- function(hlhh){
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
    geom_point(data = cord, aes(x = longitude, y = latitude)) +  # scatter plot
    geom_point(data = centre_xy, aes(x= longitude, y = latitude), colour = "blue", shape = 4, size = 3) + # centre of gravity
    #annotate("rect", xmin = min(xlim)-0.8, xmax = min(xlim) + 5, ymin = min(ylim)-0.4, ymax = min(ylim)+0.4, fill = "white", colour = "black") +
    #annotate("text", label = paste0("Area = ", as.character(round(areaoccupied,2)), " sq. deg."), x = min(x)+2, y = min(y), size = unit(3.5, "pt")) +
    stat_ellipse(data = cord, aes(x = longitude, y = latitude), type = "t") # inertia
}
