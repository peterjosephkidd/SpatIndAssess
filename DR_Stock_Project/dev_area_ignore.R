###### Individual Stock Check ######
##### Prep #####
### load packages
pckgs <- c("FLCore", "FLBRP", "dplyr", "ggplot2", "ggplotFL", "rgdal", 
           "DataExplorer", "rgeos", "sf", "mapplots", "maptools", "mapproj", 
           "beepr", "patchwork", "ineq", "icesDatras", "icesVocab", "scales", 
           "readxl", "data.table", "zoo")
for(pkg in pckgs){
  library(pkg, character.only = T, quietly = T)
}
rm(pckgs, pkg)

### Source functions
source(paste0(getwd(), "/Functions/dataprep_funs.R")) # for dealing with datras data
source(paste0(getwd(), "/Functions/spatinds_funs.R")) # for computing spatial indicators
source(paste0(getwd(), "/Functions/ROC_funs.R"))      # for ROC and TSS

### ICES Rectangles
ices_rect2 <- readOGR(dsn = paste0(getwd(), "/Data/ICES Rect"), layer = "StatRec_map_Areas_Full_20170124")
ices_rect <- makeReadableFortify(ices_rect2)
rm(ices_rect2)

### ICES Divisions
ices_divs2 <- readOGR(dsn = paste0(getwd(), "/Data/ICES Divs"), layer = "ICES_Areas_20160601_cut_dense_3857")
# ices_divs <- readOGR(dsn = "ICES North Sea Divisions", layer = "ICES_Divisons_CodHadWhgPok") # old shapefile
ices_divs <- makeReadableFortify(ices_divs2)
rm(ices_divs2)

### Load in survey metadata
allstk_metadata <- read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice_Sheets/stock_metadata_refpts.xlsx"), sheet = "stk_metadata")
allstk_refpts <- read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice_Sheets/stock_metadata_refpts.xlsx"), sheet = "stk_refpts")

### Select the stock to analyse
unique(allstk_metadata$stk_id) # ignore so.27.7d for now, need to find YFS survey data
stk.chr <- "tur.27.4"          # change accordingly

### Load in stock assessments
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock_Objects/")
list.files(stockobj.path) # identify correct stock assessment object to match with stk.chr
stkobj <- "TUR_27.4_Final_WGNSSK_2022_sam_flstock.Rdata" # change accordingly
load(paste0(stockobj.path, stkobj)) # load in the stock object
stk <- tur.27.4 # change accordingly
rm(tur.27.4)


##### Run through #####
#### Set params ####
### Get the stock species name
species <- allstk_refpts %>% filter(
  stk_name == stk.chr) %>%
  select(spcs_name, latin_name)
head(species)

### Get Valid Aphia ID using Latin name
species_aphia <- findAphia(species$latin_name, latin = TRUE)

### Get the ICES divisions for the stock of interest
stk_divs2 <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(stk_divs) %>%
  unique()
stk_divs <- strsplit(as.character(stk_divs2), split = ", ")[[1]]
print(paste0("ICES Divisions for ", species$spcs_name, " (", stk.chr, ") : ", stk_divs2))

### check stk_divs are in the ices_rect and ices_divs
stk_divs %in% ices_rect$Area_27
stk_divs %in% ices_divs$Area_27

### Get the advice sheet reference points for the stock
stk_refpts <- allstk_refpts %>% filter(
  stk_name == stk.chr)
print(stk_refpts)

#### Survey info ####
### Get survey information for the current stock we are looking at
stk_surveys <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(-description, -date_published)
print(stk_surveys)

### Select the surveys used within the stock assessment
## we will load in the survey data from local drive 
## previously downloaded in 1_Download_Survey_Data
surveydata.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/Survey Data/"

## Load the survey data used in the stock assessment for the current stock we are looking at
## Store in a list so we can loop through it later
## e.g. for Witch, BTS and NS-IBTS survey data were used. We store all this survey data into a list.
## we will filter this survey data to the correct years and quarters for each survey index later
## e.g. witch has 3 survey indices, BTS Q3, IBTS Q1, IBTS Q3, with different year ranges
data.list <- list()
for(survey in unique(stk_surveys$survey_name)){
  survey_data <- list.files(paste0(surveydata.path, survey), pattern = "*.data.rds*")
  survey_data <- try(load(paste0(surveydata.path, survey, "/", survey_data)))
  survey_data <- try(get(survey_data))
  data.list[[survey]] <- survey_data
}

### Check that surveys loaded are what we expect
summary(data.list)
row.names(summary(data.list)) %in% unique(stk_surveys$survey_name) # should be TRUE

### Get the name of the individual indexes used in the stock assessment
## Each index can be comprised of multiple survey data sources 
## Get the name of the survey indices used in the stock assessment:
stk_surveys_indices <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(survey_index) %>%
  unique()
print(stk_surveys_indices)

### Get the survey data used in each index and filter it so that it matches
## the data used in the stock assessment e.g. by Species, Years, and Quarters. 
stk_data_filtered <- list()
for(indx in stk_surveys_indices$survey_index){
  # for each survey index in the stock assessment...
  print(paste0("Survey Index: ", indx))
  # get the row of data in allstk_metadata for this survey index
  stkindx_surveys <- allstk_metadata %>%
    filter(stk_id == stk.chr,
           survey_index == indx)
  print(stkindx_surveys)

  for(survey in stkindx_surveys$survey_name){
    print(paste0("Survey: ", survey))
    # for each survey within each survey index...
    # get the start and end years of the survey data used
    yr_strt <- stkindx_surveys$survey_yrs_start[stkindx_surveys$survey_name==survey]
    yr_end <- stkindx_surveys$survey_yrs_end[stkindx_surveys$survey_name==survey] 
    # assumptions made if start or end year is not given in the stock assessment
    if(is.na(yr_end)==TRUE){yr_end <- 2022} # assume most recent data used
    if(is.na(yr_strt)==TRUE){yr_strt <- 1900} # assume oldest data used
    # get the quarters used in stock stock index for this survey
    qrs <- stkindx_surveys$survey_qrs[stkindx_surveys$survey_name==survey]
    
    # sometimes the quarters used is not available in the advice sheets, so 
    # assume all quarters were used. This isn't explicitly stated probably because some 
    # surveys are only run in certain quarters so there is no need to say.
    # Setting `qrs` to c(1:4) will retain whatever data the survey has. 
    if(is.na(qrs)){
      qrs <- c(1:4)
    }
    
    # filter the years and quarters of survey data, for each survey, within each survey index
    # CHECK: do I need to do c(qrs). Will this code work if there are multiple quartes that need to be selected
    hlhh <- try(data.list[[survey]]$hlhh %>%
                  filter(Year %in% c(yr_strt:yr_end),
                         Quarter %in% qrs)) 
    hl <- try(data.list[[survey]]$hl %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    hh <- try(data.list[[survey]]$hh %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    ca <- try(data.list[[survey]]$ca %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    
    stk_data_filtered[[indx]][[survey]] <- list(hlhh = hlhh, hl = hl, hh = hh, ca = ca)
  }
}

### Check data has been filtered to the correct index, survey, years, and quarters
for(indx in 1:length(stk_data_filtered)){
  # get survey index name from data
  message(paste0("Survey Index:             ", names(stk_data_filtered[indx])))
  # does this match with the excel doc
  print(noquote(paste0("Survey index matches? ", names(stk_data_filtered[indx]) %in% stk_surveys$survey_index)))
  for(survey in 1:length(stk_data_filtered[[indx]])){
    # get survey name within this survey index
    print(noquote(paste0("  Survey:             ", names(stk_data_filtered[[indx]][survey]))))
    # does this survey match with exceld doc
    print(noquote(paste0("  Survey matches?     ", names(stk_data_filtered[[indx]][survey]) ==
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_name"])))
    # get survey data years from the dataset
    print(noquote(paste0("  Survey years:       ", min(stk_data_filtered[[indx]][[survey]]$hlhh$Year), "-", max(stk_data_filtered[[indx]][[survey]]$hlhh$Year))))
    # does this match with the start and end years in the excel doc
    print(noquote(paste0("  Start year matches? ", 
                         min(stk_data_filtered[[indx]][[survey]]$hlhh$Year) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_yrs_start"])))
    print(noquote(paste0("  End year matches?   ", 
                         max(stk_data_filtered[[indx]][[survey]]$hlhh$Year) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_yrs_end"])))
    # get survey quarters from the dataset
    print(noquote(paste0("  Survey quarters:    ", unique(stk_data_filtered[[indx]][[survey]]$hlhh$Quarter))))
    # does this match with the survey quarters in the excel doc
    print(noquote(paste0("  Quarters match?     ", 
                         unique(stk_data_filtered[[indx]][[survey]]$hlhh$Quarter) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_qrs"])))
    print(noquote("---------------------------"))
  }
}

##### Spatial Indicators #####
### Output path to save data and plots
## Keeping this to my one drive, files may be too large for GitHub
si.output.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/10Stock_Outputs/"

# Running through eachs surevy and calculating spatial indicators manually
# to identify and fix bugs resulting from the for loop in 2_ROC_Assessment.Rmd
### Randomly choose a survey index
indx <- sample(1:length(stk_data_filtered), size = 1)
### Randomly choose a survey within the survey index
survey <- sample(1:length(stk_data_filtered[[indx]]), size = 1)
### Look at what this resulted in
print(noquote(paste0("Survey Index:       ", names(stk_data_filtered[indx]))))
print(noquote(paste0("Survey:             ", names(stk_data_filtered[[indx]][survey]))))
### assign data to individual objects to make our life easier
hlhh <- stk_data_filtered[[indx]][[survey]]$hlhh
hh <- stk_data_filtered[[indx]][[survey]]$hh
ca <- stk_data_filtered[[indx]][[survey]]$ca # we dont use this but its here if needed

#### calculate spatial indicators using this data
### we should be able to see where calculations dont look right 
yrs <- unique(hlhh$Year)
qrs <- unique(hlhh$Quarter)
#### Positive Area (by ICES rectangle) ####
print(noquote("Positive Area (Rectangle)"))
np.rects <- pa_rect(hh = hh, 
                    hlhh = hlhh, 
                    yrs = yrs, 
                    qrs = qrs, 
                    species_aphia = species_aphia, 
                    stk_divs = stk_divs)
plot(np.rects$Year, np.rects$PosArea, type = "b", ylim=c(0,1))
## Checks
print(np.rects, n = nrow(np.rects))
## Years match?
all(np.rects$Year == yrs)
## Spatial indicator values between 0 and 1?
all(np.rects$PosArea >= 0 & np.rects$PosArea <= 1)
## no. rectangles sampled non-negative?
all(np.rects$nrects >= 0)
## no. rectangles present non-negative?
all(np.rects$nrects_p >= 0)
## no. rectangles present = less than or equal to total rectangles sampled?
all(np.rects$nrects_p <= np.rects$nrects)

#### Positive Area (by Haul) ####
print(noquote("Positive Area (Haul)"))
np.hauls <- pa_haul(hh = hh, 
                    hlhh = hlhh, 
                    yrs = yrs, 
                    qrs = qrs, 
                    species_aphia = species_aphia, 
                    stk_divs = stk_divs)
plot(np.hauls$Year, np.hauls$PosArea, type = "b", ylim=c(0,1))

## Checks
print(np.hauls, n = nrow(np.hauls))
## Years match?
all(np.hauls$Year == yrs)
## Spatial indicator values between 0 and 1?
all(np.hauls$PosArea >= 0 & np.hauls$PosArea <= 1)
## no. hauls sampled non-negative?
all(np.hauls$no_haul.ids >= 0)
## no. hauls present non-negative?
all(np.hauls$pr_hauls >= 0)
## no. hauls present = less than or equal to total hauls?
all(np.hauls$pr_hauls <= np.hauls$no_haul.ids)




#### Lorenz curve data ####
lorenz <- lorenz_data(hlhh = hlhh, 
                      yrs = yrs, 
                      qrs = qrs, 
                      species_aphia = species_aphia, 
                      stk_divs = stk_divs)
lorenz_plot(lorenz)

Gini.index <- Gini(lorenz)
plot(Gini.index$Year, Gini.index$'Gini Index', type = "l", ylim=c(0,1)) 

D95 <- d95(lorenz)
plot(D95$Year, D95$D95, type = "l", ylim=c(0,1)) 

#### SPI ####
spi_data <- spi_prep(hlhh = hlhh, 
                     yrs = yrs, 
                     qrs = qrs, 
                     species_aphia = species_aphia, 
                     stk_divs = stk_divs)
#### Calculate
spi <- spi_data %>% group_by(Year) %>% # Year only, not quarter
  filter(Area_27 %in% stk_divs) %>%
  summarise(SPI = spi_calc(TotalNo_TotalDur, area = 1)) ## all areas equal weight
## Take inverse of SPI, so high numbers = good/more distributed
spi$SPI <- 1-spi$SPI 
spi$SPI[is.nan(spi$SPI)] <- 0 # change NaNs to 0 
plot(spi$Year, spi$SPI, type = "l", ylim=c(0,1))

############################
print(noquote("Lorenz Curve"))
lorenz <- lorenz_data(hlhh = hlhh, 
                      yrs = yrs, 
                      qrs = qrs, 
                      species_aphia = species_aphia, 
                      stk_divs = stk_divs)
## Proportions between 0 and 1?
all(lorenz$cumsum_prop >= 0 & lorenz$cumsum_prop<= 1) # FALSE for tur, check where
range(lorenz$cumsum_prop)
all(lorenz$rect_num_prop >= 0 & lorenz$rect_num_prop<= 1)

## Plot Lorenz curve
# Plot
colour_range <- colorRampPalette(c("blue", "green", "yellow","#FF0033"))
my_colours <- colour_range(3)

lorenz_plot <- ggplot(data = lorenz, aes(x = rect_num_prop, y = cumsum_prop)) + 
  geom_line(aes(group = Year, colour = Year), alpha = 0.3, linewidth = 2) +
  geom_abline() +
  geom_vline(xintercept = 0.95, lty = 2, colour = "black", linewidth = 0.5) +
  #coord_cartesian(ylim= c(0,1), xlim = c(0,1), expand = FALSE) +
  labs(title = paste0("Lorenz Curve: Distribution of ", species), 
       subtitle = "Dashed line = Proportion of population observed within 95% of rectangles (D95)",
       x = "Culmuative Prop Number of Rectangles", 
       y = "Culmuative Prop Sum of Species Counts") +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))+
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none")
lorenz_plot
## Each lorenz curve should be bounded by (0,0) and (1,1).
## Check which years this is not true, if any
test <- lorenz %>%
  group_by(Year) %>%
  mutate(min_cumsum_prop = min(cumsum_prop),
         max_cumsum_prop = max(cumsum_prop),
         min_rectnum_prop = min(rect_num_prop),
         max_rectnum_prop = max(rect_num_prop))
## after looking at test, I noticed negative numbers in TotalNo
## this is what is driving subsequent errors
range(lorenz$TotalNo)
## the calculation of TotalNo must be wrong !!!!
## this will lead to a knock-on effect to the calculation of Gini and D95
## lets fix the lorenz_data function now...
## ... looking further into the issue, I noticed that the -9 are actually in the raw survey data
## even before I use my surveyprep function to create the hlhh data:
range(hlhh$TotalNo)
range(hl$TotalNo)

## this is when I noticed that there are multiple species in the hlhh data
select(hlhh, c(haul.id, Valid_Aphia, Year, Quarter, HaulNo, TotalNo, StatRec, HaulDur))
unique(hlhh$Valid_Aphia)
## and then realised this is what is meant to happen...haha, 
## we filter by species right before we calculate spat inds
test <- hlhh %>%
  filter(Area_27 %in% stk_divs,
         Year %in% yrs,
         Quarter %in% qrs,
         HaulVal != "I") %>% # remove invalid hauls
  select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>%
  filter(Valid_Aphia == species_aphia) %>%
  distinct() %>%
  na.omit()
## lets find those negative TotalNo rows
test[test$TotalNo < 0,] # we can see thet are all -9
# we should remove rows of data where there are negative TotalNo
# this should be put in the prepsurveydata() function in dataprep_funs.R
# (this has now been added)
# but I will do it manually here to quickly test ou instead of rerunning scripts
hlhh <- subset(hlhh, TotalNo >= 0)
range(hlhh$TotalNo)

## lets re-run the lorenz function and look at the plot again
lorenz <- lorenz_data(hlhh = hlhh, 
                      yrs = yrs, 
                      qrs = qrs, 
                      species_aphia = species_aphia, 
                      stk_divs = stk_divs)
lorenz_plot <- ggplot(data = lorenz, aes(x = rect_num_prop, y = cumsum_prop)) + 
  geom_line(aes(group = Year, colour = Year), alpha = 0.3, linewidth = 2) +
  geom_abline() +
  geom_vline(xintercept = 0.95, lty = 2, colour = "black", linewidth = 0.5) +
  #coord_cartesian(ylim= c(0,1), xlim = c(0,1), expand = FALSE) +
  labs(title = paste0("Lorenz Curve: Distribution of ", species), 
       subtitle = "Dashed line = Proportion of population observed within 95% of rectangles (D95)",
       x = "Culmuative Prop Number of Rectangles", 
       y = "Culmuative Prop Sum of Species Counts") +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))+
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none")
lorenz_plot

## this looks much better. I imagine this will also have an effect on the calculation of all the other spatial indicators
# Note that the lorenz curve still does not start at (0,0), this isnt a huge problem
# since it does not affect the calcualtion of Gini or D95, but should be corrected
# might be an issue for years if there is only data point, as you need two datapoints
# to geta straight line. Year with one data row will be a dot at (1,1)
# also need to have a think about what the curve should look like if there are only two data points
# should it fllow the line of equality, or should it hit the bottom right???
# atm it follows the line of equality

# lets leave this for now and move onto the other spatial indicators coming from lorenz data


#### Gini index ####
print(noquote("Gini Index"))
Gini.index <- Gini(lorenz)

##### Take inverse of Gini, so high numbers = good
Gini.index$G <- 1- Gini.index$G

## see how for 1985 Gini = 1. In 1985 there is only 1 data point. Is this correct? 
## See above ramble
## also notice how some years have been omitted e.g. 1986, 1991 for tur SNS
## this is because turbot were not caught in these years
## however we need to retain these 0's rather than omitting these years completely 
## this would indicate that data in this year was not collected, which is incorrect
setdiff(unique(hlhh$Year), unique(Gini.index$Year))
## in the Gini.index output, we want to see 1986 nd 1991 with a value of 0
## lets play around with the lorenz function again
## as this issue happens when we filter the data by species code
## we will need to retain all of the hauls and then append this to the species filtered dataset
## so that rows that arent in the species dataset are joined to it with values of 0

# get each individual haul. We dont care about Valid_Aphia or TotalNo here 
allspcs_lor <- hlhh %>%
  filter(Area_27 %in% stk_divs,
         Year %in% yrs,
         Quarter %in% qrs,
         HaulVal != "I") %>% # remove invalid hauls
  select(haul.id, Year, StatRec, HaulDur) %>%
  distinct() %>%
  na.omit() %>%
  mutate(TotalNo = 0, # add 0 TotalNo 
         Valid_Aphia = species_aphia) %>% # add species aphia
  select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) # rearrange cols

all(unique(allspcs_lor$Year), unique(hlhh$Year)) # check all years in survey are retained. Should be TRUE
any(duplicated(allspcs_lor$haul.id)) # are there any rows with the same haul.id? Should be FALSE

# get the data filtered to where species were found 
spcs_lor <- hlhh %>%
filter(Area_27 %in% stk_divs,
       Year %in% yrs,
       Quarter %in% qrs,
       HaulVal != "I") %>% # remove invalid hauls
select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) %>%
filter(Valid_Aphia == species_aphia) %>%
distinct() %>%
na.omit() 

janitor::compare_df_cols_same(allspcs_lor, spcs_lor) # check cols match. Should be TRUE

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
all(unique(lorenz$Year), unique(hlhh$Year)) # check all years in survey are retained. Should be TRUE

# lets plot the lorenz curve again...
lorenz_plot <- ggplot(data = lorenz, aes(x = rect_num_prop, y = cumsum_prop)) + 
  geom_line(aes(group = Year, colour = Year), alpha = 0.3, linewidth = 2) +
  geom_abline() +
  geom_vline(xintercept = 0.95, lty = 2, colour = "black", linewidth = 0.5) +
  #coord_cartesian(ylim= c(0,1), xlim = c(0,1), expand = FALSE) +
  labs(title = paste0("Lorenz Curve: Distribution of ", species), 
       subtitle = "Dashed line = Proportion of population observed within 95% of rectangles (D95)",
       x = "Culmuative Prop Number of Rectangles", 
       y = "Culmuative Prop Sum of Species Counts") +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))+
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none")
lorenz_plot
# looks a lot different!! the curve approaches the bottom right 
# indicating hgiher inequality in the dsitribution of catches amongst rectangles
# this seems to be more correct imo

## now lets compute Lorenz derived statistics again...
#### Gini index take 2 ####
print(noquote("Gini Index"))
Gini.index <- Gini(lorenz)
## Take inverse of Gini, so high numbers = good
Gini.index$G <- 1- Gini.index$G
Gini.index
plot(Gini.index$Year, Gini.index$G, type = "l", ylim=c(0,1)) 
## time series plot looks a lot more stable which is nice
## we can also see that in the years where species werent found, Gini = NaN
## This should really = 0, as this isnt missing data
## in the rectangles smapled, there were no species, so this must be presented
# lets chnage the NaNs to 0's and see what the plot looks like (spikey...)
Gini.index$G[is.nan(Gini.index$G)] <- 0
plot(Gini.index$Year, Gini.index$G, type = "l", ylim=c(0,1)) 
# notice how 1985 also decided to show up!!

# niceeee, lets go onto D95, I envisage simialr problems 
#### D95 ####
print(noquote("D95"))
D95 <- d95(lorenz)
plot(D95$Year, D95$D95, type = "l", ylim=c(0,1)) 

# indeed, we have NaNs. Sort it out
D95$D95[is.nan(D95$D95)] <- 0
plot(D95$Year, D95$D95, type = "l", ylim=c(0,1)) 
# we see extreme values because not many rectangles were sampled in the earlier years
# I wonder if we calculate lorenz data on haul level like we did with the rectangles

### lets update the spatinds_funs.R before moving on and I forget what the hell I did
## I've updated the functions, now lets juct quickly test them
lorenz <- lorenz_data(hlhh = hlhh, 
                      yrs = yrs, 
                      qrs = qrs, 
                      species_aphia = species_aphia, 
                      stk_divs = stk_divs)
lorenz_plot(lorenz)

Gini.index <- Gini(lorenz)
plot(Gini.index$Year, Gini.index$Gini, type = "l", ylim=c(0,1)) 

D95 <- d95(lorenz)
plot(D95$Year, D95$D95, type = "l", ylim=c(0,1)) 
## after some edits, the new spat inds all look good
#
#
#
#
#
#
#
#
#
#
#
### Spread of Participation Index (SPI)
#### Prep data
print(noquote("Spread of Participation Index"))
spi_data <- spi_prep(hlhh = hlhh, 
                     yrs = yrs, 
                     qrs = qrs, 
                     species_aphia = species_aphia, 
                     stk_divs = stk_divs)
#### Calculate
spi <- spi_data %>% group_by(Year) %>% # Year only, not quarter
  filter(Area_27 %in% stk_divs) %>%
  summarise(SPI = spi_calc(TotalNo_TotalDur, area = 1)) ## all areas equal weight
## Take inverse of SPI, so high numbers = good/more distributed
spi$SPI <- 1-spi$SPI 
spi$SPI[is.nan(spi$SPI)] <- 0 # change NaNs to 0 
plot(spi$Year, spi$SPI, type = "l", ylim=c(0,1))

## weve got a lot of negative outputs in SPI, this is goiing to take some time to debug
## the code for the spi is horrible, so could probably do with a makeover
## hmmm, weird...print spi for a seconf time and the values chnaged
## maybe i coldnt read scientific numbers properly...probs the case





    ################################################################################
    ### Merge all spatial indicator values into a single data frame
    message("Merging and saving spatial indicator data...")
    ## select the columns that we need
    Gini.index <- Gini.index[c("Year", "G")]
    D95 <- D95[c("Year", "D95")]
    np.hauls <- np.hauls[c("Year", "PosArea")]
    np.rects <- np.rects[c("Year", "PosArea")]
    spi <- spi[c("Year", "SPI")]
    ## merge
    si <- Reduce(function(x, y) merge(x, y, by = "Year"), list(Gini.index, D95, np.hauls, np.rects, spi))
    ## rename some columns
    colnames(si)[2] <- "Gini Index"
    colnames(si)[4] <- "Positve Area (Rectangle)"
    colnames(si)[5] <- "Positive Area (Haul)"
    
    ### Save
    ## Create directories
    if(dir.exists(paste0(si.output.path, stk.chr)) == FALSE){
      dir.create(paste0(si.output.path, stk.chr))
    }
    if(dir.exists(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]))) == FALSE){
      dir.create(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx])))
    }
    if(dir.exists(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey]))) == FALSE){
      dir.create(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey])))
    }
    si.save.path <- paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey]))
    
    if(dir.exists(paste0(si.save.path, "/SpatIndData")) == FALSE){
      dir.create(paste0(si.save.path, "/SpatIndData"))
    }  
    save(si, file = paste0(si.save.path, "/SpatIndData/SpatIndData - ", stk.chr, " - ", names(stk_data_filtered[indx]), " - ", names(stk_data_filtered[[indx]][survey]), ".rda"))
    
    ### Plot
    message("Plotting spatial indictaor time series...")
    ## Create directory
    if(dir.exists(paste0(si.save.path, "/SpatIndPlot")) == FALSE){
      dir.create(paste0(si.save.path, "/SpatIndPlot"))
    } 
    ## convert data frame to long format
    si_long <- si %>% tidyr::pivot_longer(cols = c("Gini Index", "D95", "Positve Area (Rectangle)", "Positive Area (Haul)", "SPI" ), 
                                          names_to = "Indicator",
                                          values_to = "Value")
    ## create the plot
    si_plot <- ggplot() +
      geom_line(data = si_long, aes(x = Year, y = Value, group = Indicator, colour = Indicator)) +
      labs(title = paste0("Spatial Indicators for ", stk.chr,
                          " | Survey Index: ", names(stk_data_filtered[indx]), " | Survey: ", names(stk_data_filtered[[indx]][survey]), " (", min(yrs), "-", max(yrs), ", Q", qrs, ")")) +
      theme(plot.title = element_text(size = 8)) +
      coord_cartesian(ylim = c(0,1))
    ## save plot
    cowplot::save_plot(plot = si_plot, filename = paste0(si.save.path, "/SpatIndPlot/SpatIndPlot - ", stk.chr, " - ", names(stk_data_filtered[indx]), " - ", names(stk_data_filtered[[indx]][survey]), ".png"))
    print(noquote("-------------------------------"))
    
  }
}


###### 04/07/23 ###########
# here I try to get ages appended to hlhh data and see if the plots are the same
# ple27
### Select the stock to analyse
unique(allstk_metadata$stk_id) # ignore so.27.7d for now, need to find YFS survey data
stk.chr <- "ple.27.420"          # change accordingly

### Load in stock assessments
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock_Objects/")
list.files(stockobj.path) # identify correct stock assessment object to match with stk.chr
stkobj <- "ple.27.420_assessment_FLR_2022.RData" # change accordingly
load(paste0(stockobj.path, stkobj)) # load in the stock object
stk <- ple.27.420 # change accordingly


##### Run through #####
#### Set params ####
### Get the stock species name
species <- allstk_refpts %>% filter(
  stk_name == stk.chr) %>%
  select(spcs_name, latin_name)
head(species)

### Get Valid Aphia ID using Latin name
species_aphia <- findAphia(species$latin_name, latin = TRUE)

### Get the ICES divisions for the stock of interest
stk_divs2 <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(stk_divs) %>%
  unique()
stk_divs <- strsplit(as.character(stk_divs2), split = ", ")[[1]]
print(paste0("ICES Divisions for ", species$spcs_name, " (", stk.chr, ") : ", stk_divs2))

### check stk_divs are in the ices_rect and ices_divs
stk_divs %in% ices_rect$Area_27
stk_divs %in% ices_divs$Area_27

### Get the advice sheet reference points for the stock
stk_refpts <- allstk_refpts %>% filter(
  stk_name == stk.chr)
print(stk_refpts)

#### Survey info ####
### Get survey information for the current stock we are looking at
stk_surveys <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(-description, -date_published)
print(stk_surveys)

### Get the name of the individual indexes used in the stock assessment
## Each index can be comprised of multiple survey data sources 
## Get the name of the survey indices used in the stock assessment:
stk_surveys_indices <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(survey_index) %>%
  unique()
print(stk_surveys_indices)



indx <- "BTS+IBTS Q3"
survey <- "BTS"

stkindx_surveys <- allstk_metadata %>%
  filter(stk_id == stk.chr,
         survey_index == indx)
print(stkindx_surveys)
print(paste0("Survey: ", survey))

# for each survey within each survey index...
# get the start and end years of the survey data used
yr_strt <- stkindx_surveys$survey_yrs_start[stkindx_surveys$survey_name==survey]
yr_end <- stkindx_surveys$survey_yrs_end[stkindx_surveys$survey_name==survey] 
yr_strt
yr_end
yrs <- c(yr_strt:yr_end)
# assumptions made if start or end year is not given in the stock assessment
if(is.na(yr_end)==TRUE){yr_end <- 2022} # assume most recent data used
if(is.na(yr_strt)==TRUE){yr_strt <- 1900} # assume oldest data used
# get the quarters used in stock stock index for this survey
qrs <- stkindx_surveys$survey_qrs[stkindx_surveys$survey_name==survey]
qrs
range(hlhh_age$Age, na.rm=T)
age_range <- c(0:54)

# sometimes the quarters used is not available in the advice sheets, so 
# assume all quarters were used. This isn't explicitly stated probably because some 
# surveys are only run in certain quarters so there is no need to say.
# Setting `qrs` to c(1:4) will retain whatever data the survey has. 
if(is.na(qrs)){
  qrs <- c(1:4)
}

###############################################################################
              ################# Run from here ######################
###############################################################################
# load raw survey data
load(paste0(surveydata.path, "BTS/bts.hh.rda"))
load(paste0(surveydata.path, "BTS/bts.hl.rda"))
load(paste0(surveydata.path, "BTS/bts.ca.rda"))
hh <- bts.hh
hl <- bts.hl
ca <- bts.ca

## Remove duplicate rows of data
tictoc::tic()
message("Running unique() to get rid of duplicate rows")
hh2 <- unique(hh)
hl <- unique(hl)
ca <- unique(ca)
tictoc::toc()

## Add ICES Division to HH data
# work out which rectangles belong to which division and append this to HH data
tictoc::tic()
message("Getting location information from ICES shp")
area_div <- dplyr::distinct(ices_rect[c("ICESNAME", "Area_27", "Shape_Area")])
hh <- merge.data.frame(hh2,     
                       area_div,
                       by.x = "StatRec",
                       by.y = "ICESNAME") # this is stat rec names from shp file
message("Checking merge")
# check that the merge did not result in multiplication of rows
if(!(nrow(hh) == nrow(hh2))){
  warning("Merge not correct. Number of rows between old and new dataset are not identical")
}
rm(hh2)
tictoc::toc()

## Create lon_lat column in hh
hh$lon_lat <- paste0(hh$ShootLong, "_", hh$ShootLat)

## remove rows of data where TotalNo < 0
# identified this issue after funky Lorenz plots and TSS for D95 and Gini in turbot
# where many years of data had -9 TotalNo
# was there a reason that it was always -9??? (SNS survey)
message("Removing rows in hl where TotalNo < 0")
hl <- subset(hl, TotalNo >= 0)

## Create a Haul ID for all record types
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
ca$haul.id <- as.character(paste(ca$Year, 
                                 ca$Quarter, 
                                 ca$Country, 
                                 ca$Ship, 
                                 ca$Gear, 
                                 ca$StNo, 
                                 ca$HaulNo, 
                                 sep = ":"))

## Calculate sum of TotalNo for each species within each haul.id
hl_sum <- hl %>% 
  select(-Sex, -LngtCode, -LngtClass, -CatCatchWgt, -HLNoAtLngt, -LenMeasType) %>%
  unique() %>%
  group_by(haul.id, Valid_Aphia) %>%
  mutate(TotalNo = sum(TotalNo)) %>%
  relocate(RecordType, haul.id, Survey, Year, Quarter, Ship, Country,
           Valid_Aphia, TotalNo, Gear, GearEx, DoorType, StNo)

## Merge HH and HL data
tictoc::tic()
message("Merging hl and hh data into hlhh")
# check which columns are identical 
cols <- janitor::compare_df_cols(hh, hl_sum)
na.omit(cols[cols$hh == cols$hl_sum,])
m <- hh[c("haul.id", "Survey", "Year", "Quarter", "Month", "Survey","Country", 
          "Ship", "Gear", "GearEx", "DoorType", "HaulDur", "HaulNo", 
          "StNo", "SweepLngt", "StatRec", "Area_27", "ShootLong", "ShootLat", "lon_lat", "HaulVal", "Depth")]
hlhh <- merge(hl_sum,
              dplyr::distinct(m),
              c("haul.id", "Survey", "Year", "Quarter", "HaulNo", "StNo", "Gear", "GearEx", "DoorType","Ship", 
                "SweepLngt", "Country"))

message("Some data might be removed after merging hh and hl. This is typically due to haul.id's in HL not being in HH. Check which rows were removed, if any, by using the following code:")
print("anti_join(SURVEY_NAME.data$hl, SURVEY_NAME.data$hlhh, by = c('haul.id','Year','Quarter','Country','Ship','Gear','SweepLngt','GearEx','DoorType','StNo','HaulNo','TotalNo','HaulDur'))",
      quote = FALSE)

##############################
## Add age data from CA to HLHH
## rename cols
ca <- rename(ca, StatRec = AreaCode)

## merge age data from CA to HLHH
cols <- janitor::compare_df_cols(hl, ca)
na.omit(cols[cols$hl == cols$ca,])
m <- ca[c("haul.id", "Survey", "Year", "Quarter","Country","Valid_Aphia",
          "Ship", "Gear", "GearEx", "DoorType", "HaulNo", "StNo", "SweepLngt", 
          "SpecCode", "SpecCodeType", "Sex", "DateofCalculation", 
          "LngtClass", "Age", "StatRec")]
hhca_age <- merge(hl, m, c("haul.id", "Survey", "Year", "Quarter","Country","Valid_Aphia",
                           "Ship", "Gear", "GearEx", "DoorType", "HaulNo", "StNo", "SweepLngt", 
                           "SpecCode", "SpecCodeType", "Sex", "DateofCalculation", "LngtClass"))


## create list of datasets
data.list <- list(hh = hh, 
                  hl = hl, 
                  ca = ca, 
                  hlhh = hlhh)
tictoc::toc()
return(data.list)
# filter the years and quarters of survey data, for each survey, within each survey index
# CHECK: do I need to do c(qrs). Will this code work if there are multiple quartes that need to be selected
hlhh2 <- hlhh %>%
         filter(Year %in% c(yr_strt:yr_end),
                Quarter %in% qrs)
hl2 <- hl %>%
       filter(Year %in% c(yr_strt:yr_end),
              Quarter %in% qrs)
hh2 <- hh %>%
       filter(Year %in% c(yr_strt:yr_end),
              Quarter %in% qrs)
ca2 <- ca %>%
       filter(Year %in% c(yr_strt:yr_end),
              Quarter %in% qrs)
hlhh_age2 <- hlhh_age %>%
             filter(Year %in% c(yr_strt:yr_end),
                    Quarter %in% qrs)

np.rects <- pa_rect(hh = hh, 
                    hlhh = hlhh_age2, 
                    yrs = yrs, 
                    qrs = qrs, 
                    species_aphia = species_aphia, 
                    stk_divs = stk_divs)
plot(np.rects$Year, np.rects$PosArea, type = "b", ylim=c(0,1))

filter(hh, c(haul.id == "1996:3:GB:74RY:BT4A:1:55"))
testhl <- filter(hl, haul.id %in% c("1996:3:GB:74RY:BT4A:1:55",
                                    "1996:3:GB:74RY:BT4A:100:100", 
                                    "1996:3:GB:74RY:BT4A:102:101", 
                                    "2021:3:NL:64T2:BT8:45E711:11", 
                                    "2021:3:NL:64T2:BT8:45F116:16") ,  Valid_Aphia == species_aphia)

filter(hlhh, haul.id == "1996:3:GB:74RY:BT4A:1:55", Valid_Aphia == species_aphia)
filter(hlhh_age, haul.id == "1996:3:GB:74RY:BT4A:1:55", Valid_Aphia == species_aphia)
filter(ca, haul.id == "1996:3:GB:74RY:BT4A:1:55", Valid_Aphia == species_aphia)



dup <- duplicated(t$haul.id)
t[dup,]

filter(t, haul.id == "1985:3:NL:64SS:BT8:38F712:12")
# t is larger than hh because there is duplicated haul.id rows
# for each species. As we calculcute sum(TotalNo) for each species


######### 05/07/23 ############
# There are NA warnings in had.46a20 and ple.27.420
# Here I investigate what is going on by running through manually
# Run
## Set Parameters
### Choose Stock
### Stocks available
unique(allstk_metadata$stk_id) # ignore so.27.7d for now, need to find YFS survey data
### Select the stock to analyse
stk <- had.27.46a20
stk.chr <- "had.27.46a20"


### Species Name
### Get the stock species name
species <- allstk_refpts %>% filter(
  stk_name == stk.chr) %>%
  select(spcs_name, latin_name)
head(species)

### Get Valid Aphia ID using Latin name
species_aphia <- findAphia(species$latin_name, latin = TRUE)

### Stock Divisions
### Get the ICES divisions for the stock of interest
stk_divs2 <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(stk_divs) %>%
  unique()
print(paste0("ICES Divisions for ", species$spcs_name, " (", stk.chr, ") : ", stk_divs2))
stk_divs <- strsplit(as.character(stk_divs2), split = ", ")[[1]]
rm(stk_divs2)

### check stk_divs are in the ices_rect and ices_divs
all(stk_divs %in% ices_rect$Area_27)
all(stk_divs %in% ices_divs$Area_27)

### Stock Reference Points
### Get the advice sheet reference points for the stock
stk_refpts <- allstk_refpts %>% filter(
  stk_name == stk.chr)
print(stk_refpts)

## Survey Information
### `stk_surveys`
### Get survey information for the current stock we are looking at
stk_surveys <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(-description, -date_published)
print(stk_surveys)

### `data.list`
### Select the surveys used within the stock assessment
## we will load in the survey data from local drive 
surveydata.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/Survey Data/"

## Load the survey data used in the stock assessment for the current stock we are looking at
## Store in a list so we can loop through it later
## e.g. for Witch, BTS and NS-IBTS survey data were used. We store all this survey data into a list.
## we will filter this survey data to the correct years and quarters for each survey index later
## e.g. witch has 3 survey indices, BTS Q3, IBTS Q1, IBTS Q3, with different year ranges
data.list <- list()
for(survey in unique(stk_surveys$survey_name)){
  survey_data <- list.files(paste0(surveydata.path, survey), pattern = "*.data.rds*")
  survey_data <- try(load(paste0(surveydata.path, survey, "/", survey_data)))
  survey_data <- try(get(survey_data))
  data.list[[survey]] <- survey_data
}

### Check that surveys loaded are what we expect
summary(data.list)
row.names(summary(data.list)) %in% unique(stk_surveys$survey_name) # should be TRUE

### `stk_surveys_indices` (might not need)
### Get the name of the individual indexes used in the stock assessment
### Each index can be comprised of multiple survey data sources 
### Get the name of the survey indices used in the stock assessment:
stk_surveys_indices <- allstk_metadata %>% filter(
  stk_id == stk.chr) %>%
  select(survey_index) %>%
  unique()
print(stk_surveys_indices)

### `stk_data_filtered`

### Get the survey data used in each index and filter it so that it matches
### the data used in the stock assessment e.g. by Species, Years, and Quarters. 
stk_data_filtered <- list()
for(indx in stk_surveys_indices$survey_index){
  # for each survey index in the stock assessment...
  print(paste0("Survey Index: ", indx))
  # get the row of data in allstk_metadata for this survey index
  stkindx_surveys <- allstk_metadata %>%
    filter(stk_id == stk.chr,
           survey_index == indx)
  print(stkindx_surveys)
  #stk_data_filtered[indx] <- indx
  
  for(survey in stkindx_surveys$survey_name){
    print(paste0("Survey: ", survey))
    # for each survey within each survey index...
    # get the start and end years of the survey data used
    yr_strt <- stkindx_surveys$survey_yrs_start[stkindx_surveys$survey_name==survey]
    yr_end <- stkindx_surveys$survey_yrs_end[stkindx_surveys$survey_name==survey] 
    # assumptions made if start or end year is not given in the stock assessment
    if(is.na(yr_end)==TRUE){yr_end <- 2022} # assume most recent data used
    if(is.na(yr_strt)==TRUE){yr_strt <- 1900} # assume oldest data used
    # get the quarters used in stock stock index for this survey
    qrs <- stkindx_surveys$survey_qrs[stkindx_surveys$survey_name==survey]
    
    # sometimes the quarters used is not available in the advice sheets, so 
    # assume all quarters were used. This isn't explicitly stated probably because some 
    # surveys are only run in certain quarters so there is no need to say.
    # Setting `qrs` to c(1:4) will retain whatever data the survey has. 
    if(is.na(qrs)){
      qrs <- c(1:4)
    }
    
    # filter the years and quarters of survey data, for each survey, within each survey index
    # CHECK: do I need to do c(qrs). Will this code work if there are multiple quartes that need to be selected
    hlhh <- try(data.list[[survey]]$hlhh %>%
                  filter(Year %in% c(yr_strt:yr_end),
                         Quarter %in% qrs)) 
    hl <- try(data.list[[survey]]$hl %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    hh <- try(data.list[[survey]]$hh %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    ca <- try(data.list[[survey]]$ca %>%
                filter(Year %in% c(yr_strt:yr_end),
                       Quarter %in% qrs))
    
    stk_data_filtered[[indx]][[survey]] <- list(hlhh = hlhh, hl = hl, hh = hh, ca = ca)
  }
}

### Check data has been filtered to the correct index, survey, years, and quarters
for(indx in 1:length(stk_data_filtered)){
  # get survey index name from data
  message(paste0("Survey Index:             ", names(stk_data_filtered[indx])))
  # does this match with the excel doc
  print(noquote(paste0("Survey index matches? ", names(stk_data_filtered[indx]) %in% stk_surveys$survey_index)))
  for(survey in 1:length(stk_data_filtered[[indx]])){
    # get survey name within this survey index
    print(noquote(paste0("  Survey:             ", names(stk_data_filtered[[indx]][survey]))))
    # does this survey match with exceld doc
    print(noquote(paste0("  Survey matches?     ", names(stk_data_filtered[[indx]][survey]) ==
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_name"])))
    # get survey data years from the dataset
    print(noquote(paste0("  Survey years:       ", min(stk_data_filtered[[indx]][[survey]]$hlhh$Year), "-", max(stk_data_filtered[[indx]][[survey]]$hlhh$Year))))
    # does this match with the start and end years in the excel doc
    print(noquote(paste0("  Start year matches? ", 
                         min(stk_data_filtered[[indx]][[survey]]$hlhh$Year) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_yrs_start"])))
    print(noquote(paste0("  End year matches?   ", 
                         max(stk_data_filtered[[indx]][[survey]]$hlhh$Year) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_yrs_end"])))
    # get survey quarters from the dataset
    print(noquote(paste0("  Survey quarters:    ", unique(stk_data_filtered[[indx]][[survey]]$hlhh$Quarter))))
    # does this match with the survey quarters in the excel doc
    print(noquote(paste0("  Quarters match?     ", 
                         unique(stk_data_filtered[[indx]][[survey]]$hlhh$Quarter) == 
                           stk_surveys[stk_surveys$survey_index==names(stk_data_filtered[indx]) & 
                                         stk_surveys$survey_name==names(stk_data_filtered[[indx]][survey]), "survey_qrs"])))
    print(noquote("---------------------------"))
  }
}

### Output path to save data and plots
## Keeping this to my one drive, files may be too large for GitHub
si.output.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/10Stock_Outputs/"

### Calculate spatial indicators for every year in each individual time series of survey data
for(indx in 1:length(stk_data_filtered)){
  for(survey in 1:length(stk_data_filtered[[indx]])){
    # get survey name within this survey index
    message(names(stk_data_filtered[indx]))
    print(noquote(paste0("Survey Index:       ", names(stk_data_filtered[indx]))))
    print(noquote(paste0("Survey:             ", names(stk_data_filtered[[indx]][survey]))))
    # get filtered survey data
    hlhh <- stk_data_filtered[[indx]][[survey]]$hlhh
    hh <- stk_data_filtered[[indx]][[survey]]$hh
    yrs <- unique(hlhh$Year)
    qrs <- unique(hlhh$Quarter)
    print(noquote(paste0("Survey years:       ", min(yrs), "-", max(yrs))))
    print(noquote(paste0("Survey quarters:    ", qrs)))
    
    ### Calculate spatial indicators for each survey
    message("Calculating spatial indicators")
    ### Positive Area (by ICES rectangle)
    print(noquote("Positive Area (Rectangle)"))
    np.rects <- pa_rect(hh = hh, 
                        hlhh = hlhh, 
                        yrs = yrs, 
                        qrs = qrs, 
                        species_aphia = species_aphia, 
                        stk_divs = stk_divs)
    
    ### Positive Area (by Haul)
    print(noquote("Positive Area (Haul)"))
    np.hauls <- pa_haul(hh = hh, 
                        hlhh = hlhh, 
                        yrs = yrs, 
                        qrs = qrs, 
                        species_aphia = species_aphia, 
                        stk_divs = stk_divs)
    
    ### Lorenz curve data
    print(noquote("Lorenz Curve"))
    lorenz <- lorenz_data(hlhh = hlhh, 
                          yrs = yrs, 
                          qrs = qrs, 
                          species_aphia = species_aphia, 
                          stk_divs = stk_divs)
    #### Gini index
    print(noquote("Gini Index"))
    Gini.index <- Gini(lorenz)
    
    #### D95
    print(noquote("D95"))
    D95 <- d95(lorenz)
    
    ### Spread of Participation Index (SPI)
    #### Prep data
    print(noquote("Spread of Participation Index"))
    spi_data <- spi_prep(hlhh = hlhh, 
                         yrs = yrs, 
                         qrs = qrs, 
                         species_aphia = species_aphia, 
                         stk_divs = stk_divs)
    #### Calculate
    spi <- spi_data %>% group_by(Year) %>% # Year only, not quarter
      filter(Area_27 %in% stk_divs) %>%
      summarise(SPI = spi_calc(TotalNo_TotalDur, area = 1)) ## all areas equal weight
    #### Take inverse of SPI, so high numbers = good
    spi$SPI <- 1-spi$SPI 
    spi$SPI[is.nan(spi$SPI)] <- 0 # change NaNs to 0 
    ################################################################################
    ### Merge all spatial indicator values into a single data frame
    message("Merging and saving spatial indicator data...")
    ## select the columns that we need
    Gini.index <- Gini.index[c("Year", "Gini Index")]
    D95 <- D95[c("Year", "D95")]
    np.hauls <- np.hauls[c("Year", "PosArea")]
    np.rects <- np.rects[c("Year", "PosArea")]
    spi <- spi[c("Year", "SPI")]
    ## merge
    si <- Reduce(function(x, y) merge(x, y, by = "Year"), list(Gini.index, D95, np.hauls, np.rects, spi))
    ## rename some columns
    #colnames(si)[2] <- "Gini Index"
    colnames(si)[4] <- "Positve Area (Rectangle)"
    colnames(si)[5] <- "Positive Area (Haul)"
    
    ### Save
    ## Create directories
    if(dir.exists(paste0(si.output.path, stk.chr)) == FALSE){
      dir.create(paste0(si.output.path, stk.chr))
    }
    if(dir.exists(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]))) == FALSE){
      dir.create(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx])))
    }
    if(dir.exists(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey]))) == FALSE){
      dir.create(paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey])))
    }
    si.save.path <- paste0(si.output.path, stk.chr, "/", names(stk_data_filtered[indx]), "/", names(stk_data_filtered[[indx]][survey]))
    
    if(dir.exists(paste0(si.save.path, "/SpatIndData")) == FALSE){
      dir.create(paste0(si.save.path, "/SpatIndData"))
    }  
    save(si, file = paste0(si.save.path, "/SpatIndData/SpatIndData - ", stk.chr, " - ", names(stk_data_filtered[indx]), " - ", names(stk_data_filtered[[indx]][survey]), ".rda"))
    
    ### Plot
    message("Plotting spatial indictaor time series...")
    ## Create directory
    if(dir.exists(paste0(si.save.path, "/SpatIndPlot")) == FALSE){
      dir.create(paste0(si.save.path, "/SpatIndPlot"))
    } 
    ## convert data frame to long format
    si_long <- si %>% tidyr::pivot_longer(cols = c("Gini Index", "D95", "Positve Area (Rectangle)", "Positive Area (Haul)", "SPI" ), 
                                          names_to = "Indicator",
                                          values_to = "Value")
    ## create the plot
    si_plot <- ggplot() +
      geom_line(data = si_long, aes(x = Year, y = Value, group = Indicator, colour = Indicator)) +
      labs(title = paste0("Spatial Indicators for ", stk.chr,
                          " | Survey Index: ", names(stk_data_filtered[indx]), " | Survey: ", names(stk_data_filtered[[indx]][survey]), " (", min(yrs), "-", max(yrs), ", Q", qrs, ")")) +
      theme(plot.title = element_text(size = 8)) +
      coord_cartesian(ylim = c(0,1))
    ## save plot
    cowplot::save_plot(plot = si_plot, filename = paste0(si.save.path, "/SpatIndPlot/SpatIndPlot - ", stk.chr, " - ", names(stk_data_filtered[indx]), " - ", names(stk_data_filtered[[indx]][survey]), ".png"))
    print(noquote("-------------------------------"))
    
  }
}

### From this run through, we can see that the errors are coming from the SWC-IBTS and SCOWCGFS
### And mostly happening when we get to the Lorenz curve. Althought two issues occurred with the spi_prep function
## Lets choose one of these surveys and run through the Lorenz functions manually
hlhh <- stk_data_filtered$Q1$SCOWCGFS$hlhh
yrs <- unique(hlhh$Year)
qrs <- unique(hlhh$Quarter)

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
    select(haul.id, Valid_Aphia, Year, StatRec, TotalNo, HaulDur) # rearrange cols
  
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
    filter(Valid_Aphia == species_aphia) %>%
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

lorenz <- lorenz_data(hlhh = hlhh, 
                      yrs = yrs, 
                      qrs = qrs, 
                      species_aphia = species_aphia, 
                      stk_divs = stk_divs)

### very simple fix, TotalNo was integer in one df and numeric in another 
### so the join of dfs caused an error

### lets move onto the error in spi_prep
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

spi_data <- spi_prep(hlhh = hlhh, 
                     yrs = yrs, 
                     qrs = qrs, 
                     species_aphia = species_aphia, 
                     stk_divs = stk_divs)
#### Calculate
spi <- spi_data %>% group_by(Year) %>% # Year only, not quarter
  filter(Area_27 %in% stk_divs) %>%
  summarise(SPI = spi_calc(TotalNo_TotalDur, area = 1)) ## all areas equal weight
#### Take inverse of SPI, so high numbers = good
spi$SPI <- 1-spi$SPI 
spi$SPI[is.nan(spi$SPI)] <- 0 # change NaNs to 0 

plot(spi$Year, spi$SPI, type = "l", ylim=c(0,1))


# CoG and Inertia variations ####
# psuedo data
x <- c(-10, 5, 9, -4, -3, 12, 16, 0, 4, 17, 15, 10, 12, 11)
y <- c(50, 32, 46, 42, 59, 54, 39, 60, 46, 40, 43, 35, 40, 43)
z <- c(1, 5, 10, 6, 2, 1, 1, 2, 7, 9, 1, 10, 12, 14)
z <- c(1, 1, 20, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 14)


# current Cog:
cog <- function(x, y, z = 1, plot = FALSE, lonlat2km = FALSE, km2lonlat = FALSE){
  # Equal density
  if(length(z) == 1){
    z <- rep(z, length(x))
  }
  # Coordinate conversion
  if(lonlat2km){
    xy <- SpatialEpi::latlong2grid(cbind(x,y))
    d <- cbind(xy,z)
  }else if(km2lonlat){
    xy <- SpatialEpi::grid2latlong(cbind(x,y))
    d <- cbind(xy,z)
  } else{
    d <- as.data.frame(cbind(x,y,z))
  }
  # Density weighted CoG, z=1 = equal weighting
  cog_x <- sum(d$x*d$z)/sum(d$z)
  cog_y <- sum(d$y*d$z)/sum(d$z)
  cog_xy <- cbind(cog_x, cog_y)
  
  d <- as.data.frame(d)
  cog_xy <- as.data.frame(cog_xy)

  # Plot
  if(plot){
    if(length(unique(z))==1){
      a = NULL
    } else{
      d$a <- d$z
    }
    p <- ggplot() +
      geom_point(data = d, aes(x,y, colour = a), size = z) +
      geom_point(data = cog_xy, aes(cog_x, cog_y), colour = "blue", shape = 15, size = 2) +
      geom_text(data = cog_xy, aes(cog_x, cog_y-(cog_y*0.02), label = "CoG")) +
      paletteer::scale_colour_paletteer_c("grDevices::Geyser") +
      labs(colour = "z")
    print(p)
  }
  
  return(cog_xy)
}

cog(x,y,z, plot = T, lonlat2km = T, km2lonlat = F)

# weighted inertia
inertia <- function(x, y, z = 1, plot = FALSE, lonlat2km = FALSE, km2lonlat = FALSE, rand = FALSE){
  
  # Use rnorm generated data
  if(missing(x) & missing(y) & rand == T){
    x <- rnorm(50)
    y <- rnorm(50)
    z <- abs(rnorm(50)^2)
  }
  
  # For equal weighting of xy points
  if(length(z) == 1){
    z <- rep(z, length(x))
  }
  
  # Measurement transformation
  if(lonlat2km){
    xy <- SpatialEpi::latlong2grid(cbind(x,y))
    d <- cbind(xy,z)
  } else if(km2lonlat){
    xy <- SpatialEpi::grid2latlong(cbind(x,y))
    d <- cbind(xy,z)
  } else{
    d <- as.data.frame(cbind(x,y,z))
  }
  
  # Density weighted CoG
  cog_x <- sum(d$x*d$z)/sum(d$z)
  cog_y <- sum(d$y*d$z)/sum(d$z)
  cog_xy <- as.data.frame(cbind(cog_x, cog_y))
  # Inertia
  dx <- d$x - cog_x
  dy <- d$y - cog_y
  ix <- sum((dx^2)*d$z)/sum(d$z)
  iy <- sum((dy^2)*d$z)/sum(d$z)
  inert <- ix+iy
  # Weighted PCA
  M11 <- sum(dx^2*d$z)
  M22 <- sum(dy^2*d$z)
  M21 <- sum(dx*dy*d$z)
  M12 <- M21
  M <- matrix(c(M11, M12, M21, M22), ncol = 2)
  x1 <- eigen(M)$vectors[1, 1]
  y1 <- eigen(M)$vectors[2, 1]
  x2 <- eigen(M)$vectors[1, 2]
  y2 <- eigen(M)$vectors[2, 2]
  r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
  # Principal axis coordinates
  e1 <- (y1/x1)^2
  sx1 <- x1/abs(x1)
  sy1 <- y1/abs(y1)
  sx2 <- x2/abs(x2)
  sy2 <- y2/abs(y2)
  xa <- cog_x + sx1 * sqrt((r1 * inert)/(1 + e1))
  ya <- cog_y + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
  xb <- 2 * cog_x - xa
  yb <- 2 * cog_y - ya
  xc <- cog_x + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
  yc <- cog_y + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
  xd <- 2 * cog_x - xc
  yd <- 2 * cog_y - yc
  Imax <- r1*inert 
  Imin <- (1-r1)*inert
  isotropy <- sqrt(Imin/Imax)
  
  # Plot
  if(plot){
    paletteer::scale_colour_paletteer_c("grDevices::Geyser")
    pal <- paletteer::paletteer_c("grDevices::Geyser", length(d$x))
    d <- d %>% arrange(z) %>%
      mutate(pal = pal)
    
    par(pty = "s")
    plot(d$x,d$y, cex = d$z, col = d$pal, pch = 16)
    segments(xa,ya,xb,yb, col = "blue")
    segments(xc,yc,xd,yd, col = "blue")
    points(cog_x, cog_y, col = "blue", pch = 15)
    ell <- car::dataEllipse(c(xa,xb,xc,xd),c(ya,yb,yc,yd), 
                            levels = 0.455, add = TRUE, plot.points = F, 
                            center.pch = FALSE, col = "lightblue",
                            fill = TRUE)
    points(xa,ya, col = "blue", cex = 1, pch = 16)
    points(xb,yb, col = "blue", cex = 1, pch = 16)
    points(xc,yc, col = "blue", cex = 1, pch = 16)
    points(xd,yd, col = "blue", cex = 1, pch = 16)
    text(x = median(d$x), y = max(d$y), labels = paste0("CoG: ", round(cog_x, 2), ", ", round(cog_y, 2)))
    text(x = median(d$x), y = max(d$y)*0.9, labels = paste0("Inertia: ", round(inert, 2)))
    text(x = median(d$x), y = max(d$y)*0.8, labels = paste0("Isotropy: ", round(isotropy, 2)))
  }

  output <- as.data.frame(cbind(cog_xy, inert, isotropy))
  return(output)
}

i <- inertia(x,y,z, rand = F, plot = T, lonlat2km = F)

x <- xa
y <- yb
h <- cog_x
k <- cog_y
xh2 <- ((x-h)^2)/a^2
yk2 <- ((y-k)^2)/b^2




cog_x <- i$cog_x
cog_y <- i$cog_y


ellBase <- cbind(sqrt(eigen(M)$values[1])*cos(angles), sqrt(eigen(M)$values[2])*sin(angles)) # normal ellipse
ellRot  <- eigen(M)$vectors %*% t(ellBase) 




a       <- sqrt(((xa-cog_x)^2) + ((ya-cog_y)^2))
b       <- sqrt(((xb-cog_x)^2) + ((yb-cog_y)^2))
area    <- pi*a*b



ctr <- c(i$cog_x, i$cog_y)
A <- cov(cbind(x, y))
angles <- seq(0, 2*pi, length.out=200)          # angles for ellipse



eigVal  <- eigen(A)$values
eigVec  <- eigen(A)$vectors
eigScl  <- eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
a       <- sqrt((xMat[1,1]-cog_x)^2 + (yMat[1,1]-cog_y)^2)
b       <- sqrt((xMat[2,2]-cog_x)^2 + (yMat[2,2]-cog_y)^2)
area    <- pi*a*b

ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) # normal ellipse
ellRot  <- eigVec %*% t(ellBase)                                          # rotated ellipse
#plot((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], asp=1, type="l", lwd=2)
plot(x,y, cex = z)
matlines(xMat, yMat, lty=1, lwd=2, col="green")
points(cog_x, cog_y, col = "blue", pch = 15)
points((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], asp=1, type="l", lwd=2)

points(ctr[1], ctr[2], pch=4, col="red", lwd=3)
points(x,y)


  
  plotG <- ggplot() +
    geom_point(data = d, aes(x,y),size = z) +
    #geom_point(data = cogw, aes(V1, V2), colour = "red") +
    geom_point(data = cogs, aes(V1, V2), colour = "blue") +
    stat_ellipse(data = d, aes(x = sum(((x-cg_x)^2)*z)/sum(z), y = sum(((y-cg_y)^2)*z)/sum(z)), type = "t")  # inertia
  print(plotG)
}
inertia2.1(x,y,z)

data.pca <- prcomp(d[,1:2], cor = TRUE)

g <- ggbiplot(data.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

xw <- x*z/sum(z)
yw <- y*z/sum(z)
cov_matrix <- cov(cbind(xw, yw))
eigen_result <- eigen(cov_matrix)
eigenvalues <- eigen_result$values


# Create a sample square matrix
A <- matrix(cbind(x, y))

# Calculate eigenvalues and eigenvectors
eigen_result <- eigen(A)
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors

# Create a scatterplot
plot(0,0, type = "n", xlab = "X", ylab = "Y")

# Plot eigenvectors as arrows
arrows(cg_x, cg_y, eigenvectors[1, 1], eigenvectors[2, 1], col = "blue", length = 0.1)
arrows(cg_x, cg_y, eigenvectors[1, 2], eigenvectors[2, 2], col = "red", length = 0.1)
points(x,y)


# get filtered survey data

z = FALSE; plot = FALSE; lonlat2km = FALSE; km2lonlat = FALSE; rand = FALSE
data <- filtersurveydata(hlhh, yrs, qrs,species_aphia, stk_divs)

foo <- function(hlhh, yrs, qrs, species_aphia, stk_divs, # data specifics
                cog = T, inertia = T, iso = T,           # spatial indicators
                plot = F,                                # plot
                density = F,                             # density weighted spat inds
                lonlat2km = F, km2lonlat = F){           # conversion of lonlats to/from km
  
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  d2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")
  # Check what data is available
  if(!identical(as.numeric(unique(d2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
  ", "Only data for quarters ", paste(sort(unique(d2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
  ", "No data for quarter ", paste(c(setdiff(qrs, unique(d2$Quarter)), setdiff(unique(d2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(d2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
  ", "No data for years ", paste(c(setdiff(yrs, unique(d2$Year)), setdiff(unique(d2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(d2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  d1 <- d2 %>% 
    filter(Valid_Aphia == species_aphia) %>%
    distinct() %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur)
  
  df <- data.frame()
  
  for(yr in yrs){
    dyrly <- d1 %>%
      filter(Year == yr)
    x <- dyrly$ShootLong
    y <- dyrly$ShootLat
    # Density
    if(density == TRUE){
    z <- dyrly$TotalNo_Dur
    } else{z <- rep(1, length(x))}
  
    # Measurement transformation
    if(lonlat2km){
      xy <- SpatialEpi::latlong2grid(cbind(x,y))
      d <- cbind(xy,z)
    } else if(km2lonlat){
      xy <- SpatialEpi::grid2latlong(cbind(x,y))
      d <- cbind(xy,z)
    } else{
      d <- as.data.frame(cbind(x,y,z))
    }
    # Density weighted CoG
    cog_x <- sum(d$x*d$z)/sum(d$z)
    cog_y <- sum(d$y*d$z)/sum(d$z)
    cog_xy <- as.data.frame(cbind(cog_x, cog_y))
    
    if(any(is.na(cog_xy)) & inertia == TRUE){
      inert <- NaN
    } else if(any(is.na(cog_xy)) & isotropy == TRUE){
      isotropy = NaN
    } else{
      # Inertia
      dx <- d$x - cog_x
      dy <- d$y - cog_y
      ix <- sum((dx^2)*d$z)/sum(d$z)
      iy <- sum((dy^2)*d$z)/sum(d$z)
      inert <- ix+iy
      # Weighted PCA
      M11 <- sum(dx^2*d$z)
      M22 <- sum(dy^2*d$z)
      M21 <- sum(dx*dy*d$z)
      M12 <- M21
      M <- matrix(c(M11, M12, M21, M22), ncol = 2)
      x1 <- eigen(M)$vectors[1, 1]
      y1 <- eigen(M)$vectors[2, 1]
      x2 <- eigen(M)$vectors[1, 2]
      y2 <- eigen(M)$vectors[2, 2]
      r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
      # Principal axis coordinates
      e1 <- (y1/x1)^2
      sx1 <- x1/abs(x1)
      sy1 <- y1/abs(y1)
      sx2 <- x2/abs(x2)
      sy2 <- y2/abs(y2)
      xa <- cog_x + sx1 * sqrt((r1 * inert)/(1 + e1))
      ya <- cog_y + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
      xb <- 2 * cog_x - xa
      yb <- 2 * cog_y - ya
      xc <- cog_x + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
      yc <- cog_y + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
      xd <- 2 * cog_x - xc
      yd <- 2 * cog_y - yc
      Imax <- r1*inert 
      Imin <- (1-r1)*inert
      isotropy <- sqrt(Imin/Imax)
      
      # Plot
      if(plot){
        pal <- paletteer::paletteer_c("grDevices::Geyser", length(d$x))
        dplot <- d %>% arrange(z) %>%
          mutate(pal = pal)
        posy <- jitter(cog_y)
        
        par(pty = "s")
        plot(dplot$x,dplot$y, cex = dplot$z, col = dplot$pal, pch = 16)
        segments(xa,ya,xb,yb, col = "blue")
        segments(xc,yc,xd,yd, col = "blue")
        points(cog_x, cog_y, col = "blue", pch = 15)
        ell <- car::dataEllipse(c(xa,xb,xc,xd),c(ya,yb,yc,yd), 
                                levels = 0.455, add = TRUE, plot.points = F, 
                                center.pch = FALSE, col = "lightblue",
                                fill = TRUE)
        points(xa,ya, col = "blue", cex = 1, pch = 16)
        points(xb,yb, col = "blue", cex = 1, pch = 16)
        points(xc,yc, col = "blue", cex = 1, pch = 16)
        points(xd,yd, col = "blue", cex = 1, pch = 16)
        
        text(x = cog_x, y = posy, labels = paste0("CoG: ", round(cog_x, 2), ", ", round(cog_y, 2)))
        text(x = cog_x, y = posy*0.995, labels = paste0("Inertia: ", round(inert, 2)))
        text(x = cog_x, y = posy*0.99, labels = paste0("Isotropy: ", round(isotropy, 2)))
        title(sub = yr)
      }
    }
  output <- as.data.frame(cbind(yr, cog_xy, inert, isotropy))
  df <- rbind(df, output)
  }
  df <- df[,c(TRUE, rep(cog, 2), inertia, iso)]
  df <- df %>%
    rename("Year" = yr) %>%
    mutate(Quarter = paste(sort(unique(d2$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter)
  return(df)
}

hlhh <- stk_data_filtered$`BTS Q3`$BTS$hlhh
hh <- stk_data_filtered[[indx]][[survey]]$hh
yrs <- unique(hlhh$Year)[unique(hlhh$Year) < 2022] # make sure max does not exceed max year in 2022 stock assessment 
qrs <- unique(hlhh$Quarter)
species_aphia <- 127136 #witch
stk_divs <- c("4.a","4.b","4.c","3.a.20","3.a.21","7.d")

test 
  foo(hlhh, yrs, qrs, species_aphia, stk_divs, 
            cog = T,
            inertia = T,
            iso = T,
            density = T, 
            lonlat2km = T, 
            plot = F)




plot(test$Year, test$cog_x, type = "l")
plot(test$Year, test$cog_y, type = "l")
plot(test$Year, test$inert, type = "l")
plot(test$Year, test$isotropy, type = "l")


#### Plot PA ####
library(ggplot2)
library(beepr)
library(patchwork)
library(scales)
mappa <- function(hlhh, yrs, qrs, species_aphia, stk_divs, ices_rect, ices_divs, scale_dens = F, return = T, save = F, path = NULL){
  
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  # Dataset for all surveys sites within stock region
  d2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I")
  # Check what data is available
  if(!identical(as.numeric(unique(d2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
  ", "Only data for quarters ", paste(sort(unique(d2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", stk_divs, ".
  ", "No data for quarter ", paste(c(setdiff(qrs, unique(d2$Quarter)), setdiff(unique(d2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(d2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
  ", "No data for years ", paste(c(setdiff(yrs, unique(d2$Year)), setdiff(unique(d2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(d2$Quarter)), collapse = ", "), "), region(s) ", stk_divs, ".\n"), immediate. = TRUE)
  }
  # Dataset for positive surveys sites within stock region
  d1 <- d2 %>% 
    filter(Valid_Aphia == species_aphia) %>%
    distinct() %>%
    mutate(TotalNo_Dur = TotalNo/HaulDur)

  
  ### An empty plot to create white space - think of more efficient way
  empty <- ggplot()+geom_point(aes(1,1), colour="white")+
    theme(axis.ticks=element_blank(), 
          panel.background=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),           
          axis.title.x=element_blank(), axis.title.y=element_blank())
  
  # plot occurrence and all survey sites for each year (quarters grouped)
  ## 2019 has no data --- is this correct?
  for(yr in yrs){
    # yr <- year
    message(paste("Mapping occupancy in year", yr, "for", species))
    
    message("1. Prep")
    rangex <- c(min(hlhh$ShootLong), max(hlhh$ShootLong))
    rangey <- c(min(hlhh$ShootLat), max(hlhh$ShootLat))
    
    # datasets for all surveys and survey with species presence
    #data_ab <- subset(survey_data, survey_data$Year == yr)
    #data_occ <- subset(species_data, species_data$Year == yr)
    
    # mean survey location and means survey location with species presence
    #cg <- data.frame(type = c("survey", "presence"),
    #                 lon = c(mean(data_ab$ShootLong), mean(data_occ$ShootLong)),
    #                 lat = c(mean(data_ab$ShootLat), mean(data_occ$ShootLat)))
    message("2. Main map plot")
    ########## start the plot ##############
    survey_plot <- ggplot() +
      ##########  spatial info  ##############
    # ICES rectangles
    geom_tile(data = ices_rect, mapping = aes(x = stat_x, y = stat_y), fill = NA, colour = "darkgrey") +
      # ICES Divisions
      geom_path(data = ices_divs, mapping = aes(x = long, y = lat, group = group, fill = NULL), color = "black") +
      coord_cartesian(xlim = rangex, ylim = rangey, expand = F) +
      
      # World map
      borders("world", fill = "grey", colour = "black") +
      # coord_quickmap(xlim = rangex, ylim = rangey) +
      ########## add scatter #############
    # surveys
    geom_point(data = d2, aes(ShootLong, ShootLat), 
               size = 0.5, colour = "red3", alpha = 0.5) +
      # surveys with species presence
      geom_point(data = d1, aes(ShootLong, ShootLat), 
                 size = 0.5, colour = "cyan3") +
      # marginal distribution - not needed anymore, CDF 
      # geom_rug(data = data_ab, aes(lon, lat), col = "blue", alpha = 0.1, sides = "tr") + 
      # geom_rug(data = data_occ, aes(lon, lat), col = "red", alpha = 0.1, sides = "tr") +
      ######### mean location ###########
    
      ######### formatting ############
    guides(colour = "none", fill = "none") +
      labs( x = "", y = "") +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        plot.title = element_text(size = 8))
    
    ################################
    ################################
    ######## density plots #########
    ################################
    ################################
    if(scale_dens == TRUE){
      ### longitudinal distribution
      message("3. Longitude marginal density plot")
      denstop <- ggplot() + 
        geom_density(data = d2, aes(x = ShootLong, y = after_stat(scaled), fill = "Presence"), alpha = 0.4) +
        geom_density(data = d1, aes(x = ShootLong,  y = after_stat(scaled), fill = "Survey"), alpha = 0.4) +
        #geom_vline(data = cg[1,2:3], aes(xintercept = lon), col = "blue", lty = 2) +
        #geom_vline(data = cg[2,2:3], aes(xintercept = lon), col = "red", lty = 2) +
        xlim(rangex) +
        theme_void() +
        guides(fill = "none") +
        labs(title = paste0("NS-IBTS ", species, " Presence-Absence in ", yr, " Q1 & Q3")) +
        theme(plot.title = element_text(hjust = 0, vjust = 15, size = 10))
      
      ### latitudinal distribution
      message("4. Latitude marginal density plot")
      
      densrigh <- ggplot() + 
        geom_density(data = d2, aes(x = ShootLat, y = after_stat(scaled), fill = "Presence"), alpha = 0.4) +
        geom_density(data = d1, aes(x = ShootLat, y = after_stat(scaled),  fill = "Survey"), alpha = 0.4) +
        #geom_vline(data = cg[1,2:3], aes(xintercept = lat), col = "blue", lty = 2) +
        #geom_vline(data = cg[2,2:3], aes(xintercept = lat), col = "red", lty = 2) +
        xlim(rangey) +
        theme_void() + 
        coord_flip() +
        theme(legend.key.size = unit(0.2, "cm"),
              legend.title = element_blank(),
              legend.position = c(0.45,1.075),
              legend.text = element_text(size = 8))
      
      ### arrange the plot
      
    } else{
      ### longitudinal distribution
      message("3. Longitude marginal density plot")
      denstop <- ggplot() + 
        geom_density(data = d2, aes(x = ShootLong, fill = "Presence"), alpha = 0.4) +
        geom_density(data = d1, aes(x = ShootLong,  fill = "Survey"), alpha = 0.4) +
        #geom_vline(data = cg[1,2:3], aes(xintercept = lon), col = "blue", lty = 2) +
        #geom_vline(data = cg[2,2:3], aes(xintercept = lon), col = "red", lty = 2) +
        xlim(rangex) +
        theme_void() +
        guides(fill = "none") +
        labs(title = paste0("NS-IBTS ", species, " Presence-Absence in ", yr, " Q1 & Q3")) +
        theme(plot.title = element_text(hjust = 0, vjust = 15, size = 10))
      
      ### latitudinal distribution
      message("4. Latitude marginal density plot")
      
      densrigh <- ggplot() + 
        geom_density(data = d2, aes(x = ShootLat, fill = "Presence"), alpha = 0.4) +
        geom_density(data = d1, aes(x = ShootLat, fill = "Survey"), alpha = 0.4) +
        #geom_vline(data = cg[1,2:3], aes(xintercept = lat), col = "blue", lty = 2) +
        #geom_vline(data = cg[2,2:3], aes(xintercept = lat), col = "red", lty = 2) +
        xlim(rangey) +
        theme_void() + 
        coord_flip() +
        theme(legend.key.size = unit(0.2, "cm"),
              legend.title = element_blank(),
              legend.position = c(0.45,1.075),
              legend.text = element_text(size = 8))
      
      ### arrange the plot
      message("5. Arranging plot")
    }
    
    sp <- plot_spacer() +
      plot_spacer() +
      plot_spacer() +
      denstop +   # tl
      plot_spacer() + # mr
      plot_spacer() + # tr
      survey_plot +   # bl
      densrigh +      # mr
      plot_spacer() + # br
      plot_layout(
        ncol = 3, 
        nrow = 3, 
        widths  = c(2, 0.5, 1),
        heights = c(0.2, 0.2, 2, 0.5))
    
    if(save == TRUE & !(is.null(path))){
      ggsave(sp, filename = paste0(path, "/NS-IBTS/", species, "/1. ", species, " P-A/NS-IBTS_", species, "_PA_", yr, "_Q1_Q3.png"),
             width = 7, height = 6)}
    beep(1)
    
    if(return == TRUE){
      return(sp)}
  }
}


#### Ellipse Area ####

ellipsearea <- function(hlhh, yrs, qrs, species_aphia, stk_divs){
  yrs <- as.numeric(yrs)
  qrs <- as.numeric(qrs)
  d2 <- hlhh %>%
    filter(Area_27 %in% stk_divs,
           Year %in% c(yrs),
           Quarter %in% c(qrs),
           HaulVal != "I", 
           Valid_Aphia == species_aphia) %>%
    distinct()
  
  # Check what data is available
  if(!identical(as.numeric(unique(d2$Quarter)), qrs)){
    warning(paste0("Data not found in all survey quarters provided. 
    ", "Only data for quarters ", paste(sort(unique(d2$Quarter)), collapse = ", ")), " available in ", unique(hlhh$Survey.x), " survey data (years ", min(yrs), ":", max(yrs), "), region(s) ", paste0(stk_divs, collapse = ", "), ".
    ", "No data for quarter ", paste(c(setdiff(qrs, unique(d2$Quarter)), setdiff(unique(d2$Quarter), qrs)), collapse = ", "), "\n", immediate. = TRUE)
  }
  if(!identical(as.numeric(sort(unique(d2$Year))), yrs)){
    warning(paste0("Species not found in all survey years provided. 
    ", "No data for years ", paste(c(setdiff(yrs, unique(d2$Year)), setdiff(unique(d2$Year), yrs)), collapse = ", "), " in ", unique(hlhh$Survey.x), " survey data (quarters ", paste(sort(unique(d2$Quarter)), collapse = ", "), "), region(s) ", paste0(stk_divs, collapse = ", "), ".\n"), immediate. = TRUE)
  }

  df <- data.frame()
  
  for(yr in yrs){
    data <- data.frame(x = d2[d2$Year == yr,]$ShootLong, 
                       y = d2[d2$Year == yr,]$ShootLat)
    
    p <- ggplot(data, aes(x = x, y = y)) +
      geom_point() +
      stat_ellipse(type = "t")
    
    pb <- ggplot_build(p)
    if(nrow(pb$data[[1]]) == 0){
      ela <- 0
    } else{
      el <- pb$data[[2]][c("x", "y")]
      ctr <- MASS::cov.trob(data)$center #updated previous answer here
      dist2center <- sqrt(rowSums((t(t(el) - ctr))^2))
      ela <- pi * min(dist2center) * max(dist2center)
      }
    output <- cbind("Year" = yr, "Ellipse Area" = ela)
    df <- rbind(df, output)
  }
  df <- df %>%
    mutate(Quarter = paste(sort(unique(hlhh$Quarter)), collapse = ", ")) %>%
    relocate(Year, Quarter,`Ellipse Area`)
  return(df)
}


