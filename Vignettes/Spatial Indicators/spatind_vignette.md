---
title: "Spatial Indicators Summary"
subtitle: ""
author: "Peter Kidd & Laurence Kell"
date: "23 January, 2024"
output: 
  html_document:
    toc: true
    toc_float: true
    toc_collapsed: false
    number_section: true
    keep_md: true
mathjax: TRUE
fig_width: 6 
fig_height: 4 
tags: FLR FLCore introduction
license: Creative Commons Attribution-ShareAlike 4.0 International Public License
#knit: (function(inputFile, encoding) {
#  rmarkdown::render(inputFile, encoding = encoding, output_dir="html")})
#bibliography: bibliography.bib
---



[Jump to Introduction](#intro)

[Jump to Data Preparation](#prep)

  [Jump to Stock ple.27.420](#stk)

[Jump to Empirical Spatial Indicator Analysis](#esia)

  * [Jump to Location](#loc)
  
  * [Jump to Range](#range)
  
  * [Jump to Occupancy](#occ)
  
  * [Jump to Aggregation](#agg)

[Jump to More Information](#More)

[Jump to References](#References)

# Introduction {#intro}

This vignette provides an overview of spatial indicators used to estimate the location, range, occupancy, and aggregation of populations in the context of fisheries management.

## Data Preparation {#prep}

Spatial indicators are configured to work with DATRAS survey data. Load in some survey data and process it first before parsing through spatial indicator functions.

**Load DATRAS exchange data**

Here we load NS-IBTS Quarter 1 survey data for years 2018 to 2022. 


```r
yrs <- c(2000:2022)
qrs <- c(1)
srv <- "NS-IBTS"

hh  <- icesDatras::getDATRAS(record = "HH", srv, years = yrs, quarters = qrs)
hl <-  icesDatras::getDATRAS(record = "HL", srv, years = yrs, quarters = qrs)
ca  <- icesDatras::getDATRAS(record = "CA", srv, years = yrs, quarters = qrs) # for completeness, but not used
```
Remove duplicate rows in exchange data.


```r
hh <- unique(hh)
hl <- unique(hl)
ca <- unique(ca)
```

**ICES Statistical Rectangles**

In the exchange data (in `hh` and `hl`) we know the ICES rectangle that each haul was conducted in, but we do not have the associated division. To be able to subset data to a specific stock region, we need to download [ICES Statistical Rectangle shapefile (Quick Downloads > ICES StatRec mapped to ICES Area)](https://gis.ices.dk/sf/index.html?widget=StatRec) and append information on ICES divisions to our exchange data. A download is available on GitHub and has been converted to a `.rds` file which is loaded in here.


```r
load(url("https://github.com/peterjosephkidd/SpatIndAssess/raw/main/Data/ICES%20Rect/ices_rect.rds"))
```
Add ICES Divisions


```r
area_div <- dplyr::distinct(ices_rect[c("ICESNAME", "Area_27", "Shape_Area")])
hh <- merge.data.frame(hh, area_div, by.x = "StatRec", by.y = "ICESNAME")
ca <- merge.data.frame(ca, area_div, by.x = "AreaCode", by.y = "ICESNAME")
```

**Basic Data Processing**

Remove invalid hauls:


```r
hh <- filter(hh, !HaulVal %in% c("I", "P")) # p = partly valid, it is deprecated 
```

`-9` is a placeholder for NAs. Change to `NA` for `TotalNo` column:


```r
hl$TotalNo[hl$TotalNo == -9] <- NA
```

Create haul.id:

```r
hh$haul.id <- as.character(
  paste(hh$Year, hh$Quarter, hh$Country, hh$Ship, hh$Gear, hh$StNo, hh$HaulNo, 
        sep = ":"))
hl$haul.id <- as.character(
  paste(hl$Year, hl$Quarter, hl$Country, hl$Ship, hl$Gear, hl$StNo, hl$HaulNo, 
        sep = ":"))
ca$haul.id <- as.character(
  paste(ca$Year, ca$Quarter, ca$Country, ca$Ship, ca$Gear, ca$StNo, ca$HaulNo, 
        sep = ":"))
```


Merge haul information from `hh` with landings information in `hl`. 


```r
## Refine columns
m <- hh[c("haul.id", "Year", "Quarter", "Month", "Survey","Country", "Ship", 
          "Gear", "GearEx", "DoorType", "HaulDur", "HaulNo", "StNo", "SweepLngt", 
          "StatRec", "Area_27", "ShootLong", "ShootLat", "HaulVal", "Depth")]
## Merge HL with HH	
hlhh <- merge(hl, dplyr::distinct(m),
              c("haul.id", "Year", "Quarter", "HaulNo", "StNo", "Gear", "GearEx", 
                "DoorType","Ship", "SweepLngt", "Country", "Survey"))
rm(m)
```

[Back to Top](#top)

___

## Stock ple.27.420 {#stk}

Now that DATRAS survey data has been loaded and formatted, choose a stock to investigate and filter data accordingly. This example looks at European plaice (*Plueronectes platessa*) in Subarea 4 (North Sea) and Subdivision 20 (Skagerrak; [ple.27.420)](https://ices-library.figshare.com/articles/report/Plaice_Pleuronectes_platessa_in_Subarea_4_North_Sea_and_Subdivision_20_Skagerrak_Replacing_advice_provided_in_2022/22548568).

**Set parameters**

```r
# ple.27.420
species <- c("plaice", "Pleuronectes platessa")
species_aphia <- findAphia(species[2], latin = T)
stk_divs <- c("4.a", "4.b", "4.c", "3.a.20")
```

**Filter data**


**Basic checks**

[Back to Top](#top)

___

# Empirical Spatial Indicator Analysis {#esia}




```r
# Source Spatial Indicator functions from GitHub
i <- 1
source("https://raw.githubusercontent.com/peterjosephkidd/SpatIndAssess/main/Functions/spatinds_funs.R", print.eval = F)
```

## Location {#loc}

### **Centre of Gravity (CGx, CGy):**

The mean longitudinal and latitudinal location of the population. Changes in CG indicate whether the mean location of the population is shifting eastward/westward (CGx) or northward/southward (CGy).


```r
cg <- coginis(hlhh, yrs, qrs, species_aphia, stk_divs, 
              iso = F, inertia = F, # toggle off outputting two another spatial indicators for now
              density = T) # weight hauls by  density of catch at each sample site (F therefore uses binary presence-absence data)

head(cg)
```

```
  Year Quarter CoG (x) CoG (y)
1 2000       1    6.35    55.8
2 2001       1    4.74    54.7
3 2002       1    4.25    54.7
4 2003       1    4.16    54.6
5 2004       1    4.95    55.1
6 2005       1    5.87    55.8
```

Visualise changes in `cg` over time 

```r
cg_p <-cogplot(cg, 
               # set grid = ices_rect for ICES rectangles -- takes time to load
               grid = ices_rect, areas = stk_divs,
               xlim = c(-2, 6), ylim = c(52, 60)) 
suppressWarnings(ggplotly(cg_p, tooltip = "text"))
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-81ab4356aa4f5cdc6120" style="width:768px;height:576px;"></div>
```

## Range {#range}

### **Inertia (I):** 

Inertia describes the dispersion/variance of the population around its centre of gravity. High values of inertia indicate that the population is widely spread across space.


```r
inert <- coginis(hlhh, yrs, qrs, species_aphia, stk_divs, 
              iso = F, inertia = T, # inertia toggled on
              density = T) #

head(inert)
```

```
  Year Quarter CoG (x) CoG (y) Inertia
1 2000       1    6.35    55.8    14.8
2 2001       1    4.74    54.7    12.3
3 2002       1    4.25    54.7    12.8
4 2003       1    4.16    54.6    13.5
5 2004       1    4.95    55.1    19.7
6 2005       1    5.87    55.8    19.6
```
Plot the trend over time

```r
in_p <- ggplot(data = inert, aes(x = Year, y = Inertia)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Inertia (I) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))

in_p
```

![](../tex/spatial-inert_plot-1.png)<!-- -->


### **Extent of Occurrence (EOO):** 

A convex hull polygon is drawn around occurrence points (i.e. sample sites with catch of the target species \> 0). EOO is the area of the polygon. High EOO indicates that the population is spread over a large geographical area.

```r
eoo <- chullarea(hlhh, yrs, qrs, species_aphia, stk_divs)[1:3] # this function needs tidying up, ignore other columns for now
head(eoo)
```

```
  Year Quarter convex_hull_area
1 2000       1             89.3
2 2001       1             85.9
3 2002       1             90.0
4 2003       1             87.0
5 2004       1             87.5
6 2005       1             88.3
```

Plot the trend over time

```r
eoo_p <- ggplot(data = eoo, aes(x = Year, y = convex_hull_area)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ylab("Extent of Occurence") +
  labs(title = "Extent of Occurence (EOO)",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))

eoo_p
```

![](../tex/spatial-eoo_plot-1.png)<!-- -->


### **Ellipse Area (ELA):** 

Similar to EOO. But instead of a convex hull, ELA calculates the area of an ellipse that encompasses 95% of the occurrence sites.

```r
print(stk_divs)
```

```
[1] "4.a"    "4.b"    "4.c"    "3.a.20"
```

```r
ela <- ellarea(hlhh, yrs, qrs, species_aphia, stk_divs = stk_divs)
head(ela)
```

```
  Year Quarter Ellipse Area
1 2000       1         86.2
2 2001       1         75.0
3 2002       1         86.6
4 2003       1         92.0
5 2004       1        102.0
6 2005       1        109.4
```


```r
ela_p <- ggplot(data = ela, aes(x = Year, y = `Ellipse Area`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Ellipse Area (ELA) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))

ela_p
```

![](../tex/spatial-ela_plot-1.png)<!-- -->

## Occupancy {#occ}

### **Proportion of Presence (POP):** 

Indicates the proportion of area occupied by the target species using binary presence-absence data.

#### **Rectangle (POPR):** 

The proportion of the ICES rectangles surveyed with a catch of the target species \> 0.

```r
popr <- pa_rect(hlhh, yrs, qrs, species_aphia, stk_divs)
head(popr)
```

```
# A tibble: 6 × 5
   Year nrects Quarter nrects_p PosAreaR
  <dbl>  <int> <chr>      <dbl>    <dbl>
1  2000    163 1            127    0.779
2  2001    165 1            128    0.776
3  2002    162 1            131    0.809
4  2003    165 1            123    0.745
5  2004    165 1            131    0.794
6  2005    166 1            131    0.789
```


```r
popr_p <- ggplot(data = popr, aes(x = Year, y = `PosAreaR`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Proportion of Presence in ICES Rectangles (POPR) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Proportion of Presence (Rectangle)")

popr_p
```

![](../tex/spatial-popr_plot-1.png)<!-- -->

#### **Haul (POPH):** 

The proportion of hauls with catch of the target species \> 0.


```r
poph <- pa_haul(hlhh, yrs, qrs, species_aphia, stk_divs)
head(poph)
```

```
# A tibble: 6 × 5
   Year no_haul.ids Quarter pr_hauls PosAreaH
  <dbl>       <int> <chr>      <dbl>    <dbl>
1  2000         367 1            256    0.698
2  2001         411 1            288    0.701
3  2002         401 1            297    0.741
4  2003         398 1            276    0.693
5  2004         355 1            246    0.693
6  2005         371 1            261    0.704
```


```r
poph_p <- ggplot(data = poph, aes(x = Year, y = `PosAreaH`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Proportion of Presence in Survey Hauls (POPR) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Proportion of Presence (Haul)")

poph_p
```

![](../tex/spatial-poph_plot-1.png)<!-- -->

## Map Indicators 
The `mapdis()` function visualises the above range and occupancy indicators on a single map for one timepoint (e.g. year and quarter). The function can equally be used to visualise a single indicator. 

```r
mapdis(hlhh, yrs = 2022, qrs, species_aphia, stk_divs, ices_rect, # data & parameters
       cog = T, inertia = T, EOO = T, ELA = T, # indicator toggles
       density = T, # weight samples 
       title = "Plaice (Pleuronected platessa)\nNS-IBTS", 
       xlim = c(-5,11), ylim = c(50, 62)) # plotting window
```

![](../tex/spatial-mapdis-1.png)<!-- -->


## Aggregation {#agg}

### **Gini Index:** 

Derived from a Lorenz curve. Ranges from 0 to 1, with 1 indicating that the population is uniformly distributed across surveyed rectangles, and 0 indicating that the population was recorded in one rectangle.


```r
lordat <- lorenz_data(hlhh, yrs, qrs, species_aphia, stk_divs)
lorenz_plot(lordat) + 
  theme_minimal() +
    theme(panel.border = element_rect(colour = "black", fill = NA)) # function needs tidying
```

![](../tex/spatial-lor_data-1.png)<!-- -->


```r
gni <- Gini(lordat)
head(gni)
```

```
# A tibble: 6 × 3
   Year Quarter `Gini Index`
  <dbl> <chr>          <dbl>
1  2000 1              0.219
2  2001 1              0.199
3  2002 1              0.231
4  2003 1              0.181
5  2004 1              0.266
6  2005 1              0.256
```


```r
gni_p <- ggplot(data = gni, aes(x = Year, y = `Gini Index`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Gini Index Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) 

gni_p
```

![](../tex/spatial-gini_plot-1.png)<!-- -->

### **D95:** 

Represents the proportion of the population present in 95% of the area. Ranges from 0 to 0.95, with 0 indicating that all individuals were recorded in 5% of the surveyed area (high aggregation) and 0.95 indicating that 95% of the population were recorded in 95% of the rectangles surveyed (uniform distribution).


```r
D <- d95(lordat)
```

```
D95 is an estimate of the proportion of the population that exists in 95% of surveyed rectangles.
```


```r
d95_p <- ggplot(data = D, aes(x = Year, y = `D95`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Proportion of Catch in 95% of ICES Rectangles in Stock Region (D95) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) 

d95_p
```

![](../tex/spatial-d95_plot-1.png)<!-- -->


### **Spreading Area (SA):**

Derived from cumulative frequency data. Measures the area occupied by the population, taking into account variations in fish density values. High values of SA indicate homogeneous spatial distribution.


```r
sa_data <- spreadingarea_data(hlhh, yrs, qrs, species_aphia, stk_divs)
sa <- sa_data %>%
        group_by(Year) %>%
        summarise("Spreading Area" = spreadingarea_calc(TotalNo_Dur)) %>%
        mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% # add quarters
        relocate(Year, Quarter) 
head(sa)
```

```
# A tibble: 6 × 3
   Year Quarter `Spreading Area`
  <int> <chr>              <dbl>
1  2000 1                   80.4
2  2001 1                   87.0
3  2002 1                   97.7
4  2003 1                   74.4
5  2004 1                   95.9
6  2005 1                   95.3
```


```r
sa_p <- ggplot(data = sa, aes(x = Year, y = `Spreading Area`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Spreading Area (SA) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) 

sa_p
```

![](../tex/spatial-sa_plot-1.png)<!-- -->

### **Equivalent Area (EA):** 

The area that would be covered by a population with homogeneously distributed density. It is equal to the mean density per individual.


```r
ea <- sa_data %>%
        group_by(Year) %>%
        summarise("Equivalent Area" = equivalentarea(TotalNo_Dur)) %>%
        mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% # add quarters
        relocate(Year, Quarter)
head(ea)
```

```
# A tibble: 6 × 3
   Year Quarter `Equivalent Area`
  <int> <chr>               <dbl>
1  2000 1                    45.9
2  2001 1                    58.5
3  2002 1                    73.8
4  2003 1                    41.5
5  2004 1                    72.5
6  2005 1                    78.9
```


```r
ea_p <- ggplot(data = ea, aes(x = Year, y = `Equivalent Area`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Equivalent Area (EA) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) 

ea_p
```

![](../tex/spatial-ea_plot-1.png)<!-- -->


### **Spread of Participation Index (SPI):** 

Compares the observed spatial density distribution to the density distribution expected from a homogeneously distributed population. Ranges from 0 to 1, with 0 indicating that the population was observed in only one ICES rectangle, and 1 indicating that the population density was uniformly distributed across ICES rectangles.


```r
SPI <- spi(hlhh, yrs, qrs, species_aphia, stk_divs)[c(1,2,4)] # function needs tidying

head(SPI)
```

```
# A tibble: 6 × 3
# Groups:   Year [6]
   Year Quarter SPI.dur
  <int> <chr>     <dbl>
1  2000 1         0.435
2  2001 1         0.450
3  2002 1         0.455
4  2003 1         0.426
5  2004 1         0.500
6  2005 1         0.483
```


```r
spi_p <- ggplot(data = SPI, aes(x = Year, y = `SPI.dur`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Spread of Participation Index (SPI) Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Spread of Participation Index (SPI)")

spi_p
```

![](../tex/spatial-spi_plot-1.png)<!-- -->

[Back to Top](#top)

## Plot Indicator Trends
Plot all the the timeseries of indicators to visualise and compare trends.

```r
# List of dfs
df_list <- list(cg, inert, eoo, ela, popr, poph, gni, D, sa, ea, SPI) 

# Categorise indicators
loc <- c("CoG (x)", "CoG (y)")
ran <- c("Inertia", "EOO", "ELA")
occ <- c("POPR", "POPH")
agg <- c("Gini Index", "D95", "SA", "EA", "SPI")

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
  tidyr::pivot_longer(cols = 3:14, names_to = "Indicator", values_to = "Value") %>%
   mutate(Type = case_when(
     Indicator %in% loc  ~ "Location",
     Indicator %in% ran  ~ "Range",
     Indicator %in% occ  ~ "Occupancy",
     Indicator %in% agg  ~ "Aggregation")) %>%
  mutate(Indicator = factor(Indicator, levels = c(loc, ran, occ, agg)))

head(sidf)
```

```
# A tibble: 6 × 5
   Year Quarter Indicator  Value Type     
  <dbl> <chr>   <fct>      <dbl> <chr>    
1  2000 1       CoG (x)    6.35  Location 
2  2000 1       CoG (y)   55.8   Location 
3  2000 1       Inertia   14.8   Range    
4  2000 1       EOO       89.3   Range    
5  2000 1       ELA       86.2   Range    
6  2000 1       POPR       0.779 Occupancy
```


```r
p <- ggplot(data = sidf, aes(x = Year, y = Value)) +
  geom_line(aes(colour = Type)) +
  scale_x_continuous(breaks = seq(from = min(unique(sidf$Year)), to = max(unique(sidf$Year)), by = 2)) +
  facet_wrap(vars(Indicator), scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)
  ) +
  labs(title = "Spatial Indicator Timeseries",
       subtitle = ttl(species, stk_divs, srv, qrs, yrs)) +
  ylab("Indicator Value")

ggplotly(p)
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-29fcb2176aaaf0f0d6a4" style="width:768px;height:576px;"></div>
<script type="application/json" data-for="htmlwidget-29fcb2176aaaf0f0d6a4">{"x":{"data":[{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[0.21895213639970923,0.19851967844554852,0.23096287161812401,0.18136430687340399,0.26567250842275414,0.25631501671998658,0.2272972688178303,0.23391316365619774,0.19913866816701842,0.18683671967391469,0.19266242389467469,0.20710902647467266,0.23373714060498685,0.22534356662551192,0.20780133176450155,0.21492624825008744,0.18107444528400252,0.2330203104998696,0.21320062554926622,0.22626119944776701,0.18334129323769111,0.18038174096224857,0.21492243574861747],"text":["Year: 2000<br />Value:   0.2190<br />Type: Aggregation","Year: 2001<br />Value:   0.1985<br />Type: Aggregation","Year: 2002<br />Value:   0.2310<br />Type: Aggregation","Year: 2003<br />Value:   0.1814<br />Type: Aggregation","Year: 2004<br />Value:   0.2657<br />Type: Aggregation","Year: 2005<br />Value:   0.2563<br />Type: Aggregation","Year: 2006<br />Value:   0.2273<br />Type: Aggregation","Year: 2007<br />Value:   0.2339<br />Type: Aggregation","Year: 2008<br />Value:   0.1991<br />Type: Aggregation","Year: 2009<br />Value:   0.1868<br />Type: Aggregation","Year: 2010<br />Value:   0.1927<br />Type: Aggregation","Year: 2011<br />Value:   0.2071<br />Type: Aggregation","Year: 2012<br />Value:   0.2337<br />Type: Aggregation","Year: 2013<br />Value:   0.2253<br />Type: Aggregation","Year: 2014<br />Value:   0.2078<br />Type: Aggregation","Year: 2015<br />Value:   0.2149<br />Type: Aggregation","Year: 2016<br />Value:   0.1811<br />Type: Aggregation","Year: 2017<br />Value:   0.2330<br />Type: Aggregation","Year: 2018<br />Value:   0.2132<br />Type: Aggregation","Year: 2019<br />Value:   0.2263<br />Type: Aggregation","Year: 2020<br />Value:   0.1833<br />Type: Aggregation","Year: 2021<br />Value:   0.1804<br />Type: Aggregation","Year: 2022<br />Value:   0.2149<br />Type: Aggregation"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Aggregation","legendgroup":"Aggregation","showlegend":true,"xaxis":"x8","yaxis":"y8","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[0.53567186336140116,0.48352429663387658,0.55614291919223313,0.44819260846165287,0.63935211286088534,0.61455667074892728,0.57640865936635688,0.60976364726343113,0.51579247144652962,0.46465175040008788,0.49439952008411181,0.53919204693326694,0.53717215478289049,0.54114268783132979,0.47641088024413614,0.50663473032926321,0.38428665394748207,0.55287919084524806,0.53726095768523785,0.60182516210266923,0.46156712419917351,0.42015074308216904,0.49251807416702287],"text":["Year: 2000<br />Value:   0.5357<br />Type: Aggregation","Year: 2001<br />Value:   0.4835<br />Type: Aggregation","Year: 2002<br />Value:   0.5561<br />Type: Aggregation","Year: 2003<br />Value:   0.4482<br />Type: Aggregation","Year: 2004<br />Value:   0.6394<br />Type: Aggregation","Year: 2005<br />Value:   0.6146<br />Type: Aggregation","Year: 2006<br />Value:   0.5764<br />Type: Aggregation","Year: 2007<br />Value:   0.6098<br />Type: Aggregation","Year: 2008<br />Value:   0.5158<br />Type: Aggregation","Year: 2009<br />Value:   0.4647<br />Type: Aggregation","Year: 2010<br />Value:   0.4944<br />Type: Aggregation","Year: 2011<br />Value:   0.5392<br />Type: Aggregation","Year: 2012<br />Value:   0.5372<br />Type: Aggregation","Year: 2013<br />Value:   0.5411<br />Type: Aggregation","Year: 2014<br />Value:   0.4764<br />Type: Aggregation","Year: 2015<br />Value:   0.5066<br />Type: Aggregation","Year: 2016<br />Value:   0.3843<br />Type: Aggregation","Year: 2017<br />Value:   0.5529<br />Type: Aggregation","Year: 2018<br />Value:   0.5373<br />Type: Aggregation","Year: 2019<br />Value:   0.6018<br />Type: Aggregation","Year: 2020<br />Value:   0.4616<br />Type: Aggregation","Year: 2021<br />Value:   0.4202<br />Type: Aggregation","Year: 2022<br />Value:   0.4925<br />Type: Aggregation"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Aggregation","legendgroup":"Aggregation","showlegend":false,"xaxis":"x9","yaxis":"y9","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[80.35543405869322,86.951619159150368,97.697294694466493,74.359365818095554,95.907775540614139,95.349186219834962,82.281611312054693,79.296562479450984,71.689920540126693,67.448055802283193,63.19327503745334,74.97346758383145,86.015267742635146,80.44765328530778,64.418412846995423,73.934629398030069,60.297790279572872,82.489189916953734,75.046620193341667,78.286375008927365,59.035896422536538,63.133609336787039,49.432160222181992],"text":["Year: 2000<br />Value:  80.3554<br />Type: Aggregation","Year: 2001<br />Value:  86.9516<br />Type: Aggregation","Year: 2002<br />Value:  97.6973<br />Type: Aggregation","Year: 2003<br />Value:  74.3594<br />Type: Aggregation","Year: 2004<br />Value:  95.9078<br />Type: Aggregation","Year: 2005<br />Value:  95.3492<br />Type: Aggregation","Year: 2006<br />Value:  82.2816<br />Type: Aggregation","Year: 2007<br />Value:  79.2966<br />Type: Aggregation","Year: 2008<br />Value:  71.6899<br />Type: Aggregation","Year: 2009<br />Value:  67.4481<br />Type: Aggregation","Year: 2010<br />Value:  63.1933<br />Type: Aggregation","Year: 2011<br />Value:  74.9735<br />Type: Aggregation","Year: 2012<br />Value:  86.0153<br />Type: Aggregation","Year: 2013<br />Value:  80.4477<br />Type: Aggregation","Year: 2014<br />Value:  64.4184<br />Type: Aggregation","Year: 2015<br />Value:  73.9346<br />Type: Aggregation","Year: 2016<br />Value:  60.2978<br />Type: Aggregation","Year: 2017<br />Value:  82.4892<br />Type: Aggregation","Year: 2018<br />Value:  75.0466<br />Type: Aggregation","Year: 2019<br />Value:  78.2864<br />Type: Aggregation","Year: 2020<br />Value:  59.0359<br />Type: Aggregation","Year: 2021<br />Value:  63.1336<br />Type: Aggregation","Year: 2022<br />Value:  49.4322<br />Type: Aggregation"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Aggregation","legendgroup":"Aggregation","showlegend":false,"xaxis":"x10","yaxis":"y10","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[45.910370325321203,58.513610282348992,73.843861131995638,41.465910796967357,72.476609907924299,78.853722737143258,64.016077327737719,71.45853181411087,52.052683386251125,34.990585463345688,42.436728061077254,56.503231923263449,54.666107514820723,56.572912124799302,27.670841415810099,33.589228206626316,14.378862547158265,54.930248555560269,52.987758059540397,67.459347648109002,23.975871265786601,12.443993948052919,24.71658840882867],"text":["Year: 2000<br />Value:  45.9104<br />Type: Aggregation","Year: 2001<br />Value:  58.5136<br />Type: Aggregation","Year: 2002<br />Value:  73.8439<br />Type: Aggregation","Year: 2003<br />Value:  41.4659<br />Type: Aggregation","Year: 2004<br />Value:  72.4766<br />Type: Aggregation","Year: 2005<br />Value:  78.8537<br />Type: Aggregation","Year: 2006<br />Value:  64.0161<br />Type: Aggregation","Year: 2007<br />Value:  71.4585<br />Type: Aggregation","Year: 2008<br />Value:  52.0527<br />Type: Aggregation","Year: 2009<br />Value:  34.9906<br />Type: Aggregation","Year: 2010<br />Value:  42.4367<br />Type: Aggregation","Year: 2011<br />Value:  56.5032<br />Type: Aggregation","Year: 2012<br />Value:  54.6661<br />Type: Aggregation","Year: 2013<br />Value:  56.5729<br />Type: Aggregation","Year: 2014<br />Value:  27.6708<br />Type: Aggregation","Year: 2015<br />Value:  33.5892<br />Type: Aggregation","Year: 2016<br />Value:  14.3789<br />Type: Aggregation","Year: 2017<br />Value:  54.9302<br />Type: Aggregation","Year: 2018<br />Value:  52.9878<br />Type: Aggregation","Year: 2019<br />Value:  67.4593<br />Type: Aggregation","Year: 2020<br />Value:  23.9759<br />Type: Aggregation","Year: 2021<br />Value:  12.4440<br />Type: Aggregation","Year: 2022<br />Value:  24.7166<br />Type: Aggregation"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Aggregation","legendgroup":"Aggregation","showlegend":false,"xaxis":"x11","yaxis":"y11","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[0.43461450521789502,0.45006449715577845,0.45474911600020773,0.42566522796021966,0.49998714160810709,0.48332603433896615,0.43461852120683353,0.43715661935014882,0.41605401957077137,0.42330714806011771,0.38131477228813138,0.40654136890756631,0.4269471068910804,0.42923222794078408,0.39622533919628111,0.43000634134988891,0.37128520687214972,0.4435309289085243,0.39660609006829584,0.40637904300666106,0.37885252809441883,0.3533616803695635,0.42938658499822058],"text":["Year: 2000<br />Value:   0.4346<br />Type: Aggregation","Year: 2001<br />Value:   0.4501<br />Type: Aggregation","Year: 2002<br />Value:   0.4547<br />Type: Aggregation","Year: 2003<br />Value:   0.4257<br />Type: Aggregation","Year: 2004<br />Value:   0.5000<br />Type: Aggregation","Year: 2005<br />Value:   0.4833<br />Type: Aggregation","Year: 2006<br />Value:   0.4346<br />Type: Aggregation","Year: 2007<br />Value:   0.4372<br />Type: Aggregation","Year: 2008<br />Value:   0.4161<br />Type: Aggregation","Year: 2009<br />Value:   0.4233<br />Type: Aggregation","Year: 2010<br />Value:   0.3813<br />Type: Aggregation","Year: 2011<br />Value:   0.4065<br />Type: Aggregation","Year: 2012<br />Value:   0.4269<br />Type: Aggregation","Year: 2013<br />Value:   0.4292<br />Type: Aggregation","Year: 2014<br />Value:   0.3962<br />Type: Aggregation","Year: 2015<br />Value:   0.4300<br />Type: Aggregation","Year: 2016<br />Value:   0.3713<br />Type: Aggregation","Year: 2017<br />Value:   0.4435<br />Type: Aggregation","Year: 2018<br />Value:   0.3966<br />Type: Aggregation","Year: 2019<br />Value:   0.4064<br />Type: Aggregation","Year: 2020<br />Value:   0.3789<br />Type: Aggregation","Year: 2021<br />Value:   0.3534<br />Type: Aggregation","Year: 2022<br />Value:   0.4294<br />Type: Aggregation"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Aggregation","legendgroup":"Aggregation","showlegend":false,"xaxis":"x12","yaxis":"y12","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[6.3529885799220338,4.7359556478517169,4.2541592670873536,4.1586215954651351,4.9506770522622245,5.8708329617521189,5.215875747018667,5.9386871289947161,4.8240060910639739,4.8516772072486027,3.2405657248049535,2.766915515615294,2.4120472544503038,3.2191368890285204,3.6041269935562545,3.0237719931154308,1.7216616073432451,2.4739104501334102,1.3409117233816097,2.3762884629910177,2.7736926395169887,0.011060544789299876,3.1328962691465247],"text":["Year: 2000<br />Value:   6.3530<br />Type: Location","Year: 2001<br />Value:   4.7360<br />Type: Location","Year: 2002<br />Value:   4.2542<br />Type: Location","Year: 2003<br />Value:   4.1586<br />Type: Location","Year: 2004<br />Value:   4.9507<br />Type: Location","Year: 2005<br />Value:   5.8708<br />Type: Location","Year: 2006<br />Value:   5.2159<br />Type: Location","Year: 2007<br />Value:   5.9387<br />Type: Location","Year: 2008<br />Value:   4.8240<br />Type: Location","Year: 2009<br />Value:   4.8517<br />Type: Location","Year: 2010<br />Value:   3.2406<br />Type: Location","Year: 2011<br />Value:   2.7669<br />Type: Location","Year: 2012<br />Value:   2.4120<br />Type: Location","Year: 2013<br />Value:   3.2191<br />Type: Location","Year: 2014<br />Value:   3.6041<br />Type: Location","Year: 2015<br />Value:   3.0238<br />Type: Location","Year: 2016<br />Value:   1.7217<br />Type: Location","Year: 2017<br />Value:   2.4739<br />Type: Location","Year: 2018<br />Value:   1.3409<br />Type: Location","Year: 2019<br />Value:   2.3763<br />Type: Location","Year: 2020<br />Value:   2.7737<br />Type: Location","Year: 2021<br />Value:   0.0111<br />Type: Location","Year: 2022<br />Value:   3.1329<br />Type: Location"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(124,174,0,1)","dash":"solid"},"hoveron":"points","name":"Location","legendgroup":"Location","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[55.755391236222337,54.743617738322811,54.716292458059087,54.633488318831027,55.075691171135659,55.781925357040976,55.825591205029845,55.799951575012557,55.210330762741641,55.06860743997472,55.577355130022106,55.603177745571955,56.010508959173826,55.981322251904288,54.446678816779603,55.638336654117644,55.641708013321463,56.216209894676069,56.558415198838965,55.471394555188503,56.513549295405269,57.175159499763829,54.487858265221973],"text":["Year: 2000<br />Value:  55.7554<br />Type: Location","Year: 2001<br />Value:  54.7436<br />Type: Location","Year: 2002<br />Value:  54.7163<br />Type: Location","Year: 2003<br />Value:  54.6335<br />Type: Location","Year: 2004<br />Value:  55.0757<br />Type: Location","Year: 2005<br />Value:  55.7819<br />Type: Location","Year: 2006<br />Value:  55.8256<br />Type: Location","Year: 2007<br />Value:  55.8000<br />Type: Location","Year: 2008<br />Value:  55.2103<br />Type: Location","Year: 2009<br />Value:  55.0686<br />Type: Location","Year: 2010<br />Value:  55.5774<br />Type: Location","Year: 2011<br />Value:  55.6032<br />Type: Location","Year: 2012<br />Value:  56.0105<br />Type: Location","Year: 2013<br />Value:  55.9813<br />Type: Location","Year: 2014<br />Value:  54.4467<br />Type: Location","Year: 2015<br />Value:  55.6383<br />Type: Location","Year: 2016<br />Value:  55.6417<br />Type: Location","Year: 2017<br />Value:  56.2162<br />Type: Location","Year: 2018<br />Value:  56.5584<br />Type: Location","Year: 2019<br />Value:  55.4714<br />Type: Location","Year: 2020<br />Value:  56.5135<br />Type: Location","Year: 2021<br />Value:  57.1752<br />Type: Location","Year: 2022<br />Value:  54.4879<br />Type: Location"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(124,174,0,1)","dash":"solid"},"hoveron":"points","name":"Location","legendgroup":"Location","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[0.77914110429447858,0.77575757575757576,0.80864197530864201,0.74545454545454548,0.79393939393939394,0.78915662650602414,0.79878048780487809,0.77245508982035926,0.78787878787878785,0.86144578313253017,0.76331360946745563,0.85798816568047342,0.8392857142857143,0.83529411764705885,0.84615384615384615,0.82840236686390534,0.8497109826589595,0.78857142857142859,0.75287356321839083,0.76162790697674421,0.74712643678160917,0.74431818181818177,0.79591836734693877],"text":["Year: 2000<br />Value:   0.7791<br />Type: Occupancy","Year: 2001<br />Value:   0.7758<br />Type: Occupancy","Year: 2002<br />Value:   0.8086<br />Type: Occupancy","Year: 2003<br />Value:   0.7455<br />Type: Occupancy","Year: 2004<br />Value:   0.7939<br />Type: Occupancy","Year: 2005<br />Value:   0.7892<br />Type: Occupancy","Year: 2006<br />Value:   0.7988<br />Type: Occupancy","Year: 2007<br />Value:   0.7725<br />Type: Occupancy","Year: 2008<br />Value:   0.7879<br />Type: Occupancy","Year: 2009<br />Value:   0.8614<br />Type: Occupancy","Year: 2010<br />Value:   0.7633<br />Type: Occupancy","Year: 2011<br />Value:   0.8580<br />Type: Occupancy","Year: 2012<br />Value:   0.8393<br />Type: Occupancy","Year: 2013<br />Value:   0.8353<br />Type: Occupancy","Year: 2014<br />Value:   0.8462<br />Type: Occupancy","Year: 2015<br />Value:   0.8284<br />Type: Occupancy","Year: 2016<br />Value:   0.8497<br />Type: Occupancy","Year: 2017<br />Value:   0.7886<br />Type: Occupancy","Year: 2018<br />Value:   0.7529<br />Type: Occupancy","Year: 2019<br />Value:   0.7616<br />Type: Occupancy","Year: 2020<br />Value:   0.7471<br />Type: Occupancy","Year: 2021<br />Value:   0.7443<br />Type: Occupancy","Year: 2022<br />Value:   0.7959<br />Type: Occupancy"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,191,196,1)","dash":"solid"},"hoveron":"points","name":"Occupancy","legendgroup":"Occupancy","showlegend":true,"xaxis":"x6","yaxis":"y6","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[0.6975476839237057,0.7007299270072993,0.74064837905236913,0.69346733668341709,0.6929577464788732,0.70350404312668469,0.71546961325966851,0.72271386430678464,0.72752808988764039,0.74238227146814406,0.71296296296296291,0.76795580110497241,0.83768115942028987,0.75071633237822355,0.83552631578947367,0.7807017543859649,0.81381381381381379,0.7558139534883721,0.7053571428571429,0.72615384615384615,0.69902912621359226,0.6945244956772334,0.79716981132075471],"text":["Year: 2000<br />Value:   0.6975<br />Type: Occupancy","Year: 2001<br />Value:   0.7007<br />Type: Occupancy","Year: 2002<br />Value:   0.7406<br />Type: Occupancy","Year: 2003<br />Value:   0.6935<br />Type: Occupancy","Year: 2004<br />Value:   0.6930<br />Type: Occupancy","Year: 2005<br />Value:   0.7035<br />Type: Occupancy","Year: 2006<br />Value:   0.7155<br />Type: Occupancy","Year: 2007<br />Value:   0.7227<br />Type: Occupancy","Year: 2008<br />Value:   0.7275<br />Type: Occupancy","Year: 2009<br />Value:   0.7424<br />Type: Occupancy","Year: 2010<br />Value:   0.7130<br />Type: Occupancy","Year: 2011<br />Value:   0.7680<br />Type: Occupancy","Year: 2012<br />Value:   0.8377<br />Type: Occupancy","Year: 2013<br />Value:   0.7507<br />Type: Occupancy","Year: 2014<br />Value:   0.8355<br />Type: Occupancy","Year: 2015<br />Value:   0.7807<br />Type: Occupancy","Year: 2016<br />Value:   0.8138<br />Type: Occupancy","Year: 2017<br />Value:   0.7558<br />Type: Occupancy","Year: 2018<br />Value:   0.7054<br />Type: Occupancy","Year: 2019<br />Value:   0.7262<br />Type: Occupancy","Year: 2020<br />Value:   0.6990<br />Type: Occupancy","Year: 2021<br />Value:   0.6945<br />Type: Occupancy","Year: 2022<br />Value:   0.7972<br />Type: Occupancy"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,191,196,1)","dash":"solid"},"hoveron":"points","name":"Occupancy","legendgroup":"Occupancy","showlegend":false,"xaxis":"x7","yaxis":"y7","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[14.786901536583528,12.279674942724711,12.811680218454061,13.486880428181717,19.670001547228313,19.649322225986353,18.751702320004728,15.492909234234205,11.483842162041908,10.116454920863163,20.237391971415512,16.209409722991431,19.411825805717424,20.334335376015424,11.726182286121553,21.444517196667555,20.459752252265702,22.516867636670998,23.346358204788757,22.279384698505559,27.721755849624031,21.145888726370448,13.554452089732546],"text":["Year: 2000<br />Value:  14.7869<br />Type: Range","Year: 2001<br />Value:  12.2797<br />Type: Range","Year: 2002<br />Value:  12.8117<br />Type: Range","Year: 2003<br />Value:  13.4869<br />Type: Range","Year: 2004<br />Value:  19.6700<br />Type: Range","Year: 2005<br />Value:  19.6493<br />Type: Range","Year: 2006<br />Value:  18.7517<br />Type: Range","Year: 2007<br />Value:  15.4929<br />Type: Range","Year: 2008<br />Value:  11.4838<br />Type: Range","Year: 2009<br />Value:  10.1165<br />Type: Range","Year: 2010<br />Value:  20.2374<br />Type: Range","Year: 2011<br />Value:  16.2094<br />Type: Range","Year: 2012<br />Value:  19.4118<br />Type: Range","Year: 2013<br />Value:  20.3343<br />Type: Range","Year: 2014<br />Value:  11.7262<br />Type: Range","Year: 2015<br />Value:  21.4445<br />Type: Range","Year: 2016<br />Value:  20.4598<br />Type: Range","Year: 2017<br />Value:  22.5169<br />Type: Range","Year: 2018<br />Value:  23.3464<br />Type: Range","Year: 2019<br />Value:  22.2794<br />Type: Range","Year: 2020<br />Value:  27.7218<br />Type: Range","Year: 2021<br />Value:  21.1459<br />Type: Range","Year: 2022<br />Value:  13.5545<br />Type: Range"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(199,124,255,1)","dash":"solid"},"hoveron":"points","name":"Range","legendgroup":"Range","showlegend":true,"xaxis":"x3","yaxis":"y3","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[89.277172860000007,85.861864444999981,90.044053010000013,86.97906313999998,87.525025995000036,88.324939114999964,86.554647589999973,90.489293884999995,90.559741580000008,93.157768585000014,86.44498646000001,87.117848860000024,89.036154755000013,90.855657155000003,93.296458255000005,91.045429945000038,86.618566490000006,88.133510955000048,86.060985599999995,86.165069360000018,86.994525715000037,87.144294834999997,81.035023945000034],"text":["Year: 2000<br />Value:  89.2772<br />Type: Range","Year: 2001<br />Value:  85.8619<br />Type: Range","Year: 2002<br />Value:  90.0441<br />Type: Range","Year: 2003<br />Value:  86.9791<br />Type: Range","Year: 2004<br />Value:  87.5250<br />Type: Range","Year: 2005<br />Value:  88.3249<br />Type: Range","Year: 2006<br />Value:  86.5546<br />Type: Range","Year: 2007<br />Value:  90.4893<br />Type: Range","Year: 2008<br />Value:  90.5597<br />Type: Range","Year: 2009<br />Value:  93.1578<br />Type: Range","Year: 2010<br />Value:  86.4450<br />Type: Range","Year: 2011<br />Value:  87.1178<br />Type: Range","Year: 2012<br />Value:  89.0362<br />Type: Range","Year: 2013<br />Value:  90.8557<br />Type: Range","Year: 2014<br />Value:  93.2965<br />Type: Range","Year: 2015<br />Value:  91.0454<br />Type: Range","Year: 2016<br />Value:  86.6186<br />Type: Range","Year: 2017<br />Value:  88.1335<br />Type: Range","Year: 2018<br />Value:  86.0610<br />Type: Range","Year: 2019<br />Value:  86.1651<br />Type: Range","Year: 2020<br />Value:  86.9945<br />Type: Range","Year: 2021<br />Value:  87.1443<br />Type: Range","Year: 2022<br />Value:  81.0350<br />Type: Range"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(199,124,255,1)","dash":"solid"},"hoveron":"points","name":"Range","legendgroup":"Range","showlegend":false,"xaxis":"x4","yaxis":"y4","hoverinfo":"text","frame":null},{"x":[2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[86.234650573794283,75.009061794527256,86.618698435306371,91.986248690263892,101.99093890940873,109.39088432309727,102.13202189917172,89.927527427807547,92.377232109554299,94.526440104328103,121.95944501548333,107.66511031557539,110.18648673325463,109.3115155156639,113.39954130742144,119.02343078888994,118.80841505753159,112.31434828046088,122.32060849859137,118.90278197413632,120.5643649182913,123.97634762615925,92.204845841278512],"text":["Year: 2000<br />Value:  86.2347<br />Type: Range","Year: 2001<br />Value:  75.0091<br />Type: Range","Year: 2002<br />Value:  86.6187<br />Type: Range","Year: 2003<br />Value:  91.9862<br />Type: Range","Year: 2004<br />Value: 101.9909<br />Type: Range","Year: 2005<br />Value: 109.3909<br />Type: Range","Year: 2006<br />Value: 102.1320<br />Type: Range","Year: 2007<br />Value:  89.9275<br />Type: Range","Year: 2008<br />Value:  92.3772<br />Type: Range","Year: 2009<br />Value:  94.5264<br />Type: Range","Year: 2010<br />Value: 121.9594<br />Type: Range","Year: 2011<br />Value: 107.6651<br />Type: Range","Year: 2012<br />Value: 110.1865<br />Type: Range","Year: 2013<br />Value: 109.3115<br />Type: Range","Year: 2014<br />Value: 113.3995<br />Type: Range","Year: 2015<br />Value: 119.0234<br />Type: Range","Year: 2016<br />Value: 118.8084<br />Type: Range","Year: 2017<br />Value: 112.3143<br />Type: Range","Year: 2018<br />Value: 122.3206<br />Type: Range","Year: 2019<br />Value: 118.9028<br />Type: Range","Year: 2020<br />Value: 120.5644<br />Type: Range","Year: 2021<br />Value: 123.9763<br />Type: Range","Year: 2022<br />Value:  92.2048<br />Type: Range"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(199,124,255,1)","dash":"solid"},"hoveron":"points","name":"Range","legendgroup":"Range","showlegend":false,"xaxis":"x5","yaxis":"y5","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":54.964992389649922,"r":7.3059360730593621,"b":43.946312439463128,"l":31.415525114155258},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Spatial Indicator Timeseries","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,0.21432648401826482],"automargin":true,"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":"","hoverformat":".2f"},"annotations":[{"text":"Year","x":0.5,"y":0,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis","yshift":-26.168534661685349},{"text":"Indicator Value","x":0,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis","xshift":-16.073059360730596},{"text":"CoG (x)","x":0.10716324200913241,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"CoG (y)","x":0.375,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Inertia","x":0.625,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"EOO","x":0.89283675799086759,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"ELA","x":0.10716324200913241,"y":0.62117752871177534,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"POPR","x":0.375,"y":0.62117752871177534,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"POPH","x":0.625,"y":0.62117752871177534,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Gini Index","x":0.89283675799086759,"y":0.62117752871177534,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"D95","x":0.10716324200913241,"y":0.28784419537844197,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"SA","x":0.375,"y":0.28784419537844197,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"EA","x":0.625,"y":0.28784419537844197,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"SPI","x":0.89283675799086759,"y":0.28784419537844197,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0.71215580462155814,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.30603585696733687,6.6700849816786709],"tickmode":"array","ticktext":["0","2","4","6"],"tickvals":[0,1.9999999999999998,4,6],"categoryorder":"array","categoryarray":["0","2","4","6"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.21432648401826482,"y0":0.71215580462155814,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.21432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.28567351598173518,"x1":0.46432648401826482,"y0":0.71215580462155814,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.28567351598173518,"x1":0.46432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.53567351598173518,"x1":0.71432648401826482,"y0":0.71215580462155814,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.53567351598173518,"x1":0.71432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.78567351598173518,"x1":1,"y0":0.71215580462155814,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.78567351598173518,"x1":1,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.21432648401826482,"y0":0.37882247128822477,"y1":0.62117752871177534},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.21432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":0.62117752871177534,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.28567351598173518,"x1":0.46432648401826482,"y0":0.37882247128822477,"y1":0.62117752871177534},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.28567351598173518,"x1":0.46432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":0.62117752871177534,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.53567351598173518,"x1":0.71432648401826482,"y0":0.37882247128822477,"y1":0.62117752871177534},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.53567351598173518,"x1":0.71432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":0.62117752871177534,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.78567351598173518,"x1":1,"y0":0.37882247128822477,"y1":0.62117752871177534},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.78567351598173518,"x1":1,"y0":0,"y1":23.37899543378996,"yanchor":0.62117752871177534,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.21432648401826482,"y0":0,"y1":0.28784419537844197},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.21432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":0.28784419537844197,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.28567351598173518,"x1":0.46432648401826482,"y0":0,"y1":0.28784419537844197},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.28567351598173518,"x1":0.46432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":0.28784419537844197,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.53567351598173518,"x1":0.71432648401826482,"y0":0,"y1":0.28784419537844197},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.53567351598173518,"x1":0.71432648401826482,"y0":0,"y1":23.37899543378996,"yanchor":0.28784419537844197,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(0,0,0,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.78567351598173518,"x1":1,"y0":0,"y1":0.28784419537844197},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.78567351598173518,"x1":1,"y0":0,"y1":23.37899543378996,"yanchor":0.28784419537844197,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.28567351598173518,0.46432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"yaxis2":{"type":"linear","autorange":false,"range":[54.310254782630395,57.311583533913037],"tickmode":"array","ticktext":["55","56","57"],"tickvals":[55,56,57],"categoryorder":"array","categoryarray":["55","56","57"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.71215580462155814,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x2","title":"","hoverformat":".2f"},"xaxis3":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.53567351598173518,0.71432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y3","title":"","hoverformat":".2f"},"yaxis3":{"type":"linear","autorange":false,"range":[9.2361898744251185,28.602020896062076],"tickmode":"array","ticktext":["10","15","20","25"],"tickvals":[10,15,20,25],"categoryorder":"array","categoryarray":["10","15","20","25"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.71215580462155814,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x3","title":"","hoverformat":".2f"},"xaxis4":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.78567351598173518,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y4","title":"","hoverformat":".2f"},"yaxis4":{"type":"linear","autorange":false,"range":[80.421952229500036,93.909529970500003],"tickmode":"array","ticktext":["85","90"],"tickvals":[85,90],"categoryorder":"array","categoryarray":["85","90"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.71215580462155814,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x4","title":"","hoverformat":".2f"},"xaxis5":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.21432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y5","title":"","hoverformat":".2f"},"yaxis5":{"type":"linear","autorange":false,"range":[72.560697502945658,126.42471191774085],"tickmode":"array","ticktext":["80","90","100","110","120"],"tickvals":[80,90,100,110,120],"categoryorder":"array","categoryarray":["80","90","100","110","120"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.37882247128822477,0.62117752871177534],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x5","title":"","hoverformat":".2f"},"xaxis6":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.28567351598173518,0.46432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y6","title":"","hoverformat":".2f"},"yaxis6":{"type":"linear","autorange":false,"range":[0.73846180175246434,0.8673021631982476],"tickmode":"array","ticktext":["0.75","0.78","0.81","0.84"],"tickvals":[0.75,0.78000000000000003,0.81000000000000005,0.83999999999999997],"categoryorder":"array","categoryarray":["0.75","0.78","0.81","0.84"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.37882247128822477,0.62117752871177534],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x6","title":"","hoverformat":".2f"},"xaxis7":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.53567351598173518,0.71432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y7","title":"","hoverformat":".2f"},"yaxis7":{"type":"linear","autorange":false,"range":[0.68572157583180238,0.84491733006736069],"tickmode":"array","ticktext":["0.72","0.76","0.80","0.84"],"tickvals":[0.72000000000000008,0.76000000000000001,0.80000000000000004,0.84000000000000008],"categoryorder":"array","categoryarray":["0.72","0.76","0.80","0.84"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.37882247128822477,0.62117752871177534],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x7","title":"","hoverformat":".2f"},"xaxis8":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.78567351598173518,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y8","title":"","hoverformat":".2f"},"yaxis8":{"type":"linear","autorange":false,"range":[0.17611720258922328,0.2699370467957794],"tickmode":"array","ticktext":["0.200","0.225","0.250"],"tickvals":[0.20000000000000001,0.22500000000000003,0.25],"categoryorder":"array","categoryarray":["0.200","0.225","0.250"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.37882247128822477,0.62117752871177534],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x8","title":"","hoverformat":".2f"},"xaxis9":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.21432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y9","title":"","hoverformat":".2f"},"yaxis9":{"type":"linear","autorange":false,"range":[0.37153338100181188,0.65210538580655553],"tickmode":"array","ticktext":["0.40","0.45","0.50","0.55","0.60","0.65"],"tickvals":[0.40000000000000002,0.45000000000000001,0.5,0.55000000000000004,0.60000000000000009,0.65000000000000002],"categoryorder":"array","categoryarray":["0.40","0.45","0.50","0.55","0.60","0.65"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.28784419537844197],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x9","title":"","hoverformat":".2f"},"xaxis10":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.28567351598173518,0.46432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y10","title":"","hoverformat":".2f"},"yaxis10":{"type":"linear","autorange":false,"range":[47.018903498567767,100.11055141808072],"tickmode":"array","ticktext":["50","60","70","80","90","100"],"tickvals":[50,60,70,80,90,100],"categoryorder":"array","categoryarray":["50","60","70","80","90","100"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.28784419537844197],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x10","title":"","hoverformat":".2f"},"xaxis11":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.53567351598173518,0.71432648401826482],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y11","title":"","hoverformat":".2f"},"yaxis11":{"type":"linear","autorange":false,"range":[9.1235075085984008,82.17420917659777],"tickmode":"array","ticktext":["20","40","60","80"],"tickvals":[20,40,60,80],"categoryorder":"array","categoryarray":["20","40","60","80"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.28784419537844197],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x11","title":"","hoverformat":".2f"},"xaxis12":{"type":"linear","autorange":false,"range":[1998.9000000000001,2023.0999999999999],"tickmode":"array","ticktext":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"tickvals":[2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022],"categoryorder":"array","categoryarray":["2000","2002","2004","2006","2008","2010","2012","2014","2016","2018","2020","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":7.9701120797011216},"tickangle":-90,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.78567351598173518,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y12","title":"","hoverformat":".2f"},"yaxis12":{"type":"linear","autorange":false,"range":[0.34603040730763635,0.50731841467003425],"tickmode":"array","ticktext":["0.35","0.40","0.45","0.50"],"tickvals":[0.35000000000000003,0.40000000000000002,0.45000000000000007,0.5],"categoryorder":"array","categoryarray":["0.35","0.40","0.45","0.50"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.28784419537844197],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x12","title":"","hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":{"text":"Type","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"4bb07bb28b1":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"4bb07bb28b1","visdat":{"4bb07bb28b1":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

# More Information {#more}

-   For more information on the FLR Project for Quantitative Fisheries
    Science in R, visit the FLR webpage [^1].

[^1]: <http://flr-project.org>

## Author information

## Acknowledgements

## Software Versions

**R version 4.2.2 (2022-10-31 ucrt)**

-   FLCore: 2.6.19

-   ggplotFL: 2.7.0

-   ggplot2: 3.4.4

**Compiled**: Tue Jan 23 15:53:49 2024

[Back to Top](#top)

# References {#references}

[Back to Top](#top)