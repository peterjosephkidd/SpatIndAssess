#> R Sim Process
#> 1. Set-up ####
#install.packages("purrr")
#install.packages("magick")

library(purrr)
library(magick)
library(dplyr)
library(ggplot2)
path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/pres"
source(paste0(getwd(), "/Functions/ROC_funs.R"))      # for ROC and TSS

# 2. Data ----------------------------------------------------------------------
## 2.1. Real Data (Spatial Indicators) -------------------------------------------
### 2.1.1. Get Predictions ----------------
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/1. Outputs/Data/SpatInd/AllSurveys/ple.27.420/BTS+IBTS Q3/NS-IBTS/SpatIndData - ple.27.420 - BTS+IBTS Q3 - NS-IBTS.rda")
head(si)

### 2.1.2. Load stock objects -------------
stockobj.path <- paste0(getwd(), "/Data/DR_Stocks/Stock Objects/2022/")
for(stockfile in list.files(stockobj.path)){load(paste0(stockobj.path, stockfile))}
stk <- ple.27.420
stk.chr <- "ple.27.420"
stocklist <- list(cod.27.47d20_nov, had.27.46a20, ple.27.420, ple.27.7d, 
                  pok.27.3a46, sol.27.4, tur.27.4, whg.27.47d, wit.27.3a47d) # ignore so.27.7d for now, need to find YFS survey data
stocklist.chr <- list("cod.27.47d20_nov", "had.27.46a20", "ple.27.420", "ple.27.7d", 
                      "pok.27.3a46", "sol.27.4", "tur.27.4", "whg.27.47d", "wit.27.3a47d")
yrs <- c(range(stk)["minyear"][[1]]:range(stk)["maxyear"][[1]])
si2 <- filter(si, Year %in% yrs)

### 2.1.3. Get observations --------------
strtyr <- min(si2$Year)
endyr <- max(si2$Year)
allstk_refpts <- readxl::read_excel(paste0(getwd(), "/Data/DR_Stocks/Advice Sheets/2022/stock_metadata_refpts.xlsx"), sheet = "stk_refpts")
msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
stkssb <- as.data.frame(ssb(stk)[,ac(strtyr:endyr)])[c("year", "data")] %>%
  rename(Year = year, SSB = data) %>%
  mutate(type = "SSB")
stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt
sissb <- merge(si2, stkssb, by = "Year")

### 2.1.4. Put data into format for plot below ---------
obs_data <- data.frame(cbind(Observed = sissb$ssb.msybtrig, Timestep = sissb$Year))
obs_data$colr <- if_else(obs_data$Observed < 1, "red", "limegreen")

pred_data <- data.frame(cbind(`Spatial Indicator` = sissb$`Positive Area (Rectangle)`, Timestep = sissb$Year))
pred_data <- rename(pred_data, `Spatial Indicator` = Spatial.Indicator)
pred_data_long <- tidyr::pivot_longer(pred_data, cols = `Spatial Indicator`)

data <- merge(obs_data, pred_data, by = "Timestep")
data_long <- tidyr::pivot_longer(data, cols = `Spatial Indicator`)




## 2.2 Simulated Data --------------------------------------------------------------------
### 2.2.1 Observed Data ####
Observed <- c(runif(8, min = 1.5, max = 1.8),
              runif(4, min = 0.8, max = 1.2), 
              runif(8, min = 0.2, max = 0.5), 
              runif(4, min = 0.5, max = 0.7),
              runif(4, min = 1.1, max = 1.4),
              runif(4, min = 0.7, max = 0.9),
              runif(8, min = 0.9, max = 1.3))
hist(Observed)

obs_data <- as.data.frame(cbind(Observed))
obs_data$Timestep <- 1:40
obs_data$colr <- if_else(obs_data$Observed < 1, "red", "limegreen")


### 2.2.2. Predicted Data ####
`Okay Indicator` <- Observed <- c(runif(12, min = 6.5, max = 6.7),
                                  runif(4, min = 6, max = 6.2), 
                                  runif(4, min = 5.2, max = 5.5), 
                                  runif(8, min = 5.5, max = 6),
                                  runif(4, min = 6.1, max = 6.4),
                                  runif(4, min = 5.8, max = 6.1),
                                  runif(4, min = 5.1, max = 6.5))
`Good Indicator` <- Observed <- c(runif(8, min = 4.5, max = 5)-0.4,
                                  runif(4, min = 3.8, max = 4.2)-0.4, 
                                  runif(8, min = 3.2, max = 3.5) + 0.2, 
                                  runif(4, min = 3.5, max = 3.7) + 0.2,
                                  runif(4, min = 4.1, max = 4.4)-0.2,
                                  runif(4, min = 3.7, max = 3.9) +0.2,
                                  runif(8, min = 3.9, max = 4.3))
`Random Indicator` <- runif(40, 2, 3)
`Bad Indicator` <- c(runif(8, min = 0.2, max = 0.6),
                     runif(4, min = 0.3, max = 0.5), 
                     runif(8, min = 0.7, max = 1.2), 
                     runif(4, min = 1.2, max = 1.5),
                     runif(4, min = 1, max = 1.4),
                     runif(4, min = 1, max = 1.3),
                     runif(8, min = 0.5, max = 1))


pred_data <- as.data.frame(cbind(`Okay Indicator`, `Good Indicator`, `Random Indicator`, `Bad Indicator`))
pred_data$Timestep <- 1:40
pred_data_long <- tidyr::pivot_longer(pred_data, cols = c(`Okay Indicator`, `Good Indicator`, `Random Indicator`, `Bad Indicator`))

data <- merge(pred_data, obs_data, by = "Timestep")
data_long <- tidyr::pivot_longer(data, cols = c(`Okay Indicator`, `Good Indicator`, `Random Indicator`, `Bad Indicator`))

# 3. Timeseries plots ----------------------------------------------------------
## 3.1. Observations plot ####
obsind <- "SSB"
obsthresh <- "MSY Btrigger"
obs_plot <- ggplot() + 
  geom_line(data = obs_data, aes(x = Timestep, y = Observed), colour = "black") +
  geom_hline(yintercept = 1, colour = "grey20", lty = 2, linewidth = 1) +
  geom_point(data = obs_data, aes(x = Timestep, y = Observed, colour = colr)) +
  scale_colour_identity() + 
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black", fill = "grey20"),
        strip.text = element_text(colour = "white", face = "bold"), 
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 12),
        aspect.ratio = 1) +
  ylab(paste0(obsind, " / ", obsthresh)) +
  xlab("Year") +
  labs(title = "True Stock Status Time Series")
obs_plot
ggsave("observations.png", plot = obs_plot, path = path, 
       height = 10, width = 10, scale = 1, units = "cm")


## 3.2. Predictions plot ####
inds <- unique(pred_data_long$name)
save <- FALSE
constant <- 0.02

for(i in inds){
  data <- pred_data_long[pred_data_long$name == i,]
  thresh <- mean(data$value)+ constant
  data$colr <- if_else(data$value < thresh, "red", "limegreen")
  
  pred_plot <- ggplot(data = data) + 
    geom_line(aes(x = Timestep, y = value)) +
    geom_hline(yintercept = mean(data$value)+constant, colour = "blue", linewidth = 1) +
    geom_point(aes(x = Timestep, y = value, colour = colr)) +
    scale_colour_identity() + 
    theme(panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.background = element_rect(colour = "black", fill = "grey20"),
          strip.text = element_text(colour = "white", face = "bold"), 
          # Axis
          axis.text = element_text(size = 8),
          #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
          axis.title = element_text(size = 12),
          aspect.ratio = 1) +
    ylab("Indicator Value") +
    xlab("Year") +
    labs(title = paste0(i, " Time Series")) +
    guides(colour = "none")
  print(pred_plot)
  
  if(isTRUE(save)){
    ggsave(paste0("predictions_", i, ".png"), plot = pred_plot, path = path, 
           height = 10, width = 10, scale = 1, units = "cm")
  }
}

# 4. Bi-plot ####
inds <- unique(pred_data_long$name)
save <- TRUE

for(i in inds){
  data2 <- data_long[data_long$name == i,]
  cor_plot <- ggplot() + 
    #annotate("rect", xmin = min(data2$Observed), xmax = max(data2$Observed), ymin = min(data2$value), ymax =  mean(data2$value)+constant, alpha = 0.15) +
    geom_hline(yintercept = mean(data2$value)+constant, colour = "blue", linewidth = 1) +
    geom_text(data = data2, aes(label = Timestep, x = Observed, y = value, colour = colr)) +
    #geom_point(data = data2, aes(x = Observed, y = value, colour = colr)) +
    scale_colour_identity() +
    geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
    theme(panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          # Axis
          axis.text = element_text(size = 8),
          #axis.text.x = element_text(angle = 90, vjust = 0.3),
          axis.title = element_text(size = 12),
          aspect.ratio = 1) +
    labs(title = paste0(i, " vs. ", obsind)) +
    ylab("Indicator Value") +
    xlab(paste0(obsind, " / ", obsthresh)) 
    #scale_x_continuous(expand = c(0,0)) +
    #scale_y_continuous(expand = c(0,0))
  
  print(cor_plot)
  
  if(isTRUE(save)){
    ggsave(paste0("correlation_", i, ".png"), plot = cor_plot, path = path,
           height = 10, width = 10, scale = 1, units = "cm")
  }
}

# Confusion matrix for this threshold level
t <- mean(data2$value)+constant
tp <- paste0("TP: ", nrow(data2[data2$Observed >= 1 & data2$value >= t,]))
fn <- paste0("FN: ", nrow(data2[data2$Observed >= 1 & data2$value < t,]))
tn <- paste0("TN: ", nrow(data2[data2$Observed < 1 & data2$value < t,]))
fp <- paste0("FP: ", nrow(data2[data2$Observed < 1 & data2$value >= t,]))
confmat <- matrix(data = c(fp, tn, tp, fn), nrow = 2, ncol = 2)
rownames(confmat) <- c("Predicted Positive", "Predicted Negative")
colnames(confmat) <- c("Observed Negative", "Observed Positive")
confmat

# 4. ROC Data ####
# some vars that need to be created to parse through roc_fun4
data$Year <- data$Timestep
data$Quarter <- 3
data$StockID <- ""
data$Survey <- ""
data$SurveyIndex <- ""
data$`Survey Index, Survey Name` <- ""

rocdata <- roc_fun4(data, obs = "Observed", 
                    preds = "Spatial Indicator", 
                    subtitle = F, p = T)
rocdata$`Spatial Indicator Value` <- as.numeric(rocdata$`Spatial Indicator Value`)

# 5. Plot loop ####
## Plot with varying thresholds
#ind <- c("Good Indicator")
ind <- c("Spatial Indicator")
path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/gridplot/"
unlink(paste0(path, "/*")) # clear files in path

for(i in 1:(length(data$Year)+1)){
  j <- i
  last <- length(obs_data$Timestep)
  if(i == last+1){i <- last}
  data$Indicator <- data[,ind]
  rocdata2 <- filter(rocdata, `Spatial Indicator` == ind)
  ## 5.1. Aesthetics ####
  thresh <- sort(data$Indicator)[i]-0.0001
  if (j == 1){thresh <- thresh - 0.01}
  if (j == last+1){thresh <- thresh + 0.01}
  
  data1 <- data %>%
    mutate(shape1 = if_else(Indicator < thresh & colr == "red", 15, 0),
           shape2 = if_else(Indicator >= thresh & colr == "limegreen", 15, 0),
           shape = shape1+shape2+1)
  
  tp <- paste0("TP = ", nrow(data[data1$colr == "limegreen" & data1$shape == 16,]))
  tn <- paste0("TN = ", nrow(data[data1$colr == "red" & data1$shape == 16,]))
  fn <- paste0("FN = ", nrow(data[data1$colr == "limegreen" & data1$shape == 1,]))
  fp <- paste0("FP = ", nrow(data[data1$colr == "red" & data1$shape == 1,]))
  tpr <- paste0("TPR = ", nrow(data[data1$colr == "limegreen" & data1$shape == 16,]), "/", nrow(data[data1$colr == "limegreen" & data1$shape == 16,]), " + ",  nrow(data[data1$colr == "limegreen" & data1$shape == 1,]), " = ", round(nrow(data[data1$colr == "limegreen" & data1$shape == 16,])/(nrow(data[data1$colr == "limegreen" & data1$shape == 16,]) + nrow(data[data1$colr == "limegreen" & data1$shape == 1,])),3))
  fpr <- paste0("FPR = ", nrow(data[data1$colr == "red" & data1$shape == 1,]), "/", nrow(data[data1$colr == "red" & data1$shape == 1,]), " + ", nrow(data[data1$colr == "red" & data1$shape == 16,]), " = ", round(nrow(data[data1$colr == "red" & data1$shape == 1,])/(nrow(data[data1$colr == "red" & data1$shape == 1,]) + nrow(data[data1$colr == "red" & data1$shape == 16,])), 3))
  tss <- paste0("TSS = ", round(nrow(data[data1$colr == "limegreen" & data1$shape == 16,])/(nrow(data[data1$colr == "limegreen" & data1$shape == 16,]) + nrow(data[data1$colr == "limegreen" & data1$shape == 1,])),3), " - ",  round(nrow(data[data1$colr == "red" & data1$shape == 1,])/(nrow(data[data1$colr == "red" & data1$shape == 1,]) + nrow(data[data1$colr == "red" & data1$shape == 16,])),3), " = ", round(round(nrow(data[data1$colr == "limegreen" & data1$shape == 16,])/(nrow(data[data1$colr == "limegreen" & data1$shape == 16,]) + nrow(data[data1$colr == "limegreen" & data1$shape == 1,])),3) - round(nrow(data[data1$colr == "red" & data1$shape == 1,])/(nrow(data[data1$colr == "red" & data1$shape == 1,]) + nrow(data[data1$colr == "red" & data1$shape == 16,])),3),3))
  
  # 5.2. Thresh plot ####
  threshplot <- ggplot() + 
    geom_point(data = data1, aes(x = Observed, y = Indicator, colour = colr, shape = shape), size = 2) +
    scale_colour_identity() +
    scale_shape_identity() +
    geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
    geom_hline(yintercept = thresh, colour = "blue", lty = 1, linewidth = 1, alpha = 0.8) +
    annotate("text", label = fp, x = 0.8, y = max(data$Indicator)+0.02, size = 3.2) +
    annotate("text", label = tp, x = 1.5, y = max(data$Indicator)+0.02, size = 3.2) +
    annotate("text", label = tn, x = 0.8, y = min(data$Indicator)-0.02, size = 3.2) +
    annotate("text", label = fn, x = 1.5, y = min(data$Indicator)-0.02, size = 3.2) +
    #annotate("text", label = tpr, x = min(data$Observed), y = max(data$Indicator) + 0.2, size = 3.2) +
    theme(panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          # Axis
          axis.text = element_text(size = 8),
          #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
          axis.title = element_text(size = 12),
          aspect.ratio = 1) +
    ylab("Indicator Value") +
    xlab(paste0(obsind, " / ", obsthresh))  
  
  # 5.3. ROC Plot ####
  rocplot <- ggplot() +
    geom_point(data = rocdata2[1:j,], aes(x = FPR, y = TPR), colour = "black", size = 0.8) +
    geom_point(data = rocdata2[j,], aes(x = FPR, y = TPR), colour = "blue", size = 2) +
    geom_abline(intercept = 0, slope = 1) +
    geom_path(data = rocdata2[1:j,], aes(x = FPR, y = TPR), colour = "blue") +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
    theme(panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          # Axis
          axis.text = element_text(size = 8),
          #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
          axis.title = element_text(size = 12),
          aspect.ratio = 1) +
    ylab("True Positive Rate (TPR)") + 
    xlab("False Positive Rate (FPR)")
  
  # 5.4. TSS Plot ####
  tssplot <- ggplot() +
    geom_point(data = rocdata2[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "black", size = 0.8) +
    geom_point(data = rocdata2[j,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue", size = 2) +
    geom_line(data = rocdata2[1:j,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(-1,1), xlim = c(min(rocdata2$`Spatial Indicator Value`),max(rocdata2$`Spatial Indicator Value`))) +
    #scale_x_continuous(limits = c(0,1))
    theme(panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          # Axis
          axis.text = element_text(size = 8),
          #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
          axis.title = element_text(size = 12),
          aspect.ratio = 1) +
    ylab("True Skill Score (TSS)") + 
    xlab("Indicator Threshold Value") 
  # 5.5. Save plots ####
  ggsave(paste0("threshplot", j, ".png"), threshplot, path = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/threshplot/")
  ggsave(paste0("rocplot", j, ".png"), rocplot, path = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/rocplot/")
  ggsave(paste0("tssplot", j, ".png"), tssplot, path = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/tssplot/")
  # 5.6. Grid ####
  gridplot <- cowplot::plot_grid(threshplot, rocplot, tssplot, nrow = 1)
  cowplot::save_plot(paste0("gridplot", j, ".png"), 
                     gridplot, 
                     path = path,
                     base_height = 5,
                     base_width = 15)
}

pROC::roc(rocdata[1:26,"status"], rocdata[1:26, "Spatial Indicator Value"], plot = T)

# 6. Animation ####
capturas <- gtools::mixedsort(list.files(path, pattern = ".png"))
images <- map(paste0(path, capturas), image_read)
images <- image_join(images) # converts our list of images into a specific class
fps <- c(1,2,4,5,10,20)
fps <- 2
name <- strsplit(capturas[1], "1")[[1]][1]

dir.create(paste0(path, "Animation/"))
unlink(paste0(path, "/Animation/*"))

for(i in fps){
  myani  <- image_animate(images, fps = i, loop = 1, dispose = "previous")
  image_write(myani, paste0(path, "Animation/", name, "_Animation_fps", i, "_loop1.gif"))
}

# 7. Opt Plot ####
ind <- c("Good Indicator")
ind <- c("Spatial Indicator")

last <- length(obs_data$Timestep)
i <- last + 1
j <- i
if(i == last +1 ){i <- last}
data$Indicator <- data[,ind]
rocdata2 <- filter(rocdata, `Spatial Indicator` == ind)
opt_thresh <- rocdata2 %>%
  filter(TSS == max(TSS))
## 7.1. Aesthetics ####
thresh <- sort(data$Indicator)[i]-0.0001
if (j == 1){thresh <- thresh - 0.01}
if (j == last + 1){thresh <- thresh + 0.01}

data1 <- data %>%
  mutate(shape1 = if_else(Indicator < thresh & colr == "red", 15, 0),
         shape2 = if_else(Indicator >= thresh & colr == "limegreen", 15, 0),
         shape = shape1+shape2+1)

tp <- paste0("TP = ", opt_thresh$TP)
tn <- paste0("TN = ", opt_thresh$TN)
fn <- paste0("FN = ", opt_thresh$FN)
fp <- paste0("FP = ", opt_thresh$FP)
tpr <- paste0("TPR = ", opt_thresh$TPR)
fpr <- paste0("FPR = ", opt_thresh$FPR)
tss <- paste0("TSS = ", opt_thresh$TSS)

data1 <- data1 %>%
  mutate(shape1 = if_else(Indicator < opt_thresh$`Spatial Indicator Value`-0.001 & colr == "red", 15, 0),
         shape2 = if_else(Indicator >= opt_thresh$`Spatial Indicator Value`-0.001 & colr == "limegreen", 15, 0),
         shape = shape1+shape2+1)

## 7.2. Thresh plot ####
threshplot <- ggplot() + 
  geom_point(data = data1, aes(x = Observed, y = Indicator, colour = colr, shape = shape), size = 2) +
  scale_colour_identity() +
  scale_shape_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  geom_text(data = opt_thresh, aes(x = 1.5, y = `Spatial Indicator Value` - 0.01, label = paste0("Threshold = ", round(`Spatial Indicator Value`, 3)))) +
  geom_hline(yintercept = opt_thresh$`Spatial Indicator Value`, colour = "blue", lty = 1, linewidth = 1, alpha = 0.8) +
  annotate("text", label = fp, x = 0.8, y = max(data$Indicator)+0.02, size = 3.2) +
  annotate("text", label = tp, x = 1.5, y = max(data$Indicator) +0.02, size = 3.2) +
  annotate("text", label = tn, x = 0.8, y = min(data$Indicator)-0.02, size = 3.2) +
  annotate("text", label = fn, x = 1.5, y = min(data$Indicator)-0.02, size = 3.2) +
  #annotate("text", label = tpr, x = min(data$Observed), y = max(data$Indicator) + 0.2, size = 3.2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 12),
        aspect.ratio = 1) +
  ylab("Indicator Value") +
  xlab(paste0(obsind, " / ", obsthresh))

## 7.3. ROC Plot ####
rocplot <- ggplot() +
  geom_point(data = rocdata2[1:j,], aes(x = FPR, y = TPR), colour = "black", size = 0.8) +
  geom_point(data = opt_thresh, aes(x = FPR, y = TPR), colour = "blue", size = 2) +
  geom_text(data = opt_thresh, aes(x = 0.25, y = 0.5, label = paste0("AUC = ", round(AUC, 3)))) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = rocdata2[1:j,], aes(x = FPR, y = TPR), colour = "blue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 12),
        aspect.ratio = 1) +
  ylab("True Positive Rate (TPR)") + 
  xlab("False Positive Rate (FPR)")

## 7.4. TSS Plot ####
tssplot <- ggplot() +
  geom_point(data = rocdata2[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "black", size = 0.8) +
  geom_point(data = opt_thresh, aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue", size = 2) +
  geom_text(data = opt_thresh, aes(x = `Spatial Indicator Value`-0.02, y = TSS + 0.1, label = paste0("TSS = ", round(TSS, 3)))) +
  geom_vline(data = opt_thresh, aes(xintercept = `Spatial Indicator Value`), colour = "blue", lty = 2) +
  geom_line(data = rocdata2[1:j,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1,1), xlim = c(min(rocdata2$`Spatial Indicator Value`),max(rocdata2$`Spatial Indicator Value`))) +
  #scale_x_continuous(limits = c(0,1))
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 12),
        aspect.ratio = 1) +
  ylab("True Skill Score (TSS)") + 
  xlab("Indicator Threshold Value") 

## 7.5. Time Series ####
data_opt <- pred_data_long[pred_data_long$name == ind,]
thresh <- opt_thresh$`Spatial Indicator Value`-0.001
data_opt$value <- data_opt$value/thresh
data_opt$colr <- if_else(data_opt$value < 1, "red", "limegreen")

opt_plot <- ggplot(data = data_opt) + 
  geom_line(aes(x = Timestep, y = value)) +
  geom_hline(yintercept = 1, colour = "blue", linewidth = 1) +
  geom_point(aes(x = Timestep, y = value, colour = colr)) +
  scale_colour_identity() + 
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black", fill = "grey20"),
        strip.text = element_text(colour = "white", face = "bold"), 
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 12),
        aspect.ratio = 1) +
  labs(title = "Spatial Indicator Time Series") +
  ylab("Indicator Value / Optimal Threshold") +
  xlab("Year") +
  #labs(title = paste0(i, " Time Series")) +
  guides(colour = "none")


## 7.5. Save plots ####
ggsave(paste0("threshplot_opt", j, ".png"), threshplot, path = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/threshplot/")
ggsave(paste0("rocplot_opt", j, ".png"), rocplot, path = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/rocplot/")
ggsave(paste0("tssplot_opt", j, ".png"), tssplot, path = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/tssplot/")
ggsave(paste0("predictions_opt", i, ".png"), plot = opt_plot, path = path, height = 10, width = 10, scale = 1, units = "cm")

## 7.6. Grid ####
gridplot <- cowplot::plot_grid(threshplot, rocplot, tssplot, nrow = 1)
cowplot::save_plot(paste0("gridplot_opt", j, ".png"), 
                   gridplot, 
                   path = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROCprocess/gridplot/",
                   base_height = 5,
                   base_width = 15)

