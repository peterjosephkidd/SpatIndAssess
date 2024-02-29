#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>                                ROC functions
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>#> List of functions:
#>
#> 1. roc_fun4()
#> 2. rocR()
#> Pending changes:
#>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
roc_fun4 <- function(data, obs, preds, return = "long", p = FALSE, auclvl = 0.5, subtitle = TRUE){
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ROC >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#> This function takes an observations and predictions then calculates
#> ROC statisitcs (TP, FP, TN, FP, TSS, AUC)
#> Data can be returned in a long format for plotting. Or a list of wide dfs.
#> A basic plot can also be plotted. 
#> 
#>>> ARGUMENTS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> data: the dataframe with series of obs and preds
#> obs: the column name in `data` that corresponds to the true observations from 
#>      which to validate predictions against
#> preds: the column name(s) in `data` that correspond to predictions which you
#>        want to validate against obs. This can be a list of column names c()
#> return: can be set to "widelist" or "long". 
#>         "widelist" will output a df per 
#>         pred in wide format. These will be stored in a list of wide dfs. 
#>         "long" will return a single df. This format is useful for plotting with
#>         ggplot
#>  p: if TRUE, a basic ROC plot will be produced
#>  auclvl: used to colour to the ROC curve in the plot. 
#>          ROC curve below auclvl = red
#>          ROC curve above auclvl = green
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#  
  roc_widelist <- list()
  roc_longlist <- list()
  
  for(i in 1:length(preds)){
    pred <- preds[i]
    # select data
    roc2 <- data %>% select("Year", "Quarter", "StockID", "SurveyIndex", "Survey", `Survey Index, Survey Name`)
    roc2$obs <- data[obs]
    roc2$pred <- data[pred]
    roc2$status <- as.vector(roc2$obs >= 1)
    roc2 <- arrange(roc2, pred)
    roc2 <- roc2[!is.na(roc2$pred),] # remove rows with NAs in the spat ind (usually from where the ere wasnt data to calucate the index)
    
    if(length(unique(roc2$status)) < 2){
      warning(paste0("\nIn all the survey years, the true status of the stock from stock assessments is ", 
                     unique(roc2$status),
                     ".\n There needs to be contrast in status of the stock for ROC to be computed"), immediate. = TRUE)
      }
    
    rocoutput2 <- list()
    for(j in 1:nrow(roc2)){
      predt <- roc2$pred[j,] - 0.00001
      pos <- which(roc2$pred > predt) # the instances classified as positive
      tp  <- nrow(roc2[pos,][roc2[pos,]$status == "TRUE",]) # how many of these are actually positive 
      fp  <- nrow(roc2[pos,][roc2[pos,]$status == "FALSE",]) # how many of these are actually negative 
      neg <- which(roc2$pred < predt) # the instances classified as negative
      tn  <- nrow(roc2[neg,][roc2[neg,]$status == "FALSE",]) # how many of these are actually negative 
      fn  <- nrow(roc2[neg,][roc2[neg,]$status == "TRUE",]) # how many of these are actually positive 
      tpr <- tp/(tp+fn)
      fpr <- fp/(fp+tn)
      tss <- tpr-fpr
      output <- c(tp, fp, tn, fn, tpr, fpr, tss)
      rocoutput2 <- rbind(rocoutput2, output)
    }
    
    colnames(rocoutput2) <- c("TP", "FP", "TN", "FN", "TPR", "FPR", "TSS")
    rocoutput <- cbind(roc2, rocoutput2)
    
    # create new last row with pseudo data to make ROC curve start at (FPR = 0, TPR = 0)
    botleft <- c(998, 
                 unique(rocoutput$Quarter), 
                 unique(rocoutput$StockID), 
                 unique(rocoutput$SurveyIndex), 
                 unique(rocoutput$Survey), 
                 unique(rocoutput$`Survey Index, Survey Name`),  
                 999, 
                 max(rocoutput$pred)+ 0.01, 
                 NA, 
                 0, length(roc2$status[roc2$status == "TRUE"]), length(roc2$status[roc2$status == "FALSE"]), 0, 0, 0, 0)
    rocoutput <- rbind(rocoutput, botleft)
    rocoutput$TPR <- as.numeric(rocoutput$TPR)
    rocoutput$FPR <- as.numeric(rocoutput$FPR)
    rocoutput$TSS <- as.numeric(rocoutput$TSS)
    rocoutput$Year <- as.numeric(rocoutput$Year)
    rocoutput$AUC <- sum(diff(1-rocoutput$FPR)*zoo::rollmean(rocoutput$TPR,2)) 
    
    # wide format
    roc_widelist[[i]] <- rocoutput
    # long format (useful for ggplot)
    roc_long <- rocoutput %>% tidyr::pivot_longer(cols = c("pred"), 
                                                  names_to = "Spatial Indicator",
                                                  values_to = "Spatial Indicator Value")
    roc_long$`Spatial Indicator` <- pred
    #roc_long$`Spatial Indicator Value`[roc_long$Year==998,]
    roc_long <- data.table::as.data.table(roc_long)
    roc_longlist[[i]] <- roc_long
    
    if(p == TRUE){
      rocoutput3 <- rocoutput %>% 
        mutate(colr = if_else(AUC <= auclvl, "red", "limegreen"))
      rocoutput4 <- filter(rocoutput3, Year > 1000)
      rocoutput4$cFPR <- filter(rocoutput3, Year > 1000)
      
      if(subtitle == T){
        subtext <- paste0(unique(rocoutput3$StockID), " | ", unique(rocoutput3$`Survey Index, Survey Name`), " | ", min(rocoutput4$Year), " - ", max(rocoutput4$Year))
        } else{subtext <- NULL}
        
      roc_plot <- ggplot(data = rocoutput3) +
        geom_path(aes(x = FPR, y = TPR, colour = colr), alpha = 0.7, linewidth = 1.2) +
        scale_colour_identity() +
        geom_point(aes(x = FPR, y = TPR)) +
        geom_abline(slope = 1, intercept = 0) + 
        annotate("text", label = paste0("AUC = ", round(unique(rocoutput3$AUC),3)), x = 0.8, y = 0.1) +
        labs(title = colnames(rocoutput3$pred), subtitle =  subtext)
      print(roc_plot)
    }
  }
  
  # compress list of long dataframe
  roc_long <- as.data.frame(data.table::rbindlist(roc_longlist))
  if(return == "widelist"){
    return(roc_widelist)}else{
      return(roc_long)
    }
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
rocR <- function(data, obs, preds, return = "long", p = FALSE, auclvl = 0.5, subtitle = TRUE){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ROC >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  #> This function takes an observations and predictions then calculates
  #> ROC statisitcs (TP, FP, TN, FP, TSS, AUC)
  #> Data can be returned in a long format for plotting. Or a list of wide dfs.
  #> A basic plot can also be plotted. 
  #> 
  #>>> ARGUMENTS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #> data: the dataframe with series of obs and preds
  #> obs: the column name in `data` that corresponds to the true observations from 
  #>      which to validate predictions against
  #> preds: the column name(s) in `data` that correspond to predictions which you
  #>        want to validate against obs. This can be a list of column names c()
  #> return: can be set to "widelist" or "long". 
  #>         "widelist" will output a df per 
  #>         pred in wide format. These will be stored in a list of wide dfs. 
  #>         "long" will return a single df. This format is useful for plotting with
  #>         ggplot
  #>  p: if TRUE, a basic ROC plot will be produced
  #>  auclvl: used to colour to the ROC curve in the plot. 
  #>          ROC curve below auclvl = red
  #>          ROC curve above auclvl = green
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#  
  
  roc_widelist <- list()
  roc_longlist <- list()
  
  for(i in 1:length(preds)){
    
    pred <- preds[i]
    writeLines(paste0(obs, " vs. ", pred))
    obs_data  <- data[colnames(data) == state]
    status    <- as.vector(obs_data >= 1)
    pred_data <- data[colnames(data) == pred]
    
    # select data
    roc2 <- data %>% 
      select("Year", "Quarter", "StockKeyLabel", "SurveyIndex", "SurveyName", "SurveyNameIndex") %>%
      cbind(., obs_data, pred_data, status) %>%
      arrange(., pick(pred))
    
    #roc2 <- roc2[!is.na(roc2$pred),] # remove rows with NAs in the spat ind (usually from where the ere wasnt data to calucate the index)
    rocoutput2 <- list()
    if (nrow(roc2) == 0){
      
      warning(paste0("No data for current survey and index within years and quarters provided. Filling df with NAs."), immediate. = TRUE)
      TP <- NA; FP <- NA; TN <- NA; FN <- NA; TPR <- NA; FPR <- NA; TSS <- NA
      output <- cbind(TP, FP, TN, FN, TPR, FPR, TSS)
      rocoutput2 <- rbind(rocoutput2, output)
      
    } else if (all(is.na(roc2[colnames(roc2) == pred]))){
      
      warning(paste0("No spatial indicator data for current survey and index. Filling df with NAs."), immediate. = TRUE)
      TP <- rep(NA, nrow(roc2));  FP <- rep(NA, nrow(roc2)); TN <- rep(NA, nrow(roc2))
      FN <- rep(NA, nrow(roc2)); TPR <- rep(NA, nrow(roc2)); FPR <- rep(NA, nrow(roc2))
      TSS <- rep(NA, nrow(roc2))
      
      output <- cbind(TP, FP, TN, FN, TPR, FPR, TSS)
      rocoutput2 <- rbind(rocoutput2, output)
      
    } else if (length(unique(roc2$status)) < 2){
      
      warning(paste0("No contrast in stock status. Filling df with NAs."), immediate. = TRUE)
      TP <- rep(NA, nrow(roc2));  FP <- rep(NA, nrow(roc2)); TN <- rep(NA, nrow(roc2))
      FN <- rep(NA, nrow(roc2)); TPR <- rep(NA, nrow(roc2)); FPR <- rep(NA, nrow(roc2))
      TSS <- rep(NA, nrow(roc2))
      
      output <- cbind(TP, FP, TN, FN, TPR, FPR, TSS)
      rocoutput2 <- rbind(rocoutput2, output)
      
    } else{
      
      for(j in 1:nrow(roc2)) {
        
        predt <- roc2[j,colnames(roc2) == pred] - 0.00001
        
        if (is.na(predt)) {
          
          warning(paste0("stk: ",     roc2[j,]$StockKeyLabel,
                         "   srv: ",  roc2[j,]$SurveyName,
                         "   indx: ", roc2[j,]$SurveyName,
                         "   Yr: ",   roc2[j,]$Year,
                         "   Qr: ",   roc2[j,]$Quarter,
                         "\nNo spatial indicator data. Filling row with NAs."))
          
          TP <- NA; FP <- NA; TN <- NA; FN <- NA; TPR <- NA; FPR <- NA; TSS <- NA
          output <- cbind(TP, FP, TN, FN, TPR, FPR, TSS)
          rocoutput2 <- rbind(rocoutput2, output)
          
        } else {
          
          pos <- which(roc2[colnames(roc2) == pred] > predt)     # the instances classified as positive
          TP  <- nrow(roc2[pos,][roc2[pos,]$status == "TRUE",])  # how many of these are actually positive 
          FP  <- nrow(roc2[pos,][roc2[pos,]$status == "FALSE",]) # how many of these are actually negative 
          neg <- which(roc2[colnames(roc2) == pred] < predt)     # the instances classified as negative
          TN  <- nrow(roc2[neg,][roc2[neg,]$status == "FALSE",]) # how many of these are actually negative 
          FN  <- nrow(roc2[neg,][roc2[neg,]$status == "TRUE",])  # how many of these are actually positive 
          TPR <- TP/(TP+FN)
          FPR <- FP/(FP+TN)
          TSS <- TPR-FPR
          
          output <- cbind(TP, FP, TN, FN, TPR, FPR, TSS)
          rocoutput2 <- rbind(rocoutput2, output)
          
        }
      }
    }
    
    rocoutput <- cbind(roc2, rocoutput2)
    
    if (all(is.na(rocoutput$TSS))) {
      rocoutput$AUC <- NA
    } else {
      
      botleft <- c(998, 
                   unique(rocoutput$Quarter), 
                   unique(rocoutput$StockKeyLabel), 
                   unique(rocoutput$SurveyIndex), 
                   unique(rocoutput$SurveyName), 
                   unique(rocoutput$SurveyNameIndex),  
                   NA, 
                   max(rocoutput[colnames(rocoutput) == pred], na.rm = T) + 0.01, 
                   NA, 
                   0, length(roc2$status[roc2$status == "TRUE"]), length(roc2$status[roc2$status == "FALSE"]), 0, 0, 0, 0)
      
      rocoutput <- rbind(rocoutput, botleft)
      
      rocoutput <- rocoutput %>%
        mutate(TP = as.numeric(TP),
               FP = as.numeric(FP),
               TN = as.numeric(TN),
               FN = as.numeric(FN),
               TPR = as.numeric(TPR),
               FPR = as.numeric(FPR),
               TSS = as.numeric(TSS),
               Year = as.numeric(Year))
      
      TPRauc <- na.omit(rocoutput)
      rocoutput$AUC <- sum(diff(1-TPRauc$FPR)*zoo::rollmean(TPRauc$TPR,2)) 
    }
    
    roc_widelist[[i]] <- rocoutput
    
    roc_long <- rocoutput %>% 
      tidyr::pivot_longer(cols = colnames(rocoutput)[colnames(rocoutput) == pred], 
                          names_to = "Spatial Indicator",
                          values_to = "Spatial Indicator Value")
    
    roc_long <- data.table::as.data.table(roc_long)
    roc_longlist[[i]] <- roc_long
    
    if(p == TRUE & all(!is.na(rocoutput$TSS))){
      rocoutput3 <- rocoutput %>% 
        mutate(colr = if_else(AUC <= auclvl, "red", "limegreen"))
      rocoutput3b <- rocoutput3[!is.na(rocoutput3[colnames(rocoutput3) == pred]),]
      
      rocoutput4 <- filter(rocoutput3, Year > 1000)
      rocoutput4$cFPR <- filter(rocoutput3, Year > 1000)
      
      if(subtitle == T){
        subtext <- paste0(unique(rocoutput3$StockKeyLabel), " | ", unique(rocoutput3$SurveyNameIndex), " | ", min(as.numeric(rocoutput4$Year)), " - ", max(as.numeric(rocoutput4$Year)), " | Qrs ", unique(rocoutput3$Quarter))
      } else{subtext <- NULL}
      
      roc_plot <- ggplot(data = rocoutput3b) +
        geom_path(aes(x = FPR, y = TPR, colour = colr), alpha = 0.7, linewidth = 1.2) +
        scale_colour_identity() +
        geom_point(aes(x = FPR, y = TPR)) +
        geom_abline(slope = 1, intercept = 0) + 
        annotate("text", label = paste0("AUC = ", round(unique(rocoutput3$AUC),3)), x = 0.8, y = 0.1) +
        labs(title = paste0(obs, " vs. ", pred), subtitle =  subtext)
      print(roc_plot)
    }
    
    roc_long <- as.data.frame(data.table::rbindlist(roc_longlist))
    
  }
  
  if(format.df == "widelist"){
    return(roc_widelist)
  } else {
    return(roc_long)
  }
}