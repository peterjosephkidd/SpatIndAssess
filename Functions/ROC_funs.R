################################################################################
################################## ROC_fun #####################################
################################################################################

roc_fun <- function(all_data, state, ind, pos_state = "high", pos_ind = "high"){
  if(pos_state == "high" & pos_ind == "high"){
    roc2 <- all_data %>% 
      select("Year", state, ind)
    roc <- roc2 %>%
      mutate(State = as.vector(roc2["state"] >= 1)) %>% # create TRUE/FALSE column (T = good status)
      arrange(ind) %>%
      mutate(#Rank = row_number(),
        TP = (sum(State == TRUE)) - (cumsum(State == TRUE)),
        TN = (cumsum(State == FALSE)),
        FP = (sum(State == FALSE)) - TN,
        FN = (sum(State == TRUE)) - TP) %>%
      rowwise() %>%
      mutate(N = sum(TP, TN, FP, FN),
             TPR = TP/(TP+FN), # ability to identify positive cases
             TNR = TN/(TN+FP), # ability to identify negatiev cases
             FPR = 1-TNR, 
             TSS = TPR + TNR - 1) # perfect pred = 1, rand = 0, worse than rand = <0
    return(roc)
  } else if(pos_state == "high" & pos_ind == "low"){
    roc2 <- all_data %>% 
      select("Year", state, ind)
    roc <- roc2 %>%
      mutate(State = as.vector(roc2["state"] >= 1)) %>%
      arrange(ind) %>%
      mutate(#Rank = row_number(),
        TP = (cumsum(State == TRUE)),
        TN = (sum(State == FALSE)) - (cumsum(State == FALSE)),
        FP = (sum(State == FALSE)) - TN,
        FN = (sum(State == TRUE)) - TP) %>%
      rowwise() %>%
      mutate(N = sum(TP, TN, FP, FN),
             TPR = TP/(TP+FN), # ability to identify positive cases
             TNR = TN/(TN+FP), # ability to identify negatiev cases
             FPR = 1-TNR, 
             TSS = TPR + TNR - 1) # perfect pred = 1, rand = 0, worse than rand = <0
    return(roc)
  } else if(pos_state == "low" & pos_ind == "high"){
    roc2 <- all_data %>% 
      select("Year", state, ind)
    roc <- roc2 %>%
      mutate(State = as.vector(roc2["state"] <= 1)) %>%
      arrange(ind) %>%
      mutate(#Rank = row_number(),
        TP = (sum(State == TRUE)) - (cumsum(State == TRUE)),
        TN = (cumsum(State == FALSE)),
        FP = (sum(State == FALSE)) - TN,
        FN = (sum(State == TRUE)) - TP) %>%
      rowwise() %>%
      mutate(N = sum(TP, TN, FP, FN),
             TPR = TP/(TP+FN), # ability to identify positive cases
             TNR = TN/(TN+FP), # ability to identify negatiev cases
             FPR = 1-TNR, 
             TSS = TPR + TNR - 1) # perfect pred = 1, rand = 0, worse than rand = <0 
    return(roc)
  } else if(pos_state == "low" & pos_ind == "low"){
    roc2 <- all_data %>% 
      select("Year", state, ind)
    roc <- roc2 %>%
      mutate(State = as.vector(roc2["state"] <= 1)) %>%
      arrange(ind) %>%
      mutate(#Rank = row_number(),
        TP = (cumsum(State == TRUE)),
        TN = (sum(State == FALSE)) - (cumsum(State == FALSE)),
        FP = (sum(State == FALSE)) - TN,
        FN = (sum(State == TRUE)) - TP) %>%
      rowwise() %>%
      mutate(N = sum(TP, TN, FP, FN),
             TPR = TP/(TP+FN), # ability to identify positive cases
             TNR = TN/(TN+FP), # ability to identify negatiev cases
             FPR = 1-TNR, 
             TSS = TPR + TNR - 1) # perfect pred = 1, rand = 0, worse than rand = <0
    return(roc)
  }
}

################################################################################
################################# ROC_fun2 #####################################
################################################################################

# fixed some bugs in roc_fun
    
roc_fun2 <- function(all_data, state, ind){
  # select data
  roc2 <- all_data %>% 
    select("Year", state, ind)
  # Add TRUE/FALSE labels for true stock status
  roc <- roc2 %>%
    mutate(label = as.vector(roc2["state"] >= 1)) %>% # create TRUE/FALSE column (T = good status)
    arrange(ind)
  if(length(unique(roc$label)) < 2){
    warning(paste0("Only one level in roc$label. ROC curve cannot be calculated. Two levels are needed. In all the survey years, the true status of the stock from stock assessments is ", unique(roc$label) ,". 

Number of levels = ", length(unique(roc$label)), "; ",
"Label of the level = ", unique(roc$label), "; ",
"Number of ", unique(roc$label), " state records = ", length(roc$label)), immediate. = TRUE)
  }
  # calculate ROC stats
  roc <- roc %>% mutate(
    TP = sum(roc$label == TRUE) - cumsum(lag(roc$label == TRUE, default = 0)),
    FP = sum(roc$label == FALSE) - cumsum(lag(label == FALSE, default = 0)),
    TN = cumsum(lag(roc$label == FALSE, default = 0)),
    FN = cumsum(lag(label == TRUE, default = 0)),
    TPR = (sum(roc$label == TRUE) - cumsum(lag(roc$label == TRUE, default = 0)))/(sum(roc$label==TRUE)),
    FPR = (sum(roc$label == FALSE) - cumsum(lag(roc$label == FALSE, default = 0)))/(sum(roc$label==FALSE)),
    TNR = cumsum(lag(roc$label == FALSE, default = 0))/(sum(roc$label==FALSE)),
    TSS =  (sum(roc$label == TRUE) - cumsum(lag(roc$label == TRUE, default = 0)))/(sum(roc$label==TRUE)) + cumsum(lag(roc$label == FALSE, default = 0))/(sum(roc$label==FALSE)) -1)# TPR + TNR - 1
  # create new last row with pseudo data to make ROC curve start at (FPR = 0, TPR = 0)
  init <- data.frame(Year = 998,
                     state = 999, 
                     ind = max(roc$ind)+0.01, 
                     label = NA, 
                     TP = 0,
                     FP = 0,
                     TN = sum(roc$label==FALSE),
                     FN = sum(roc$label==TRUE),
                     TPR = 0,
                     FPR = 0, 
                     TNR = 1,
                     TSS = 0)
  # insert new row and arrange by spatial indicator value
  roc <- roc %>%
  rows_insert(init) %>%
  arrange(ind)
  
  return(roc)
}


roc_fun3 <- function(all_data, state, inds, return = "long"){
  
  roc_widelist <- list()
  roc_longlist <- list()
  
  for(i in 1:length(inds)){
    ind <- inds[i]
    # select data
    roc2 <- all_data %>% select("Year", "Quarter", "StockID", "SurveyIndex", "Survey", `Survey Index, Survey Name`)
    roc2$state <- all_data[state]
    roc2$ind <- all_data[ind]
    roc2$status <- as.vector(roc2$state >= 1)
    roc2 <- arrange(roc2, ind)
    roc2 <- roc2[!is.na(roc2$ind),] # remove rows with NAs in the spat ind (usually from where the ere wasnt data to calucate the index)
    
    if(length(unique(roc2$status)) < 2){
      warning(paste0("\nIn all the survey years, the true status of the stock from stock assessments is ", 
                     unique(roc2$status),
                     ".\n There needs to be contrast in status of the stock for ROC to be computed"), immediate. = TRUE)}
    
    # calculate ROC stats
    roc <- roc2 %>% mutate(
      TP = sum(roc2$status == TRUE) - cumsum(lag(roc2$status == TRUE, default = 0)),
      FP = sum(roc2$status == FALSE) - cumsum(lag(status == FALSE, default = 0)),
      TN = cumsum(lag(roc2$status == FALSE, default = 0)),
      FN = cumsum(lag(status == TRUE, default = 0)),
      TPR = (sum(roc2$status == TRUE) - cumsum(lag(roc2$status == TRUE, default = 0)))/(sum(roc2$status==TRUE)),
      FPR = (sum(roc2$status == FALSE) - cumsum(lag(roc2$status == FALSE, default = 0)))/(sum(roc2$status==FALSE)),
      TNR = cumsum(lag(roc2$status == FALSE, default = 0))/(sum(roc2$status==FALSE)),
      TSS =  (sum(roc2$status == TRUE) - cumsum(lag(roc2$status == TRUE, default = 0)))/(sum(roc2$status==TRUE)) + cumsum(lag(roc2$status == FALSE, default = 0))/(sum(roc2$status==FALSE)) -1)# TPR + TNR - 1
    
    # create new last row with pseudo data to make ROC curve start at (FPR = 0, TPR = 0)
    row <- c(998, unique(roc$Quarter), unique(roc$StockID), unique(roc$SurveyIndex), 
             unique(roc$Survey), unique(roc$`Survey Index, Survey Name`),  
             999, max(roc$ind)+ 0.01, NA, 0, 0, sum(roc$status==FALSE), 
             sum(roc$status==TRUE), 0, 0, 1, 0)
    
    # bind (this changes all cols to character)
    roc <- rbind(roc, row)
    # ocnvert cols back to numeric
    roc <- roc %>%
      mutate_at(vars(Year, TP, FP, TN, FN, TPR, FPR, TNR, TSS), as.numeric)
    # wide format
    roc_widelist[[i]] <- roc
    # long format (useful for ggplot)
    roc_long <- roc %>% tidyr::pivot_longer(cols = c("ind"), 
                                            names_to = "Spatial Indicator",
                                            values_to = "Spatial Indicator Value")
    roc_long$`Spatial Indicator` <- ind
    #roc_long$`Spatial Indicator Value`[roc_long$Year==998,]
    roc_long <- as.data.table(roc_long)
    roc_longlist[[i]] <- roc_long
  }
  
  #rocall_wide <- do.call(rbind, roc_widelist)
  # compress list of long dataframe
  # rocall_long <- do.call(rbind, roc_longlist) # behaving weird with rownames so using datatable alternative:
  roc_long <- as.data.frame(rbindlist(roc_longlist))
  if(return == "widelist"){
    return(roc_widelist)}else{
      return(roc_long)
    }
}


roc_fun4 <- function(data, obs, preds, return = "long", p = FALSE, auclvl = 0.5){
  
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
      tp <- nrow(roc2[pos,][roc2[pos,]$status == "TRUE",]) # how many of these are actually positive 
      fp <- nrow(roc2[pos,][roc2[pos,]$status == "FALSE",]) # how many of these are actually negative 
      neg <- which(roc2$pred < predt) # the instances classified as negative
      tn <- nrow(roc2[neg,][roc2[neg,]$status == "FALSE",]) # how many of these are actually negative 
      fn <- nrow(roc2[neg,][roc2[neg,]$status == "TRUE",]) # how many of these are actually positive 
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
    rocoutput$AUC <- sum(diff(1-rocoutput$FPR)*rollmean(rocoutput$TPR,2)) 
    
    # wide format
    roc_widelist[[i]] <- rocoutput
    # long format (useful for ggplot)
    roc_long <- rocoutput %>% tidyr::pivot_longer(cols = c("pred"), 
                                                  names_to = "Spatial Indicator",
                                                  values_to = "Spatial Indicator Value")
    roc_long$`Spatial Indicator` <- pred
    #roc_long$`Spatial Indicator Value`[roc_long$Year==998,]
    roc_long <- as.data.table(roc_long)
    roc_longlist[[i]] <- roc_long
    
    if(p == TRUE){
      rocoutput3 <- rocoutput %>% 
        mutate(colr = if_else(AUC <= auclvl, "red", "limegreen"))
      rocoutput4 <- filter(rocoutput3, Year > 1000)
      rocoutput4$cFPR <- filter(rocoutput3, Year > 1000)
      
      roc_plot <- ggplot(data = rocoutput3) +
        geom_path(aes(x = FPR, y = TPR, colour = colr), alpha = 0.7, linewidth = 1.2) +
        scale_colour_identity() +
        geom_point(aes(x = FPR, y = TPR)) +
        geom_abline(slope = 1, intercept = 0) + 
        annotate("text", label = paste0("AUC = ", round(unique(rocoutput3$AUC),3)), x = 0.8, y = 0.1) +
        labs(title = colnames(rocoutput3$pred), subtitle =  paste0(unique(rocoutput3$StockID), " | ", unique(rocoutput3$`Survey Index, Survey Name`), " | ", min(rocoutput4$Year), " - ", max(rocoutput4$Year)))
      print(roc_plot)
    }
  }
  
  # compress list of long dataframe
  roc_long <- as.data.frame(rbindlist(roc_longlist))
  if(return == "widelist"){
    return(roc_widelist)}else{
      return(roc_long)
    }
}

################################################################################
################################# ROC_Group ####################################
################################################################################

# For plotting multiple roc curves on one plot. Has been updated to use roc_fun2

roc_group <- function(SpatInds, StatusInds, stk_status, stk_name, species_name, survey_index, survey_name){
  cols <- 2
  stack <- 0.95
  
  qrs <- unique(SpatInds$Quarter)
  qrs_text <- paste(as.character(sort(qrs)), collapse = ", ")
  
  #x11(width = 20, height = 15)
  par(mar = c(5,5,5,22), xpd = T)
  p <- recordPlot()
  
  plot(1, type = "n", xlim = c(0,1), ylim = c(0,1), 
       xlab = "False positive rate", 
       ylab = "True positive rate",
       xaxs = "r",
       yaxs = "r",
       cex.axis = 1.5, 
       cex.lab = 1.5)
  
  if(missing(survey_index)|missing(survey_name)==TRUE){
    title = paste0("ROC: Indicators vs ", stk_status," (",species_name,")
  ", min(yrs), "-", max(yrs), " (Q", qrs_text, ")")
  }
  title = paste0("ROC: Indicators vs ", stk_status," (",species_name,")
  Survey Index: ", survey_index, ", Survey: ", survey_name, " (", min(yrs), "-", max(yrs), ", Q", qrs_text, ")")
  
  subtitle = paste0("Stock: ", stk_name)
  
  title(main = title,
        cex.main = 1.8,
        adj = 0,
        line = 1)
  title(sub = subtitle,
        cex.sub = 1.5,
        adj = 0.95,
        line = -2)
  #mtext(side=3, line=2, at=-0.07, adj= 0.1, cex=1.8, mytitle)
  #mtext(side=3, line=1, at=-0.07, adj= 0.1, cex=1.5, mysubtitle)
  
       #main = paste0("ROC: Indicators vs ", stk_status," (",species$spcs,")"),
       #sub = paste0("Stock: ", names(stk)))
  
  for(stk_index in colnames(SpatInds[match("Gini Index", colnames(SpatInds)):length(colnames(SpatInds))])){
  # Create dataset with all spatial indicators and true stock status
  all_data <- Reduce(function(x, y) merge(x, y, by = "Year"), list(SpatInds, StatusInds))

  all_data[ncol(all_data)+1] <- all_data[stk_index] # copies indicator to end column
  colnames(all_data)[ncol(all_data)] <- "ind" # changes name of end column
  all_data[ncol(all_data)+1] <- all_data[stk_status] # copies status to end column
  colnames(all_data)[ncol(all_data)] <- "state" # changes name of end column
  
  roc <- roc_fun2(all_data, 
                 state = state, 
                 ind = ind)
  
    lines(roc$FPR, roc$TPR, type = "l", col = cols, lwd = 2)
  
  opt_thresh <- order(roc[,"TSS"],decreasing = T)[1]
  opt_threshx <- as.data.frame(roc[opt_thresh, "FPR"])
  opt_threshy <- as.data.frame(roc[opt_thresh, "TPR"])
  table(unique(roc$State))
  
  x <- 1-roc$FPR
  y <- roc$TPR
  AUC <- sum(diff(x)*rollmean(y,2)) 
  
  points(opt_threshx, opt_threshy, pch = 19, cex = 1.8, col = cols)
  
  legend(legend = paste0(stk_index, " (", round(AUC, 2), ")"), 
         fill = cols, 
         x = 1.04, 
         y = stack, 
         cex = 1.5, 
         bty = "n", 
         inset = c(-0.2,0))
  
  mtext(text = "Spatial Indicator (AUC)", line = -2, at = 1.25, cex = 1.5)
  
  cols <- cols+1
  stack <- stack-0.07
  }
  par(xpd = F)
  abline(0,1, lwd = 2)
  return(p)
}

################################################################################
################################# TSS_Group ####################################
################################################################################

# for plotting the true skill score of multiple spatial indicators on one plot 
# currenly uses roc_fun within this function. Use tss_group2 to use roc_fun2.
tss_group <- function(SpatInds, StatusInds, stk_status, stk_name, species_name){
  cols <- 2
  stack <- 0.95
  
  qrs_text <- paste(as.character(sort(qrs)), collapse = ", ")
  #x11(width = 20, height = 15)
  par(mar = c(5,5,5,22), xpd = T)
  p <- recordPlot()
  
  plot(1, type = "n", xlim = c(0,1), ylim = c(-1,1), 
       xlab = "Index Threshold", 
       ylab = "TSS",
       xaxs = "r",
       yaxs = "r",
       cex.axis = 1.5, 
       cex.lab = 1.5)
  #mytitle = paste0("True Skill Score: Indicators vs ", stk_status," (",species$spcs,")", min(yrs), "-", max(yrs), " (Q", qrs_text, ")")
  mytitle = paste0("True Skill Score: Indicators vs ", stk_status," (",species_name,")
", min(yrs), "-", max(yrs), " (Q", qrs_text, ")")
  mysubtitle = paste0("Stock: ", stk_name)
  title(main = mytitle,
        cex.main = 1.8,
        adj = 0,
        line = 1)
  title(sub = mysubtitle,
        cex.sub = 1.5,
        adj = 0.95,
        line = -2)
  #mtext(side=3, line=2, at=-0.07, adj= 0.1, cex=1.8, mytitle)
  #mtext(side=3, line=1, at=-0.07, adj= 0.1, cex=1.5, mysubtitle)
  
  #main = paste0("ROC: Indicators vs ", stk_status," (",species$spcs,")"),
  #sub = paste0("Stock: ", names(stk)))
  
  for(stk_index in colnames(SpatInds[-1])){
    par(mar = c(5,5,3,18), xpd = T)
    
    # Create dataset with all spatial indicators and true stock status
    all_data <- Reduce(function(x, y) merge(x, y, by = "Year"), list(SpatInds, StatusInds))
    
    all_data[ncol(all_data)+1] <- all_data[stk_index]  # copies indicator to end column
    colnames(all_data)[ncol(all_data)] <- "ind"        # changes name of end column
    all_data[ncol(all_data)+1] <- all_data[stk_status] # copies status to end column
    colnames(all_data)[ncol(all_data)] <- "state"      # changes name of end column
    
    roc <- roc_fun(all_data, 
                   state = state, 
                   ind = ind, 
                   pos_state = "high", # high value = better status
                   pos_ind = "high") # high value = better stat
  
    opt_thresh <- order(roc[,"TSS"],decreasing = T)[1]
    opt_threshx <- as.data.frame(roc[opt_thresh, "ind"])
    opt_threshy <- round(as.data.frame(roc[opt_thresh, "TSS"]),2)
    
    
    lines(roc$ind, roc$TSS, type = "l", col = cols, lwd = 2)
    legend(legend = stk_index, 
           fill = cols, 
           x = 1.04, 
           y = stack, 
           cex = 1.5, 
           bty = "n", 
           inset = c(-0.2,0))
    
    par(xpd = F)
    abline(v = opt_threshx, col = alpha(cols, 0.5), lty = 2, alpha = 0.5)
    text(opt_threshx, opt_threshy+0.06, labels = opt_threshy)
    points(opt_threshx, opt_threshy, labels = opt_threshy, pch = 19)
    
    mtext(text = "Spatial Indicator", line = -4, at = 1.2, cex = 1.5)

    cols <- cols+1
    stack <- stack-0.12
    
  }
  return(p)
}

################################################################################
################################ TSS_Group2 ####################################
################################################################################

# for plotting the true skill score of multiple spatial indicators on one plot 
# currenly uses roc_fun within this function. Use tss_group2 to use roc_fun2.
tss_group2 <- function(SpatInds, StatusInds, stk_status, stk_name, species_name, survey_index, survey_name){
  cols <- 2
  stack <- 0.95
  
  qrs <- unique(SpatInds$Quarter)
  qrs_text <- paste(as.character(sort(qrs)), collapse = ", ")
  #x11(width = 20, height = 15)
  par(mar = c(5,5,5,22), xpd = T)
  p <- recordPlot()
  
  plot(1, type = "n", xlim = c(0,1), ylim = c(-1,1), 
       xlab = "Index Threshold", 
       ylab = "TSS",
       xaxs = "r",
       yaxs = "r",
       cex.axis = 1.5, 
       cex.lab = 1.5)
  if(missing(survey_index)|missing(survey_name)==TRUE){
    title = paste0("ROC: Indicators vs ", stk_status," (",species_name,")
  ", min(yrs), "-", max(yrs), " (Q", qrs_text, ")")
  }
  title = paste0("True Skill Score: Indicators vs ", stk_status," (",species_name,")
  Survey Index: ", survey_index, ", Survey: ", survey_name, " (", min(yrs), "-", max(yrs), ", Q", qrs_text, ")")
  
  subtitle = paste0("Stock: ", stk_name)
  
  title(main = title,
        cex.main = 1.8,
        adj = 0,
        line = 1)
  title(sub = subtitle,
        cex.sub = 1.5,
        adj = 0.95,
        line = -2)
  #mtext(side=3, line=2, at=-0.07, adj= 0.1, cex=1.8, mytitle)
  #mtext(side=3, line=1, at=-0.07, adj= 0.1, cex=1.5, mysubtitle)
  
  #main = paste0("ROC: Indicators vs ", stk_status," (",species$spcs,")"),
  #sub = paste0("Stock: ", names(stk)))
  
  for(stk_index in colnames(SpatInds[match("Gini Index", colnames(SpatInds)):length(colnames(SpatInds))])){
    par(mar = c(5,5,3,18), xpd = T)
    
    # Create dataset with all spatial indicators and true stock status
    all_data <- Reduce(function(x, y) merge(x, y, by = "Year"), list(SpatInds, StatusInds))
    
    all_data[ncol(all_data)+1] <- all_data[stk_index]  # copies indicator to end column
    colnames(all_data)[ncol(all_data)] <- "ind"        # changes name of end column
    all_data[ncol(all_data)+1] <- all_data[stk_status] # copies status to end column
    colnames(all_data)[ncol(all_data)] <- "state"      # changes name of end column
    
    # call to roc_fun
    roc <- roc_fun2(all_data, 
                   state = state, 
                   ind = ind)
  
    # retrieving the optimum spatial indicator threshold
    opt_thresh <- order(roc[,"TSS"],decreasing = T)[1]
    opt_threshx <- as.data.frame(roc[opt_thresh, "ind"])
    opt_threshy <- round(as.data.frame(roc[opt_thresh, "TSS"]),2)
    
    lines(roc$ind, roc$TSS, type = "l", col = cols, lwd = 2)
    legend(legend = stk_index, 
           fill = cols, 
           x = 1.04, 
           y = stack, 
           cex = 1.5, 
           bty = "n", 
           inset = c(-0.2,0))
    
    par(xpd = F)
    # plot the optimum thrreshold
    abline(v = opt_threshx, col = alpha(cols, 0.5), lty = 2, alpha = 0.5)
    # add optimum threshold value
    text(opt_threshx, opt_threshy+0.06, labels = opt_threshy)
    # place a point at the optimum threshold
    points(opt_threshx, opt_threshy, labels = opt_threshy, pch = 19)
    
    # manual legend title
    mtext(text = "Spatial Indicator", line = -4, at = 1.2, cex = 1.5)
    # change colour for each spatial indicator
    cols <- cols+1
    # adjust poisiton of each legend output manually
    stack <- stack-0.12
  }
  return(p)
}                     
