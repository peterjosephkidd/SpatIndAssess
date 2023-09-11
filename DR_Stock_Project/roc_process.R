#> Run singlesurvey script for:
#> had.27.46a20, NS-IBTS, Q1
#> 
stk <- had.27.46a20
yrs <- c(range(stk)["minyear"][[1]]:range(stk)["maxyear"][[1]])
np.hauls2 <- filter(np.hauls, Year %in% yrs)

# Spatial Indicator Plot ####
pred <- ggplot() + geom_line(data = np.hauls2, aes(x = Year, y = PosAreaH)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1,) +
  ylab("Positive Area (Haul)")

# Plot SSB/MSY Btrigger ####
strtyr <- min(yrs)
endyr <- max(yrs)

msybtrig_refpt <- allstk_refpts$MSY_Btrigger[allstk_refpts$stk_name == stk.chr]
stkssb <- as.data.frame(ssb(stk)[,ac(strtyr:endyr)])[c("year", "data")] %>%
  rename(Year = year, SSB = data) %>%
  mutate(type = "SSB")
stkssb$ssb.msybtrig <- stkssb$SSB/msybtrig_refpt

ssb_plot <- ggplot() + geom_line(data = stkssb, aes(x = Year, y = ssb.msybtrig), colour = "black") +
  geom_hline(yintercept = 1, colour = "grey20", lty = 2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.3),
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("SSB/MSY Btrigger")

# Get ROC stuff ####
sissb$StockID <- "had.27.46a20"
sissb$SurveyIndex <- "Q1"
sissb$Survey <- "NS-IBTS"
sissb$`Survey Index, Survey Name` <- paste0(sissb$SurveyIndex, ", ", sissb$Survey)
rocdata <- roc_fun3(sissb, state = "ssb.msybtrig", inds = "PosAreaH")

# Plot SI against SSB ####
sissb <- merge(np.hauls2, stkssb, by = "Year")
sissb <- sissb %>%
  mutate(colour = if_else(ssb.msybtrig >= 1, "limegreen", "red"))
npos <- nrow(sissb[sissb$colour == "limegreen",])
nneg <- nrow(sissb[sissb$colour == "red",])


sissb_plot <- ggplot() + geom_point(data = sissb, aes(x = ssb.msybtrig, y = PosAreaH, colour = colour)) +
  scale_colour_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  annotate("text", label = paste0("n = ", nneg), x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = max(sissb$PosAreaH)) +
  annotate("text", label = paste0("n = ", npos), x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = max(sissb$PosAreaH)) +
  
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("Positive Area (Haul)") +
  xlab("SSB/MSY Btrigger")

## Plot with min threshold ####
thresh <- sort(sissb$PosAreaH)[1]-0.0001
sissb1 <- sissb %>%
  mutate(shape1 = if_else(PosAreaH < thresh & colour == "red", 15, 0),
         shape2 = if_else(PosAreaH >= thresh & colour == "limegreen", 15, 0),
         shape = shape1+shape2+1)

tp <- paste0("TP = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]))
tn <- paste0("TN = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]))
fp <- paste0("FN = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]))
fn <- paste0("FP = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]))
tpr <- paste0("TPR = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), "/", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), " + ",  nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]), " = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3))
fpr <- paste0("FPR = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), "/", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), " + ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]), " = ", round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])), 3))
tss <- paste0("TSS = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3), " - ",  round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3), " = ", round(round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3) - round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3),3))

threshmin <- ggplot() + geom_point(data = sissb1, aes(x = ssb.msybtrig, y = PosAreaH, colour = colour, shape = shape)) +
  scale_colour_identity() +
  scale_shape_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  geom_hline(yintercept = thresh, colour = "blue", lty = 1, size = 1, alpha = 0.8) +
  annotate("text", label = fn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = min(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = fp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = min(sissb$PosAreaH), size = 3.2) +
  #annotate("text", label = tpr, x = min(sissb$ssb.msybtrig), y = max(sissb$PosAreaH) + 0.2, size = 3.2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) + 
  labs(title  = "Min", caption = paste0(tpr, "\n", fpr, "\n", tss)) +
  ylab("Positive Area (Haul)") +
  xlab("SSB/MSY Btrigger")

## Plot 2nd threshold ####
thresh <- sort(sissb$PosAreaH)[2]-0.0001
sissb1 <- sissb %>%
  mutate(shape1 = if_else(PosAreaH < thresh & colour == "red", 15, 0),
         shape2 = if_else(PosAreaH >= thresh & colour == "limegreen", 15, 0),
         shape = shape1+shape2+1)

tp <- paste0("TP = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]))
tn <- paste0("TN = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]))
fp <- paste0("FN = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]))
fn <- paste0("FP = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]))
tpr <- paste0("TPR = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), "/", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), " + ",  nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]), " = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3))
fpr <- paste0("FPR = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), "/", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), " + ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]), " = ", round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])), 3))
tss <- paste0("TSS = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3), " - ",  round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3), " = ", round(round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3) - round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3),3))

thresh2 <- ggplot() + geom_point(data = sissb1, aes(x = ssb.msybtrig, y = PosAreaH, colour = colour, shape = shape)) +
  scale_colour_identity() +
  scale_shape_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  geom_hline(yintercept = thresh, colour = "blue", lty = 1, size = 1, alpha = 0.8) +
  annotate("text", label = fn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = min(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = fp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = min(sissb$PosAreaH), size = 3.2) +
  #annotate("text", label = tpr, x = min(sissb$ssb.msybtrig), y = max(sissb$PosAreaH) + 0.2, size = 3.2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1,
        plot.subtitle = element_text(tpr)) +
  labs(title = "2nd", caption = paste0(tpr, "\n", fpr, "\n", tss)) +
  ylab("") +
  xlab("SSB/MSY Btrigger")

## Plot 3rd threshold ####
thresh <- sort(sissb$PosAreaH)[3]-0.0001
sissb1 <- sissb %>%
  mutate(shape1 = if_else(PosAreaH < thresh & colour == "red", 15, 0),
         shape2 = if_else(PosAreaH >= thresh & colour == "limegreen", 15, 0),
         shape = shape1+shape2+1)

tp <- paste0("TP = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]))
tn <- paste0("TN = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]))
fp <- paste0("FN = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]))
fn <- paste0("FP = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]))
tpr <- paste0("TPR = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), "/", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), " + ",  nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]), " = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3))
fpr <- paste0("FPR = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), "/", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), " + ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]), " = ", round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])), 3))
tss <- paste0("TSS = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3), " - ",  round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3), " = ", round(round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3) - round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3),3))



thresh3 <- ggplot() + geom_point(data = sissb1, aes(x = ssb.msybtrig, y = PosAreaH, colour = colour, shape = shape)) +
  scale_colour_identity() +
  scale_shape_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  geom_hline(yintercept = thresh, colour = "blue", lty = 1, size = 1, alpha = 0.8) +
  annotate("text", label = fn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = min(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = fp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = min(sissb$PosAreaH), size = 3.2) +
  #annotate("text", label = tpr, x = min(sissb$ssb.msybtrig), y = max(sissb$PosAreaH) + 0.2, size = 3.2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1,
        plot.subtitle = element_text(tpr)) +
  labs(title = "3rd", caption = paste0(tpr, "\n", fpr, "\n", tss)) +
  ylab("") +
  xlab("SSB/MSY Btrigger")

## Plot 10th threshold ####
thresh <- sort(sissb$PosAreaH)[10]-0.0001
sissb1 <- sissb %>%
  mutate(shape1 = if_else(PosAreaH < thresh & colour == "red", 15, 0),
         shape2 = if_else(PosAreaH >= thresh & colour == "limegreen", 15, 0),
         shape = shape1+shape2+1)

tp <- paste0("TP = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]))
tn <- paste0("TN = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]))
fp <- paste0("FN = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]))
fn <- paste0("FP = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]))
tpr <- paste0("TPR = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), "/", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), " + ",  nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]), " = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3))
fpr <- paste0("FPR = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), "/", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), " + ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]), " = ", round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])), 3))
tss <- paste0("TSS = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3), " - ",  round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3), " = ", round(round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3) - round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3),3))

thresh10 <- ggplot() + geom_point(data = sissb1, aes(x = ssb.msybtrig, y = PosAreaH, colour = colour, shape = shape)) +
  scale_colour_identity() +
  scale_shape_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  geom_hline(yintercept = thresh, colour = "blue", lty = 1, size = 1, alpha = 0.8) +
  annotate("text", label = fn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = min(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = fp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = min(sissb$PosAreaH), size = 3.2) +
  #annotate("text", label = tpr, x = min(sissb$ssb.msybtrig), y = max(sissb$PosAreaH) + 0.2, size = 3.2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1,
        plot.subtitle = element_text(tpr)) +
  labs(title =  "10th", caption = paste0(tpr, "\n", fpr, "\n", tss)) +
  ylab("") +
  xlab("SSB/MSY Btrigger")

## Plot 30th threshold ####
thresh <- sort(sissb$PosAreaH)[30]-0.0001
sissb1 <- sissb %>%
  mutate(shape1 = if_else(PosAreaH < thresh & colour == "red", 15, 0),
         shape2 = if_else(PosAreaH >= thresh & colour == "limegreen", 15, 0),
         shape = shape1+shape2+1)

tp <- paste0("TP = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]))
tn <- paste0("TN = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]))
fp <- paste0("FN = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]))
fn <- paste0("FP = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]))
tpr <- paste0("TPR = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), "/", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), " + ",  nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]), " = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3))
fpr <- paste0("FPR = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), "/", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), " + ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]), " = ", round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])), 3))
tss <- paste0("TSS = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3), " - ",  round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3), " = ", round(round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3) - round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3),3))

thresh30 <- ggplot() + geom_point(data = sissb1, aes(x = ssb.msybtrig, y = PosAreaH, colour = colour, shape = shape)) +
  scale_colour_identity() +
  scale_shape_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  geom_hline(yintercept = thresh, colour = "blue", lty = 1, size = 1, alpha = 0.8) +
  annotate("text", label = fn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = min(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = fp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = min(sissb$PosAreaH), size = 3.2) +
  #annotate("text", label = tpr, x = min(sissb$ssb.msybtrig), y = max(sissb$PosAreaH) + 0.2, size = 3.2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1,
        plot.subtitle = element_text(tpr)) +
  labs(title = "30th", caption = paste0(tpr, "\n", fpr, "\n", tss)) +
  ylab("") +
  xlab("SSB/MSY Btrigger")

## Plot last threshold ####
thresh <- sort(sissb$PosAreaH)[length(sissb$PosAreaH)]+0.0001
sissb1 <- sissb %>%
  mutate(shape1 = if_else(PosAreaH < thresh & colour == "red", 15, 0),
         shape2 = if_else(PosAreaH >= thresh & colour == "limegreen", 15, 0),
         shape = shape1+shape2+1)

tp <- paste0("TP = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]))
tn <- paste0("TN = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]))
fp <- paste0("FN = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]))
fn <- paste0("FP = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]))
tpr <- paste0("TPR = ", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), "/", nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]), " + ",  nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,]), " = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3))
fpr <- paste0("FPR = ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), "/", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]), " + ", nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,]), " = ", round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])), 3))
tss <- paste0("TSS = ", round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3), " - ",  round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3), " = ", round(round(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,])/(nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 16,]) + nrow(sissb[sissb1$colour == "limegreen" & sissb1$shape == 1,])),3) - round(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,])/(nrow(sissb[sissb1$colour == "red" & sissb1$shape == 1,]) + nrow(sissb[sissb1$colour == "red" & sissb1$shape == 16,])),3),3))

threshmax <- ggplot() + geom_point(data = sissb1, aes(x = ssb.msybtrig, y = PosAreaH, colour = colour, shape = shape)) +
  scale_colour_identity() +
  scale_shape_identity() +
  geom_vline(xintercept = 1, colour = "grey20", lty = 2) +
  geom_hline(yintercept = thresh, colour = "blue", lty = 1, size = 1, alpha = 0.8) +
  geom_hline(yintercept = 0.6582633, colour = "blue", lty = 2) +
  annotate("text", label = fn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = max(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = tn, x = median(sissb$ssb.msybtrig[sissb$colour == "red"]), y = min(sissb$PosAreaH), size = 3.2) +
  annotate("text", label = fp, x = median(sissb$ssb.msybtrig[sissb$colour == "limegreen"]) + 0.4, y = min(sissb$PosAreaH), size = 3.2) +
  #annotate("text", label = tpr, x = min(sissb$ssb.msybtrig), y = max(sissb$PosAreaH) + 0.2, size = 3.2) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1,
        plot.subtitle = element_text(tpr)) +
  labs(title = "Max", caption = paste0(tpr, "\n", fpr, "\n", tss)) +
  ylab("") +
  xlab("SSB/MSY Btrigger")

# ROC Curve ####
sissb$StockID <- "had.27.46a20"
sissb$SurveyIndex <- "Q1"
sissb$Survey <- "NS-IBTS"
sissb$`Survey Index, Survey Name` <- paste0(sissb$SurveyIndex, ", ", sissb$Survey)
rocdata <- roc_fun3(sissb, state = "ssb.msybtrig", inds = "PosAreaH")

## ROC1 ####
roc1 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = FPR, y = TPR), colour = "blue") +
  geom_abline(intercept = 0, slope = 1) +
  #geom_path(data = rocdata[1,], aes(x = FPR, y = TPR), colour = "blue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("True Positive Rate") + 
  xlab("False Positive Rate") 

## ROC2 ####
roc2 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = FPR, y = TPR), colour = "black") +
  geom_point(data = rocdata[2,], aes(x = FPR, y = TPR), colour = "blue") +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = rocdata[1:2,], aes(x = FPR, y = TPR), colour = "blue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("False Positive Rate") 

## ROC3 ####
roc3 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = FPR, y = TPR), colour = "black") +
  geom_point(data = rocdata[3,], aes(x = FPR, y = TPR), colour = "blue") +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = rocdata[1:3,], aes(x = FPR, y = TPR), colour = "blue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("False Positive Rate") 
  
## ROC10 ####
roc10 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = FPR, y = TPR), colour = "black") +
  geom_point(data = rocdata[10,], aes(x = FPR, y = TPR), colour = "blue") +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = rocdata[1:10,], aes(x = FPR, y = TPR), colour = "blue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
         ylab("") + 
         xlab("False Positive Rate")

## ROC30 ####
roc30 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = FPR, y = TPR), colour = "black") +
  geom_point(data = rocdata[30,], aes(x = FPR, y = TPR), colour = "blue") +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = rocdata[1:30,], aes(x = FPR, y = TPR), colour = "blue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("False Positive Rate")

## ROCfull ####
optthresh <- rocdata[rocdata$TSS==max(rocdata$TSS),]
  
rocfull <- ggplot() +
  #geom_point(data = rocdata, aes(x = FPR, y = TPR), colour = "blue") +
  geom_point(data = rocdata[nrow(rocdata),], aes(x = FPR, y = TPR), colour = "blue") +
  geom_point(data = rocdata[1,], aes(x = FPR, y = TPR), colour = "black") +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = rocdata, aes(x = FPR, y = TPR), colour = "blue") +
  geom_point(data = optthresh, aes(x = FPR, y = TPR), fill = "blue", colour = "black", size = 2, shape = 22) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("False Positive Rate") 

# TSS ####
rocdata$`Spatial Indicator Value` <- as.numeric(rocdata$`Spatial Indicator Value`)
rocdata$TSS <- as.numeric(rocdata$TSS)

## TSS1 ####
tss1 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  #geom_path(data = rocdata[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1,1), xlim = c(0.53,0.85)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  xlab("Indicator Value") + 
  ylab("True Skill Score") 

## TSS2 ####
tss2 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "black") +
  geom_point(data = rocdata[2,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_line(data = rocdata[1:2,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1,1), xlim = c(0.53,0.85)) +
  #scale_x_continuous(limits = c(0,1))
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("Indicator Value") 

## TSS3 ####
tss3 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "black") +
  geom_point(data = rocdata[3,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_line(data = rocdata[1:3,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1,1), xlim = c(0.53,0.85)) +
  #scale_x_continuous(limits = c(0,1))
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("Indicator Value") 

## TSS10 ####
tss10 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "black") +
  geom_point(data = rocdata[10,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_line(data = rocdata[1:10,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1,1), xlim = c(0.53,0.85)) +
  #scale_x_continuous(limits = c(0,1))
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("Indicator Value") 

## TSS30 ####
tss30 <- ggplot() +
  geom_point(data = rocdata[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "black") +
  geom_point(data = rocdata[30,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_line(data = rocdata[1:30,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1,1), xlim = c(0.53,0.85)) +
  #scale_x_continuous(limits = c(0,1))
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("Indicator Value") 

## TSSfull ####
optthresh$`Spatial Indicator Value` <- as.numeric(optthresh$`Spatial Indicator Value`)
optthresh$TSS <- as.numeric(optthresh$TSS)

tssfull <- ggplot() +
  #geom_point(data = rocdata, aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_point(data = rocdata[1,], aes(x = `Spatial Indicator Value`, y = TSS), colour = "black") +
  geom_point(data = rocdata[nrow(rocdata),], aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_line(data = rocdata, aes(x = `Spatial Indicator Value`, y = TSS), colour = "blue") +
  geom_point(data = optthresh, aes(x = `Spatial Indicator Value`, y = TSS), colour = "black", fill = "blue", size = 2, shape = 22) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1,1), xlim = c(0.53,0.85)) +
  #scale_x_continuous(limits = c(0,1))
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("Indicator Value") 




# Grid ####
g1 <- gridExtra::grid.arrange(threshmin, thresh2, thresh3, thresh10, thresh30, threshmax, nrow = 1)
g2 <- gridExtra::grid.arrange(roc1, roc2, roc3, roc10, roc30, rocfull, nrow = 1)
g3 <- gridExtra::grid.arrange(tss1, tss2, tss3, tss10, tss30, tssfull, nrow = 1)
g4 <- gridExtra::grid.arrange(g1, g2, g3, ncol = 1)
g5 <- gridExtra::grid.arrange(pred, ssb_plot, ncol = 1)
g6 <- gridExtra::grid.arrange(g5,sissb_plot,g4, ncol = 6, nrow = 1)

g1 <- gridExtra::grid.arrange(threshmin, roc1, tss1, ncol = 1)
g2 <- gridExtra::grid.arrange(thresh2, roc2, tss2, ncol = 1)
g3 <- gridExtra::grid.arrange(thresh3, roc3, tss3, ncol = 1)
g10 <- gridExtra::grid.arrange(thresh10, roc10, tss10, ncol = 1)
g30 <- gridExtra::grid.arrange(thresh30, roc30, tss30, ncol = 1)
gfull <- gridExtra::grid.arrange(threshmax, rocfull, tssfull, ncol = 1)
g1a <- gridExtra::grid.arrange(pred, ssb_plot, NULL, ncol = 1)
g1b <- gridExtra::grid.arrange(sissb_plot, NULL, NULL, ncol = 1)

g <- gridExtra::grid.arrange(g1a, g1b, g1, g2, g3, g10, g30, gfull, ncol = 8, nrow = 1)

g2a <- gridExtra::grid.arrange(pred, ssb_plot, ncol = 1)
g2b <- gridExtra::grid.arrange(g2a, sissb_plot, ncol =2, nrow = 1)
g2b1 <- gridExtra::grid.arrange(g2a, sissb_plot, g1, g2, g3, g10, g30, gfull, ncol = 8, nrow = 1)
g2c <- gridExtra::grid.arrange(g1, g2, g3, g10, g30, gfull, ncol = 6, nrow = 1)

cowplot::save_plot(plot = g2b, filename = paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Process/", "left.png"), base_height = 6, base_width = 8)

p1 <- (threshmin| thresh2| thresh3| thresh10| thresh30| threshmax)/
  (roc1| roc2| roc3| roc10| roc30| rocfull) / 
  (tss1| tss2| tss3| tss10| tss30| tssfull)
cowplot::save_plot(plot = p1, filename = paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Process/", "right.png"), base_height = 6, base_width = 12)



install.packages('patchwork')
library(patchwork)



p2 <- (pred)/
      (plot_spacer() | sissb_plot)/
      (ssb_plot)





plot_grid(pred, NULL, threshmin, thresh2, thresh3, thresh10, thresh30, threshmax, NULL, sissb_plot, roc1, roc2, roc3, roc10, roc30, rocfull, ssb_plot, NULL, tss1, tss2, tss3, tss10, tss30, tssfull, ncol = 8, nrow = 3)


## Perf ####
tpr1 <- c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1)
fpr1 <- c(0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 1)
d <- as.data.frame(cbind(tpr1, fpr1))

rocperf <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 0.6) +
  annotate("text", label = "AUC = 1", x = 0.125, y = 0.875, size = 2.75) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("True Positive Rate") + 
  xlab("False Positive Rate") 

## Random ####
tpr1 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
fpr1 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
d <- as.data.frame(cbind(tpr1, fpr1))

rocrand <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 0.6) +
  annotate("text", label = "AUC = 0.5", x = 0.125, y = 0.875, size = 2.75) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("False Positive Rate") 

## Imperf #### 
fpr1 <- c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1)
tpr1 <- c(0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 1)

d <- as.data.frame(cbind(tpr1, fpr1))

rocimp <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 0.6) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  annotate("text", label = "AUC = 0", x = 0.125, y = 0.875, size = 2.75) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("False Positive Rate") 

exampleroc <- plot_grid(rocperf, rocrand, rocimp, nrow = 1, labels = c("(a)", "(b)", "(c)"))

cowplot::save_plot(plot = exampleroc, filename = paste0("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/DATRAS/Spatial Indicator R Project/ROC/Process/", "exampleroc.png"), base_height = 6, base_width = 8)

