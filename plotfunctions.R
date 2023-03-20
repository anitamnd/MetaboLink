##
#   Plotfunctions for omicsapp
##

makedriftplot <- function(data = NULL, seq = NULL, data2 = NULL, seq2 = NULL, plottype, featureselect = NULL, compare = F) {
  if(plottype == 1 && !is.null(featureselect)) {
    values <- as.data.frame(t(data[, seq[, 1] %in% c("QC", "Sample")]))
    sequence <- seq[seq[, 1] %in% c("QC", "Sample"), 3]
    label <- seq[seq[, 1] %in% c("QC", "Sample"), 1]
    plotdata <- data.frame(sequence, label, values)
    plotdata <- plotdata[order(plotdata$sequence), ]
    if(compare) {
      values <- as.data.frame(t(data2[, seq2[, 1] %in% c("QC","Sample")]))
      sequence <- seq2[seq2[, 1] %in% c("QC", "Sample"), 3]
      label <- seq2[seq2[, 1] %in% c("QC", "Sample"), 1]
      plotdata2 <- data.frame(sequence, label, values)
      plotdata2 <- plotdata2[order(plotdata2$sequence), ]
    }
    p <- list()
#    for(i in seq(length(featureselect))){
#      p[[i]] <- print(ggplot(plotdata, mapping = aes(x = sequence, y = plotdata[,featureselect[i]+2])) +
#        geom_point(aes(color = label), size = 3.5) +
#        geom_smooth(data = plotdata[plotdata$label %in% "QC",], mapping = aes(x = sequence, y = plotdata[plotdata$label %in% "QC",featureselect[i]+2]), color = "black", alpha = 0.2))
#    }

      p <- lapply(seq(length(featureselect)), function(x) {
        print(ggplot(plotdata, mapping = aes(x = sequence, y = plotdata[, featureselect[x] + 2])) +
                        geom_point(aes(color = label), size = 3.5) +
                        geom_smooth(data = plotdata[plotdata$label %in% "QC", ], mapping = aes(x = sequence, y = plotdata[plotdata$label %in% "QC", featureselect[x] + 2]), color = "black", alpha = 0.2))
      })

    do.call(grid.arrange, c(p, ncol = 1))
    #grid.arrange(p[[1]],p[[2]])
  } else if (plottype == 2) {
    return(plot(11:20, 11:20))
  } else if (plottype == 3) {
    return(plot(1:10, 1:10))
  }
}

driftplot <- function(data, seq) {
  values <- as.data.frame(t(data[seq[, 1] %in% c("QC", "Sample")]))
  sequence <- seq[seq[, 1] %in% c("QC", "Sample"), 3]
  label <- seq[seq[, 1] %in% c("QC", "Sample"), 1]
  plotdata <- data.frame(sequence, label, values)
  plotdata <- plotdata[order(plotdata$sequence), ]
  ggplot(plotdata, mapping = aes(x = sequence, y = plotdata[, 3])) +
    geom_point(aes(color = label), size = 3.5) +
    geom_smooth(data = plotdata[plotdata$label %in% "QC",],
                mapping = aes(x = sequence, y = plotdata[plotdata$label %in% "QC", 3]),
                color = "black", alpha = 0.2,
                method = "loess",
                formula = "y ~ x")
}

myboxplot <- function(data, seq, log, ylog) {
  values <- as.data.frame(t(data[seq[, 1] %in% c("QC", "Sample")]))
  if (log == "ln") {
    values[values == 0] <- NA
    values <- log(values)
  } else if(log == "log2") {
    values[values == 0] <- NA
    values <- log2(values)
  } else if(log == "log10") {
    values[values == 0] <- NA
    values <- log10(values)
  }

  seq[seq[, 1] %in% "QC", 4] <- "QC"
  class <- seq[seq[, 1] %in% c("QC", "Sample"), 4]
  plotdata <- data.frame(values, class)
  colnames(plotdata)[1] <- "values"
  plot <- ggplot(plotdata, aes(x = class, y = values, fill = factor(class))) +
    geom_boxplot() +
    theme(legend.position = "none") +
    labs(y = "Intensity",
         title = data[seq[, 1] %in% "Name"])
  if(ylog == "log2") {
    plot <- plot + scale_y_continuous(trans = 'log2')
  } else if (ylog == "log10") {
    plot <- plot + scale_y_continuous(trans = 'log10')
  }
  plot
}

cvscatterplot <- function(data1, data2, seq, name1, name2) {
  cv1 <- cv(data1[, seq[, 1] %in% c("QC")])
  cv2 <- cv(data2[, seq[, 1] %in% c("QC")])
  diff <- cv1 - cv2
  cvdat <- data.frame(cv1, cv2, diff)

  ggplot(data = cvdat, mapping = aes(x = cv2, y = cv1)) +
    geom_point(aes(color = after_stat(diff > 0), size = 10)) +
    scale_color_manual(values = c("darkgreen", "red")) +
    geom_abline() +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_rect(fill = "white", colour = "black")) +
    guides(size = "none") +
    labs(x = paste0("CV of QC samples in ", name2),
         y = paste0("CV of QC samples in ", name1))
}