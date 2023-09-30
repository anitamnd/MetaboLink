checkseq <- function(seq) {
  columns_to_check <- c("sample", "batch", "order", "class", "time", "paired")
  missing_columns <- setdiff(columns_to_check, colnames(seq))
  # If any of the columns are missing, add them as empty columns
  if (length(missing_columns) > 0) {
    seq[missing_columns] <- lapply(1:length(missing_columns), function(x) seq[missing_columns[x]] <- NA )
  }
  seq <- seq[, c("sample", "batch", "order", "class", "time", "paired")]
  return(seq)
}

blankfiltration <- function(dat, seq, xbf, keepis) {
  dat[seq[, 1] %in% "Blank"][is.na(dat[seq[, 1] %in% "Blank"])] <- 0
  bf <- apply(dat[seq[, 1] %in% "Blank"], 1, mean) * xbf < apply(dat[seq[, 1] %in% "QC"], 1, mean, na.rm = TRUE)
  if (keepis) {
    is <- grepl("\\(IS\\)", toupper(dat[seq[, 1] %in% "Name"][, 1]))
    dat <- dat[bf | is, ]
  } else {
    dat <- dat[bf, ]
  }
  return(dat)
}

isfunc <- function(dat, seq, is, method, qc) {
  rt <- which(seq[, 1] == "RT")
  isname <- is
  is <- as.numeric(gsub(" .*$", "", is))
  sel <- if (qc) c("Sample", "QC") else "Sample"
  sdat <- dat[seq[, 1] %in% sel]
  sdat[sdat == 0] <- NA
  is <- is[complete.cases(sdat[is, ])]
  near <- sapply(dat[, rt], function(y) {
    which.min(abs(dat[is, rt] - y))
  })
  if (method == "Same lipid structure") {
    name <- dat[seq[, 1] %in% "Name"]
    istype <- gsub(" .*$", "", name[is, ])
    near <- sapply(seq(name[, 1]), function(x) {
      if (gsub(" .*$", "", name[x, 1]) %in% istype) {
        which(istype %in% gsub(" .*$", "", name[x, 1]))
      } else {
        near[x]
      }
    })
  }
  sdat <- sapply(seq(ncol(sdat)), function(j) {
    sapply(seq(nrow(sdat)), function(i) {
      sdat[i, j] <- sdat[i, j] / sdat[is, j][near[i]]
    })
  })
  isnorm <- sapply(seq(nrow(sdat)), function(x) {
    isname[near[x]]
  })
  dat[seq[, 1] %in% sel] <- sdat
  dat <- cbind(dat, data.frame(isnorm = isnorm))
  return(dat)
}

isopti <- function(dat, seq, is, method, qc) {
  iscomb <- Map(combn, list(is), seq_along(is), simplify = FALSE)
  iscomb <- lapply(rapply(iscomb, enquote, how = "unlist"), eval)
  progressSweetAlert(id = "pbis", title = "Finding best IS combination", value = 0, total = length(iscomb), striped = T, display_pct = T)
  islow <- lapply(iscomb, function(x) {
    isdat <- isfunc(dat, seq, x, method, qc)
    mean(apply(isdat[, seq[, 1] %in% "QC"], 1, sd, na.rm = T) / apply(isdat[, seq[, 1] %in% "QC"], 1, mean, na.rm = T) * 100)
    # updateProgressBar(id = "pbis", value = which(iscomb %in% iscomb[x]), total = length(iscomb))
  })
  closeSweetAlert()
  return(unlist(iscomb[which.min(unlist(islow))]))
}

findis <- function(dat) {
  nr <- grepl("\\(IS\\)", toupper(dat[, 1]))
  if (sum(nr) > 0) {
    name <- as.vector(dat[nr, 1])
    is <- paste(which(nr), " - ", name)
    return(is)
  } else {
    return(character(0))
  }
}

identifylabels <- function(data) {
  lab <- sapply(names(data), function(x) {
    if (grepl("BLANK", toupper(x), fixed = TRUE) && is.numeric(data[, x])) {
      "Blank"
    } else if (grepl("QC", toupper(x), fixed = TRUE) && is.numeric(data[, x])) {
      "QC"
    } else if (grepl("NAME", toupper(x), fixed = TRUE)) {
      "Name"
    } else if (grepl("MASS|M/Z|M.Z", toupper(x)) && is.numeric(data[, x])) {
      "Mass"
    } else if (grepl("RT|TIME|RETENTION", toupper(x)) && is.numeric(data[, x])) {
      "RT"
    } else if (grepl("[[:digit:]]", toupper(x)) && is.numeric(data[, x])) {
      "Sample"
    } else {
      "-"
    }
  })
  if (sum(lab == "Name") > 1) {
    lab[lab == "Name" & duplicated(lab)] <- "-"
  }
  lab <- factor(lab, levels = c("Name", "Blank", "QC", "Sample", "RT", "Mass", "Adduct_pos", "Adduct_neg", "-"))
  return(lab)
}

imputation <- function(dat, seq, method, minx = 1, onlyqc, remaining) {
  if (onlyqc) {
    datsq <- dat[seq[, 1] %in% "QC"]
    seqsq <- seq[seq[, 1] %in% "QC", ]
  } else {
    datsq <- dat[seq[, 1] %in% c("Sample", "QC")]
    seqsq <- seq[seq[, 1] %in% c("Sample", "QC"), ]
  }
  datsq[datsq == 0] <- NA
  seqsq[seqsq[, 1] %in% c("QC"), ]$class <- "QC"
  seqsq$class[is.na(seqsq$class)] <- "Sample"

  if (method == "KNN") {
    datsq <- as.matrix(datsq)
    knndat <- impute.knn(datsq, k = 10, rowmax = .99, colmax = .99, maxp = 15000) # TODO skal k sættes anderledes evt mindste class -1?
    impsqdat <- as.data.frame(knndat$data)
  } else if (method == "Median") {
    impsqdat <- imp_median(datsq, seqsq)
  } else if (method == "Min/X") {
    impsqdat <- imp_minx(datsq, seqsq, minx)
  }

  if (sum(is.na(impsqdat)) > 0) {
    if (remaining == "Min/X") {
      for (i in 1:nrow(impsqdat)) {
        impsqdat[i, is.na(impsqdat[i, ])] <- min(impsqdat[i, ], na.rm = T) / minx
      }
    } else if (remaining == "zero") {
      for (i in 1:nrow(impsqdat)) {
        impsqdat[i, is.na(impsqdat[i, ])] <- 0
      }
    } else if (remaining == "Median") {
      for (i in 1:nrow(impsqdat)) {
        impsqdat[i, is.na(impsqdat[i, ])] <- median(as.numeric(impsqdat[i, ]), na.rm = T)
      }
    }
  }
  if (onlyqc) {
    dat[seq[, 1] %in% "QC"] <- impsqdat
  } else {
    dat[seq[, 1] %in% c("Sample", "QC")] <- impsqdat
  }
  return(dat)
}

cutoffrm <- function(dat, seq, cutoff, method) {
  cutoff <- cutoff / 100
  if ("entire data" %in% method) {
    datm <- dat[seq[, 1] %in% "Sample"]
    datm[datm == 0] <- NA
    keep <- rowSums(!is.na(datm)) / ncol(datm) >= cutoff
  }
  if ("in QC" %in% method) {
    datm <- dat[seq[, 1] %in% "QC"]
    datm[datm == 0] <- NA
    keep <- rowSums(!is.na(datm)) / ncol(datm) >= cutoff
  }
  if ("in group" %in% method) {
    datm <- dat[seq[, 1] %in% "Sample"]
    datm[datm == 0] <- NA
    classes <- factor(seq[, 4], exclude = NA)
    nseq <- seq[seq[, 1] %in% "Sample", ]
    keep_m <- matrix(FALSE, nrow(datm), ncol = length(levels(classes)))
    for (cl in 1:length(levels(classes))) {
      cl_f <- datm[, nseq[, 4] %in% levels(classes)[cl]]
      keep_m[, cl] <- rowSums(!is.na(cl_f)) / ncol(cl_f) >= cutoff
    }
    keep <- apply(keep_m, 1, function(x) any(x))
  }
  dat <- dat[keep, ]
  return(dat)
}

imp_median <- function(dat, seq) {
  datm <- data.frame(seq$class, t(dat))
  datm <- datm %>%
    group_by(seq.class) %>%
    mutate_if(
      is.numeric,
      function(x) {
        ifelse(is.na(x), median(x, na.rm = T), x)
      }
    )
  datm <- data.frame(t(datm[, -1]))
  colnames(datm) <- colnames(dat)
  row.names(datm) <- row.names(dat)
  return(datm)
}

imp_minx <- function(dat, seq, minx) {
  datm <- data.frame(seq$class, t(dat))
  datm <- datm %>%
    group_by(seq.class) %>%
    mutate_if(
      is.numeric,
      function(x) {
        ifelse(is.na(x),
          min(x, na.rm = T) / minx,
          x
        )
      }
    )
  datm <- data.frame(t(datm[, -1]))
  colnames(datm) <- colnames(dat)
  row.names(datm) <- row.names(dat)
  datm[datm == "Inf"] <- NA
  return(datm)
}

cvmean <- function(dat) {
  mean(apply(dat, 1, sd, na.rm = T) / apply(dat, 1, mean, na.rm = T), na.rm = T) * 100
}

cv <- function(dat) {
  round(apply(dat, 1, sd, na.rm = T) / apply(dat, 1, mean, na.rm = T) * 100, 2)
}

pcaplot <- function(data, class) {
  data[data == 0] <- NA
  data <- data[complete.cases(data), ]
  class[!is.na(class)] <- class[!is.na(class)]
  class[is.na(class)] <- "No class"
  prin <- prcomp(log(t(data)), rank. = 2, scale = F)
  pov <- summary(prin)[["importance"]]["Proportion of Variance", ]
  pov <- round(pov * 100, 2)
  components <- prin[["x"]]
  components <- data.frame(components)
  label <- paste0(row.names(components), ": ", class)
  col <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(unique(class)))
  pca <- plot_ly(components, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers", text = label, hoverinfo = "text", color = class, colors = col)
  pca <- pca %>% layout(
    legend = list(title = list(text = "color")),
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = paste0("PC1 (", pov[1], "% explained var.)"),
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor = "#ffff"
    ),
    yaxis = list(
      title = paste0("PC2 (", pov[2], "% explained var.)"),
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor = "#ffff"
    )
  )
  return(pca)
}

driftcorrection <- function(dat, seq, method, ntree = 500, degree = 2, QCspan) {
  seqsq <- seq[seq[, 1] %in% c("Sample", "QC"), ]
  datsq <- dat[, seq[, 1] %in% c("Sample", "QC")]
  datsqsorted <- datsq %>% select(order(seqsq$order))
  qcid <- as.numeric(sort(seqsq[seqsq[, 1] %in% "QC", ]$order))
  frame <- data.frame("qcid" = 1:ncol(datsq))
  dcdat <- as.matrix(datsqsorted)
  progressSweetAlert(id = "pbdc", title = "Correcting drift", value = 0, total = nrow(dcdat), striped = T, display_pct = T)
  if (method == "QC-RFSC (random forrest)") {
    for (i in 1:nrow(dcdat)) {
      forest <- randomForest(data.frame(qcid), as.numeric(dcdat[i, qcid]), ntree = ntree)
      pv <- predict(forest, frame)
      dcdat[i, ] <- as.numeric(dcdat[i, ]) / pv
      updateProgressBar(id = "pbdc", value = i, total = nrow(dcdat))
    }
  }
  if (method == "QC-RLSC (robust LOESS)") {
    for (i in 1:nrow(dcdat)) {
      loess <- loess(dcdat[i, qcid] ~ qcid,
        span = QCspan,
        degree = degree
      )
      pv <- predict(loess, frame)
      dcdat[i, ] <- as.numeric(dcdat[i, ]) / pv
      updateProgressBar(id = "pbdc", value = i, total = nrow(dcdat))
    }
  }
  dat[seq[, 1] %in% c("Sample", "QC")] <- dcdat[, seqsq$order]
  closeSweetAlert()
  return(dat)
}

findadduct <- function(dat, seq) {
  names <- dat[, seq[, 1] %in% "Name"]
  reg <- gregexpr("_\\[(.*?)_", names)
  adduct <- regmatches(names, reg)
  adduct <- as.character(adduct)
  adduct[adduct > 0] <- NA
  adduct <- gsub("^.|.$", "", adduct)
  adduct <- gsub(" ", "", adduct)
  return(adduct)
}

merge.func <- function(dat1, seq1, dat2, seq2, ppmtol, rttol) {
  progressSweetAlert(id = "pb", title = "Work in progress", display_pct = T, value = 0, striped = T)
  mass <- dat1[, seq1[, 1] %in% "Mass"]
  if ("Adduct_pos" %in% seq1[, 1]) {
    adduct <- dat1[, seq1[, 1] %in% "Adduct_pos"]
    ionmode1 <- "pos"
  } else {
    adduct <- dat1[, seq1[, 1] %in% "Adduct_neg"]
    ionmode1 <- "neg"
  }
  mass <- monomass(adduct, mass, ionmode1)
  rt <- dat1[, seq1[, 1] %in% "RT"]
  first <- data.frame(mass, rt)
  updateProgressBar(id = "pb", value = 10)
  mass <- dat2[, seq2[, 1] %in% "Mass"]
  if ("Adduct_pos" %in% seq2[, 1]) {
    adduct <- dat2[, seq2[, 1] %in% "Adduct_pos"]
    ionmode2 <- "pos"
  } else {
    adduct <- dat2[, seq2[, 1] %in% "Adduct_neg"]
    ionmode2 <- "neg"
  }
  mass <- monomass(adduct, mass, ionmode2)
  rt <- dat2[, seq2[, 1] %in% "RT"]
  second <- data.frame(mass, rt)
  updateProgressBar(id = "pb", value = 20)
  comb <- rbind(first, second)
  distmz <- as.matrix(dist(comb$mass))
  updateProgressBar(id = "pb", value = 30)
  v <- rep(comb$mass, each = dim(distmz)[1])
  updateProgressBar(id = "pb", value = 40)
  distp <- distmz / v
  updateProgressBar(id = "pb", value = 50)
  distppm <- distp * 10^6
  updateProgressBar(id = "pb", value = 60)
  distrt <- as.matrix(dist(comb$rt))
  updateProgressBar(id = "pb", value = 70)
  adj <- distppm <= ppmtol & distrt <= rttol
  updateProgressBar(id = "pb", value = 80)
  graph <- graph.adjacency(adj)
  updateProgressBar(id = "pb", value = 90)
  mergeid <- clusters(graph)$membership
  updateProgressBar(id = "pb", value = 100)
  colnames(dat2) <- colnames(dat1)
  combineddat <- as.data.frame(rbind(dat1, dat2))
  combineddat[, "mergeID"] <- mergeid
  ionmode1 <- rep(ionmode1, nrow(dat1))
  ionmode2 <- rep(ionmode2, nrow(dat2))
  combineddat[, "ionmode"] <- c(ionmode1, ionmode2)
  closeSweetAlert()
  return(combineddat)
}

monomass <- function(adduct, mz, ionmode) {
  masscorrection <- read.csv("./csvfiles/adducts.csv")
  monomass <- sapply(seq(adduct), function(i) {
    if (adduct[i] %in% masscorrection[, 1]) {
      j <- which(masscorrection[, 1] %in% adduct[i])
      cm <- masscorrection[j, 3]
      mass <- masscorrection[j, 2]
      monomass <- mz[i] * cm - mass
      return(monomass)
    } else if (ionmode == "pos") {
      return(mz[i] - 1.007276)
    } else {
      return(mz[i] + 1.007276)
    }
    return(monomass)
  })
}

finddup <- function(out_dup, rankings) {
  prio <- apply(out_dup, 1, function(x) {
    duplicaterank(x, rankings)
  })
  dup_prio <- cbind(out_dup, prio)
  rows <- NULL
  for (i in unique(dup_prio[, 2])) {
    lowp <- dup_prio[i == dup_prio[, 2], ][, 9] %in% min(dup_prio[i == dup_prio[, 2], ][, 9])
    mincv <- min(dup_prio[i == dup_prio[, 2], ][lowp, 8])
    keeprow <- rownames(dup_prio[i == dup_prio[, 2], ][lowp, ][dup_prio[i == dup_prio[, 2], ][lowp, 8] == mincv, ])
    keeprow <- keeprow[1]
    rows <- c(rows, keeprow)
  }
  numb <- which(rownames(out_dup) %in% rows)
  return(numb)
}

duplicaterank <- function(duplicate, rankings) {
  j <- sapply(1:10, function(x) {
    if (rankings[x, 1] == "") {
      FALSE
    } else {
      grepl(toupper(rankings[x, 1]), toupper(duplicate[5]))
    }
  })
  if (sum(j) > 0) {
    return(min(rankings[j, 2]))
  } else {
    return(10)
  }
}

# Statistics

addcols <- function(dat, seq, groups, maxreps) {
  tdat <- dat[,1]
  rgroup <- c("")
  rtime <- c("")
  for(group in 1:length(groups)) {
    groupdat <- dat[, seq[, 4] %in% groups[group]]
    time <- seq[seq[, 4] %in% groups[group], 5]
    tdat <- cbind(tdat, groupdat)
    if(length(groupdat) < maxreps) {
      fcol <- t(rep(NA, maxreps - length(groupdat)))
      tdat <- cbind(tdat, fcol)
    }
    rgroup <- append(rgroup, rep(paste("g", groups[group], sep = ""), maxreps))
    if(any(complete.cases(seq[, 5])))
      rtime <- append(rtime, t(time))
  }
  tdat <- rbind(rgroup, tdat)
  if(any(complete.cases(seq[, 5])))
    colnames(tdat) <- paste(colnames(tdat), rgroup, paste("t", rtime, sep=""), sep = "_")
  else
    colnames(tdat) <- paste(colnames(tdat), rgroup, sep = "_")
  return(tdat)
}

mrtrans <- function(dat, log, mean) {
  logn <- if (log) "LogNorm" else "NULL"
  meanc <- if (mean) "MeanCenter" else "NULL"

  write.csv(dat, file = "ma_temp.csv", row.names=FALSE)

  mSet<-InitDataObjects("pktable", "stat", FALSE)
  mSet<-Read.TextData(mSet, "ma_temp.csv", "colu", "disc") # duplicates not allowed
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet)
  mSet<-SanityCheckData(mSet)
  mSet<-FilterVariable(mSet, "none", -1, "F", 25, F) #TODO do we want to filter?
  mSet<-PreparePrenormData(mSet)
  mSet<-Normalization(mSet, "GroupPQN", logn, meanc, "QC", ratio=FALSE, ratioNum=20)

  file.remove("ma_temp.csv")
  dat <- cbind(dat[-1,1], t(mSet[["dataSet"]][["norm"]]))
  return(dat)
}

group_test <- function(data, seq) {
  library(limma)
  group <- paste("G", seq[, 1], sep="")
  sample <- rownames(seq)
  group <- factor(group)
  sample <- factor(sample)

  colnames(data) <- paste(group, colnames(data), sep=".")
  design <- model.matrix(~0+group)
  colnames(design) <- levels(group)
  contrast.matrix <- makeContrasts(contrasts=paste(colnames(design)[1], "-", colnames(design)[2]),levels=design)

  lm.fit <- lmFit(data, design)
  lm.contr <- contrasts.fit(lm.fit,contrast.matrix)
  lm.ebayes <- eBayes(lm.contr)
  results <- topTable(lm.ebayes, adjust="BH", number=Inf)
  detach(package:limma,unload=TRUE)
  return(results)
}

ts_test1 <- function(data, seq, paired) {
  library(limma)
  library(statmod)
  group <- paste("G", seq[, 1], sep="")
  time <- paste("T", seq[, 2], sep="")
  sample <- rownames(seq)
  group_time <- paste(group, time, sep="_")

  group <- factor(group)
  time <- factor(time)
  sample <- factor(sample)
  group_time <- factor(group_time)
  pairs <- factor(seq[, 3])

  if(length(levels(time)) == 1) {
    colnames(data) <- paste(group, colnames(data), sep=".")
    if(paired) { 
      design <- model.matrix(~pairs+group)
      coef <- paste("group", levels(group)[length(levels(group))], sep="")
    } else {
      design <- model.matrix(~0+group)
      colnames(design) <- levels(group)
      coef <- NULL
    }
    fit <- lmFit(data, design) # TODO partial NA coefficients?
    print(coef)
  }
  else if (length(levels(group)) == 1) {
    colnames(data) <- paste(time, colnames(data), sep=".")
    if(paired) { 
      design <- model.matrix(~pairs+time)
      coef <- paste("time", levels(time)[length(levels(time))], sep="")
    } else {
      design <- model.matrix(~0+time)
      colnames(design) <- levels(time)
      coef <- NULL
    }
    fit <- lmFit(data, design)
  }
  else {
    colnames(data) <- paste(group_time, colnames(data), sep=".")
    print(colnames(data))

    design <- model.matrix(~0+group_time)
    colnames(design) <- levels(group_time)
  }
  #  block = paired! makes sense??
  # duplicate correlation ONLY if we have paired samples
  # if(paired)
  #   corfit <- duplicateCorrelation(data, design, block=sample)
  # fit <- lmFit(data, design, block=sample, correlation=corfit$consensus)
  # contrast_table <- makeContrasts(contrasts = paste("S=", levels(group_time)[1], "-", levels(group_time)[2]),
  #                                 levels = design)
  # fit <- contrasts.fit(fit, contrast_table)
  fit <- eBayes(fit)
  
  results <- topTable(fit, coef=coef, adjust="BH")
  detach(package:limma,unload=TRUE)
  return(results)
}

windowselect <- function(input) {
  hide("welcome_panel")
  if (input == "pca") show("pca_panel") else hide("pca_panel")
  if (input == "drift") show("drift_panel") else hide("drift_panel")
  if (input == "feature") show("boxplot_panel") else hide("boxplot_panel")
  if (input == "sequence") show("sequence_panel") else hide("sequence_panel")
  if (input == "datatable") show("datatable_panel") else hide("datatable_panel")
  if (input == "statistics") show("statistics_panel") else hide("statistics_panel")
  if (input == "export") show("export_panel") else hide("export_panel")
  if (input == "info") show("info_panel") else hide("info_panel")
}