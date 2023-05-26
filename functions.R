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
  #TODO to which internal standard each feature was normalized
  rt <- which(seq[, 1] == "RT")
  isname <- is
  is <- as.numeric(gsub(" .*$", "", is))
  if (qc) {
    sel <- c("Sample", "QC")
  } else {
    sel <- "Sample"
  }
  sdat <- dat[seq[, 1] %in% sel]
  sdat[sdat == 0] <- NA
  print(1)
  print(class(sdat))
  # which of the selected internal standards has the retention time closest to each of the features 
  near <- sapply(dat[, rt], function(y) {
    which.min(abs(dat[is, rt] - y))
  })
  # make a substring from the start of the annotation and until the first white space. 
  # If thissubstring matches any of the substrings from the internal standards, it will 
  # prioritize normalizing to this. If no substring matches are found, it will 
  # normalize to the internal standard with the most similar retention time
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
  sapply(seq(ncol(sdat)), function(j) {
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
  #TODO grey out IS with missing values
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
    if (grepl("BLANK", toupper(x)) && is.numeric(data[, x])) {
      "Blank"
    } else if (grepl("QC", toupper(x)) && is.numeric(data[, x])) {
      "QC"
    } else if (grepl("NAME", toupper(x))) {
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
  if ("in class" %in% method) {
    datm <- dat[seq[, 1] %in% "Sample"]
    datm[datm == 0] <- NA
    classes <- factor(seq[, 4], exclude = NA)
    nseq <- seq[complete.cases(seq), ]
    keep_m <- matrix(FALSE, nrow(datm), ncol = 6)

    for (cl in 1:length(levels(classes))) {
      cl_f <- datm[, nseq[, 4] %in% levels(classes)[cl]]
      keep_m[, cl] <- rowSums(!is.na(cl_f)) / ncol(cl_f) >= cutoff
    }
    keep <- apply(keep_m, 1, function(x) any(x))
  }
  dat <- dat[keep, ]
  return(dat)
} #TODO if you don't select anything?

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

## Statistics
# Order columns by group
# Check replicates and adds empty columns if necessary
addNAColumns <- function(dat, seq, groups, maxreps) {
  tdat <- dat[,1]
  for(group in 1:length(levels(groups))) {
    groupdat <- dat[, seq[, 4] %in% levels(groups)[group]]
    tdat <- cbind(tdat, groupdat)
    if(length(groupdat) < maxreps) {
      tdat <- cbind(tdat, t(rep(NA, maxreps - length(groupdat))))
    }
    colnames(tdat)[(ncol(tdat)-maxreps+1):ncol(tdat)] <- rep(paste("G", levels(groups)[group], sep = "_"), maxreps)
  }
  return(tdat)
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