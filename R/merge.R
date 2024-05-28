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


mergeDatasets <- function(dataset1, sequence1, dataset2, sequence2, ppmTolerance, rtTolerance) {
  progressSweetAlert(id = "pb", title = "Work in progress", display_pct = T, value = 0, striped = T)
  mass <- dataset1[, sequence1[, 1] %in% "Mass"]
  if ("Adduct_pos" %in% sequence1[, 1]) {
    adduct <- dataset1[, sequence1[, 1] %in% "Adduct_pos"]
    ionmode1 <- "pos"
  } else {
    adduct <- dataset1[, sequence1[, 1] %in% "Adduct_neg"]
    ionmode1 <- "neg"
  }
  mass <- monomass(adduct, mass, ionmode1)
  rt <- dataset1[, sequence1[, 1] %in% "RT"]
  first <- data.frame(mass, rt)
  updateProgressBar(id = "pb", value = 10)
  mass <- dataset2[, sequence2[, 1] %in% "Mass"]
  if ("Adduct_pos" %in% sequence2[, 1]) {
    adduct <- dataset2[, sequence2[, 1] %in% "Adduct_pos"]
    ionmode2 <- "pos"
  } else {
    adduct <- dataset2[, sequence2[, 1] %in% "Adduct_neg"]
    ionmode2 <- "neg"
  }
  mass <- monomass(adduct, mass, ionmode2)
  rt <- dataset2[, sequence2[, 1] %in% "RT"]
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
  adj <- distppm <= ppmTolerance & distrt <= rtTolerance
  updateProgressBar(id = "pb", value = 80)
  graph <- graph_from_adjacency_matrix(adj)
  updateProgressBar(id = "pb", value = 90)
  mergeid <- components(graph)$membership
  updateProgressBar(id = "pb", value = 100)
  colnames(dataset2) <- colnames(dataset1)
  combineddat <- as.data.frame(rbind(dataset1, dataset2))
  combineddat[, "mergeID"] <- mergeid
  ionmode1 <- rep(ionmode1, nrow(dataset1))
  ionmode2 <- rep(ionmode2, nrow(dataset2))
  combineddat[, "ionmode"] <- c(ionmode1, ionmode2)
  closeSweetAlert()
  return(combineddat)
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
  j <- sapply(1:nrow(rankings), function(x) {
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