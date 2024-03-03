
# Calculate monoisotopic mass from m/z values
calculateMonoisotopicMass <- function(adducts, masses, ionMode, massCorrection) {
  indices <- match(adducts, massCorrection[, 1])
  
  correctedMasses <- ifelse(!is.na(indices),
                            masses * massCorrection[indices, "charge"] - massCorrection[indices, "weight"],
                            ifelse(ionMode == "pos", masses - 1.007276, masses + 1.007276))
  return(correctedMasses)
}

# Data extraction
determineIonMode <- function(labels) {
  return(ifelse("Adduct_pos" %in% labels, "pos", "neg"))
}

extractMassAndAdduct <- function(dataset, labels, ionMode) {
  mass <- dataset[, labels == "Mass"]
  adduct <- dataset[, labels == paste("Adduct", ionMode, sep = "_")]
  return(list(mass = mass, adduct = adduct))
}

extractRetentionTime <- function(dataset, labels) {
  return(dataset[, labels == "RT"])
}

# Merging 
calculateDistances <- function(combined) {
  distMz <- as.matrix(dist(combined$mass))
  ppmDist <- (distMz / rep(combined$mass, each = nrow(distMz))) * 1e6
  distRt <- as.matrix(dist(combined$rt))
  return(list(ppmDist = ppmDist, distRt = distRt))
}

createAdjacencyMatrix <- function(ppmDist, distRt, ppmTolerance, rtTolerance) {
  return((ppmDist <= ppmTolerance) & (distRt <= rtTolerance))
}

mergeAndLabelDatasets <- function(dataset1, dataset2, mergeID, ionMode1, ionMode2) {
  combinedData <- rbind(dataset1, dataset2)
  combinedData$mergeID <- mergeID
  combinedData$ionMode <- c(rep(ionMode1, nrow(dataset1)), rep(ionMode2, nrow(dataset2)))
  return(combinedData)
}

analyzeGraphComponents <- function(adjacencyMatrix) {
  graph <- igraph::graph.adjacency(adjacencyMatrix, mode = "undirected")
  return(igraph::clusters(graph)$membership)
}


mergeDatasets_new <- function(dataset1, sequence1, dataset2, sequence2, ppmTolerance, rtTolerance) {
  massCorrection <- read.csv("./csvfiles/adducts.csv")
  # Extract data from datasets
  data1 <- extractMassAndAdduct(dataset1, sequence1[, 1], determineIonMode(sequence1[, 1]))
  data2 <- extractMassAndAdduct(dataset2, sequence2[, 1], determineIonMode(sequence2[, 1]))

  # Calculate monoisotopic masses
  mass1 <- calculateMonoisotopicMass(data1$adduct, data1$mass, determineIonMode(sequence1[, 1]), massCorrection)
  mass2 <- calculateMonoisotopicMass(data2$adduct, data2$mass, determineIonMode(sequence2[, 1]), massCorrection)

  # Combine mass and retention time data
  combined <- rbind(data.frame(mass = mass1, rt = extractRetentionTime(dataset1, sequence1[, 1])),
                    data.frame(mass = mass2, rt = extractRetentionTime(dataset2, sequence2[, 1])))

  # Calculate distances and create adjacency matrix
  distances <- calculateDistances(combined)
  adjMatrix <- createAdjacencyMatrix(distances$ppmDist, distances$distRt, ppmTolerance, rtTolerance)

  # Analyze graph components and merge datasets
  mergeID <- analyzeGraphComponents(adjMatrix)
  mergedDatasets <- mergeAndLabelDatasets(dataset1, dataset2, mergeID, determineIonMode(sequence1[, 1]), determineIonMode(sequence2[, 1]))
  return(mergedDatasets)
}

# Function to identify and extract data for duplicate clusters
extractDuplicateClusters <- function(mergedDatasets) {
  clustCounts <- table(mergedDatasets$mergeID)
  duplicateClusters <- subset(as.data.frame(clustCounts), Freq > 1)
  duplicateData <- mergedDatasets[mergedDatasets$mergeID %in% duplicateClusters$Var1, ]
  return(duplicateData)
}

# Function to prepare the output data frame with necessary transformations
prepareOutputDataFrame <- function(data, activeSequence, cv) {
  # Modify column names based on 'activeSequence' and prepare the final data frame
  colnames(data)[activeSequence[, 1] %in% c("Adduct_pos", "Adduct_neg")] <- "adduct"
  outData <- data.frame(
    "nClust" = sapply(data$mergeID, function(x) table(data$mergeID)[as.character(x)]),
    "Cluster_ID" = data$mergeID,
    "Ion_mode" = data$ionMode,
    "Adductor" = data$adduct,
    "Name" = data[, which(activeSequence[, 1] %in% "Name")],
    "RT" = data[, which(activeSequence[, 1] %in% "RT")],
    "Mass" = data[, which(activeSequence[, 1] %in% "Mass")],
    "CV" = cv
  )
  return(outData[order(outData$nClust, outData$Cluster_ID, decreasing = TRUE), ])
}

# Function to find the starting row of each cluster in the data
findClusterEndpoints <- function(data) {
  return(which(!duplicated(data$Cluster_ID)))
}



duplicateRank <- function(duplicate, rankings) {
  ranks <- sapply(1:nrow(rankings), function(x) {
    if (rankings[x, 1] != "") {
      grepl(toupper(rankings[x, 1]), toupper(duplicate[5]))
    } else {
      FALSE
    }
  })
  if (any(ranks)) {
    return(min(as.numeric(rankings[ranks, 2])))
  } else {
    return(10)
  }
}

findDuplicate <- function(output, rankings) {
  priorities <- apply(output, 1, function(row) duplicateRank(row, rankings))
  output$priorities <- priorities
  
  selectedRows <- integer(0)  # Initialize an empty vector to store selected row indices
  for (priority in unique(output$priorities)) {
    subsetRows <- which(output$priorities == priority)
    minCV <- min(output$CV[subsetRows])
    selectedRow <- subsetRows[which.min(output$CV[subsetRows] == minCV)]
    selectedRows <- c(selectedRows, selectedRow)
  }
  return(selectedRows)
}
