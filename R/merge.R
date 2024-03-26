# Merge different ion mode datasets
#source("data_utils.R")

#' Calculate Monoisotopic Mass Function
#'
#' This function calculates the monoisotopic mass of each observation in the data. 
#' It corrects the masses based on the adducts and corresponding mass correction values. 
#' If there is no match in adduct and massCorrection data, it adjusts the masses based on the ion mode.
#'
#' @param adducts A character vector representing the adducts associated with each observation.
#' @param masses A numeric vector representing the masses to be corrected.
#' @param ionMode A character string representing the ion mode. Can be 'pos' or 'neg'.
#' @param massCorrection A data frame containing the correction values.
#'
#' @return A numeric vector representing the corrected monoisotopic masses.
calculateMonoisotopicMass <- function(adducts, masses, ionMode, massCorrection) {
  indices <- match(adducts, massCorrection[, 1])
  
  correctedMasses <- ifelse(!is.na(indices),
                            masses * massCorrection[indices, "charge"] - massCorrection[indices, "weight"],
                            ifelse(ionMode == "pos", masses - 1.007276, masses + 1.007276))
  return(correctedMasses)
}

#' Calculate Distances Function
#'
#' This function calculates the distances in mass and retention time (rt) between all pairs of observations in the combined data.
#'
#' @param combined A data frame containing the combined data.
#'
#' @return A list containing the ppm distance and rt distance matrices.
calculateDistances <- function(combined) {
  distMz <- as.matrix(dist(combined$mass))
  ppmDist <- (distMz / rep(combined$mass, each = nrow(distMz))) * 1e6
  distRt <- as.matrix(dist(combined$rt))

  return(list(ppmDist = ppmDist, distRt = distRt))
}


#' Create Adjacency Matrix Function
#'
#' This function creates an adjacency matrix based on ppm and rt tolerances.
#'
#' @param ppmDist A numeric matrix representing the ppm distances.
#' @param distRt A numeric matrix representing the rt distances.
#' @param ppmTolerance A numeric value representing the ppm tolerance.
#' @param rtTolerance A numeric value representing the rt tolerance.
#'
#' @return A logical matrix representing the adjacency matrix.
createAdjacencyMatrix <- function(ppmDist, distRt, ppmTolerance, rtTolerance) {

  return((ppmDist <= ppmTolerance) & (distRt <= rtTolerance))
}


#' Merge and Label Datasets Function
#'
#' This function merges two datasets and labels them with a merge ID and ion mode.
#'
#' @param dataset1 A data frame representing the first dataset.
#' @param dataset2 A data frame representing the second dataset.
#' @param mergeID A numeric value representing the merge ID.
#' @param ionMode1 A character string representing the ion mode of the first dataset.
#' @param ionMode2 A character string representing the ion mode of the second dataset.
#'
#' @return A data frame representing the merged and labeled dataset.
mergeAndLabelDatasets <- function(dataset1, dataset2, mergeID, ionMode1, ionMode2) {
  combinedData <- rbind(dataset1, dataset2)
  combinedData$mergeID <- mergeID
  combinedData$ionMode <- c(rep(ionMode1, nrow(dataset1)), rep(ionMode2, nrow(dataset2)))
  return(combinedData)
}


#' Analyze Graph Components Function
#'
#' This function analyzes the components of a graph represented by an adjacency matrix.
#'
#' @param adjacencyMatrix A logical matrix representing the adjacency matrix of the graph.
#'
#' @return A numeric vector representing the membership of each vertex in the graph.
analyzeGraphComponents <- function(adjacencyMatrix) {
  graph <- igraph::graph.adjacency(adjacencyMatrix, mode = "undirected")
  return(igraph::clusters(graph)$membership)
}


#' Merge Datasets Function
#'
#' This function merges two datasets based on ppm and rt tolerances. It calculates monoisotopic masses, combines mass and retention time data, calculates distances, creates an adjacency matrix, analyzes graph components, and finally merges and labels the datasets.
#'
#' @param dataset1 A data frame representing the first dataset.
#' @param sequence1 A data frame representing the sequence of the first dataset.
#' @param dataset2 A data frame representing the second dataset.
#' @param sequence2 A data frame representing the sequence of the second dataset.
#' @param ppmTolerance A numeric value representing the ppm tolerance.
#' @param rtTolerance A numeric value representing the rt tolerance.
#'
#' @return A data frame representing the merged and labeled dataset.
mergeDatasets <- function(dataset1, sequence1, dataset2, sequence2, ppmTolerance, rtTolerance) {
  massCorrection <- read.csv("./csvfiles/adducts.csv")

  # Determine positive and negative ion mode datasets
  ion_mode_1 <- determine_ion_mode(sequence1[, 1])
  ion_mode_2 <- determine_ion_mode(sequence2[, 1])

  # Extract data from datasets
  data1 <- select_mass_and_adduct(dataset1, sequence1[, 1], ion_mode_1)
  data2 <- select_mass_and_adduct(dataset2, sequence2[, 1], ion_mode_2)

  # Calculate monoisotopic masses
  mass1 <- calculateMonoisotopicMass(data1$adduct, data1$mass, ion_mode_1, massCorrection)
  mass2 <- calculateMonoisotopicMass(data2$adduct, data2$mass, ion_mode_2, massCorrection)

  # Combine mass and retention time data
  combined <- rbind(data.frame(mass = mass1, rt = select_retention_time(dataset1, sequence1[, 1])),
                    data.frame(mass = mass2, rt = select_retention_time(dataset2, sequence2[, 1])))

  # Calculate distances and create adjacency matrix
  distances <- calculateDistances(combined)
  adjMatrix <- createAdjacencyMatrix(distances$ppmDist, distances$distRt, ppmTolerance, rtTolerance)

  # Analyze graph components and merge datasets
  mergeID <- analyzeGraphComponents(adjMatrix)
  mergedDatasets <- mergeAndLabelDatasets(dataset1, dataset2, mergeID, ion_mode_1, ion_mode_2)
  return(mergedDatasets)
}

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
