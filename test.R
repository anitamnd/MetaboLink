source("functions.R")
source("merge.R")

data_pos <- read.csv("C:/Users/aname/Downloads/JFH bsc lipid pos_1_MERGETEST1.csv")
data_neg <- read.csv("C:/Users/aname/Downloads/JFH bsc lipid neg_mergeTEST1.csv")

dataset1 <- data_pos
dataset2 <- data_neg

labels <- identifyLabels(data_pos)
sequence <- data.frame(
    labels = labels,
    batch =  NA, 
    order = NA,
    group = NA,
    time = NA,
    paired = NA,
    amount = NA
)
rownames(sequence) <- colnames(data_pos)

merged_old <- mergeDatasets(data_pos, sequence, data_neg, sequence, 10, 0.1)
merged_new <- mergeDatasets_new(data_pos, sequence, data_neg, sequence, 10, 0.1)

duplicates <- extractDuplicateClusters(merged_new)
coefVariation <- cv(extractQCs(duplicates, sequence[, 1])) 
output <- prepareOutputDataFrame(duplicates, sequence, coefVariation)
cluster_ends <- findClusterEndpoints(output)
