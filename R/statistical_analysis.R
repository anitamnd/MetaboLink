getGroupTime <- function(sequence) {
    sequence <- sequence[complete.cases(sequence[, 4]) & complete.cases(sequence[, 5]), ] # Remove rows with missing group or time
    group <- paste("G", sequence[, 4], sep="") # Group names
    time <- paste("T", sequence[, 5], sep="") # Time points
    group_time <- paste(group, time, sep="_")   # Group-time combinations
}

groupComparison <- function(data, sequence, groups) {
    library(limma)
    # Select samples corresponding to groups
    keep <- sequence[, 1] %in% "Name" | sequence[, 4] %in% groups # Keep samples with group names
    # Debugging
    print("Keep: ")
    print(keep)
    selected <- data[, keep] # Select samples
    # Debugging
    print("Selected: ")
    print(head(selected))
    sequence <- sequence[sequence[, 4] %in% groups, ] # Select rows with group names
    # Debugging
    print("sequence: ")
    print(head(sequence))
    groups <- paste("G", sequence[, 4], sep="") # Group names
    # Debugging
    print(groups)
    group <- factor(groups) # Create factor for groups
    # Debugging
    print(group)

    # Change column names to group names 
    colnames(selected)[2:ncol(selected)] <- paste(group, colnames(selected)[2:ncol(selected)], sep=".") # Change column names to group names

    design <- model.matrix(~ 0 + group) # Create design matrix
    # Debugging
    print("Design: ")
    print(design)
    colnames(design) <- levels(group) # Change column names to group names
    contrast.matrix <- makeContrasts(contrasts = paste(colnames(design)[1], "-", colnames(design)[2]), levels = design) # Create contrast matrix
    # Debugging
    print("contrast.matrix: ")
    print(contrast.matrix)
    
    lm.fit <- lmFit(selected, design) # Fit linear model
    # Debugging
    print("lm.fit: ")
    print(lm.fit)
    lm.contr <- contrasts.fit(lm.fit, contrast.matrix) # Fit contrasts
    # Debugging
    print("lm.contr: ")
    print(lm.contr)
    lm.ebayes <- eBayes(lm.contr) # Empirical Bayes moderation
    # Debugging
    print("lm.ebayes: ")
    print(lm.ebayes)
    results <- topTable(lm.ebayes, adjust = "BH", number = Inf) # Get top table
    # Debugging
    print("results: ")
    print(head(results))
    detach(package:limma, unload = TRUE) # Detach limma package
    return(results)
}

groupComparisonPaired <- function(data, sequence, groups) {
    library(limma)
    # Select samples corresponding to groups
    keep <- sequence[, 1] %in% "Name" | sequence[, 4] %in% groups # Keep samples with group names
    selected <- data[, keep] # Select samples
    samples <- selected[, 2:ncol(selected)] # Select samples
    sequence <- sequence[sequence[, 4] %in% groups, ] # Select rows with group names
    groups <- paste("G", sequence[, 4], sep="") #TODO ?
    group <- factor(groups) # Create factor for groups
    # print(group) 
    paired <- sequence[, 6] # Get paired column
    # print(paired)

    # Change column names to group names
    colnames(selected)[2:ncol(selected)] <- paste(group, colnames(selected)[2:ncol(selected)], sep=".") # Change column names to group names

    design <- model.matrix(~ 0 + group) # Create design matrix
    colnames(design) <- levels(group) # Change column names to group names
    corfit <- duplicateCorrelation(samples, design, block=paired) # Get correlation
    # print(corfit$consensus) #TODO show this somewhere? 
    contrast.matrix <- makeContrasts(contrasts = paste(colnames(design)[1], "-", colnames(design)[2]), levels = design) # Create contrast matrix
    # print(contrast.matrix)
    
    lm.fit <- lmFit(selected, design, block = paired, correlation=corfit$consensus) # Fit linear model
    lm.contr <- contrasts.fit(lm.fit, contrast.matrix) # Fit contrasts
    lm.ebayes <- eBayes(lm.contr) # Empirical Bayes moderation
    results <- topTable(lm.ebayes, adjust = "BH", number = Inf) # Get top table
    detach(package:limma, unload = TRUE) # Detach limma package
    return(results)
}

groupComparisonTime <- function(data, sequence, groups, times) {
    library(limma)
    # Select samples corresponding to selected groups and time points
    keep <- sequence[, 1] %in% "Name" # Keep samples with group names
    groupTime1 <- sequence[, 4] %in% groups[1] & sequence[, 5] %in% times[1] # Keep samples with group 1 and time 1
    groupTime2 <- sequence[, 4] %in% groups[2] & sequence[, 5] %in% times[2] # Keep samples with group 2 and time 2
    selected <- data[, keep | groupTime1 | groupTime2] # Select samples
    sequence <- sequence[groupTime1 | groupTime2, ] # Select rows with group names

    groups <- paste(sequence[, 4], sequence[, 5], sep = ".") # Group-time combinations
    group <- factor(groups) # Create factor for groups

    # print(group)
    # Change column names to group names 
    colnames(selected)[2:ncol(selected)] <- paste(group, colnames(selected)[2:ncol(selected)], sep=".") # Change column names to group names

    # print(colnames(selected))

    design <- model.matrix(~ 0 + group) # Create design matrix
    colnames(design) <- levels(group) # Change column names to group names
    # print(design)
    contrast.matrix <- makeContrasts(contrasts = paste(colnames(design)[1], "-", colnames(design)[2]), levels = design) # Create contrast matrix

    lm.fit <- lmFit(selected, design) # Fit linear model 
    lm.contr <- contrasts.fit(lm.fit, contrast.matrix) # Fit contrasts
    lm.ebayes <- eBayes(lm.contr) # Empirical Bayes moderation
    results <- topTable(lm.ebayes, adjust = "BH", number = Inf) # Get top table
    detach(package:limma, unload = TRUE)
    return(results)
}

referenceGroupComparison <- function(data, reference, groups) {
    library(limma)
    library(qvalue)
    groups <- factor(groups) # Create factor for groups
    design <- model.matrix(~ 0 + groups) # Create design matrix
    numConditions <- length(levels(groups)) # Number of conditions
    position <- match(reference, levels(groups)) # Position of reference group
    contrasts <- NULL
    for (i in (1:numConditions)[-position]) { # Create contrasts
        contrasts <- append(contrasts, paste(colnames(design)[i], "-", colnames(design)[reference], sep = "")) # Create contrast
    }
    contrast.matrix <- makeContrasts(contrasts = contrasts, levels = design) # Create contrast matrix
    # print(contrast.matrix) # Print contrast matrix
    lm.fit <- lmFit(data, design) # Fit linear model
    lm.contr <- contrasts.fit(lm.fit, contrast.matrix) # Fit contrasts
    lm.ebayes <- eBayes(lm.contr) # Empirical Bayes moderation
    results <- topTable(lm.ebayes, adjust = "BH", number = Inf) # Get top table
    detach(package:limma, unload = TRUE) 
    return(results)
}

is_valid_combination <- function(group1, group2) {
    g1 <- strsplit(group1, "_")[[1]][1] # Get group 1
    t1 <- strsplit(group1, "_")[[1]][2] # Get time 1
    g2 <- strsplit(group2, "_")[[1]][1] # Get group 2
    t2 <- strsplit(group2, "_")[[1]][2] # Get time 2
    return((g1 == g2 && t1 != t2) || (g1 != g2 && t1 == t2))
}

generate_contrasts <- function(combinations) {
    contrasts <- c()
    for (i in 1:ncol(combinations)) {
        group1 <- combinations[1, i] # Get group 1
        group2 <- combinations[2, i] # Get group 2
        contrast_name <- paste0(group1, "_vs_", group2) # Contrast name
        contrast_value <- paste0(group1, " - ", group2) # Contrast value
        contrast <- paste(contrast_name, contrast_value, sep = " = ") # Create contrast
        contrasts <- append(contrasts, contrast) #  Add contrast to list
    }
    return(contrasts)
}

pairedAnalysis <- function(data, group_time, contrasts, paired) {
    library(limma)
    samples <- data[, 2:ncol(data)] # Select samples

    design <- model.matrix(~0+group_time) # Create design matrix
    colnames(design) <- levels(group_time) # Change column names to group-time combinations
    # print(design)
    corfit <- duplicateCorrelation(samples, design, block=paired) # Get correlation
    # print(corfit$consensus) # Show consensus

    lm.fit <- lmFit(data, design, block = paired, correlation=corfit$consensus) # Fit linear model
    contrast_table <- makeContrasts(contrasts = contrasts, levels = design) # Create contrast table
    lm.contr <- contrasts.fit(lm.fit,contrast_table) # Fit contrasts
    lm.ebayes <- eBayes(lm.contr) # Empirical Bayes moderation
    
    results <- list() # Create list for results
    for(contrast in contrasts) {
        results[[contrast]] <- topTable(lm.ebayes, coef = contrast, adjust = "BH", number = Inf) # Get top table
    }
    # print(names(results))
    #results <- topTable(lm.ebayes, adjust = "BH", number = Inf)
    detach(package:limma, unload = TRUE)
    return(results)
}