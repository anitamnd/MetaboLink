getGroupTime <- function(sequence) {
    sequence <- sequence[complete.cases(sequence[, 4]), ]
    group <- paste("G", sequence[, 4], sep="")
    time <- paste("T", sequence[, 5], sep="")
    group_time <- paste(group, time, sep="_")
}

groupComparison <- function(data, sequence, groups) {
    library(limma)
    # Select samples corresponding to groups
    keep <- sequence[, 1] %in% "Name" | sequence[, 4] %in% groups
    selected <- data[, keep]
    sequence <- sequence[sequence[, 4] %in% groups, ]
    groups <- paste("G", sequence[, 4], sep="")
    group <- factor(groups)

    # Change column names to group names 
    colnames(selected)[2:ncol(selected)] <- paste(group, colnames(selected)[2:ncol(selected)], sep=".")

    design <- model.matrix(~ 0 + group)
    colnames(design) <- levels(group)
    contrast.matrix <- makeContrasts(contrasts = paste(colnames(design)[1], "-", colnames(design)[2]), levels = design)

    lm.fit <- lmFit(selected, design)
    lm.contr <- contrasts.fit(lm.fit, contrast.matrix)
    lm.ebayes <- eBayes(lm.contr)
    results <- topTable(lm.ebayes, adjust = "BH", number = Inf)
    detach(package:limma, unload = TRUE)
    return(results)
}

referenceGroupComparison <- function(data, reference, groups) {
    library(limma)
    library(qvalue)
    groups <- factor(groups)
    design <- model.matrix(~ 0 + groups)
    numConditions <- length(levels(groups))
    position <- match(reference, levels(groups))
    contrasts <- NULL
    for (i in (1:numConditions)[-position]) {
        contrasts <- append(contrasts, paste(colnames(design)[i], "-", colnames(design)[reference], sep = ""))
    }
    contrast.matrix <- makeContrasts(contrasts = contrasts, levels = design)
    print(contrast.matrix)
    lm.fit <- lmFit(data, design)
    lm.contr <- contrasts.fit(lm.fit, contrast.matrix)
    lm.ebayes <- eBayes(lm.contr)
    results <- topTable(lm.ebayes, adjust = "BH", number = Inf)
    detach(package:limma, unload = TRUE)
    return(results)
}

is_valid_combination <- function(group1, group2) {
    g1 <- strsplit(group1, "_")[[1]][1]
    t1 <- strsplit(group1, "_")[[1]][2]
    g2 <- strsplit(group2, "_")[[1]][1]
    t2 <- strsplit(group2, "_")[[1]][2]
    (g1 == g2 && t1 != t2) || (g1 != g2 && t1 == t2)
}

generate_contrasts <- function(combinations) {
    contrasts <- c()
    for (i in 1:ncol(combinations)) {
        group1 <- combinations[1, i]
        group2 <- combinations[2, i]
        contrast_name <- paste0(group1, "_vs_", group2)
        contrast_value <- paste0(group1, " - ", group2)
        contrast <- paste(contrast_name, contrast_value, sep = " = ")
        contrasts <- append(contrasts, contrast)
    }
    return(contrasts)
}

pairedAnalysis <- function(data, group_time, contrasts, paired) {
    library(limma)
    samples <- data[, 2:ncol(data)]

    design <- model.matrix(~0+group_time)
    colnames(design) <- levels(group_time)
    print(design)
    corfit <- duplicateCorrelation(samples, design, block=paired)
    print(corfit$consensus)

    lm.fit <- lmFit(data, design, block = paired, correlation=corfit$consensus)
    contrast_table <- makeContrasts(contrasts = contrasts, levels = design)
    lm.contr <- contrasts.fit(lm.fit,contrast_table)
    lm.ebayes <- eBayes(lm.contr)
    
    results <- list()
    for(contrast in contrasts) {
        results[[contrast]] <- topTable(lm.ebayes, coef = contrast, adjust = "BH", number = Inf)
    }
    print(names(results))
    #results <- topTable(lm.ebayes, adjust = "BH", number = Inf)
    detach(package:limma, unload = TRUE)
    return(results)
}