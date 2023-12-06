groupComparison <- function(data, sequence) { # here we assume the sequence only has the 2 groups for comparison!
  library(limma)
  group <- paste("G", sequence[, 1], sep="")
  sample <- rownames(sequence)
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

referenceGroupComparison <- function(data, reference, groups) {
# remove non samples
# groups
    design <- model.matrix(~0+groups)
    numConditions <- length(levels(groups))
    contrasts <- NULL
    reference <- 1 # change to input reference group
    for (i in (1:numConditions)[-reference]){
        contrasts <- append(contrasts, paste(colnames(design)[i], "-", colnames(design)[reference], sep = ""))
    }
    contrast.matrix <- makeContrasts(contrasts = contrasts, levels = design)
    print(contrast.matrix)
    lm.fit <- lmFit(data, design)
    lm.contr <- contrasts.fit(lm.fit, contrast.matrix)
    lm.ebayes <- eBayes(lm.contr)
    results <- topTable(lm.ebayes, adjust="BH", number=Inf)

    # uncorrected p-values
    plvalues <- lm.ebayes$p.value
    # corrected p-values
    qvals <- qvalue(plvalues, fdr.level = 0.05) #TODO option to choose FDR level?
    fdrs <- p.adjust(plvalues, method = "BH")
    paste("Number of differentialy regulated features with reference to group 1:", sum(qvals$significant))

    return(results)
}

pairedComparison <- function() {

}

timeSeries <- function() {
    #TODO
    # see last statistics notebook chunk
}