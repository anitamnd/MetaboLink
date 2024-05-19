selectPolySTest <- function(data, sequence, groups, time) {
  tseq <- sequence[sequence[, 1] %in% c("Name", "Sample"), ]
  baseCondition <- tseq[, 1] %in% "Name"
  if(!any(time == "")) {
    timeGroupCondition <- (tseq[, 5] %in% time[1] & tseq[, 4] %in% groups[1]) |
                          (tseq[, 5] %in% time[2] & tseq[, 4] %in% groups[2])                      
    condition <- baseCondition | timeGroupCondition
  } else {
    condition <- baseCondition | (tseq[, 4] %in% groups)
  }
  selected <- data[, condition, drop = FALSE]
  selected_sequence <- tseq[condition, ]

  selected <- selected[!duplicated(selected[, 1]), ] #TODO right now we remove duplicates before sending but this might be wrong 
  
  return(list(selected = selected, selected_sequence = selected_sequence))
}

addEmptyCols <- function(data, sequence, groups, replicates) {
  processed <- data[, 1] # feature names
  rgroup <- c("") # group vector
  rtime <- c("") # time vector

  for(group in 1:length(groups)) {
    groupCols <- data[, sequence[, 4] %in% groups[group]]
    time <- sequence[sequence[, 4] %in% groups[group], 5]
    processed <- cbind(processed, groupCols)
    if(length(groupCols) < replicates) {
      missing <- t(rep(NA, replicates - length(groupCols)))
      processed <- cbind(processed, missing)
    }
    rgroup <- append(rgroup, rep(paste("g", groups[group], sep = ""), replicates))
    if(any(complete.cases(sequence[, 5])))
      rtime <- append(rtime, t(time))
  }
  if(any(complete.cases(sequence[, 5])))
    colnames(processed) <- paste(colnames(processed), rgroup, paste("t", rtime, sep=""), sep = "_")
  else
    colnames(processed) <- paste(colnames(processed), rgroup, sep = "_")
  return(processed)
}

prepareMessage <- function(data, sequence) {
  if(any(complete.cases(sequence[, 5]))) {
    group_time <- paste(na.omit(sequence[, 4]), na.omit(sequence[, 5]), sep = "_")
    groups <- factor(group_time, exclude = NA)
  } else {
    groups <- factor(sequence[, 4], exclude = NA)
  }
  numrep <- max(table(groups))
  groups <- levels(groups)
  numcond <- length(groups)
  data <- addEmptyCols(data, sequence, groups, numrep)
  message <- toJSON(list(
    numrep = numrep, numcond = numcond, grouped = FALSE, 
    firstquantcol = 2, expr_matrix = as.list(as.data.frame(data))
  ))
  return(message)
}

sendMessage <- function(message, app) {
  js$send_message(url=paste0("http://computproteomics.bmb.sdu.dk:443/app_direct/", app), 
                    dat=message, tool=app)
}

export <- function(data, sequence, groups, time, app) {
  selected <- selectPolySTest(data, sequence, groups, time)
  message <- prepareMessage(selected$selected, selected$selected_sequence)
  sendMessage(message, app)
}