library(data.table)

# Define the class
setClass("Experiment", 
         slots = list(
           name = "character",
           dataTable = "data.table",
           sequence = "data.table",
           qualityControls = "data.table",
           samples = "data.table",
           results = "list"
         ))

# Create a constructor function for easy object creation
myExperiment <- function(name, dataTable, sequence, qualityControls, samples, results) {
  new("Experiment", 
      name = name, 
      dataTable = dataTable, 
      sequence = sequence,
      qualityControls = qualityControls, 
      samples = samples,
      results = results)
}

myExperiment <- function() {
  new("Experiment", 
      name = character(0), 
      dataTable = data.frame(), 
      sequence = data.frame(),
      qualityControls = data.frame(), 
      samples = data.frame(),
      results = list())
}