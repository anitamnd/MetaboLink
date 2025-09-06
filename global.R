library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(spsComps)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(impute)
library(randomForest)
library(writexl)
library(igraph)
library(stringi)
library(BiocManager)
library(shinycssloaders)
library(jsonlite)
library(shinyalert)
library(shinybusy)
library(gtools)
library(caret)
library(rgoslin)
library(ggbeeswarm)
library(broom)

library(webchem)
library(PubChemR)
library(data.table)
library(KEGGREST)
library(car)
# for heatmap 
library(stringr)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(circlize)    # for colorRamp2
library(grid)        # for manual text annotation
library(stats)       # for t.test, aov
# volcano 
library(colourpicker)
library(scales)

# pathway 
library(clusterProfiler)
# library(org.Mm.eg.db)

library(ggraph)
library(tidygraph)
library(tidyr)
library(networkD3)
library(network)
library(sna)
library(visNetwork)
library(threejs)
library(ndtv)
library(ggnetwork)

# lipid heatmap
library("lipidomeR") # new, used in Lipid Heatmap


#options(repos = BiocManager::repositories())
source("functions.R")

# Source files in R folder
rFiles <- list.files("./R", pattern = "\\.R$", full.names = TRUE)
for (file in rFiles) {
  source(file)
}