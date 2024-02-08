# JLspec
---

JLspec is a web-based application created with shiny R and it is available at http://computproteomics.bmb.sdu.dk/Metabolomics/.


## Installation

### Clone repository

```
git clone https://github.com/anitamnd/jlspec-2.0
```
### Install dependencies
Use the following code to install the required R packages:

```
install.packages('BiocManager', repos='http://cran.us.r-project.org')
library(BiocManager)
BiocManager::install(c('dplyr','plotly','shiny','shinyBS','shinydashboard','shinycssloaders','limma','shinyjs','shinyalert','shinyWidgets','spsComps','ggplot2','ggrepel','gridExtra','impute','randomForest','writexl','stringi','igraph'), ask=F)
```

You can then run the app from the server.R or ui.R files using RStudio or by writing "shiny::runApp()" in the R terminal.

---

## Contact
If you have any suggestions for improvement, contact anitamnd@outlook.com. If you encounter bugs or have further questions or requests, you can raise an issue at the issue page.
