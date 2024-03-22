# MetaboLink
---

MetaboLink is a web-based application created with shiny R and it is available at http://computproteomics.bmb.sdu.dk/Metabolomics/.
Documentation on how to use the app can be found in [Wiki MetaboLink](https://github.com/anitamnd/MetaboLink/wiki).

Example files to be used with the app are included in the folder _example_files_.

## Implementation on own computer

### Docker

```
docker pull anitamnd/metabolink
```

```
docker run -t -i -p 3838:3838 anitamnd/metabolink
```

and access the shiny app through http://localhost:3838


## Instalation

### Clone repository

```
git clone https://github.com/anitamnd/MetaboLink
```

### Install dependencies
Use the following code to install the required R packages:

```
install.packages('BiocManager', repos='http://cran.us.r-project.org')
library(BiocManager)
BiocManager::install(c('dplyr','plotly','shiny','shinyBS','shinydashboard','shinycssloaders','limma','shinyjs','shinyalert','shinyWidgets','spsComps','ggplot2','ggrepel','gridExtra','impute','randomForest','writexl','stringi','igraph'), ask=F)
```

You can then run the app from the server.R or ui.R files using RStudio or run the app on a shiny-server.

---

## Contact

For software issues and general questions, please submit an issue.
