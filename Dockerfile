FROM rocker/shiny
LABEL maintainer = "Ana Mendes <anamendesml@outlook.com>"
LABEL description = "Docker image of JLspec"

RUN apt-get -o Acquire::AllowInsecureRepositories=true -o Acquire::AllowDowngradeToInsecureRepositories=true update && apt-get install -y libssl-dev liblzma-dev libbz2-dev libicu-dev && apt-get clean 

RUN R -e "install.packages('BiocManager', repos='http://cran.us.r-project.org'); \
        update.packages(ask=F); \
        BiocManager::install(c('dplyr','plotly'),ask=F)"

RUN R -e "library(BiocManager); BiocManager::install(c('matrixStats','DT','gplots', 'shiny', 'shinyBS','shinydashboard','limma',\
            'shinyjs','shinyalert','shinyWidgets','spsComps','ggplot2','ggrepel','gridExtra','impute','randomForest',\
            'writexl','igraph','stringi', 'jsonlite', 'devtools'),ask=F)"

RUN R -e "library(devtools); devtools::install_github('igraph/rigraph')"

RUN R -e "install.packages('pacman'); library(pacman); \
        pacman::p_load(c('impute', 'pcaMethods', 'globaltest', 'GlobalAncova', 'Rgraphviz', 'preprocessCore', 'genefilter',\
             'sva', 'limma', 'KEGGgraph', 'siggenes', 'BiocParallel', 'MSnbase', 'multtest', 'RBGL', 'edgeR','fgsea','httr','qs'))"

RUN R -e "devtools::install_github('xia-lab/MetaboAnalystR')"

RUN rm -rf /srv/shiny-server
RUN mkdir /srv/shiny-server
COPY *R  /srv/shiny-server/
COPY *html  /srv/shiny-server/
COPY csvfiles/*  /srv/shiny-server/csvfiles/