FROM rocker/shiny
LABEL maintainer = "Ana Mendes <anamendesml@outlook.com"
LABEL description = "Docker image for the shiny app of the project ''"

RUN apt-get -o Acquire::AllowInsecureRepositories=true -o Acquire::AllowDowngradeToInsecureRepositories=true update && apt-get install -y libssl-dev liblzma-dev libbz2-dev libicu-dev && apt-get clean 

RUN R -e "install.packages('BiocManager', repos='http://cran.us.r-project.org'); \
  update.packages(ask=F); \
  BiocManager::install(c('dplyr','plotly'),ask=F)"
RUN R -e "library(BiocManager); BiocManager::install(c('matrixStats','DT','gplots','shinyBS','shinydashboard','limma',\
            'shinyjs','shinyalert','shinyWidgets','spsComps','ggplot2','ggrepel','gridExtra','impute','randomForest',\
            'writexl','igraph','stringi'),ask=F)"

RUN rm -rf /srv/shiny-server
RUN mkdir /srv/shiny-server
RUN mkdir /srv/shiny-server/www
COPY www/* /srv/shiny-server/www/
COPY *R  /srv/shiny-server/
COPY *html  /srv/shiny-server/
COPY csvfiles/*  /srv/shiny-server/csvfiles/