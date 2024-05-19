FROM rocker/shiny
LABEL maintainer = "Ana Mendes <anamendesml@outlook.com>"
LABEL description = "Docker image of MetaboLink"

RUN rm -rf /srv/shiny-server/*
COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server/
RUN apt update; apt install -y libglpk-dev


RUN R -e "install.packages('BiocManager', repos='http://cran.us.r-project.org'); \
        update.packages(ask=F)"

RUN R -e "library(BiocManager);BiocManager::install(c('dplyr','plotly','matrixStats','DT','gplots', \
        'shiny','shinyBS','shinydashboard','shinycssloaders','limma','shinyjs','shinyalert', \
        'shinyWidgets','spsComps','ggplot2','ggrepel','gridExtra','impute', \
        'randomForest','writexl','stringi','igraph','colorspace'), ask=F)"
        