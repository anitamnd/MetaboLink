FROM rocker/shiny
LABEL maintainer = "Ana Mendes <anamendesml@outlook.com>"
LABEL description = "Docker image of MetaboLink"

COPY . /srv/shiny-server/metabolink
WORKDIR /srv/shiny-server/metabolink

RUN R -e "install.packages('BiocManager', repos='http://cran.us.r-project.org'); \
        update.packages(ask=F)"

RUN R -e "library(BiocManager);BiocManager::install(c('dplyr','plotly','matrixStats','DT','gplots', \
        'shiny','shinyBS','shinydashboard','limma','shinyjs','shinyalert', \
        'shinyWidgets','spsComps','ggplot2','ggrepel','gridExtra','impute', \
        'randomForest','writexl','stringi','igraph'), ask=F)"

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/metabolink', host = '0.0.0.0', port = 3838)"]