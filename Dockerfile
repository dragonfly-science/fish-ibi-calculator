FROM  dragonflyscience/dragonverse-18.04:2020-10-20


RUN Rscript -e "install.packages('shinydashboard', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('leaflet.esri', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('shinyWidgets', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('shinyjs', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('shinyjqui', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('showtext', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('shinycssloaders', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('reactable', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('quantreg', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('tippy', repos = 'https://cloud.r-project.org')"

RUN Rscript -e "webshot::install_phantomjs()"

