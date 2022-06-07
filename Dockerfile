FROM  dragonflyscience/dragonverse-20.04:2021-10-20

RUN Rscript -e "install.packages('shinydashboard', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('shinyWidgets', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('shinyjs', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('shinycssloaders', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('reactable', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "install.packages('sass', repos = 'https://cloud.r-project.org')"

RUN Rscript -e "webshot::install_phantomjs()"
RUN mv /root/bin/phantomjs /usr/local/bin/phantomjs
