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
RUN Rscript -e "install.packages('webshot', repos = 'https://cloud.r-project.org')"

RUN apt-get update
RUN apt-get install build-essential chrpath libssl-dev libxft-dev -y
RUN apt-get install libfreetype6 libfreetype6-dev -y
RUN apt-get install libfontconfig1 libfontconfig1-dev -y
RUN cd ~
ARG PHANTOM_JS="2.1.14"

RUN wget https://github.com/Medium/phantomjs/archive/refs/tags/2.1.14.tar.gz
#RUN wget https://github.com/Medium/phantomjs/archive/refs/$PHANTOM_JS.tar.gz
RUN tar -xvf $PHANTOM_JS.tar.gz
RUN mv phantomjs-$PHANTOM_JS /usr/local/share
RUN ln -sf /usr/local/share/phantom-$PHANTOM_JS/bin/phantomjs /usr/local/bin
RUN phantomjs --version
