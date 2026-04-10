FROM rocker/shiny:4.2.1

WORKDIR /srv/shiny-server

ENV RENV_PATHS_ROOT=renv
ENV RENV_PATHS_LIBRARY=renv/library

RUN apt-get update && apt-get install -y \
    cmake \
    libnlopt-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install('ggtree', ask = FALSE)"

COPY program/renv.lock /srv/shiny-server/
RUN R -e "install.packages('renv'); renv::restore()"

COPY program/shinyApp /srv/shiny-server/shinyApp

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/shinyApp', host='0.0.0.0', port=3838)"]

