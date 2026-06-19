# syntax=docker/dockerfile:1
# cOmicsArt — single image, two modes (see start.sh):
#   MODE=app      -> Shiny app on 3838
#   MODE=rstudio  -> RStudio Server on 8787 (development)
#
# RStudio base (gives us RStudio Server for dev mode). The Shiny app itself
# runs via shiny::runApp, with the shiny package provided by renv::restore.
# rocker/rstudio:4.2.1 is amd64-only, so we pin the platform: on Apple Silicon
# this builds/runs under emulation (Docker Desktop + Rosetta). The whole
# pipeline (Docker Hub image, GitHub Actions runners) is amd64 anyway.
FROM --platform=linux/amd64 rocker/rstudio:4.2.1

WORKDIR /srv/shiny-server

# Restore packages into the global site library so EVERY R session — the app,
# and any RStudio session for any user — finds them on the default .libPaths()
# with no renv auto-activation needed.
ENV RENV_PATHS_CACHE=/srv/shiny-server/renv/cache

RUN apt-get update -o Acquire::Retries=5 \
 && apt-get install -y --no-install-recommends \
    ca-certificates \
    # build toolchain (very commonly needed)
    build-essential \
    gfortran \
    make \
    pkg-config \
    cmake \
    # networking + crypto (httr/curl/openssl)
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    # XML/HTML parsing (xml2/rvest)
    libxml2-dev \
    # compression (lots of packages)
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    # git-backed installs (remotes/devtools)
    git \
    libgit2-dev \
    # database + misc common deps
    libsqlite3-dev \
    libpq-dev \
    # image/plot rendering stack (ggplot, ragg, magick, text)
    libcairo2-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libmagick++-dev \
    # V8 / JS tooling (some htmlwidgets, etc.)
    libv8-dev \
    # spatial / units (often pulled in indirectly)
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    # Java (some packages need a JDK)
    default-jdk \
    # special dep for nloptr
    libnlopt-dev \
 && rm -rf /var/lib/apt/lists/*

# Copy ONLY the lockfile first so the (slow) restore layer is cached when only
# app code changes later.
COPY program/renv.lock /srv/shiny-server/renv.lock

# Restore the locked environment into the global site library.
# BiocManager is installed first so renv can resolve the 50 Bioconductor pkgs.
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/srv/shiny-server/renv/cache \
    R -e "install.packages(c('renv','BiocManager'), repos='https://cloud.r-project.org'); \
          BiocManager::install(version='3.16', ask=FALSE, update=FALSE); \
          renv::restore(lockfile='/srv/shiny-server/renv.lock', \
                        library='/usr/local/lib/R/site-library', prompt=FALSE)"

# Now copy the frequently-changing app code (baked default; overridden by a
# volume mount in dev/CI).
COPY program/shinyApp /srv/shiny-server/shinyApp

# Launcher that switches between app and rstudio modes.
COPY start.sh /usr/local/bin/start.sh
RUN chmod +x /usr/local/bin/start.sh

EXPOSE 3838 8787

ENTRYPOINT ["/usr/local/bin/start.sh"]
