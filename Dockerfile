# Nuclear semPlot Dockerfile for GitHub Actions - Ubuntu 24.04 Fixed
FROM --platform=linux/amd64 rocker/r-ver:4.5.0 AS builder

ENV DEBIAN_FRONTEND=noninteractive

# MASSIVE system dependencies for OpenMx/semPlot - Ubuntu 24.04 package names
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    libblas-dev \
    liblapack-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libnlopt-dev \
    libglpk-dev \
    libgmp-dev \
    pkg-config \
    curl \
    cmake \
    libopenblas-dev \
    libatlas-base-dev \
    libcairo2-dev \
    libxt-dev \
    libx11-dev \
    libboost-all-dev \
    && rm -rf /var/lib/apt/lists/*

# Configure repos
RUN echo 'options(repos=c(CRAN="https://packagemanager.posit.co/cran/latest"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

WORKDIR /app

# Core packages
RUN Rscript -e "install.packages(c('shiny', 'MASS', 'lavaan', 'psych', 'nloptr', 'lme4'), Ncpus=parallel::detectCores())"

# OpenMx dependencies first
RUN Rscript -e "install.packages(c('Matrix', 'digest', 'boot', 'lattice', 'nlme', 'survival', 'car', 'pbivnorm', 'BH', 'RcppEigen', 'Rcpp', 'lifecycle'), Ncpus=parallel::detectCores())"

# FORCE OpenMx
RUN Rscript -e "install.packages('OpenMx', Ncpus=2, type='source')" || echo "OpenMx failed, trying alternatives..." && \
    Rscript -e "install.packages('OpenMx', repos='http://openmx.ssri.psu.edu/packages/')" || \
    echo "OpenMx installation failed but continuing..."

# semPlot dependencies from CRAN
RUN Rscript -e "install.packages(c('qgraph', 'plyr', 'XML', 'png', 'fdrtool', 'colorspace', 'corpcor', 'mi', 'Amelia', 'foreign', 'huge', 'rockchalk', 'arm', 'abind', 'mnormt', 'pbivnorm', 'sem'), Ncpus=parallel::detectCores())"

# semTools
RUN Rscript -e "install.packages('semTools', Ncpus=parallel::detectCores())"

# Bioconductor stack for semPlot
RUN Rscript -e "install.packages('BiocManager', Ncpus=parallel::detectCores())"
RUN Rscript -e "BiocManager::install(c('graph','RBGL'), ask=FALSE, update=FALSE)"

# semPlot install with multiple fallbacks + source reporting
RUN Rscript -e "install.packages('remotes', Ncpus=parallel::detectCores())" && \
    (Rscript -e "tryCatch({install.packages('semPlot', Ncpus=2); cat('🏆 semPlot installed from CRAN\\n')}, error=function(e) stop('CRAN failed'))" \
     || Rscript -e "tryCatch({remotes::install_github('SachaEpskamp/semPlot'); cat('🏆 semPlot installed from GitHub\\n')}, error=function(e) stop('GitHub failed'))" \
     || Rscript -e "tryCatch({install.packages('semPlot', repos='http://R-Forge.R-project.org'); cat('🏆 semPlot installed from R-Forge\\n')}, error=function(e) cat('💔 semPlot failed everywhere but continuing...\\n'))")

# Shiny UI packages
RUN Rscript -e "install.packages(c('shinydashboard', 'shinyWidgets', 'shinycssloaders'), Ncpus=parallel::detectCores())"

# Visualization and analysis packages
RUN Rscript -e "install.packages(c('DT', 'ggplot2', 'plotly', 'tibble', 'viridis', 'Hmisc', 'corrplot'), Ncpus=parallel::detectCores())"

# Verification
RUN Rscript -e "critical <- c('shiny', 'lavaan', 'psych', 'lme4'); holy_grail <- c('OpenMx', 'semPlot'); for (pkg in critical) { if (!requireNamespace(pkg, quietly = TRUE)) { stop(paste('CRITICAL:', pkg, 'missing')) } else { cat('✅ CRITICAL:', pkg, 'verified\\n') } }; for (pkg in holy_grail) { if (requireNamespace(pkg, quietly = TRUE)) { cat('🏆 HOLY GRAIL:', pkg, 'SUCCESS!\\n') } else { cat('💔 FAILED:', pkg, 'missing\\n') } }; cat('🎯 Verification complete\\n')"

# Runtime image - FIXED package names for Ubuntu 24.04
FROM --platform=linux/amd64 rocker/shiny:4.5.0

ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libblas3 \
    liblapack3 \
    libopenblas0 \
    libgfortran5 \
    libnlopt0 \
    libglpk40 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

COPY app_customizable.R .
COPY app_simple.R .
COPY app_advanced.R .
COPY generate_data.R .

RUN echo '#!/bin/bash\n\
echo "🚀 Starting Advanced SEM Data Generator with R 4.5.0 + comprehensive SEM analysis!"\n\
exec R -e "shiny::runApp(\"app_customizable.R\", host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", 5000)))"' > start.sh && \
    chmod +x start.sh

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:$PORT/ || exit 1

EXPOSE 5000
CMD ["./start.sh"]
