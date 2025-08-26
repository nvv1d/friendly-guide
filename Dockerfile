# Nuclear semPlot Dockerfile for GitHub Actions - FIXED
FROM --platform=linux/amd64 rocker/r-ver:4.5.0 AS builder

ENV DEBIAN_FRONTEND=noninteractive

# MASSIVE system dependencies for OpenMx/semPlot
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
    && rm -rf /var/lib/apt/lists/*

# Configure repos
RUN echo 'options(repos=c(CRAN="https://packagemanager.posit.co/cran/latest"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

WORKDIR /app

# Core packages
RUN Rscript -e "install.packages(c('shiny', 'MASS', 'lavaan', 'psych', 'nloptr', 'lme4'), Ncpus=parallel::detectCores())"

# OpenMx dependencies first
RUN Rscript -e "install.packages(c('Matrix', 'digest', 'boot', 'lattice', 'nlme', 'survival', 'car', 'pbivnorm', 'BH', 'RcppEigen', 'Rcpp', 'lifecycle'), Ncpus=parallel::detectCores())"

# FORCE OpenMx (the troublemaker)
RUN Rscript -e "install.packages('OpenMx', Ncpus=2, type='source')" || echo "OpenMx failed, trying alternatives..." && \
    Rscript -e "install.packages('OpenMx', repos='http://openmx.ssri.psu.edu/packages/')" || \
    echo "OpenMx installation failed but continuing..."

# semPlot dependencies
RUN Rscript -e "install.packages(c('qgraph', 'plyr', 'XML', 'png', 'fdrtool', 'colorspace', 'corpcor', 'mi', 'Amelia', 'foreign', 'huge', 'rockchalk', 'arm', 'abind', 'mnormt', 'pbivnorm', 'sem'), Ncpus=parallel::detectCores())"

# semTools
RUN Rscript -e "install.packages('semTools', Ncpus=parallel::detectCores())"

# THE MOMENT OF TRUTH - semPlot
RUN Rscript -e "install.packages('semPlot', Ncpus=2, dependencies=TRUE)" || \
    echo "semPlot failed but app will work without it"

# Other packages
RUN Rscript -e "install.packages(c('DT', 'ggplot2', 'tibble', 'viridis', 'Hmisc'), Ncpus=parallel::detectCores())"

# FIXED: Single-line verification command
RUN Rscript -e "critical <- c('shiny', 'lavaan', 'psych', 'lme4'); holy_grail <- c('OpenMx', 'semPlot'); for (pkg in critical) { if (!requireNamespace(pkg, quietly = TRUE)) { stop(paste('CRITICAL:', pkg, 'missing')) } else { cat('✅ CRITICAL:', pkg, 'verified\n') } }; for (pkg in holy_grail) { if (requireNamespace(pkg, quietly = TRUE)) { cat('🏆 HOLY GRAIL:', pkg, 'SUCCESS!\n') } else { cat('💔 FAILED:', pkg, 'missing\n') } }; cat('🎯 Verification complete\n')"

# Runtime image
FROM --platform=linux/amd64 rocker/shiny:4.5.0

ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl libblas3 liblapack3 libopenblas-base libgfortran5 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the arsenal
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy app files
COPY app_customizable.R .
COPY app_simple.R .
COPY generate_data.R .

# Startup script
RUN echo '#!/bin/bash\n\
echo "🚀 Starting SEM Data Generator with GitHub Actions Magic!"\n\
exec R -e "shiny::runApp(\"app_customizable.R\", host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", 5000)))"' > start.sh && \
    chmod +x start.sh

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:$PORT/ || exit 1

EXPOSE 5000
CMD ["./start.sh"]
