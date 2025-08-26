# NUCLEAR OPTION: Get semPlot working at ALL COSTS
FROM --platform=linux/amd64 rocker/r-ver:4.4.1 AS builder

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
    libgraphviz-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Configure for maximum performance
RUN echo 'options(repos=c(CRAN="https://packagemanager.posit.co/cran/latest"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

WORKDIR /app

# Layer 1: Core R packages
RUN Rscript -e "install.packages(c('shiny', 'MASS'), Ncpus=parallel::detectCores())"

# Layer 2: SEM essentials  
RUN Rscript -e "install.packages(c('lavaan', 'psych'), Ncpus=parallel::detectCores())"

# Layer 3: Statistical modeling base
RUN Rscript -e "install.packages(c('nloptr', 'lme4'), Ncpus=parallel::detectCores())"

# Layer 4: OpenMx dependencies (THE HEAVY STUFF)
RUN Rscript -e "install.packages(c('Matrix', 'digest', 'MASS', 'methods', 'parallel', 'stats', 'utils', 'boot', 'lattice', 'nlme', 'survival', 'car', 'pbivnorm', 'snowfall', 'BH', 'RcppEigen', 'Rcpp', 'StanHeaders', 'lifecycle'), Ncpus=parallel::detectCores())"

# Layer 5: FORCE OpenMx installation (the troublemaker)
RUN Rscript -e "install.packages('OpenMx', Ncpus=1, type='source', configure.args='--enable-openmp')" || \
    Rscript -e "install.packages('OpenMx', repos='http://openmx.ssri.psu.edu/packages/')" || \
    Rscript -e "remotes::install_github('OpenMx/OpenMx')" || \
    echo "OpenMx failed but continuing..."

# Layer 6: semPlot dependencies (now that OpenMx might exist)
RUN Rscript -e "install.packages(c('qgraph', 'plyr', 'XML', 'png', 'fdrtool', 'colorspace', 'corpcor', 'mi', 'Amelia', 'boot', 'foreign', 'huge', 'rockchalk', 'arm', 'abind', 'mnormt', 'pbivnorm', 'sem'), Ncpus=parallel::detectCores(), dependencies=TRUE)"

# Layer 7: semTools 
RUN Rscript -e "install.packages('semTools', Ncpus=parallel::detectCores())"

# Layer 8: THE MOMENT OF TRUTH - semPlot installation
RUN Rscript -e "install.packages('semPlot', Ncpus=1, dependencies=TRUE, type='source')" || \
    Rscript -e "remotes::install_github('SachaEpskamp/semPlot')" || \
    echo "semPlot still failed but we tried everything..."

# Layer 9: Other packages
RUN Rscript -e "install.packages(c('DT', 'ggplot2', 'tibble', 'viridis'), Ncpus=parallel::detectCores())"
RUN Rscript -e "install.packages('Hmisc', Ncpus=parallel::detectCores())"

# BRUTAL verification - check if OpenMx AND semPlot work
RUN Rscript -e "
critical_packages <- c('shiny', 'lavaan', 'psych', 'lme4')
holy_grail <- c('OpenMx', 'semPlot')

for (pkg in critical_packages) { 
  if (!requireNamespace(pkg, quietly = TRUE)) { 
    stop(paste('CRITICAL:', pkg, 'missing')) 
  } else {
    cat('✅ CRITICAL:', pkg, 'verified\n')
  }
}

for (pkg in holy_grail) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat('🏆 HOLY GRAIL:', pkg, 'FINALLY WORKS!\n')
  } else {
    cat('💀 STILL FAILED:', pkg, 'refuses to install\n')
  }
}

cat('🎯 Nuclear installation attempt complete\n')
"

# Stage 2: Runtime image
FROM --platform=linux/amd64 rocker/shiny:4.4.1

ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

# Runtime dependencies (keep it minimal)
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libblas3 \
    liblapack3 \
    libopenblas-base \
    libgfortran5 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the nuclear arsenal
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy app files
COPY app_customizable.R .
COPY app_simple.R .
COPY generate_data.R .

# Startup script
RUN echo '#!/bin/bash\n\
echo "🚀 Starting SEM Data Generator with NUCLEAR SEMPLOT on port $PORT"\n\
exec R -e "shiny::runApp(\"app_customizable.R\", host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", 5000)))"' > start.sh && \
    chmod +x start.sh

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:$PORT/ || exit 1

EXPOSE 5000
CMD ["./start.sh"]
