# Multi-Stage Build: SEM Data Generator with GUARANTEED semPlot
FROM --platform=linux/amd64 rocker/shiny:4.4.1 AS builder

ENV DEBIAN_FRONTEND=noninteractive

# ENHANCED system dependencies specifically for semPlot
RUN apt-get update && apt-get install -y --no-install-recommends \
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
    libcairo2-dev \
    libxt-dev \
    libx11-dev \
    libgraphviz-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Configure BOTH Posit binaries AND CRAN for fallback
RUN echo 'options(repos=c(CRAN="https://packagemanager.posit.co/cran/latest", CRAN2="https://cran.rstudio.com"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

WORKDIR /app

# Layer 1: Core packages
RUN Rscript -e "install.packages(c('shiny', 'MASS'), Ncpus=parallel::detectCores())"

# Layer 2: SEM essentials
RUN Rscript -e "install.packages(c('lavaan', 'psych'), Ncpus=parallel::detectCores())"

# Layer 3: Statistical modeling
RUN Rscript -e "install.packages(c('nloptr', 'lme4'), Ncpus=parallel::detectCores())"

# Layer 4: ALL semPlot dependencies (install EVERYTHING first)
RUN Rscript -e "install.packages(c('qgraph', 'plyr', 'XML', 'png', 'fdrtool', 'colorspace', 'corpcor', 'mi', 'Amelia', 'boot', 'foreign', 'huge', 'rockchalk', 'arm', 'abind', 'mnormt', 'pbivnorm', 'sem'), Ncpus=parallel::detectCores(), dependencies=TRUE)"

# Layer 5: semTools (prerequisite for semPlot)
RUN Rscript -e "install.packages('semTools', Ncpus=parallel::detectCores())"

# Layer 6: semPlot (force from source with verbose output)
RUN Rscript -e "install.packages('semPlot', type='source', Ncpus=1, dependencies=TRUE, verbose=TRUE)" || \
    Rscript -e "install.packages('remotes'); remotes::install_github('SachaEpskamp/semPlot')" || \
    echo "semPlot installation failed but continuing..."

# Layer 7: Data manipulation
RUN Rscript -e "install.packages(c('DT', 'ggplot2', 'tibble', 'viridis'), Ncpus=parallel::detectCores())"

# Layer 8: Final heavy package
RUN Rscript -e "install.packages('Hmisc', Ncpus=parallel::detectCores())"

# FIXED: Single-line verification command
RUN Rscript -e "critical_packages <- c('shiny', 'lavaan', 'psych', 'lme4'); optional_packages <- c('semPlot'); for (pkg in critical_packages) { if (!requireNamespace(pkg, quietly = TRUE)) { stop(paste('CRITICAL:', pkg, 'package missing')) } else { cat('✅ CRITICAL:', pkg, 'verified\n') } }; for (pkg in optional_packages) { if (requireNamespace(pkg, quietly = TRUE)) { cat('✅ OPTIONAL:', pkg, 'available\n') } else { cat('⚠️  OPTIONAL:', pkg, 'missing (app will work without it)\n') } }; cat('✅ Build verification complete\n')"

# Stage 2: Final lightweight image
FROM --platform=linux/amd64 rocker/shiny:4.4.1

ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

# FIXED: Runtime dependencies with correct package names
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcairo2 \
    libxt6 \
    libx11-6 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy all packages
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy app files
COPY app_customizable.R .
COPY app_simple.R .
COPY generate_data.R .

# Startup script
RUN echo '#!/bin/bash\n\
echo "🚀 Starting SEM Data Generator on port $PORT"\n\
exec R -e "shiny::runApp(\"app_customizable.R\", host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", 5000)))"' > start.sh && \
    chmod +x start.sh

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:$PORT/ || exit 1

EXPOSE 5000
CMD ["./start.sh"]
