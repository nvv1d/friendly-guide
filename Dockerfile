# Multi-Stage Build: SEM Data Generator with semPlot
FROM --platform=linux/amd64 rocker/shiny:4.4.1 AS builder

ENV DEBIAN_FRONTEND=noninteractive

# Enhanced system dependencies for semPlot
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
    && rm -rf /var/lib/apt/lists/*

# Configure Posit binaries
RUN echo 'options(repos=c(CRAN="https://packagemanager.posit.co/cran/latest"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

WORKDIR /app

# Layer 1: Core packages
RUN Rscript -e "install.packages(c('shiny', 'MASS'), Ncpus=parallel::detectCores())"

# Layer 2: SEM essentials
RUN Rscript -e "install.packages(c('lavaan', 'psych'), Ncpus=parallel::detectCores())"

# Layer 3: Statistical modeling
RUN Rscript -e "install.packages(c('nloptr', 'lme4'), Ncpus=parallel::detectCores())"

# Layer 4: Graphics and plotting dependencies
RUN Rscript -e "install.packages(c('qgraph', 'plyr', 'XML', 'png', 'fdrtool', 'colorspace', 'corpcor'), Ncpus=parallel::detectCores())"

# Layer 5: semTools (usually works fine)
RUN Rscript -e "install.packages('semTools', Ncpus=parallel::detectCores())"

# Layer 6: semPlot (with all deps satisfied)
RUN Rscript -e "install.packages('semPlot', Ncpus=parallel::detectCores(), dependencies=TRUE)"

# Layer 7: Additional modeling
RUN Rscript -e "install.packages(c('arm', 'rockchalk'), Ncpus=parallel::detectCores())"

# Layer 8: UI packages
RUN Rscript -e "install.packages(c('DT', 'ggplot2', 'tibble', 'viridis'), Ncpus=parallel::detectCores())"

# Layer 9: Final heavy package
RUN Rscript -e "install.packages('Hmisc', Ncpus=parallel::detectCores())"

# Verification - now including semPlot
RUN Rscript -e "critical_packages <- c('shiny', 'lavaan', 'psych', 'lme4', 'semPlot'); for (pkg in critical_packages) { if (!requireNamespace(pkg, quietly = TRUE)) { stop(paste('CRITICAL:', pkg, 'package missing')) } }; cat('✅ All critical packages including semPlot verified\n')"

# Stage 2: Final image
FROM --platform=linux/amd64 rocker/shiny:4.4.1

ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

# Runtime graphics dependencies for semPlot
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
echo "🚀 Starting SEM Data Generator with semPlot on port $PORT"\n\
exec R -e "shiny::runApp(\"app_customizable.R\", host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", 5000)))"' > start.sh && \
    chmod +x start.sh

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:$PORT/ || exit 1

EXPOSE 5000
CMD ["./start.sh"]
