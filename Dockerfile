# Multi-Stage Build: Optimized SEM Data Generator
# Stage 1: Builder with Posit binaries
FROM --platform=linux/amd64 rocker/shiny:4.4.1 AS builder

# Set environment for non-interactive installs
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies (minimal set)
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
    && rm -rf /var/lib/apt/lists/*

# Configure R to use Posit Package Manager (precompiled binaries)
RUN echo 'options(repos=c(CRAN="https://packagemanager.posit.co/cran/latest"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

# Install packages in staged layers (for Docker caching)
WORKDIR /app

# Layer 1: Core Shiny and base stats
RUN Rscript -e "install.packages(c('shiny', 'MASS'), Ncpus=parallel::detectCores())"

# Layer 2: Core SEM packages
RUN Rscript -e "install.packages(c('lavaan', 'psych'), Ncpus=parallel::detectCores())"

# Layer 3: Statistical modeling (these have heavy deps)
RUN Rscript -e "install.packages(c('nloptr', 'lme4'), Ncpus=parallel::detectCores())"

# Layer 4: SEM tools (handle semPlot gracefully)
RUN Rscript -e "install.packages('semTools', Ncpus=parallel::detectCores())"

# Layer 5: Additional modeling packages
RUN Rscript -e "install.packages(c('arm', 'rockchalk'), Ncpus=parallel::detectCores())"

# Layer 6: Data manipulation and visualization
RUN Rscript -e "install.packages(c('DT', 'ggplot2', 'tibble', 'viridis'), Ncpus=parallel::detectCores())"

# Layer 7: Final heavy package
RUN Rscript -e "install.packages('Hmisc', Ncpus=parallel::detectCores())"

# Layer 8: Optional semPlot (may fail, but won't stop build)
RUN Rscript -e "tryCatch(install.packages('semPlot', Ncpus=parallel::detectCores()), error=function(e) cat('⚠️ semPlot installation failed, but continuing...\n'))"

# Verification step - Only check essential packages
RUN Rscript -e "critical_packages <- c('shiny', 'lavaan', 'psych', 'lme4'); for (pkg in critical_packages) { if (!requireNamespace(pkg, quietly = TRUE)) { stop(paste('CRITICAL:', pkg, 'package missing')) } }; cat('✅ All critical packages verified\n')"

# Stage 2: Final lightweight image
FROM --platform=linux/amd64 rocker/shiny:4.4.1

ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

# Install minimal runtime dependencies only
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy precompiled R packages from builder
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy application files
COPY app_customizable.R .
COPY app_simple.R .
COPY generate_data.R .

# Create startup script
RUN echo '#!/bin/bash\n\
echo "🚀 Starting SEM Data Generator on port $PORT"\n\
exec R -e "shiny::runApp(\"app_customizable.R\", host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", 5000)))"' > start.sh && \
    chmod +x start.sh

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:$PORT/ || exit 1

EXPOSE 5000

CMD ["./start.sh"]
