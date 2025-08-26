# Dockerfile for SEM Data Generator - Railway Platform Deployment
FROM r-base:4.4.1

# Set environment variables for Railway
ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libcairo2-dev \
    libxt-dev \
    zlib1g-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy R package installation script
COPY install_packages.R .

# Install R packages (optimized for faster installation)
RUN Rscript install_packages.R

# Copy application files
COPY app_customizable.R .
COPY app_simple.R .
COPY generate_data.R .

# Create a startup script for Railway
RUN echo '#!/bin/bash\n\
# Startup script for Railway deployment\n\
echo "Starting SEM Data Generator on port $PORT"\n\
Rscript -e "library(shiny); runApp('\''app_customizable.R'\'', host='\''0.0.0.0'\'', port=as.numeric(Sys.getenv('\''PORT'\'', 5000)))"' > start.sh && \
    chmod +x start.sh

# Expose the port
EXPOSE 5000

# Health check for Railway
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:$PORT/ || exit 1

# Start the application
CMD ["./start.sh"]
