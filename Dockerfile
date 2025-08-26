# Minimal Dockerfile for SEM Data Generator - Optimized with Multi-Stage Build
# Stage 1: The "Builder" - Installs all dependencies
FROM rocker/shiny:4.4.1 AS builder

# Set environment variables for non-interactive setup
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies required by R packages
# PROACTIVELY ADDED: Common libraries for networking, XML, and fonts to prevent future errors.
RUN apt-get update && apt-get install -y \
    libnlopt-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy and run the robust R package installation script
# This creates a layer with all the compiled R libraries.
WORKDIR /app
COPY install_packages.R .
RUN Rscript install_packages.R


# Stage 2: The "Final Image" - Creates the minimal production image
FROM rocker/shiny:4.4.1

# Set environment variables
ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

WORKDIR /app

# --- Optimized Asset Copying ---
# 1. Copy the pre-compiled R libraries from the "builder" stage.
# This is the key step that avoids reinstalling everything.
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# 2. Copy the application files
COPY app_customizable.R .
COPY app_simple.R .
COPY generate_data.R .

# Create a simple startup script
RUN echo '#!/bin/bash\n\
echo "Starting SEM Data Generator on port $PORT"\n\
exec R -e "shiny::runApp('\''app_customizable.R'\'', host='\''0.0.0.0'\'', port=as.numeric(Sys.getenv('\''PORT'\'', 5000)))"' > start.sh && \
    chmod +x start.sh

# Expose the application port
EXPOSE 5000

# Health check to ensure the application is running
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:$PORT/ || exit 1

# Start the application
CMD ["./start.sh"]
