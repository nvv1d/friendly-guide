# Minimal Dockerfile for SEM Data Generator - Optimized Version
# Uses a robust R script for package installation.
FROM rocker/shiny:4.4.1

# Set environment variables for non-interactive setup
ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

# --- System & R Package Installation ---

# 1. Install system dependencies required by R packages (e.g., nloptr)
# This is the critical step that prevents build timeouts.
RUN apt-get update && apt-get install -y \
    libnlopt-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# 2. Copy and run the robust R package installation script
# This script contains the complete list of packages and includes retry logic.
WORKDIR /app
COPY install_packages.R .
RUN Rscript install_packages.R

# --- Application Setup ---

# Copy the rest of the application files
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
