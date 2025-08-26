# Minimal Dockerfile for SEM Data Generator - Alternative approach
FROM rocker/shiny:4.4.1

# Set environment variables
ENV PORT=5000
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Install additional R packages directly
RUN R -e "install.packages(c('MASS', 'DT', 'ggplot2'), repos='https://cran.rstudio.com/')"

# Copy application files
COPY app_customizable.R .
COPY app_simple.R .
COPY generate_data.R .

# Simple startup script
RUN echo '#!/bin/bash\n\
echo "Starting SEM Data Generator on port $PORT"\n\
exec R -e "shiny::runApp('\''app_customizable.R'\'', host='\''0.0.0.0'\'', port=as.numeric(Sys.getenv('\''PORT'\'', 5000)))"' > start.sh && \
    chmod +x start.sh

# Expose the port
EXPOSE 5000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:$PORT/ || exit 1

# Start the application
CMD ["./start.sh"]
