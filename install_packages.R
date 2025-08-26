# Install required packages for SEM Data Generator Shiny App
# Fixed version for Docker deployment

cat("Starting R package installation...\n")
cat("R version:", R.version.string, "\n")
cat("Library paths:", .libPaths(), "\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Essential packages for SEM Data Generator
essential_packages <- c(
  "shiny",
  "lavaan",  # Critical for SEM functionality
  "psych",   # Factor analysis, reliability, correlations
  "MASS"
)

# Optional packages for enhanced features
optional_packages <- c(
  "DT",
  "ggplot2",
  "semPlot",   # For SEM plotting
  "semTools"   # Additional SEM utilities
)

# Function to install packages with error handling
install_with_retry <- function(packages, max_retries = 3) {
  for (pkg in packages) {
    cat("Installing package:", pkg, "\n")
    
    for (retry in 1:max_retries) {
      tryCatch({
        if (!requireNamespace(pkg, quietly = TRUE)) {
          install.packages(pkg, 
                          repos = "https://cran.rstudio.com/",
                          dependencies = TRUE,
                          Ncpus = 1)  # Single core to avoid issues
          cat("Successfully installed:", pkg, "\n")
        } else {
          cat("Package already available:", pkg, "\n")
        }
        break  # Success, exit retry loop
      }, error = function(e) {
        cat("Attempt", retry, "failed for package", pkg, ":", e$message, "\n")
        if (retry == max_retries) {
          cat("Failed to install", pkg, "after", max_retries, "attempts\n")
        } else {
          Sys.sleep(2)  # Wait before retry
        }
      })
    }
  }
}

# Install essential packages first
cat("Installing essential packages...\n")
install_with_retry(essential_packages)

# Install optional packages
cat("Installing optional packages...\n")
install_with_retry(optional_packages)

# Verify installations
cat("\nVerifying package installations:\n")
all_packages <- c(essential_packages, optional_packages)
success_count <- 0

for (pkg in all_packages) {
  tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    cat("✓ Successfully loaded:", pkg, "\n")
    success_count <- success_count + 1
  }, error = function(e) {
    cat("✗ Failed to load:", pkg, "-", e$message, "\n")
  })
}

cat("\nInstallation Summary:\n")
cat("Successfully loaded", success_count, "out of", length(all_packages), "packages\n")

# Critical check for essential SEM packages
critical_packages <- c("shiny", "lavaan", "psych")
for (pkg in critical_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("CRITICAL:", pkg, "package is not available. Deployment will fail."))
  } else {
    cat("✓ CRITICAL:", pkg, "package is available and ready\n")
  }
}

cat("Package installation complete!\n")
