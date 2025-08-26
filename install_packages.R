# Install required packages for SEM Data Generator Shiny App
# Optimized version that compiles from source to ensure cross-platform compatibility.

cat("Starting R package installation...\n")
cat("R version:", R.version.string, "\n")

# --- Use Standard CRAN Mirror to Compile from Source ---
# This ensures that packages are built for the Docker container's architecture,
# avoiding binary compatibility issues when building on ARM-based machines.
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# --- Consolidated Package List ---
# Essential packages for core functionality, including dependencies that caused prior build failures.
# The order is important here to resolve dependencies correctly (nloptr -> lme4 -> arm/rockchalk).
essential_packages <- c(
  "shiny",
  "lavaan",      # Critical for SEM functionality
  "psych",       # Factor analysis, reliability, correlations
  "MASS",
  "nloptr",      # Dependency for lme4
  "lme4"         # Dependency for arm and rockchalk
)

# Optional packages for enhanced features and UI improvements.
optional_packages <- c(
  "DT",
  "ggplot2",
  "semPlot",     # For SEM plotting
  "semTools",    # Additional SEM utilities
  "arm",         # Depends on lme4
  "rockchalk",   # Depends on lme4
  "tibble",
  "viridis",
  "Hmisc"
)

# --- Robust Installation Function ---
# Function to install packages with error handling and retries.
install_with_retry <- function(packages, max_retries = 3) {
  for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat("Package already available:", pkg, "\n")
      next
    }
    
    cat("Installing package:", pkg, "\n")
    for (retry in 1:max_retries) {
      tryCatch({
        # Install from source
        install.packages(pkg,
                         dependencies = TRUE,
                         Ncpus = 1) 
        cat("Successfully installed:", pkg, "\n")
        break # Success, exit retry loop
      }, error = function(e) {
        cat("Attempt", retry, "failed for package", pkg, ":", e$message, "\n")
        if (retry == max_retries) {
          cat("Failed to install", pkg, "after", max_retries, "attempts\n")
        } else {
          Sys.sleep(2) # Wait before retrying
        }
      })
    }
  }
}

# --- Installation and Verification ---
# Install essential packages first
cat("\n--- Installing essential packages ---\n")
install_with_retry(essential_packages)

# Install optional packages
cat("\n--- Installing optional packages ---\n")
install_with_retry(optional_packages)

# Verify all installations
cat("\n--- Verifying package installations ---\n")
all_packages <- c(essential_packages, optional_packages)
success_count <- 0

for (pkg in all_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("✓ Successfully loaded:", pkg, "\n")
    success_count <- success_count + 1
  } else {
    cat("✗ Failed to load:", pkg, "\n")
  }
}

cat("\n--- Installation Summary ---\n")
cat("Successfully loaded", success_count, "out of", length(all_packages), "packages\n")

# Critical check for essential packages
critical_packages_check <- c("shiny", "lavaan", "psych", "lme4")
for (pkg in critical_packages_check) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("CRITICAL:", pkg, "package is not available. Deployment will fail."))
  } else {
    cat("✓ CRITICAL:", pkg, "package is available and ready\n")
  }
}

cat("\nPackage installation complete!\n")
