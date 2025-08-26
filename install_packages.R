# Optimized R Package Installation for SEM Data Generator
# Uses Posit Package Manager binaries for fast installation

cat("🔧 Configuring R for binary package installation...\n")

# Configure Posit Package Manager for precompiled binaries
options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"),
  Ncpus = parallel::detectCores()
)

cat("📦 Installing SEM Data Generator packages...\n")

# All packages in dependency order
packages <- c(
  # Core
  "shiny", "MASS",
  # SEM essentials  
  "lavaan", "psych",
  # Statistical modeling
  "nloptr", "lme4", 
  # Extended SEM
  "semPlot", "semTools",
  # Additional modeling
  "arm", "rockchalk",
  # UI and visualization
  "DT", "ggplot2", "tibble", "viridis",
  # Statistical utilities
  "Hmisc"
)

# Install all packages (binaries install very fast)
install.packages(packages)

# Verify critical packages
cat("✅ Verifying installation...\n")
critical <- c("shiny", "lavaan", "psych", "lme4", "semPlot")

for (pkg in critical) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("❌ CRITICAL:", pkg, "package missing"))
  } else {
    cat("✅", pkg, "ready\n")
  }
}

cat("🎉 Package installation complete!\n")
