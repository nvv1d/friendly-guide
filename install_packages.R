# Install required packages for SEM Data Generator Shiny App
# Optimized for faster installation

# Set user library path in Replit environment
user_lib <- file.path(getwd(), "R_libs")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE)
}
.libPaths(c(user_lib, .libPaths()))

# Essential packages only for faster installation
essential_packages <- c(
  "shiny",
  "MASS"  # Already available in base R typically
)

# Optional packages for enhanced features
optional_packages <- c(
  "DT",
  "ggplot2"
)

# Install essential packages first
cat("Installing essential packages:", paste(essential_packages, collapse = ", "), "\n")
new_essential <- essential_packages[!(essential_packages %in% installed.packages()[,"Package"])]
if(length(new_essential)) {
  install.packages(new_essential, 
                  lib = user_lib, 
                  repos = "https://cran.rstudio.com/",
                  dependencies = c("Depends", "Imports"),  # Minimal dependencies
                  Ncpus = parallel::detectCores())
}

# Install optional packages
cat("Installing optional packages:", paste(optional_packages, collapse = ", "), "\n")
new_optional <- optional_packages[!(optional_packages %in% installed.packages()[,"Package"])]
if(length(new_optional)) {
  install.packages(new_optional, 
                  lib = user_lib, 
                  repos = "https://cran.rstudio.com/",
                  dependencies = c("Depends", "Imports"),  # Minimal dependencies
                  Ncpus = parallel::detectCores())
}

# Test installation
all_packages <- c(essential_packages, optional_packages)
success <- TRUE
for (pkg in all_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Package not available:", pkg, "\n")
  } else {
    cat("Loaded successfully:", pkg, "\n")
  }
}

cat("Installation complete! You can run the app with the available packages.\n")
