# Simple SEM Data Generator - Command Line Version
# Generates realistic SEM data and saves as CSV

# Set up library path
user_lib <- file.path(getwd(), "R_libs")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

# Generate the SEM data function from the original script
generate_sem_data <- function(n = 310, seed = 12345) {
  set.seed(seed)
  
  cat("Generating SEM data with n =", n, "and seed =", seed, "\n")
  
  # Response bias and data quality parameters
  response_bias_prob <- 0.12
  acquiescence_strength <- 0.3
  extreme_response_prob <- 0.08
  midpoint_bias_prob <- 0.15
  missing_data_prob <- 0.00  # No missing data as requested
  outlier_prob <- 0.04
  social_desirability_strength <- 0.25
  fatigue_effect_strength <- 0.15
  
  # Factor loadings with realistic variation
  tsri_loadings <- list(
    satisfaction = c(0.78, 0.82, 0.61, 0.80, 0.77),
    instrumental_help = c(0.58, 0.76, 0.81, 0.74, 0.79),
    conflict = c(0.72, 0.85, 0.79, 0.55)
  )
  
  uwes_loadings <- list(
    vigor = c(0.76, 0.63, 0.74, 0.78, 0.82, 0.75),
    dedication = c(0.84, 0.87, 0.81, 0.83, 0.68),
    absorption = c(0.77, 0.79, 0.82, 0.59, 0.80, 0.78)
  )
  
  est_loadings <- list(
    cognitive_empathy = c(0.71, 0.74, 0.76, 0.52, 0.79, 0.72, 0.77, 0.75, 0.78),
    negative_affective = c(0.68, 0.72, 0.75, 0.70, 0.49),
    positive_affective = c(0.80, 0.83, 0.78, 0.62, 0.79)
  )
  
  tas_loadings <- list(
    donation = c(0.84, 0.87, 0.82),
    emergency_help = c(0.79, 0.81, 0.54),
    everyday_help = c(0.74, 0.76, 0.78, 0.72, 0.75, 0.58),
    social_responsibility = c(0.71, 0.73, 0.76, 0.74, 0.77, 0.53)
  )
  
  cat("Creating latent factor scores...\n")
  
  # Create latent factor scores with realistic imperfections
  altruism_donation <- rnorm(n, 0, 1)
  altruism_emergency <- 0.58 * altruism_donation + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
  altruism_everyday <- 0.72 * altruism_donation + sqrt(1 - 0.72^2) * rnorm(n, 0, 1)
  altruism_social <- 0.61 * altruism_donation + sqrt(1 - 0.61^2) * rnorm(n, 0, 1)
  
  empathy_cognitive <- rnorm(n, 0, 1)
  empathy_negative <- 0.32 * empathy_cognitive + sqrt(1 - 0.32^2) * rnorm(n, 0, 1)
  empathy_positive <- 0.58 * empathy_cognitive + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
  
  # Create composite scores with realistic noise
  altruism_composite <- (altruism_donation + altruism_emergency + altruism_everyday + altruism_social) / 4
  empathy_composite <- (empathy_cognitive + empathy_negative + empathy_positive) / 3
  
  # Add method variance
  method_factor1 <- rnorm(n, 0, 0.3)
  method_factor2 <- rnorm(n, 0, 0.25)
  
  # Generate TSR factors with realistic complexity
  tsr_satisfaction <- 0.35 * altruism_composite + 0.31 * empathy_composite + 
                     0.15 * method_factor1 + sqrt(1 - 0.35^2 - 0.31^2 - 0.15^2) * rnorm(n, 0, 1)
  tsr_instrumental <- 0.74 * tsr_satisfaction + 0.12 * method_factor1 + 
                     sqrt(1 - 0.74^2 - 0.12^2) * rnorm(n, 0, 1)
  tsr_conflict <- -0.58 * tsr_satisfaction + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
  
  # Generate Work Engagement with realistic complexity
  we_vigor <- 0.28 * altruism_composite + 0.25 * empathy_composite + 0.42 * tsr_satisfaction + 
             0.11 * method_factor1 + sqrt(1 - 0.28^2 - 0.25^2 - 0.42^2 - 0.11^2) * rnorm(n, 0, 1)
  we_dedication <- 0.81 * we_vigor + sqrt(1 - 0.81^2) * rnorm(n, 0, 1)
  we_absorption <- 0.75 * we_vigor + sqrt(1 - 0.75^2) * rnorm(n, 0, 1)
  
  cat("Generating observed indicators...\n")
  
  # Function to generate observed indicators with realistic response patterns
  generate_indicators <- function(latent_scores, loadings, scale_range, reverse_code = FALSE, 
                                 item_position = 1, scale_name = "unknown") {
    indicators <- matrix(NA, nrow = length(latent_scores), ncol = length(loadings))
    
    # Generate response bias indicators for each person
    has_acquiescence <- runif(n) < response_bias_prob
    has_extreme_response <- runif(n) < extreme_response_prob
    has_midpoint_bias <- runif(n) < midpoint_bias_prob
    
    # Survey fatigue increases with item position
    fatigue_effect <- pmin(0.4, (item_position - 1) * fatigue_effect_strength / 50)
    
    # Social desirability varies by scale
    social_desirability <- ifelse(scale_name %in% c("TAS", "EST"), 
                                 social_desirability_strength, 0.1)
    
    for (i in 1:length(loadings)) {
      error_var <- 1 - loadings[i]^2
      
      # Add some cross-loadings for realism
      cross_loading_noise <- rnorm(length(latent_scores), 0, 0.15)
      
      true_score <- loadings[i] * latent_scores + sqrt(error_var) * rnorm(length(latent_scores), 0, 1) +
                    cross_loading_noise
      
      # Apply response biases
      bias_adjusted_score <- true_score
      
      # Acquiescence bias (tendency to agree)
      bias_adjusted_score[has_acquiescence] <- bias_adjusted_score[has_acquiescence] + 
                                              rnorm(sum(has_acquiescence), acquiescence_strength, 0.2)
      
      # Social desirability bias
      bias_adjusted_score <- bias_adjusted_score + 
                            rnorm(length(bias_adjusted_score), social_desirability, 0.15)
      
      # Survey fatigue (reduces variance and shifts toward midpoint)
      fatigue_noise <- rnorm(length(bias_adjusted_score), 0, fatigue_effect)
      bias_adjusted_score <- bias_adjusted_score * (1 - fatigue_effect) + fatigue_noise
      
      # Transform to appropriate scale with heteroscedasticity
      scale_variance <- runif(length(bias_adjusted_score), 0.8, 1.2)
      
      if (scale_range[1] == 1 && scale_range[2] == 5) {  # 5-point scale
        raw_scores <- pnorm(bias_adjusted_score * scale_variance) * 4 + 1
        indicators[, i] <- round(raw_scores)
      } else if (scale_range[1] == 0 && scale_range[2] == 6) {  # 7-point scale
        raw_scores <- pnorm(bias_adjusted_score * scale_variance) * 6
        indicators[, i] <- round(raw_scores)
      } else if (scale_range[1] == 1 && scale_range[2] == 4) {  # 4-point scale
        raw_scores <- pnorm(bias_adjusted_score * scale_variance) * 3 + 1
        indicators[, i] <- round(raw_scores)
      }
      
      # Apply extreme response bias (push toward ends)
      extreme_indices <- has_extreme_response & runif(length(has_extreme_response)) < 0.6
      indicators[extreme_indices, i] <- ifelse(indicators[extreme_indices, i] > mean(scale_range),
                                             scale_range[2], scale_range[1])
      
      # Apply midpoint bias (push toward middle)
      midpoint_indices <- has_midpoint_bias & runif(length(has_midpoint_bias)) < 0.4
      midpoint_value <- round(mean(scale_range))
      indicators[midpoint_indices, i] <- midpoint_value
      
      # Apply bounds
      indicators[, i] <- pmax(scale_range[1], pmin(scale_range[2], indicators[, i]))
      
      # Reverse code if needed
      if (reverse_code) {
        indicators[, i] <- scale_range[2] + scale_range[1] - indicators[, i]
      }
      
      # Add occasional outliers
      outlier_indices <- runif(length(latent_scores)) < outlier_prob
      if (sum(outlier_indices) > 0) {
        scale_values <- scale_range[1]:scale_range[2]
        # Ensure we have enough probabilities for all scale values
        if (length(scale_values) == 4) {
          probs <- c(0.4, 0.2, 0.2, 0.2)  # 4-point scale
        } else if (length(scale_values) == 5) {
          probs <- c(0.4, 0.15, 0.15, 0.15, 0.15)  # 5-point scale
        } else if (length(scale_values) == 7) {
          probs <- c(0.3, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1)  # 7-point scale
        } else {
          probs <- rep(1/length(scale_values), length(scale_values))  # Equal probabilities
        }
        outlier_values <- sample(scale_values, sum(outlier_indices), replace = TRUE, prob = probs)
        indicators[outlier_indices, i] <- outlier_values
      }
    }
    
    # Add missing data
    missing_indices <- matrix(runif(n * length(loadings)) < missing_data_prob, 
                             nrow = n, ncol = length(loadings))
    indicators[missing_indices] <- NA
    
    return(indicators)
  }
  
  # Generate all observed indicators
  # TSRI-C (5-point scale: 1-5)
  tsri_sat <- generate_indicators(tsr_satisfaction, tsri_loadings$satisfaction, c(1, 5), 
                                 item_position = 1:5, scale_name = "TSRI")
  tsri_ih <- generate_indicators(tsr_instrumental, tsri_loadings$instrumental_help, c(1, 5),
                                item_position = 6:10, scale_name = "TSRI")
  tsri_con <- generate_indicators(tsr_conflict, tsri_loadings$conflict, c(1, 5), reverse_code = TRUE,
                                 item_position = 11:14, scale_name = "TSRI")
  
  # UWES (7-point scale: 0-6)
  uwes_vig <- generate_indicators(we_vigor, uwes_loadings$vigor, c(0, 6),
                                 item_position = 15:20, scale_name = "UWES")
  uwes_ded <- generate_indicators(we_dedication, uwes_loadings$dedication, c(0, 6),
                                 item_position = 21:25, scale_name = "UWES")
  uwes_abs <- generate_indicators(we_absorption, uwes_loadings$absorption, c(0, 6),
                                 item_position = 26:31, scale_name = "UWES")
  
  # EST (4-point scale: 1-4)
  est_cog <- generate_indicators(empathy_cognitive, est_loadings$cognitive_empathy, c(1, 4),
                                item_position = 32:40, scale_name = "EST")
  est_neg <- generate_indicators(empathy_negative, est_loadings$negative_affective, c(1, 4),
                                item_position = 41:45, scale_name = "EST")
  est_pos <- generate_indicators(empathy_positive, est_loadings$positive_affective, c(1, 4),
                                item_position = 46:50, scale_name = "EST")
  
  # TAS (5-point scale: 1-5)
  tas_don <- generate_indicators(altruism_donation, tas_loadings$donation, c(1, 5),
                                item_position = 51:53, scale_name = "TAS")
  tas_emh <- generate_indicators(altruism_emergency, tas_loadings$emergency_help, c(1, 5),
                                item_position = 54:56, scale_name = "TAS")
  tas_evh <- generate_indicators(altruism_everyday, tas_loadings$everyday_help, c(1, 5),
                                item_position = 57:62, scale_name = "TAS")
  tas_srs <- generate_indicators(altruism_social, tas_loadings$social_responsibility, c(1, 5),
                                item_position = 63:68, scale_name = "TAS")
  
  cat("Assembling final dataset...\n")
  
  # Create final dataset
  data <- data.frame(
    # TSRI-C items
    TSRI_1 = tsri_sat[, 1], TSRI_3 = tsri_sat[, 2], TSRI_5 = tsri_sat[, 3], 
    TSRI_13 = tsri_sat[, 4], TSRI_14 = tsri_sat[, 5],
    TSRI_2 = tsri_ih[, 1], TSRI_6 = tsri_ih[, 2], TSRI_9 = tsri_ih[, 3], 
    TSRI_10 = tsri_ih[, 4], TSRI_12 = tsri_ih[, 5],
    TSRI_4 = tsri_con[, 1], TSRI_7 = tsri_con[, 2], TSRI_8 = tsri_con[, 3], 
    TSRI_11 = tsri_con[, 4],
    
    # UWES items
    UWES_1 = uwes_vig[, 1], UWES_4 = uwes_vig[, 2], UWES_8 = uwes_vig[, 3], 
    UWES_12 = uwes_vig[, 4], UWES_15 = uwes_vig[, 5], UWES_17 = uwes_vig[, 6],
    UWES_2 = uwes_ded[, 1], UWES_5 = uwes_ded[, 2], UWES_7 = uwes_ded[, 3], 
    UWES_10 = uwes_ded[, 4], UWES_13 = uwes_ded[, 5],
    UWES_3 = uwes_abs[, 1], UWES_6 = uwes_abs[, 2], UWES_9 = uwes_abs[, 3], 
    UWES_11 = uwes_abs[, 4], UWES_14 = uwes_abs[, 5], UWES_16 = uwes_abs[, 6],
    
    # EST items
    EST_1 = est_cog[, 1], EST_2 = est_cog[, 2], EST_3 = est_cog[, 3], 
    EST_4 = est_cog[, 4], EST_5 = est_cog[, 5], EST_6 = est_cog[, 6],
    EST_7 = est_cog[, 7], EST_8 = est_cog[, 8], EST_9 = est_cog[, 9],
    EST_10 = est_neg[, 1], EST_11 = est_neg[, 2], EST_12 = est_neg[, 3], 
    EST_13 = est_neg[, 4], EST_14 = est_neg[, 5],
    EST_15 = est_pos[, 1], EST_16 = est_pos[, 2], EST_17 = est_pos[, 3], 
    EST_18 = est_pos[, 4], EST_19 = est_pos[, 5],
    
    # TAS items
    TAS_1 = tas_don[, 1], TAS_2 = tas_don[, 2], TAS_3 = tas_don[, 3],
    TAS_4 = tas_emh[, 1], TAS_5 = tas_emh[, 2], TAS_6 = tas_emh[, 3],
    TAS_7 = tas_evh[, 1], TAS_8 = tas_evh[, 2], TAS_9 = tas_evh[, 3], 
    TAS_10 = tas_evh[, 4], TAS_11 = tas_evh[, 5], TAS_13 = tas_evh[, 6],
    TAS_12 = tas_srs[, 1], TAS_14 = tas_srs[, 2], TAS_15 = tas_srs[, 3], 
    TAS_16 = tas_srs[, 4], TAS_17 = tas_srs[, 5], TAS_18 = tas_srs[, 6]
  )
  
  return(data)
}

# Generate data and save to CSV
cat("=== SEM Data Generator ===\n\n")
data <- generate_sem_data(n = 310, seed = 12345)

# Check data structure and statistics
cat("Dataset generated successfully!\n")
cat("Dimensions:", nrow(data), "cases,", ncol(data), "variables\n")

# Calculate missing data percentage
missing_pct <- sum(is.na(data)) / (nrow(data) * ncol(data)) * 100
cat("Missing data percentage:", round(missing_pct, 2), "%\n")

# Check for response patterns
extreme_responses <- rowSums(data == 1 | data == 5 | data == 0 | data == 6 | data == 4, na.rm = TRUE)
cat("Average extreme responses per person:", round(mean(extreme_responses), 2), "\n")

# Check variance patterns
item_variances <- apply(data, 2, var, na.rm = TRUE)
cat("Range of item variances:", round(min(item_variances, na.rm = TRUE), 3), "to", 
    round(max(item_variances, na.rm = TRUE), 3), "\n\n")

# Save the dataset
output_file <- "realistic_sem_data.csv"
write.csv(data, output_file, row.names = FALSE)
cat("Data saved to:", output_file, "\n")

# Display first few rows
cat("\nFirst 10 rows of the generated data:\n")
print(head(data, 10))

cat("\nData generation complete!\n")