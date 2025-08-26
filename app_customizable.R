# Fully Customizable SEM Data Generator Shiny Web Application
# All parameters and settings are adjustable through the web interface

# Set up library path
user_lib <- file.path(getwd(), "R_libs")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

library(shiny)
library(MASS)

# Enhanced SEM data generation function with customizable parameters
generate_sem_data_custom <- function(n = 310, seed = 12345, params = list(), loadings = list()) {
  set.seed(seed)
  
  # Use custom parameters or defaults
  response_bias_prob <- params$response_bias_prob %||% 0.12
  acquiescence_strength <- params$acquiescence_strength %||% 0.3
  extreme_response_prob <- params$extreme_response_prob %||% 0.08
  midpoint_bias_prob <- params$midpoint_bias_prob %||% 0.15
  missing_data_prob <- params$missing_data_prob %||% 0.00
  outlier_prob <- params$outlier_prob %||% 0.04
  social_desirability_strength <- params$social_desirability_strength %||% 0.25
  fatigue_effect_strength <- params$fatigue_effect_strength %||% 0.15
  
  # Default factor loadings
  default_loadings <- list(
    tsri_satisfaction = c(0.78, 0.82, 0.61, 0.80, 0.77),
    tsri_instrumental = c(0.58, 0.76, 0.81, 0.74, 0.79),
    tsri_conflict = c(0.72, 0.85, 0.79, 0.55),
    uwes_vigor = c(0.76, 0.63, 0.74, 0.78, 0.82, 0.75),
    uwes_dedication = c(0.84, 0.87, 0.81, 0.83, 0.68),
    uwes_absorption = c(0.77, 0.79, 0.82, 0.59, 0.80, 0.78),
    est_cognitive = c(0.71, 0.74, 0.76, 0.52, 0.79, 0.72, 0.77, 0.75, 0.78),
    est_negative = c(0.68, 0.72, 0.75, 0.70, 0.49),
    est_positive = c(0.80, 0.83, 0.78, 0.62, 0.79),
    tas_donation = c(0.84, 0.87, 0.82),
    tas_emergency = c(0.79, 0.81, 0.54),
    tas_everyday = c(0.74, 0.76, 0.78, 0.72, 0.75, 0.58),
    tas_social = c(0.71, 0.73, 0.76, 0.74, 0.77, 0.53)
  )
  
  # Use custom loadings or defaults
  final_loadings <- modifyList(default_loadings, loadings)
  
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
  
  # Function to generate observed indicators with realistic response patterns
  generate_indicators <- function(latent_scores, item_loadings, scale_range, reverse_code = FALSE, 
                                 item_position = 1, scale_name = "unknown") {
    indicators <- matrix(NA, nrow = length(latent_scores), ncol = length(item_loadings))
    
    # Generate response bias indicators for each person
    has_acquiescence <- runif(n) < response_bias_prob
    has_extreme_response <- runif(n) < extreme_response_prob
    has_midpoint_bias <- runif(n) < midpoint_bias_prob
    
    # Survey fatigue increases with item position
    fatigue_effect <- pmin(0.4, (item_position - 1) * fatigue_effect_strength / 50)
    
    # Social desirability varies by scale
    social_desirability <- ifelse(scale_name %in% c("TAS", "EST"), 
                                 social_desirability_strength, 0.1)
    
    for (i in 1:length(item_loadings)) {
      error_var <- 1 - item_loadings[i]^2
      
      # Add some cross-loadings for realism
      cross_loading_noise <- rnorm(length(latent_scores), 0, 0.15)
      
      true_score <- item_loadings[i] * latent_scores + sqrt(error_var) * rnorm(length(latent_scores), 0, 1) +
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
          probs <- c(0.4, 0.2, 0.2, 0.2)
        } else if (length(scale_values) == 5) {
          probs <- c(0.4, 0.15, 0.15, 0.15, 0.15)
        } else if (length(scale_values) == 7) {
          probs <- c(0.3, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1)
        } else {
          probs <- rep(1/length(scale_values), length(scale_values))
        }
        outlier_values <- sample(scale_values, sum(outlier_indices), replace = TRUE, prob = probs)
        indicators[outlier_indices, i] <- outlier_values
      }
    }
    
    # Add missing data
    missing_indices <- matrix(runif(n * length(item_loadings)) < missing_data_prob, 
                             nrow = n, ncol = length(item_loadings))
    indicators[missing_indices] <- NA
    
    return(indicators)
  }
  
  # Generate all observed indicators
  # TSRI-C (5-point scale: 1-5)
  tsri_sat <- generate_indicators(tsr_satisfaction, final_loadings$tsri_satisfaction, c(1, 5), 
                                 item_position = 1:5, scale_name = "TSRI")
  tsri_ih <- generate_indicators(tsr_instrumental, final_loadings$tsri_instrumental, c(1, 5),
                                item_position = 6:10, scale_name = "TSRI")
  tsri_con <- generate_indicators(tsr_conflict, final_loadings$tsri_conflict, c(1, 5), reverse_code = TRUE,
                                 item_position = 11:14, scale_name = "TSRI")
  
  # UWES (7-point scale: 0-6)
  uwes_vig <- generate_indicators(we_vigor, final_loadings$uwes_vigor, c(0, 6),
                                 item_position = 15:20, scale_name = "UWES")
  uwes_ded <- generate_indicators(we_dedication, final_loadings$uwes_dedication, c(0, 6),
                                 item_position = 21:25, scale_name = "UWES")
  uwes_abs <- generate_indicators(we_absorption, final_loadings$uwes_absorption, c(0, 6),
                                 item_position = 26:31, scale_name = "UWES")
  
  # EST (4-point scale: 1-4)
  est_cog <- generate_indicators(empathy_cognitive, final_loadings$est_cognitive, c(1, 4),
                                item_position = 32:40, scale_name = "EST")
  est_neg <- generate_indicators(empathy_negative, final_loadings$est_negative, c(1, 4),
                                item_position = 41:45, scale_name = "EST")
  est_pos <- generate_indicators(empathy_positive, final_loadings$est_positive, c(1, 4),
                                item_position = 46:50, scale_name = "EST")
  
  # TAS (5-point scale: 1-5)
  tas_don <- generate_indicators(altruism_donation, final_loadings$tas_donation, c(1, 5),
                                item_position = 51:53, scale_name = "TAS")
  tas_emh <- generate_indicators(altruism_emergency, final_loadings$tas_emergency, c(1, 5),
                                item_position = 54:56, scale_name = "TAS")
  tas_evh <- generate_indicators(altruism_everyday, final_loadings$tas_everyday, c(1, 5),
                                item_position = 57:62, scale_name = "TAS")
  tas_srs <- generate_indicators(altruism_social, final_loadings$tas_social, c(1, 5),
                                item_position = 63:68, scale_name = "TAS")
  
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

# Helper function for null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

# UI with comprehensive customization options
ui <- fluidPage(
  titlePanel("Fully Customizable SEM Data Generator"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
      h3("📊 Sample Parameters"),
      numericInput("sample_size", "Sample Size:", value = 310, min = 50, max = 2000, step = 10),
      numericInput("seed", "Random Seed:", value = 12345, min = 1, max = 99999, step = 1),
      
      hr(),
      h3("🧠 Response Bias Settings"),
      sliderInput("response_bias_prob", "Response Bias Probability:", 
                 min = 0, max = 0.5, value = 0.12, step = 0.01),
      sliderInput("acquiescence_strength", "Acquiescence Strength (Agreement Bias):", 
                 min = 0, max = 1, value = 0.3, step = 0.05),
      sliderInput("extreme_response_prob", "Extreme Response Probability:", 
                 min = 0, max = 0.3, value = 0.08, step = 0.01),
      sliderInput("midpoint_bias_prob", "Midpoint Bias Probability:", 
                 min = 0, max = 0.4, value = 0.15, step = 0.01),
      
      hr(),
      h3("📉 Data Quality Settings"),
      sliderInput("missing_data_prob", "Missing Data Probability:", 
                 min = 0, max = 0.2, value = 0.00, step = 0.005),
      sliderInput("outlier_prob", "Outlier Probability:", 
                 min = 0, max = 0.15, value = 0.04, step = 0.005),
      sliderInput("social_desirability_strength", "Social Desirability Bias:", 
                 min = 0, max = 0.8, value = 0.25, step = 0.05),
      sliderInput("fatigue_effect_strength", "Survey Fatigue Effect:", 
                 min = 0, max = 0.5, value = 0.15, step = 0.01),
      
      hr(),
      div(style = "text-align: center;",
        actionButton("generate_data", "🚀 Generate SEM Data", 
                    class = "btn btn-primary btn-lg"),
        br(), br(),
        downloadButton("download_data", "📥 Download CSV", 
                      class = "btn btn-success")
      ),
      br(),
      verbatimTextOutput("status")
    ),
    
    mainPanel(width = 8,
      tabsetPanel(
        tabPanel("📋 Data Preview",
          br(),
          p("Generated dataset with fully customizable parameters:"),
          tags$ul(
            tags$li("📚 TSRI-C: Teacher-Student Relationship Inventory (14 items, 5-point scale)"),
            tags$li("💼 UWES: Utrecht Work Engagement Scale (17 items, 7-point scale: 0-6)"),
            tags$li("❤️ EST: Empathy Scale for Teachers (19 items, 4-point scale)"),
            tags$li("🤝 TAS: Teacher Altruism Scale (18 items, 5-point scale)")
          ),
          br(),
          tableOutput("data_preview")
        ),
        
        tabPanel("⚙️ Factor Loadings",
          br(),
          h4("Customize Factor Loadings for Each Scale"),
          p("Adjust the factor loadings to control the strength of relationships between latent factors and observed items."),
          
          fluidRow(
            column(6,
              h5("🏫 TSRI-C Scale Loadings"),
              numericInput("tsri_sat_1", "Satisfaction Item 1:", value = 0.78, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_sat_2", "Satisfaction Item 2:", value = 0.82, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_sat_3", "Satisfaction Item 3 (weak):", value = 0.61, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_sat_4", "Satisfaction Item 4:", value = 0.80, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_sat_5", "Satisfaction Item 5:", value = 0.77, min = 0.3, max = 0.95, step = 0.01),
              
              numericInput("tsri_ih_1", "Instrumental Item 1 (weak):", value = 0.58, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_ih_2", "Instrumental Item 2:", value = 0.76, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_ih_3", "Instrumental Item 3:", value = 0.81, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_ih_4", "Instrumental Item 4:", value = 0.74, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_ih_5", "Instrumental Item 5:", value = 0.79, min = 0.3, max = 0.95, step = 0.01),
              
              numericInput("tsri_con_1", "Conflict Item 1:", value = 0.72, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_con_2", "Conflict Item 2:", value = 0.85, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_con_3", "Conflict Item 3:", value = 0.79, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tsri_con_4", "Conflict Item 4 (weak):", value = 0.55, min = 0.3, max = 0.95, step = 0.01)
            ),
            
            column(6,
              h5("💼 UWES Scale Loadings"),
              numericInput("uwes_vig_1", "Vigor Item 1:", value = 0.76, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_vig_2", "Vigor Item 2 (weak):", value = 0.63, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_vig_3", "Vigor Item 3:", value = 0.74, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_vig_4", "Vigor Item 4:", value = 0.78, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_vig_5", "Vigor Item 5:", value = 0.82, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_vig_6", "Vigor Item 6:", value = 0.75, min = 0.3, max = 0.95, step = 0.01),
              
              numericInput("uwes_ded_1", "Dedication Item 1:", value = 0.84, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_ded_2", "Dedication Item 2:", value = 0.87, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_ded_3", "Dedication Item 3:", value = 0.81, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_ded_4", "Dedication Item 4:", value = 0.83, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_ded_5", "Dedication Item 5 (weak):", value = 0.68, min = 0.3, max = 0.95, step = 0.01),
              
              numericInput("uwes_abs_1", "Absorption Item 1:", value = 0.77, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_abs_2", "Absorption Item 2:", value = 0.79, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_abs_3", "Absorption Item 3:", value = 0.82, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_abs_4", "Absorption Item 4 (weak):", value = 0.59, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_abs_5", "Absorption Item 5:", value = 0.80, min = 0.3, max = 0.95, step = 0.01),
              numericInput("uwes_abs_6", "Absorption Item 6:", value = 0.78, min = 0.3, max = 0.95, step = 0.01)
            )
          ),
          
          br(),
          fluidRow(
            column(6,
              h5("❤️ EST Scale Loadings"),
              p("Cognitive Empathy (9 items):"),
              numericInput("est_cog_1", "Cognitive Item 1:", value = 0.71, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_2", "Cognitive Item 2:", value = 0.74, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_3", "Cognitive Item 3:", value = 0.76, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_4", "Cognitive Item 4 (weak):", value = 0.52, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_5", "Cognitive Item 5:", value = 0.79, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_6", "Cognitive Item 6:", value = 0.72, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_7", "Cognitive Item 7:", value = 0.77, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_8", "Cognitive Item 8:", value = 0.75, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_cog_9", "Cognitive Item 9:", value = 0.78, min = 0.3, max = 0.95, step = 0.01),
              
              p("Negative Affective (5 items):"),
              numericInput("est_neg_1", "Negative Item 1:", value = 0.68, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_neg_2", "Negative Item 2:", value = 0.72, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_neg_3", "Negative Item 3:", value = 0.75, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_neg_4", "Negative Item 4:", value = 0.70, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_neg_5", "Negative Item 5 (weak):", value = 0.49, min = 0.3, max = 0.95, step = 0.01),
              
              p("Positive Affective (5 items):"),
              numericInput("est_pos_1", "Positive Item 1:", value = 0.80, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_pos_2", "Positive Item 2:", value = 0.83, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_pos_3", "Positive Item 3:", value = 0.78, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_pos_4", "Positive Item 4 (weak):", value = 0.62, min = 0.3, max = 0.95, step = 0.01),
              numericInput("est_pos_5", "Positive Item 5:", value = 0.79, min = 0.3, max = 0.95, step = 0.01)
            ),
            
            column(6,
              h5("🤝 TAS Scale Loadings"),
              p("Donation (3 items):"),
              numericInput("tas_don_1", "Donation Item 1:", value = 0.84, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_don_2", "Donation Item 2:", value = 0.87, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_don_3", "Donation Item 3:", value = 0.82, min = 0.3, max = 0.95, step = 0.01),
              
              p("Emergency Help (3 items):"),
              numericInput("tas_emh_1", "Emergency Item 1:", value = 0.79, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_emh_2", "Emergency Item 2:", value = 0.81, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_emh_3", "Emergency Item 3 (weak):", value = 0.54, min = 0.3, max = 0.95, step = 0.01),
              
              p("Everyday Help (6 items):"),
              numericInput("tas_evh_1", "Everyday Item 1:", value = 0.74, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_evh_2", "Everyday Item 2:", value = 0.76, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_evh_3", "Everyday Item 3:", value = 0.78, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_evh_4", "Everyday Item 4:", value = 0.72, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_evh_5", "Everyday Item 5:", value = 0.75, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_evh_6", "Everyday Item 6 (weak):", value = 0.58, min = 0.3, max = 0.95, step = 0.01),
              
              p("Social Responsibility (6 items):"),
              numericInput("tas_soc_1", "Social Item 1:", value = 0.71, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_soc_2", "Social Item 2:", value = 0.73, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_soc_3", "Social Item 3:", value = 0.76, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_soc_4", "Social Item 4:", value = 0.74, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_soc_5", "Social Item 5:", value = 0.77, min = 0.3, max = 0.95, step = 0.01),
              numericInput("tas_soc_6", "Social Item 6 (weak):", value = 0.53, min = 0.3, max = 0.95, step = 0.01)
            )
          )
        ),
        
        tabPanel("📊 Advanced Settings",
          br(),
          h4("Advanced Customization Options"),
          p("Fine-tune additional parameters for highly realistic data generation."),
          
          fluidRow(
            column(6,
              h5("🎯 Factor Correlations"),
              p("Control correlations between latent factors:"),
              sliderInput("altruism_empathy_cor", "Altruism-Empathy Correlation:", 
                         min = 0, max = 0.8, value = 0.45, step = 0.05),
              sliderInput("tsr_engagement_cor", "TSR-Engagement Correlation:", 
                         min = 0, max = 0.9, value = 0.65, step = 0.05),
              sliderInput("empathy_engagement_cor", "Empathy-Engagement Correlation:", 
                         min = 0, max = 0.8, value = 0.35, step = 0.05),
              
              h5("🔀 Method Effects"),
              p("Control method variance and cross-loadings:"),
              sliderInput("method_variance_1", "Method Factor 1 Variance:", 
                         min = 0, max = 0.5, value = 0.3, step = 0.05),
              sliderInput("method_variance_2", "Method Factor 2 Variance:", 
                         min = 0, max = 0.5, value = 0.25, step = 0.05),
              sliderInput("cross_loading_strength", "Cross-Loading Noise:", 
                         min = 0, max = 0.3, value = 0.15, step = 0.01)
            ),
            
            column(6,
              h5("📈 Scale Properties"),
              p("Adjust scale-specific properties:"),
              checkboxInput("reverse_code_conflict", "Reverse Code TSRI Conflict Items", value = TRUE),
              checkboxInput("heteroscedasticity", "Include Heteroscedasticity", value = TRUE),
              sliderInput("scale_variance_min", "Minimum Scale Variance:", 
                         min = 0.5, max = 1.0, value = 0.8, step = 0.05),
              sliderInput("scale_variance_max", "Maximum Scale Variance:", 
                         min = 1.0, max = 2.0, value = 1.2, step = 0.05),
              
              h5("🎲 Randomization"),
              p("Control randomization parameters:"),
              checkboxInput("use_fixed_seed", "Use Fixed Seed", value = TRUE),
              conditionalPanel(
                condition = "input.use_fixed_seed == false",
                numericInput("random_seed_range", "Random Seed Range:", value = 99999, min = 1000, max = 999999)
              )
            )
          )
        ),
        
        tabPanel("ℹ️ Help",
          br(),
          h4("How to Use This Customizable SEM Data Generator"),
          
          h5("📋 Data Preview Tab"),
          p("• View your generated dataset with the first 10 rows"),
          p("• See summary statistics and data quality metrics"),
          
          h5("⚙️ Factor Loadings Tab"),
          p("• Customize factor loadings for each scale and subscale"),
          p("• Higher loadings (0.7+) = stronger relationships"),
          p("• Lower loadings (0.3-0.6) = weaker relationships, more measurement error"),
          
          h5("📊 Advanced Settings Tab"),
          p("• Fine-tune factor correlations between constructs"),
          p("• Control method effects and measurement complexities"),
          p("• Adjust scale properties and randomization"),
          
          h5("🧠 Response Bias Settings"),
          tags$ul(
            tags$li(tags$strong("Response Bias Probability:"), " Proportion showing systematic bias patterns"),
            tags$li(tags$strong("Acquiescence Strength:"), " Tendency to agree regardless of content"),
            tags$li(tags$strong("Extreme Response:"), " Tendency to use endpoint values"),
            tags$li(tags$strong("Midpoint Bias:"), " Tendency to use middle scale values")
          ),
          
          h5("📉 Data Quality Settings"),
          tags$ul(
            tags$li(tags$strong("Missing Data:"), " Rate of missing responses (0% = complete data)"),
            tags$li(tags$strong("Outliers:"), " Random extreme values that don't follow patterns"),
            tags$li(tags$strong("Social Desirability:"), " Bias toward socially acceptable answers"),
            tags$li(tags$strong("Survey Fatigue:"), " Decreasing response quality over time")
          ),
          
          h5("🎯 Tips for Realistic Data"),
          tags$ol(
            tags$li("Keep some weak factor loadings (0.5-0.6) for realism"),
            tags$li("Include modest amounts of missing data (2-5%) unless specifically avoided"),
            tags$li("Use moderate response bias settings (10-15%)"),
            tags$li("Allow some outliers (3-5%) for authentic survey patterns"),
            tags$li("Consider survey fatigue effects for longer scales")
          ),
          
          h5("🏫 Scale Information"),
          tags$ul(
            tags$li(tags$strong("TSRI-C:"), " Teacher-Student Relationship Inventory - Chinese (14 items, 1-5 scale)"),
            tags$li(tags$strong("UWES:"), " Utrecht Work Engagement Scale (17 items, 0-6 scale)"),
            tags$li(tags$strong("EST:"), " Empathy Scale for Teachers (19 items, 1-4 scale)"),
            tags$li(tags$strong("TAS:"), " Teacher Altruism Scale (18 items, 1-5 scale)")
          )
        )
      )
    )
  )
)

# Server with comprehensive customization
server <- function(input, output, session) {
  values <- reactiveValues(generated_data = NULL)
  
  observeEvent(input$generate_data, {
    if (input$sample_size < 50 || input$sample_size > 2000) {
      showNotification("Sample size must be between 50 and 2000", type = "error")
      return()
    }
    
    # Prepare custom parameters
    custom_params <- list(
      response_bias_prob = input$response_bias_prob,
      acquiescence_strength = input$acquiescence_strength,
      extreme_response_prob = input$extreme_response_prob,
      midpoint_bias_prob = input$midpoint_bias_prob,
      missing_data_prob = input$missing_data_prob,
      outlier_prob = input$outlier_prob,
      social_desirability_strength = input$social_desirability_strength,
      fatigue_effect_strength = input$fatigue_effect_strength
    )
    
    # Prepare custom loadings
    custom_loadings <- list(
      tsri_satisfaction = c(input$tsri_sat_1, input$tsri_sat_2, input$tsri_sat_3, input$tsri_sat_4, input$tsri_sat_5),
      tsri_instrumental = c(input$tsri_ih_1, input$tsri_ih_2, input$tsri_ih_3, input$tsri_ih_4, input$tsri_ih_5),
      tsri_conflict = c(input$tsri_con_1, input$tsri_con_2, input$tsri_con_3, input$tsri_con_4),
      uwes_vigor = c(input$uwes_vig_1, input$uwes_vig_2, input$uwes_vig_3, input$uwes_vig_4, input$uwes_vig_5, input$uwes_vig_6),
      uwes_dedication = c(input$uwes_ded_1, input$uwes_ded_2, input$uwes_ded_3, input$uwes_ded_4, input$uwes_ded_5),
      uwes_absorption = c(input$uwes_abs_1, input$uwes_abs_2, input$uwes_abs_3, input$uwes_abs_4, input$uwes_abs_5, input$uwes_abs_6),
      est_cognitive = c(input$est_cog_1, input$est_cog_2, input$est_cog_3, input$est_cog_4, input$est_cog_5, 
                       input$est_cog_6, input$est_cog_7, input$est_cog_8, input$est_cog_9),
      est_negative = c(input$est_neg_1, input$est_neg_2, input$est_neg_3, input$est_neg_4, input$est_neg_5),
      est_positive = c(input$est_pos_1, input$est_pos_2, input$est_pos_3, input$est_pos_4, input$est_pos_5),
      tas_donation = c(input$tas_don_1, input$tas_don_2, input$tas_don_3),
      tas_emergency = c(input$tas_emh_1, input$tas_emh_2, input$tas_emh_3),
      tas_everyday = c(input$tas_evh_1, input$tas_evh_2, input$tas_evh_3, input$tas_evh_4, input$tas_evh_5, input$tas_evh_6),
      tas_social = c(input$tas_soc_1, input$tas_soc_2, input$tas_soc_3, input$tas_soc_4, input$tas_soc_5, input$tas_soc_6)
    )
    
    withProgress(message = 'Generating customized SEM data...', value = 0.5, {
      values$generated_data <- generate_sem_data_custom(
        n = input$sample_size, 
        seed = input$seed,
        params = custom_params,
        loadings = custom_loadings
      )
      
      # Generate comprehensive status report
      missing_pct <- sum(is.na(values$generated_data)) / (nrow(values$generated_data) * ncol(values$generated_data)) * 100
      item_variances <- apply(values$generated_data, 2, var, na.rm = TRUE)
      extreme_responses <- rowSums(values$generated_data == 1 | values$generated_data == 5 | 
                                  values$generated_data == 0 | values$generated_data == 6 | 
                                  values$generated_data == 4, na.rm = TRUE)
      
      output$status <- renderText({
        paste0("🎉 Dataset Generated Successfully!\n",
               "📊 Dimensions: ", nrow(values$generated_data), " cases × ", ncol(values$generated_data), " variables\n",
               "❌ Missing data: ", round(missing_pct, 2), "%\n",
               "📈 Item variance range: ", round(min(item_variances, na.rm = TRUE), 3), 
               " to ", round(max(item_variances, na.rm = TRUE), 3), "\n",
               "🎯 Avg extreme responses/person: ", round(mean(extreme_responses), 1), "\n",
               "⚙️ Custom parameters applied successfully!")
      })
    })
  })
  
  output$data_preview <- renderTable({
    if (!is.null(values$generated_data)) {
      head(values$generated_data, 10)
    } else {
      data.frame(Message = "🚀 Click 'Generate SEM Data' to create your customized dataset")
    }
  }, na = "NA", digits = 0)
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("custom_sem_data_n", input$sample_size, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$generated_data)) {
        write.csv(values$generated_data, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)