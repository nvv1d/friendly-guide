# SEM Data Generator Shiny Web Application
# Based on realistic SEM data generation with customizable parameters

# Set up library path for Replit environment
user_lib <- file.path(getwd(), "R_libs")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(MASS)
library(psych)
library(lavaan)
library(shinycssloaders)
library(shinyWidgets)
library(corrplot)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "SEM Data Generator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Generation", tabName = "generation", icon = icon("database")),
      menuItem("Data Preview", tabName = "preview", icon = icon("table")),
      menuItem("Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Correlations", tabName = "correlations", icon = icon("project-diagram")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .progress-bar {
          background-color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Data Generation Tab
      tabItem(tabName = "generation",
        fluidRow(
          box(
            title = "Sample Parameters", status = "primary", solidHeader = TRUE, width = 6,
            numericInput("sample_size", "Sample Size:", value = 310, min = 50, max = 2000, step = 10),
            sliderInput("seed", "Random Seed:", min = 1, max = 99999, value = 12345, step = 1),
            hr(),
            h4("Response Bias Parameters"),
            sliderInput("response_bias_prob", "Response Bias Probability:", 
                       min = 0, max = 0.3, value = 0.12, step = 0.01),
            sliderInput("acquiescence_strength", "Acquiescence Strength:", 
                       min = 0, max = 1, value = 0.3, step = 0.05),
            sliderInput("extreme_response_prob", "Extreme Response Probability:", 
                       min = 0, max = 0.2, value = 0.08, step = 0.01)
          ),
          
          box(
            title = "Data Quality Parameters", status = "info", solidHeader = TRUE, width = 6,
            sliderInput("midpoint_bias_prob", "Midpoint Bias Probability:", 
                       min = 0, max = 0.3, value = 0.15, step = 0.01),
            sliderInput("missing_data_prob", "Missing Data Probability:", 
                       min = 0, max = 0.1, value = 0.03, step = 0.005),
            sliderInput("outlier_prob", "Outlier Probability:", 
                       min = 0, max = 0.1, value = 0.04, step = 0.005),
            sliderInput("social_desirability_strength", "Social Desirability Strength:", 
                       min = 0, max = 0.5, value = 0.25, step = 0.05),
            sliderInput("fatigue_effect_strength", "Survey Fatigue Strength:", 
                       min = 0, max = 0.3, value = 0.15, step = 0.01)
          )
        ),
        
        fluidRow(
          box(
            title = "Factor Loading Adjustments", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            column(3,
              h5("TSRI Loadings"),
              numericInput("tsri_satisfaction_weak", "Satisfaction Weak Item:", value = 0.61, min = 0.3, max = 0.9, step = 0.01),
              numericInput("tsri_instrumental_weak", "Instrumental Weak Item:", value = 0.58, min = 0.3, max = 0.9, step = 0.01),
              numericInput("tsri_conflict_weak", "Conflict Weak Item:", value = 0.55, min = 0.3, max = 0.9, step = 0.01)
            ),
            column(3,
              h5("UWES Loadings"),
              numericInput("uwes_vigor_weak", "Vigor Weak Item:", value = 0.63, min = 0.3, max = 0.9, step = 0.01),
              numericInput("uwes_dedication_weak", "Dedication Weak Item:", value = 0.68, min = 0.3, max = 0.9, step = 0.01),
              numericInput("uwes_absorption_weak", "Absorption Weak Item:", value = 0.59, min = 0.3, max = 0.9, step = 0.01)
            ),
            column(3,
              h5("EST Loadings"),
              numericInput("est_cognitive_weak", "Cognitive Weak Item:", value = 0.52, min = 0.3, max = 0.9, step = 0.01),
              numericInput("est_negative_weak", "Negative Weak Item:", value = 0.49, min = 0.3, max = 0.9, step = 0.01),
              numericInput("est_positive_weak", "Positive Weak Item:", value = 0.62, min = 0.3, max = 0.9, step = 0.01)
            ),
            column(3,
              h5("TAS Loadings"),
              numericInput("tas_emergency_weak", "Emergency Weak Item:", value = 0.54, min = 0.3, max = 0.9, step = 0.01),
              numericInput("tas_everyday_weak", "Everyday Weak Item:", value = 0.58, min = 0.3, max = 0.9, step = 0.01),
              numericInput("tas_social_weak", "Social Weak Item:", value = 0.53, min = 0.3, max = 0.9, step = 0.01)
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Generate Data", status = "success", solidHeader = TRUE, width = 12,
            div(style = "text-align: center;",
              actionButton("generate_data", "Generate SEM Data", 
                          class = "btn-success btn-lg", icon = icon("play-circle")),
              br(), br(),
              downloadButton("download_data", "Download CSV", 
                           class = "btn-primary", icon = icon("download"))
            ),
            br(),
            withSpinner(verbatimTextOutput("generation_status"), type = 4)
          )
        )
      ),
      
      # Data Preview Tab
      tabItem(tabName = "preview",
        fluidRow(
          box(
            title = "Generated Dataset Preview", status = "primary", solidHeader = TRUE, width = 12,
            withSpinner(DT::dataTableOutput("data_table"), type = 4)
          )
        )
      ),
      
      # Statistics Tab
      tabItem(tabName = "statistics",
        fluidRow(
          box(
            title = "Data Summary Statistics", status = "info", solidHeader = TRUE, width = 6,
            withSpinner(verbatimTextOutput("summary_stats"), type = 4)
          ),
          box(
            title = "Missing Data Patterns", status = "warning", solidHeader = TRUE, width = 6,
            withSpinner(verbatimTextOutput("missing_data_info"), type = 4)
          )
        ),
        
        fluidRow(
          box(
            title = "Item Variance Distribution", status = "success", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("variance_plot"), type = 4)
          ),
          box(
            title = "Response Pattern Analysis", status = "primary", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("response_pattern_plot"), type = 4)
          )
        ),
        
        fluidRow(
          box(
            title = "Scale Reliability Estimates", status = "info", solidHeader = TRUE, width = 12,
            withSpinner(verbatimTextOutput("reliability_stats"), type = 4)
          )
        )
      ),
      
      # Correlations Tab
      tabItem(tabName = "correlations",
        fluidRow(
          box(
            title = "Correlation Matrix", status = "primary", solidHeader = TRUE, width = 12,
            selectInput("correlation_subset", "Select Variables:",
                       choices = c("All Variables" = "all",
                                 "TSRI Items" = "tsri",
                                 "UWES Items" = "uwes", 
                                 "EST Items" = "est",
                                 "TAS Items" = "tas"),
                       selected = "all"),
            withSpinner(plotOutput("correlation_plot", height = "600px"), type = 4)
          )
        )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
        fluidRow(
          box(
            title = "About SEM Data Generator", status = "info", solidHeader = TRUE, width = 12,
            h3("Overview"),
            p("This application generates realistic structural equation modeling (SEM) data with customizable parameters for research and educational purposes."),
            
            h3("Generated Scales"),
            tags$ul(
              tags$li(tags$strong("TSRI-C:"), " Teacher-Student Relationship Inventory - Chinese (14 items, 5-point scale)"),
              tags$li(tags$strong("UWES:"), " Utrecht Work Engagement Scale (17 items, 7-point scale: 0-6)"),
              tags$li(tags$strong("EST:"), " Empathy Scale for Teachers (19 items, 4-point scale)"),
              tags$li(tags$strong("TAS:"), " Teacher Altruism Scale (18 items, 5-point scale)")
            ),
            
            h3("Realism Features"),
            tags$ul(
              tags$li("Response bias patterns (acquiescence, extreme responding, midpoint bias)"),
              tags$li("Missing data with realistic patterns"),
              tags$li("Survey fatigue effects"),
              tags$li("Social desirability bias"),
              tags$li("Outliers and measurement error"),
              tags$li("Varying factor loadings including weak items"),
              tags$li("Cross-loadings and method effects")
            ),
            
            h3("Parameters Guide"),
            tags$ul(
              tags$li(tags$strong("Response Bias Probability:"), " Proportion of respondents showing systematic bias (0-0.3)"),
              tags$li(tags$strong("Acquiescence Strength:"), " Tendency to agree regardless of content (0-1)"),
              tags$li(tags$strong("Missing Data Probability:"), " Rate of missing responses (0-0.1)"),
              tags$li(tags$strong("Social Desirability:"), " Bias toward socially acceptable responses (0-0.5)")
            ),
            
            h3("Usage Instructions"),
            tags$ol(
              tags$li("Adjust parameters in the Data Generation tab"),
              tags$li("Click 'Generate SEM Data' to create your dataset"),
              tags$li("Preview the data in the Data Preview tab"),
              tags$li("Examine statistics and patterns in the Statistics tab"),
              tags$li("View correlations in the Correlations tab"),
              tags$li("Download your dataset as CSV when satisfied")
            ),
            
            h3("Technical Details"),
            p("The data generation process creates realistic factor structures with appropriate psychometric properties. 
              The generated data includes all the complexities found in real survey data, making it suitable for 
              testing SEM models, reliability analysis, and methodological research.")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Reactive values to store generated data
  values <- reactiveValues(
    generated_data = NULL,
    generation_complete = FALSE,
    factor_scores = NULL
  )
  
  # Data generation function (modularized from original script)
  generate_sem_data <- function(n, params, loadings) {
    
    # Set seed for reproducibility
    set.seed(params$seed)
    
    # Create latent factor scores with realistic imperfections
    altruism_donation <- rnorm(n, 0, 1)
    altruism_emergency <- 0.58 * altruism_donation + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
    altruism_everyday <- 0.72 * altruism_donation + sqrt(1 - 0.72^2) * rnorm(n, 0, 1)
    altruism_social <- 0.61 * altruism_donation + sqrt(1 - 0.61^2) * rnorm(n, 0, 1)
    
    empathy_cognitive <- rnorm(n, 0, 1)
    empathy_negative <- 0.32 * empathy_cognitive + sqrt(1 - 0.32^2) * rnorm(n, 0, 1)
    empathy_positive <- 0.58 * empathy_cognitive + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
    
    # Create composite scores
    altruism_composite <- (altruism_donation + altruism_emergency + altruism_everyday + altruism_social) / 4
    empathy_composite <- (empathy_cognitive + empathy_negative + empathy_positive) / 3
    
    # Method variance
    method_factor1 <- rnorm(n, 0, 0.3)
    method_factor2 <- rnorm(n, 0, 0.25)
    
    # Generate TSR factors
    tsr_satisfaction <- 0.35 * altruism_composite + 0.31 * empathy_composite + 
                       0.15 * method_factor1 + sqrt(1 - 0.35^2 - 0.31^2 - 0.15^2) * rnorm(n, 0, 1)
    tsr_instrumental <- 0.74 * tsr_satisfaction + 0.12 * method_factor1 + 
                       sqrt(1 - 0.74^2 - 0.12^2) * rnorm(n, 0, 1)
    tsr_conflict <- -0.58 * tsr_satisfaction + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
    
    # Generate Work Engagement
    we_vigor <- 0.28 * altruism_composite + 0.25 * empathy_composite + 0.42 * tsr_satisfaction + 
               0.11 * method_factor1 + sqrt(1 - 0.28^2 - 0.25^2 - 0.42^2 - 0.11^2) * rnorm(n, 0, 1)
    we_dedication <- 0.81 * we_vigor + sqrt(1 - 0.81^2) * rnorm(n, 0, 1)
    we_absorption <- 0.75 * we_vigor + sqrt(1 - 0.75^2) * rnorm(n, 0, 1)
    
    # Store factor scores
    factor_scores <- data.frame(
      altruism_donation = altruism_donation,
      altruism_emergency = altruism_emergency,
      altruism_everyday = altruism_everyday,
      altruism_social = altruism_social,
      empathy_cognitive = empathy_cognitive,
      empathy_negative = empathy_negative,
      empathy_positive = empathy_positive,
      tsr_satisfaction = tsr_satisfaction,
      tsr_instrumental = tsr_instrumental,
      tsr_conflict = tsr_conflict,
      we_vigor = we_vigor,
      we_dedication = we_dedication,
      we_absorption = we_absorption
    )
    
    # Generate indicators using the helper function
    tsri_sat <- generate_indicators(tsr_satisfaction, loadings$tsri_satisfaction, c(1, 5), 
                                   item_position = 1:5, scale_name = "TSRI", params = params)
    tsri_ih <- generate_indicators(tsr_instrumental, loadings$tsri_instrumental, c(1, 5),
                                  item_position = 6:10, scale_name = "TSRI", params = params)
    tsri_con <- generate_indicators(tsr_conflict, loadings$tsri_conflict, c(1, 5), reverse_code = TRUE,
                                   item_position = 11:14, scale_name = "TSRI", params = params)
    
    uwes_vig <- generate_indicators(we_vigor, loadings$uwes_vigor, c(0, 6),
                                   item_position = 15:20, scale_name = "UWES", params = params)
    uwes_ded <- generate_indicators(we_dedication, loadings$uwes_dedication, c(0, 6),
                                   item_position = 21:25, scale_name = "UWES", params = params)
    uwes_abs <- generate_indicators(we_absorption, loadings$uwes_absorption, c(0, 6),
                                   item_position = 26:31, scale_name = "UWES", params = params)
    
    est_cog <- generate_indicators(empathy_cognitive, loadings$est_cognitive, c(1, 4),
                                  item_position = 32:40, scale_name = "EST", params = params)
    est_neg <- generate_indicators(empathy_negative, loadings$est_negative, c(1, 4),
                                  item_position = 41:45, scale_name = "EST", params = params)
    est_pos <- generate_indicators(empathy_positive, loadings$est_positive, c(1, 4),
                                  item_position = 46:50, scale_name = "EST", params = params)
    
    tas_don <- generate_indicators(altruism_donation, loadings$tas_donation, c(1, 5),
                                  item_position = 51:53, scale_name = "TAS", params = params)
    tas_emh <- generate_indicators(altruism_emergency, loadings$tas_emergency, c(1, 5),
                                  item_position = 54:56, scale_name = "TAS", params = params)
    tas_evh <- generate_indicators(altruism_everyday, loadings$tas_everyday, c(1, 5),
                                  item_position = 57:62, scale_name = "TAS", params = params)
    tas_srs <- generate_indicators(altruism_social, loadings$tas_social, c(1, 5),
                                  item_position = 63:68, scale_name = "TAS", params = params)
    
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
    
    return(list(data = data, factor_scores = factor_scores))
  }
  
  # Helper function to generate indicators (adapted from original)
  generate_indicators <- function(latent_scores, loadings, scale_range, reverse_code = FALSE, 
                                 item_position = 1, scale_name = "unknown", params) {
    n <- length(latent_scores)
    indicators <- matrix(NA, nrow = n, ncol = length(loadings))
    
    # Generate response bias indicators for each person
    has_acquiescence <- runif(n) < params$response_bias_prob
    has_extreme_response <- runif(n) < params$extreme_response_prob
    has_midpoint_bias <- runif(n) < params$midpoint_bias_prob
    
    # Survey fatigue increases with item position
    fatigue_effect <- pmin(0.4, (item_position[1] - 1) * params$fatigue_effect_strength / 50)
    
    # Social desirability varies by scale
    social_desirability <- ifelse(scale_name %in% c("TAS", "EST"), 
                                 params$social_desirability_strength, 0.1)
    
    for (i in 1:length(loadings)) {
      error_var <- 1 - loadings[i]^2
      
      # Add cross-loadings for realism
      cross_loading_noise <- rnorm(n, 0, 0.15)
      
      true_score <- loadings[i] * latent_scores + sqrt(error_var) * rnorm(n, 0, 1) + cross_loading_noise
      
      # Apply response biases
      bias_adjusted_score <- true_score
      
      # Acquiescence bias
      bias_adjusted_score[has_acquiescence] <- bias_adjusted_score[has_acquiescence] + 
                                              rnorm(sum(has_acquiescence), params$acquiescence_strength, 0.2)
      
      # Social desirability bias
      bias_adjusted_score <- bias_adjusted_score + 
                            rnorm(n, social_desirability, 0.15)
      
      # Survey fatigue
      fatigue_noise <- rnorm(n, 0, fatigue_effect)
      bias_adjusted_score <- bias_adjusted_score * (1 - fatigue_effect) + fatigue_noise
      
      # Transform to appropriate scale
      scale_variance <- runif(n, 0.8, 1.2)
      
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
      
      # Apply extreme response bias
      extreme_indices <- has_extreme_response & runif(n) < 0.6
      indicators[extreme_indices, i] <- ifelse(indicators[extreme_indices, i] > mean(scale_range),
                                             scale_range[2], scale_range[1])
      
      # Apply midpoint bias
      midpoint_indices <- has_midpoint_bias & runif(n) < 0.4
      midpoint_value <- round(mean(scale_range))
      indicators[midpoint_indices, i] <- midpoint_value
      
      # Apply bounds
      indicators[, i] <- pmax(scale_range[1], pmin(scale_range[2], indicators[, i]))
      
      # Reverse code if needed
      if (reverse_code) {
        indicators[, i] <- scale_range[2] + scale_range[1] - indicators[, i]
      }
      
      # Add occasional outliers
      outlier_indices <- runif(n) < params$outlier_prob
      if (sum(outlier_indices) > 0) {
        outlier_values <- sample(scale_range[1]:scale_range[2], sum(outlier_indices), 
                                prob = c(0.4, 0.1, 0.1, 0.1, 0.3))
        indicators[outlier_indices, i] <- outlier_values
      }
    }
    
    # Add missing data
    missing_indices <- matrix(runif(n * length(loadings)) < params$missing_data_prob, 
                             nrow = n, ncol = length(loadings))
    indicators[missing_indices] <- NA
    
    return(indicators)
  }
  
  # Generate data when button is clicked
  observeEvent(input$generate_data, {
    
    # Validate inputs
    if (input$sample_size < 50 || input$sample_size > 2000) {
      showNotification("Sample size must be between 50 and 2000", type = "error")
      return()
    }
    
    # Prepare parameters
    params <- list(
      seed = input$seed,
      response_bias_prob = input$response_bias_prob,
      extreme_response_prob = input$extreme_response_prob,
      midpoint_bias_prob = input$midpoint_bias_prob,
      missing_data_prob = input$missing_data_prob,
      outlier_prob = input$outlier_prob,
      acquiescence_strength = input$acquiescence_strength,
      social_desirability_strength = input$social_desirability_strength,
      fatigue_effect_strength = input$fatigue_effect_strength
    )
    
    # Prepare loadings
    loadings <- list(
      tsri_satisfaction = c(0.78, 0.82, input$tsri_satisfaction_weak, 0.80, 0.77),
      tsri_instrumental = c(input$tsri_instrumental_weak, 0.76, 0.81, 0.74, 0.79),
      tsri_conflict = c(0.72, 0.85, 0.79, input$tsri_conflict_weak),
      uwes_vigor = c(0.76, input$uwes_vigor_weak, 0.74, 0.78, 0.82, 0.75),
      uwes_dedication = c(0.84, 0.87, 0.81, 0.83, input$uwes_dedication_weak),
      uwes_absorption = c(0.77, 0.79, 0.82, input$uwes_absorption_weak, 0.80, 0.78),
      est_cognitive = c(0.71, 0.74, 0.76, input$est_cognitive_weak, 0.79, 0.72, 0.77, 0.75, 0.78),
      est_negative = c(0.68, 0.72, 0.75, 0.70, input$est_negative_weak),
      est_positive = c(0.80, 0.83, 0.78, input$est_positive_weak, 0.79),
      tas_donation = c(0.84, 0.87, 0.82),
      tas_emergency = c(0.79, 0.81, input$tas_emergency_weak),
      tas_everyday = c(0.74, 0.76, 0.78, 0.72, 0.75, input$tas_everyday_weak),
      tas_social = c(0.71, 0.73, 0.76, 0.74, 0.77, input$tas_social_weak)
    )
    
    withProgress(message = 'Generating SEM data...', value = 0, {
      incProgress(0.2, detail = "Setting up parameters")
      
      tryCatch({
        incProgress(0.3, detail = "Creating factor structures")
        result <- generate_sem_data(input$sample_size, params, loadings)
        
        incProgress(0.7, detail = "Processing indicators")
        values$generated_data <- result$data
        values$factor_scores <- result$factor_scores
        values$generation_complete <- TRUE
        
        incProgress(1, detail = "Complete!")
        
        # Generate status report
        missing_pct <- sum(is.na(values$generated_data)) / (nrow(values$generated_data) * ncol(values$generated_data)) * 100
        extreme_responses <- rowSums(values$generated_data == 1 | values$generated_data == 5 | 
                                   values$generated_data == 0 | values$generated_data == 6 | 
                                   values$generated_data == 4, na.rm = TRUE)
        item_variances <- apply(values$generated_data, 2, var, na.rm = TRUE)
        
        status_message <- paste(
          paste("Dataset dimensions:", nrow(values$generated_data), "cases,", ncol(values$generated_data), "variables"),
          paste("Missing data percentage:", round(missing_pct, 2), "%"),
          paste("Average extreme responses per person:", round(mean(extreme_responses), 2)),
          paste("Range of item variances:", round(min(item_variances, na.rm = TRUE), 3), "to", 
                round(max(item_variances, na.rm = TRUE), 3)),
          paste("Generation completed successfully at", Sys.time()),
          sep = "\n"
        )
        
        output$generation_status <- renderText(status_message)
        
        showNotification("Data generation completed successfully!", type = "success", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error generating data:", e$message), type = "error", duration = 5)
        output$generation_status <- renderText(paste("Error:", e$message))
      })
    })
  })
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    req(values$generated_data)
    
    DT::datatable(
      values$generated_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        searchHighlight = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display nowrap compact",
      filter = "top",
      rownames = TRUE
    ) %>%
      DT::formatRound(columns = 1:ncol(values$generated_data), digits = 0)
  })
  
  # Summary statistics output
  output$summary_stats <- renderText({
    req(values$generated_data)
    
    summary_data <- values$generated_data
    
    # Calculate basic statistics
    n_cases <- nrow(summary_data)
    n_vars <- ncol(summary_data)
    
    # Scale-specific summaries
    tsri_vars <- grep("TSRI", names(summary_data), value = TRUE)
    uwes_vars <- grep("UWES", names(summary_data), value = TRUE)  
    est_vars <- grep("EST", names(summary_data), value = TRUE)
    tas_vars <- grep("TAS", names(summary_data), value = TRUE)
    
    tsri_summary <- summary(summary_data[tsri_vars])
    uwes_summary <- summary(summary_data[uwes_vars])
    est_summary <- summary(summary_data[est_vars]) 
    tas_summary <- summary(summary_data[tas_vars])
    
    paste(
      paste("Total Cases:", n_cases),
      paste("Total Variables:", n_vars),
      "",
      paste("TSRI Scale (", length(tsri_vars), "items, 1-5 scale):"),
      paste("Mean range:", round(min(sapply(summary_data[tsri_vars], mean, na.rm = TRUE)), 2), 
            "to", round(max(sapply(summary_data[tsri_vars], mean, na.rm = TRUE)), 2)),
      "",
      paste("UWES Scale (", length(uwes_vars), "items, 0-6 scale):"),
      paste("Mean range:", round(min(sapply(summary_data[uwes_vars], mean, na.rm = TRUE)), 2), 
            "to", round(max(sapply(summary_data[uwes_vars], mean, na.rm = TRUE)), 2)),
      "",
      paste("EST Scale (", length(est_vars), "items, 1-4 scale):"),
      paste("Mean range:", round(min(sapply(summary_data[est_vars], mean, na.rm = TRUE)), 2), 
            "to", round(max(sapply(summary_data[est_vars], mean, na.rm = TRUE)), 2)),
      "",
      paste("TAS Scale (", length(tas_vars), "items, 1-5 scale):"),
      paste("Mean range:", round(min(sapply(summary_data[tas_vars], mean, na.rm = TRUE)), 2), 
            "to", round(max(sapply(summary_data[tas_vars], mean, na.rm = TRUE)), 2)),
      sep = "\n"
    )
  })
  
  # Missing data information
  output$missing_data_info <- renderText({
    req(values$generated_data)
    
    missing_by_var <- sapply(values$generated_data, function(x) sum(is.na(x)))
    missing_by_case <- rowSums(is.na(values$generated_data))
    
    total_missing <- sum(is.na(values$generated_data))
    total_cells <- nrow(values$generated_data) * ncol(values$generated_data)
    missing_pct <- round(total_missing / total_cells * 100, 2)
    
    paste(
      paste("Total missing values:", total_missing, "out of", total_cells, paste0("(", missing_pct, "%)")),
      paste("Variables with most missing data:"),
      paste(names(sort(missing_by_var, decreasing = TRUE)[1:5]), 
            sort(missing_by_var, decreasing = TRUE)[1:5], 
            "missing", collapse = "\n"),
      "",
      paste("Cases with missing data:", sum(missing_by_case > 0), "out of", nrow(values$generated_data)),
      paste("Maximum missing per case:", max(missing_by_case)),
      paste("Average missing per case:", round(mean(missing_by_case), 2)),
      sep = "\n"
    )
  })
  
  # Variance plot
  output$variance_plot <- renderPlotly({
    req(values$generated_data)
    
    item_vars <- apply(values$generated_data, 2, var, na.rm = TRUE)
    var_data <- data.frame(
      Item = factor(names(item_vars), levels = names(item_vars)),
      Variance = item_vars,
      Scale = case_when(
        grepl("TSRI", names(item_vars)) ~ "TSRI",
        grepl("UWES", names(item_vars)) ~ "UWES",
        grepl("EST", names(item_vars)) ~ "EST",
        grepl("TAS", names(item_vars)) ~ "TAS",
        TRUE ~ "Other"
      )
    )
    
    p <- ggplot(var_data, aes(x = Item, y = Variance, fill = Scale)) +
      geom_col() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Item Variance by Scale", y = "Variance", x = "Items") +
      scale_fill_brewer(type = "qual", palette = "Set1")
    
    ggplotly(p) %>% layout(height = 400)
  })
  
  # Response pattern plot
  output$response_pattern_plot <- renderPlotly({
    req(values$generated_data)
    
    # Calculate extreme response patterns
    extreme_low <- rowSums(values$generated_data == 1 | values$generated_data == 0, na.rm = TRUE)
    extreme_high <- rowSums(values$generated_data == 5 | values$generated_data == 6 | values$generated_data == 4, na.rm = TRUE)
    total_extreme <- extreme_low + extreme_high
    
    pattern_data <- data.frame(
      Case = 1:length(total_extreme),
      Extreme_Responses = total_extreme,
      Type = case_when(
        total_extreme >= quantile(total_extreme, 0.9, na.rm = TRUE) ~ "High Extreme",
        total_extreme <= quantile(total_extreme, 0.1, na.rm = TRUE) ~ "Low Extreme",
        TRUE ~ "Normal"
      )
    )
    
    p <- ggplot(pattern_data, aes(x = Extreme_Responses, fill = Type)) +
      geom_histogram(bins = 20, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Distribution of Extreme Response Patterns", 
           x = "Number of Extreme Responses", y = "Frequency") +
      scale_fill_manual(values = c("High Extreme" = "red", "Low Extreme" = "blue", "Normal" = "gray"))
    
    ggplotly(p) %>% layout(height = 400)
  })
  
  # Reliability statistics
  output$reliability_stats <- renderText({
    req(values$generated_data)
    
    tryCatch({
      # Calculate Cronbach's alpha for each scale
      tsri_vars <- grep("TSRI", names(values$generated_data), value = TRUE)
      uwes_vars <- grep("UWES", names(values$generated_data), value = TRUE)
      est_vars <- grep("EST", names(values$generated_data), value = TRUE)
      tas_vars <- grep("TAS", names(values$generated_data), value = TRUE)
      
      alpha_tsri <- psych::alpha(values$generated_data[tsri_vars], na.rm = TRUE)$total$raw_alpha
      alpha_uwes <- psych::alpha(values$generated_data[uwes_vars], na.rm = TRUE)$total$raw_alpha
      alpha_est <- psych::alpha(values$generated_data[est_vars], na.rm = TRUE)$total$raw_alpha
      alpha_tas <- psych::alpha(values$generated_data[tas_vars], na.rm = TRUE)$total$raw_alpha
      
      # Calculate subscale reliabilities
      tsri_sat_vars <- c("TSRI_1", "TSRI_3", "TSRI_5", "TSRI_13", "TSRI_14")
      tsri_ih_vars <- c("TSRI_2", "TSRI_6", "TSRI_9", "TSRI_10", "TSRI_12")
      tsri_con_vars <- c("TSRI_4", "TSRI_7", "TSRI_8", "TSRI_11")
      
      alpha_tsri_sat <- psych::alpha(values$generated_data[tsri_sat_vars], na.rm = TRUE)$total$raw_alpha
      alpha_tsri_ih <- psych::alpha(values$generated_data[tsri_ih_vars], na.rm = TRUE)$total$raw_alpha
      alpha_tsri_con <- psych::alpha(values$generated_data[tsri_con_vars], na.rm = TRUE)$total$raw_alpha
      
      uwes_vig_vars <- c("UWES_1", "UWES_4", "UWES_8", "UWES_12", "UWES_15", "UWES_17")
      uwes_ded_vars <- c("UWES_2", "UWES_5", "UWES_7", "UWES_10", "UWES_13")
      uwes_abs_vars <- c("UWES_3", "UWES_6", "UWES_9", "UWES_11", "UWES_14", "UWES_16")
      
      alpha_uwes_vig <- psych::alpha(values$generated_data[uwes_vig_vars], na.rm = TRUE)$total$raw_alpha
      alpha_uwes_ded <- psych::alpha(values$generated_data[uwes_ded_vars], na.rm = TRUE)$total$raw_alpha
      alpha_uwes_abs <- psych::alpha(values$generated_data[uwes_abs_vars], na.rm = TRUE)$total$raw_alpha
      
      paste(
        "SCALE RELIABILITY ESTIMATES (Cronbach's Alpha)",
        "=" %>% rep(50) %>% paste(collapse = ""),
        "",
        "Overall Scale Reliabilities:",
        paste("TSRI Total:", round(alpha_tsri, 3)),
        paste("UWES Total:", round(alpha_uwes, 3)),
        paste("EST Total:", round(alpha_est, 3)),
        paste("TAS Total:", round(alpha_tas, 3)),
        "",
        "TSRI Subscales:",
        paste("  Satisfaction:", round(alpha_tsri_sat, 3)),
        paste("  Instrumental Help:", round(alpha_tsri_ih, 3)),
        paste("  Conflict:", round(alpha_tsri_con, 3)),
        "",
        "UWES Subscales:",
        paste("  Vigor:", round(alpha_uwes_vig, 3)),
        paste("  Dedication:", round(alpha_uwes_ded, 3)),
        paste("  Absorption:", round(alpha_uwes_abs, 3)),
        "",
        "Note: Values above 0.70 indicate acceptable internal consistency.",
        "Values above 0.80 indicate good internal consistency.",
        sep = "\n"
      )
      
    }, error = function(e) {
      paste("Error calculating reliability statistics:", e$message)
    })
  })
  
  # Correlation plot
  output$correlation_plot <- renderPlot({
    req(values$generated_data)
    req(input$correlation_subset)
    
    if (input$correlation_subset == "all") {
      cor_data <- values$generated_data
      title_text <- "Correlation Matrix - All Variables"
    } else if (input$correlation_subset == "tsri") {
      cor_data <- values$generated_data[grep("TSRI", names(values$generated_data))]
      title_text <- "Correlation Matrix - TSRI Items"
    } else if (input$correlation_subset == "uwes") {
      cor_data <- values$generated_data[grep("UWES", names(values$generated_data))]
      title_text <- "Correlation Matrix - UWES Items"
    } else if (input$correlation_subset == "est") {
      cor_data <- values$generated_data[grep("EST", names(values$generated_data))]
      title_text <- "Correlation Matrix - EST Items"
    } else if (input$correlation_subset == "tas") {
      cor_data <- values$generated_data[grep("TAS", names(values$generated_data))]
      title_text <- "Correlation Matrix - TAS Items"
    }
    
    tryCatch({
      cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
      corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                        order = "hclust", tl.cex = 0.8, tl.col = "black",
                        title = title_text, mar = c(0,0,2,0))
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating correlation plot:", e$message), cex = 1.2)
    })
  }, height = 600)
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("sem_data_", Sys.Date(), "_", format(Sys.time(), "%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      req(values$generated_data)
      write.csv(values$generated_data, file, row.names = FALSE)
    }
  )
  
  # Enable download button only when data is generated
  observe({
    shinyjs::toggleState("download_data", !is.null(values$generated_data))
  })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 5000))
