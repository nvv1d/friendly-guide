
# Advanced SEM Data Generator & Analyzer with Customizable Profiles
# Professional-grade SEM modeling with comprehensive fit indices and path analysis

# Set up library path
user_lib <- file.path(getwd(), "R_libs")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

# Load comprehensive SEM packages
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(MASS)
library(lavaan)
library(psych)
library(semPlot)
library(semTools)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(plotly)
library(viridis)

# Global profile storage - initialize as regular list first
default_profiles <- list(
  "Profile #1 - Educational SEM (Default)" = list(
    scales = list(
      TSRI = list(name = "Teacher-Student Relationship", factors = c("Satisfaction", "Instrumental", "Conflict"), items = 14, range = c(1,5)),
      UWES = list(name = "Work Engagement", factors = c("Vigor", "Dedication", "Absorption"), items = 17, range = c(0,6)),
      EST = list(name = "Empathy Scale for Teachers", factors = c("Cognitive", "Negative", "Positive"), items = 19, range = c(1,4)),
      TAS = list(name = "Teacher Altruism Scale", factors = c("Donation", "Emergency", "Everyday", "Social"), items = 18, range = c(1,5))
    ),
    model_structure = list(
      predictors = c("EST_Cognitive", "EST_Positive", "TAS_Donation"),
      mediators = c("TSRI_Satisfaction", "UWES_Vigor"),
      outcomes = c("UWES_Dedication", "UWES_Absorption")
    ),
    factor_correlations = list(
      empathy_altruism = 0.45,
      tsr_engagement = 0.65,
      empathy_engagement = 0.35
    )
  )
)

# Enhanced SEM data generation with profile support
generate_advanced_sem_data <- function(n = 310, seed = 12345, profile = profiles$data[[1]], params = list()) {
  set.seed(seed)
  
  # Default parameters
  default_params <- list(
    response_bias_prob = 0.12,
    acquiescence_strength = 0.3,
    extreme_response_prob = 0.08,
    midpoint_bias_prob = 0.15,
    missing_data_prob = 0.02,
    outlier_prob = 0.04,
    social_desirability_strength = 0.25,
    fatigue_effect_strength = 0.15,
    measurement_error_variance = 0.15,
    method_effects = TRUE,
    heteroscedasticity = TRUE
  )
  
  params <- modifyList(default_params, params)
  
  # Generate latent factors based on profile
  factors <- list()
  
  # TSRI factors
  factors$tsr_satisfaction <- rnorm(n, 0, 1)
  factors$tsr_instrumental <- 0.74 * factors$tsr_satisfaction + sqrt(1 - 0.74^2) * rnorm(n, 0, 1)
  factors$tsr_conflict <- -0.58 * factors$tsr_satisfaction + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
  
  # Empathy factors
  factors$empathy_cognitive <- rnorm(n, 0, 1)
  factors$empathy_negative <- 0.32 * factors$empathy_cognitive + sqrt(1 - 0.32^2) * rnorm(n, 0, 1)
  factors$empathy_positive <- 0.58 * factors$empathy_cognitive + sqrt(1 - 0.58^2) * rnorm(n, 0, 1)
  
  # Altruism factors
  factors$altruism_donation <- rnorm(n, 0, 1)
  factors$altruism_emergency <- profile$factor_correlations$empathy_altruism * factors$altruism_donation + 
    sqrt(1 - profile$factor_correlations$empathy_altruism^2) * rnorm(n, 0, 1)
  factors$altruism_everyday <- 0.72 * factors$altruism_donation + sqrt(1 - 0.72^2) * rnorm(n, 0, 1)
  factors$altruism_social <- 0.61 * factors$altruism_donation + sqrt(1 - 0.61^2) * rnorm(n, 0, 1)
  
  # Work Engagement factors with structural relationships
  factors$we_vigor <- 0.28 * factors$empathy_cognitive + 0.25 * factors$empathy_positive + 
    0.42 * factors$tsr_satisfaction + sqrt(1 - 0.28^2 - 0.25^2 - 0.42^2) * rnorm(n, 0, 1)
  factors$we_dedication <- profile$factor_correlations$tsr_engagement * factors$we_vigor + 
    sqrt(1 - profile$factor_correlations$tsr_engagement^2) * rnorm(n, 0, 1)
  factors$we_absorption <- 0.75 * factors$we_vigor + sqrt(1 - 0.75^2) * rnorm(n, 0, 1)
  
  # Generate observed indicators with enhanced realism
  generate_advanced_indicators <- function(latent_scores, loadings, scale_range, params, item_names) {
    n_items <- length(loadings)
    indicators <- matrix(NA, nrow = length(latent_scores), ncol = n_items)
    
    for (i in 1:n_items) {
      # Base true score with factor loading
      true_score <- loadings[i] * latent_scores + 
        sqrt(max(0.1, 1 - loadings[i]^2)) * rnorm(length(latent_scores), 0, 1)
      
      # Add measurement error
      if (params$measurement_error_variance > 0) {
        measurement_error <- rnorm(length(latent_scores), 0, params$measurement_error_variance)
        true_score <- true_score + measurement_error
      }
      
      # Apply response biases
      bias_mask <- runif(length(latent_scores)) < params$response_bias_prob
      if (sum(bias_mask) > 0) {
        true_score[bias_mask] <- true_score[bias_mask] + 
          rnorm(sum(bias_mask), params$acquiescence_strength, 0.2)
      }
      
      # Transform to scale range
      if (params$heteroscedasticity) {
        scale_variance <- runif(length(latent_scores), 0.8, 1.2)
      } else {
        scale_variance <- rep(1, length(latent_scores))
      }
      
      # Scale transformation
      scale_min <- scale_range[1]
      scale_max <- scale_range[2]
      scale_width <- scale_max - scale_min
      
      transformed_scores <- pnorm(true_score * scale_variance) * scale_width + scale_min
      indicators[, i] <- round(pmax(scale_min, pmin(scale_max, transformed_scores)))
      
      # Apply extreme response bias
      extreme_mask <- runif(length(latent_scores)) < params$extreme_response_prob
      if (sum(extreme_mask) > 0) {
        extreme_values <- ifelse(indicators[extreme_mask, i] > mean(scale_range), scale_max, scale_min)
        indicators[extreme_mask, i] <- extreme_values
      }
      
      # Add missing data
      missing_mask <- runif(length(latent_scores)) < params$missing_data_prob
      indicators[missing_mask, i] <- NA
    }
    
    colnames(indicators) <- item_names
    return(indicators)
  }
  
  # Generate all scale indicators
  data_list <- list()
  
  # TSRI indicators
  tsri_loadings <- list(
    satisfaction = c(0.78, 0.82, 0.61, 0.80, 0.77),
    instrumental = c(0.58, 0.76, 0.81, 0.74, 0.79),
    conflict = c(0.72, 0.85, 0.79, 0.55)
  )
  
  data_list$tsri_sat <- generate_advanced_indicators(factors$tsr_satisfaction, tsri_loadings$satisfaction, 
                                                     c(1, 5), params, paste0("TSRI_SAT_", 1:5))
  data_list$tsri_ih <- generate_advanced_indicators(factors$tsr_instrumental, tsri_loadings$instrumental, 
                                                    c(1, 5), params, paste0("TSRI_IH_", 1:5))
  data_list$tsri_con <- generate_advanced_indicators(factors$tsr_conflict, tsri_loadings$conflict, 
                                                     c(1, 5), params, paste0("TSRI_CON_", 1:4))
  
  # UWES indicators
  uwes_loadings <- list(
    vigor = c(0.76, 0.63, 0.74, 0.78, 0.82, 0.75),
    dedication = c(0.84, 0.87, 0.81, 0.83, 0.68),
    absorption = c(0.77, 0.79, 0.82, 0.59, 0.80, 0.78)
  )
  
  data_list$uwes_vig <- generate_advanced_indicators(factors$we_vigor, uwes_loadings$vigor, 
                                                     c(0, 6), params, paste0("UWES_VIG_", 1:6))
  data_list$uwes_ded <- generate_advanced_indicators(factors$we_dedication, uwes_loadings$dedication, 
                                                     c(0, 6), params, paste0("UWES_DED_", 1:5))
  data_list$uwes_abs <- generate_advanced_indicators(factors$we_absorption, uwes_loadings$absorption, 
                                                     c(0, 6), params, paste0("UWES_ABS_", 1:6))
  
  # EST indicators
  est_loadings <- list(
    cognitive = c(0.71, 0.74, 0.76, 0.52, 0.79, 0.72, 0.77, 0.75, 0.78),
    negative = c(0.68, 0.72, 0.75, 0.70, 0.49),
    positive = c(0.80, 0.83, 0.78, 0.62, 0.79)
  )
  
  data_list$est_cog <- generate_advanced_indicators(factors$empathy_cognitive, est_loadings$cognitive, 
                                                    c(1, 4), params, paste0("EST_COG_", 1:9))
  data_list$est_neg <- generate_advanced_indicators(factors$empathy_negative, est_loadings$negative, 
                                                    c(1, 4), params, paste0("EST_NEG_", 1:5))
  data_list$est_pos <- generate_advanced_indicators(factors$empathy_positive, est_loadings$positive, 
                                                    c(1, 4), params, paste0("EST_POS_", 1:5))
  
  # TAS indicators
  tas_loadings <- list(
    donation = c(0.84, 0.87, 0.82),
    emergency = c(0.79, 0.81, 0.54),
    everyday = c(0.74, 0.76, 0.78, 0.72, 0.75, 0.58),
    social = c(0.71, 0.73, 0.76, 0.74, 0.77, 0.53)
  )
  
  data_list$tas_don <- generate_advanced_indicators(factors$altruism_donation, tas_loadings$donation, 
                                                    c(1, 5), params, paste0("TAS_DON_", 1:3))
  data_list$tas_emh <- generate_advanced_indicators(factors$altruism_emergency, tas_loadings$emergency, 
                                                    c(1, 5), params, paste0("TAS_EMH_", 1:3))
  data_list$tas_evh <- generate_advanced_indicators(factors$altruism_everyday, tas_loadings$everyday, 
                                                    c(1, 5), params, paste0("TAS_EVH_", 1:6))
  data_list$tas_srs <- generate_advanced_indicators(factors$altruism_social, tas_loadings$social, 
                                                    c(1, 5), params, paste0("TAS_SRS_", 1:6))
  
  # Combine all data
  final_data <- do.call(cbind, data_list)
  
  return(list(
    data = as.data.frame(final_data),
    factors = factors,
    loadings = list(tsri = tsri_loadings, uwes = uwes_loadings, est = est_loadings, tas = tas_loadings),
    params = params
  ))
}

# Comprehensive SEM analysis function
perform_comprehensive_sem_analysis <- function(data, profile) {
  results <- list()
  
  tryCatch({
    # 1. Reliability Analysis with multiple methods
    results$reliability <- list()
    
    # Cronbach's Alpha
    tsri_items <- grep("TSRI_", names(data), value = TRUE)
    uwes_items <- grep("UWES_", names(data), value = TRUE)
    est_items <- grep("EST_", names(data), value = TRUE)
    tas_items <- grep("TAS_", names(data), value = TRUE)
    
    results$reliability$alpha <- list(
      TSRI = psych::alpha(data[tsri_items], check.keys = TRUE),
      UWES = psych::alpha(data[uwes_items], check.keys = TRUE),
      EST = psych::alpha(data[est_items], check.keys = TRUE),
      TAS = psych::alpha(data[tas_items], check.keys = TRUE)
    )
    
    # Omega reliability (more accurate)
    results$reliability$omega <- list(
      TSRI = psych::omega(data[tsri_items], nfactors = 3),
      UWES = psych::omega(data[uwes_items], nfactors = 3),
      EST = psych::omega(data[est_items], nfactors = 3),
      TAS = psych::omega(data[tas_items], nfactors = 4)
    )
    
    # 2. Confirmatory Factor Analysis with comprehensive fit indices
    cfa_model <- '
    # TSRI factors
    TSRI_Satisfaction =~ TSRI_SAT_1 + TSRI_SAT_2 + TSRI_SAT_3 + TSRI_SAT_4 + TSRI_SAT_5
    TSRI_Instrumental =~ TSRI_IH_1 + TSRI_IH_2 + TSRI_IH_3 + TSRI_IH_4 + TSRI_IH_5
    TSRI_Conflict =~ TSRI_CON_1 + TSRI_CON_2 + TSRI_CON_3 + TSRI_CON_4
    
    # UWES factors
    UWES_Vigor =~ UWES_VIG_1 + UWES_VIG_2 + UWES_VIG_3 + UWES_VIG_4 + UWES_VIG_5 + UWES_VIG_6
    UWES_Dedication =~ UWES_DED_1 + UWES_DED_2 + UWES_DED_3 + UWES_DED_4 + UWES_DED_5
    UWES_Absorption =~ UWES_ABS_1 + UWES_ABS_2 + UWES_ABS_3 + UWES_ABS_4 + UWES_ABS_5 + UWES_ABS_6
    
    # EST factors
    EST_Cognitive =~ EST_COG_1 + EST_COG_2 + EST_COG_3 + EST_COG_4 + EST_COG_5 + EST_COG_6 + EST_COG_7 + EST_COG_8 + EST_COG_9
    EST_Negative =~ EST_NEG_1 + EST_NEG_2 + EST_NEG_3 + EST_NEG_4 + EST_NEG_5
    EST_Positive =~ EST_POS_1 + EST_POS_2 + EST_POS_3 + EST_POS_4 + EST_POS_5
    
    # TAS factors
    TAS_Donation =~ TAS_DON_1 + TAS_DON_2 + TAS_DON_3
    TAS_Emergency =~ TAS_EMH_1 + TAS_EMH_2 + TAS_EMH_3
    TAS_Everyday =~ TAS_EVH_1 + TAS_EVH_2 + TAS_EVH_3 + TAS_EVH_4 + TAS_EVH_5 + TAS_EVH_6
    TAS_Social =~ TAS_SRS_1 + TAS_SRS_2 + TAS_SRS_3 + TAS_SRS_4 + TAS_SRS_5 + TAS_SRS_6
    '
    
    results$cfa <- lavaan::cfa(cfa_model, data = data, estimator = "MLR", missing = "fiml")
    
    # Comprehensive fit indices
    results$fit_indices <- lavaan::fitmeasures(results$cfa, c(
      "chisq", "df", "pvalue", "chisq.scaled", "df.scaled", "pvalue.scaled",
      "cfi", "cfi.scaled", "tli", "tli.scaled", "rmsea", "rmsea.scaled", 
      "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic", "bic2"
    ))
    
    # 3. Structural Equation Model
    sem_model <- paste0(cfa_model, '
    
    # Structural relationships based on profile
    UWES_Vigor ~ EST_Cognitive + EST_Positive + TSRI_Satisfaction
    UWES_Dedication ~ UWES_Vigor + TSRI_Satisfaction
    UWES_Absorption ~ UWES_Vigor + UWES_Dedication
    TSRI_Satisfaction ~ EST_Cognitive + TAS_Donation
    ')
    
    results$sem <- lavaan::sem(sem_model, data = data, estimator = "MLR", missing = "fiml")
    results$sem_fit <- lavaan::fitmeasures(results$sem, c(
      "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", 
      "rmsea.ci.upper", "srmr", "aic", "bic"
    ))
    
    # 4. Path coefficients and R-squared
    results$path_coefficients <- lavaan::parameterEstimates(results$sem, standardized = TRUE)
    results$r_squared <- lavaan::inspect(results$sem, "r2")
    
    # 5. Modification indices
    results$modification_indices <- lavaan::modindices(results$sem, sort = TRUE, maximum.number = 10)
    
    # 6. Measurement invariance (if applicable)
    results$measurement_invariance <- semTools::measurementInvariance(
      model = cfa_model, data = data, group = NULL, estimator = "MLR"
    )
    
  }, error = function(e) {
    results$error <- paste("Analysis error:", e$message)
  })
  
  return(results)
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Advanced SEM Analyzer with Customizable Profiles"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Profile Management", tabName = "profiles", icon = icon("user-cog")),
      menuItem("Data Generation", tabName = "generation", icon = icon("database")),
      menuItem("SEM Analysis", tabName = "analysis", icon = icon("project-diagram")),
      menuItem("Path Diagram", tabName = "paths", icon = icon("share-alt")),
      menuItem("Model Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Export Results", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .box { border-radius: 5px; }
        .fit-excellent { color: #00A65A; font-weight: bold; }
        .fit-good { color: #3C8DBC; font-weight: bold; }
        .fit-acceptable { color: #F39C12; font-weight: bold; }
        .fit-poor { color: #DD4B39; font-weight: bold; }
      "))
    ),
    
    tabItems(
      # Profile Management Tab
      tabItem(tabName = "profiles",
        fluidRow(
          box(
            title = "Profile Management", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6,
                h4("Current Profile"),
                selectInput("active_profile", "Select Profile:",
                           choices = names(default_profiles),
                           selected = names(default_profiles)[1]),
                br(),
                h5("Profile Settings"),
                textInput("profile_name", "Profile Name:", value = "Profile #1 - Educational SEM (Default)"),
                numericInput("n_factors", "Number of Factors:", value = 13, min = 2, max = 20),
                textAreaInput("custom_scales", "Custom Scale Definitions (JSON):", 
                             value = '{"CUSTOM": {"name": "Custom Scale", "factors": ["Factor1", "Factor2"], "items": 10, "range": [1,7]}}',
                             rows = 4)
              ),
              column(6,
                h4("Model Structure"),
                textAreaInput("predictors", "Predictors (comma-separated):", 
                             value = "EST_Cognitive, EST_Positive, TAS_Donation", rows = 2),
                textAreaInput("mediators", "Mediators (comma-separated):", 
                             value = "TSRI_Satisfaction, UWES_Vigor", rows = 2),
                textAreaInput("outcomes", "Outcomes (comma-separated):", 
                             value = "UWES_Dedication, UWES_Absorption", rows = 2),
                br(),
                actionButton("save_profile", "Save Current Profile", class = "btn-success"),
                br(), br(),
                actionButton("create_new_profile", "Create New Profile", class = "btn-info")
              )
            )
          )
        )
      ),
      
      # Data Generation Tab
      tabItem(tabName = "generation",
        fluidRow(
          box(
            title = "Advanced Data Generation Parameters", status = "primary", solidHeader = TRUE, width = 6,
            numericInput("sample_size", "Sample Size:", value = 310, min = 50, max = 5000),
            numericInput("seed", "Random Seed:", value = 12345),
            sliderInput("response_bias_prob", "Response Bias Probability:", 
                       min = 0, max = 0.3, value = 0.12, step = 0.01),
            sliderInput("missing_data_prob", "Missing Data Rate:", 
                       min = 0, max = 0.1, value = 0.02, step = 0.005),
            sliderInput("measurement_error_variance", "Measurement Error Variance:", 
                       min = 0, max = 0.3, value = 0.15, step = 0.01),
            checkboxInput("method_effects", "Include Method Effects", value = TRUE),
            checkboxInput("heteroscedasticity", "Include Heteroscedasticity", value = TRUE)
          ),
          
          box(
            title = "Generation Controls", status = "success", solidHeader = TRUE, width = 6,
            div(style = "text-align: center;",
              actionButton("generate_advanced_data", "Generate Advanced SEM Data", 
                          class = "btn-success btn-lg", icon = icon("cogs")),
              br(), br(),
              downloadButton("download_advanced_data", "Download Dataset", 
                           class = "btn-primary", icon = icon("download")),
              br(), br(),
              verbatimTextOutput("generation_status")
            )
          )
        )
      ),
      
      # SEM Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Reliability Analysis", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("reliability_results")
          ),
          
          box(
            title = "Model Fit Indices", status = "warning", solidHeader = TRUE, width = 6,
            tableOutput("fit_indices_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Path Coefficients", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("path_coefficients_table")
          )
        ),
        
        fluidRow(
          box(
            title = "R-Squared Values", status = "primary", solidHeader = TRUE, width = 6,
            tableOutput("r_squared_table")
          ),
          
          box(
            title = "Modification Indices", status = "warning", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("modification_indices_table")
          )
        )
      ),
      
      # Path Diagram Tab
      tabItem(tabName = "paths",
        fluidRow(
          box(
            title = "SEM Path Diagram", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("path_diagram", height = "700px"),
            br(),
            fluidRow(
              column(4, selectInput("layout_type", "Layout:", choices = c("tree", "circle", "spring"), selected = "tree")),
              column(4, checkboxInput("show_labels", "Show Parameter Labels", value = TRUE)),
              column(4, checkboxInput("show_coefficients", "Show Coefficients", value = TRUE))
            )
          )
        )
      ),
      
      # Model Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(
            title = "Model Comparison", status = "primary", solidHeader = TRUE, width = 12,
            p("Compare different model specifications and fit indices."),
            tableOutput("model_comparison_table")
          )
        )
      ),
      
      # Export Results Tab
      tabItem(tabName = "export",
        fluidRow(
          box(
            title = "Export Analysis Results", status = "success", solidHeader = TRUE, width = 12,
            h4("Available Exports:"),
            br(),
            downloadButton("export_full_report", "Full Analysis Report (HTML)", class = "btn-primary"),
            br(), br(),
            downloadButton("export_lavaan_syntax", "Lavaan Model Syntax", class = "btn-info"),
            br(), br(),
            downloadButton("export_fit_indices", "Fit Indices (CSV)", class = "btn-success"),
            br(), br(),
            downloadButton("export_path_coefficients", "Path Coefficients (CSV)", class = "btn-warning")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Initialize profiles as reactiveValues in server
  profiles <- reactiveValues(
    current = 1,
    data = default_profiles
  )
  
  # Reactive values
  values <- reactiveValues(
    generated_data = NULL,
    sem_results = NULL,
    current_profile = default_profiles[[1]]
  )
  
  # Profile management
  observeEvent(input$active_profile, {
    if (input$active_profile %in% names(profiles$data)) {
      values$current_profile <- profiles$data[[input$active_profile]]
    }
  })
  
  # Data generation
  observeEvent(input$generate_advanced_data, {
    withProgress(message = 'Generating advanced SEM data...', {
      incProgress(0.3, detail = "Creating latent structure")
      
      params <- list(
        response_bias_prob = input$response_bias_prob,
        missing_data_prob = input$missing_data_prob,
        measurement_error_variance = input$measurement_error_variance,
        method_effects = input$method_effects,
        heteroscedasticity = input$heteroscedasticity
      )
      
      incProgress(0.6, detail = "Generating indicators")
      result <- generate_advanced_sem_data(
        n = input$sample_size,
        seed = input$seed,
        profile = values$current_profile,
        params = params
      )
      
      values$generated_data <- result
      
      incProgress(0.9, detail = "Running SEM analysis")
      values$sem_results <- perform_comprehensive_sem_analysis(result$data, values$current_profile)
      
      incProgress(1.0, detail = "Complete!")
    })
    
    output$generation_status <- renderText({
      if (!is.null(values$generated_data)) {
        paste0("✅ Dataset Generated Successfully!\n",
               "📊 Dimensions: ", nrow(values$generated_data$data), " × ", ncol(values$generated_data$data), "\n",
               "📈 Missing Rate: ", round(mean(is.na(values$generated_data$data)) * 100, 2), "%\n",
               "🎯 Profile: ", input$active_profile)
      }
    })
  })
  
  # Reliability results
  output$reliability_results <- renderText({
    req(values$sem_results)
    
    if (!is.null(values$sem_results$reliability)) {
      rel <- values$sem_results$reliability
      
      paste0("RELIABILITY ANALYSIS\n",
             "==================\n\n",
             "Cronbach's Alpha:\n",
             "TSRI: α = ", round(rel$alpha$TSRI$total$raw_alpha, 3), "\n",
             "UWES: α = ", round(rel$alpha$UWES$total$raw_alpha, 3), "\n",
             "EST: α = ", round(rel$alpha$EST$total$raw_alpha, 3), "\n",
             "TAS: α = ", round(rel$alpha$TAS$total$raw_alpha, 3), "\n\n",
             "McDonald's Omega (Hierarchical):\n",
             "TSRI: ω = ", round(rel$omega$TSRI$omega_h, 3), "\n",
             "UWES: ω = ", round(rel$omega$UWES$omega_h, 3), "\n",
             "EST: ω = ", round(rel$omega$EST$omega_h, 3), "\n",
             "TAS: ω = ", round(rel$omega$TAS$omega_h, 3))
    } else {
      "Generate data to see reliability analysis"
    }
  })
  
  # Fit indices table
  output$fit_indices_table <- renderTable({
    req(values$sem_results$fit_indices)
    
    fit <- values$sem_results$fit_indices
    
    # Create interpretation
    interpret_fit <- function(index, value) {
      if (index == "cfi" || index == "tli") {
        if (value >= 0.95) "Excellent" else if (value >= 0.90) "Good" else if (value >= 0.80) "Acceptable" else "Poor"
      } else if (index == "rmsea") {
        if (value <= 0.05) "Excellent" else if (value <= 0.08) "Good" else if (value <= 0.10) "Acceptable" else "Poor"
      } else if (index == "srmr") {
        if (value <= 0.05) "Excellent" else if (value <= 0.08) "Good" else if (value <= 0.10) "Acceptable" else "Poor"
      } else {
        ""
      }
    }
    
    key_indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
    
    fit_df <- data.frame(
      Index = c("Chi-square", "df", "p-value", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC"),
      Value = round(fit[key_indices], 4),
      Interpretation = sapply(seq_along(key_indices), function(i) {
        interpret_fit(key_indices[i], fit[key_indices[i]])
      }),
      stringsAsFactors = FALSE
    )
    
    fit_df
  })
  
  # Path coefficients table
  output$path_coefficients_table <- DT::renderDataTable({
    req(values$sem_results$path_coefficients)
    
    paths <- values$sem_results$path_coefficients
    paths_filtered <- paths[paths$op == "~" & abs(paths$std.all) > 0.1, 
                           c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "std.all")]
    
    DT::datatable(paths_filtered, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  caption = "Structural Path Coefficients (|β| > 0.1)") %>%
      DT::formatRound(c("est", "se", "z", "std.all"), 3) %>%
      DT::formatRound("pvalue", 4)
  })
  
  # R-squared table
  output$r_squared_table <- renderTable({
    req(values$sem_results$r_squared)
    
    r2_df <- data.frame(
      Variable = names(values$sem_results$r_squared),
      R_Squared = round(values$sem_results$r_squared, 3),
      Variance_Explained = paste0(round(values$sem_results$r_squared * 100, 1), "%"),
      stringsAsFactors = FALSE
    )
    
    r2_df[order(-r2_df$R_Squared), ]
  })
  
  # Path diagram
  output$path_diagram <- renderPlot({
    req(values$sem_results$sem)
    
    tryCatch({
      semPlot::semPaths(values$sem_results$sem, 
                       what = "std", 
                       layout = input$layout_type,
                       edge.label.cex = if(input$show_coefficients) 0.8 else 0,
                       nodeLabels = input$show_labels,
                       style = "ram",
                       optimizeLatRes = TRUE,
                       residuals = FALSE,
                       intercepts = FALSE,
                       thresholds = FALSE,
                       rotation = 2)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Path diagram generation error. Try different layout.", cex = 1.2)
    })
  })
  
  # Download handlers
  output$download_advanced_data <- downloadHandler(
    filename = function() {
      paste0("advanced_sem_data_", input$active_profile, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$generated_data)
      write.csv(values$generated_data$data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 5000))
