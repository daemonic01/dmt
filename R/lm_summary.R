
# This script creates a model summary table from one or more lm objects.

# Taking model data, creating summaries
lm_summary <- function(model_data, Sigma = FALSE, AIC = TRUE, BIC = TRUE, LogLik = TRUE, RMSE = TRUE) {
  
  ### Check packages
  source("R/requirements.R")
  requirements_check()
  
  # Checking the format of 'model_data' and the number of arguments.
  if (!inherits(model_data, "lm")) {
    stop("The 'model_data' must be an lm() object.")
  }
  
  # Try to create model_summary data
  tryCatch({
    model_summary <- summary(model_data)
    
  }, error = function() {
    stop("Failed to create summaries.")
  })
  
    ########################################
   ###   CREATING MODEL SUMMARY TABLE   ###
  ########################################
  
  # Extracting nessessary data
  tryCatch({
    coefficients <- coef(model_summary)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    std_error <- model_summary$sigma
    r_squared_change <- model_summary$r.squared - model_summary$adj.r.squared
    f_change <- model_summary$fstatistic[1] - model_summary$fstatistic[2]
    df1 <- model_summary$fstatistic[2]
    df2 <- model_summary$fstatistic[3]
    sig_f_change <- model_summary$fstatistic[4]
    aic <- format(AIC(model_data), nsmall = 2, digits = 2)
    bic <- format(BIC(model_data), nsmall = 2, digits = 2)
    log_lik <- as.numeric(logLik(model_data))
    nobs <- length(model_data$residuals)
    sigma <- sigma(model_data)
  }, error = function() {
    stop("Failed to extract nessessary data for model summary table.")
  })
  
  # Computing RMSE
  tryCatch({
    dependent_name <- as.character(model_data$terms[[2]])
    real_values <- model_data$model[[dependent_name]]
    predicted_values <- unname(predict(model_data))
    diff_squared <- (real_values - predicted_values)^2
    mean_diff_sq <- mean(diff_squared)
    rmse <- format(sqrt(mean_diff_sq), nsmall = 2, digits = 2)
  }, error = function() {
    stop("Failed to compute RMSE value.")
  })

  
  # Creating Model Summary table
  tryCatch({
    model_summary_table <- data.frame("N" = nobs,
                                      "R" = sqrt(r_squared),
                                      "R Square" = r_squared,
                                      "Adjusted R Square" = adj_r_squared,
                                      "Std. Error of the Estimate" = std_error,
                                      "R Square Change" = r_squared_change,
                                      "F Change" = f_change,
                                      "df1" = df1,
                                      "df2" = df2,
                                      "Sig. F Change" = sig_f_change)
    if (Sigma) {
      model_summary_table$Sigma <- sigma
    }
    
    if (AIC) {
      model_summary_table$AIC <- aic
    }
    
    if (BIC) {
      model_summary_table$BIC <- bic
    }
    
    if(LogLik) {
      model_summary_table$LogLik <- log_lik
    }
    
    if (RMSE) {
      model_summary_table$RMSE <- rmse
    }
    
  }, error = function() {
    stop("Failed to create Model Summary table.")
  })
  
    ###########################################
   ###   FORMATTING AND RETURNING TABLES   ###
  ###########################################
  
  # Format table
  form_model_summary_table <- format(model_summary_table, digits = 3)
  
  # Return table
  return(form_model_summary_table)
}


