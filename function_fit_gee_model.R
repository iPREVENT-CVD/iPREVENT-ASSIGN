####################################################################################################################################
# general information
####################################################################################################################################

# Author: Marie de Bakker
# Date: 24/02/2026
# Information: functions

##################
### function 1 ###
##################

fit_gee_model <- function(dataset, model_formula, outcome){
  
  # store outcome and time variable
  dataset$outcome <- dataset[, outcome]
  
  # outcome should be coded as numeric (instead of factor) to prevent errors within binomial gee model
  dataset$outcome <- ifelse(dataset$outcome == "Yes", 1,
                            ifelse(dataset$outcome == "No", 0, NA)) %>% as.numeric()
  
  # it's important that the dataset is ordered by subject_id and year
  dataset <- dataset[order(dataset$id, dataset$year), ]
  
  # fit model
  gee_fit <- geeglm(formula = as.formula(model_formula),
                    data = dataset,
                    family = binomial,
                    id = id,
                    corstr = "exchangeable")
  
  return(gee_fit)
  
}

##################
### function 2 ###
##################

extract_estimates_per_year <- function(model, variable_of_interest, level_of_interest, time_variable = "year"){
  
  library(broom)
  
  # dataset used to fit the model
  dataset <- model[["data"]]
  
  # coefficient of interest
  coef_of_interest <- paste0(variable_of_interest, level_of_interest)
  
  # levels of time variable
  times <- levels(dataset[, time_variable])
  
  # in order to extract the coefficients and 95%CI of interest, we need to 'fill in' the model formula 
  # for each combination (coef of interest + interaction term with year of interest)
  # in order to do so, we first create a list with these combinations and set their value to 1 (i.e., use coefficient)
  contrasts <- lapply(times, function(i){
    # for the reference level of time, you would only need the coefficient of interest
    # thus: # if i (= time) is equal to the reference level, only use coefficient of interest
    if(i == levels(dataset[, time_variable])[1]){ 
      setNames(1, coef_of_interest) 
    } else {
      # otherwise, you would need both the coefficient of interest as well as the interaction term
      interaction_of_interest <- paste0(time_variable, i,":", coef_of_interest)
      setNames(c(1,1), c(coef_of_interest, interaction_of_interest))
    }
  })
  
  # give names to the contrast for each year
  names(contrasts) <- times
  
  # subsequently, fill in model formula for each combination in contrast
  # only the coefficients where contrast = 1 will be extracted
  results <- lapply(contrasts, function(i){
    
    # calculate the estimate, by extracting the coefficients of the model
    # and summing the coefficients for the the contrast of interest (i)
    estimate <- sum(coef(model)[names(i)]*i)  # if coef for contrast of interest are c(0.5, 0.2), then this happens: c(0.5, 0.2)* c(1, 1) = c(0.5, 0.2) -- > sum() = 0.7
    
    # calculate the standard error (used for 95%CI) (i.e., the sqrt of the variance of the sum of coefficients [I think])
    se <- sqrt(t(i) %*% vcov(model)[names(i), names(i)] %*% i)
    
    # store the estimates and lower and upper limit (as ORs instead of logodds)
    data.frame(odds_ratio = exp(estimate),
               lower_limit = exp(estimate - 1.96*se),
               upper_limit = exp(estimate + 1.96*se))
  })
  
  # store as data.frame
  df_results <- do.call(rbind, results)
  df_results[, paste0(time_variable)] <- rownames(df_results)
  
  return(df_results)
  
}
