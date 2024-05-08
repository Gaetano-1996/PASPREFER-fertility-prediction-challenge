# This is an example script to generate the outcome variable given the input dataset.
# 
# This script should be modified to prepare your own submission that predicts 
# the outcome for the benchmark challenge by changing the clean_df and predict_outcomes function.
# 
# The predict_outcomes function takes a data frame. The return value must
# be a data frame with two columns: nomem_encr and outcome. The nomem_encr column
# should contain the nomem_encr column from the input data frame. The outcome
# column should contain the predicted outcome for each nomem_encr. The outcome
# should be 0 (no child) or 1 (having a child).
# 
# clean_df should be used to clean (preprocess) the data.
# 
# run.R can be used to test your submission.

# List your packages here. Don't forget to update packages.R!
library(tidyverse)
library(data.table)
library(bundle)
library(butcher)
library(tidymodels)

clean_df = function(df, background_df = NULL) {
  # Preprocess the input dataframe to feed the model.
  ### If no cleaning is done (e.g. if all the cleaning is done in a pipeline)
  # leave only the "return df" command
  
  # Parameters:
  # df (dataframe): The input dataframe containing the raw data
  #(e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background (dataframe): Optional input dataframe containing background data
  #(e.g., from PreFer_train_background_data.csv or
  #PreFer_fake_background_data.csv).
  
  # Returns:
  # data frame: The cleaned dataframe with only the necessary columns
  # and processed variables.
  
  df = setDT(df)
  # selecting only obs. for which the outcome is available 
  df[outcome_available == 1,]
  
  # extracting subset of predictors
  keepcols = c("nomem_encr",
               'age',
               'gender',
               'intention',
               'intention2',
               'intention3',
               'health',
               'limited',
               'depress',
               'implife'
  )
  
  # Gender
  # imputation of gender from background info
  imp.gender = ifelse(is.na(df$cf20m003),df$gender_bg,df$cf20m003)
  # refactoring of gender variable
  # converting gender into factor
  df[, gender := factor(
    imp.gender,
    levels = c(1, 2),
    labels = c('male', 'female'),
    exclude = NULL)]
  
  # Age
  # ensuring numeric type
  df$birthyear_bg = as.numeric(df$birthyear_bg)
  age_birth = 2020 - df$birthyear_bg
  
  # creating classes
  breaks = c(-Inf, 24, 30, 34, 40, Inf)
  lab = c("<=24", "25-30", "31-34", "35-40", ">40")
  df[, age := cut(age_birth, breaks = breaks, labels = lab, right = TRUE)]
  
  # Fertility Intention (cf20m128, cf20m129, cf20m130)
  df = df %>% 
    mutate(intention = factor(cf20m128,
                              levels = c(1, 2, 3, NA),
                              labels = c('yes', 'no', 'notknown', 'missing'),
                              exclude = NULL),
           intention2 = cut(cf20m129,
                            breaks = c(-Inf, 0, 1, 2, Inf),
                            labels = c("0", "1", "2", ">2"),
                            right = T),
           intention3 = cut(cf20m130,
                            breaks =  c(-Inf, 1, 3, 6, Inf),
                            labels = c("0-1", "2-3", "4-6", ">6"),
                            right = TRUE))
  
  # Dealing with NAs (intention2)
  df[intention == "no", intention2 := "0"]
  df[is.na(intention2), intention2 := "missing"]
  
  # Dealing with NAs (intention3)
  df[intention == "no", intention3 := "no"]
  df[is.na(intention3), intention3 := "missing"]
  
  # Health
  
  # ch20m004  How would you describe your health, generally speaking?
  df$ch20m004 <- as.numeric(df$ch20m004)
  breaks = c(-Inf, 3, Inf)
  lab = c("low", "high")
  df[, health := cut(ch20m004, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(health), health := "missing"]
  
  # ch20m020	To what extent did your physical health or emotional problems 
  #           hinder your daily activities over the past month?
  
  df$ch20m020 <- as.numeric(df$ch20m020)
  breaks = c(-Inf, 1, 2, Inf)
  lab = c("no", "hardly", ">= a bit") #limited?
  df[, limited := cut(ch20m020, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(limited), limited := "missing"]
  
  # Depression
  # ch20m011	I felt very anxious
  # ch20m012	I felt so down that nothing could cheer me up
  # ch20m013	I felt calm and peaceful
  # ch20m014	I felt depressed and gloomy
  # ch20m015	I felt happy
  
  # Define a vector of new levels in reverse order (for the two "positive items")
  new_levels <- c(6, 5, 4, 3, 2, 1)
  
  # Convert the two positive items to a factor with the new levels
  df[, ch20m013 := factor(ch20m013, levels = new_levels)]
  df[, ch20m015 := factor(ch20m015, levels = new_levels)]
  
  df$ch20m011 <- as.numeric(df$ch20m011)
  df$ch20m012 <- as.numeric(df$ch20m012)
  df$ch20m013 <- as.numeric(df$ch20m013)
  df$ch20m014 <- as.numeric(df$ch20m014)
  df$ch20m015 <- as.numeric(df$ch20m015)
  
  # Now, sum all the scores
  df[, sum_depr := rowSums(.SD, na.rm = TRUE), 
     .SDcols = c("ch20m011", "ch20m012", "ch20m013", "ch20m014", "ch20m015")]
  
  df$sum_depr <- as.numeric(df$sum_depr)
  breaks = c(-Inf, 0, 10, 14, Inf)
  lab = c("missing", "low", "med", "high") #level of depression
  df[, depress := cut(sum_depr, breaks = breaks, labels = lab, right = TRUE)]
  
  # Personality
  # cp20l017	So far I have gotten the important things I want in life
  df$cp20l017 <- as.numeric(df$cp20l017)
  breaks = c(-Inf, 4, 6, Inf)
  lab = c("low", "med", "high") #level of satisfaction with achievements in life
  df[, implife := cut(cp20l017, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(implife), implife := "missing"]
  
  
  # data.table projection
  df = df[, .SD, .SDcols = keepcols]
  
  return(df)
}

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  # Generate predictions using the saved model and the input dataframe.
    
  # The predict_outcomes function accepts a dataframe as an argument
  # and returns a new dataframe with two columns: nomem_encr and
  # prediction. The nomem_encr column in the new dataframe replicates the
  # corresponding column from the input dataframe The prediction
  # column contains predictions for each corresponding nomem_encr. Each
  # prediction is represented as a binary value: '0' indicates that the
  # individual did not have a child during 2021-2023, while '1' implies that
  # they did.
  
  # Parameters:
  # df (dataframe): The data dataframe for which predictions are to be made.
  # background_df (dataframe): The background data dataframe for which predictions are to be made.
  # model_path (str): The path to the saved model file (which is the output of training.R).

  # Returns:
  # dataframe: A dataframe containing the identifiers and their corresponding predictions.
  
  ## This script contains a bare minimum working example
  if (!("nomem_encr" %in% colnames(df))) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }
  
  # Load the model
  model <- readRDS(model_path)
  
  # Unbundle the model 
  model = unbundle(model)
  
  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)
  
  # Generate predictions from model
  predictions <- predict(model,
                         df,
                         type = "prob")
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  predictions <- ifelse(predictions$.pred_1 > 0.5, 1, 0)
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[, "nomem_encr"],
                           "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction")
  
  # Return only dataset with predictions and identifier
  return(df_predict)
}
