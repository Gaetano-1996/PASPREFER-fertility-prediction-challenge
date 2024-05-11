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

clean_df = function(df, # train data
                    background_df = NULL){ # background data (if needed)
  # Preprocess the input dataframe to feed the model.
  
  # @ARGS:
  # df -> [data.frame]: The input dataframe containing the raw data
  #(e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background -> [data.frame]: Optional input dataframe containing background 
  #data (e.g., from PreFer_train_background_data.csv or
  #PreFer_fake_background_data.csv).
  
  # @RETURNS:
  # df -> [data.frame]: The cleaned dataframe with only the necessary columns
  #and processed variables.
  
  df = setDT(df)
  # selecting only obs. for which the outcome is available 
  df = df[outcome_available == 1,]
  
  # extracting subset of predictors
  keepcols = c("nomem_encr",
               'age',
               'gender',
               'intention',
               'intention2',
               'intention3',
               #'past_intention',
               'health',
               'limited',
               'depress',
               'implife',
               'education',
               'partner',
               'n_children',
               'occupation',
               'income',
               'migration'
  )
  
  # GENDER 
  # Imputation of gender from background info
  imp.gender = ifelse(is.na(df$cf20m003),df$gender_bg,df$cf20m003)
  
  # Converting gender into factor
  df[, gender := factor(
    imp.gender,
    levels = c(1, 2),
    labels = c('male', 'female'),
    exclude = NULL)]
  
  # AGE 
  # Ensuring numeric type
  df$birthyear_bg = as.numeric(df$birthyear_bg)
  # Creating the age from the birth year
  age_birth = 2020 - df$birthyear_bg
  
  # Creating classes
  breaks = c(-Inf, 24, 30, 34, 40, Inf)
  lab = c("<=24", "25-30", "31-34", "35-40", ">40")
  df[, age := cut(age_birth, breaks = breaks, labels = lab, right = TRUE)]
  
  # FERTILITY INTENTION 
  # cf20m128: Do you think you will have [more] children in the future?
  df[, intention := factor(
    cf20m128,
    levels = c(1, 2, 3, NA),
    labels = c('yes', 'no', 'notknown', 'missing'),
    exclude = NULL
  )]
  
  # cf20m129: How many [more] children do you think you will have in the future?
  df[, intention2 := cut(
    cf20m129,
    breaks = c(-Inf, 0, # 0
               1, #1
               2, #2
               Inf),#>2
    labels = c("0", "1", "2", ">2"),
    right = T
  )]
  
  # Dealing with NAs
  df[intention == "no", intention2 := "0"]
  df[is.na(intention2), intention2 := "missing"]
  
  # cf20m130: Within how many years do you hope to have your [next/first] child?
  # handling weird value, such as 2025
  df[cf20m130>2020, cf20m130 := cf20m130-2020]
  
  df[, intention3 := cut(
    cf20m130,
    breaks =  c(-Inf,1, # 0-1
                3, # 2-3
                6, # 4-6
                Inf),# >6
    labels = c("0-1", "2-3", "4-6", ">6"),
    right = T
  )]
  
  # Dealing with NAs
  df[intention == "no", intention3 := "no"]
  df[is.na(intention3), intention3 := "missing"]
  
  
  # EDUCATION
  # oplcat_2020
  # Coerce to numeric
  df$oplcat_2020 <- as.numeric(df$oplcat_2020)
  # Converting to ordinal
  df[, education := cut(
    oplcat_2020,
    breaks = c(-Inf, 2, #lower = up to int sec
               3, # high sec.
               4, 
               5,
               6, Inf),
    labels = c('lower', 'high sec.', 
               'int. vocational','high vocational',
               'university','missing'),
    right = T)]
  df[is.na(education), education := 'missing']
  
  # NUMBER OF CHILDREN (COMPOSED) 
  # cf20-19-18-17*454 + cf20-19-18-17*455
  df$n_children = ifelse(df$cf20m454==2,0,df$cf20m455)
  
  df$n_children = ifelse(is.na(df$n_children),
                         ifelse(df$cf19l454==2,NA,df$cf19l455),
                         df$n_children)
  
  df$n_children = ifelse(is.na(df$n_children),
                         ifelse(df$cf18k454==2,NA,df$cf18k455),
                         df$n_children)
  
  df$n_children <- ifelse(is.na(df$n_children), 
                          ifelse(df$cf17j454==2,NA, df$cf17j455),
                          df$n_children)
  
  # Converting it to a factor
  df[,n_children := cut(
    n_children,
    breaks = c(-Inf,0, #0
               1, #1
               2, #2
               Inf), #>2
    labels = c("0", "1", "2", ">2"),
    right=T
  )]
  
  # Dealing with NAs
  df[is.na(n_children), n_children := "missing"]
  
  # PARTNERSHIP (composed) 
  # cf20m024 + burgstat_2020
  df$partner <- as.numeric(df$cf20m024==1) # have a partner in 2020
  df$burgstat_2020 <- as.numeric(df$burgstat_2020==1) # married in 2020
  # imputing partnership from civil status (2020)
  df$partner <- ifelse(is.na(df$partner),
                       df$burgstat_2020==1,
                       df$partner)
  # coercing to factor
  df[,partner := as.factor(partner)]
  # Dealing with NAs
  df[is.na(partner), partner := "missing"]
  
  # OCCUPATION  
  # belbezig_2020
  
  df <- df %>%
    mutate(occupation = case_when(
      belbezig_2020 %in% c(1, 2) ~ "work",
      belbezig_2020 %in% c(3:5, 8:13) ~ "not_work" ,
      belbezig_2020 %in% c(7) ~ "in_school",
      TRUE ~ "missing"
    ))
  df[, occupation := factor(occupation)]
  # INCOME 
  # nettoink_f_20**
  # Imputation from 2019
  df$nettoink_f_2020 = ifelse(is.na(df$nettoink_f_2020),
                              df$nettoink_f_2019,
                              df$nettoink_f_2020)
  
  # Imputation from 2018
  df$nettoink_f_2020 <- ifelse(is.na(df$nettoink_f_2020),
                               df$nettoink_f_2018,
                               df$nettoink_f_2020)
  
  df$nettoink_f_2020 <- as.numeric(df$nettoink_f_2020)
  breaks = c(-Inf, 900, 1900, 2400, Inf)
  lab = c("q1", "q2", "q3",  "q4")
  df[, income := cut(nettoink_f_2020, 
                     breaks = breaks, 
                     labels = lab, 
                     right = TRUE)]
  
  # Dealing with NAs
  df[is.na(nettoink_f_2020), income := "missing"]
  
  # MIGRATION BG 
  table(df$migration_background_bg, useNA = "always")
  
  df <- df %>%
    mutate(migration = case_when(
      migration_background_bg == 0 ~ "Dutch",
      migration_background_bg == 101 ~ "West_1st" ,
      migration_background_bg == 102 ~ "Not_west_1st",
      migration_background_bg == 201 ~ "West_2nd",
      migration_background_bg == 202 ~ "Not_west_2nd",
      TRUE ~ "missing"
    ))
  
  df[,migration := factor(migration)]
  # HEALTH 
  
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
  
  # DEPRESSION (COMPOSED) 
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
  
  # PERSONALITY 
  # cp20l017	So far I have gotten the important things I want in life
  df$cp20l017 <- as.numeric(df$cp20l017)
  breaks = c(-Inf, 4, 6, Inf)
  lab = c("low", "med", "high") #level of satisfaction with achievements in life
  df[, implife := cut(cp20l017, breaks = breaks, labels = lab, right = TRUE)]
  df[is.na(implife), implife := "missing"]
  
  
  # data.table projection
  df =df[, .SD, .SDcols = keepcols]
  
  return(df)
}

predict_outcomes <- function(df, 
                             background_df = NULL, 
                             model_path = "./model.rds"){
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
  predictions <- ifelse(predictions$.pred_1 > 0.4, # lowering probs
                        1, 0)
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[, "nomem_encr"],
                           "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction")
  
  # Return only dataset with predictions and identifier
  return(df_predict)
}