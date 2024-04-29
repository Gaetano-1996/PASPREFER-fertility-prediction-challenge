# This is an example script to train your model given the (cleaned) input dataset.
# 
# This script will not be run on the holdout data, 
# but the resulting model model.joblib will be applied to the holdout data.
# 
# It is important to document your training steps here, including seed, 
# number of folds, model, et cetera

train_save_model = function(cleaned_df, # clean training set
                            outcome_df, # outcome set
                            save_to ="./"){ # path for saving the model 
  
  # Trains a model using the cleaned dataframe and saves the model to a file.
  
  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function 
  #                         to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable 
  #                         (e.g., PreFer_train_outcome or PreFer_fake_outcome).
  
  # Combine cleaned_df and outcome_df
  model_df <- merge(cleaned_df, 
                    outcome_df, 
                    by = "nomem_encr")
  # selecting only those with outcome available
  model_df_out = model_df[!is.na(new_child),]
  
  # Logistic regression model
  model <- glm(new_child ~ gender + age + intention, 
               family = "binomial", 
               data = model_df_out)
  
  # diagnostics
  print(summary(model))
  
  # SAVING THE MODEL
  model_pathname = paste0(save_to, "model.rds")
  saveRDS(model, 
          model_pathname)
}
