# PASPREFER - FINAL SUBMISSION #
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
  model_df = merge(cleaned_df, # merging with outcome data
                   outcome_df, 
                   by = "nomem_encr") %>% 
    filter(!is.na(new_child)) %>% # filtering obs. with no outcome
    mutate(new_child = factor(new_child)) # converting response to factor
  
  
  # Data Modeling
  # Splitting data budget for cross-validation only
  set.seed(123)
  
  # Defining folds 
  folds = vfold_cv(model_df,
                   repeats = 3, # repeating 10-folds cv for 3 time 
                   strata = new_child) # stratifying for classes
  
  # OTHER DATA RECIPE (NO INTENTION)
  recipe_other = 
    recipe(formula = new_child ~ .,
           data = model_df %>% select(-nomem_encr,
                                      # no intentions
                                      -intentionB,-intention2B, -intention3B,
                                      # taking values sum and 2c
                                      -values_1c, -values_3c, -values_4c,
                                      # removing low importance vars
                                      -migration, -gender_emp, -soc_lib)) 
  
  # INTENTION ONLY
  recipe_intentions = 
    recipe(
      # try to include intentionB to acocunt for "not_known" level
      formula = new_child ~  intentionB + intention2B + intention3B,
      data = model_df) 
  
  # MODEL SPECIFICATION
  rf_specs =
    rand_forest(
      mtry = tune(),
      min_n = tune(),
      trees = tune()
    ) %>% 
    set_mode("classification") %>% 
    set_engine("ranger")
  
  # WORKFLOWS
  wf_other = 
    workflow() %>% 
    add_recipe(recipe_other) %>% 
    add_model(rf_specs)
  
  wf_intentions = 
    workflow() %>% 
    add_recipe(recipe_intentions) %>% 
    add_model(rf_specs)
  
  # Creating general control grid for stack
  ctrl_grid = control_stack_grid()
  
  ## TUNING OTHER MODEL --------------------------------------------------------
  set.seed(93568)
  grid_other =
    grid_latin_hypercube(
      mtry(range = c(1,30)),
      trees(range = c(50,2000)),
      min_n(),
      size = 250
    )
  
  set.seed(93568)
  doParallel::registerDoParallel( # parallel backend
    cores = detectCores())
  
  rf_other = 
    wf_other %>% 
    tune_grid(
      resample = folds,
      grid = grid_other,
      metrics = metric_set(pr_auc),
      control = ctrl_grid
    )
  
  
  ## TUNING INTENTIONS MODEL ---------------------------------------------------
  set.seed(93568)
  grid_intentions =
    grid_latin_hypercube(
      mtry(range = c(1,2)),
      trees(range= c(1,1000)),
      min_n(), # default from 2 to 40
      size = 125
    )
  
  set.seed(93568)
  doParallel::registerDoParallel( # parallel backend
    cores = detectCores())
  
  rf_intentions = 
    wf_intentions %>% 
    tune_grid(
      resample = folds,
      grid = grid_intentions,
      metrics = metric_set(pr_auc),
      control = ctrl_grid
    )
  
  ## Stacking --------------------------------------------------------------------
  set.seed(93568)
  rf_stack =
    stacks() %>% 
    add_candidates(rf_intentions) %>% 
    add_candidates(rf_other) %>% 
    blend_predictions(
      metric = metric_set(f_meas),
      times = 100 # improve stability of the resampling process
    ) %>% 
    fit_members()
  
  
  # Saving the model 
  # Serialization 
  save_bundle =
    rf_stack %>%
    butcher() %>% # extracting the actual model from its parnsip format
    bundle() # serilization of the model -> convert into binary
  
  # Save binary  
  model_pathname = paste0(save_to, "model.rds")
  saveRDS(save_bundle, 
          model_pathname)
  
}