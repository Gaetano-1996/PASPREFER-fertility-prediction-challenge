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
  model_df = merge(cleaned_df, # merging with outcome data
                   outcome_df, 
                   by = "nomem_encr") %>% 
    filter(!is.na(new_child)) %>% # filtering obs. with no outcome
    mutate(new_child = factor(new_child)) # converting response to factor
  # for classification
  
  
  # Data Modeling
  # Splitting data budget for cross-validation only
  set.seed(123)
  
  # Defining folds 
  folds = vfold_cv(model_df,
                   repeats = 3, # repeating 10-folds cv for 3 time 
                   strata = new_child) # stratifying for classes
  
  # Defining recipe
  rf_recipe =
    recipe(formula = new_child ~.,
           data = model_df %>% 
             # removing id code from variable
             select(-nomem_encr, -intention))  
  
  # Model Specification
  rf_spec = 
    rand_forest(mtry = tune(), # N. of candiadates predictors at each split
                min_n = tune(), # Min. N. of obs. x terminal node
                trees = tune()) %>% # N. of trees
    set_mode("classification") %>% 
    set_engine("ranger")
  
  
  # Setting workflow
  rf_wflow =
    workflow() %>% 
    add_recipe(rf_recipe) %>% 
    add_model(rf_spec)
  
  # Hyper-params grid
  grid_params = grid_regular(
    finalize(mtry(), # number of candidates for each split
             model_df %>% 
               select(-nomem_encr, -intention)),
    trees(range = c(100,5000)), # number of obs. for final node (smoothness)
    min_n(range = c(2,20)), # number of parallel sample (1 tree: 1 sample)
    levels = 5
  )
  
  # Tuning
  set.seed(123)
  doParallel::registerDoParallel( # parallel backend
    cores = detectCores()) # setting number of cores to the max available 
  
  rf_tune =
    tune_grid(
      rf_wflow, # workflow
      resamples = folds, # resample
      grid = grid_params, # hyper-parameters grid
      control = control_grid(
        verbose = F, 
        allow_par = T), # allowing for parallel backend
      metrics = metric_set(
        f_meas)
    )
  
  # Finalizing the model 
  final_model =
    rf_wflow %>% 
    finalize_workflow(select_best(
      rf_tune, metric = "f_meas"
    )) 
  
  
  # Saving the model 
  # Serialization 
  save_bundle =
    final_model %>% 
    fit(model_df) %>%
    butcher() %>% # extracting the actual model from its parnsip format
    bundle() # serilization of the model -> convert into binary
  
  # Save binary  
  model_pathname = paste0(save_to, "model.rds")
  saveRDS(save_bundle, 
          model_pathname)
  
}
