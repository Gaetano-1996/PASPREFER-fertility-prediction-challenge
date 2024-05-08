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
  
  
  # Modeling
  # splitting data budget between train and test
  set.seed(123)
  # stratifying to help with imbalance
  model_split = initial_split(model_df,
                              strata = new_child) 
  model_train = training(model_split) # training set
  model_test = testing(model_split) # testing set
  
  # Resampling (bootstrap version)
  set.seed(123)
  # stratifying at resample leve as well
  boots = bootstraps(model_train,
                     strata = new_child)
  
  # Defining recipe
  rf_recipe =
    recipe(formula = new_child ~.,
           data = model_train) %>% 
    step_rm(nomem_encr) # removing id code from variable
  
  # Model Specification
  rf_spec = 
    rand_forest(mtry = tune(), 
                min_n = tune(), 
                trees = tune()) %>% 
    set_mode("classification") %>% 
    set_engine("ranger") # using ranger for speeeed
  
  # Setting workflow
  rf_wflow =
    workflow() %>% 
    add_recipe(rf_recipe) %>% 
    add_model(rf_spec)
  
  # Hyper-params grid
  grid_params = grid_regular(
    mtry(range= c(3,8)), # number of candidates for each split
    trees(range = c(100,1000)), # number of obs. for final node (smoothness)
    min_n(range = c(2,20)) # number of parallel sample (1 tree: 1 sample)
  )
  
  # Tuning
  set.seed(123)
  doParallel::registerDoParallel( # parallel backend
    cores = detectCores()) # setting number of cores to the max available 
  
  rf_tune =
    tune_grid(
      rf_wflow, # workflow
      resamples = boots, # resample
      grid = grid_params, # hyper-parameters grid
      control = control_grid(
        verbose = F, 
        allow_par = T # allowing for parallel backend
      )
    )
  
  # Finalizing the model 
  final_model =
    rf_wflow %>% 
    finalize_workflow(select_best(
      rf_tune, metric = "accuracy"
    )) %>% 
    last_fit(model_split)
  
  
  # Saving the model 
  # Serialization 
  save_bundle =
    final_model %>% 
    extract_workflow() %>% 
    butcher() %>% # extracting the actual model from its parnsip format
    bundle() # serilization of the model -> convert into binary
  
  # Save binary  
  model_pathname = paste0(save_to, "model.rds")
  saveRDS(save_bundle, 
          model_pathname)
  
  # returning the model to the env
  # return(model)
}
