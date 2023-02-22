library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
# Set target-specific options such as packages.
tar_option_set(packages = c("tibble", "tidymodels", "tidyverse", "here"))

tar_source("functions_step_two.R")
tar_source("functions_step_one.R")
tar_source("used_libraries.R")

# End this file with a list of target objects.
list(
  
  tar_files(
    name = file_paths,
    command = list.files(path = here("data/train_test"),
                         pattern = "*.qs",
                         full.names = TRUE)
  ),
  tar_target(
    name = train_test_data,
    command = read_qsfiles(file_paths),
    pattern = map(file_paths))
  ,
  tar_target(
    name = calibration_data,
    command = create_tbl2(in_data = train_test_data[[1]],
                          target_name = "target",
                          breaks = c(-Inf, 2, 8, 15, 22, 29, 31),
                          labels = c("1", "2", "3", "4", "5", "6"),
                          remove_cols = c("dd_id", "date", "X", "Y"),
                          spatial = FALSE)),
  
  tar_target(
    name = validation_data,
    command = create_tbl2(in_data = train_test_data[[2]],
                          target_name = "target",
                          breaks = c(-Inf, 2, 8, 15, 22, 29, 31),
                          labels = c("1", "2", "3", "4", "5", "6"),
                          remove_cols = c("dd_id", "date", "X", "Y"),
                          spatial = FALSE)),
  
  tar_target(
    name = rf_recipe,
    command = set_recipe(in_data = calibration_data,
                         over_ratio = 0.8)),
  
  tar_target(
    name = rf_model,
    command = set_model(ncore = 15)
  ),
  
  tar_target(
    name = rf_workflow,
    command = set_workflow(model_setup = rf_model,
                           model_recipe = rf_recipe)),
  
  tar_target(
    name = rf_resample,
    command = set_resampling(in_data = calibration_data,
                             folds = 3,
                             type = "cv",
                             rep_num = 2)),
  
  tar_target(
    name = rf_runmodel,
    command = tune_model(model_workflow = rf_workflow,
                         resamples = rf_resample,
                         grid_iter = 100)),
  tar_target(
    name = rf_impvar,
    command = select_vimp(in_runmodel = rf_runmodel,
                          model_workflow = rf_workflow,
                          cal_data = calibration_data,
                          pvalue_numper = 60)),
  
  tar_target(
    name = rf_best,
    command = select_best_model(in_model = rf_runmodel)),
  
  tar_target(
    name = rf_tuned_step2,
    command = fit_final_model(in_data = calibration_data,
                              model_workflow = rf_workflow,
                              best_model = rf_best)),
  # tar_target(
  #   name = cal_predicted_tbl,
  #   command = cal_model_data(final_model = rf_tuned_step2,
  #                            new_data = calibration_data)
  # ),
  
  tar_target(
    name = val_predicted_tbl,
    command = cal_model_data(final_model = rf_tuned_step2,
                             new_data = validation_data)
  ),
  tar_target(
    name = cal_criteria,
    command = evaluate_model(model_tuned_workflow = rf_runmodel)
  ),
  tar_target(
    name = val_criteria,
    command = evaluate_model(pred_tbl = val_predicted_tbl)
  )  
  
)
