rm(list = ls())
# load the functions the training dataset 
source("used_libraries.R")
source("functions_step_one.R")
path <- "/home/home1/hydro/abbasi/DRYvER_modeling_version2/"
path_data <- paste0(path, "model_data.RData")
model_data1 <- load_data(path_data = path_data)
in_task <- create_task(in_tbl = model_data1$model_data,
                       target_name = "target",
                       breaks = c(-Inf, 0, 31), 
                       labels = c("0", "1"),
                       problem_type = "classif")

path_resdir <- getwd()
# plan running random forest first step ----
plan_runmodels <- drake::drake_plan(
  measures = target(list(classif = msr("classif.bacc"),
                         regr = msr("regr.mae"))
  ),
  task = target(in_task),
  baselearners = target(create_baselearners(task)),
  seplearners = target(readd(baselearners, subtarget_list = FALSE)),
  autotuning = target(
    set_tuning(in_learner = seplearners,
               in_measure = measures,
               nfeatures = length(task$feature_names),
               insamp_nfolds = 4, insamp_neval = 30,
               insamp_nbatch = parallel::detectCores(logical = FALSE) - 5)
    , dynamic = map(seplearners)
  ),
  resamplingset = target(
    set_cvresampling(rsmp_id = "repeated_cv",
                     in_task = task,
                     outsamp_nrep = 2,
                     outsamp_nfolds = 3)
  ),
  rfresampled_classif = target(
    dynamic_resample(in_task = task,
                     in_learner = autotuning,
                     in_resampling = resamplingset,
                     store_models = TRUE,
                     type = "classif"),
    dynamic = map(autotuning)
  ),
  rfbm_classif = target(
    combine_bm(in_resampleresults = readd(rfresampled_classif,
                                          subtarget_list = TRUE),
               write_qs = T, inp_resdir = path_resdir)
    # ,
    # trigger  = trigger(mode = "condition", condition =FALSE)
  ),
  selected_learner = target("oversample_10.classif.ranger"),
  tasks_featsel = target(
    select_features(
      in_bm = rfbm_classif,
      in_lrnid =  selected_learner,
      in_task = task,
      pcutoff = 0.05,
      inp_resdir = path_resdir
    )
    # ,
    # trigger  = trigger(mode = "condition", condition =FALSE)
  ),
  resamplingset_featsel = target(
    set_cvresampling(rsmp_id = in_strategy,
                     in_task = task,
                     outsamp_nrep = in_outrep,
                     outsamp_nfolds = in_outfolds),
    transform = map(in_strategy = c('repeated_cv', "repeated_spcv_coords"),
                    in_outrep = c(2, 1),
                    in_outfolds = c(3, 20),
                    .names = c('featsel_cv', 'featsel_spcv'))
    # ,
    # trigger  = trigger(mode = "condition", condition =TRUE)
  ),
  
  rfresampled_featsel = target(
    dynamic_resamplebm(in_task = in_taskfeatsel,
                       in_bm = rfbm_classif,
                       in_lrnid =  selected_learner,
                       in_resampling = in_resampling,
                       store_models = store_models,
                       inp_resdir = path_resdir,
                       type = 'classif'),
    transform= map(in_taskfeatsel = c(tasks_featsel[[2]],
                                      tasks_featsel[[2]]),
                   in_resampling = c(featsel_cv, featsel_spcv),
                   store_models = c(TRUE, TRUE),
                   .names = c('res_featsel_cv', 'res_featsel_spcv'))
    # trigger  = trigger(mode = "condition", condition =FALSE)
  ),
  
  # rfeval_featall = target(c(res_all_cv))
  # ,
  rfeval_featsel = target(c(res_featsel_cv, res_featsel_spcv)) #Cannot use combine as lead to BenchmarkResult directly in the branching)
  # ,
  # 
  # rfbm_featall= target(
  #   analyze_benchmark(in_bm = rfeval_featall,
  #                     in_measure = measures)
  # ,
  # trigger  = trigger(mode = "condition", condition =FALSE)
  # )
  ,
  
  rfbm_featsel = target(
    analyze_benchmark(in_bm = rfeval_featsel,
                      in_measure = measures)
    # ,
    # trigger  = trigger(mode = "condition", condition =FALSE)
  ),
  rftuned = target(
    selecttrain_rf(in_rf = res_featsel_cv,
                   in_learnerid = selected_learner,
                   in_task = "binary_class")
    # ,
    # trigger  = trigger(mode = "condition", condition =FALSE)
  ),
  predvars = target(
    predname_df(task = task)
  ),
  
  
  vimp_plot = ggvimp(in_rftuned = rftuned, in_predvars = predvars,
                     varnum=17, spatial_rsp = FALSE),
  
  pd_plot = ggpartialdep(in_rftuned=rftuned,
                         in_predvars=predvars,
                         colnums=1:27,
                         nvariate=1,  nodupli = FALSE, ngrid = 20, parallel = F,
                         spatial_rsp = FALSE)
)

