
# ----------------- training the RF model --------
load_data <- function(path_data, remove_cols = NULL){
  load(path_data)
  filename_char <- basename(path_data) %>%
    tools::file_path_sans_ext()
  model_data <- get(filename_char) %>% na.omit()
  
  len_chr <- grep(".*character", sapply(model_data, typeof)) %>%
    length()
  cols_chr <- names(model_data) %>%
    .[grep(".*character", sapply(model_data, typeof))]
  
  if (is.null(remove_cols) & len_chr == 0){
    model_data <- model_data[is.finite(rowSums(model_data)), ]
  } else if (!is.null(remove_cols) & len_chr == 0) {
    model_data <- model_data %>% 
      dplyr::select(-dplyr::all_of(remove_cols)) %>% 
      .[is.finite(rowSums(.)), ]
  } else if (!is.null(remove_cols) & len_chr > 0) {
    remove_cols <- c(remove_cols, cols_chr)
    dd_id_ts <- model_data %>% pull(cols_chr)
    model_data <- model_data %>% 
      dplyr::select(-dplyr::all_of(remove_cols)) 
    infinite_rows <- which(!is.finite(rowSums(model_data))) %>% 
      as.numeric()
    model_data <- model_data[is.finite(rowSums(model_data)), ]
    dd_id_ts <- dd_id_ts[-infinite_rows]
  } else if (is.null(remove_cols) & len_chr > 0) {
    dd_id_ts <- model_data %>% pull(cols_chr)
    date_ts <- model_data %>% pull("date")
    model_data <- model_data %>% 
      dplyr::select(-dplyr::all_of(c(cols_chr, "date"))) 
    infinite_rows <- which(!is.finite(rowSums(model_data))) %>% 
      as.numeric()
    model_data <- model_data[is.finite(rowSums(model_data)), ]
    # dd_id_ts <- dd_id_ts[-infinite_rows]
    # date_ts <- date_ts[-infinite_rows]
  } else
    cat("the remove_col must be a character vector.")
  model_data <- model_data %>% `rownames<-`(NULL)
  return(list(model_data = model_data,
              dd_id_ts = dd_id_ts,
              date_ts = date_ts))
}

## classify 
classify_target <- function(tbl,target_col, breaks, labels){
  target_class <- tbl %>%
    pull(target_col) %>%
    cut(breaks = breaks,
        labels = labels)
  return(target_class)
}

# creating task for different problem type ---
create_task <- function(in_tbl, target_name, breaks, labels, problem_type, step, ...){
  colnames(in_tbl) <- make.names(names(in_tbl),
                                 unique = TRUE)
  if (problem_type == "regr"){
    task <- mlr3spatiotempcv::TaskRegrST$new(
      id = "regression_no_flow_days",
      backend = in_tbl,
      target = target_name,
      coordinate_names = c("X", "Y")
    )
  }else if (problem_type == "classif") {
    if (step == "first") {
      target_class <- classify_target(tbl = in_tbl,
                                      target_col = target_name,
                                      breaks = breaks,
                                      labels = labels)
      
      in_tbl_editted <- in_tbl %>%
        dplyr::select(-target_name) %>% 
        dplyr::mutate(target_class) %>%
        `rownames<-`(NULL)
      
      task <- mlr3spatiotempcv::TaskClassifST$new(
        id = "binary_class",
        backend = in_tbl_editted,
        target = "target_class",
        coordinate_names = c("X", "Y"))
    }
    if (step == "second"){
      target_class <- in_tbl %>%
        dplyr::pull(target_name)%>% 
        cut(breaks = breaks,
            labels = labels)
      
      in_tbl_editted <- in_tbl %>%
        dplyr::select(-target_name) %>% 
        dplyr::mutate(target_class) %>%
        `rownames<-`(NULL)
      
      # data_undersampled <- scutr::SCUT(data = in_tbl_editted,
      #                                  cls_col = "target_class",
      #                                  undersample = undersample_hclust)
      # data_undersampled <- data_undersampled %>%
      #   mutate_if(is.character, as.factor)
      
      task <- mlr3spatiotempcv::TaskClassifST$new(
        id = "multi_class",
        backend = in_tbl_editted,
        target = "target_class",
        coordinate_names = c("X", "Y"))
    }
    
    
    
  } else {
    stop("the problem type have to be one of regr and classif types,
         please check the problem_type argument.\n")
  }
  
  return(task)
  }

## create baselearners -----
create_baselearners <- function(in_task, ncores = parallel::detectCores()-3){
  lrns <- list()
  
  if (is.list(in_task)) {
    in_task <- in_task[[1]]
  }
  if (inherits(in_task, "TaskClassif")) {
    
    lrns[['lrn_ranger']] <- mlr3::lrn('classif.ranger',
                                      num.trees = 1000,
                                      sample.fraction = 0.632,
                                      replace = FALSE,
                                      splitrule = 'gini',
                                      predict_type = 'prob',
                                      importance = 'impurity_corrected',
                                      respect.unordered.factors = 'order')
    set_threads(lrns[["lrn_ranger"]], n = ncores)
    po_over_10 <- mlr3pipelines::po("classbalancing", id = "oversample_10", adjust = "minor",
                                    reference = "minor", shuffle = TRUE,
                                    ratio = 10)
    po_over_20 <- mlr3pipelines::po("classbalancing", id = "oversample_20", adjust = "minor",
                                    reference = "minor", shuffle = TRUE,
                                    ratio = 20)
    #Create graph learners so that oversampling happens systematically upstream of all training
    lrns[['lrn_ranger_overp_10']] <- mlr3pipelines::GraphLearner$new(
      po_over_10 %>>% lrns[['lrn_ranger']])
    lrns[['lrn_ranger_overp_20']] <- mlr3pipelines::GraphLearner$new(
      po_over_20 %>>% lrns[['lrn_ranger']])
  }
  # if (inherits(in_task, 'TaskRegr')) {
  #   #Create regression learner with maxstat
  #   lrns[['lrn_ranger_maxstat']] <- mlr3::lrn('regr.ranger',
  #                                             num.trees=800,
  #                                             sample.fraction = 0.632,
  #                                             min.node.size = 10,
  #                                             replace=FALSE,
  #                                             # splitrule = 'maxstat',
  #                                             importance = 'permutation',
  #                                             respect.unordered.factors = 'order')
  #   set_threads(lrns[["lrn_ranger_maxstat"]], n = ncores)
  # }
  
  return(lrns)
}

## set tuning parameters ------
set_tuning <- function(in_learner, in_measure, nfeatures,
                       insamp_nfolds, insamp_neval, insamp_nbatch) {
  
  if (is.list(in_learner)) {
    in_learner <- in_learner[[1]]
  }
  
  #Define paramet space to explore
  regex_tuneset <- function(in_learner) {
    prmset <- names(in_learner$param_set$tags)
    tune_rf <- ParamSet$new(list(
      ParamInt$new(grep(".*mtry", prmset, value=T)[1],
                   lower = floor(0.1*nfeatures),
                   upper = floor(0.5*nfeatures)), #Half number of features
      ParamDbl$new(grep(".*fraction", prmset, value=T),
                   lower = 0.2,
                   upper = 0.8)
    ))
    
    in_split =in_learner$param_set$get_values()[
      grep(".*split(rule|stat)", prmset, value=T)]
    
    if (in_split == 'maxstat') {
      tune_rf$add(
        ParamDbl$new(prmset[grep(".*alpha", prmset)],
                     lower = 0.01, upper = 0.1)
      )
      
    } else if (any(grepl(".*min.node.size", prmset))) {
      tune_rf$add(
        ParamInt$new(prmset[grep(".*min.node.size", prmset)],
                     lower = 1, upper = 10)
      )
    } else if (any(grepl(".*splitstat", prmset))) {
      tune_rf$add(
        ParamDbl$new(prmset[grep(".*alpha", prmset)],
                     lower = 0.01, upper = 0.1)
      )
    }
  }
  
  #Define inner resampling strategy
  rcv_rf = rsmp("cv", folds=insamp_nfolds) #aspatial CV repeated 10 times
  
  #Define termination rule
  evalsn = mlr3tuning::trm("evals", n_evals = insamp_neval) #termine tuning after insamp_neval rounds
  
  if (in_learner$task_type == 'classif') {
    if (inherits(in_measure, 'list')) {
      in_measure <- in_measure$classif
    }
    
    if (grepl('classif[.]cforest$', in_learner$id)) {
      learnertune <- in_learner
    } else if (grepl('classif[.]ranger$', in_learner$id)) {
      learnertune <- AutoTuner$new(learner= in_learner,
                                   resampling = rcv_rf,
                                   measure = in_measure,
                                   search_space = regex_tuneset(in_learner),
                                   terminator = evalsn,
                                   tuner =  tnr("random_search",
                                                batch_size = insamp_nbatch)) #batch_size determines level of parallelism
    } else{
      stop('The classification learner provided is not configurable with this workflow yet...')
    }
  } else if (in_learner$task_type == 'regr') {
    if (inherits(in_measure, 'list')) {
      in_measure <- in_measure$regr
    }
    
    learnertune <- AutoTuner$new(learner= in_learner,
                                 resampling = rcv_rf,
                                 measure = in_measure,
                                 search_space = regex_tuneset(in_learner),
                                 terminator = evalsn,
                                 tuner =  tnr("random_search",
                                              batch_size = insamp_nbatch))
  }
  
  #learnertune$store_tuning_instance = FALSE
  learnertune$id <- in_learner$id
  
  return(learnertune)
}

## set_cvresampling -----
set_cvresampling <- function(rsmp_id, in_task, outsamp_nrep, outsamp_nfolds) {
  #repeated_cv or repeated-spcv-coords
  outer_resampling = rsmp(rsmp_id,
                          repeats = outsamp_nrep,
                          folds = outsamp_nfolds)
  outer_resampling$instantiate(in_task)
  
  return(outer_resampling)
}

# dynamic_resample--------
# subsampling the data for dealing with imbalanced data
dynamic_resample <- function(in_task, in_learner, in_resampling, type,
                             store_models = FALSE) {
  if (is.list(in_learner)) {
    in_learner <- in_learner[[1]]
  }
  
  if (is.list(in_task)) {
    in_task <- in_task[[1]]
  }
  
  if (inherits(in_learner, 'BenchmarkResult')) {
    print(('BenchmarkResults was provided, getting the learner...'))
    in_learner <- in_learner$learners$learner[[1]]
  }
  
  #Make sure autotuner matches task (adjust mtry)
  if (inherits(in_learner, 'AutoTuner')) {
    in_learner <- reset_tuning(in_autotuner = in_learner,
                               in_task = in_task)
  }
  
  if ((in_learner$task_type == 'classif' & type=='classif') |
      (in_learner$task_type == 'regr' & type=='regr')) {
    resmp_rs <- mlr3::resample(learner = in_learner, task = in_task,
                               resampling = in_resampling, store_models = store_models
    )
    return(resmp_rs)
  }
}
# weighted_sd -----
weighted_sd <- function(x, w=NULL, na.rm=FALSE) {
  if (na.rm) {
    x <-  na.omit(x)
    if (length(w) > length(x)) {
      w <- w[-which(is.na(x))]
    }
  }
  
  if (length(w)==0) {
    w <- rep(1, length(x))
  }
  
  #Compute weighted standard deviation
  return(sqrt(sum((w) * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)))
}

# combine_bm -----------
combine_bm <- function(in_resampleresults, write_qs = NULL, inp_resdir = NULL) {
  #When tried as_benchmark_result.ResampleResult, got "Error in setcolorder(data, slots) :
  # x has some duplicated column name(s): uhash. Please remove or rename the
  # duplicate(s) and try again.". SO use this instead
  print('Converting to benchmark results...')
  if (length(in_resampleresults) > 1) {
    bmres_list <- lapply(
      in_resampleresults[!sapply(in_resampleresults, is.null)],
      function(rsmpres) {
        print(rsmpres)
        if (!is.null(rsmpres)) {
          as_benchmark_result(rsmpres)
        }
      })
    #BenchmarkResult$new(rsmpres$data)})
    
    print('Combining...')
    bmrbase = bmres_list[[1]]
    for (i in 2:length(bmres_list)) {
      if (in_resampleresults[[i]]$task$task_type ==
          in_resampleresults[[1]]$task$task_type) {
        print(i)
        bmrbase$combine(bmres_list[[i]])
      } else {
        warning('ResampleResult #', i,
                'is not of the same task type as the first ResampleResult you provided, skipping...')
      }
    }
  } 
  else {
    warning('You provided only one resample result to combine_bm,
            simply returning output from as_benchmark_result...')
    bmrbase = as_benchmark_result(in_resampleresults[[1]])
  }
  print('Done combining, now writing to qs...')
  if (write_qs) {
    out_filen <- paste0('combine_bm', format(Sys.time(), '%Y%m%d%H%M%s'), '.qs')
    out_qs <- file.path(inp_resdir, out_filen)
  }
  
  qs::qsave(bmrbase, out_qs)
  
  return(out_filen)
}
# select_features ------
select_features <- function(in_bm, in_lrnid, in_task, pcutoff, inp_resdir = NULL) {
  
  #If path, read qs
  if (inherits(in_bm, "character")) {
    in_bm <- qs::qread(file.path(inp_resdir, in_bm))
  }
  
  #get desired resampled_results/learner
  if (inherits(in_bm, "BenchmarkResult")) {
    in_rf <- in_bm$filter(learner_ids = in_lrnid)
  } else {
    in_rf <- as_benchmark_result(in_bm)
  }
  
  #Apply feature/variable selection
  vimp <- weighted_vimportance_nestedrf(
    rfresamp = in_rf$resample_result(uhash=unique(as.data.table(in_rf)$uhash)),
    pvalue = TRUE) %>%
    .[,imp_wmeanper := imp_wmean/sum(imp_wmean)]
  
  task_featsel <- in_task$clone()$select(
    vimp[imp_pvalue <= pcutoff, as.character(varnames)])
  task_featsel$id <- paste0(in_task$id, '_featsel')
  
  return(list(in_task, task_featsel))
}
# weighted_vimportance_nestedrf ----------
weighted_vimportance_nestedrf <- function(rfresamp,
                                          pvalue = TRUE, pvalue_permutn = 35) {
  varnames <- rfresamp$task$feature_names
  rfresampdt <- as.data.table(rfresamp)
  
  vimportance_all <- rfresampdt[, extract_impperf_nestedrf(
    in_rflearner = learner, #Extract vimp and perf for each resampling instance
    in_task = task,
    imp=T, perf=T, pvalue=pvalue, pvalue_permutn), by=iteration] %>%
    cbind(., varnames)
  
  ####!!!!!!!!!!Adapt to allow for other measure than classif.bacc!!!!!!!!######
  out_vimportance <- vimportance_all[
    , list(imp_wmean = weighted.mean(importance, classif.bacc), #Compute weighted mean
           imp_wsd =  weighted_sd(x=importance, w=classif.bacc)), #Compute weighted sd
    by=varnames]
  
  if (pvalue) {
    out_vimportance <- cbind(
      out_vimportance,
      vimportance_all[,
                      list(imp_pvalue = weighted.mean(pvalue, classif.bacc)), #Compute weighted mean of pvalue
                      by=varnames][, !'varnames']
    )
  }
  
  return(out_vimportance)
}

# extract_impperf_nestedrf -----
extract_impperf_nestedrf <- function(in_rflearner, in_task,
                                     imp = TRUE, perf = TRUE,
                                     pvalue = TRUE, pvalue_permutn = 35) {
  
  in_task <- in_task[[1]]
  in_rflearner <- in_rflearner[[1]]
  
  if (inherits(in_rflearner, "AutoTuner")) {
    sublrn <- in_rflearner$model$learner
  } else {
    sublrn <- in_rflearner
  }
  
  print(paste0("Computing variable importance for resampling instance hash #",
               sublrn$hash))
  
  outobj <- cbind(
    if (imp) {
      ####################### IF GraphLearner ####################################
      if (inherits(sublrn, "GraphLearner")) {
        
        if ('classif.ranger' %in% names(sublrn$model)) {
          
          if (pvalue == TRUE) {
            in_formula <- as.formula(paste0(in_task$target_names, '~.'))
            
            importance_pvalues(
              sublrn$model$classif.ranger$model,
              method = "altmann",
              num.permutations = pvalue_permutn,
              data = in_task$data(),
              formula= in_formula
            )
            
          } else {
            data.table(importance=sublrn$model$classif.ranger$model$variable.importance)
          }
        }
        
        else if ('classif.cforest' %in% names(sublrn$model)) {
          if (pvalue == TRUE) {
            warning("p_value calculation is only available for ranger classification rf, ignoring p_value.
                    In addition, default parameters were used in partykit::varimp, adjust as needed.")
          }
          data.table(importance=
                       partykit::varimp(sublrn$model$classif.cforest$model,
                                        nperm = 1,
                                        OOB = TRUE,
                                        risk = "misclassification",
                                        conditional = FALSE,
                                        threshold = .2))
          }
      } else { ####################### IF direct model ####################################
        if (pvalue == TRUE) { #If want pvalue associated with predictor variables
          in_formula <- as.formula(paste0(in_task$target_names, '~.'))
          
          importance_pvalues(
            in_rflearner$model,
            method = "altmann",
            num.permutations = pvalue_permutn,
            data = in_task$data(),
            formula= in_formula
          )
        } else { #If pvalue == FALSE
          data.table(importance= in_rflearner$model$learner$importance())
        }
        
      }
    },
    if (perf) {
      perf_id <- in_rflearner$instance_args$measure$id
      outperf <- in_rflearner$tuning_result[, get(perf_id)]
      data.table(outperf) %>% setnames(perf_id)
    }
  )
  
  return(outobj)
}

# dynamic_resamplebm ----
dynamic_resamplebm <- function(in_task, in_bm, in_lrnid, in_resampling, type,
                               inp_resdir = NULL, store_models = FALSE) {
  #If path, read qs
  if (inherits(in_bm, "character")) {
    in_bm <- qs::qread(file.path(inp_resdir, in_bm))
  }
  
  #get desired resampled_results/learner
  in_rf <- in_bm$filter(learner_ids = in_lrnid)
  
  rsmp_out <- dynamic_resample(in_task = in_task,
                               in_learner = in_rf,
                               in_resampling = in_resampling,
                               type = type,
                               store_models = store_models)
  
  return(rsmp_out)
}
# reset_tuning -----
reset_tuning <- function(in_autotuner, in_task, in_lrnid = NULL) {
  if (inherits(in_autotuner, 'list') & !is.null(in_lrnid)) {
    in_autotuner <- in_autotuner[[
      which(unlist(lapply(in_autotuner, function(lrn) {lrn$id == in_lrnid})))
      ]]
  }
  
  tuneargs_ini <- in_autotuner$instance_args
  
  autotuner_new <- set_tuning(in_learner = tuneargs_ini$learner,
                              in_measure = tuneargs_ini$measure,
                              nfeatures = length(in_task$feature_names),
                              insamp_nfolds= tuneargs_ini$resampling$param_set$values$folds,
                              insamp_neval= tuneargs_ini$terminator$param_set$values$n_evals,
                              insamp_nbatch= in_autotuner$tuner$param_set$values$batch_size
  )
  
  return(autotuner_new)
}

# analyze_benchmark -----
analyze_benchmark <- function(in_bm, in_measure, inp_resdir=NULL) {
  
  #If path, read qs
  if (inherits(in_bm, "character")) {
    in_bm <- qs::qread(file.path(inp_resdir, in_bm))
  }
  
  print(paste('It took',
              in_bm$aggregate(mlr3::msr('time_both'))$time_both,
              'seconds to train and predict with the',
              in_bm$aggregate(msr('time_both'))$learner_id,
              'model...'))
  
  bmdt <- as.data.table(in_bm)
  
  if (in_bm$task_type == 'regr') {
    print(in_bm$aggregate(in_measure$regr))
    boxcomp <- mlr3viz::autoplot(in_bm, measure = in_measure$regr)
    
    preds <- lapply(seq_len(bmdt[,.N]), function(rsmp_i) {
      preds <- bmdt$prediction[[rsmp_i]] %>%
        as.data.table %>%
        .[, `:=`(outf = bmdt$iteration[[rsmp_i]],
                 task = bmdt$task[[rsmp_i]]$id,
                 task_type = in_bm$task_type,
                 learner = bmdt$learner[[rsmp_i]]$id)]
      return(preds)
    }) %>%
      do.call(rbind, .)
    
    if (!('prob.1' %in% names(preds)) & 'response' %in% names(preds)) {
      preds[, prob.1 := response]
    }
  }
  
  
  if (in_bm$task_type == 'classif_st') {
    print(in_bm$aggregate(measures=in_measure$classif))
    boxcomp <- mlr3viz::autoplot(in_bm, measure = in_measure$classif)
    
    preds <- lapply(seq_len(bmdt[,.N]), function(rsmp_i) {
      preds <- data.table(outf = bmdt$iteration[[rsmp_i]],
                          task = bmdt$task[[rsmp_i]]$id,
                          learner = bmdt$learner[[rsmp_i]]$id,
                          task_type = in_bm$task_type,
                          pred = list(bmdt$prediction[[rsmp_i]]))
      return(preds)
    }) %>%
      do.call(rbind, .)
  }
  
  tasklearner_unique <- preds[, expand.grid(unique(task), unique(learner))] %>%
    `colnames<-`(c('task', 'learner')) %>%
    setDT
  
  tasklearner_unique[, learner_format := dplyr::case_when(
    learner == 'classif.ranger'~'default RF',
    learner == 'oversample.classif.ranger'~'default RF - oversampled',
    learner == 'classweights.classif.ranger'~'default RF - weighted classes',
    learner == 'classif.cforest'~'CIF',
    learner == 'oversample.classif.cforest'~'CIF - oversampled',
    learner == 'classweights.classif.cforest'~'CIF - weighted classes',
  )]
  
  glist <- lapply(1:nrow(tasklearner_unique), function(tsklrn) {
    print(tasklearner_unique[tsklrn,])
    subpred <- preds[task ==tasklearner_unique$task[tsklrn] &
                       learner == tasklearner_unique$learner[tsklrn],]
    
    ggmisclass_out <- ggmisclass_single(in_predictions = subpred)
    
    gout <- ggmisclass_out$plot +
      ggtitle(paste(tasklearner_unique$task[tsklrn],
                    tasklearner_unique$learner_format[tsklrn])) +
      labs(x='Threshold', y='Value')
    
    if (tsklrn < nrow(tasklearner_unique)) {
      gout <- gout +
        theme(legend.position = 'none')
    }
    
    return(list(plot = ggplotGrob(gout),
                interthres_dt = data.table(
                  learner = as.character(tasklearner_unique$learner[tsklrn]),
                  thresh = ggmisclass_out$interthresh
                )
    )
    )
  }) %>%
    unlist(recursive=F)
  
  
  return(list(
    bm_misclasscomp=do.call("grid.arrange",
                            list(grobs=glist[seq(1, length(glist), 2)])), #Get all plots out of the nested list
    bm_boxcomp = boxcomp,
    interthresh_dt = rbindlist(glist[seq(2, length(glist), 2)]) #Get all threshold data.table rows out of nested list
  ))
}

# get_outerrsmp ---------
get_outerrsmp <- function(in_rftuned, spatial_rsp=FALSE) {
  #Adapt whether return resample result or output from selecttrain_rf
  if (inherits(in_rftuned, 'list')) {
    #If there is more than one type of outer resampling
    if (length(in_rftuned$rf_outer$uhash) > 1) {
      #Check which resampling is spatial — only works if one is spatial
      sp_i <- which(unlist(lapply(in_rftuned$rf_outer, function(x) {
        grepl('.*Resampling.*Sp.*', x$resampling$format())
      })))
      
      #If user request that spatial resampling be used
      if (spatial_rsp==TRUE) {
        
        if (length(sp_i)>0) {
          rsmp_res <- in_rftuned$rf_outer[[min(sp_i)]]
        } else { #But if there is no spatial resampling provided
          stop("spatial_rsp==TRUE but the in_rftuned does not include
               any Spatial Resampling")
        }
        #If user didn't request spatial resampling to be used, grab the first
        #resampling that is not spatial
        } else {
          rsmp_res <- in_rftuned$rf_outer[[
            min((1:length(in_rftuned$rf_outer))[-sp_i])]]
        }
      
      #If there is only one type of outer resampling
    } else {
      print("Only one resampling result, ignoring spatial_rsp argument...")
      rsmp_res <- in_rftuned$rf_outer
    }
    #If in_rftuned is already a ResampleResult, simply return it
  } else if (inherits(in_rftuned, "ResampleResult")) {
    rsmp_res <- in_rftuned
  }
  return(rsmp_res)
}

# ggmisclass_single ---------
ggmisclass_single <- function(in_predictions=NULL, in_rftuned=NULL, spatial_rsp=FALSE) {
  #Get predicted probabilities of intermittency for each gauge
  # in_gaugestats[!is.na(cly_pc_cav), intermittent_predprob :=
  #                 as.data.table(in_predictions)[order(row_id), mean(prob.1), by=row_id]$V1]
  #Get misclassification error, sensitivity, and specificity for different classification thresholds
  #i.e. binary predictive assignment of gauges to either perennial or intermittent class
  
  #If provided resampling results rather than prediction table, extract 
  if (!is.null(in_rftuned)) {
    rsmp_res <- get_outerrsmp(in_rftuned, spatial_rsp=spatial_rsp)
    in_predictions <- rsmp_res$prediction()
  }
  
  #Get confusion matrices for range of thresholds (i.e., probability of flow intermittence
  #above which a watercourse is classified as non-perennial)
  threshold_confu_dt <- ldply(seq(0,1,0.01), threshold_misclass, in_predictions) %>%
    setDT
  
  #Get classification threshold at which sensitivity and specificity are the most similar
  balanced_thresh <- threshold_confu_dt[which.min(abs(spec-sens)),]
  print(paste('Sensitivity =', round(balanced_thresh$sens,2),
              'and Specificity =', round(balanced_thresh$spec,2),
              'at a classification threshold of', balanced_thresh$i))
  
  #Plot trends in confusion matrix metrics with increasing threshold
  gout <- ggplot(melt(threshold_confu_dt, id.vars='i'),
                 aes(x=i, y=value, color=variable, linetype=variable)) +
    geom_line(size=1.2) +
    geom_vline(xintercept=balanced_thresh$i, alpha=1/2) +
    geom_hline(yintercept=balanced_thresh$spec, alpha=1/2) +
    annotate('text', x=(balanced_thresh$i), y=0.4,
             label=balanced_thresh$i, angle=-90) +
    annotate('text', x=0.9, y=(balanced_thresh$spec),
             label=round(balanced_thresh$sens,2)) +
    scale_x_continuous(expand=c(0,0), name='Threshold') +
    scale_y_continuous(expand=c(0,0), name='Value') +
    scale_color_brewer(palette='Dark2',  #colorblind friendly
                       labels=c('Misclassification rate',
                                'Sensitivity (true positives)',
                                'Specificity (true negatives)')) +
    theme_bw()
  
  #Plot it
  return(list(plot = gout,
              interthresh = balanced_thresh$i))
}

# threshold_misclass ------
threshold_misclass <- function(i=0.5, in_preds) {
  #---- Get confusion matrix ----
  if (inherits(in_preds, 'PredictionClassif')) {
    confu <- as.data.table(in_preds$set_threshold(1-i)$confusion) #Get confusion matrix directly
  }
  
  if (is.data.table(in_preds)) {
    #If task associated with predictions is a classification and has records
    if (in_preds[task_type == 'classif_st',.N] > 0) {
      #For each CV repetition:
      #   1. set the probability threshold to compute a confusion matrix to 1-i
      #     (i being the threshold to classify something as 1, set_threshold
      #     being based on prob.0, not prob.1)
      #   2. Compute confusion matrix
      confu <- in_preds[, as.data.table(pred[[1]]$set_threshold(1-i)$confusion),
                        by=outf] %>%
        #Aggregate confusion matrices across repetitions
        .[, .(N=sum(N)), by=.(response, truth)]
    }
    
    #If task associated with predictions is a regression and has records
    if (in_preds[task_type == 'regr', .N] > 0) {
      #Reclassify continuous predictions into binary response across all records
      confu <- in_preds[, response := fifelse(prob.1>=i, '1', '0')] %>%
        .[, truth := as.character(truth)] %>%
        #Create aggregate confusion matrix
        .[, .N, by=.(response, truth)]
    }
  }
  
  #---- Compute statistics based on confusion matrix and format into data.table----
  outvec <- data.table(
    i,
    misclas = confu[truth != response, sum(N)] / confu[, sum(N)],
    sens = confu[truth == '1' & response == '1', N] / confu[truth=='1', sum(N)],
    spec  = confu[truth=='0' & response==0, N]/confu[truth=='0', sum(N)]
  )
  return(outvec)
}

# selecttrain_rf --------
selecttrain_rf <- function(in_rf, in_learnerid=NULL, in_task = NULL,
                           insamp_nfolds =  NULL, insamp_nevals = NULL) {
  
  outlist <- list()
  
  ######### Prepare autotuner for full training ####################
  # If a ResampleResult was provided
  if (inherits(in_rf, 'ResampleResult')) {
    in_bmsel <- in_rf$clone()
    iter_selected <- in_bmsel$score() %>% 
      as.data.table() %>%
      .[order(classif.ce)] %>%
      .[1,iteration]
    lrn_autotuner <- in_bmsel$learners[[iter_selected]]
    in_task <- in_bmsel$task
    outlist[['rf_outer']] <- in_rf
    
    # If a BenchmarkResult was provided
  } else if (inherits(in_rf, 'BenchmarkResult')) {
    in_bmsel <- in_rf$clone()$filter(learner_ids = in_learnerid,
                                     task_id = in_task)
    
    lrn_autotuner <- in_bmsel$clone()$learners$learner[[1]]
    in_task <-in_bmsel$tasks$task[[1]]
    
    #Return outer sampling object for selected model (or list of outer sampling objects)
    uhashes <- unique(as.data.table(in_bmsel)$uhash)
    if (length(uhashes) == 1) {
      outlist[['rf_outer']] <- in_bmsel$resample_result(uhash=uhashes)
    } else {
      outlist[['rf_outer']] <- lapply(uhashes, function(x) {
        in_bmsel$resample_result(uhash=x)
      })
    }
  } else if (inherits(in_rf, 'AutoTuner')) {
    lrn_autotuner <- in_rf
  }
  
  if (!is.null(insamp_nfolds)) {
    lrn_autotuner$instance_args$resampling$param_set$values$folds <- insamp_nfolds
  }
  
  if (!is.null(insamp_nevals)) {
    lrn_autotuner$instance_args$terminator$param_set$values$n_evals <- insamp_nevals
  }
  
  ######### Train it ####################
  # lrn_autotuner$model$learner$param_set$values = mlr3misc::insert_named(
  #   lrn_autotuner$model$learner$param_set$values,
  #   list(classif.ranger.importance = 'permutation')
  # )
  lrn_autotuner$train(in_task)
  
  outlist[['task']] <- in_task
  outlist[['rf_inner']] <- lrn_autotuner
  
  return(outlist)
}
# ggvimp ------------
ggvimp <- function(in_rftuned, in_predvars, varnum = 17, spatial_rsp=FALSE) {
  rsmp_res <- get_outerrsmp(in_rftuned, spatial_rsp=spatial_rsp)
  
  #Get variable importance and format them
  varimp_basic <- weighted_vimportance_nestedrf(rfresamp = rsmp_res,
                                                pvalue = FALSE) %>%
    merge(., in_predvars, by.x='varnames', by.y = "varname") %>%
    .[, `:=`(varnames = factor(varnames, varnames[order(-imp_wmean)]),
             Category = factor(Category,
                               levels = c('Climate', 'Hydrology', 'Landcover',
                                          'Physiography', 'Soils & Geology'))
    )] %>%
    setorder(-imp_wmean)
  varimp_basic$varnames <- c("drainage area", "slope", "Q_sd_12_s", "Q_s", "Q_min_p12_s", "P_to_PET_ratio",
                             "Natural vegetation", "runoff_dvar_s", "Q_mean_p3_s","gwr_to_runoff_ratio",
                             "Q_cv_12_s","Q_min_p3_s","Karst_extent_s","Q_mean_p12_s",
                             "Land cover",  "wet days", "karst status") %>% as.factor(.)
  outp <- varimp_basic[1:varnum] %>% 
    mutate(varnames = fct_reorder(varnames, desc(imp_wmean))) %>% 
    ggplot(. ,aes(x=varnames,
                  color =Category, fill=Category)) +
    geom_bar(aes(y=imp_wmean), stat = 'identity', alpha=0.7) +
    geom_errorbar(aes(ymin=imp_wmean-imp_wsd, ymax=imp_wmean+imp_wsd)) +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(tolower(x), width = 27)
    },
    limits=rev) +
    scale_fill_manual(values=c('#fdb462','#80b1d3','#b3de69','#bc80bd','#696868'),
                      drop=FALSE) +
    scale_color_manual(values=c('#fdb462','#80b1d3','#b3de69','#bc80bd','#696868'),
                       drop=FALSE) +
    theme_classic() +
    theme(axis.text.x = element_text(size=8),
          # axis.title.x  = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14),
          legend.position = c(0.80, 0.5)
    ) +
    scale_y_continuous(expand=c(0,0), position = 'left') +
    labs(y = "Variable importance (actual impurity reduction)")+
    coord_flip(ylim=c(0, max(varimp_basic[, max(imp_wmean+imp_wsd)+10], 100))) 
  #Plot 'em
  
  
  return(outp)
}

# ggpartialdep -----------
ggpartialdep <- function (in_rftuned, in_predvars, colnums, ngrid, nodupli=T,
                          nvariate = 2,
                          parallel=T, spatial_rsp=FALSE) {
  
  #Get outer resampling of interest
  rsmp_res <- get_outerrsmp(in_rftuned, spatial_rsp=spatial_rsp)
  
  #Get partial dependence across all folds and repeats
  nlearners <-with(rsmp_res$resampling$param_set$values, folds*repeats)
  datdf <- as.data.frame(rsmp_res$task$data()) #This may be shortened
  varimp <- weighted_vimportance_nestedrf(rsmp_res, pvalue=FALSE) %>%
    setorder(-imp_wmean)
  
  if (length(colnums) > nrow(varimp)) {
    colnums <- colnums[1:nrow(varimp)]
    print('colnums argument exceeded the number of variables,
          reduced it to ', nrow(varimp), ' variables')
  }
  
  if (nodupli) {
    selcols <- as.character(
      varimp$varnames[!duplicated(substr(varimp$varnames, 1,3))][colnums])
  } else {
    selcols <- as.character(
      varimp$varnames[colnums])
  }
  
  if (parallel) {
    print(paste("Computing partial dependence with future.apply across", nlearners,
                "CV folds"))
    pd <- future.apply::future_lapply(seq_len(nlearners),
                                      extract_pd_nestedrf,
                                      in_rftuned = rsmp_res,
                                      datdf = datdf,
                                      selcols = selcols,
                                      nvariate = nvariate,
                                      ngrid = ngrid,
                                      future.scheduling = structure(TRUE,ordering = "random"),
                                      future.packages = c("data.table","edarf","ranger"))
    
  } else {
    print(paste("Computing partial dependence iteratively across", nlearners,
                "CV folds"))
    pd <- lapply(seq_len(nlearners),
                 extract_pd_nestedrf,
                 in_rftuned = rsmp_res,
                 datdf = datdf,
                 selcols = selcols,
                 nvariate = nvariate,
                 ngrid = ngrid)
  }
  
  #Get weighted mean
  varvec <- paste0('var', 1:nvariate)
  valvec <- paste0('value', 1:nvariate)
  
  pdformat <- do.call(rbind, pd) %>%
    setDT %>%
    .[, list(mean1 = weighted.mean(`1`, classif.bacc)),
      by= c(varvec, valvec)] %>%
    .[, variables := var1] %>%
    merge(., in_predvars, by.x='var1', by.y='varname')
  
  datdf2 <- as.data.table(datdf)[, target_class := as.numeric(as.character(target_class))]
  
  if (nvariate ==1) {
    tileplots_l <- pdformat[,list(list(ggplotGrob(
      ggplot(.SD, aes(x=value1, y=mean1)) +
        geom_line() +
        geom_rug(data=datdf2,
                 aes_string(x=eval(var1),y='target_class'),
                 alpha=1/3) +
        scale_y_continuous(name='Partial dependence (probability of intermittency)',
                           limits= c(min(mean1)-0.01, max(mean1)+0.01),  #c(0.25, 0.425),
                           expand=c(0,0))+
        scale_x_continuous(name=stringr::str_wrap(eval(variables), width = 30)) +
        theme_classic() +
        theme(text = element_text(size=12),
              axis.title.y = element_blank())
    ))), by=.(var1)]
    
  } else if (nvariate == 2) {
    pdformat[, variables := paste(var1, var2)]
    
    vargrid <- t(combn(1:length(selcols), 2))
    #leglims <- pdformat[, c(min(mean1), max(mean1))]
    
    #Iterate over every pair of variables
    
    tileplots_l <- pdformat[,list(list(ggplotGrob(
      ggplot(.SD, aes(x=value1, y=value2)) +
        geom_tile(aes(fill = mean1)) +
        scale_fill_distiller(palette='Grey') +
        geom_jitter(data=datdf,
                    aes_string(color='intermittent_o1800', x=eval(var1),y=eval(var2)),
                    alpha=1/3) +
        scale_color_manual(values=c('#0F9FD6','#ff9b52')) +
        labs(x=stringr::str_wrap(in_predvars[varcode==eval(var1), varname],
                                 width = 20),
             y=stringr::str_wrap(in_predvars[varcode==eval(var2), varname],
                                 width = 20)) +
        theme_bw() +
        theme(text = element_text(size=12))
    )))
    , by=.(var1, var2)]
  }
  
  pagelayout <-   lapply(1:(nrow(tileplots_l) %/% 9), function(p_i) {
    (p_i-1)*9+(1:9)
  })
  if (nrow(tileplots_l) %% 9 > 0) {
    pagelayout[[nrow(tileplots_l) %/% 9 + 1]] <-
      (nrow(tileplots_l) %/% 9)*9+(1:(nrow(tileplots_l) %% 9))
  }
  
  
  tileplots_multipl <- lapply(pagelayout, function(page) {
    print(page)
    return(do.call("grid.arrange",list(
      grobs=(tileplots_l[page,V1]),
      left = 'Partial dependence (probability of intermittency)')))
  })
  return(tileplots_multipl)
  }

# extract_pd_nestedrf ------------
extract_pd_nestedrf <- function(learner_id=1, in_rftuned, datdf,
                                selcols, nvariate, ngrid) {
  in_mod <- as.data.table(in_rftuned)[eval(learner_id),] #Go through data.table format to have access to both tasks and learners
  
  #Get fold-specific performance measure
  foldperf <- extract_impperf_nestedrf(in_rflearner = in_mod$learner,
                                       in_task = in_mod$task,
                                       imp=F, perf=T, pvalue=F)
  
  # selcols <- in_vimp_plot$data %>% #Can use that if extracting from tunredrf is expensive
  #   setorder(-imp_wmean) %>%
  #   .[colnums, variable]
  
  
  if (inherits(in_mod$learner[[1]]$learner, "GraphLearner")) {
    in_fit <- in_mod$learner[[1]]$learner$model$classif.ranger$model
  } else {
    in_fit <- in_mod$learner[[1]]$learner$model
  }
  
  ngridvec <- c(ngrid, ngrid)
  
  #Make dataset of all combinations of selected column names, two at a time
  if (nvariate == 1) {
    pdcomb <- lapply(selcols, function(i) {
      print(i)
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i),
                                         n = ngridvec, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i)] %>%
        setnames(i, 'value1')
    }
    ) %>%
      do.call(rbind, .)
    
  } else if (nvariate == 2) {
    vargrid <- combn(selcols, 2, simplify=F) %>%
      do.call(rbind, .)
    
    #Get marginal distribution of the effect of two columns at a time
    pdcomb <- mapply(function(i, j) {
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i, j),
                                         n = ngridvec,
                                         interaction = TRUE, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i, var2=j)] %>%
        setnames(c(i,j), c('value1', 'value2'))
      
      
      return(pdout)
    }, vargrid[,1], vargrid[,2], SIMPLIFY = FALSE) %>%
      do.call(rbind, .)
  } else {
    print('Warning: function cannot yet work with more than two variables at a time')
  }
  
  return(pdcomb)
}
# get_outerrsmp ---------
get_outerrsmp <- function(in_rftuned, spatial_rsp=FALSE) {
  #Adapt whether return resample result or output from selecttrain_rf
  if (inherits(in_rftuned, 'list')) {
    #If there is more than one type of outer resampling
    if (length(in_rftuned$rf_outer$uhash) > 1) {
      #Check which resampling is spatial — only works if one is spatial
      sp_i <- which(unlist(lapply(in_rftuned$rf_outer, function(x) {
        grepl('.*Resampling.*Sp.*', x$resampling$format())
      })))
      
      #If user request that spatial resampling be used
      if (spatial_rsp==TRUE) {
        
        if (length(sp_i)>0) {
          rsmp_res <- in_rftuned$rf_outer[[min(sp_i)]]
        } else { #But if there is no spatial resampling provided
          stop("spatial_rsp==TRUE but the in_rftuned does not include
               any Spatial Resampling")
        }
        #If user didn't request spatial resampling to be used, grab the first
        #resampling that is not spatial
        } else {
          rsmp_res <- in_rftuned$rf_outer[[
            min((1:length(in_rftuned$rf_outer))[-sp_i])]]
        }
      
      #If there is only one type of outer resampling
    } else {
      print("Only one resampling result, ignoring spatial_rsp argument...")
      rsmp_res <- in_rftuned$rf_outer
    }
    #If in_rftuned is already a ResampleResult, simply return it
  } else if (inherits(in_rftuned, "ResampleResult")) {
    rsmp_res <- in_rftuned
  }
  return(rsmp_res)
}

# ggvimp ------------
ggvimp <- function(in_rftuned, in_predvars, varnum = 17, spatial_rsp=FALSE) {
  rsmp_res <- get_outerrsmp(in_rftuned, spatial_rsp=spatial_rsp)
  
  #Get variable importance and format them
  varimp_basic <- weighted_vimportance_nestedrf(rfresamp = rsmp_res,
                                                pvalue = FALSE) %>%
    merge(., in_predvars, by.x='varnames', by.y = "varname") %>%
    .[, `:=`(varnames = factor(varnames, varnames[order(-imp_wmean)]),
             Category = factor(Category,
                               levels = c('Climate', 'Hydrology', 'Landcover',
                                          'Physiography', 'Soils & Geology'))
    )] %>%
    setorder(-imp_wmean)
  
  varimp_basic$varnames <- c("drainage_area", "slope", "Q_iav_sd", "Q",  "Q_min_p12",
                             "P_to_PET_ratio","pot_nat_vegetation", "runoff_dvar",
                             "Q_mean_p3",  "gwr_to_runoff_ratio", "Q_iav_cv",
                             "Q_min_p3", "Karst_fraction", "Q_mean_p12",
                             "land_cover", "wet_days",  "karst_status") %>% as.factor(.)
  
  #Plot 'em
  outp <- varimp_basic[1:varnum] %>% 
    mutate(varnames = fct_reorder(varnames, desc(imp_wmean))) %>% 
    ggplot(.,aes(x=varnames, color =Category, fill=Category)) +
    geom_bar(aes(y=imp_wmean), stat = 'identity', alpha=0.7) +
    geom_errorbar(aes(ymin=imp_wmean-imp_wsd, ymax=imp_wmean+imp_wsd)) +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(x, width = 27)
    },
    limits=rev) +
    scale_fill_manual(values=c('#fdb462','#80b1d3','#b3de69','#bc80bd','#696868'),
                      drop=FALSE) +
    scale_color_manual(values=c('#fdb462','#80b1d3','#b3de69','#bc80bd','#696868'),
                       drop=FALSE) +
    theme_classic() +
    theme(axis.text.x = element_text(size=8),
          # axis.title.x  = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14),
          legend.position = c(0.80, 0.5)
    ) +
    scale_y_continuous(expand=c(0,0), position = 'left') +
    labs(y = "Variable importance (actual impurity reduction)")+
    coord_flip(ylim=c(0, max(varimp_basic[, max(imp_wmean+imp_wsd)+10], 100))) 
  
  return(outp)
}

# ggpartialdep ---------- 
ggpartialdep <- function (in_rftuned, in_predvars, colnums, ngrid, nodupli=T,
                          nvariate = 2,
                          parallel=T, spatial_rsp=FALSE) {
  
  #Get outer resampling of interest
  rsmp_res <- get_outerrsmp(in_rftuned, spatial_rsp=spatial_rsp)
  
  #Get partial dependence across all folds and repeats
  nlearners <-with(rsmp_res$resampling$param_set$values, folds*repeats)
  datdf <- as.data.frame(rsmp_res$task$data()) #This may be shortened
  varimp <- weighted_vimportance_nestedrf(rsmp_res, pvalue=FALSE) %>%
    setorder(-imp_wmean)
  
  if (length(colnums) > nrow(varimp)) {
    colnums <- colnums[1:nrow(varimp)]
    cat('colnums argument exceeded the number of variables,
        reduced it to ', nrow(varimp), ' variables')
  }
  
  if (nodupli) {
    selcols <- as.character(
      varimp$varnames[!duplicated(substr(varimp$varnames, 1,3))][colnums])
  } else {
    selcols <- as.character(
      varimp$varnames[colnums])
  }
  
  if (parallel) {
    print(paste("Computing partial dependence with future.apply across", nlearners,
                "CV folds"))
    pd <- future.apply::future_lapply(seq_len(nlearners),
                                      extract_pd_nestedrf,
                                      in_rftuned = rsmp_res,
                                      datdf = datdf,
                                      selcols = selcols,
                                      nvariate = nvariate,
                                      ngrid = ngrid,
                                      future.scheduling = structure(TRUE,ordering = "random"),
                                      future.packages = c("data.table","edarf","ranger"))
    
  } else {
    print(paste("Computing partial dependence iteratively across", nlearners,
                "CV folds"))
    pd <- lapply(seq_len(nlearners),
                 extract_pd_nestedrf,
                 in_rftuned = rsmp_res,
                 datdf = datdf,
                 selcols = selcols,
                 nvariate = nvariate,
                 ngrid = ngrid)
  }
  
  #Get weighted mean
  varvec <- paste0('var', 1:nvariate)
  valvec <- paste0('value', 1:nvariate)
  
  pdformat <- do.call(rbind, pd) %>%
    setDT %>%
    .[, list(mean1 = weighted.mean(`1`, classif.bacc)),
      by= c(varvec, valvec)] %>%
    .[, variables := var1] %>%
    merge(., in_predvars, by.x='var1', by.y='varname')
  
  datdf2 <- as.data.table(datdf)[, target_class := as.numeric(as.character(target_class))]
  
  if (nvariate ==1) {
    tileplots_l <- pdformat[,list(list(ggplotGrob(
      ggplot(.SD, aes(x=value1, y=mean1)) +
        geom_line() +
        geom_rug(data=datdf2,
                 aes_string(x=eval(var1),y='target_class'),
                 alpha=1/3) +
        scale_y_continuous(name='Partial dependence (probability of intermittency)',
                           limits= c(min(mean1)-0.01, max(mean1)+0.01),  #c(0.25, 0.425),
                           expand=c(0,0))+
        scale_x_continuous(name=stringr::str_wrap(eval(variables), width = 30)) +
        theme_classic() +
        theme(text = element_text(size=12),
              axis.title.y = element_blank())
    ))), by=.(var1)]
    
  } else if (nvariate == 2) {
    pdformat[, variables := paste(var1, var2)]
    
    vargrid <- t(combn(1:length(selcols), 2))
    #leglims <- pdformat[, c(min(mean1), max(mean1))]
    
    #Iterate over every pair of variables
    
    tileplots_l <- pdformat[,list(list(ggplotGrob(
      ggplot(.SD, aes(x=value1, y=value2)) +
        geom_tile(aes(fill = mean1)) +
        scale_fill_distiller(palette='Grey') +
        geom_jitter(data=datdf,
                    aes_string(color='intermittent_o1800', x=eval(var1),y=eval(var2)),
                    alpha=1/3) +
        scale_color_manual(values=c('#0F9FD6','#ff9b52')) +
        labs(x=stringr::str_wrap(in_predvars[varcode==eval(var1), varname],
                                 width = 20),
             y=stringr::str_wrap(in_predvars[varcode==eval(var2), varname],
                                 width = 20)) +
        theme_bw() +
        theme(text = element_text(size=12))
    )))
    , by=.(var1, var2)]
  }
  
  pagelayout <-   lapply(1:(nrow(tileplots_l) %/% 9), function(p_i) {
    (p_i-1)*9+(1:9)
  })
  if (nrow(tileplots_l) %% 9 > 0) {
    pagelayout[[nrow(tileplots_l) %/% 9 + 1]] <-
      (nrow(tileplots_l) %/% 9)*9+(1:(nrow(tileplots_l) %% 9))
  }
  
  
  tileplots_multipl <- lapply(pagelayout, function(page) {
    print(page)
    return(do.call("grid.arrange",list(
      grobs=(tileplots_l[page,V1]),
      left = 'Partial dependence (probability of intermittency)')))
  })
  return(tileplots_multipl)
  }

# extract_pd_nestedrf -----------
extract_pd_nestedrf <- function(learner_id=1, in_rftuned, datdf,
                                selcols, nvariate, ngrid) {
  in_mod <- as.data.table(in_rftuned)[eval(learner_id),] #Go through data.table format to have access to both tasks and learners
  
  #Get fold-specific performance measure
  foldperf <- extract_impperf_nestedrf(in_rflearner = in_mod$learner,
                                       in_task = in_mod$task,
                                       imp=F, perf=T, pvalue=F)
  
  # selcols <- in_vimp_plot$data %>% #Can use that if extracting from tunredrf is expensive
  #   setorder(-imp_wmean) %>%
  #   .[colnums, variable]
  
  
  if (inherits(in_mod$learner[[1]]$learner, "GraphLearner")) {
    in_fit <- in_mod$learner[[1]]$learner$model$classif.ranger$model
  } else {
    in_fit <- in_mod$learner[[1]]$learner$model
  }
  
  ngridvec <- c(ngrid, ngrid)
  
  #Make dataset of all combinations of selected column names, two at a time
  if (nvariate == 1) {
    pdcomb <- lapply(selcols, function(i) {
      print(i)
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i),
                                         n = ngridvec, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i)] %>%
        setnames(i, 'value1')
    }
    ) %>%
      do.call(rbind, .)
    
  } else if (nvariate == 2) {
    vargrid <- combn(selcols, 2, simplify=F) %>%
      do.call(rbind, .)
    
    #Get marginal distribution of the effect of two columns at a time
    pdcomb <- mapply(function(i, j) {
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i, j),
                                         n = ngridvec,
                                         interaction = TRUE, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i, var2=j)] %>%
        setnames(c(i,j), c('value1', 'value2'))
      
      
      return(pdout)
    }, vargrid[,1], vargrid[,2], SIMPLIFY = FALSE) %>%
      do.call(rbind, .)
  } else {
    print('Warning: function cannot yet work with more than two variables at a time')
  }
  
  return(pdcomb)
}

predname_df <- function(task, feat_name_vec = NULL, category = NULL){
  
  if (inherits(task, "Task")) {
    feat_name <- in_task$feature_names
  } else
    feat_name <- feat_name_vec
  
  if (is.null(category)) {
    cat <- c("Hydrology", "Climate", "Hydrology",  "Physiography", "Landcover", "Landcover",
             "Soils & Geology", "Soils & Geology", "Hydrology", "Hydrology","Hydrology",
             "Hydrology", "Landcover", "Hydrology", "Hydrology","Hydrology",
             "Physiography", "Climate")
  } else
    cat <- category
  
  predvars <- cbind(feat_name, cat) %>%
    `colnames<-`(c("varname", "Category"))
  
  return(predvars)
  
}

# selected_learner_fun ----------
selected_learner_fun <- function(in_bm){
  learner_selected <- in_bmsel$score() %>% 
    as.data.table() %>%
    .[order(classif.ce)] %>%
    .[1,learner_id]  
  
  return(learner_selected)
}


# ----------------------- Functions for creating plots for step one ----------------------

pred_prob <- function(in_rftuned, val_task = NULL, data_type = "validation"){
  
  if (data_type == "validation") {
    if (!is.null(val_task)) {
      pred <- in_rftuned$rf_inner$predict(val_task)
    } 
    else
      stop("please provide the task for validation data.\n")
    
  } else {
    # best_cv <- in_rftuned$rf_inner$archive$benchmark_result$aggregate() %>%
    #   as.data.table() %>% 
    #   setorder(., classif.ce) %>% 
    #   .[1, nr]
    pred <-
      # lapply(seq_along(in_rftuned$rf_inner$archive$predictions(best_cv)), 
      #                        function(x) in_rftuned$rf_inner$archive$predictions(best_cv)[[x]] %>%
      #                          as.data.table()) %>% 
      in_rftuned$rf_outer$prediction() %>% as.data.table() %>% 
      # do.call(rbind, .) %>% 
      distinct(row_ids, .keep_all = TRUE) %>%
      setorder(row_ids)
    
  }
  
  return(pred)
  
}


create_df <- function(data, in_pred, dd_id = NULL, date = NULL,...){
  
  if (is.null(dd_id)) {
    df <- cbind(data[, c("dd_id", "date", "drainage_area")], as.data.table(in_pred))
  } else {
    if (!is.null(dd_id) & !is.null(date)){
      df <- cbind( dd_id, date, drainage_area = data[,"drainage_area"], as.data.table(in_pred))
    } else
      stop("please provide dd_id and date data.")
    
  }
  
  return(df)
}  
compute_ratio <- function(df){
  
  
  ratio_df <- df %>% as_tibble(.) %>% 
    group_by(dd_id) %>% 
    dplyr::summarize(count = n(), upa = mean(drainage_area),
                     no_flow_obs = sum(truth == 1),
                     no_flow_pre = sum(response == 1),
                     ratio_no_flow_obs = (no_flow_obs / count) * 100,
                     ratio_no_flow_pre = (no_flow_pre / count) * 100,
                     ratio = ratio_no_flow_pre / ratio_no_flow_obs)
  
  
  return(ratio_df)
}

perc_intermit_fig <- function(model_tuned = rftuned,
                              val_task = NULL,
                              in_data,
                              stations_shp,
                              worldmap_shp,
                              data_type = "calibration"){
  if (model_step == "one") {
    
    in_pred <- pred_prob(in_rftuned = model_tuned,
                         val_task = val_task,
                         data_type = data_type)
    
    df <- create_df(data = in_data,
                    in_pred = in_pred,
                    data_type = data_type)
    
    ratio_df <- compute_ratio(df)
    
    upa <- df %>% as_tibble(.) %>% 
      group_by(dd_id) %>% 
      distinct(drainage_area) %>%
      `colnames<-`(c("dd_id", "upa"))
    
    new_df <- merge(ratio_df, upa, by.x = "dd_id", by.y = "dd_id") %>% as_tibble() %>% 
      select(-upa.y) %>% 
      dplyr::rename(., upa = upa.x)
  }
  
  
  merge_st <- merge(new_df, stations_shp, by = "dd_id") %>% 
    sf::st_as_sf()
  
  ratio_no_flow_obs_classes <- merge_st %>% pull(ratio_no_flow_obs) %>%
    cut(breaks = c(-Inf, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
        labels = c("0", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70",
                   "70-80", "80-90", "90-100", "> 100"))
  
  jet <- c("#CCCCCC", "#004DFF", "#007FFF", "#00B2FF", "#FFE500",
           "#FFB300", "#FF7F00", "#FF4C00", "#FF1900", "#E50000", 
           "#B20000")
  
  p_no_flow_obs <- merge_st %>% mutate(ratio_no_flow_obs_classes) %>% 
    ggplot() +
    geom_sf(data = worldmap_shp,  fill = "white") +
    geom_sf(aes(color = ratio_no_flow_obs_classes))+
    coord_sf(xlim = c(-20, 45), ylim = c(35, 73))+
    theme_bw()+ labs(x = "longitude", y = "latitude")+
    scale_color_manual(values = jet, name = "% no-flow observation")+
    theme(legend.title = element_text(colour = "black",
                                      family = "Times New Roman",
                                      size =  14, 
                                      face = "bold"),
          axis.title = element_text(colour = "blue", 
                                    family = "Times New Roman",
                                    size =  14, 
                                    face = "bold"),
          legend.position = c(0.40, 0.1),
          legend.text = element_text(size = 10,
                                     family = "Times New Roman",
                                     face = "bold"),
          legend.direction = "horizontal",
          legend.justification = "left") +
    # annotate(geom="text", x=-15, y=70, label="b",
    #          family = "Times New Roman", size = 12) +
    guides(color = guide_legend(title.position = "top", 
                                title.hjust = 0.5)) 
  
  number_each_ratioclass <- merge_st %>%
    mutate(ratio_no_flow_obs_classes) %>% 
    group_by(ratio_no_flow_obs_classes) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::summarise(count = n())
  
  # plot of ratio ---
  ratio_classes <- merge_st  %>% pull(ratio) %>% 
    cut(breaks = c(-Inf, 0, 0.5, 0.7, 0.9, 1.1, 1.3, 2, 100, Inf),
        labels = c("0", "0-0.5", "0.5-0.7", "0.7-0.9", "0.9-1.1",
                   "1.1-1.3", "1.3-2", " > 2",
                   "no-flow predicted, but not observed"))
  color_vec <- c("#3288BD", "#66C2A5", "#ABDDA4","#E6F598","#FFFF00",
                 "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F")
  p_ratio <- merge_st %>% mutate(ratio_classes) %>% 
    ggplot() +
    geom_sf(data = worldmap, fill = "white") +
    geom_sf(aes(color = ratio_classes))+
    coord_sf(xlim = c(-20, 45), ylim = c(35, 73))+
    theme_bw()+ labs(x = "longitude", y = "latitude")+
    scale_color_manual(values = color_vec, na.value = "grey90",
                       name = "Pre no-flow months / Obs no-flow months",
                       labels = c("0", "0-0.5", "0.5-0.7", "0.7-0.9",
                                  "0.9-1.1", "1.1-1.3", "1.3-2", " > 2",
                                  "no-flow predicted, but not observed",
                                  "perennial observed and predicted")) +
    theme(legend.title = element_text(colour = "black",
                                      family = "Times New Roman",
                                      size =  14, 
                                      face = "bold"),
          axis.title = element_text(colour = "blue",
                                    family = "Times New Roman",
                                    size =  12, 
                                    face = "bold"),
          legend.position = c(0.32, 0.07),
          legend.text = element_text(size = 10,
                                     family = "Times New Roman",
                                     face = "bold"),
          legend.direction = "horizontal",
          legend.justification = "left")+
    # annotate(geom="text", x=-15, y=70, label="d",
    #          family = "Times New Roman", size = 12) +
    guides(color = guide_legend(title.position = "top", 
                                title.hjust = 0.5)) 
  
  
  out <- list(plot_noflow_obs = p_no_flow_obs,
              plot_ratio = p_ratio,
              number_each_ratioclass = number_each_ratioclass,
              ratio_df = new_df, 
              in_pred = as.data.table (in_pred))
  
  return(out)
  
}
differ_ratio_calibvalid_fig <- function(calib_data, valid_data, 
                                        stations_shp, worldmap_shp){
  
  calib_ratio <- 
    create_task(in_tbl = subset(calib_data, select = -date),
                target_name = "target",
                breaks = c(-Inf, 0, 31), 
                labels = c("0", "1"),
                problem_type = "classif",
                step = "first") %>% 
    as.data.table() %>% 
    dplyr::group_by(dd_id) %>% 
    dplyr::summarise(count = n(),
                     perennial_cal = sum(target_class == 0)/count *100,
                     intermittent_cal = sum(target_class == 1)/count *100)
  valid_ratio <- 
    create_task(in_tbl = subset(valid_data, select = -date),
                target_name = "target",
                breaks = c(-Inf, 0, 31), 
                labels = c("0", "1"),
                problem_type = "classif",
                step = "first") %>% 
    as.data.table() %>% 
    dplyr::group_by(dd_id) %>% 
    dplyr::summarise(count = n(),
                     perennial_val = sum(target_class == 0)/count *100,
                     intermittent_val = sum(target_class == 1)/count *100)
  
  diff_datasets <- calib_ratio %>%
    dplyr::right_join(., valid_ratio, by = "dd_id") %>%
    mutate(difference = intermittent_cal - intermittent_val) %>% 
    mutate(new_diff = ifelse(difference == 0, NA, difference))
  
  diff_classes <- diff_datasets %>% dplyr::pull(new_diff) %>% 
    cut(breaks = c(-100, -80, -60, -40, -20, -10, 0, 10, 20, 40, 60, 80, 100),
        labels = c("< -80", "(-80,-60]", "(-60,-40]", "(-40,-20]", "(-20,-10]",
                   "(-10,0)", "(0,10]", "(10,20]", "(20,40]", "(40,60]",
                   "(60, 80]", "> 80"))
  
  jet2 <- c("#CC0033", "#FF3366", "#FF6666", "#FF9999",
            "#FFCC99", "#FFFF33", "#00FF00", "#99FFFF", "#33CCFF", 
            "#3399CC", "#3366CC", "#0000FF")
  
  merge_st <- merge(diff_datasets, stations_shp, by = "dd_id") %>% 
    sf::st_as_sf()
  
  p <- merge_st %>% mutate(diff_classes) %>%
    ggplot() +
    geom_sf(data = worldmap_shp, fill = "white") +
    geom_sf(aes(color = diff_classes))+
    coord_sf(xlim = c(-20, 45), ylim = c(35, 73))+
    theme_bw()+ labs(x = "longitude", y = "latitude")+
    scale_color_manual(values = jet2, na.value = "grey90",
                       labels = c("< -80", "(-80,-60]", "(-60,-40]", "(-40,-20]",
                                  "(-20,-10]", "(-10,0)", "(0,10]", "(10,20]",
                                  "(20,40]", "(40,60]", "(60, 80]", "> 80",
                                  "no difference"))+
    theme(legend.title = element_text(colour = "black",
                                      family = "Times New Roman",
                                      size =  14, 
                                      face = "bold"),
          axis.title = element_text(colour = "blue",
                                    family = "Times New Roman",
                                    size =  12,
                                    face = "bold"),
          legend.position = c(0.32, 0.07),
          legend.text = element_text(size = 10,
                                     family = "Times New Roman",
                                     face = "bold"),
          legend.direction = "horizontal",
          legend.justification = "left")+
    guides(color = guide_legend(title.position = "top", 
                                title.hjust = 0.5,
                                title = "% calib.intermittent - % valid.intermittent"))
  
  number_each_ratioclass <- merge_st %>% mutate(diff_classes) %>% 
    group_by(diff_classes) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::summarise(count = n())
  
  out <- list(p = p,
              number_each_ratioclass = number_each_ratioclass)
  
  return(out)
  
}

scatter_fig_calval <- function(cal_df, val_df){
  nse_cal <- ie2misc::vnse(cal_df$ratio_no_flow_pre, 
                           cal_df$ratio_no_flow_obs, na.rm = FALSE)
  nse_val <- ie2misc::vnse(val_df$ratio_no_flow_pre,
                           val_df$ratio_no_flow_obs, na.rm = FALSE)
  nse_df <- data.frame(dataset = c("Calibratio", "validation"),
                       NSE = round(c(nse_cal, nse_val), digits = 2))
  p_out <- ggplot()+
    geom_point(aes(x = ratio_no_flow_obs, y = ratio_no_flow_pre),
               data = cal_df)+
    geom_point(aes(x = ratio_no_flow_obs, y = ratio_no_flow_pre),
               data = val_df, color = "red", alpha = 0.4)+
    geom_abline(intercept = 0, slope = 1, size = 0.5)+
    annotate(geom="text", x=75, y=15, label="Underestimated",
             family = "Times New Roman", size = 6)+
    annotate(geom="text", x=10, y=85, label="Overestimated",
             family = "Times New Roman",  size = 6)+  
    theme_bw()+
    labs(x = "Percentage of no-flow observed",
         y = "Percentage of no-flow predicted") +
    theme(axis.title = element_text(colour = "black",
                                    family = "Times New Roman",
                                    size =  12, 
                                    face = "bold"),
          axis.text = element_text(colour = "black",
                                   family = "Times New Roman",
                                   size =  10, 
                                   face = "bold"))+
    annotate(geom = "table", x = 100, y = 55, label = list(nse_df),
             family = "Times New Roman",  size = 4)
  
  return(p_out)
}

boxplot_fig_calval <- function(cal_df, val_df){
  
  ls <- list(cal_df,
             val_df)
  label_names <- c("calibration", "validation")
  label_dataset <- list(cal = c("(0-2] \n(256/464)", "(2-5] \n(482/844)",
                                "(5-10] \n(435/661)", "(10-50] \n(2853/3838)",
                                "(50-500] \n(6695/8860)",
                                "(500-2,500] \n(2299/2849)", "(2500-10,000] \n(600/781)", "> 10,000 \n(553/857)"),
                        val = c("(0-2] \n(125/237)", "(2-5] \n(243/429)",
                                "(5-10] \n(267/347)", "(10-50] \n(1437/2132)",
                                "(50-500] \n(3659/4902)",
                                "(500-2,500] \n(1046/1416)", "(2500-10,000] \n(229/412)", "> 10,000 \n(309/459)"))
  p <- lapply(seq_along(ls), function(i){
    ls[[i]] %>% 
      filter(no_flow_obs > 0 | no_flow_pre > 0) %>% 
      mutate(bins = cut(upa,
                        breaks = c(-Inf, 2, 5, 10, 50, 
                                   500, 2500, 10000, Inf),
                        labels = label_dataset[[i]])) %>%
      dplyr::select(c(bins, ratio_no_flow_obs, ratio_no_flow_pre)) %>%
      reshape2::melt(., id.vars = c("bins")) %>%
      ggplot(aes(bins, y = value, fill = variable))+
      geom_boxplot() +
      theme_bw()+
      scale_fill_discrete(name = "datasets", labels = c("no-flow observed",
                                                        "no-flow predicted"))+
      labs(x = "Upstream area of streamflow gauging stations",
           y = "Ratio")+
      theme(legend.title = element_text(colour = "black",
                                        family = "Times New Roman",
                                        size =  14, 
                                        face = "bold"),
            legend.text =  element_text(colour = "black",
                                        family = "Times New Roman",
                                        size =  11, 
                                        face = "bold"),
            legend.position = "bottom",
            axis.title = element_text(colour = "black",
                                      family = "Times New Roman",
                                      size =  12,
                                      face = "bold"),
            axis.text = element_text(colour = "black",
                                     family = "Times New Roman",
                                     size =  10, 
                                     face = "bold")
      ) +
      annotate(geom = "text", x = 1.5, y = 95, label = label_names[i],
               family = "Times New Roman",  size = 5, fontface = "bold")
  })
  
  prow <- plot_grid( p[[1]] + theme(legend.position="none"),
                     p[[2]] + theme(legend.position="none"),
                     align = 'vh',
                     hjust = 1,
                     nrow = 1)
  legend_b <- get_legend(p[[1]] + theme(legend.position="bottom"))
  p_out <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
  
  return(p_out)
}

