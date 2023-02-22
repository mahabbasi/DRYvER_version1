
read_qsfiles <-  function(files_path){
  
  files_name <- basename(files_path) %>% 
    gsub(".qs","",.)
  
  ls <- lapply(seq_along(files_path), function(x, path){
    path[x] %>% qs::qread(.)
    
  }, path = files_path)
  
  names(ls) <- files_name
  
  return(ls)
}

create_tbl2 <- function(in_data, target_name, breaks, labels, remove_cols, spatial = FALSE){
  
  in_tbl <- in_data %>% 
    as.data.table() %>% 
    dplyr::filter(target > 0)
  
  target_class <- classify_target(in_tbl,
                                  target_col = target_name,
                                  breaks = breaks, 
                                  labels = labels)
  
  if (spatial) {
    
    remove_cols <- remove_cols[! remove_cols %in% c('X', 'Y')]
    
    df <- in_tbl %>%
      dplyr::select(-target_name) %>% 
      dplyr::mutate(target_class) %>%
      `rownames<-`(NULL) %>% 
      dplyr::select(-remove_cols)
    
    out <- sf::st_as_sf(
      df,
      # "coords" is in x/y order -- so longitude goes first!
      coords = c("X", "Y"),
      # Set our coordinate reference system to EPSG:4326,
      # the standard WGS84 geodetic coordinate reference system
      crs = 4326
    )
    
  } else {
    
    out <- in_tbl %>%
      dplyr::select(-target_name) %>% 
      dplyr::mutate(target_class) %>%
      `rownames<-`(NULL) %>% 
      dplyr::select(-remove_cols)
    
  }
  return(out)
}

set_model <- function(ncore = 10, mode = "classification"){
  
  rf_model <- 
    rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
    set_engine("ranger", num.threads = ncore,
               importance = "impurity_corrected", verbose = 1) %>% 
    set_mode(mode)
  
}

set_recipe <- function(in_data, over_ratio){
  
  in_data %>% 
    recipes::recipe(target_class ~., data = .) %>% 
    step_smotenc(target_class, over_ratio = over_ratio) %>% 
    prep() %>% 
    bake(new_data = NULL) %>% 
    recipes::recipe(target_class~., data = .)
}

set_workflow <- function( model_setup, model_recipe){
  
  options(tidymodels.dark = TRUE)
  
  workflow() %>% 
    add_recipe(model_recipe) %>% 
    add_model(model_setup)
}

set_resampling <- function(in_data, type = "cv", folds = 5, rep_num = 1, times = 20, spatial = FALSE){
  
  if(spatial) {
    
    resam <- spatial_clustering_cv(in_data, 
                                   v = folds,
                                   repeats = rep_num,
                                   cluster_function = "hclust")
  }
  
  if (type == "cv") {
    resam <- rsample::vfold_cv(in_data,
                               v = folds,
                               repeats = rep_num)
  }
  
  if (type == "nested_cv") {
    resam <- nested_cv(in_data, 
                       outside = vfold_cv(repeats = rep_num), 
                       inside = bootstraps(times = times))
  }
  
  return(resam)
}

tune_model <- function(model_workflow, resamples, grid_iter) {
  
  model_workflow %>% 
    tune_grid(.,
              resamples = resamples,
              grid = grid_iter,
              control = control_grid(save_pred = TRUE, verbose = TRUE),
              metrics = metric_set(bal_accuracy))
  
}

select_best_model <- function(in_model, metric = "bal_accuracy",
                              limit_perc = 10, hparam_name = "min_n", ...){
  in_model %>% 
    tune::select_by_pct_loss(.,
                             metric = "bal_accuracy",
                             limit = limit_perc,
                             get(hparam_name))
}

fit_final_model <- function(in_data, model_workflow, best_model){
  
  model_workflow %>%
    tune::finalize_workflow(
      best_model
    ) %>% 
    parsnip::fit(in_data)
}

fit_model_resamples <- function(in_data, model_workflow, best_model, resamples){
  
  model_workflow %>%
    tune::finalize_workflow(
      best_model
    ) %>% 
    fit_resamples(
      resamples = resamples, 
      metrics = metric_set(
        recall, precision, f_meas, 
        accuracy, kap,
        roc_auc, sens, spec),
      control = control_resamples(save_pred = TRUE, verbose = TRUE)
    )
  
}

cal_model_data <- function(final_model, 
                           new_data, 
                           target_name = "target_class"){
  new_data %>% 
    predict(final_model, .) %>% 
    bind_cols(predict(final_model, new_data, type = "prob")) %>% 
    bind_cols(new_data %>% 
                select(target_name))
}

evaluate_model <- function(pred_tbl = NULL, 
                           model_tuned_workflow = NULL){
  
  some_metrics <- metric_set(bal_accuracy,
                             recall,
                             precision,
                             sensitivity, 
                             specificity,
                             f_meas)
  
  out <- list()
  
  if (!is.null(model_tuned_workflow)) {
    pred_tbl <- model_tuned_workflow %>% 
      collect_predictions() %>% 
      distinct(.row, .keep_all = TRUE)
  }
  
  out[["metrics"]] <- pred_tbl %>% 
    some_metrics(.,
                 truth = target_class,
                 estimate = .pred_class)
  
  out[["confusion_mat"]] <- pred_tbl %>% 
    conf_mat(., target_class, .pred_class)
  
  return(out)
}

select_vimp <- function(in_runmodel, model_workflow, cal_data, pvalue_numper = 40){
  
  imp_list <- lapply(seq_along(in_runmodel), function(i){
    
    cat("The", i, "iteration out of", as.character(length(in_runmodel)) ,
        " model is running.\n")
    
    model_d <- tune::show_best(in_runmodel, n = length(in_runmodel)) %>% 
      .[i, ]
    
    model_workflow %>%
      tune::finalize_workflow(
        model_d
      ) %>% 
      parsnip::fit(cal_data) %>% 
      extract_fit_engine() %>% 
      ranger::importance_pvalues(method = "altmann", 
                                 formula = target_class ~ .,
                                 data = cal_data,
                                 num.permutations = pvalue_numper) %>% 
      as.data.table(keep.rownames = TRUE) %>%
      `colnames<-`(c(paste0("varname",i),
                     paste0("importnace",i),
                     paste0("pvalue",i))) 
  })
  
  out <- do.call(cbind, imp_list) %>%
    as.data.table() %>% 
    mutate(meanvimp = rowMeans(select(., starts_with("imp"))),
           rowdev = rowSds(as.matrix(select(., starts_with("imp"))))) %>% 
    dplyr::select(varname1, meanvimp, rowdev) %>% 
    `colnames<-`(c("varnames", "imp_wmean", "imp_wsd"))
  
  return(out)
}

vimp_fig_step2 <- function(impvar_tbl){
  cat_names <- c('Hydrology', 'Physiography','Physiography', 'Climate',
                 'Hydrology', 'Hydrology', 'Hydrology','Hydrology', 'Hydrology',
                 'Hydrology', 'Hydrology','Landcover', 'Landcover', 
                 'Soils & Geology', 'Hydrology', 'Climate', 'Soils & Geology', 
                 'Landcover') %>% 
    as.factor()
  varimp_basic <- impvar_tbl %>% 
    mutate(Category = cat_names) %>%
    setorder(-imp_wmean)
  varimp_basic$varnames <- c("Q", "slope",  "drainage_area", "P_to_PET_ratio",
                             "Q_mean_p3",  "Q_min_p3", "Q_min_p12", "Q_iav_sd", 
                             "Q_mean_p12", "runoff_dvar", "gwr_to_runoff_ratio", 
                             "pot_nat_vegetation", "land_cover", "karst_status",
                             "Q_iav_cv","wet_days", "Karst_fraction", "glacier_fraction") %>% 
    as.factor(.)
  outp <- varimp_basic[1:18] %>% 
    mutate(varnames = fct_reorder(varnames, desc(imp_wmean))) %>% 
    ggplot(. ,aes(x=varnames,
                  color =Category, fill=Category)) +
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

## create plots for the step2 modeling process
cal_drydays_per_calss <- function(data, station_shp, worldmap_shp){
  
  cal_task <- create_task(in_tbl = subset(data, select = -date) %>% 
                            filter(target > 0),
                          target_name = "target",
                          breaks = c(-Inf, 2, 8, 15, 22, 29, 31), 
                          labels = c("1", "2", "3", "4", "5", "6"),
                          problem_type = "classif",
                          step = "second")
  ratio_classes <- cal_task %>% 
    as.data.table() %>%
    dplyr::group_by(dd_id) %>% 
    dplyr::summarise(count = n(),
                     class_1 = sum(target_class == 1)/count *100,
                     class_2 = sum(target_class == 2)/count *100,
                     class_3 = sum(target_class == 3)/count *100,
                     class_4 = sum(target_class == 4)/count *100,
                     class_5 = sum(target_class == 5)/count *100,
                     class_6 = sum(target_class == 6)/count *100
    ) %>% 
    left_join(., station_shp, by = "dd_id") %>% 
    sf::st_as_sf()
  
  p_all_classes <- lapply(seq_along(1:6), function(i){
    
    label_name <- c("observed % of months with 1-2 no-flow days",
                    "observed % of months with 3-8 no-flow days",
                    "observed % of months with 9-15 no-flow days",
                    "observed % of months with 16-22 no-flow days",
                    "observed % of months with 23-29 no-flow days",
                    "observed % of months with 30-31 no-flow days")
    
    classified_class <- ratio_classes %>% pull(paste0("class_", i)) %>%
      cut(breaks = c(-Inf, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
          labels = c("0", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70",
                     "70-80", "80-90", "90-100"))
    jet <- c("#999999", "#004DFF", "#007FFF", "#00B2FF", "#FFE500",
             "#FFB300", "#FF7F00", "#FF4C00", "#FF1900", "#E50000", 
             "#B20000")
    ratio_classes %>% mutate(classified_class) %>% 
      ggplot() +
      geom_sf(data = worldmap_shp,  fill = "white") +
      geom_sf(aes(color = classified_class))+
      coord_sf(xlim = c(-20, 45), ylim = c(35, 73))+
      theme_bw()+ labs(x = "longitude", y = "latitude")+
      scale_color_manual(values = jet, name = label_name[i], drop = FALSE)+
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
            legend.justification = "left"
      ) +
      guides(color = guide_legend(title.position = "top", 
                                  title.hjust = 0.5))
  })
  
  return(p_all_classes)
  
}

#' Map the percentage of correctly classified of the six classes per gauging station
#' 
#'   

classified_classcor_fig <- function(raw_data, pred_data, stations_shp, worldmap_shp){
  
  if (inherits(pred_data, "tune_results")) {
    in_data <- pred_data %>% 
      collect_predictions() %>% 
      dplyr::distinct(.row, .keep_all = TRUE) %>% 
      mutate(dd_id = raw_data %>% 
               dplyr::filter(target > 0) %>% 
               .[,"dd_id"])
    
  } else {
    in_data <- pred_data %>% 
      mutate(dd_id = raw_data %>% 
               dplyr::filter(target > 0) %>% 
               .[,"dd_id"])
  }
  
  new_df <- in_data %>% 
    group_by(dd_id) %>% 
    dplyr::summarise(count = n(),
                     ratio = sum(target_class == 1 & .pred_class == 1 |
                                   target_class == 2 & .pred_class == 2 |
                                   target_class == 3 & .pred_class == 3 |
                                   target_class == 4 & .pred_class == 4 |
                                   target_class == 5 & .pred_class == 5 |
                                   target_class == 6 & .pred_class == 6) / count * 100)
  
  merge_st <- merge(new_df, stations_shp, by = "dd_id") %>% 
    sf::st_as_sf()
  classified_ratio <- merge_st %>% pull(ratio) %>%
    cut(breaks = c(-Inf, 10, 30, 50, 70, 90, 100),
        labels = c("0-10", "10-30", "30-50", "50-70", "70-90", "90-100"))
  
  jet <- c("#B20000", "#FF1900",
           "#FF7F00",  "#FFE500", "#00B2FF", "#004DFF")
  
  p_out <- merge_st %>% mutate(classified_ratio) %>% 
    ggplot() +
    geom_sf(data = worldmap_shp,  fill = "white") +
    geom_sf(aes(color = classified_ratio))+
    coord_sf(xlim = c(-20, 45), ylim = c(35, 73))+
    theme_bw()+ labs(x = "longitude", y = "latitude")+
    scale_color_manual(values = jet, 
                       name = "Observed % of months correctly classified",
                       drop = FALSE) +
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
          legend.justification = "left"
    ) +
    # annotate(geom="text", x=-15, y=70, label="a",
    #          family = "Times New Roman", size = 12) +
    guides(color = guide_legend(title.position = "top", 
                                title.hjust = 0.5))
  return(p_out)
}



