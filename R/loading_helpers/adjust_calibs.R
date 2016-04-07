adjust_calibs <- function(plot = TRUE,video) {
  pts <- read_csv(paste0("processed_data/aois/",video,"_calibs.csv")) %>%
    mutate(start_time = start_time + CALIB_SACCADE_TIME,
           end_time = end_time + CALIB_SACCADE_TIME)
  
  #pull out calibration data
  learn_data <- filter(all_results, Stimulus == CALIB_STIM) %>%
    group_by(subj) %>%
    mutate(Time = (Time - min(Time))/1000000) %>%
    filter(!is.na(y) & !is.na(x))

  calib_data <- lapply(1:nrow(pts), function (pt) {
    learn_data %>%
      filter(Time >= pts[pt,]$start_time,
             Time <= pts[pt,]$end_time) %>%
      mutate(point = pts[pt,]$point,
             instance = pts[pt,]$instance)}) %>%
    bind_rows %>%
    arrange(subj, Time) %>%
    group_by(subj) %>%
    left_join(pts) 
  
  if(video == "soc_word") {
    
    pts2 <- read_csv(paste0("processed_data/aois/",video,"_calibs_2.csv")) %>%
      mutate(start_time = start_time + CALIB_SACCADE_TIME,
             end_time = end_time + CALIB_SACCADE_TIME)
    
    SWITCH_SUBJ = "2013_04_30_265"
    
    first_ind = first(which(calib_data$subj == SWITCH_SUBJ))
    calib_data <- calib_data[1:(first_ind-1),]
 
    second_learn_data <- learn_data[first_ind:nrow(learn_data),]
    
    calib_data_2 <- lapply(1:nrow(pts2), function (pt) {
      second_learn_data %>%
        filter(Time >= pts2[pt,]$start_time,
               Time <= pts2[pt,]$end_time) %>%
        mutate(point = pts2[pt,]$point,
               instance = pts2[pt,]$instance)}) %>%
      bind_rows %>%
      arrange(subj, Time) %>%
      group_by(subj) %>%
      left_join(pts2) 
    
    calib_data <- bind_rows(calib_data,calib_data_2) %>%
      arrange(subj, Time) %>%
      group_by(subj) %>%
      left_join(pts2) 
  }
  
  keep_subjs <- calib_data %>%
    summarise(n = n()) %>%
    filter(n > 1)
  
  calib_data %<>% filter(subj %in% (keep_subjs$subj))
  
  x_models <- calib_data %>%
    do(x_model = rlm(true_x ~ x, data = .)) 
  y_models <- calib_data %>%
    do(y_model = rlm(true_y ~ y, data = .))
  
  models <- left_join(x_models,y_models)
  subjs <- unique(calib_data$subj)
  
  predicted_data <- lapply(subjs, function (s) {
    models <- filter(models, subj == s)
    filter(calib_data, subj == s) %>%
      mutate(predicted_x = predict(models$x_model[[1]]),
             predicted_y = predict(models$y_model[[1]]))}) %>%
    bind_rows %>%
    rename(empirical_x = x, empirical_y = y) %>%
    group_by(subj) %>%
    gather(measure,value,empirical_x,predicted_x,empirical_y,predicted_y) %>%
    separate(measure, into = c("measure", "dimension"), sep = "\\_") %>%
    spread(dimension,value)
  
  
  labeli <- function(variable, value){
    names_li <- list("empirical" = "Before Correction", 
                     "predicted" = "After Correction")
    return(names_li[value])
  }
  
  plot_calibs <- function(s) {
    subj_data = filter(predicted_data,subj == s)
    
    ggplot(aes(x = x,y = y,color=interaction(measure,instance)), data = subj_data) + 
      facet_grid(. ~ measure, labeller = labeli) +
      geom_point(size = .8) +
      geom_point(aes(x = true_x, y = true_y), color="black", shape = 3, size = 3)+
      scale_color_brewer(palette="Set1") +
      scale_x_continuous(limits=c(0, X_MAX), breaks=seq(0, X_MAX, 500))+
      scale_y_continuous(limits=c(0, Y_MAX), breaks=seq(0, Y_MAX, 500))+
      theme_bw() +
      theme(legend.position = "none", axis.title.x = element_blank(),
            axis.title.y = element_blank())
    ggsave(paste0("processed_data/calib_adjust/",video,
                  "/",GROUP,"/",s,".pdf"), width=8, height=4)
  }
  
  if(plot == TRUE) {plot_calibs <- sapply(subjs, plot_calibs)
  } else{
    adjusted_subjs <- read_csv(paste0("processed_data/calib_adjust/", video, 
                                      "/", GROUP, "/calibs.csv"))
    
    adjusted_data <- lapply(subjs, function (s) {
     # print(s)
      action <- filter(adjusted_subjs, str_detect(s, subj))
      
      if(action$include == 1) {
        out_data <- filter(all_results,subj == s) 
        if(action$adjust == 1) {
          models <- filter(models, subj == s)
          out_data$x = predict(models$x_model[[1]], newdata = out_data)
          out_data$y = predict(models$y_model[[1]], newdata = out_data)
        }
      }
      else{
        out_data <- NULL
      }
      return(out_data)}) %>%
      bind_rows %>%
      mutate(x = ifelse(x <= 0 | x >= X_MAX | y <= 0 | y >= Y_MAX, NA, x),
             y = ifelse(x <= 0 | x >= X_MAX | y <= 0 | y >= Y_MAX, NA, y)) %>%
      group_by(subj)
  }
}
