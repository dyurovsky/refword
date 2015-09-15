read_raw_data <- function(file_names) {
  
  # strip filenames down to subject ids
  rename_typical_subj <- function(subj) {
    subj <- sub("Reflook4_2\\s\\(\\d\\)_","",subj)
    subj <- sub("Reflook4_2_","",subj)
    subj <- sub("_\\d+\\s.+","",subj)
  }
  # strip filenames down to subject ids
  rename_asd_subj <- function(subj) {
    subj <- sub("-eye_data Samples_fixed.txt","",subj)
    subj <- paste0(strsplit(subj,"_")[[1]][1:2],collapse="_")
  }
  
  if(GROUP == "ASD") {rename_subj <- rename_asd_subj
  } else {rename_subj <- rename_typical_subj}
  
  
  read_data <- function(x) {
    # Deal with idiosyncacies caused by different number of calib points
    result <- tryCatch(mutate(fread(paste0(ET_DATA_DIR, x), skip = SKIP_LINES_1),
                              subj = rename_subj(x)),
                       error = function(e) mutate(fread(paste0(ET_DATA_DIR, x),
                                                        skip=SKIP_LINES_2),
                                                  subj = rename_subj(x)))
    
    # Deal with Timestamps greater than R's max 32bit int
    if(max(result$Time) > .Machine$integer.max){
      result %<>% 
        mutate(Time = Time - min(Time) + 1,
               Time = as.integer(Time))
    }
    
    return(result)
  }
  
  #read all files and concatenate
  all_raw_data <- bind_rows(lapply(file_names,read_data)) %>%
    mutate(Stimulus = tolower(Stimulus)) %>%
    filter(!Stimulus %in% EXCLUDED_STIMS)
  
  # Make each x/y estimate the average of left and right eyes
  xs <- rowMeans(all_raw_data[,c("L POR X [px]","R POR X [px]")], na.rm=TRUE)
  ys <- rowMeans(all_raw_data[,c("L POR Y [px]","R POR Y [px]")], na.rm=TRUE)
  
  all_raw_data$x = xs
  all_raw_data$y = ys
  
  # Drop points outside the screen area
  all_results <- all_raw_data %>%
    select(Time,Stimulus,subj,x,y) %>%
    mutate(x = ifelse(x == 0 | x >= X_MAX | y == 0 | y >= Y_MAX, NA, x),
           y = ifelse(x == 0 | x >= X_MAX | y == 0 | y >= Y_MAX, NA, y))
  
  return(all_results)
}
