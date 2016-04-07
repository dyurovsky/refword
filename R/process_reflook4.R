#load libraries for data manipulation and graphing
library(MASS)
library(dplyr)
library(directlabels)
library(xtable)
library(tidyr)
library(data.table)
library(bit64)
library(jpeg)
library(stringr)
library(XML)
library(readr)
library(tidyr)
library(magrittr)
library(zoo)
library(lubridate)
library(ggplot2)
library(langcog)
###############################################################################
################################### Constants #################################
###############################################################################
#try/catch for different number of calib points
MAX_HEADER_ROWS <- 40

video = 4

VIDEOS = c("reflook", "birthday", "kitchen", "soc_word")

SOCWORD_LEARN_STIMS = c("s1_1manu.avi", "s1_2bosa.avi", "s1_3bosa.avi",
                        "s1_4manu.avi", "s1_5manu.avi", "s1_6bosa.avi", 
                        "s1_7bosa.avi", "s1_8manu.avi")

LEARN_STIMS = list("reflook4_4.avi", "ref-asd-birthday2.avi", 
                "ref-asd-kitchen2.avi", SOCWORD_LEARN_STIMS)

CALIB_STIMS = c("reflook4_4.avi", "ref-asd-birthday2.avi",
                "ref-asd-kitchen2.avi","calib.avi")



EXCLUDED_STIMS <- read_csv(paste0("processed_data/loading_files/",
                                  VIDEOS[video],"_excluded.csv"))

LEARN_STIM <- LEARN_STIMS[video]
CALIB_STIM <- CALIB_STIMS[video]

X_MAX <- 1680
Y_MAX <- 1050

CALIB_SACCADE_TIME <- .3

FRAME_RATE <- 30

IMG_WIDTHS = c(1280,1920,1920,960)
IMG_HEIGHTS = c(720,1080,1080,540)

IMG_WIDTH = IMG_WIDTHS[video]
IMG_HEIGHT= IMG_HEIGHTS[video]

SCALE_X = X_MAX/IMG_WIDTH
SCALE_Y = Y_MAX/IMG_HEIGHT

LEARN_STIM_LENGTHS = c(256.823, 293.493, 307.373, NA)

LEARN_STIM_LENGTH <- LEARN_STIM_LENGTHS[video]

TEST_LENGTHS <- c(6, 6, 6, 7.5)
TEST_LENGTH <- TEST_LENGTHS[video]

AOI_BUFFER <- 25

GROUP <- "child"
ET_DATA_DIR <- paste0("raw_data/et_data/",VIDEOS[video],"/",GROUP,"/")

trunc <- function(x, ..., prec = 4) base::trunc(x * 10^prec, ...) / 10^prec

###############################################################################
############################### Read all raw data #############################
###############################################################################
source('loading_helpers/read_raw_data.R')
all_result_files <- list.files(path = ET_DATA_DIR, pattern = '*.txt',
                               all.files = FALSE)

all_results <- read_raw_data(all_result_files)

###############################################################################
############################ Calibration adjustment ###########################
###############################################################################
source('loading_helpers/adjust_calibs.R')
adjusted_data <- adjust_calibs(plot = FALSE, video = VIDEOS[video])

if(VIDEOS[video] == "soc_word") {
# SOC WORD DOESN'T HAVE CALIB DATA FOR SOME SUBJS
missing_calibs <- all_results %>%
  group_by(subj) %>%
  summarise(calib_missing = sum(Stimulus == CALIB_STIM) == 0) %>%
  filter(calib_missing)

adjusted_data <- filter(all_results, subj %in% missing_calibs$subj) %>%
  bind_rows(adjusted_data)
}


 ###############################################################################
################################ Get Trial Data ###############################
###############################################################################
zero_times <- function(gaze_data, stim_length, split = FALSE) {
  
  gaze_data <- gaze_data %>%
    mutate(Time = (Time - min(Time))/1000000) %>%
    mutate(instance = ifelse(split & (Time > TEST_LENGTH + .2), 
                             2, 1)) %>% #reflook4 issue
     group_by(instance, add = TRUE) %>%
     mutate(Time = (Time - min(Time)))
  
  if(!is.na(stim_length))
    gaze_data %<>% mutate(Time = (Time - (max(Time) - stim_length)))
  
  gaze_data %>%
    filter(Time > 0) %>%
    mutate(Time = trunc(floor(FRAME_RATE * Time) / FRAME_RATE))%>%
    group_by(Time, add=TRUE) %>%
    summarise_each(funs(mean = mean(., na.rm = T)), c(x, y))
}

learn_data <- adjusted_data %>%
  filter(Stimulus %in% unlist(LEARN_STIM)) %>%
  group_by(subj, Stimulus) %>%
  zero_times(., LEARN_STIM_LENGTH)

test_annotations <- read_csv(paste0("processed_data/aois/",
                                    VIDEOS[video], "_tests.csv")) %>%
  mutate(Stimulus = tolower(Stimulus))

test_data <- adjusted_data %>%
  ungroup() %>%
  filter(str_detect(Stimulus, ".jpg" )) %>%
  filter(!str_detect(Stimulus, "simpkids")) %>%
  filter(!str_detect(EXCLUDED_STIMS, Stimulus)) %>%
  group_by(subj, Stimulus) %>%
  zero_times(., TEST_LENGTH, split = T) %>%
  left_join(test_annotations) 

###############################################################################
################################## Read AOIS ##################################
###############################################################################
train_annotations <- read_csv(paste0("processed_data/aois/", 
                                     VIDEOS[video], "_trains.csv"))

if(VIDEOS[video] == "soc_word") {
  source('loading_helpers/load_aois_socword.R')
  train_aois <- get_train_aois(video = VIDEOS[video],
                               annotations = train_annotations) %>%
    mutate(Time = trunc(Time)) %>%
    filter(!str_detect(aoi_name, "hand")) %>%
    left_join(train_annotations)
  
} else {
  source('loading_helpers/load_aois.R')
  train_aois <- get_train_aois(video = VIDEOS[video]) %>%
    mutate(Time = trunc(Time)) %>%
    filter(!str_detect(aoi_name, "hand"))
}

test_aois <- get_test_aois() %>%
  mutate(Time = trunc(Time))

###############################################################################
################################ Make Heatmaps ################################
###############################################################################


###############################################################################
################################# Process AOIS ################################
###############################################################################
closest <- function(time) {
  times = trunc(seq(floor(time),ceiling(time), 1/FRAME_RATE))
  diffs <- abs(time - times)
  times[which(diffs == min(diffs))]
}

train_all_aoi_data <- left_join(learn_data, train_aois) %>%
  filter(!is.na(aoi_name)) %>%
  mutate(in_aoi = (x >= top_left_x - AOI_BUFFER) & 
           (x <= bottom_right_x + AOI_BUFFER) &
           (y >= top_left_y - AOI_BUFFER) &
           (y <= bottom_right_y + AOI_BUFFER)) %>%
  ungroup()

if(VIDEOS[video] == "soc_word") {
  train_all_aoi_data %<>%
    mutate(Time = Time - point_of_disambiguation) %>%
    filter(Time >= -2)
  
  train_other_data <- train_all_aoi_data %>%
    group_by(subj, trial, instance, Time)
} else{
  train_other_data <- train_all_aoi_data %>%
    group_by(subj, instance, Time) #for reflook4 dunno
}

train_other_data %<>%
  summarise(num_in = sum(in_aoi)) %>%
  filter(is.na(num_in) | num_in == 0) %>%
  ungroup() %>%
  rename(aoi_name = num_in) %>%
  mutate(aoi_name = as.factor(aoi_name))

if(VIDEOS[video] != "soc_word") {
  train_aoi_data <- train_all_aoi_data %>%
    filter(in_aoi) %>%
    select(subj, instance, Time, aoi_name) %>% #for reflook4 dunno
    bind_rows(train_other_data)
  
  train_annotated_data <- bind_rows(lapply(1:nrow(train_annotations), function(x) {
    filter(train_aoi_data, Time >= as.numeric(train_annotations[x,"start"]) & 
             Time <= as.numeric(train_annotations[x,"end"])) %>%
      cbind(train_annotations[x,])})) %>%
    rowwise() %>%
    mutate(aoi = if(is.na(aoi_name)) as.character(NA)
           else if(tolower(aoi_name) == tolower(Target)) "Target"
           else if(tolower(aoi_name) == tolower(Competitor)) "Competitor"
           else if(tolower(aoi_name) == tolower(Face)) "Face"
#            else if(!is.na(Other_Face) & 
#                    tolower(aoi_name) == tolower(Other_Face)) "Other Face"
           else if(tolower(aoi_name) == "0") "None"
           else "Other") %>%
    select(-aoi_name, -Target, -Competitor, -Face, -Hand) %>%
                                  #  -Other_Face, -Hand) %>%
    mutate(window_type = if(Time >= start & 
                            Time < name1) "baseline"
           else if(Time >= name1 & 
                   Time < look) "name_look"
           else if(Time >= look &
                   Time < name2) "look_name2"
           else if(Time >= name2 &
                   Time < initiate_reach) "name2_reach"
           else if(Time >= initiate_reach &
                   Time < point_of_contact) "reach_contact"
           else if(Time >= point_of_contact &
                   Time <= end) "contact_end"
           else "NA") %>%
    ungroup() %>%
    filter(window_type != "NA") %>%
    mutate(window_type = factor(window_type, 
                                levels = c("baseline", "name_look", "look_name2",
                                           "name2_reach", "reach_contact",
                                           "contact_end"))) %>%
    select(-start, -name1, -look, -name2, -name3,
           -initiate_reach ,-point_of_contact, -end) %>%
    group_by(subj, trial) %>%
    mutate(Time = Time - min(Time)) %>%
    rowwise() %>%
    mutate(Time = closest(Time))
} else {
  
  train_aoi_data <- train_all_aoi_data %>%
    filter(in_aoi) %>%
    select(subj, trial, instance, Time, aoi_name) %>%
    bind_rows(train_other_data)
  
  train_annotated_data <- train_aoi_data %>%
    left_join(train_annotations) %>%
    rowwise() %>%
    mutate(aoi = if(is.na(aoi_name)) as.character(NA)
           else if(tolower(aoi_name) == tolower(Target)) "Target"
           else if(tolower(aoi_name) == tolower(Competitor)) "Competitor"
           else if(tolower(aoi_name) == tolower(Face)) "Face"
           #  else if(aoi.name == Hand) "Hand"
           else if(tolower(aoi_name) == "0") "None"
           else "Other") %>%
    select(-aoi_name, -Target, -Competitor, -Face) %>%
    rowwise() %>%
    mutate(Time = closest(Time))
    
}
 

# Get all times in AOIS
test_all_aoi_data <- left_join(test_data, test_aois) %>%
  mutate(in_aoi = (x >= top_left_x - AOI_BUFFER) & 
           (x <= bottom_right_x + AOI_BUFFER) &
           (y >= top_left_y - AOI_BUFFER) &
           (y <= bottom_right_y + AOI_BUFFER)) 

test_other_data <- test_all_aoi_data %>%
  group_by(subj, Stimulus, instance, Time, type) %>%
  summarise(num_in = sum(in_aoi)) %>%
  filter(is.na(num_in) | num_in == 0) %>%
  ungroup() %>%
  rename(aoi_name = num_in) %>%
  mutate(aoi_name = as.factor(aoi_name))

test_aoi_data <- test_all_aoi_data %>%
  filter(in_aoi) %>%
  select(subj, Stimulus, instance, type, Time, aoi_name) %>%
  bind_rows(test_other_data) %>%
  ungroup() %>%
  arrange(subj, Stimulus, instance, Time) %>%
  left_join(test_annotations) %>%
  filter(!is.na(target)) %>%
  rowwise() %>%
  mutate(aoi = if(is.na(aoi_name)) as.character(NA)
         else if(aoi_name == target) "Target"
         else if(aoi_name == "0") "Other"
         else "Competitor") %>%
  mutate(onset = floor(FRAME_RATE * onset) / FRAME_RATE) %>%
  group_by(type, trial) %>%
  mutate(Time = trunc(Time - onset)) %>%
  rowwise() %>%
  mutate(Time = closest(Time)) %>%
  select(-target, -aoi_name, -instance, -Stimulus, -onset)

###############################################################################
############################# Add Demographic Data ############################
###############################################################################
demo_data <- fread(paste0('raw_data/demo_data/',VIDEOS[video],
                             "/", GROUP,'.subinfoR4.csv'))

if(VIDEOS[video] == "soc_word"){
  demo_data %<>%
    select(subid, age, gender, english, parent_education, premature) %>%
    rename(subj = subid)
  
  test_aoi_data %<>% left_join(demo_data)
  train_annotated_data %<>% left_join(demo_data) 
} else if(GROUP == "child") {
  
  # 
  # tmp <- raw_child_test_data %>%
  #   rowwise() %>%
  #   mutate(english = if(is.na(english)) as.numeric(NA)
  #          else if(english == 6) 100
  #          else if(english == 5) 90
  #          else if(english == 4) 75
  #          else if(english == 3) 50
  #          else if(english == 2) 25
  #          else if(english == 1) 5
  #          else 0)
  # 
  # tmp <- tmp %>%
  #   rowwise() %>%
  #   mutate(premie = if(is.na(premie)) as.logical(NA)
  #          else if(premie == 1) FALSE
  #          else TRUE)
  
  demo_data %<>%
    select(subid, dot, dob, gender, english, premie) %>%
    mutate_each(funs(mdy), dot, dob) %>%
    mutate(age = (dot - dob)/dyears(1),
           gender = factor(gender, labels = c("Male", "Female",NA))) %>%
    rename(subj = subid) %>%
    select(-dot, -dob)
  test_aoi_data %<>% left_join(demo_data)
  train_annotated_data %<>% left_join(demo_data) 
} else if(GROUP == "ASD") {
  demo_data %<>%
    select(subid, dot, dob, gender) %>%
    mutate_each(funs(mdy), dot, dob) %>%
    mutate(age = (dot - dob) / dyears(1),
           gender = factor(gender, levels = c("m", "f"),
                           labels = c("Male", "Female"))) %>%
    rename(subj = subid) %>%
    select(-dot, -dob)
  test_aoi_data %<>% left_join(demo_data)
  train_annotated_data %<>% left_join(demo_data) 
} else if(GROUP == "long") {
  demo_data %<>%
    select(subid, dot, dob, gender) %>%
    mutate_each(funs(mdy), dot, dob) %>%
    mutate(age = (dot - dob)/dyears(1),
           gender = factor(gender, labels = c("Male", "Female"))) %>%
    rename(subj = subid) %>%
    select(-dot,-dob)
  test_aoi_data %<>% left_join(demo_data)
  train_annotated_data %<>% left_join(demo_data) 
}
###############################################################################
############################## Write Output Data ##############################
###############################################################################
write_csv(test_aoi_data, paste0('processed_data/csvs/',VIDEOS[video], "/",
                               GROUP,'/test_data.csv'))
write_csv(train_annotated_data, paste0('processed_data/csvs/',
                                      VIDEOS[video], "/", 
                                      GROUP,'/train_data.csv'))

tmp <- train_annotated_data %>%

  group_by(Time, trial, subj) %>%
  summarise(correct = sum(aoi == "Target",na.rm=T)/
              (sum(aoi=="Target",na.rm=T)+sum(aoi=="Competitor",na.rm=T))) %>%
  summarise(correct = mean(correct, na.rm = T)) %>%
  summarise(correct = mean(correct, na.rm = T))




