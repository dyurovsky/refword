# clear all previous variables
rm(list=ls())

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
SKIP_LINES_1 <- 35
SKIP_LINES_2 <- 38

video = 2

VIDEOS = c("reflook", "birthday", "kitchen")
LEARN_STIMS = c("reflook4_4.avi","ref-asd-birthday2.avi","ref-asd-kitchen2.avi")

EXCLUDED_STIMS <- read_csv(paste0("processed_data/loading_files/",
                                  VIDEOS[video],"_excluded.csv"))

LEARN_STIM <- LEARN_STIMS[video]

X_MAX <- 1680
Y_MAX <- 1050

CALIB_SACCADE_TIME <- .3

FRAME_RATE <- 30

IMG_WIDTHS = c(1280,1920,1920)
IMG_HEIGHTS = c(720,1080,1080)

IMG_WIDTH = IMG_WIDTHS[video]
IMG_HEIGHT= IMG_HEIGHTS[video]

SCALE_X = X_MAX/IMG_WIDTH
SCALE_Y = Y_MAX/IMG_HEIGHT

LEARN_STIM_LENGTHS = c(256.823,293.493,307.373)

LEARN_STIM_LENGTH <- LEARN_STIM_LENGTHS[video]

TEST_LENGTH <- 6

AOI_BUFFER <- 25

GROUP <- "ASD"
ET_DATA_DIR <- paste0("raw_data/et_data/",VIDEOS[video],"/",GROUP,"/")
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
adjusted_data <- adjust_calibs(plot = FALSE,video = VIDEOS[video])
###############################################################################
################################ Get Trial Data ###############################
###############################################################################
zero_times <- function(gaze_data, stim_length, split=TRUE) {
  
  gaze_data %>%
    mutate(Time = (Time - min(Time))/1000000) %>%
#     mutate(instance = ifelse(!split | Time <= 6.2,1,2)) %>%
#     group_by(instance, add = TRUE) %>%
    mutate(Time = (Time - min(Time)),
           Time = (Time - (max(Time) - stim_length))) %>%
    filter(Time > 0) %>%
    mutate(Time = floor(FRAME_RATE * Time) / FRAME_RATE)%>%
    group_by(Time, add=TRUE) %>%
    summarise_each(funs(mean = mean(., na.rm = T)), c(x, y))
}

learn_data <- adjusted_data %>%
  filter(Stimulus == LEARN_STIM) %>%
  group_by(subj, Stimulus) %>%
  zero_times(.,LEARN_STIM_LENGTH, split = FALSE)

test_annotations <- read_csv(paste0("processed_data/aois/",
                                    VIDEOS[video], "_tests.csv")) %>%
  mutate(Stimulus = tolower(Stimulus))

raw_test_data <- filter(adjusted_data, str_detect(Stimulus, ".jpg" )) %>%
  filter(!str_detect(Stimulus,"simpkids")) %>%
  left_join(test_annotations)

fam_data <- raw_test_data %>%
  filter(type == "Familiar") %>%
  group_by(subj, type, Stimulus, instance) %>%
  zero_times(.,TEST_LENGTH)

nov_data <- raw_test_data %>%
  filter(type == "Novel") %>%
  group_by(subj, type, Stimulus, instance) %>%
  zero_times(.,TEST_LENGTH)

test_data <- bind_rows(fam_data, nov_data)

###############################################################################
################################## Read AOIS ##################################
###############################################################################
source('loading_helpers/load_aois.R')
train_aois <- get_train_aois(video = VIDEOS[video])

train_annotations <- read_csv(paste0("processed_data/aois/", 
                                     VIDEOS[video], "_trains.csv"))


train_all_aoi_data <- left_join(learn_data, train_aois) %>%
  filter(!is.na(aoi_name)) %>%
  mutate(in_aoi = (x >= top_left_x - AOI_BUFFER) & 
           (x <= bottom_right_x + AOI_BUFFER) &
           (y >= top_left_y - AOI_BUFFER) &
           (y <= bottom_right_y + AOI_BUFFER)) %>%
  ungroup()
  
  
train_other_data <- train_all_aoi_data %>%
  group_by(subj, Time) %>%
#  group_by(subj,instance,Time) %>% #for reflook4 dunno
  summarise(num_in = sum(in_aoi)) %>%
  filter(is.na(num_in) | num_in == 0) %>%
  ungroup() %>%
  rename(aoi_name = num_in) %>%
  mutate(aoi_name = as.factor(aoi_name))

train_aoi_data <- train_all_aoi_data %>%
  filter(in_aoi) %>%
  select(subj,Time,aoi_name) %>%
  #select(subj,instance,Time,aoi.name) %>% for reflook4 dunno
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
       #  else if(aoi.name == Hand) "Hand"
         else if(tolower(aoi_name) == "0") "None"
         else "Other") %>%
  select(-aoi_name, -Target, -Competitor, -Face, -Hand) %>%
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
  mutate(window.type = factor(window_type, 
                              levels = c("baseline","name_look","look_name2",
                                         "name2_reach","reach_contact",
                                         "contact_end"))) %>%
            select(-start, -name1, -look, -name2, -name3,
                   -initiate_reach ,-point_of_contact, -end) %>% #-instance
  group_by(subj,trial) %>%
  mutate(Time = Time - min(Time))

test_aois <- get_test_aois()

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
  rowwise() %>%
  mutate(aoi = if(is.na(aoi_name)) as.character(NA)
         else if(aoi_name == target) "Target"
         else if(aoi_name == "0") "Other"
         else "Competitor") %>%
  mutate(onset =  floor(FRAME_RATE * onset) / FRAME_RATE) %>%
  group_by(trial) %>%
  mutate(Time = Time - onset) %>%
  select(-target,-aoi_name,-instance,-Stimulus,-onset)

###############################################################################
############################# Add Demographic Data ############################
###############################################################################
demo_data <- read_csv(paste0('raw_data/demo_data/',VIDEOS[video],
                             "/", GROUP,'.subinfoR4.csv'))

if(GROUP == "child") {
  demo.data %<>%
    select(subid,dot,dob,gender,english,premie) %>%
    mutate_each(funs(mdy),dot,dob) %>%
    mutate(age = (dot - dob)/eyears(1),
           gender = factor(gender, labels = c("Male", "Female",NA))) %>%
    rename(subj = subid) %>%
    select(-dot,-dob)
  test.aoi.data %<>% left_join(demo.data)
  train.annotated.data %<>% left_join(demo.data) 
} else if(GROUP == "ASD") {
  demo_data %<>%
    select(subid, dot, dob, gender) %>%
    mutate_each(funs(mdy), dot, dob) %>%
    mutate(age = (dot - dob) / eyears(1),
           gender = factor(gender, levels = c("m", "f"),
                           labels = c("Male", "Female"))) %>%
    rename(subj = subid) %>%
    select(-dot, -dob)
  test_aoi_data %<>% left_join(demo_data)
  train_annotated_data %<>% left_join(demo_data) 
  }
###############################################################################
############################## Write Output Data ##############################
###############################################################################
write_csv(test_aoi_data, paste0('processed_data/csvs/',VIDEOS[video], "/",
                               GROUP,'/reflook_test_data.csv'))
write_csv(train_annotated_data, paste0('processed_data/csvs/',
                                      VIDEOS[video], "/", 
                                      GROUP,'/reflook_train_data.csv'))

tmp <- test_aoi_data %>%
  group_by(Time, subj, trial) %>%
  summarise(correct = sum(aoi == "Target",na.rm=T)/
              (sum(aoi=="Target",na.rm=T)+sum(aoi=="Competitor",na.rm=T))) %>%
  summarise(correct = mean(correct,na.rm = T)) %>%
  summarise(correct = mean(correct, na.rm = T))




