#load libraries for data manipulation and graphing
library(data.table)
library(directlabels)
library(dplyr)
library(xtable)
library(tidyr)
library(magrittr)
library(readr)
library(langcog)
library(ggplot2)

#splits ages into half-years
split_ages <-function(x) {floor(x)}
na.mean <- function(x){mean(x,na.rm=T)}
na.sum <- function(x){sum(x,na.rm=T)}
#Constants for window-of-interest analysis


#Use color-brewer colors for graphing
# man_cols <- c("#e41a1c","#377eb8","#4daf4a",
#               "#984ea3","#ff7f00","#a65628",
#               "#f781bf","#999999")
# 
# man_cols <- c("#d0d1e6","#74a9cf","#0570b0","#023858","#ec7014","#1a9850")
# 

man_cols <- c("#c6dbef","#6baed6","#2171b5","#08306b","#f16913","#6a51a3")

###############################################################################
################################ LOADING DATA #################################
###############################################################################
#read looking data
raw_child_train_data <- fread("processed_data/csvs/reflook/child/train_data.csv") %>%
  mutate(age_grp = as.factor(split_ages(age))) %>%
 # mutate(age_grp = "child") %>%
  filter(age >= 1 & age < 5)

raw_child_test_data <- fread("processed_data/csvs/reflook/child/test_data.csv") %>%
  #mutate(age_grp = "child") %>%
  mutate(age_grp = as.factor(split_ages(age))) %>%
  filter(age >= 1 & age < 5)

raw_adult_train_data <- fread("processed_data/csvs/reflook/adult/train_data.csv") %>%
  mutate(age = NA, age_grp = "adult") 

raw_adult_test_data <- fread("processed_data/csvs/reflook/adult/test_data.csv") %>%
  mutate(age = NA, age_grp = "adult") 

raw_ASD_train_data <- fread("processed_data/csvs/reflook/ASD/train_data.csv") %>%
  filter(age <= 8) %>%
  mutate(age_grp = "ASD") 

raw_ASD_test_data <- fread("processed_data/csvs/reflook/ASD/test_data.csv") %>%
  filter(age <= 8) %>%
  mutate(age_grp = "ASD")

###############################################################################
################################## Exclusions #################################
###############################################################################
start_ps <- raw_child_test_data %>%
  group_by(age_grp) %>%
  summarise(n = length(unique(subj)))
  
dropped_english <- raw_child_test_data %>%
  filter(english < 4 | is.na(english)) %>%
  summarise(n = (length(unique(subj))))

dropped_premie <- raw_child_test_data %>%
  filter(premie == 2 | is.na(premie)) %>%
  summarise(n = (length(unique(subj))))

kept_ps <- raw_child_test_data %>%
#  filter(english >= 4, !is.na(english)) 
  filter(english >= 4, premie != 2, !is.na(premie), !is.na(english)) 

count_girls <- kept_ps %>%
  group_by(age_grp,gender) %>%
  summarise(kept_girls = length(unique(subj))) %>%
  filter(gender == "Female") %>%
  select(-gender)

count_ps <- kept_ps %>%
  group_by(age_grp) %>%
  summarise(n = length(unique(subj))) %>%
  left_join(count_girls)

kept_child_train_data <- raw_child_train_data %>%
  filter(subj %in% unique(kept_ps$subj)) %>%
  select(-gender,-english,-premie)

kept_child_test_data <- raw_child_test_data %>%
  filter(subj %in% unique(kept_ps$subj)) %>%
  select(-gender,-english,-premie)

# raw.ASD.data <- read_csv("data/reflook4_ASD_data.csv") %>%
#   mutate(age.grp = "ASD") %>%
#  filter(age >= 1 & age < 6)
# 
raw_test_data <- bind_rows(kept_child_test_data, raw_adult_test_data,
                           raw_ASD_test_data) %>%
#   mutate(age_grp = factor(age_grp, levels = c("1", "2", "3", "4",
#                                                   "ASD", "adult")),
         mutate(subj = paste0(age_grp, "_", subj))

raw_train_data <- bind_rows(kept_child_train_data, raw_adult_train_data,
                            raw_ASD_train_data) %>%
  mutate(window_type = factor(window_type, 
                              levels = c("baseline","name_look","look_name2",
                                         "name2_reach","reach_contact",
                                         "contact_end")),
#          age_grp = factor(age_grp, levels = c("1", "2", "3", "4",
#                                               "ASD", "adult")),
         subj = paste0(age_grp, "_", subj))


na_out_missing <- function(data, PROP = .5, MIN_TRIALS = 4) {
  
na_props <- data %>%
  group_by(subj, trial) %>%
  summarise(na_prop = sum(is.na(aoi))/length(aoi))

na_subjs <- na_props %>%
  summarise(na_trials = sum(na_prop > PROP)) %>%
  group_by(subj) %>%
  filter(na_trials > (8 - MIN_TRIALS))

complete_data <- na_props %>%
  filter(!subj %in% na_subjs$subj) %>%
  filter(na_prop <= PROP) %>%
  select(-na_prop) %>%
  left_join(data)

missing_data <- na_props %>%
  filter(!subj %in% na_subjs$subj) %>%
  filter(na_prop > PROP) %>%
  select(-na_prop) %>%
  left_join(mutate(data, aoi = NA))

missing_subjs <- na_props %>%
  filter(subj %in% na_subjs$subj) %>%
  left_join(mutate(data, aoi = NA))
  

bind_rows(complete_data,missing_data, missing_subjs) %>%
  arrange(subj,trial,Time)
}

novel_data <- filter(raw_test_data, type == "Novel")
familiar_data <- filter(raw_test_data, type == "Familiar")

test_data <- bind_rows(na_out_missing(novel_data),
                       na_out_missing(familiar_data))
train_data <- na_out_missing(raw_train_data)

# Reshape data for subsequent analyses 
source('analysis_helpers/munge_data_reflook4.R')

# Create window-of-analysis graphs
source('analysis_helpers/dot_graphs_reflook4.R')

# Compute statistical models for paper
source('analysis_helpers/statistical_models.R')

# Reshape timecourse data for subequent analyses
source('timecourse_data_reflook4.R')

# Create timecourse graphs for paper
source('timecourse_graphs_reflook4.R')