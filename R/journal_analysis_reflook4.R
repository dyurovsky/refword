# clear all previous variables
rm(list=ls())

#get lab version of useful R functions

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
library(cowplot)

#splits ages into half-years
split.ages <-function(x) {floor(x)}
na.mean <- function(x){mean(x,na.rm=T)}
na.sum <- function(x){sum(x,na.rm=T)}
#Constants for window-of-interest analysis
TEST_START <- 1
TEST_END <- 4
TRAIN_START <- 0
TRAIN_END <- 15

#Use color-brewer colors for graphing
man_cols <- c("#e41a1c","#377eb8","#4daf4a",
              "#984ea3","#ff7f00","#a65628",
              "#f781bf","#999999")

man_cols <- c("#d0d1e6","#74a9cf","#0570b0","#023858","#ec7014","#1a9850")


man_cols <- c("#c6dbef","#6baed6","#2171b5","#08306b","#f16913","#6a51a3")

###############################################################################
################################ LOADING DATA #################################
###############################################################################
#read looking data
raw.child.train.data <- fread("processed_data/csvs/child/reflook_train_data.csv") %>%
  mutate(age.grp = as.factor(split.ages(age))) %>%
  filter(age >= 1 & age < 5)

raw.child.test.data <- fread("processed_data/csvs/child/reflook_test_data.csv") %>%
  mutate(age.grp = as.factor(split.ages(age))) %>%
  mutate(gender = factor(gender,labels=c("Male", "Female"))) %>%
  filter(age >= 1 & age < 5)


raw.adult.train.data <- fread("processed_data/csvs/adult/reflook_train_data.csv") %>%
  mutate(age = NA,age.grp = "adult") 
raw.adult.test.data <- fread("processed_data/csvs/adult/reflook_test_data.csv") %>%
  mutate(age = NA,age.grp = "adult") 

raw.ASD.train.data <- fread("processed_data/csvs/ASD/reflook_train_data.csv") %>%
  filter(is.na(age) | age <= 7) %>%
  mutate(age.grp = "ASD") 

raw.ASD.test.data <- fread("processed_data/csvs/ASD/reflook_test_data.csv") %>%
  filter(is.na(age) | age <= 7) %>%
  mutate(age.grp = "ASD")

raw.ASD.train.data <- fread("processed_data/csvs/birthday/reflook_train_data.csv") %>%
  filter(is.na(age) | age <= 7) %>%
  mutate(age.grp = "ASD") 

raw.ASD.test.data <- fread("processed_data/csvs/birthday/reflook_test_data.csv") %>%
  filter(is.na(age) | age <= 7) %>%
  mutate(age.grp = "ASD")


raw.test.data <- raw.ASD.test.data
raw.train.data <- raw.ASD.train.data

###############################################################################
################################## Exclusions #################################
###############################################################################
start.ps <- raw.child.test.data %>%
  group_by(age.grp) %>%
  summarise(n = length(unique(subj)))
  
dropped.english <- raw.child.test.data %>%
  filter(english <4) %>%
  summarise(n = (length(unique(subj))))

dropped.premie <- raw.child.test.data %>%
  filter(premie == 2) %>%
  summarise(n = (length(unique(subj))))

kept.ps <- raw.child.test.data %>%
  filter(english >= 4) 

count.girls <- kept.ps %>%
  group_by(age.grp,gender) %>%
  summarise(kept.girls = length(unique(subj))) %>%
  filter(gender == "Female")

count.ps <- kept.ps %>%
  group_by(age.grp) %>%
  summarise(n = length(unique(subj))) %>%
  left_join(kept.girls)

kept.child.train.data <- raw.child.train.data %>%
  filter(subj %in% unique(kept.ps$subj)) %>%
  select(-gender,-english,-premie)

kept.child.test.data <- raw.child.test.data %>%
  filter(subj %in% unique(kept.ps$subj)) %>%
  select(-gender,-english,-premie)

# raw.ASD.data <- read_csv("data/reflook4_ASD_data.csv") %>%
#   mutate(age.grp = "ASD") %>%
#  filter(age >= 1 & age < 6)


raw.test.data <- bind_rows(kept.child.test.data,raw.adult.test.data)
raw.train.data <- raw.train.data%>%#bind_rows(kept.child.train.data,raw.adult.train.data) %>%
  mutate(window.type = factor(window.type, 
                              levels = c("baseline","name.look","look.name2",
                                         "name2.reach","reach.contact",
                                         "contact.end")))


na.out.missing <- function(data,prop = .5) {
  

na.props <- data %>%
  group_by(subj,trial) %>%
  summarise(na.prop = sum(is.na(aoi))/length(aoi))

complete.data <- na.props %>%
  filter(na.prop <= .5) %>%
  select(-na.prop) %>%
  left_join(data)

missing.data <- na.props %>%
  filter(na.prop > .5) %>%
  select(-na.prop) %>%
  left_join(mutate(data,aoi=NA))

bind_rows(complete.data,missing.data) %>%
  arrange(subj,trial,Time)
}

clean.test.data <- na.out.missing(raw.test.data)
clean.train.data <- na.out.missing(raw.train.data)

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