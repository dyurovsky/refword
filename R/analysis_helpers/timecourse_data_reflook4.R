###############################################################################
############################### TIMECOURSE DATA ###############################
###############################################################################
library(zoo)
TEST_TIMECOURSE_START <- - 1
TEST_TIMECOURSE_END <- 4
TRAIN_TIMECOURSE_END <- 14.9

# Subset to test

# Subset to train
timecourse_train <- train_data %>%
  group_by(age_grp, object_type, clip_type, type,
           window_type,Time,trial,aoi) %>%
  summarise(n = n()) %>%
  filter(aoi != "NA") %>%
  mutate(n = n / sum(n)) %>%
  group_by(age_grp,trial.type,clip.type,learn.type,trial.num,
           look,name2,reach,contact,end,time.step,aoi,add=FALSE) %>%
  filter(aoi == "Target" | aoi == "Competitor" | aoi== "Face") %>%
  # | aoi == "Hand" | aoi == "Other Face"
  summarise_each(funs(na.mean,sem),n) %>%
  rename(prop = na.mean) %>% #na.mean causes problem with rollapply
  group_by(trial.type,clip.type,learn.type,trial.num,
           look,name2,reach,contact,end,time.step,age.grp,aoi) %>%
  mutate(roll.mean = rollapply(prop,6,FUN=na.mean, partial=TRUE),
         roll.sem = rollapply(sem,6,FUN=na.mean, partial=TRUE)) %>%
  rename(na.mean = prop)


# Compute timecourse for standard analysis
timecourse_test <- test_data %>%
  filter(Time >= TEST_TIMECOURSE_START,
         Time <= TEST_TIMECOURSE_END) %>%
  group_by(age_grp, type, Time, trial) %>%
  summarise(prop = sum(aoi=="Target", na.rm=T)/
              (sum(aoi=="Target", na.rm = T) + sum(aoi=="Competitor", na.rm = T))) %>%
  summarise_each(funs(mean(., na.rm = T), sem(., na.rm = T)), prop) %>%
  rename(prop = mean) %>% #cause problems with rollapply
  mutate(roll_mean = rollapply(prop, 15, FUN = mean, partial = TRUE),
         roll_sem = rollapply(sem, 15, FUN = mean, partial = TRUE)) %>%
  rename(mean = prop)

# Compute side of the screen Ps were on for each test trial
split_type <- test_data %>%
  group_by(age_grp) %>%
  filter(Time == 0) %>%
  mutate(split_type = aoi) %>%
  select(subj, type, trial, split_type)

# Subset test data to just for split analysis
split_data <- left_join(test_data, split_type) %>%
  filter(split_type == "Target" | split_type == "Competitor",
         Time >= 0)

# Compute timecourses for split analysis
split_timecourse <- split_data %>%
  group_by(age_grp, type, split_type, Time, subj, trial) %>%
  summarise(prop = sum(aoi=="Target", na.rm=T)/
              (sum(aoi=="Target", na.rm=T)+sum(aoi=="Competitor", na.rm=T))) %>%
  summarise(prop = mean(prop, na.rm = T)) %>%
  summarise_each(funs(mean(., na.rm = T), sem(., na.rm = T)), prop) %>%
  rename(prop = mean)

split_timecourse[split_timecourse$split_type == "Target","prop"] <- 
  1 - split_timecourse[split_timecourse$split_type == "Target", "prop"] 

split_timecourse %<>%
  mutate(roll_mean = rollapply(prop, 6, FUN = na.mean, partial=TRUE)) %>%
  group_by(age_grp, type, Time) %>%
  mutate(max = max(roll_mean), min = min(roll_mean))

split_timecourse[with(split_timecourse,
                             split_type=="Target" & min != roll_mean),
                    c("min","max")]<- 0

split_timecourse[with(split_timecourse,
                         split_type=="Competitor" & max != roll_mean),
                    c("min","max")]<- 0




