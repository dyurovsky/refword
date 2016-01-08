###############################################################################
############################# SPLIT TRAIN AND TEST ############################
###############################################################################
TEST_START <- .5
TEST_END <- 3.5
TRAIN_START <- 0
TRAIN_END <- 15

#summarize across individual trials
test_data_subj <- test_data %>% 
  filter(Time >=TEST_START, 
         Time <= TEST_END) %>%
  group_by(type, age_grp, subj, trial)%>%
  summarise(
    prop = sum(aoi == "Target", na.rm = TRUE) / 
      (sum(aoi == "Target", na.rm = TRUE)+
         sum(aoi == "Competitor", na.rm = TRUE))) %>%
  summarise(prop = mean(prop, na.rm = TRUE))

train_data_subj <- train_data %>%
  group_by(window_type, age, age_grp, subj, trial) %>%
 # group_by(video, age_grp, subj, trial) %>%
  summarise(
    Target = mean(aoi == "Target", na.rm = T),
    Face = mean(aoi == "Face", na.rm = T),
    Competitor = mean(aoi == "Competitor", na.rm = T),
    TD = sum(aoi == "Target", na.rm = T)/
      (sum(aoi == "Target", na.rm = T) + 
         sum(aoi == "Competitor", na.rm = T))) %>%
  summarise_each(funs(mean(., na.rm = T)), Target, Face, Competitor, TD) %>%
  gather(aoi, prop, Target:TD) %>%
  group_by(age_grp, window_type,aoi) 
  #group_by(video, age_grp, window_type, aoi) 



test_data_age <- multi_boot_standard(test_data_subj, column = "prop", na.rm = T)


train_data_age <- multi_boot_standard(train_data_subj, column="prop", na.rm = T)



# # wide_form_test <- test_data_subj %>%
# #   ungroup() %>%
# #   select(video, subj, type, subj, prop) %>%
# #   unite(test, video, type) %>%
# #   spread(test, prop)
# # 
# # wide_form_train <- train_data_subj %>%
# #   filter(aoi == "TD" | aoi == "Face") %>%
# #   ungroup() %>%
# #   select(video, subj,aoi, prop) %>%
# #   unite(test, video, aoi) %>%
# #   spread(test, prop)
# 
# wide_form_data <- left_join(wide_form_test, wide_form_train)
# 
# write_csv(wide_form_data,"wide_form_asd.csv")

###############################################################################
######################## SUBSET DATA FOR ANALYSES BELOW #######################
###############################################################################

train_data_td <- train_data_subj %>%
  filter(aoi == "TD") %>%
  select(-aoi)

train_data_t <- train_data_subj %>%
  filter(aoi == "Target") %>%
  select(-aoi)

train_data_notd <- train_data_subj %>%
  filter(aoi != "TD") %>%
  select(-aoi)

test_data_subj$window_type <- "test"

preflook_data <- bind_rows(train_data_td, test_data_subj)


#Demographic data
demo_data <- test_data %>%
  distinct(subj) %>%
  group_by(age_grp) %>%
  summarise(n = n(),
            num_girls = sum(gender=="Female",na.rm=T))






%>%
# 
# quartz(width=5,height=4)
# ggplot(filter(test.data.subj,age.grp == "ASD"), aes(x=age))+
#   geom_bar(binwidth=.5,fill="white", color="black") +
#   scale_x_continuous(name = "Age",limits=c(2,6)) + 
#   scale_y_continuous(limits = c(0,10), breaks=seq(0,10,5),
#                      name = "Number of Children") +
#   theme_bw(base_size=18) + theme(legend.position="none")

