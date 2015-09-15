###############################################################################
############################# SPLIT TRAIN AND TEST ############################
###############################################################################
#summarize across individual trials
test.data.subj <- clean.test.data %>% 
  filter(Time >=TEST_START, 
         Time <= TEST_END) %>%
  group_by(type,age.grp,subj,trial)%>%
  summarise(
    prop = na.sum(aoi=="Target")/(na.sum(aoi=="Target")+
                                         na.sum(aoi=="Competitor"))) %>%
  summarise(prop = na.mean(prop))

train.data.subj <- clean.train.data %>%
  group_by(object_type,window.type,age.grp,subj,trial) %>%
  summarise(
    Target = na.mean(aoi=="Target"),
    Face = na.mean(aoi=="Face"),
    Competitor = na.mean(aoi=="Competitor"),
    TD = na.sum(aoi=="Target")/(na.sum(aoi=="Target")+na.sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean),Target,Face,Competitor,TD) %>%
  gather(aoi,prop,Target:TD) %>%
  group_by(age.grp,window.type,aoi) 

test.data.age <- multi_boot_standard(test.data.subj,column = "prop", na.rm = T)


train.data.age <- multi_boot_standard(train.data.subj,column="prop", na.rm = T)

# 
# #Just nov trials
train.data.novel <- filter(train.data, trial.num %in% c(1,2,4,5,7,8)) %>%
  mutate(referent = ifelse(trial.num %in% c(1,4,8),"fep","toma"))

test.data.novel <- filter(test.data,trial.type=="Novel") %>%
  select(-trial.type) %>%
   mutate(referent = ifelse(trial.num %in% c(2,3), "fep","toma")) %>%
   group_by(referent,add=TRUE) %>%
   summarise(prop = na.mean(prop))
# 
# 
# #summarize by subject
# train.data.subj <- train.data %>%
#  # group_by(referent,add=TRUE) %>%
#   summarise_each(funs(na.mean),(-trial.num)) %>%
#   gather(aoi,prop,Target:TD)
# 
# train.data.subj.td <- train.data.subj %>%
#   filter(aoi=="TD") %>%
#   select(-aoi)
# 
# train.data.subj.t <- train.data.subj %>%
#   filter(aoi=="Target") %>%
#   select(-aoi)
# 
# train.data.subj.f <- train.data.subj %>%
#   filter(aoi=="Face") %>%
#   select(-aoi)
# 
# train.data.subj.td.and.f <- train.data.subj %>%
#   filter(aoi=="Face" | aoi=="TD")

#aggregate by trial.type
train.data.trial <- train.data.subj %>%
  group_by(object_type,window.type,age.grp,aoi) %>%
  multi_boot_standard(., column = "prop", na.rm = T)
###############################################################################
######################## SUBSET DATA FOR ANALYSES BELOW #######################
###############################################################################

train.data.td <- train.data.trial %>%
  filter(aoi=="TD") %>%
  select(-aoi)

train.data.t <- train.data.trial %>%
  filter(aoi=="Target") %>%
  select(-aoi)

train.data.notd <- filter(train.data.trial,aoi != "TD")

test.data.trial$window.type <- "test"

preflook.data <- bind_rows(train.data.td,test.data.trial)


ttest.data <- test.data.subj %>%
  mutate(window.type = as.character(trial.type)) %>%
  select(-trial.type)

ttest.data = bind_rows(ttest.data,
                   train.data.subj.td %>%
                     mutate(window.type = as.character(window.type)) %>%
                     select(-trial.type)) %>%
  mutate(window.type = factor(window.type,
                              levels=c(levels(train.data.subj$window.type),
                                       "Familiar","Novel")))

#Demographic data
demo.data <- test.data.subj %>%
  group_by(age.grp,type) %>%
  summarise(n = n(),
            num.girls = sum(gender=="Female")) %>%
  filter(trial.type=="Familiar") %>%
  select(-trial.type)
# 
# quartz(width=5,height=4)
# ggplot(filter(test.data.subj,age.grp == "ASD"), aes(x=age))+
#   geom_bar(binwidth=.5,fill="white", color="black") +
#   scale_x_continuous(name = "Age",limits=c(2,6)) + 
#   scale_y_continuous(limits = c(0,10), breaks=seq(0,10,5),
#                      name = "Number of Children") +
#   theme_bw(base_size=18) + theme(legend.position="none")

