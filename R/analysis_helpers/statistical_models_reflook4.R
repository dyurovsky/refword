###############################################################################
################################# T-STATISTICS ################################
###############################################################################
dependents <- test_data_subj %>%
 # filter(age_grp == "ASD") %>%
  filter(age_grp != "ASD", age_grp != "adult") %>%
   spread(type, prop) 

independents <- train_data_subj %>%
#  filter(age_grp == "ASD") %>%
  filter(age_grp != "ASD", age_grp != "adult") %>%
  spread(window_type,prop) %>%
  filter(aoi == "TD")

model_df <- left_join(dependents, independents) %>%
  filter(!is.na(Novel), !is.na(look_name2))

lm_age <-  lm(Novel ~  age, data = model_df)

model_df$resid <- lm_age$residuals

lm_look <- lm(resid ~  look_name2 ,
         data = model_df)

model_df$predict <- predict(lm)

#ggcorplot(model_df)
with(model_df,cor.test(Novel,predict))



#Make a table of t, df, and p-vals
ts <- ttest.data %>%
  group_by(age.grp,window.type) %>%
  summarise( t = t.test(prop,mu=.5)$statistic,
             df = t.test(prop,mu=.5)$parameter,
             p.val = t.test(prop,mu=.5)$p.value,
             prop = na.mean(prop))

# Age-related differences in learning trials
typical.learn.data <- filter(train.data.subj.td,
                             trial.type=="Learning", 
                             !age.grp %in% c("adult","ASD")) %>%
  mutate(age.grp = as.numeric(age.grp))

learning.lm <- lmer(prop ~ age * window.type + (1|subj),
                  data = typical.learn.data)

summary(learning.lm)

# Age-related differences in test trials
typical.test.data <- filter(test.data.subj,
                             trial.type!="Learning", 
                             !age.grp %in% c("adult","ASD")) %>%
  mutate(age.grp = as.numeric(age.grp))

test.lm <- lm(prop ~ age+trial.type, data = typical.test.data)
summary(test.lm)

# Age-related differences in test trials
typical.test.data <- filter(test.data.subj,
                            trial.type!="Learning", 
                            !age.grp %in% c("adult","ASD")) %>%
  mutate(age.grp = as.numeric(age.grp))

typical.learn.target <- filter(train.data.subj.t,
                             trial.type=="Learning", 
                             !age.grp %in% c("adult","ASD")) %>%
  mutate(age.grp = as.numeric(age.grp))

# Predicting test from training
typical.lmer.data <- spread(typical.test.data,trial.type,prop) %>%
  left_join(spread(typical.learn.target,window.type,prop)) %>%
  select(-trial.type) %>%
  filter(!is.na(look.name2))
  
mediate.lm <- lm(Novel ~ age + look.name2 + reach.contact, data=typical.lmer.data)


###############################################################################
############################# MIXED-EFFECTS MODELS ############################
###############################################################################

### EXPERIMENT 1 ###


#for kids only
lmer.data.typical <- filter(lmer.data,age.grp != "adult", age.grp != "ASD")
lmer.data.adult <- filter(lmer.data,age.grp == "adult")
lmer.data.asd <- filter(lmer.data,age.grp == "ASD")

lmer.data.subset <- filter(lmer.data.typical,
                           !is.na(baseline),
                           !is.na(name.look),
                           !is.na(look.name2),
                           !is.na(reach.contact),
                           !is.na(contact.end),
                           !is.na(Novel))


lmer.data.asd.subset <- filter(lmer.data.asd,
                           !is.na(look.name2),
                           !is.na(Novel))


# lm1 <- lmer(Novel ~  baseline + name.look + look.name2 + 
#               reach.contact + contact.end + (1 |subj) + (1|referent) ,
#    data=lmer.data.subset)
# 
 lm2 <- lmer(Novel ~ age + (1|subj) + (1|referent),
             data=lmer.data.subset)


lm1 <- lmer(Novel ~  baseline + name.look + look.name2 + 
              reach.contact + contact.end + (1|subj) + (1|referent),
            data=lmer.data.subset)
# qplot(lmer.data.subset$Novel)

lmer.data.subset$preds.wl <-predict(lm1)
lmer.data.subset$preds.age <- predict(lm2)
with(lmer.data.subset,cor(Novel,preds.wl))
with(lmer.data.subset,cor(Novel,preds.age))

lm2 <- glmer(Novel ~ look.name2 + (1|subj),
             data=lmer.data.adult,family="binomial")

lm3 <- lmer(Novel ~ look.name2 + (1|subj),
             data=lmer.data.asd.subset)


lm3 <- lmer(Novel ~ baseline + name.look + look.name2 + 
            reach.contact + contact.end + (1 |subj) + (1|referent),
          data=lmer.data.asd.subset)

lmer.data.asd.subset$preds.wl <-predict(lm3)
lmer.data.subset$preds.age <- predict(lm2)
with(lmer.data.asd.subset,cor.test(Novel,preds.wl))
with(lmer.data.subset,cor(Novel,preds))


lmer.data.asd.subset %<>% 
  mutate(age.grp = factor(split.ages(age)))

quartz(width=4.5,height=4,title = "Test Data")
ggplot(lmer.data.asd.subset, 
       aes(x=preds.wl, y=Novel,shape=age.grp,group=1))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,colour="darkred",size=1)+
  scale_x_continuous(limits = c(.35,.75), breaks=seq(.3,.7,.1),
                     name = "Referential Looking") +
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.1),
                     name = "Looks to Target vs. Competitor") +
  theme_bw(base_size=18) + 
  theme(legend.position="none") + 
  scale_shape(solid=FALSE)+
  annotate(geom="text",x = .45, y = .8,size=5,
           label = gg_lm.txt(lmer.data.asd.subset$preds.wl,
                             lmer.data.asd.subset$Novel))



quartz(width=4.5,height=4,title = "Test Data")
ggplot(lmer.data.asd.subset, 
       aes(x=age, y=Novel,shape=age.grp,group=1))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,colour="darkred",size=1)+
   scale_x_continuous(limits = c(2,6),name = "Age") +
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.1),
                     name = "Looks to Target vs. Competitor") +
  theme_bw(base_size=18) + 
  theme(legend.position="none") + 
  scale_shape(solid=FALSE)+
  annotate(geom="text",x = 3, y = .8,size=5,
           label = gg_lm.txt(lmer.data.asd.subset$age,
                             lmer.data.asd.subset$Novel))


#Model Reported in Experiment 1
lm2 <- lm(Novel ~  look.name2, data = lmer.data)
lmer.data$wl.pred <- predict(lm2,newdata=lmer.data)

lm3 <- lm(prop.Novel ~ prop.baseline + prop.name.look + prop.look.name2 + 
            prop.name2.reach + prop.reach.contact + prop.contact.end,
          data = lm.data)
lm.data$wl.pred <- predict(lm3,newdata=lm.data)

e1.lm3 <- glmer(prop ~ age * trial.type + (1 | subj), 
                family="binomial", data = lmer.data.e1)
anova(e1.lm2,e1.lm3)

### EXPERIMENTS 1 AND 2 ###
e1and2.lm1 <- glmer(prop ~ age + exp + (1 | subj), 
                    family="binomial", data = lmer.data.e1and2)

e1and2.lm2 <- glmer(prop ~ age + exp + trial.type + (1 | subj),
                    family="binomial", data = lmer.data.e1and2)

anova(e1and2.lm1,e1and2.lm2)

e1and2.lm3 <- glmer(prop ~ age + exp + trial.type + Familiar + (1 | subj),
                    family="binomial", data = lmer.data.e1and2)

anova(e1and2.lm2,e1and2.lm3)

#Model reported in Table 1
e1and2.lm4 <- glmer(prop ~ age + exp*trial.type + Familiar + (1 | subj), 
                    family="binomial", data = lmer.data.e1and2)

anova(e1and2.lm3,e1and2.lm4)

###############################################################################
################################# PAPER TABLES ################################
###############################################################################
#TABLE 1
e1and2.tab <- as.data.frame(summary(e1and2.lm4)$coef)
e1and2.tab$Predictor <- c("Intercept","Age(years)","NonSalient",
                          "Salient","Learning","ME","Familiar",
                          "Non-Salient*Learning","Salient*Learning",
                          "Non-Salient*ME","Salient*ME")
e1and2.tab <- e1and2.tab[c(1,2,7,4,3,5,6,9,8,11,10),]
rownames(e1and2.tab) <- NULL
e1and2.tab <- e1and2.tab[,c(5,1:4)]
e1and2.tab$stars <- sapply(e1and2.tab[,5],getstars)
names(e1and2.tab)[6] <- ""

names(e1and2.tab)[4:5] <- c("$z$ value","$p$ value")

print(xtable(e1and2.tab,
             align = c("l","l","r","r","r","r","l"),
             label = "tab:model_table"),
      include.rownames=FALSE,hline.after=c(0,nrow(e1and2.tab)),
      sanitize.text.function=function(x){x})