###############################################################################
################## FIGURE 5: LEARNING TRIAL LOOKING IN EXP 1 ##################
###############################################################################

plotting_data_notd <- filter(train_data_age,aoi != "TD") %>%
  group_by(window_type, aoi) %>%
  mutate(window_num = which(levels(window_type) == window_type)) %>%
  ungroup()# %>%
#  mutate(age.grp = factor(age.grp,labels = c(1,2,3,4,"Adult")))

quartz(width=10,height=3,title = "Learning")
ggplot(plotting_data_notd, aes(x = window_num, y = mean, colour = aoi, 
                               group = aoi)) +
  facet_grid(. ~ age_grp) +
  geom_pointrange(aes(ymin = ci_lower,
                      ymax = ci_upper),
                  position = position_dodge(.1),
                  size=.8)+
  geom_line() +
  scale_x_continuous(name = "",breaks=seq(.5,6.5),limits=c(.5,8.2),
                     labels=c("Baseline", "Name", "Look", "Name 2", 
                              "Reach", "Contact", "End"))+
  expand_limits(x = 10) +
  scale_y_continuous(limits = c(0,.8), breaks = seq(0, .8, .1),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none", panel.grid=element_blank()) +
  geom_dl(aes(label = aoi), method = list("last.qp", cex=.75, dl.trans(x=x +.2))) + 
  scale_color_brewer(palette = "Set1") 

###############################################################################
################# FIGURE 6: LEARNING AND TEST PROPS. IN EXP 1 #################
###############################################################################
quartz(width=9,height=7,title = "Test Data")
ggplot(test_data_age, 
       aes(x=video, y=mean,colour=type))+
#  facet_wrap(window.type ~ trial.type) +
  geom_pointrange(aes(ymin = ci_lower,
                      ymax = ci_upper),
                  position = position_dodge(.3),
                  size=.8)+
  geom_hline(aes(yintercept=.5),lty=2)  +
  geom_line(aes(group=type)) +
  scale_x_discrete(name = "Age(years)")+ 
  scale_y_continuous(limits = c(.1,1), breaks=seq(.2,1,.1),
                     name = "Prop. Looks to Target vs. Competitor") +
  theme_bw(base_size=18) + 
  theme(legend.position="none")+#c(.95,.6),legend.title=element_blank()) +
  scale_color_brewer(palette = "Set1") 

plotting.data <- preflook.data %>%
  filter(trial.type != "Learning") %>%
  ungroup() %>%
  mutate(age.grp = factor(age.grp,
                          labels = c(1,2,3,4,"Adult","ASD"))) %>%
  group_by(age.grp,trial.type) %>%
  mutate(type = if(age.grp =="ASD") "atypical"
         else if(age.grp == "Adult") "adult"
         else "typical")

  quartz(width=5,height=4,title = "Test Data")
  ggplot(test_data_age, 
         aes(x=video, y=mean,colour=type))+
    geom_pointrange(aes(ymin = ci_lower,
                        ymax = ci_upper),
                    size=.8, position = position_dodge(.2))+
    geom_hline(aes(yintercept=.5),lty=2)  +
    geom_line(aes(group=c(type))) +
    scale_x_discrete(name = "Age(years)")+ 
    scale_y_continuous(limits = c(.4,1), breaks=seq(.4,1,.1),
                       name = "Prop. Looks to Target vs. Competitor") +
    theme_bw(base_size=14) + 
    theme(legend.position=c(.2,.875),
          legend.title=element_blank(),
          panel.grid=element_blank())+
    scale_color_brewer(palette="Set1")

plotting_data <- train_data_age %>%
  rowwise() %>%
  filter(age_grp != "adult") %>%
  mutate(window_num = which(levels(window_type) == window_type)) %>%
  filter(aoi == "TD") %>%
#   ungroup() %>%
  mutate(age_grp = factor(age_grp,levels = c("child", "ASD"),
                          labels = c("Typical", "ASD")))

quartz(width=8,height=4,title = "Test Data")
ggplot(plotting_data, 
       aes(x = window_num, y = mean, colour = age_grp, group = age_grp))+
  geom_pointrange(aes(ymin = ci_upper,
                      ymax = ci_lower),
                  size=1)+
  geom_hline(aes(yintercept=.5),lty=2)  +
  geom_line() +
  scale_x_continuous(name = "",breaks=seq(.5,6.5),limits=c(.5,6.5),
                     labels=c("Baseline", "Name", "Look", "Name 2", 
                              "Reach", "Contact", "End"))+
  scale_y_continuous(limits = c(.1,1), breaks=seq(.1,1,.1),
                     name = "Prop. Looks to Target vs. Competitor") +
  theme_bw(base_size=16) + 
  theme(legend.position=c(.11,.74),legend.title=element_blank(),
        panel.grid=element_blank()) +
  #guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values= c("#e41a1c", "#2171b5"))

###########################################
### WORD LEARNING PREDICTION FIGURE

lmer.data <- filter(lmer.data,!is.na(age))
cor(lmer.data.typical$Novel,lmer.data.typical$look.name2,use="complete.obs")

###############################################################################
################# PREDICTION FIGURE #################
###############################################################################
gg_lm.txt <- function(p1,p2)
{
  l <- lm(p2 ~ p1)
  cl <- coef(l)
  paste("r = ",sprintf("%2.2f",sqrt(summary(l)$r.squared)),
             getstars(anova(l)$"Pr(>F)"[1]),sep="")
}

quartz(height=4,width=8)
layout(matrix(1:2, 1, 2, byrow = TRUE))
plot(lm.data$age,lm.data$prop.Novel,pch=as.numeric(lm.data$age.grp),
     bty="n",ylim=c(0,1),xlim=c(1,5),yaxp=c(0,1,4),xpd="n",
     ylab="Prop. Looks to Target",xlab="Age (years)")
lm.txt(lm.data$age,lm.data$prop.Novel,x=3,yoff=.4,c="red",lt=1)

plot(lm.data$look.name2,lm.data$prop.Novel,#pch=as.numeric(lm.data$age.grp),
     ylim=c(0,1),xlim=c(.4,.8),yaxp=c(0,1,4),xpd="n",
     ylab="Prop. Looks to Target",xlab="Referential Looking")
lm.txt(lm.data$prop.look.name2,lm.data$prop.Novel,x=.4,yoff=.4,c="red",lt=1)
legend(.15,.45,pch=1:4,c("1-2 years","2-3 years",
                         "3-4 years","4-5 years"),bty="n",xpd="n")

qplot(prop.look.name2,prop.Novel,data=lm.data) + geom_smooth(method="lm")

quartz(height=4,width=8)
layout(matrix(1:2, 1, 2, byrow = TRUE))
plot(mss_hard$age,mss_hard$test_prop,pch=mss_hard$age.grp,bty="n",
     ylim=c(0,1),xlim=c(1,5),yaxp=c(0,1,4),xpd="n",
     ylab="Brief Look learning",xlab="Age (years)")
lm.txt(mss_hard$age,mss_hard$test_prop,x=1.5,yoff=.4,c="red",lt=1)

plot(mss_hard$train_prop,mss_hard$test_prop,pch=mss_hard$age.grp,bty="n",
     ylim=c(0,1),xlim=c(-.1,.65),yaxp=c(0,1,4),xpd="n",
     ylab="Brief Look learning",xlab="Referential Looking")
lm.txt(mss_hard$train_prop,mss_hard$test_prop,x=.05,yoff=.35,c="red",lt=1)
legend(.35,.40,pch=1:4,c("1-2 years","2-3 years","3-4 years","4-5 years"),bty="n",xpd="n")


dev.off()

