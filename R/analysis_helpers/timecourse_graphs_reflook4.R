###############################################################################
################ FIGURE 2: LEARNING TRIAL TIMECOURSE IN EXP 1 #################
###############################################################################
quartz(width=12,height=6,title = "Train Looking")
ggplot(filter(timecourse_train,learn_type=="novel"), 
       aes(x=time_step, y=na.mean, colour=age_grp, fill = age_grp))+
  facet_grid(trial_num ~ aoi) +
  geom_ribbon(aes(ymin = roll_mean_roll.sem,
                  ymax = roll_mean_roll.sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_vline(aes(xintercept=look),lty=1) +
  geom_vline(aes(xintercept=name2),lty=2) +
  geom_vline(aes(xintercept=reach),lty=1) +
  geom_vline(aes(xintercept=contact),lty=2) +
  scale_x_continuous(limits = c(-2,15),breaks=seq(-2,15,1),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size=14) + #theme(legend.position=c(.5, .5),
  #       legend.direction = "horizontal") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_color_manual(name="Age Group",values=man_cols) +
  scale_fill_manual(name="Age Group", values=man_cols)

###############################################################################
################ FIGURE 2: LEARNING TRIAL TIMECOURSE IN EXP 1 #################
###############################################################################
quartz(width=11,height=4,title = "Train Looking")
ggplot(filter(timecourse.train,learn.type=="familiar"), 
       aes(x=time.step, y=na.mean, colour=age.grp, fill = age.grp))+
  facet_grid(trial.num ~ aoi) +
  geom_ribbon(aes(ymin = roll.mean-roll.sem,
                  ymax = roll.mean+roll.sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_vline(aes(xintercept=look),lty=1) +
  geom_vline(aes(xintercept=name2),lty=2) +
  geom_vline(aes(xintercept=reach),lty=1) +
  geom_vline(aes(xintercept=contact),lty=2) +
  scale_x_continuous(limits = c(-2,15),breaks=seq(-2,15,1),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size=14) + #theme(legend.position=c(.5, .5),
  #       legend.direction = "horizontal") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_color_manual(name="Age Group",
                     values=man_cols,breaks=c("4","3","2","1")) +
  scale_fill_manual(name="Age Group",
                    values=man_cols,breaks=c("4","3","2","1"))


###############################################################################
################### FIGURE 3: TEST TRIAL TIMECOURSE IN EXP 1 ##################
###############################################################################
quartz(width=6,height=4,title = "Test Looking")
# timecourse.test$age.grp <- factor(timecourse.test$age.grp,
#                                   labels = c("1", "2", "3", "4", "A", "ASD"))

ggplot(filter(timecourse_test,Time == round(Time, 2), 
              age_grp %in% c("1", "2", "3"),
              type == "Novel"),
       aes(x=Time, y= roll_mean, 
           colour=age_grp, fill = age_grp))+
 # facet_grid(. ~ type) +
  geom_ribbon(aes(ymin = roll_mean-roll_sem,
                  ymax = roll_mean+roll_sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_hline(aes(yintercept=.5),lty=2)  +
  scale_x_continuous(limits = c(-1,4.5),breaks=seq(-2,4,1),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(.3,.75), breaks=seq(.3,.7,.1),
                     name = "Proportion Looking to Target") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_color_manual(name="Age Group",
                     values=man_cols) +
  scale_fill_manual(name="Age Group",
                    values=man_cols) +
  theme_bw(base_size = 16)+
  theme(legend.position="none", panel.grid = element_blank(),
        axis.title.x=element_text(vjust=-.5), axis.title.y=element_text(vjust=1))+
  geom_dl(aes(label=age_grp),method=list("last.qp",cex=1.5,dl.trans(x=x +.2)))
###############################################################################
################## FIGURE 4: ONSET-CONTINGENT PLOT FOR EXP 1 ##################
###############################################################################
# split.timecourse$age.grp <- factor(split.timecourse$age.grp,
#                                   labels = c("1", "2", "3", "4", 
#                                              "Adult", "ASD"))

quartz(width=8.5, height = 5, title = "Test Looking")
ggplot(split_timecourse, aes(x = Time, y = roll_mean, 
                               colour = type, fill = type,
                               linetype = split_type))+
 # facet_grid(trial.type ~ age.grp) +
  facet_grid(. ~ type) +
  geom_line(size = .8) +
  geom_hline(aes(yintercept=.5),lty=2)+
  geom_ribbon(aes(ymin = min,ymax = max), fill="gray", alpha=.2, 
              colour=NA) +
  scale_x_continuous(limits = c(0,TEST_TIMECOURSE_END),
                     breaks=seq(-1,TEST_TIMECOURSE_END),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Switch") +
  theme_bw(base_size=14) + 
  theme(legend.position=c(.08,.915), panel.grid = element_blank()) +
  guides(colour=FALSE,linetype=guide_legend(title=NULL)) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_linetype_discrete(name="Split Type")



