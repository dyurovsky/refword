###############################################################################
################ FIGURE 2: LEARNING TRIAL TIMECOURSE IN EXP 1 #################
###############################################################################
quartz(width=12,height=6,title = "Train Looking")
ggplot(filter(timecourse.train,learn.type=="novel"), 
       aes(x=time.step, y=na.mean, colour=age.grp, fill = age.grp))+
  facet_grid(trial.num ~ aoi) +
  geom_ribbon(aes(ymin = roll.mean-roll.sem,
                  ymax = roll.mean+roll.sem),
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
quartz(width=7.5,height=3,title = "Test Looking")
# timecourse.test$age.grp <- factor(timecourse.test$age.grp,
#                                   labels = c("1", "2", "3", "4", "A", "ASD"))

ggplot(filter(timecourse.test,Time==round(Time,2)),
       aes(x=Time, y=roll.mean, 
           colour=age.grp, fill = age.grp))+
  facet_grid(~ type) +
  geom_ribbon(aes(ymin = roll.mean-roll.sem,
                  ymax = roll.mean+roll.sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_hline(aes(yintercept=.5),lty=2)  +
#   scale_x_continuous(limits = c(-1,4.75),breaks=seq(-2,4,1),
#                      name = "Time(s)") + 
#   scale_y_continuous(limits = c(.25,1), breaks=seq(.25,1,.25),
#                      name = "Prop. Looks to Target") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_color_manual(name="Age Group",
                     values=man_cols) +
  scale_fill_manual(name="Age Group",
                    values=man_cols) +
  theme(legend.position="none")+
  geom_dl(aes(label=age.grp),method=list("last.qp",cex=1.2,dl.trans(x=x +.2)))
###############################################################################
################## FIGURE 4: ONSET-CONTINGENT PLOT FOR EXP 1 ##################
###############################################################################
split.timecourse$age.grp <- factor(split.timecourse$age.grp,
                                  labels = c("1", "2", "3", "4", 
                                             "Adult", "ASD"))

quartz(width=8.5,height=5,title = "Test Looking")
ggplot(split.timecourse, aes(x=Time, y=roll.mean, 
                               colour=type, fill = type,
                               linetype=split.type))+
 # facet_grid(trial.type ~ age.grp) +
  facet_wrap(~ type) +
  geom_line(size=.8) +
  geom_hline(aes(yintercept=.5),lty=2)+
  geom_ribbon(aes(ymin=min,ymax=max),fill="gray",alpha=.2, 
              colour=NA) +
  scale_x_continuous(limits = c(0,TEST_TIMECOURSE_END),
                     breaks=seq(-1,TEST_TIMECOURSE_END),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Switch") +
  theme_bw(base_size=14) + theme(legend.position=c(.08,.915)) +
  guides(colour=FALSE,linetype=guide_legend(title=NULL)) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_linetype_discrete(name="Split Type")



