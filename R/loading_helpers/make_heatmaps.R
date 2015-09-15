
###############################################################################
################################ Make heat maps ###############################
###############################################################################

# Learning Heatmap
learn.heatmap.data <- zero.times(filter(adjusted.data,Stimulus==LEARN.STIM),
                         LEARN.STIM.LENGTH,split=FALSE)

times <- sort(unique(learn.heatmap.data$Time))
frames <- list.files(path = "../stimuli/reflook/images/reflook4_4", pattern = '*.jpeg',
                     all.files = FALSE)

plot.maps <- function(time,frame) {
  data <- filter(learn.heatmap.data,Time == time)
  aois <- filter(train.aois,Time == time) %>%
    mutate(x = 0,y=0)
  
  img <- readJPEG(paste0("../stimuli/reflook/images/reflook4_4/",frame))
  
  pdf(file = paste0("processed_data/heatmaps/reflook4/",
                    GROUP,"/",frame,".pdf"),width=640,height=360)
  
  ggplot(aes(x = x, y =Y.MAX - y), data = data) +
    #stat_density2d(aes(fill=..level..),geom="polygon") +
    annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                      0, X.MAX, 0, Y.MAX) +
    stat_density2d(aes(fill=..level..,alpha=..level..), geom="polygon") +
    scale_fill_gradient(low="blue", high="green") +
   # geom_point()+
    geom_rect(aes(xmin = top.left.x-AOI.BUFFER,
                  xmax=bottom.right.x+AOI.BUFFER,
                  ymin = Y.MAX-top.left.y+AOI.BUFFER,ymax=Y.MAX-bottom.right.y-AOI.BUFFER),
              data = aois,fill=NA,colour="white", size=50) +
    scale_x_continuous(limits=c(0,X.MAX),expand=c(0,0))+
    scale_y_continuous(limits=c(0,Y.MAX),expand=c(0,0)) +
    theme_bw() +
     theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank(),
           plot.margin = unit(c(0,0,0,0), "cm"))

  dev.off()
}

plots <- mapply(plot.maps,times[605:605],frames[600:600])

plot.test.map <- function(time) {
  
  stim.data <- filter(stim.data,Time==times[time])
  
  ggplot(aes(x = x, y =Y.MAX - y), data = stim.data) +
    #stat_density2d(aes(fill=..level..),geom="polygon") +
    annotation_custom(rasterGrob(stim.img, width=unit(1,"npc"), 
                                 height=unit(1,"npc")), 
                      8, X.MAX-8, 5, Y.MAX-5) +
    stat_density2d(aes(fill=..level..,alpha=..level..), geom="polygon") +
    scale_fill_gradient(low="blue", high="red") +
    scale_x_continuous(limits=c(0,X.MAX),expand=c(0,0))+
    scale_y_continuous(limits=c(0,Y.MAX),expand=c(0,0)) +
    theme(line=element_blank(),text=element_blank(),title=element_blank(),
          legend.position="none") +
    ggsave(paste0(stim.dir,"/",trial,"_",time,".pdf"))
}

fam.tests <- unique(fam.data$Stimulus)

for(stim in fam.tests) {
  
  stim.data <- filter(fam.data,Stimulus == stim)
  stim.img <- readJPEG(paste0("../stimuli/reflook/images/tests/",stim))
  
  stim.name <- sub(".jpg","",stim)
  stim.dir <- paste0('processed_data/heatmaps/tests/',stim.name)
  dir.create(stim.dir,showWarnings = FALSE)
  
  times <- sort(unique(stim.data$Time))
  
  plots <- sapply(1:length(times),plot.test.map)
}

nov.tests <- unique(nov.data$Stimulus)

for(stim in nov.tests) {
  stim.img <- readJPEG(paste0("../stimuli/reflook/images/tests/",stim))
  
  stim.name <- sub(".jpg","",stim)
  stim.dir <- paste0('processed_data/heatmaps/tests/',stim.name)
  dir.create(stim.dir,showWarnings = FALSE)
  
  for(trial in c(1,2)) {
    
    stim.data <- filter(nov.data,Stimulus == stim,trial==trial)
    times <- sort(unique(stim.data$Time))
    
    plots <- sapply(1:length(times),plot.test.map)
    
  }
}
