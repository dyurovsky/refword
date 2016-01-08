
###############################################################################
################################ Make heat maps ###############################
###############################################################################
times <- sort(unique(learn_data$Time))
frames <- list.files(path = paste0("../stimuli/", VIDEOS[video], "/frames"), 
                     pattern = '*.jpeg',
                     all.files = FALSE)

times <- times[200:250]
frames <- frames[200:250]

plot_maps <- function(time, frame) {
  data <- filter(learn_data, Time == time)
  aois <- filter(train_aois, Time == time) %>%
    mutate(x = 0, y = 0)
  
  img <- readJPEG(paste0("../stimuli/", VIDEOS[video], "/frames/",frame))
  
  pdf(file = paste0("processed_data/heatmaps/", VIDEOS[video], "/",
                    GROUP, "/" ,frame,".pdf"), width=640, height=360)

  
  heat_map <- ggplot(aes(x = x, y = Y_MAX - y), data = data) +
    #stat_density2d(aes(fill=..level..),geom="polygon") +
    annotation_custom(rasterGrob(img, width=unit(1, "npc"), height=unit(1, "npc")), 
                      0, X_MAX, 0, Y_MAX) +
    stat_density2d(aes(fill=..level..,alpha=..level..), 
                   geom="polygon") +
    scale_fill_gradient(low="blue", high="green") 
  
  if(nrow(aois) > 0) {
    heat_map <- heat_map + geom_rect(aes(xmin = top_left_x - AOI_BUFFER,
                  xmax=bottom_right_x + AOI_BUFFER,
                  ymin = Y_MAX - top_left_y + AOI_BUFFER, 
                  ymax = Y_MAX - bottom_right_y - AOI_BUFFER),
              data = aois, fill=NA, colour="white", size=50) 
  }
  
  heat_map <- heat_map + 
    scale_x_continuous(limits=c(0, X_MAX), expand=c(0, 0))+
    scale_y_continuous(limits=c(0, Y_MAX), expand=c(0, 0)) +
    theme_bw() +
    theme(axis.line = element_blank(), axis.text.x = element_blank(),
          axis.text.y=element_blank(), axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), legend.position="none",
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"))
   
  print(heat_map)
  
  dev.off()
}

plots <- mapply(plot_maps, times, frames)

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
