library(grid)
###############################################################################
################################ Make heat maps ###############################
###############################################################################
if(VIDEOS[video] == "soc_word") {
  learn_video <- "s1_1manu"
  
  # SWITCH_SUBJ = "2013_04_30_265"
  # 
  # first_ind = first(which(calib_data$subj == SWITCH_SUBJ))
  # calib_data <- calib_data[(first_ind+1):nrow(calib_data),]
  # 
  #heatmap_data <- calib_data
  #heatmap_aois <- data.frame()
  
  heatmap_data <- filter(learn_data, Stimulus == paste0(learn_video,".avi"))
  heatmap_aois <- filter(train_aois, Stimulus == paste0(learn_video,".avi"))

  times <- sort(unique(heatmap_data$Time))
  frames <- list.files(path = paste0("../stimuli/", VIDEOS[video], 
                                     "/frames/",learn_video), 
                       pattern = '*.jpeg', all.files = FALSE, full.names = TRUE)
} else {
  heatmap_aois <- train_aois
  heatmap_data <- learn_data
  times <- sort(unique(learn_data$Time))
  frames <- list.files(path = paste0("../stimuli/", VIDEOS[video], "/frames"), 
                       pattern = '*.jpeg', all.files = FALSE, full.names = TRUE)
}

#times <- times[1:1441]
#frames <- frames[1:1441]

output_type <- "jpeg" #jpeg or pdf

plot_maps <- function(time, frame) {
  
  VIDEO_HEIGHT = ifelse(VIDEOS[video] == "soc_word", 480, 360)
  
  data <- filter(heatmap_data, Time == time)
  aois <- filter(heatmap_aois, Time == time) %>%
    mutate(x = 0, y = 0)
  
  raw_img <- load.image(frame) %>%
    pad(., 180, "y") %>%save.image("try.jpeg")
  #


  # img <- readJPEG(frame)
  # 
  # img <- img[,120:839,]
  # 
  img <- readJPEG("try.jpeg")
  img <- img[,120:839,]
  # 
  if(output_type == "pdf") {
    pdf(file = paste0("processed_data/heatmaps/", VIDEOS[video], "/",
                      GROUP, "/" , last(unlist(str_split(frame, "/"))),
                      ".pdf"), width=640, height=VIDEO_HEIGHT)
  } else {
    jpeg(file = paste0("processed_data/heatmaps/", VIDEOS[video], "/",
                      GROUP, "/" , last(unlist(str_split(frame, "/"))),
                      ".jpeg"), width=640, height=VIDEO_HEIGHT)
  }

  
  heat_map <- ggplot(aes(x = x, y = Y_MAX - y), data = data) +
    #stat_density2d(aes(fill=..level..),geom="polygon") +
    annotation_custom(rasterGrob(img, width=unit(1, "npc"), height=unit(1, "npc")), 
                      0, X_MAX, 0, Y_MAX) +
    #geom_point(color = "red") +
    stat_density2d(aes(fill=..level..,alpha=..level..), 
                   geom="polygon") +
    scale_alpha(range = c(.01, .25)) +
   # scale_fill_gradient(low="blue", high="green", limits = c(0,.00002))
    scale_fill_gradient(low="blue", high="green", limits = c(0,.0001))
  
  if(nrow(aois) > 0) {
    heat_map <- heat_map + geom_rect(aes(xmin = top_left_x - AOI_BUFFER,# - 280,
                  xmax=bottom_right_x + AOI_BUFFER,# - 280,
                  ymin = Y_MAX - top_left_y + AOI_BUFFER,# - 130, 
                  ymax = Y_MAX - bottom_right_y - AOI_BUFFER),# - 130),
              data = aois, fill=NA, colour="white", size=1) 
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

stats <- ggplot_build(heat_map)
table(stats$data[[2]]$level)


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
