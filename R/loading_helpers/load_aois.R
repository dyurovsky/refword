get_test_aois <- function() {
  test_aoi_xml <- xmlToList('processed_data/aois/test_aois/o_book_dog (AOIs).xml')
  
  aoi_names <- c("right","left")
  make_test_aoi = function(aoi_num) {
    aois <- data.frame(Time = seq(0, TEST_LENGTH, (1 / FRAME_RATE))) %>%
      mutate(aoi_name = aoi_names[aoi_num],
             top_left_x = as.numeric(test_aoi_xml[aoi_num]$
                                       DynamicAOI$Points[1]$Point$X),
             top_left_y = as.numeric(test_aoi_xml[aoi_num]$
                                       DynamicAOI$Points[1]$Point$Y),
             bottom_right_x = as.numeric(test_aoi_xml[aoi_num]$
                                           DynamicAOI$Points[2]$Point$X),
             bottom_right_y = as.numeric(test_aoi_xml[aoi_num]$
                                           DynamicAOI$Points[2]$Point$Y))
  }
  
  bind_rows(lapply(1:length(aoi_names), make_test_aoi))
}

get_train_aois <- function(video) {
   train_aoi_xml <- xmlToList(paste0("processed_data/aois/train_aois/",
                                     video, "_aois.xml"))
   
   if(video == "reflook") {
     train_aois <- train_aoi_xml[sapply(1:length(train_aoi_xml), function(x) {
       str_count(train_aoi_xml[x]$DynamicAOI$Name,"M1_") > 0 | 
         str_count(train_aoi_xml[x]$DynamicAOI$Name,"M2_") > 0 | 
         str_count(train_aoi_xml[x]$DynamicAOI$Name,"D1_") > 0 | 
         str_count(train_aoi_xml[x]$DynamicAOI$Name,"D2_") > 0}, simplify = TRUE)]
   }
  
  
   train_aois <- train_aoi_xml
   
  make_aoi_keyframe = function(aoi, key_frame) {
    key_frame_df <- aoi$KeyFrames[key_frame]$KeyFrame
    if(key_frame_df$Visible == "false") {
      data.frame(Time = as.numeric(key_frame_df$Timestamp)/1000000,
                 top_left_x = NA,
                 top_left_y = NA,
                 bottom_right_x = NA,
                 bottom_right_y = NA)
    }
    else {
      data.frame(Time = as.numeric(key_frame_df$Timestamp)/1000000,
                 top_left_x = as.numeric(key_frame_df$Points[1]$Point$X) * SCALE_X,
                 top_left_y = as.numeric(key_frame_df$Points[1]$Point$Y) * SCALE_Y,
                 bottom_right_x = as.numeric(key_frame_df$Points[2]$Point$X) * SCALE_X,
                 bottom_right_y = as.numeric(key_frame_df$Points[2]$Point$Y) * SCALE_Y)
    }
  }
  
  make_train_aoi <- function(aoi) {
    
    # Get the AOIs for each manually coded keyframe
    aoi_df <- bind_rows(lapply(1:length(aoi$KeyFrames), 
                               function(key_frame) {
                                 make_aoi_keyframe(aoi, key_frame)})) %>%
      mutate(aoi_name = aoi$Name) %>%
      mutate(Time = floor(FRAME_RATE * Time) / FRAME_RATE)
    
    # Find the times between keyframes
    all_times <- seq(min(aoi_df$Time),max(aoi_df$Time), 1 / FRAME_RATE)
    
    #Drop the last frame if it was the invisible point
    if(is.na(tail(aoi_df,1)$top_left_x)) {
      aoi_df <- aoi_df[1:(nrow(aoi_df) - 1),]
      all_times <- all_times[1:(length(all_times) - 1)]
    }
    
    # Floating point tolerance issue fix for setdiff
    diff_times <- all_times[sapply(all_times, function(x) {
      Reduce(and, abs((aoi_df$Time-x)) >= 1e-5)})]
    
    # Make a NA-filled array for the times between keyframes
    fills <- data.frame(Time = diff_times,
                        aoi_name = aoi_df[1,"aoi_name"],
                        top_left_x = as.numeric(NA),
                        top_left_y = as.numeric(NA),
                        bottom_right_x = as.numeric(NA),
                        bottom_right_y = as.numeric(NA))
    
    # Do constant interpolation to fill in AOIs for these between-times
    all_pts <- bind_rows(aoi_df,fills) %>%
      arrange(Time) 
    
    all_pts %>%
      mutate_each(funs(na.approx(., Time, method = "constant", rule = 2)),
                  top_left_x,top_left_y,bottom_right_x,bottom_right_y)
  }
  
  train_aoi_pts <- bind_rows(lapply(1:length(train_aois), function(aoi_num) {
    make_train_aoi(train_aois[aoi_num]$DynamicAOI)})) %>%
    mutate(Time = floor(FRAME_RATE * Time)/FRAME_RATE) %>%
    arrange(Time)
  
}
