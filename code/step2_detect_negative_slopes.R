# Author:Krista Kraskura
# Last update/run: April 28  2025
# Title: Respo analysis, facilitating functions. 
# 
# ******************************************
# STEP 2 in analysis flow 

# *******************************************
# *******************************************
# Load necessary libraries
library(tidyverse)
library(pracma) # for changepoints findpeaks() function
library(here)
library(readxl)

# Outputs from file respirometry_analysis_convert.R
converted.files<-list.files(path = here("csv_files/"), pattern = ".csv")
converted.files<-converted.files[grepl("lag",converted.files)&!grepl("empty",converted.files)]

# Define the function to detect negative slopes -----
#' Title
#'
#' @param data 
#' @param data_name 
#' @param min_duration 
#' @param min_r_squared 
#' @param start_cut_slope 
#' @param end_cut_slope 
#' @param data_start_exclude the number of minutes to exclude from the entire data minutes
#' @param data_duration entire data minutes
#' @param temp_spline_spar 
#' @param added_inflection_point_min added inflection points that the second derivative 
#' @param smooth_data option 1 = smooth with loess and peaks found by 2nd deriv (default); 2 = manual values, but must provide a slopes_start and slopes_end vectors; if neither provided, then all data are used.   
#' @param slopes_start vector of values for each slopes start minutes 
#' @param slopes_start vector of values for each slopes end minutes (must the same length as slope_start)

#' @returns
#' @export
#'
#' @examples
detect_negative_slopes <- function(data,
                                   data_name,
                                   min_duration,
                                   data_duration,
                                   min_r_squared = NULL, 
                                   start_cut_slope = 0, 
                                   end_cut_slope = 0,
                                   data_start_exclude = 0, 
                                   temp_spline_spar = 0.3,
                                   added_inflection_point_min = 0,
                                   smooth_data = 3, 
                                   slopes_start = NA,
                                   slopes_end = NA) {
  
  # Initialize an empty list to store the results
  results <- list()

  # Cut each data file/recording to be no longer than 'data_duration' length in minutes
  data<-data[data$time_min >= data_start_exclude,] 
  # Cut each data file/recording to exclude 'data_start_exclude' length in minutes
  data<-data[data$time_min <= data_duration,] 

  # smootheing option "1" = loess 
  if(smooth_data == 1){
    message("smoothing data using loess")
    # Smooth the data *******************
    # considered smoothing options:
    # smooth_temp_data<-rollapply(data$Ch1_temp, width=150, FUN=mean, align = "center", fill=NA)
    # smooth_temp_data<-predict(smooth.spline(x = data$time_min, y = data$Ch1_temp, spar = temp_spline_spar))$y
    smooth_temp_data<-loess(data$Ch1_temp~data$time_min, span=temp_spline_spar)$fitted
  
      if(!is.na(added_inflection_point_min)){
        added_flipoint_timemin<-added_inflection_point_min
        added_flipoint_rows<-which(data$time_min==round(added_flipoint_timemin))
      }else{
        added_flipoint_timemin<-NA
        added_flipoint_rows<-NA
      }
      
      # 2nd derivative for flip points 
    r <- rle(smooth_temp_data)
    sorted_flip_rows<-sort(c(added_flipoint_rows,
                       c(1, # add first datapoint
                        which(rep(x = abs(diff(sign(diff(c(-Inf, r$values, -Inf))))) == 2,
                        times = r$lengths)),
                        nrow(data)))) # add last datapoint
    sorted_flip_times<-sort(c(added_flipoint_timemin, data$time_min[sorted_flip_rows]))

    # the vectors of start and end times for each segment for slope estimations. 
    slope_times_start<-c(1:length(sorted_flip_times)-1)
    slope_times_end<-c(2:length(sorted_flip_times))
    
    # Flip points from 'pracma' package ****************************
    ## considered peak detecing functions 
    # flip_points<-findpeaks(smooth_temp_data, nups = 1, ndowns = 1)
    # peak_min<-data$time_min[flip_points[, 2]] # The time of the peak
    # high_min<-data$time_min[flip_points[, 3]] # The time of when the peaks starts
    # low_min<-data$time_min[flip_points[, 4]] # The time of when the peaks ends
    # 
    ## Predicted values and added first and last rows of the data for complete segments
    # sorted_flip_times<-unique(sort(c(data$time_min[1], peak_min, high_min, low_min, data$time_min[nrow(data)])))
    # sorted_flip_rows<-unique(sort(c(1, flip_points[, 2], flip_points[, 3], flip_points[, 4], nrow(data))))
    # end using flip points *******************

  }else if (smooth_data == 2){
    # data not smoothed, the breaks are manually logged and provided  
    message("no smoothing, manually provided values")
    slope_times_start<-c(slopes_start)
    slope_times_end<-c(slopes_end)

  }else if (smooth_data == 3){
    message("no smoothing, no provided slope valies, one slope accross all data")
    slope_times_start<-data$time_min[1] # start the first timestamp
    slope_times_end<-data$time_min[c(nrow(data))]
    
  }else{
    stop("Need to provide a valid method of analysis through 'smooth_data'")
  }
  
  # All values that are NA in the dataframe turn to zero
	# data[is.na(data)] <- 0
  
  # Loop over the length of the values mapping the data segments for slopes
  for(i in 1:length(slope_times_start)){
     
    # Start and end times and corresponding rows for each segment  
    start_time <- as.numeric(slope_times_start[i]) + start_cut_slope
    end_time <- as.numeric(slope_times_end[i]) - end_cut_slope
    
    # Linear regression of temperature data
    model_temp<-lm(data$Ch1_temp ~ data$time_min)
    temp_slope<-coef(model_temp)[2]
    if(temp_slope<0){
      temp_direction <- "down"
    }else{
      temp_direction <- "up"
    }

    # Calculate the duration of the slope
    # Original time in s, convert to min
    duration <- as.numeric(end_time - start_time)
    # print(duration)
    # The first 2min and last 1min are taken away
    
    # Check if the duration is withing the threshold, if not skip the loop
    if (duration >= min_duration) {      
  
      # Data subset withing the timeframe:
      data0<-data[data$time_min >= start_time+start_cut_slope &
                    data$time_min <= end_time-end_cut_slope, ] 
      # take off a minute one each end
      
			# temp recorded for each measurement cycle
			temp_mean<-round(mean(data0[,"Ch1_temp"]),2) 
			temp_max<-round(max(data0[,"Ch1_temp"]),2)
			temp_min<-round(min(data0[,"Ch1_temp"]),2)
			
      # Calculate the slope and R squared using linear regression for each channel
      # Channel 1 
      
      if(!(is.na(data0$Ch1_O2[1]) & (is.na(data0$Ch1_O2[nrow(data0)])))){

        model <- lm(Ch1_O2 ~ time_min, data = data0, na.action = "na.exclude")
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        r_squared <- summary(model)$r.squared
        
        if(is.null(min_r_squared)){
          min_r_squared<-0
        }
        
        # Check if the slope is negative and the R squared is above the threshold
        if (c(r_squared > min_r_squared)) {
          # Store the results
          channel<-"Ch1"
          results[[length(results) + 1]] <- data.frame(paste("min", round(start_time), "_", round(end_time), sep = ""),
                                                       start_time,
                                                       end_time, # take this out after plotting
                                                       r_squared,
                                                       as.numeric(intercept),
                                                       as.numeric(slope),
                                                       temp_min, temp_max, temp_mean,
                                                       channel,
                                                       as.character(data0$DateTime[1]),
                                                       "SMR", duration, temp_slope)
          
        }
      } 
      # Channel 2
      if(!(is.na(data0$Ch2_O2[1]) & (is.na(data0$Ch2_O2[nrow(data0)])))){
        
        model <- lm(Ch2_O2 ~ time_min, data = data0, na.action = "na.exclude")
        slope <- coef(model)[2]
        intercept <- coef(model)[1]
        r_squared <- summary(model)$r.squared
        
        if(is.null(min_r_squared)){
          min_r_squared<-0
        }
        
        # Check if the R squared is above the threshold
        if (c(r_squared > min_r_squared)) {
          # Store the results
          channel<-"Ch2"
          results[[length(results) + 1]] <- data.frame(paste("min", round(start_time), "_", round(end_time), sep = ""),
                                                       start_time,
                                                       end_time, # take this out after plotting
                                                       r_squared,
                                                       as.numeric(intercept),
                                                       as.numeric(slope),
                                                       temp_min, temp_max, temp_mean,
                                                       channel,
                                                       as.character(data0$DateTime[1]),
                                                       "SMR", duration, temp_slope)
        }
      } 
      # Channel 3
      if(!(is.na(data0$Ch3_O2[1]) & (is.na(data0$Ch3_O2[nrow(data0)])))){

        model <- lm(Ch3_O2 ~ time_min, data = data0, na.action = "na.exclude")
        slope <- coef(model)[2]
        intercept <- coef(model)[1]
        r_squared <- summary(model)$r.squared

        if(is.null(min_r_squared)){
          min_r_squared<-0
        }                
        # Check if R squared is above the threshold
        if (c(slope < 0 && r_squared > min_r_squared)) {
          # Store the results
          channel<-"Ch3"
          results[[length(results) + 1]] <- data.frame(paste("min", round(start_time), "_", round(end_time), sep = ""),
                                                       start_time,
                                                       end_time, # take this out after plotting
                                                       r_squared,
                                                       as.numeric(intercept),
                                                       as.numeric(slope),
                                                       temp_min, temp_max, temp_mean,
                                                       channel,
                                                       as.character(data0$DateTime[1]),
                                                       "SMR", duration, temp_slope)
        }
      } 
      # Channel 4
      if(!(is.na(data0$Ch4_O2[1]) & (is.na(data0$Ch4_O2[nrow(data0)])))){

        model <- lm(Ch4_O2 ~ time_min, data = data0, na.action = "na.exclude")
        slope <- coef(model)[2]
        intercept <- coef(model)[1]
        r_squared <- summary(model)$r.squared

        if(is.null(min_r_squared)){
          min_r_squared<-0
        }        
        # Check if R squared is above the threshold
        if (c(r_squared > min_r_squared)) {
          # Store the results
          channel<-"Ch4"
          results[[length(results) + 1]] <- data.frame(paste("min", round(start_time), "_", round(end_time), sep = ""),
                                                       start_time,
                                                       end_time, # take this out after plotting
                                                       r_squared,
                                                       as.numeric(intercept),
                                                       as.numeric(slope),
                                                       temp_min, temp_max, temp_mean,
                                                       channel,
                                                       as.character(data0$DateTime[1]),
                                                       "SMR", duration, temp_slope)
          
          # c("time_frame", "min_start", "min_end","r2", "b", "m", 
          #               "t_min", "t_max", "t_mean" ,"Ch", "DateTime_start",
          #               "type", "n_min", "ID_code") # << in original export files for compatible with MMR_SMR_AS_EPOC function
          # Rename the last row from 'ID_code' to "Temp_slope"
        }
      } 
      
    }else{
      message(paste("cycle too short", duration))
      next # Next if the duration is less than specified
    }
  }
    
  # Visualize the results
  # Figure for oxygen data
  # print(head(data))
  
  # data$spline_predict<-predict(smooth.spline(x = data$time_min, y = data$Ch1_temp, spar = 0.8))$y
  # data$spline_predict<-rollapply(data$Ch1_temp, width=150, FUN=mean, align = "center", fill=NA)
  data$spline_predict<-loess(data$Ch1_temp~data$time_min, span=0.2)$fitted
  
  p_ch<-data %>% 
    dplyr::select(!Ch1_temp) %>% 
    pivot_longer(cols = 4:7) %>% 
    ggplot(aes(x = time_min, y = value, color = name))+
    geom_line(linewidth = 0.4, alpha = 0.2) +
    scale_color_manual(values = c("Ch1_O2" = "#f59432","Ch1" = "#f59432",
                                  "Ch2_O2" = "#009986","Ch2" = "#009986",
                                  "Ch3_O2" = "#805eea","Ch3" = "#805eea",
                                  "Ch4_O2" = "#633e1c","Ch4" = "#633e1c")) +
    theme_bw() +
    scale_x_continuous(breaks=seq(data_start_exclude-1, max(data$time_min), 10))+
    labs(x = "Time, minutes", y = "Oxygen, mg/L")+
    theme(legend.title = element_blank(),
          legend.position = "top")+
    ggtitle(label = data_name)
  
  p_t<-data %>% 
    dplyr::select(time_min, Ch1_temp) %>% 
    ggplot(aes(x = time_min, y = Ch1_temp))+
    geom_line(linewidth = 0.4, color = "black") +
    theme_bw() +
    labs(x = "Time, minutes", y = "Temperature, ºC")+
    theme(legend.position = "none")
  
  if(smooth_data == 2){
  p_t<- p_t + 
    geom_line(data = data, mapping=aes(y = spline_predict, x = time_min), linewidth = 0.6) +
    geom_vline(xintercept = c(slope_times_start),
               col="green4", linetype = 2, linewidth = 0.5, alpha = 0.5)+
    geom_vline(xintercept = c(slope_times_end),
               col="yellow4", linetype = 2, linewidth = 0.5, alpha = 0.5)
  }
  
  # If data is empty stop the function and give message
  if(length(results) == 0){
    message(paste("no data:", data_name))
    return(list("no data"))
    
  }else{
    # Wrangle data outputs to mach the format needed
    results.df<-dplyr::bind_rows(lapply(results, function(x) data.frame(value = x)))
    rownames(results.df)<-1:nrow(results.df)
    names(results.df)<-c("time_frame", "min_start", "min_end","r2", "b", "m", 
                          "t_min", "t_max", "t_mean" ,"Ch", "DateTime_start",
                          "type", "n_min", "Temp_slope")
    
    # Only keep manually selected slope segments
    # results.df<-results.df[which(results.df$Temp_slope>=0), ]
    if(smooth_data == 2){
      p_ch<-p_ch +
      geom_segment(data = results.df, 
                   aes(x = min_start,
                       y = b + m * min_start,
                       xend = min_end,
                       yend = b + m * min_end,
                       color = Ch), linetype = "solid")      
    }
    
    # The entire trend for each channel 
    if(smooth_data > 2 ){
      p_ch<-p_ch +
      geom_smooth(method = "lm", se = FALSE, 
                  mapping = aes(color = name), linewidth = 0.7)+
      ggtitle(label = data_name, 
              subtitle = paste("n slopes detected = ", nrow(results.df), 
                                  "\  lowest R2 = ",
                                  round(min(results.df$r2), 2), sep = ""))
    }
    
  
    
    plot<-cowplot::plot_grid(p_ch, p_t, ncol = 1, rel_heights = c(1, 0.5)) 

    # Turn the results in the desirable format for MMR_SMR_AS_EPOC function
    results.df<-results.df %>% 
      dplyr::select(!min_end) 
    
    # sanitiy check 
    if(length(start_times0) == length(unique(results.df$time_frame))){
      message("YEY!")
    }else{
      message("Ney...")
    }
    
    # Return the results
    return(list(results.df, plot))
  }
}



# **************************************************
# Loop to analyze/detect negative slopes in all files --------
# Output saved in format compatible to MMR_RMR_AS_EPOC format. 
summaryTable<-data.frame("dataname" = NA,
                         "time_min" = NA,
                         "NArows" = NA,
                         "min_temp" = NA,
                         "max_temp" = NA, 
                         "Run" = NA)
# logged slopes
d2<-readxl::read_xlsx(here("ARCHIVE/log_slope_sections.xlsx"), sheet = 1)
  

# no empties 
for(k in 21:length(converted.files)){

  # exclude non-reliable data, using notes as guidance
  # if(converted.files[k] == "2023-12-06_120950_120623_23C_B12A13nono_cory_A_converted.csv"){next}
  # if(converted.files[k] == "2023-12-06_152338_120623_23C_B12A13nono_tile_A_converted.csv"){next}
  if(converted.files[k] == "2023-12-08_164634_120823_15C_nononoB2A3B3_tile_A_converted_lag.csv"){
    print('remove dec 8 box 1')
    next
  } 
  if(converted.files[k] == "2023-12-08_115055_120823_15C_nononoB2A3B3_cory_A_converted_lag.csv"){
    print('remove dec 8 box 1')
    next
  } 
  
  print(paste(k, "||", converted.files[k]))
  # Read in data
  d<-read.csv(here("csv_files", converted.files[k]))
  
  # Which times to use for slope measures
  start_times0<-d2[grepl(x = d2$Filename, pattern = converted.files[k], ignore.case = TRUE),"start"]
  start_times0<-c(as.numeric(strsplit(as.character(start_times0[1,]), ",")[[1]]))
  end_times0<-d2[grepl(x = d2$Filename, pattern = converted.files[k], ignore.case = TRUE),"end"]
  end_times0<-c(as.numeric(strsplit(as.character(end_times0[1,]), ",")[[1]]))
 
  # print(start_times0)
  # print(end_times0)

  # Format date and continuous time
  d$DateTime<-strptime(paste(d$date), format = "%d-%m-%Y", tz = "GMT")
  d$time_min<-d$time_sec/60
  d_name<-converted.files[k]
  
  # get type or Run of data
  if(grepl("tile", converted.files[k])){
     Run<-"Tile"
  }
  if(grepl("cory", converted.files[k])){
     Run<-"Corynactis"
  }
  
  # get temperatures and times of data 
  min_temp<-min(d$Ch1_temp, na.rm = TRUE)
  max_temp<-max(d$Ch1_temp, na.rm = TRUE)
  time_min<-max(d$time_min, na.rm = TRUE)

  # make a log if there were any NA data points 
  d_pre<-d
  d<-d[c(which(!is.na(d$Ch1_temp))),] # make sure there are no NA is temperature
  d<-d[c(which(!is.na(d$time_min))),] # make sure there are no NA is time
  d_post<-d
  NArows<-diff(nrow(d_pre), nrow(d_post))
  if(identical(NArows, integer(0))){
    NArows <- 0
  }
  
  # print(paste(k, converted.files[k], sep = ": "))
  # log data in summary table out out 
  summaryTable<-rbind(summaryTable,
                      c(converted.files[k],
                        time_min,
                        NArows,
                        min_temp,
                        max_temp,
                        Run))
  
  if(max(d$time_min) > 15) {
    # data longer than 15 minutes
    if(grepl("tile", converted.files[k])){
      # TILE data analysis
      # print(c("TILE minutes on file:", as.numeric(round(d$time_min[nrow(d)], 2))))
      # bad data at the start, file starting cut off first adjusted from 
      # first 10 minutes to first 20 and 40 minutes
      if(d_name == "2024-02-18_140427_021824_AMB_12141214151311_tile_B_converted_lag.csv"|
         d_name == "2024-02-18_140427_021824_AMB_12141214151311_tile_A_converted_lag.csv"|
         d_name == "2024-02-14_143551_2024-02-14_AMD_98976108_tile_A_converted_lag.csv"|
         d_name == "2023-12-12_144729_121123_15C_A7B7E7A8B9E10_tile_B_converted_lag.csv"|
         d_name == "2024-02-14_194709_2024-02-14_PMC_791010689_tile_A_converted_lag.csv"){
        print("cut")
        d_output<-detect_negative_slopes(data = d,
                                 data_name = d_name,
                                 data_start_exclude = 40,
                                 data_duration = 80, # timeframe no longer than
                                 min_duration = 5,
                                 smooth_data = 3)
      }else{  
      # *************************************
      # RUN this if using manually logged data
      d_output<-detect_negative_slopes(data = d,
                                       data_name = d_name,
                                       data_start_exclude = 20,
                                       data_duration = 80, # timeframe no longer than
                                       min_duration = 5,
                                       smooth_data = 3)
      }
    }else{
      # CORY data analysis
      # print(c("CORY minutes on file:", as.numeric(round(d$time_min[nrow(d)], 2))))
      # 2023-12-11_132532_121123_23C_B11A14E14B14A15E15_cory_B_converted_lag_slopes
      # need to take off first 60 minutes. 
      if(d_name == "2023-12-11_132532_121123_23C_B11A14E14B14A15E15_cory_B_converted_lag.csv"){
        d_output<-detect_negative_slopes(data = d,
                                       data_name = d_name,
                                       data_start_exclude = 60,
                                       data_duration = 160, # timeframe no longer than
                                       min_duration = 5,
                                       smooth_data = 3)        
      }else if(d_name == "2023-12-12_110637_121123_15C_A7B7E7A8B9E10_cory_B_converted_lag.csv"){
         d_output<-detect_negative_slopes(data = d,
                                       data_name = d_name,
                                       data_start_exclude = 83,
                                       data_duration = 160, # timeframe no longer than
                                       min_duration = 5,
                                       smooth_data = 3)
      }else{
        # *************************************
        # RUN this if using manually logged data
        d_output<-detect_negative_slopes(data = d,
                                         data_name = d_name,
                                       data_start_exclude = 45,
                                       data_duration = 160, # timeframe no longer than
                                       min_duration = 5,
                                       smooth_data = 3) 
      }
    }
    
  }else{
    print(c("Not analyzed (less than 15 min of data):", converted.files[k]))
  }
  
  # Save the outputs if there was data
  if(length(d_output) == 2) {
  write.csv(x = d_output[[1]],
            file = here("MMR_SMR_AS_EPOC", "csv_input_files",
                        gsub(x = d_name, pattern = ".csv", "_analyzed.csv")),
            row.names = FALSE)
  ggsave(filename = here("slope_detection_output_plots",
                         gsub(x = d_name, pattern = ".csv", "_slopes.png")),
         plot = d_output[[2]],
         width = 7,
         height = 6)
  }
  
}

# summaryTable
