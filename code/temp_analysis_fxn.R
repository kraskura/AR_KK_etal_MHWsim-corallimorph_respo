
#' @title Function to extract temperature data 
#'
#' @param data 
#' @param filename
#'
#' 
trail_temps<-function(data, filename){

  d<-read.csv(data)
  # d <- d[!apply(is.na(d) | d == "", 1, all),] # if the row has no data, take it out 
  # d <- subset(d, !trimws(date) == "") # if the row has no date, take it out 
  mean_temp<-mean(as.numeric(d[, "Ch1_temp"]), na.rm = T)
  min_temp<-min(as.numeric(d[, "Ch1_temp"]), na.rm = T)
  max_temp<-max(as.numeric(d[, "Ch1_temp"]), na.rm = T)
  time_measure<-(as.numeric(d[, "time_sec"], na.rm = T)[nrow(d)]-
                as.numeric(d[, "time_sec"], na.rm = T)[1])/60 # in minutes
  if(grepl("_A_", filename)){
    box <- "A"
  }else{
    box<- "B"
  }

  d0<-data.frame(mean_temp_test_C = mean_temp,
                min_temp_test_C = min_temp,
                max_temp_test_C = max_temp,
                length_measure_min = time_measure,
                filename_converted = filename,
                Box = box)
  return(d0)
          
}
