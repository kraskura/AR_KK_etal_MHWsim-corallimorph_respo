# Author: Krista Kraskura (used Meta AI to facilitate coding)
# Last update: Apr 2024
# Title: Characterising temperature effects in the system, facilitating functions. 

# install devtools before this if necessary 
# devtools::install_github("kraskura/AnalyzeResp", force = TRUE)

# other libraries, dependencies:
library(pacman)
p_load(stats, ggplot2, scales, grDevices, graphics, utils, 
       dplyr, magrittr, pryr, tidyr, plotrix, mclust, 
       gridExtra, DescTools, rMR, tidyr, here, tidyverse, tictoc)

library(AnalyzeResp)
library(here)

# *******************************************
# *******************************************
# Convert text to usable csv files EMPTY FILES ONLY 
respo.files<-list.files(path = here(), pattern = "empty.txt") # 20 files 

for(i in 1:length(respo.files)){ 

  if(any(suppressWarnings(grepl(x = read.delim(respo.files[i]),
            pattern = "BOX2 [B]", fixed = TRUE)))){ # if the file has Device Box2 [B] detected
    textFileConvert(txt_file = respo.files[i],
                  local_path = FALSE,
                  type_file = "Firesting_2023",
                  N_Ch = 4,
                  convert_units = T,
                  units_from = "pct", # units of oxygen measured 
                  units_to = "mg/L",
                  salinity = 35, # full strength sea water
                  atm_pressure = 1,
                  device = "B")
  }
  # all files have an "A" device
  textFileConvert(txt_file = respo.files[i],
                local_path = FALSE,
                type_file = "Firesting_2023",
                N_Ch = 4,
                convert_units = T,
                units_from = "pct", # units of oxygen measured 
                units_to = "mg/L",
                salinity = 35, # full strength sea water
                atm_pressure = 1,
                device = "A")   
}

# *******************************************
# *******************************************
# Function to get the slope for each temperatur


  d<-read.csv(here("csv_files/2023-11-30_170922_113023_19C_empty_A_converted.csv"))
  # Calculate the mean of y
  par(mfrow = c(5, 5))  ## set the layout to be 3 by 3
  d<-d %>% 
    mutate(scale_Ch1_O2=scale(Ch1_O2, center = TRUE, scale = FALSE)[, 1], 
           scale_Ch2_O2=scale(Ch2_O2, center = TRUE, scale = FALSE)[, 1],
           scale_Ch3_O2=scale(Ch3_O2, center = TRUE, scale = FALSE)[, 1],
           scale_Ch4_O2=scale(Ch4_O2, center = TRUE, scale = FALSE)[, 1],
           scale_temp=scale(Ch1_temp, center = TRUE, scale = FALSE)[, 1])
  
  plot(d$time_sec/60, d$scale_temp, cex = 0.2)
  points(d$time_sec/60, d$scale_Ch1_O2, cex = 0.1, col = "grey")
  points(d$time_sec/60, d$scale_Ch2_O2, cex = 0.1, col = "pink")
  points(d$time_sec/60, d$scale_Ch3_O2, cex = 0.1, col = "yellow3")
  points(d$time_sec/60, d$scale_Ch4_O2, cex = 0.1, col = "dodgerblue")



