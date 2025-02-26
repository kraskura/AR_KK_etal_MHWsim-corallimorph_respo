# Author:Krista Kraskura
# Date: july 6 2024; last update: Feb 23, 2025
# Title: Respo analysis 

# ******************************************
# STEP 1 in analysis flow 

# *******************************************
# *******************************************
# 1. Libraries:
# install devtools before this if necessary 
devtools::install_github("kraskura/AnalyzeResp", force = TRUE)

# other libraries, dependencies:
library(pacman)
p_load(stats, ggplot2, scales, grDevices, graphics, utils, 
       dplyr, magrittr, pryr, tidyr, plotrix, mclust, 
       gridExtra, DescTools, rMR, tidyr, here, tidyverse, tictoc)

library(AnalyzeResp)
here()

source(here("code", "temp_analysis_fxn.R"))
summary.file0<-readxl::read_excel(path = here("data/respirometry_info.xlsx"), sheet = "merged data")

# *******************************************
# *******************************************
# 2. (run once) create local folder for organisation 
AnalyzeResp::organizeAnalysisLocally(SDA.folder = FALSE,
                        MMR.folder = FALSE,
                        SMR.folder = FALSE,
                        BACK_RESP.folder = FALSE) # don't need SDA and MMR - no data for this experiment


# *******************************************
# *******************************************
# 3. Convert text to usable csv files
# FIRST PASS: with rerecorded, fluctuating temperatures
tic("first text convert")
respo.files<-list.files(path = here(), pattern = ".txt") # 71 files 

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
toc()# 315.86 sec elapsed

# *******************************************
# *******************************************
# Use only if temperature correcting o2 trends --------
# Run using the mean temperatures of the trial
# first loop to extract the mean temperatures
tic("get mean temps")
converted.files0<-list.files(path = here("csv_files/"), pattern = ".csv") # 110 files with A and B boxes
for(i in 1:length(converted.files0)){
  # print(i)
  if (i == 1){
    d <- trail_temps(data = here("csv_files",converted.files0[i]),
                     filename = converted.files0[i])
  }else{
    d0 <- trail_temps(data = here("csv_files",converted.files0[i]),
                     filename = converted.files0[i])
    d<-rbind(d, d0)
  }
}
toc() # 5.997 sec

# get the type of the data cory and tile
d$type<-NA
for (i in 1:nrow(d)){
  if(grepl("cory", d$filename_converted[i])){
    d$type[i] <- "Corynactis"
  }else if (grepl("tile", d$filename_converted[i])){
    d$type[i] <- "Tile"
  }
}

# unique /short filename id's
d$filenameID <- paste(substr(d$filename_converted, start = 1, stop = 20),
                      d$type,
                      d$Box,
                      sep = "_")

# summary file formatting 
summary.file0<- summary.file0 %>% 
  tidyr::fill(Filename) %>% # fill empty lines/rows
  filter(!Run == "Empty") %>% # take out empty runs (tile is used as a background)
  mutate(indivID = paste(Genet, Tank, sep = "")) %>% 
  mutate(filenameID = paste(substr(Filename, start = 1, stop = 20), Run, Box, sep = "_"))

# head(summary.file0$filenameID)
# head(d$filenameID)

summary.file <- merge(summary.file0, d, by = "filenameID")
# nrow(summary.file)
# view(summary.file)


# ********************************************
# ********************************************
tic("second text convert with normalized temps") 
# normalize temperature to the mean of the trail
for(i in 1:length(respo.files)){ 

  print(i)
  print(respo.files[i])
  
  if(respo.files[i] == "2023-12-05_102008_120523_23C_E11A15B15E15.txt"){
    # the cory file is already there, this was a repeat saved file.
    next
  }
  
  # the main name 
  temp_corrected1<-as.numeric(summary.file[which(summary.file$Filename == respo.files[i])[1], "mean_temp_test_C"])
  # the alternative filename
  temp_corrected2<-as.numeric(summary.file[which(summary.file$`Alternative filename` == respo.files[i])[1], "mean_temp_test_C"])
  if(!is.na(temp_corrected1)){
    temp_corrected<-temp_corrected1
    print(paste("Primary data file, temp:", round(temp_corrected,2)))
  }else if(!is.na(temp_corrected2)){
    temp_corrected<-temp_corrected2
    print(paste("Alt data file, temp:", round(temp_corrected,2)))
    # "2023-12-06_120950_120623_23C_B12A13nono_cory_A_converted.csv" <<  comes out with no data, repeat file 
    next
  }else{
    print("No mean temp recorded")
  }

  
  if(any(suppressWarnings(grepl(x = read.delim(respo.files[i]),
            pattern = "BOX2 [B]", fixed = TRUE)))){ # if the file has Device Box2 [B] detected
    textFileConvert(txt_file = respo.files[i],
                  local_path = FALSE,
                  type_file = "Firesting_2023",
                  N_Ch = 4,
                  convert_units = T,
                  temperature = temp_corrected,
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
                temperature = temp_corrected,
                units_from = "pct", # units of oxygen measured
                units_to = "mg/L",
                salinity = 35, # full strength sea water
                atm_pressure = 1,
                device = "A")
}
toc() # 309 sec

