# Author:Krista Kraskura
# Date: july 6 2024; last update/run: April 28, 2025
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
                  device = "B")
  }
  # all files have an "A" device
  textFileConvert(txt_file = respo.files[i],
                local_path = FALSE,
                type_file = "Firesting_2023",
                N_Ch = 4,
                device = "A")   
}
toc()# 315.86 sec elapsed



# *******************************************
# *******************************************
# Use only if temperature correcting o2 trends --------
# Run using the mean temperatures of the trial
# first loop to extract the mean temperatures
# tic("get mean temps")
# converted.files0<-list.files(path = here("csv_files/"), pattern = ".csv") # 110 files with A and B boxes
# for(i in 1:length(converted.files0)){
#   # print(i)
#   if (i == 1){
#     d <- trail_temps(data = here("csv_files",converted.files0[i]),
#                      filename = converted.files0[i])
#   }else{
#     d0 <- trail_temps(data = here("csv_files",converted.files0[i]),
#                      filename = converted.files0[i])
#     d<-rbind(d, d0)
#   }
# }
# toc() # 5.997 sec
# 
# # get the type of the data cory and tile
# d$type<-NA
# for (i in 1:nrow(d)){
#   if(grepl("cory", d$filename_converted[i])){
#     d$type[i] <- "Corynactis"
#   }else if (grepl("tile", d$filename_converted[i])){
#     d$type[i] <- "Tile"
#   }
# }
# 
# # unique /short filename id's
# d$filenameID <- paste(substr(d$filename_converted, start = 1, stop = 20),
#                       d$type,
#                       d$Box,
#                       sep = "_")
# 
# # summary file formatting 
# summary.file0<- summary.file0 %>% 
#   tidyr::fill(Filename) %>% # fill empty lines/rows
#   filter(!Run == "Empty") %>% # take out empty runs (tile is used as a background)
#   mutate(indivID = paste(Genet, Tank, sep = "")) %>% 
#   mutate(filenameID = paste(substr(Filename, start = 1, stop = 20), Run, Box, sep = "_"))
# 
# # head(summary.file0$filenameID)
# # head(d$filenameID)
# 
# summary.file <- merge(summary.file0, d, by = "filenameID")
# nrow(summary.file)
# view(summary.file)


# ********************************************
# ********************************************
# temperature corect with 20 minute lag 
# normalize temperature to the mean of the trail
# no empty files
converted.respo.filesA<-list.files(here("csv_files"), pattern = "cory_A_converted.csv")
converted.respo.filesB<-list.files(here("csv_files"), pattern = "cory_B_converted.csv")
converted.respo.filesAt<-list.files(here("csv_files"), pattern = "tile_A_converted.csv")
converted.respo.filesBt<-list.files(here("csv_files"), pattern = "tile_B_converted.csv")

converted.respo.files<-c(converted.respo.filesA, converted.respo.filesB,
                         converted.respo.filesAt, converted.respo.filesBt)

min_lagA<-8.5
min_lagB<-9.2 #9.2

for(i in 1:length(converted.respo.files)){
  
  d0<-read.csv(here("csv_files", converted.respo.files[i]) )
  
  if(grepl("A_converted.csv", converted.respo.files[i])){
    min_lag<-round(min_lagA*60)
  }
  if(grepl("B_converted.csv", converted.respo.files[i])){
    min_lag<-round(min_lagB*60)
  } 
  d0$Ch1_temp_lag<-dplyr::lag(d0$Ch1_temp, n = min_lag)
  
  
  d0$Ch1_O2_lag<-rMR::DO.unit.convert(d0$Ch1_O2,
                       DO.units.in = "pct",
                       DO.units.out = "mg/L",
                       bar.units.in = "atm",
                       bar.units.out = "atm",
                       salinity.units = "pp.thou",
                       bar.press = 1,
                       temp.C = d0$Ch1_temp_lag,
                       salinity = 35)
  
  d0$Ch2_O2_lag<-rMR::DO.unit.convert(d0$Ch2_O2,
                       DO.units.in = "pct",
                       DO.units.out = "mg/L",
                       bar.units.in = "atm",
                       bar.units.out = "atm",
                       salinity.units = "pp.thou",
                       bar.press = 1,
                       temp.C = d0$Ch1_temp_lag,
                       salinity = 35)
  
  d0$Ch3_O2_lag<-rMR::DO.unit.convert(d0$Ch3_O2,
                       DO.units.in = "pct",
                       DO.units.out = "mg/L",
                       bar.units.in = "atm",
                       bar.units.out = "atm",
                       salinity.units = "pp.thou",
                       bar.press = 1,
                       temp.C = d0$Ch1_temp_lag,
                       salinity = 35)
  
  d0$Ch4_O2_lag<-rMR::DO.unit.convert(d0$Ch4_O2,
                       DO.units.in = "pct",
                       DO.units.out = "mg/L",
                       bar.units.in = "atm",
                       bar.units.out = "atm",
                       salinity.units = "pp.thou",
                       bar.press = 1,
                       temp.C = d0$Ch1_temp_lag,
                       salinity = 35)
  
  d0<-d0[-c(1:min_lag), ]
  
  d_lag<-d0 %>% 
    dplyr::select(date, time, time_sec, Ch1_O2_lag, Ch1_temp, Ch2_O2_lag, Ch3_O2_lag, Ch4_O2_lag)
  colnames(d_lag)<-c("date", "time", "time_sec", "Ch1_O2", "Ch1_temp", "Ch2_O2", "Ch3_O2", "Ch4_O2")
  
  p1<-d_lag %>%
    dplyr::select(!Ch1_temp) %>%
    pivot_longer(cols = 4:7) %>% 
    ggplot(aes(x = time_sec/60, y = value, color = name))+
    geom_line(linewidth = 0.4, alpha = 0.2) +
    scale_color_manual(values = c("Ch1_O2" = "#f59432","Ch1" = "#f59432",
                                  "Ch2_O2" = "#009986","Ch2" = "#009986",
                                  "Ch3_O2" = "#805eea","Ch3" = "#805eea",
                                  "Ch4_O2" = "#633e1c","Ch4" = "#633e1c")) +
    theme_bw() +
    labs(x = "Time, minutes", y = "Oxygen, mg/L")+
    theme(legend.title = element_blank(),
          legend.position = "top")+
    ggtitle(converted.respo.files[i])
  
  p1_temp<-d_lag %>%
    ggplot(aes(x = time_sec/60, y = Ch1_temp))+
    geom_line(linewidth = 0.4, alpha = 0.2) +
    theme_bw() +
    labs(x = "Time, minutes", y = "Temperature ºC")+
    theme(legend.title = element_blank(),
          legend.position = "top")
  
  p1_combo<-cowplot::plot_grid(p1, p1_temp, nrow = 2, rel_heights = c(1.7, 1))
  p1_combo
  summary(lm(d_lag$Ch2_O2 ~ d_lag$time_sec))$r.squared
  
  ggsave(filename = here("temp_slope_detection/plots/",
                      gsub(x = converted.respo.files[i], pattern =  ".{4}$", replacement = "_temp.png")),
         plot = p1_combo, 
         height = 10, width = 6)
    
  write.csv(d_lag, here("temp_slope_detection/csv/",
                      gsub(x = converted.respo.files[i], pattern =  ".{4}$", replacement = "_lag.csv")),
            row.names = FALSE)
  write.csv(d_lag, here("csv_files/",
                      gsub(x = converted.respo.files[i], pattern =  ".{4}$", replacement = "_lag.csv")),
            row.names = FALSE)
  
}
  

