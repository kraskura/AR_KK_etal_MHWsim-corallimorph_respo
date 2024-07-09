# Author:Krista Kraskura
# Date: july 6 2024
# Title: Respo analysis 

# *******************************************
# 1. Libraries:
# install devtools before this if nessessary 
devtools::install_github("kraskura/AnalyzeResp", force = TRUE)

# other libraries, dependencies:
library(pacman)
p_load(stats, ggplot2, scales, grDevices, graphics, utils, 
       dplyr, magrittr, pryr, tidyr, plotrix, mclust, 
       gridExtra, DescTools, rMR, tidyr, here)

library(AnalyzeResp)
here()

# *******************************************
# 2. create local folder for organisation
organizeAnalysisLocally() # can  delete SDA and MMR - no data for this experiment

# *******************************************
# 3. convert text to usable csv files
# all files that need to be analyzed
respo.files<-list.files(path = here("data/respo data/"), pattern = ".txt")

# loop to convert files 
for(i in 1:length(respo.files)){
  # print(i)
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
  if(c(i > 42 | c(i > 29 & i < 41))){ 
    # 30 - 40 and 43 onwards: files have data from two devices
    # print(i)
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
}

# *******************************************
# 4. Analyze data using SMR function
# assuming 10 minute respo fragments with 30 reset. 
converted.files<-list.files(path = here("csv_files/"), pattern = ".csv")

# length(converted.files)
for(i in 1:length(converted.files)){
print(i)
 SMR(data = converted.files[i],
    cycle_end = 20,
    cycle_start = 0.5, # 30 seconds
    first_cycle = "flush",
    chop_start = 0,
    chop_end = 0,
    date_format = c("%d-%m-%Y	%H:%M:%OS", "GMT"), 	
    local_path = FALSE,
    background_data = FALSE,
    sda_data = FALSE)
}

