# Author:Krista Kraskura
# Date: Feb 4 2025, last update feb 22 2025
# Title: Respo analysis 

# ******************************************
# STEP 3 in analysis flow 

# *******************************************
# *******************************************
# 1. Libraries:
# install devtools before this if necessary 

# devtools::install_github("kraskura/AnalyzeResp", force = TRUE)
# 3 # dont install packages s/ change as needed 
# 
# # other libraries, dependencies: 
library(pacman)
p_load(stats, ggplot2, scales, grDevices, graphics, utils,
       dplyr, magrittr, pryr, tidyr, plotrix, mclust,
       gridExtra, DescTools, rMR, tidyr, here, tidyverse)
library(AnalyzeResp)
here()

# *******************************************
# *******************************************
# 2. MMR_SMR_AS_EPOC to get animal specific values 
summary.file<-readxl::read_excel(path = here("data/respirometry_info.xlsx"), sheet = "merged data")
smr.files<-list.files(path = here("MMR_SMR_AS_EPOC/csv_input_files/"), pattern = "cory")
back.files<-list.files(path = here("MMR_SMR_AS_EPOC/csv_input_files/"), pattern = "tile")

# summary file formatting 
summary.file<- summary.file %>% 
  tidyr::fill(Filename) 
# take out empty runs (tile is used as a background)
summary.file<- summary.file %>% 
  filter(!Run == "Empty") %>% 
  mutate(indivID = paste(Box, "-", Genet, Tank, "-P", Polyps, sep = ""))

# view(summary.file)
# volumes 
# 0.088 L with tile (tile = 0.004 L) << this is the volume 
# 0.092 L without tiles (empty); don't use 


for(i in 1:length(smr.files)){

  print(paste(i, "/out of", length(smr.files)))
  
  # exclude (should be already excluded in step 2)
  # 2023-12-08_164634_120823_15C_nononoB2A3B3_tile_A_converted_lag_slopes; no quality data 
  # if(smr.files[i] == "2023-12-08_164634_120823_15C_nononoB2A3B3_tile_A_converted_lag"){
  #   print("not analyzing box 1 2023 dec 8 ")
  #   next 
  # }
  
  # get file match 
  smr.file0<-smr.files[i]
  back.file0<-back.files[grepl(gsub("cory", "tile",   gsub("^.{30}", "", smr.file0)), back.files)]
  file.match.id<-gsub("_cory.*", "", gsub("^.{25}", "", smr.file0))
  
  # print(c("*** pairs:", smr.file0, back.file0, "***"))
  
  if(length(smr.file0) < 1) {
    print("next")
    next
  }

  # A and B files   
  if(length(smr.file0) > 0){    
    
    smr.file<-smr.file0
    back.file<-back.file0
      
    if(any(grepl(file.match.id, summary.file$`Alternative filename`))){
      message("file in alternative rows")
      next
    }
            
    if(grepl("_B_converted", smr.file)){
      # B file data
      # mass 
      ch1<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "1", "Sum body size (g)"])
        if(length(ch1) == 0){ch1<- NA}
      ch2<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "2", "Sum body size (g)"])
        if(length(ch2) == 0){ch2<- NA}
      ch3<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "3", "Sum body size (g)"])
        if(length(ch3) == 0){ch3<- NA}
      ch4<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "4", "Sum body size (g)"])
       if(length(ch4) == 0){ch4<- NA}
      # masses<-c(ch1, ch2, ch3, ch4)
      
      # ID
      ch1.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "1", "indivID"])
        if(rlang::is_empty(ch1.id)){ch1.id<- NA}
      ch2.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "2", "indivID"])
        if(rlang::is_empty(ch2.id)){ch2.id<- NA}
      ch3.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "3", "indivID"])
        if(rlang::is_empty(ch3.id)){ch3.id<- NA}
      ch4.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "B" &
                          summary.file$Channel == "4", "indivID"])
        if(rlang::is_empty(ch4.id)){ch4.id<- NA}
      # IDs<-c(ch1.id, ch2.id, ch3.id, ch4.id)
      
      
      # Analysis function run: 
      MMR_SMR_AS_EPOC(data.SMR = smr.file,
                    AnimalID = c(ch1.id, ch2.id, ch3.id, ch4.id),
                    BW.animal = c(ch1/1000, ch2/1000, ch3/1000, ch4/1000),
                    resp.V = c(0.088, 0.088, 0.088, 0.088), 
                    r2_threshold_smr = 0.7,
                    background_post = back.file,
                    match_background_Ch = FALSE,
                    local_path = F,
                    MLND = F)
    
    }else{
      # A file data
      # 2023-12-08_115055_120823_15C_nononoB2A3B3_cory.txt # no body size

      # mass 
      ch1<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename) &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "1","Sum body size (g)"])
        if(length(ch1) == 0){ch1<- NA}
      ch2<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "2", "Sum body size (g)"])
        if(length(ch2) == 0){ch2<- NA}
      ch3<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "3", "Sum body size (g)"])
        if(length(ch3) == 0){ch3<- NA}
      ch4<-as.numeric(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "4", "Sum body size (g)"])
        if(length(ch4) == 0){ch4<- NA}
      # masses<-c(ch1, ch2, ch3, ch4)
      
      # ID
      ch1.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "1", "indivID"])
        if(ch1.id == "character(0)"){ch1.id<- "NA"}
      ch2.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "2", "indivID"])
        if(ch2.id == "character(0)"){ch2.id<- "NA"}
      ch3.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "3", "indivID"])
        if(ch3.id == "character(0)"){ch3.id<- "NA"}
      ch4.id<-as.character(summary.file[grepl(file.match.id, summary.file$Filename)  &
                          summary.file$Run == "Corynactis" & 
                          summary.file$Box == "A" &
                          summary.file$Channel == "4", "indivID"])
        if(ch4.id == "character(0)"){ch4.id<- "NA"}
      # IDs<-c(ch1.id, ch2.id, ch3.id, ch4.id)
      
      if(all(is.na(unlist(c(ch1/1000, ch2/1000, ch3/1000, ch4/1000))))){
        message("not analyzed, no masses")
        next
      }else{
        print(unlist(c(ch1.id, ch2.id, ch3.id, ch4.id)))
        # Analysis function run: 
        MMR_SMR_AS_EPOC(data.SMR = smr.file,
                    AnimalID = unlist(c(ch1.id, ch2.id, ch3.id, ch4.id)),
                    BW.animal = unlist(c(ch1/1000, ch2/1000, ch3/1000, ch4/1000)),
                    resp.V = c(0.088, 0.088, 0.088, 0.088), # correct
                    r2_threshold_smr = 0.75,
                    background_post = back.file,
                    match_background_Ch = TRUE,
                    local_path = F,
                    MLND = F) 
      }
    }
  }
}













