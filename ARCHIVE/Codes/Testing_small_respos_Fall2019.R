

source("/Users/kristakraskura/Github_repositories/Respirometry-firesting/MR_firesting_v2.0.2.R")

#################### testing / improving the Respo code new code 

## Last run: 2019, june 18


# 1. set working directory with raw data files where everything is gping to be organized 
setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/2. experimental setup /Small respos /")

# 2. create all working directories 
organize_MR_analysis(create = "Full")# bacterial respiration currently has similar format than that for auto cycles. 

# 3. convert all raw firesting data files into csv files and automatically save them in "csv_files" subfolders, either in AUTO or MANUAL or bacterial main folders, accordingly. 

# 8 ch Firesting -- done oct 1 2019
txt_csv_convert(txt_file = "Sep27_2019_test_FISHcycle6f4m.txt", N_Ch=4, path = "AUTO")
txt_csv_convert(txt_file = "Sep27_2019_test_FISHbackpostcycle5f10m.txt", N_Ch=4, path="BACTERIAL_RESP")
txt_csv_convert(txt_file = "Sep23_2019_test_100mlpersitaltic_250mljar2.txt", N_Ch=4, path="AUTO")
txt_csv_convert(txt_file = "Sep27_2019_test_FISH.txt", N_Ch=4, path = "AUTO")



# 4. Use MMR function on MMR files

setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/2. experimental setup /Small respos /MANUAL/csv_files")
# MMR(data.MMR="",
#     cycles = 1,
#     cycle_start = c(0),
#     cycle_end = c(4),
#     Ch1=c(1,0),
#     Ch2=c(1,0),
#     Ch3=c(1,0),
#     Ch4=c(0,0), 
#     N_Ch=4,
#     path="folder")


# 5. Use SMR function on auto files
setwd("../../")
setwd("./AUTO/csv_files")
 
SMR(data = "Sep27_2019_test_FISHcycle6f4m.csv", 
    inventory_data = NA,
    cycle_start = 6, 
    cycle_end = 10, 
    chop_start = 10/60, 
    chop_end = 10/60, 
    N_Ch=4,
    path="foldering",
    flush_plot = "OFF")
### // ---  8 Ch Firesting 

# Need to comnien several smr files to make one to be input in MMR_SMR_EPOC_AS function?
# 5.1 gluing cleaned and ready to go smr files together:
# 1. run these through the smr function (see just above) 
setwd("../../")
setwd("./AUTO/csv_analyzed")

# combine_smr(smr_files= c("jun23_2019_opeaSize_box4_1smr_analyzed.csv", "jun23_2019_opeaSize_box4_1smr2_analyzed.csv", "jun23_2019_opeaSize_box4_1smr3_analyzed.csv"))
# the glued filed with name "GLUED" will be automatically placed in the folder for full AS, EPOC etc. analysis

# 6. Use SMR function on background respiration files
setwd("../../")
setwd("./BACTERIAL_RESP/csv_files")

SMR(data = "Sep27_2019_test_FISHbackpostcycle5f10m-cut30min.csv", 
    inventory_data = NA,
    cycle_start = 5, 
    cycle_end = 15, 
    chop_start = 10/60, 
    chop_end = 10/60, 
    N_Ch=4,
    path="foldering",
    flush_plot = "OFF")



setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/2. experimental setup /Small respos /MMR_SMR_AS_EPOC/csv_input_files")

MMR_SMR_AS_EPOC(
	data.MMR = "none",
	data.SMR = "Sep27_2019_test_FISHcycle6f4m_analyzed.csv", 
	AnimalID = c("1","2","NA","NA"),
	BW.animal = c(0.0015, 0.0004, 0,0),
	drop_ch = c(3,4),
	resp.V = c(0.065,0.020,0,0), 
	r2_threshold_smr = 0.85,
	r2_threshold_mmr = 0.9,
	min_length_mmr = 60, 
	epoc_threshold = 1.1, # <--- this argument is for any level of RMR, e.g. 10% above SMR = 1.1 (i.e. 110% SMR)
	background_prior = NA,
	background_post = NA, 
	background_slope = 0.01258, 
  background.V = 0.065,
	match_background_Ch = FALSE, 
	MLND=FALSE,
	mmr_background = "SAME_slope", 
	path="foldering")

# --- // end 