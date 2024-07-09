

source("/Users/kristakraskura/Github_repositories/Respirometry_Performances/Codes/MR_firesting_v2.1.0.R")

# source("/Users/kristakraskura/Github_repositories/Respirometry-Performances/Codes/Firesting_split_files.R")

source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")


setwd("/Users/kristakraskura/Desktop/BOX/UCSB/Research/Anemones/Corynactis californica /Backgound tests_filtered seawater/")


###### ALL MR ANALYSIS --------------
organize_MR_analysis(create = "MMR_SMR_EPOC_AS")# 
organize_MR_analysis(create = "AUTO")# 

txt_csv_convert(txt_file = "backtest1_feb14-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest1-feb14-box1.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "animal_test1-feb15-box1.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "animal_test1-feb15-box4.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "backtest2_feb16-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest2-feb16-box1.txt", path="BACTERIAL_RESP", N_Ch=4)

txt_csv_convert(txt_file = "backtest1_feb19-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest1_feb19-box1.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "animal_test2-feb19-box1.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "animal_test2-feb19-box4.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "backtest2_feb20-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest2-feb20-box1.txt", path="BACTERIAL_RESP", N_Ch=4)

# added March 8 
txt_csv_convert(txt_file = "backtest1-feb23-box1.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest1-feb23-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "animal_test3-feb23-box1.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "animal_test3-feb23-box4.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "backtest2-feb24-box1.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest2-feb24-box4.txt", path="BACTERIAL_RESP", N_Ch=4)

txt_csv_convert(txt_file = "backtestOVN-feb24-box1.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtestOVN-feb24-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "animal_test4-feb25-box1.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "animal_test4-feb25-box4.txt", path="AUTO", N_Ch=4)
txt_csv_convert(txt_file = "backtest2-feb26-box1.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest2-feb26-box4.txt", path="BACTERIAL_RESP", N_Ch=4)

# backgrounds only
txt_csv_convert(txt_file = "backtest1-mar1-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest1-mar3-box4.txt", path="BACTERIAL_RESP", N_Ch=4)
txt_csv_convert(txt_file = "backtest1.2-mar3-box4.txt", path="BACTERIAL_RESP", N_Ch=4)

# setwd("../../")
setwd("./BACTERIAL_RESP/csv_files")

# march 8 addditions 

SMR(data="backtest1-feb23-box1.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="backtest1-feb23-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest2-feb24-box1.csv", # flushplot on doesnt work 
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=1, chop_end=0, flush_plot="OFF",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="backtest2-feb24-box4.csv", # flushplot on doesnt work 
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=1, chop_end=0, flush_plot="OFF",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtestOVN-feb24-box1.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="backtestOVN-feb24-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest2-feb26-box1.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="backtest2-feb26-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest1-mar1-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="backtest1-mar3-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="backtest1.2-mar3-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

# march 8 additons end 

SMR(data="backtest1_feb14-box4.CSV",
        inventory_data=NA,
        cycle_start=10, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest1-feb14-box1.CSV",
        inventory_data=NA,
        cycle_start=10, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest2_feb16-box4.CSV",
        inventory_data=NA,
        cycle_start=10, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest2-feb16-box1.CSV",
        inventory_data=NA,
        cycle_start=10, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

# Run 2 - 24 H respo 10:10 cycles, post bleaching all equipment.
SMR(data="backtest1_feb19-box4.CSV",
        inventory_data=NA,
        cycle_start=10, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest1_feb19-box1.csv",
        inventory_data=NA,
        cycle_start=10, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

# these are on cycle 10:20 
SMR(data="backtest2-feb20-box1.CSV",
        inventory_data=NA,
        cycle_start=10, cycle_end=30, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="backtest2_feb20-box4.CSV",
        inventory_data=NA,
        cycle_start=10, cycle_end=30, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")


setwd("../../AUTO/csv_files")

SMR(data="animal_test1-feb15-box1.csv",
        inventory_data=NA,
        cycle_start=10, cycle_end=30, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="animal_test1-feb15-box4.csv",
        inventory_data=NA,
        cycle_start=10, cycle_end=30, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

# Run 2 - 24 H respo 10:20 cycles, post bleaching all equipment.
SMR(data="animal_test2-feb19-box1.csv",
        inventory_data=NA,
        cycle_start=10, cycle_end=30, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="animal_test2-feb19-box4.csv",
        inventory_data=NA,
        cycle_start=10, cycle_end=30, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

### march 8 
SMR(data="animal_test4-feb25-box1.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="animal_test4-feb25-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")

SMR(data="animal_test3-feb23-box1.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
SMR(data="animal_test3-feb23-box4.csv",
        inventory_data=NA,
        cycle_start=5, cycle_end=20, chop_start=2, chop_end=0, flush_plot="ON",
        N_Ch=4, path="UseFolders", date_format = "m/d/y")
### march 8 


setwd("../../MMR_SMR_AS_EPOC/csv_input_files/")
MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test1-feb15-box4_analyzed.csv",
                AnimalID = c("tinycol", "onebig", "empty","cortiny"),
                BW.animal = c(15.408/1000, 5.025/1000, 0, 5.04/1000), # smallCol 0.408; singleBig 0.025 g, empty; 2babies 0.04; 
                drop_ch = 3,
                resp.V = c(0.062, 0.062, 0.102, 0.015), 
                r2_threshold_smr = 0.1,
                r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                background_prior = "backtest1_feb14-box4_analyzed.csv",
                background_post = "backtest2_feb16-box4_analyzed.csv",
                background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)

MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test1-feb15-box1_analyzed.csv",
                AnimalID = c("empty", "DIRTYcol", NA, NA), # empty, full col. 10.321 
                drop_ch = c(3,4),
                BW.animal = c(0, 60.321/1000, 0, 0),
                resp.V = c(0.600, 0.470, 0, 0), 
                r2_threshold_smr = 0.1, r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                background_prior = "backtest1-feb14-box1_analyzed.csv",
                background_post = "backtest2-feb16-box1_analyzed.csv",
                background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)

# run 2 -------
MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test2-feb19-box1_analyzed.csv",
                AnimalID = c("BIGcol", "DIRTYcol", NA, NA), 
                drop_ch = c(3,4),
                BW.animal = c(60.321/1000, 10.75/1000, 0, 0),
                resp.V = c(0.600, 0.470, 0, 0), 
                r2_threshold_smr = 0.1,
                r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                # background_prior = "backtest1_feb19-box1_analyzed.csv",
                background_post = "backtest2-feb20-box1_analyzed.csv",
                # background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)


MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test2-feb19-box4_analyzed.csv",
                AnimalID = c("CoolCol", "failCol", NA, "2babies"), #  
                BW.animal = c(10.6/1000, 10.2/1000, 0, 5.08/1000),
                drop_ch = 3,
                resp.V = c(0.062, 0.062, 0.102, 0.015), 
                r2_threshold_smr = 0.1,
                r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                # background_prior = "backtest1_feb19-box4_analyzed.csv",
                background_post = "backtest2_feb20-box4_analyzed.csv",
                # background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)


# late Feb runs 
MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test3-feb23-box1_analyzed.csv",
                AnimalID = c("BigCol", "ClamCol", NA, NA), #  
                BW.animal = c(66.8/1000, 44.2/1000, 0, 0),
                drop_ch = c(3,4),
                resp.V = c(0.600, 0.470, 0, 0), 
                r2_threshold_smr = 0.1,
                r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                background_prior = "backtest1-feb23-box1_analyzed.csv",
                background_post = "backtest2-feb24-box1_analyzed.csv",
                # background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)

MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test3-feb23-box4_analyzed.csv",
                AnimalID = c("c1", "c2", "c3", "c4"), #  
                BW.animal = c(15.33/1000, 10.1/1000, 10/1000, 5.04/1000),
                # drop_ch = c(3,4),
                resp.V = c(0.062, 0.062, 0.102, 0.015), 
                r2_threshold_smr = 0.1,
                r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                background_prior = "backtest1-feb23-box4_analyzed.csv",
                background_post = "backtest2-feb24-box4_analyzed.csv",
                # background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)

# late Feb runs 
MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test4-feb25-box1_analyzed.csv",
                AnimalID = c("BigCol", "ClamCol", NA, NA), #  
                BW.animal = c(70/1000, 44.2/1000, 0, 0),
                drop_ch = c(3,4),
                resp.V = c(0.600, 0.470, 0, 0), 
                r2_threshold_smr = 0.1,
                r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                background_prior = "backtestOVN-feb24-box1_analyzed.csv",
                background_post = "backtest2-feb26-box1_analyzed.csv",
                # background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)

MMR_SMR_AS_EPOC(data.MMR = "none",
                data.SMR = "animal_test4-feb25-box4_analyzed.csv",
                AnimalID = c("c1", "c2", "c3", "c4"), #  
                BW.animal = c(18/1000, 11/1000, 11/1000, 8/1000),
                # drop_ch = c(3,4),
                resp.V = c(0.062, 0.062, 0.102, 0.015), 
                r2_threshold_smr = 0.1,
                r2_threshold_mmr = 0.9,
                min_length_mmr = 1,
                background_prior = "backtestOVN-feb24-box4_analyzed.csv",
                background_post = "backtest2-feb26-box4_analyzed.csv",
                # background_linear_gr = TRUE, 
                MLND=FALSE,
                path = "folders",
                match_background_Ch = TRUE)






