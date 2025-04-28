# Author: Krista Kraskura 
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
# Function to get find temperature lag for temperture corrections. 

filesA<-list.files(path = here("csv_files/"), pattern = "empty_A_converted.csv")
filesB<-list.files(path = here("csv_files/"), pattern = "empty_B_converted.csv")
empty_files<-c(filesA, filesB)


for (j in 1:length(empty_files)){
  if(j == 7){
    next
  }

  d<-read.csv(here("csv_files", empty_files[j]))
  if(max(d$time_sec/60, na.rm = T) < 60 ){next}

  
  d_scaled <- d %>%
    mutate(scale_Ch1_O2=scale(Ch1_O2, center = TRUE, scale = FALSE)[, 1],
           scale_Ch2_O2=scale(Ch2_O2, center = TRUE, scale = FALSE)[, 1],
           scale_Ch3_O2=scale(Ch3_O2, center = TRUE, scale = FALSE)[, 1],
           scale_Ch4_O2=scale(Ch4_O2, center = TRUE, scale = FALSE)[, 1],
           scale_temp=scale(Ch1_temp, center = TRUE, scale = FALSE)[, 1],
           time_min = time_sec/60) %>%
    select(scale_Ch1_O2, scale_Ch2_O2, scale_Ch3_O2, scale_Ch4_O2, scale_temp,
           date, time, time_min)
  
  # cit to one or two cycles 
  cut_min_start<-20
  cut_min_end<-60

  # regression 
  # incremenmin
  min_increment<-seq(0, 20*60, by = 10)
  
  for(t in 1:length(min_increment)){
    d_test<-d_scaled[d_scaled$time_min > cut_min_start & d_scaled$time_min< cut_min_end, ]
    d_test$scale_temp2<- dplyr::lag(d_test$scale_temp, n = min_increment[t])
    
    d_test<-d_test %>% 
      dplyr::rowwise() %>% 
      mutate(Ch_meanO2 = mean(c(scale_Ch1_O2,scale_Ch2_O2,
                          scale_Ch3_O2, scale_Ch4_O2), na.rm = TRUE), .groups = "keep")
    # regression probe 1
    r2<-summary(lm(scale_temp2 ~ Ch_meanO2, data = d_test))$r.squared
    slope<-summary(lm(scale_temp2 ~ Ch_meanO2, data = d_test))$coef[2, 1]

      if(slope < 0){# only negative slopes
        if(t == 1){ 
          r2_table<-data.frame("r2" = r2, "time_inc" = min_increment[t], 
                               "slope" = slope,
                               "time_lag" = min_increment[t]/60)
        }else{
          r2_row<-data.frame("r2" = r2, "time_inc" = min_increment[t],
                             "slope" = slope,
                             "time_lag" = min_increment[t]/60)
          r2_table<-rbind(r2_table, r2_row)
        }
      }
    }
  
    # max(r2_table$r2)
    lag_rows<-r2_table[r2_table$r2 == max(r2_table$r2),"time_inc"]
    lag_min<-r2_table[r2_table$r2 == max(r2_table$r2),"time_lag"]
    
    d_l<-d_scaled %>%
    mutate(scale_temp_lag = dplyr::lag(scale_temp, n = lag_rows)) %>% 
    pivot_longer(cols = c(scale_Ch1_O2, scale_Ch2_O2,
                          scale_Ch3_O2, scale_Ch4_O2, scale_temp, scale_temp_lag),
                 names_to = "scaled_var",
                 values_to = "scaled_val")
    
    d_l2<-d_scaled %>%
    mutate(scale_temp_lag = dplyr::lag(scale_temp, n = lag_rows)) %>% 
    pivot_longer(cols = c(scale_Ch1_O2, scale_Ch2_O2,
                          scale_Ch3_O2, scale_Ch4_O2),
                 names_to = "scaled_var",
                 values_to = "scaled_val")
  
    pAllcorr<-d_l2[d_l2$time_min > cut_min_start & d_l2$time_min< cut_min_end, ] %>%
      ggplot(aes(x = scale_temp_lag, y = scaled_val,
                    color = scaled_var))+
      geom_point(size = 0.1)+
      scale_color_manual(values = c("scale_Ch1_O2" = "#f59432",
                                    "scale_Ch2_O2" = "#009986",
                                    "scale_Ch3_O2" = "#805eea",
                                    "scale_Ch4_O2" = "#633e1c"))+
      theme_bw()+
      facet_wrap(.~scaled_var, scales = "free")
    pAllcorr

    # figures temp dependence and o2
    pAll<-d_l[d_l$time_min > cut_min_start & d_l$time_min< cut_min_end, ] %>%
      ggplot(aes(x = time_min, y = scaled_val,
                    color = scaled_var))+
      geom_line(linewidth = 0.1)+
      scale_color_manual(values = c("scale_Ch1_O2" = "#f59432",
                                    "scale_temp" = "grey",
                                    "scale_temp_lag" = "red3",
                                    "scale_Ch2_O2" = "#009986",
                                    "scale_Ch3_O2" = "#805eea",
                                    "scale_Ch4_O2" = "#633e1c"))+
      theme_bw()+
      ggtitle(paste("Time lag = ", lag_min, sep = ""))
     pAll
    
    
    # *****************
    # pAllcorrog<-ggplot(d_l2, aes(x = scale_temp, y = scaled_val,
    #                 color = scaled_var))+
    #   geom_point(size = 0.1)+
    #   scale_color_manual(values = c("scale_Ch1_O2" = "#f59432",
    #                                 "scale_Ch2_O2" = "#009986",
    #                                 "scale_Ch3_O2" = "#805eea",
    #                                 "scale_Ch4_O2" = "#633e1c"))+
    #   theme_bw()+
    #   facet_wrap(.~scaled_var, scales = "free")
    # pAllcorrog
    # 
    # # figures temp dependence and o2 
    # pAllog<-ggplot(d_l, aes(x = time_min, y = scaled_val,
    #                 color = scaled_var))+
    #   # annotate("rect", xmin = tpts_st, xmax = tpts_end,
    #   #          ymin = -Inf, ymax = Inf, 
    #   #          alpha = .1)+
    #   geom_line(linewidth = 0.1)+
    #   scale_color_manual(values = c("scale_Ch1_O2" = "#f59432",
    #                                 "scale_temp" = "grey",
    #                                 "scale_temp_lag" = "red3",
    #                                 "scale_Ch2_O2" = "#009986",
    #                                 "scale_Ch3_O2" = "#805eea",
    #                                 "scale_Ch4_O2" = "#633e1c"))+
    #   theme_bw()
    # 
    ggsave(filename = here("temp_slope_detection", "plots",
                     paste(gsub('.{4}$', '', empty_files[j]), "_plot.png", sep = "")),
           cowplot::plot_grid(pAll, pAllcorr,
                              nrow = 2,
                              ncol = 1,
                              rel_heights = c(0.6, 1)),
         width = 8, height = 8)
    print(lag_min)
  
  # # linear relationships 
  # for(i in 1:length(tpts_st)){
  #   # lm(scaled_val ~ time_min, data = d_l)
  #   d0<-d_l[c(which((round(d_l$time_min) == tpts_st[i]))[1]:
  #           which((round(d_l$time_min) == tpts_end[i]))[1]),]
  #   
  #   lms<-d0 %>% 
  #     group_by(factor(scaled_var)) %>%
  #     group_modify(~ broom::tidy(lm(scaled_val ~ time_min, data = .x))) %>% 
  #     as.data.frame() %>% 
  #     mutate(start_time = tpts_st[i],
  #            end_time = tpts_end[i],
  #            runI = i)
  #   
  #   p<-ggplot(d0, aes(x = time_min, y = scaled_val,
  #                 color = scaled_var))+
  #   geom_line(linewidth = 0.1)+
  #   scale_color_manual(values = c("scale_Ch1_O2" = "#f59432",
  #                                 "scale_temp" = "black",
  #                                 "scale_Ch2_O2" = "#009986",
  #                                 "scale_Ch3_O2" = "#805eea",
  #                                 "scale_Ch4_O2" = "#633e1c"))+
  #   geom_smooth(method = "lm", se = FALSE)+
  #   theme_bw()
  #   
  #   assign(x = paste("p", i, sep = ""), value = p)
  #   
  #   
  #   ggsave(filename = here("temp_slope_detection", "plots",
  #                    paste(gsub('.{4}$', '', empty_files[j]), "_plot.png", sep = "")),
  #          cowplot::plot_grid(pAll, p1, p2, p3), width = 8, height = 4)
  #   
  #   write.csv(x = rbind(lms1, lms2, lms3),
  #             file = here("temp_slope_detection", "csv",
  #                  paste(gsub('.{4}$', '', empty_files[j]), "_lms.csv", sep = "")))
  # }
  # print(j)
}

# slope analysis: 
list_csv_files <- paste(here("temp_slope_detection/"),
                        list.files(path = here("MMR_SMR_AS_EPOC",
                                               "csv_analyzed_SMR")),
                        sep ="")
# compile all data in one dataframe and add filename
data.rmr <- do.call(rbind, lapply(list_csv_files, function(x) cbind(read.csv(x), filename=gsub("^.{114}", "", x))))

    