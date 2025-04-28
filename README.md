# MHWsim-respo
Respirometry for MHWsim experiment
Authors and key roles: Amelia Ritger (experimental work) and Krista Kraskura (data analysis)

#### Steps of analysis:

1. text convert originally recoreded .txt files to .csv (no DO conversion of units, or temp correction). At the same analysis step also perform temperature corrections of DO with a _box_ specific shift in timing. (the respirometer internal temperature lagged by ~8.5 minute in box 1 and by 9.2 minutes in box 2). All data were visually assessed

  - outputs saved in './temp_slope_detection' directory 
  - final csv file for analysis saved in './csv_files' 
  
2. Run a function to extract linear regression slope for each animal and for each background. 

  - outputs saved in './slope_detection_output_plots' directory 

3. `MMR_SMR_AS_EPOC` function run with the following metrics:
  
  - all slopes at least 15 minutes long
  - R^2 set to 0.75
  - background correction are the mean of all channels together for each animal (each chamber)
  - background files used for timeframes 20-80 minutes (default), or 40-80 mintues (5 files)
  - cory files used for timeframes 45-160 muntes (default), 83 - 160 minutes (1 file), 60- 160 minutes (1 file)
  - all visaully assessed
