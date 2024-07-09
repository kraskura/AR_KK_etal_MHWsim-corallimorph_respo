library(tidyverse)
library(janitor)

# import respirometry_info
size <- read_csv("respirometry_size.csv") %>%
  clean_names() %>%
  select(-comments)

size_long <- size %>%
  pivot_longer(cols = size1a_cm:size11b_cm,
               names_to = "measurement",
               values_to = "value") %>%
  drop_na(value)

# for consecutive rows that share the same first 5 values in the string of the mesaurement column, add those two values
size_measure <- size_long %>%
  mutate(measurement=str_remove_all(measurement, "[^0-9]"),
         measurement=str_replace_all(measurement, "^", "polyp ")) %>%
  group_by(date, temperature, time_start, box, channel, genet, tank, run, polyps, measurement) %>%
  summarize(body_size_mm = (sum(value)/2)*10) %>%
  mutate(body_mass_g = 0.00442 + body_size_mm*0.0788 + body_size_mm^2*0.017) %>% #polynomial equation calculated from 308 measured polyps in 2019
  ungroup()

# export to .csv - These are the data that show the mm and g for every measured polyp
write_csv(size_measure, "bodySizeCalc.csv")

size_sum <- size_measure %>%
  group_by(date, temperature, time_start, box, channel, genet, tank, run, polyps) %>%
  summarize(sum_body_size_mm = sum(body_size_mm),
            min_body_size_mm = min(body_size_mm),
            mean_body_size_mm = mean(body_size_mm),
            max_body_size_mm = max(body_size_mm),
            sum_body_mass_g = sum(body_mass_g),
            min_body_mass_g = min(body_mass_g),
            mean_body_mass_g = mean(body_mass_g),
            max_body_mass_g = max(body_mass_g)) %>%
  ungroup()

# export to .csv - These are the data that show the summarized mm and g for all polyps per run
write_csv(size_sum, "bodySizeSum.csv")

#import respirometry run info
run <- read_csv("respirometry_run.csv") %>%
  clean_names()

#merge run and size_sum where the time_start and box is the same
all <- left_join(run, size_sum, by=c("date", "time_start", "box", "channel", "genet", "tank", "run")) %>%
  select(-polyps.x, -comments, -temperature.y) %>%
  rename(polyps = polyps.y, temperature = temperature.x) %>%
  select(-min_body_size_mm, -mean_body_size_mm, -max_body_size_mm, -min_body_mass_g, -mean_body_mass_g, -max_body_mass_g) %>%
  mutate(sum_body_size_mm = round(sum_body_size_mm, 4),
         sum_body_mass_g = round(sum_body_mass_g, 4))

# export to .csv - These are the merged data that will be copied over to a new sheet in Excel "respirometry_info.xlsx" file
write_csv(all, "bodySizeAll.csv")
