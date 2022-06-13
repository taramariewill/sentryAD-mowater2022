library(lubridate)
library(tidyverse)
library(viridis)
library(plyr)
library(dplyr)

load("sentry_anaerobic.rda")

pilot_ccr <- sentry_anaerobic$pilot$ccr
pilot_lab <- sentry_anaerobic$pilot$lab_2020
full_scale_ccr <- sentry_anaerobic$full_scale$ccr

full_scale_ccr$datetime <- ymd_hms(full_scale_ccr$datetime)
pilot_ccr$datetime <- ymd_hms(pilot_ccr$datetime)
pilot_lab$datetime <- ymd(pilot_lab$datetime)

full_scale_d2 <- sentry_anaerobic$full_scale$d2

pilot_lab_2019 <- sentry_anaerobic$pilot$lab_2019
pilot_lab_2020 <- sentry_anaerobic$pilot$lab_2020
#PILOT SCALE CLEANING


daily_AD2I_temp1 <- pilot_ccr[1:100308,] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_AD2_Influent = mean(na.omit(ccr_ad2_influent))) |>
  mutate(datetime = ymd(paste0("2019-",mnt,"-",dy))) |>
  as.data.frame()
daily_AD2I_temp2 <- pilot_ccr[100309:434437,] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_AD2_Influent = mean(na.omit(ccr_ad2_influent))) |>
  mutate(datetime = ymd(paste0("2020-",mnt,"-",dy))) |>
  as.data.frame()
pilot_ccr_daily_D2_Influent <- rbind(daily_AD2I_temp1[, 3:4], daily_AD2I_temp2[, 3:4])

daily_AD2_temp1 <- pilot_ccr[1:100308,] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_AD2 = mean(na.omit(ccr_ad2_influent))) |>
  mutate(datetime = ymd(paste0("2019-",mnt,"-",dy))) |>
  as.data.frame()
daily_AD2_temp2 <- pilot_ccr[100309:434437,] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_AD2 = mean(na.omit(ccr_ad2))) |>
  mutate(datetime = ymd(paste0("2020-",mnt,"-",dy))) |>
  as.data.frame()
pilot_ccr_daily_D2 <- rbind(daily_AD2_temp1[, 3:4], daily_AD2_temp2[, 3:4])

daily_AD1_temp1 <- pilot_ccr[1:100308,] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_AD1 = mean(na.omit(ccr_ad2_influent))) |>
  mutate(datetime = ymd(paste0("2019-",mnt,"-",dy))) |>
  as.data.frame()
daily_AD1_temp2 <- pilot_ccr[100309:434437,] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_AD1 = mean(na.omit(ccr_ad1))) |>
  mutate(datetime = ymd(paste0("2020-",mnt,"-",dy))) |>
  as.data.frame()
pilot_ccr_daily_D1 <- rbind(daily_AD1_temp1[, 3:4], daily_AD1_temp2[, 3:4])

pilot_ccr_daily_D1$datetime <- ymd(pilot_ccr_daily_D1$datetime)
pilot_ccr_daily_D2$datetime <- ymd(pilot_ccr_daily_D2$datetime)
pilot_ccr_daily_D2_Influent$datetime <- ymd(pilot_ccr_daily_D2_Influent$datetime)

pilot_daily_temp<- merge(pilot_ccr_daily_D1[, 1:2],
                         pilot_ccr_daily_D2[, 1:2],
                         by = "datetime")
pilot_ccr_daily_ALL <- merge(pilot_daily_temp[1:3],
                             pilot_ccr_daily_D2_Influent[1:2], by = "datetime")
#FULL SCALE CLEANING




full_scale_ccr_daily_digester1 <- full_scale_ccr[c(1:386450),] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_digester = mean(na.omit(ccr_digestor))) |>
  mutate(datetime = ymd(paste0("2021-",mnt,"-",dy))) |>
  as.data.frame()
full_scale_ccr_daily_digester2 <- full_scale_ccr[c(386451:438796),] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_digester = mean(na.omit(ccr_digestor))) |>
  mutate(datetime = ymd(paste0("2022-",mnt,"-",dy))) |>
  as.data.frame()

full_scale_ccr_daily_digester <- rbind(full_scale_ccr_daily_digester1[,3:4],
                                       full_scale_ccr_daily_digester2[, 3:4])

full_scale_ccr_daily_slurry1 <- full_scale_ccr[c(1:386450),] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_slurry = mean(na.omit(ccr_slurry))) |>
  mutate(datetime = ymd(paste0("2021-",mnt,"-",dy))) |>
  as.data.frame()
full_scale_ccr_daily_slurry2 <- full_scale_ccr[c(386451:438796),] |>
  group_by(mnt = month(datetime), dy = day(datetime)) |>
  dplyr::summarize(daily_ccr_slurry = mean(na.omit(ccr_slurry))) |>
  mutate(datetime = ymd(paste0("2022-",mnt,"-",dy))) |>
  as.data.frame()

full_scale_ccr_daily_slurry <- rbind(full_scale_ccr_daily_slurry1[,3:4],
                                     full_scale_ccr_daily_slurry2[, 3:4])

full_daily_ccr <- merge(full_scale_ccr_daily_digester[,1:2],
                        full_scale_ccr_daily_slurry[, 1:2], by = "datetime")

full_scale_d2$datetime <- ymd(full_scale_d2$datetime)
full_d2_merged <- merge(full_scale_d2[, 1:45],
                        full_daily_ccr[, 1:3], by = "datetime")

#merging the 2019 and 2020 dataset
colnames(pilot_lab_2019)
colnames(pilot_lab_2020)

name_check_2019 <- colnames(pilot_lab_2019)
name_check_2020 <- colnames(pilot_lab_2020)
setdiff(name_check_2019, name_check_2020)
setdiff(name_check_2020, name_check_2019)

merging_pilot_2019 <- pilot_lab_2019
merging_pilot_2020 <- pilot_lab_2020

colnames(merging_pilot_2019)[colnames(merging_pilot_2019) %in% 
                               c("fog_TS_pct",
                                 "food_TS_pct",
                                 "primary_sludge_TS_avg_pct",
                                 "primary_sludge_volatile_acids_mgl",
                                 "primary_sludge_soluble_chemical_oxygen_mgl",
                                 "twas_TS_pct",
                                 "twas_volatile_acids_mgl",
                                 "twas_soluble_chemical_oxygen_mgl",
                                 "start_time_feed_pilot_1_hrs",
                                 "pilot_1_total_hours_of_feed",
                                 "stop_time_feed_pilot_2_hrs",
                                 "PD1_total_alkalinity_mgl",
                                 "PD1_soluable_chemical_oxygen_mgl",
                                 "PD1_volatile_solids_pct",
                                 "PD2_chemical_oxygen_demand_mgl",
                                 "PD2_total_solids_pct",
                                 "fog_soluble_chemical_oxygen_mgl",
                                 "food_soluble_chemical_oxygen_mgl",
                                 "primary_sludge_VS_avg_pct",
                                 "primary_sludge_chemical_oxygen_demand_mgl",
                                 "primary_sludge_total_alkalinity_mgl",
                                 "twas_VS_pct",
                                 "twas_chemical_oxygen_demand_mgl",
                                 "twas_total_alkalinity_mgl",
                                 "stop_time_feed_pilot_1_hrs",
                                 "start_time_feed_pilot_2_hrs",
                                 "pilot_2_total_hours_of_feed_hrs",
                                 "PD1_chemical_oxygen_demand_mgl",
                                 "PD1_total_solids_pct",
                                 "PD2_total_alkalinity_mgl",
                                 "PD2_soluable_chemical_oxygen_mgl",
                                 "PD2_volatile_solids_pct")]<-c("fog_total_solids_pct",
                                                                "food_total_solids_pct",
                                                                "PS_total_solids_avg_pct",
                                                                "PS_volatile_acids_mgl",
                                                                "PS_soluable_chemical_oxygen_demand_mgl",
                                                                "TWAS_total_solids_pct",
                                                                "TWAS_total_volatile_acids_mgl",
                                                                "TWAS_soluable_chemical_oxygen_demand_mgl",
                                                                "PD1_start_time_feed_hrs",
                                                                "PD1_total_hours_of_feed_hrs",
                                                                "PD2_stop_time_feed_hrs",
                                                                "PD1_alkalinity_mgl",
                                                                "PD1_sCOD_mgl",
                                                                "PD1_VS_pct",
                                                                "PD2_COD_mgl",
                                                                "PD2_TS_pct",
                                                                "fog_soluable_chemical_oxygen_demand_mgl",
                                                                "food_soluable_chemical_oxygen_demand_mgl",
                                                                "PS_VS_avg_pct",
                                                                "PS_chemical_oxygen_demand_mgl",
                                                                "PS_total_alkalinity_mgl",
                                                                "TWAS_VS_pct",
                                                                "TWAS_chemical_oxygen_demand_mgl",
                                                                "TWAS_total_alkalinity_mgl",
                                                                "PD1_stop_time_feed_hrs",
                                                                "PD2_start_time_feed_hrs",
                                                                "PD2_total_hours_of_feed_hrs",
                                                                "PD1_COD_mgl",
                                                                "PD1_TS_pct",
                                                                "PD2_alkalinity_mgl",
                                                                "PD2_sCOD_mgl",
                                                                "PD2_VS_pct")
setdiff(colnames(merging_pilot_2019), colnames(merging_pilot_2020))
setdiff(colnames(merging_pilot_2020), colnames(merging_pilot_2019))



full_pilot <- rbind.fill(merging_pilot_2019, merging_pilot_2020)
full_pilot$datetime <- ymd(full_pilot$datetime)


#merging full pilot and full pilot daily CCR

full_pilot_merged <- merge(full_pilot[,1:70],pilot_ccr_daily_ALL[,1:4], by = "datetime" )
pilot_2020_merged <- merge(pilot_lab_2020[,1:68], pilot_ccr_daily_ALL[,1:4], by = "datetime")


#END OF CLEANING
