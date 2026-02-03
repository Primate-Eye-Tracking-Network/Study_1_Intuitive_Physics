# This scripts combines the i2mc output with tobii data, in order to compare the two fixations. It generates another data file that will be read by the visualisation script.

library(tidyverse)
library(shiny)


toi <- read_csv('https://docs.google.com/spreadsheets/d/1U8GY4A_Km1MCNk3HLHWA4l8c7-bbGLUqiYnM7IbQ2Fs/gviz/tq?tqx=out:csv&sheet=Tabellenblatt1',
                col_select = c(video_file:condition, contains('apple'), ip_test_event_start = `IP_test_event_start check_NEWCORRECT`)) %>% 
  filter(video_file != 'attentioncheck.mp4')

df_tobii_raw <- read_delim('data/raw/PET_network_study1_Ngamba_raw_new_AOIs.tsv') %>% 
  rename_with(str_to_lower) %>% 
  rename_with(~gsub('[ (]', '', (gsub(' ', '_', .x)))) %>% 
  rename_with(~gsub('\\)', '', .x)) %>% 
  mutate(recname = str_c(participant_name, '_', str_extract(recording_name, '[:digit:]+'))) %>% 
  select(where(~!all(is.na(.))))

df_tobii <- df_tobii_raw %>%
  left_join(toi, by = join_by(presented_media_name == video_file)) %>% 
  group_by(recname, presented_media_name) %>% 
  mutate(media_timestamp = recording_timestamp - first(recording_timestamp),
         toi = case_when(media_timestamp >= ip_test_event_start * 1000 ~ T,
                         media_timestamp < ip_test_event_start * 1000 ~ F),
         session_type = case_when(str_detect(timeline_name, 'cont|test') ~ 'exp', 
                                  TRUE ~ str_extract(timeline_name, 'calibration|qualitycheck'))) %>% 
  ungroup()  


df_i2mc <- read_csv('data/temp/i2mc_csv.csv', col_select = c(i2mc_index = `...1`, everything()))

df_test <- df_tobii %>% 
  filter(str_detect(presented_media_name, 'cont|test')) %>% 
  left_join(df_i2mc, by = join_by(recname, recording_timestamp >= startT, recording_timestamp <= endT)) %>% 
  mutate(media_timestamp = media_timestamp/1000000) %>% 
  select(recording_timestamp, participant_name:recording_date, recording_start_time, timeline_name, recording_resolution_height,
         recording_resolution_width, average_calibration_accuracy_degrees, average_calibration_precision_sd_degrees,
         event:gaze_point_right_y, pupil_diameter_left:validity_right, presented_stimulus_name:fixation_point_y, recname:fixRangeY)

df_test %>% write_csv('data/temp/i2mc_fix.csv')
