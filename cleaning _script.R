library(tidyverse)
library(janitor)

#Takes the sona data column names from row one to be assigned next
sona_names <- read_csv("raw_data/Confidence+Delay+-+SONA+-+Rhi_March+28,+2020_03.56.zip", n_max = 0) %>% names()

#Reads in the sona data, removes the meta data and assignsand cleans the column names
sona <- read_csv("raw_data/Confidence+Delay+-+SONA+-+Rhi_March+28,+2020_03.56.zip", 
                 col_names = sona_names, 
                 skip = 3) %>% 
                clean_names()

#Takes the M-Turk data column names from row one to be assigned next
mturk_names <- read_csv("raw_data/Confidence+Delay+-+MTurk_March+28,+2020_03.55.zip", n_max = 0) %>% names()

#Reads in the M-Turk data, removes the meta data and assigns and cleans the column names
mturk <- read_csv("raw_data/Confidence+Delay+-+MTurk_March+28,+2020_03.55.zip", 
                  col_names = mturk_names, 
                  skip = 3) %>% 
                  clean_names()

#Takes the public data column names from row one to be assigned next
public_names <- read_csv("raw_data/Confidence+Delay+-+Rhi_March+28,+2020_03.55.zip", n_max = 0) %>% names()
#Reads in the public data, removes the meta data and assigns and cleans the column names 
public <- read_csv("raw_data/Confidence+Delay+-+Rhi_March+28,+2020_03.55.zip", 
                   col_names = public_names,  
                   skip = 3) %>%
                   clean_names()


# cleaning mturk -------------------------------------------------------------------

mturk_clean <- mturk %>%
  separate(
    recorded_date,
    c("date", "time"),
    sep = " "
  ) %>%
  mutate(age = as.numeric(age)) %>%
  select(
    status,
    ip_address,
    progress,
    date,
    time,
    "id" = response_id,
    "video_164" = time_video164_page_submit,
    "video_170" = time_video170_page_submit,
    "video_173" = time_video173_page_submit,
    "video_176" = time_video176_page_submit,
    lineup164,
    lineup170,
    lineup173,
    lineup176,
    "hypothesis_disconfirmation" = q123_page_submit,
    "distraction" = q87_page_submit,
    "conf_scale" = conf_scale_1,
    attention1,
    attention2,
    gender,
    age,
    "seen_video_before" = q91
  ) %>%
  filter(
    status == 0,
    progress >= 98,
    age <= 99,
    is.na(seen_video_before) | seen_video_before == 2,
    attention1 == 3 | attention2 == 4) %>%
  group_by(ip_address, date, time) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  pivot_longer(starts_with("video"),
               names_to = "video",
               values_drop_na = T) %>%
  select(-value) %>%
  pivot_longer(starts_with("lineup"),
               names_to = "lineup",
               values_to = "lineup_response",
               values_drop_na = T) %>%
  mutate(gender = as.character(gender)) %>%
  mutate(
    gender = if_else(str_detect(gender, "1"), "Male", gender),
    gender = if_else(str_detect(gender, "2"), "Female", gender),
    gender = if_else(str_detect(gender, "3"), "Other", gender),
    gender = if_else(str_detect(gender, "4"), "Prefer not to say", gender)) %>%
  separate(video, c("remove", "video"), sep = "_") %>% 
  separate(lineup, c("remove1", "lineup"), sep = "p") %>% 
  select(-seen_video_before, -ip_address, -progress, -date, -time, -remove, -remove1, -status, -attention1, -attention2)

  

# cleaning sona -----------------------------------------------------------

sona_clean <- sona %>% 
  separate(
    recorded_date, 
    c("date", "time"), 
    sep = " "
  ) %>% 
  mutate(age = as.numeric(age)) %>% 
  select(
    status,
    ip_address,
    progress,
    date,
    time,
    "id" = response_id,
    "video_164" = time_video164_page_submit,
    "video_170" = time_video170_page_submit,
    "video_173" = time_video173_page_submit,
    "video_176" = time_video176_page_submit,
    lineup164,
    lineup170,
    lineup173,
    lineup176,
    "hypothesis_disconfirmation" = q123_page_submit,
    "distraction" = q87_page_submit,
    "conf_scale" = conf_scale_1,
    conf_scale_1,
    attention1,
    attention2,
    gender,
    age,
    "seen_video_before" = q91
  ) %>% 
  filter(
    status == "IP Address", 
    progress >= 98, 
    age <= 99, 
    is.na(seen_video_before) | seen_video_before == 2,
    attention1 == "To pick up a VCR" | attention2 == "A handbag") %>% 
  pivot_longer(starts_with("video"),
               names_to = "video",
               values_drop_na = T) %>% 
  select(-value) %>% 
  pivot_longer(starts_with("lineup"),
               names_to = "lineup",
               values_to = "lineup_response",
               values_drop_na = T) %>%
  mutate(lineup_response = 
           if_else(str_detect(lineup_response, "Not there"), "7", lineup_response)) %>%
  mutate(lineup_response = as.numeric(lineup_response)) %>%
  separate(video, c("remove", "video"), sep = "_") %>%
  separate(lineup, c("remove1", "lineup"), sep = "p") %>% 
  select(-seen_video_before, -ip_address, -progress, -date, -time, -remove, -remove1, -status, -attention1, -attention2)


# cleaning public ---------------------------------------------------------

public_clean <- public %>%
  separate(
    recorded_date,
    c("date", "time"),
    sep = " "
  ) %>%
  mutate(age = as.numeric(age)) %>%
  select(
    status,
    ip_address,
    progress,
    date,
    time,
    "id" = response_id,
    "video_164" = time_video164_page_submit,
    "video_170" = time_video170_page_submit,
    "video_173" = time_video173_page_submit,
    "video_176" = time_video176_page_submit,
    lineup164,
    lineup170,
    lineup173,
    lineup176,
    "hypothesis_disconfirmation" = q123_page_submit,
    "distraction" = q87_page_submit,
    "conf_scale" = conf_scale_1,
    conf_scale_1,
    attention1,
    attention2,
    gender,
    age,
    "seen_video_before" = q91
  ) %>%
  filter(
    status == 0,
    progress >= 98,
    age <= 99,
    is.na(seen_video_before) | seen_video_before == 2,
    attention1 == 3 | attention2 == 4) %>%
  group_by(ip_address, date, time) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  pivot_longer(starts_with("video"),
               names_to = "video",
               values_drop_na = T) %>%
  select(-value) %>%
  pivot_longer(starts_with("lineup"),
               names_to = "lineup",
               values_to = "lineup_response",
               values_drop_na = T) %>%
  mutate(gender = as.character(gender)) %>%
  mutate(
    gender = if_else(str_detect(gender, "1"), "Male", gender),
    gender = if_else(str_detect(gender, "2"), "Female", gender),
    gender = if_else(str_detect(gender, "3"), "Other", gender),
    gender = if_else(str_detect(gender, "4"), "Prefer not to say", gender)) %>%
  separate(video, c("remove", "video"), sep = "_") %>% 
  separate(lineup, c("remove1", "lineup"), sep = "p") %>% 
  select(-seen_video_before, -ip_address, -progress, -date, -time, -remove, -remove1, -status, -attention1, -attention2)



# combining tibbles -------------------------------------------------------

combined_clean <- mturk_clean %>%
  bind_rows(sona_clean, public_clean)
  

# Creating condition variable

combined_clean <- combined_clean %>% 
  mutate(
    condition = 
      ifelse(!is.na(hypothesis_disconfirmation), 
                    "hypothesis disconfirmation",
                    ifelse(!is.na(distraction), 
                                  "distraction", 
                                  "no delay")
             )
    ) %>% 
  select(-hypothesis_disconfirmation, -distraction)
             
# creating accuracy variable ----------------------------------------------

# Participants saw 1 of 4 videos and then the lineup was either target present or target absent.
# The videos were paired 164 & 173, 170 & 176. The linup for matching video was the target absent 
# lineup for the respective pair 

# Video 164 target = 6
# Video 170 target = 3
# Video 173 target = 5
# Video 176 target = 6
# 7 is always lineup rejected

combined_clean <- combined_clean %>% 
  mutate(accuracy = 
         ifelse(video == 164 & lineup == 164 & lineup_response == 6, 
                "correct", 
                ifelse(video == 164 & lineup == 173 & lineup_response == 7, 
                       "correct",
                       ifelse(video == 170 & lineup == 170 & lineup_response == 3, 
                              "correct",
                              ifelse(video == 170 & lineup == 176 & lineup_response == 7, 
                                     "correct",
                                     ifelse(video == 173 & lineup == 173 & lineup_response == 5, 
                                            "correct",
                                            ifelse(video == 173 & lineup == 164 & lineup_response == 7, 
                                                   "correct",
                                                   ifelse(video == 176 & lineup == 176 & lineup_response == 6, 
                                                          "correct",
                                                          ifelse(video == 176 & lineup == 170 & lineup_response == 7, 
                                                                 "correct", "incorrect"
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
                              )
                       )
                )
         ) %>% 
  mutate(confidence_bin = 
           ifelse(conf_scale <= 50,
                  1,
                  ifelse(conf_scale <= 80, 
                         2, 3)))


# Creating response variable ----------------------------------------------
# -3= Filler ID (TA)
# -2= Innocent ID (TA) (innocent is the assigned to the target actual target from the lineup)
# -1= Incorrect Rejection (TP)
# 0= Filler ID (TP)
# 1= Correct ID (TP)
# 2= Correct rejection (TA)

combined_clean <- combined_clean %>% 
  mutate(response = 
           ifelse(accuracy == "correct" & lineup_response == 7, "correct rejection",
                  ifelse(accuracy == "correct" & lineup_response != 7, "correct id",
                         ifelse(accuracy == "incorrect" & lineup_response == 7, "incorrect rejection",
                                ifelse(accuracy == "incorrect" & video == lineup, "filler id (tp)",
                                       ifelse(video == 164 & lineup == 173 & lineup_response == 5, 
                                                     "innocent id",
                                                     ifelse(video == 170 & lineup == 176 & lineup_response == 6, 
                                                                   "innocent id",
                                                                   ifelse(video == 173 & lineup == 164 & lineup_response == 6, 
                                                                                 "innocent id",
                                                                                 ifelse(video == 176 & lineup == 170 & lineup_response == 3, 
                                                                                               "innocent id", "filler id (ta)")
                                                                          )
                                                            )
                                              )
                                       )
                                )
                         )
                  )
         )
                                       
                                
                         
                  
         

#remove these variables
rm(public, public_names, public_clean, mturk, mturk_names, mturk_clean, sona, sona_names, sona_clean)

# write clean data to csv -------------------------------------------------

write_csv(combined_clean, "cleaned_data/conf_dely_clean.csv")
