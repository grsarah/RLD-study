
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

df <- read_csv("FILTERED DATA.csv") %>%
  mutate(OUTCOME = case_when(
    (RESULT == "HIT" | RESULT == "GOAL" | RESULT == "BEHIND" | RESULT == "NO SCORE") ~ 'EFFECTIVE',
    (RESULT == "NEUTRAL" | (RESULT == "NO DISPOSAL" & lead(TEAM_BALL == "STOPPAGE", 1)) | (RESULT == "NO DISPOSAL" & lead(TEAM_BALL == "OFFENCE WIN", 1))) ~ 'NEUTRAL',
    (RESULT == "MISS" | (RESULT == "NO DISPOSAL" & lead(TEAM_BALL == "DEFENCE WIN", 1))) ~ 'INEFFECTIVE'
  )) %>% 
  #drop_na(PHASE_DURATION) %>% 
 # mutate(Frequency.bin = cut(PHASE_START, breaks = seq(from = 0, to = 9000, by = 120))) %>%
  mutate(Freq_dur = 60/PHASE_DURATION) %>% 
  drop_na(Freq_dur) %>% 
  select(#Frequency.bin, 
    PHASE, PHASE_DURATION, Freq_dur, everything())


# df.press <- df %>% 
#   group_by(PHASE, WEEK, SETTING) %>% 
#   summarise(mean = mean(PRESSURE_POINTS, na.rm = T))
# 
# df.press1 <- df.press %>% 
#   group_by(WEEK, SETTING) %>% 
#   summarise(mean = mean(mean, na.rm = T))
# 
# write_csv(df.press1, "PRESSURE.csv")

# create new dataframe with relevant columns standardised per hour.

#df.filtered <- df %>%
 # select(Frequency.bin, PHASE, PHASE_START, PHASE_END, PHASE_DURATION, DATE, WEEK, DAY, DATE, SETTING, NUMBER, START_TIME, DURATION, CARRIER_DENSITY, DIRECTION, DISPOSAL, DISTANCE, GAIN_POSS, KEY_MOMENT, LOCATION, MOVEMENT, PRESSURE, RECEIVER_DENSITY, RESULT, TEAM_BALL, LOCATION_START, LOCATION_END, X_VALUE_START, Y_VALUE_START, X_VALUE_END, Y_VALUE_END, DIST_TO_GOAL_START, DIST_TO_GOAL_END, DIST_TO_OPPO_GOAL_START, DIST_TO_OPPO_GOAL_END, ZONE_START, ZONE_END, IN50_ENTRY, REBOUND_50, PRESSURE_POINTS, CHAIN_METRES_GAINED, CHAIN_METRES_PER_SEC, OUTCOME)

# select relevant columns and apply loop to standardise for time

#df.relative <- df.filtered %>%
df.filtered <- df %>% 
  dplyr::group_by(DATE, SETTING, WEEK, PHASE, PHASE_DURATION) %>%
  filter(PHASE_DURATION > 0) %>% 
  summarise(
    # TEAM WON BALL
    OFF_WIN_PERMIN = (sum(TEAM_BALL == "OFFENCE WIN", na.rm = T)*Freq_dur), 
    DEF_WIN_PERMIN = (sum(TEAM_BALL == "DEFENCE WIN", na.rm = T)*Freq_dur), 
    STOPPAGE_PERMIN = (sum(TEAM_BALL == "STOPPAGE", na.rm = T)*Freq_dur),
    #POSS TYPE
    MARK_PERMIN = (sum(GAIN_POSS == "MARK", na.rm = T)*Freq_dur), 
    GP_PERMIN = (sum(GAIN_POSS == "GP", na.rm = T)*Freq_dur), 
    GB_PERMIN = (sum(GAIN_POSS == "GROUNDBALL-GET", na.rm = T)*Freq_dur), 
    #DISPOAL TYPE
    KICKS_PERMIN = (sum(DISPOSAL == "KICK", na.rm = T)*Freq_dur), 
    HB_PERMIN = (sum(DISPOSAL == "HANDBALL", na.rm = T)*Freq_dur), 
    TACKLED = (sum(DISPOSAL == "TACKLED", na.rm = T)*Freq_dur),
    NO_DISP_PERMIN = (sum(DISPOSAL == "NO DISPOSAL", na.rm = T)*Freq_dur),
  #  #TIP
    DURATION_0_1S_PERMIN = (sum(DURATION <=1, na.rm = T)*Freq_dur), 
    DURATION_1_2S_PERMIN = (sum(DURATION >1 & DURATION <=2, na.rm = T)*Freq_dur), 
    DURATION_2_3S_PERMIN = (sum(DURATION >2 & DURATION <=3, na.rm = T)*Freq_dur), 
    DURATION_MORE3S_PERMIN = (sum(DURATION >3, na.rm = T)*Freq_dur), 
    #MOVEMENT
    DYNAMIC_PERMIN = (sum(MOVEMENT == "DYNAMIC", na.rm = T)*Freq_dur), 
    STATIONARY_PERMIN = (sum(MOVEMENT == "STATIONARY", na.rm = T)*Freq_dur), 
    #PRESSURE
    PHYSICAL_PER_MIN = (sum(PRESSURE == "PHYSICAL", na.rm = T)*Freq_dur), 
    CLOSING_PER_MIN = (sum(PRESSURE == "CLOSING", na.rm = T)*Freq_dur), 
    CHASING_PER_MIN = (sum(PRESSURE == "CHASING", na.rm = T)*Freq_dur), 
    CORRALLING_PER_MIN = (sum(PRESSURE == "CORRALLING", na.rm = T)*Freq_dur), 
    NO_PRESSURE_PER_MIN = (sum(PRESSURE == "NO PRESSURE", na.rm = T)*Freq_dur),
  PRESSURE_POINTS_PER_MIN = (sum(PRESSURE_POINTS, na.rm = T)*Freq_dur), 
    #CARRIER DENSITY
    MORE3_OPPO_PERMIN = (sum(CARRIER_DENSITY == "MORE 3 OPPO", na.rm = T)*Freq_dur), 
    THREE_OPPO_PERMIN = (sum(CARRIER_DENSITY == "3 OPPO", na.rm = T)*Freq_dur), 
    TWO_OPPO_PER_MIN = (sum(CARRIER_DENSITY == "2 OPPO", na.rm = T)*Freq_dur), 
    ONE_OPPO_PER_MIN = (sum(CARRIER_DENSITY == "1 OPPO", na.rm = T)*Freq_dur), 
    NO_OPPO_PER_MIN = (sum(CARRIER_DENSITY == "NO OPP", na.rm = T)*Freq_dur), 
    #DISTANCE
    SHORT_DIST_PERMIN = (sum(DISTANCE == "0-25M", na.rm = T)*Freq_dur), 
    MEDIUM_DIST_PERMIN = (sum(DISTANCE == "25-40M", na.rm = T)*Freq_dur), 
    LONG_DIST_PERMIN = (sum(DISTANCE == "40", na.rm = T)*Freq_dur), 
    DIST_NO = (sum(DISTANCE == "NO DISPOSAL", na.rm = T)*Freq_dur),
    #RECEIVER DENSITY
    UNCONTESTED_PER60MIN = (sum(RECEIVER_DENSITY == "UNCONTESTED", na.rm = T)*Freq_dur),
    EVEN_PER60MIN = (sum((RECEIVER_DENSITY == "EVEN") | (RECEIVER_DENSITY == "CONTEST"), na.rm = T)*Freq_dur),
    SUPERIOR_PER60MIN = (sum(RECEIVER_DENSITY == "SUPERIOR", na.rm = T)*Freq_dur),
    INFERIOR_PER60MIN = (sum(RECEIVER_DENSITY == "INFERIOR", na.rm = T)*Freq_dur),
    LEADING_PER60MIN = (sum(RECEIVER_DENSITY == "LEADING", na.rm = T)*Freq_dur),
    SHOT_GOAL_PER60MIN = (sum(RECEIVER_DENSITY == "SHOT AT GOAL", na.rm = T)*Freq_dur),
    NO_RECIEVER_PER60MIN = (sum(RECEIVER_DENSITY == "NO RECEIVER", na.rm = T)*Freq_dur),
    RECIEVE_NO = (sum(RECEIVER_DENSITY == "NO DISPOSAL", na.rm = T)*Freq_dur),
    #OUTCOME
    EFFECTIVE_PERMIN = (sum(OUTCOME == "EFFECTIVE", na.rm = T)*Freq_dur),
    INEFFECTIVE_PERMIN = (sum(OUTCOME == "INEFFECTIVE", na.rm = T)*Freq_dur),
    NEUTRAL_PERMIN = (sum(OUTCOME == "NEUTRAL", na.rm = T)*Freq_dur)) %>%
  distinct() %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  ungroup() 

write_csv(df.relative, "RELATIVE DATA.csv")
  
  df.descriptive <- df.filtered %>%
   # group_by(SETTING, WEEK, DATE) %>%
    group_by(SETTING, WEEK) %>%
   # drop_na() %>% 
    summarise(
      AVE_OFF_WIN_PERMIN = mean(OFF_WIN_PERMIN),
      SD_OFF_WIN_PERMIN = sd(OFF_WIN_PERMIN),
      AVE_DEF_WIN_PERMIN = mean(DEF_WIN_PERMIN), 
      SD_DEF_WIN_PERMIN = sd(DEF_WIN_PERMIN), 
      AVE_STOPPAGE_PERMIN = mean(STOPPAGE_PERMIN),
      SD_STOPPAGE_PERMIN = sd(STOPPAGE_PERMIN),
      #POSS TYPE
      AVE_MARK_PERMIN = mean(MARK_PERMIN),
      SD_MARK_PERMIN = sd(MARK_PERMIN),
      AVE_GP_PERMIN = mean(GP_PERMIN), 
      SD_GP_PERMIN = sd(GP_PERMIN), 
      AVE_GB_PERMIN = mean(GB_PERMIN), 
      SD_GB_PERMIN = sd(GB_PERMIN), 
      #DISPOSAL TYPE
      AVE_KICKS_PERMIN = mean(KICKS_PERMIN), 
      SD_KICKS_PERMIN = sd(KICKS_PERMIN), 
      AVE_HB_PERMIN = mean(HB_PERMIN), 
      SD_HB_PERMIN = sd(HB_PERMIN), 
      AVE_TACKLED = mean(TACKLED),
      SD_TACKLED = sd(TACKLED),
      AVE_NO_DISP_PERMIN = mean(NO_DISP_PERMIN),
      SD_NO_DISP_PERMIN = sd(NO_DISP_PERMIN),
      #TIP
      AVE_DURATION_0_1S_PERMIN = mean(DURATION_0_1S_PERMIN), 
      SD_DURATION_0_1S_PERMIN = sd(DURATION_0_1S_PERMIN), 
      AVE_DURATION_1_2S_PERMIN = mean(DURATION_1_2S_PERMIN), 
      SD_DURATION_1_2S_PERMIN = sd(DURATION_1_2S_PERMIN), 
      AVE_DURATION_2_3S_PERMIN = mean(DURATION_2_3S_PERMIN), 
      SD_DURATION_2_3S_PERMIN = sd(DURATION_2_3S_PERMIN), 
      AVE_DURATION_MORE3S_PERMIN = mean(DURATION_MORE3S_PERMIN), 
      SD_DURATION_MORE3S_PERMIN = sd(DURATION_MORE3S_PERMIN), 
      #MOVEMENT
      AVE_DYNAMIC_PERMIN = mean(DYNAMIC_PERMIN), 
      SD_DYNAMIC_PERMIN = sd(DYNAMIC_PERMIN), 
      AVE_STATIONARY_PERMIN = mean(STATIONARY_PERMIN), 
      SD_STATIONARY_PERMIN = sd(STATIONARY_PERMIN), 
      #PRESSURE
      AVE_PHYSICAL_PER_MIN = mean(PHYSICAL_PER_MIN), 
      SD_PHYSICAL_PER_MIN = sd(PHYSICAL_PER_MIN), 
      AVE_CLOSING_PER_MIN = mean(CLOSING_PER_MIN), 
      SD_CLOSING_PER_MIN = sd(CLOSING_PER_MIN), 
      AVE_CHASING_PER_MIN = mean(CHASING_PER_MIN), 
      SD_CHASING_PER_MIN = sd(CHASING_PER_MIN), 
      AVE_CORRALLING_PER_MIN = mean(CORRALLING_PER_MIN), 
      SD_CORRALLING_PER_MIN = sd(CORRALLING_PER_MIN), 
      AVE_NO_PRESSURE_PER_MIN = mean(NO_PRESSURE_PER_MIN), 
      SD_NO_PRESSURE_PER_MIN = sd(NO_PRESSURE_PER_MIN), 
      #CARRIER DENSITY
      AVE_MORE3_OPPO_PERMIN = mean(MORE3_OPPO_PERMIN), 
      SD_MORE3_OPPO_PERMIN = sd(MORE3_OPPO_PERMIN), 
      AVE_THREE_OPPO_PERMIN = mean(THREE_OPPO_PERMIN), 
      SD_THREE_OPPO_PERMIN = sd(THREE_OPPO_PERMIN), 
      AVE_TWO_OPPO_PER_MIN = mean(TWO_OPPO_PER_MIN), 
      SD_TWO_OPPO_PER_MIN = sd(TWO_OPPO_PER_MIN), 
      AVE_ONE_OPPO_PER_MIN = mean(ONE_OPPO_PER_MIN), 
      SD_ONE_OPPO_PER_MIN = sd(ONE_OPPO_PER_MIN), 
      AVE_NO_OPPO_PER_MIN = mean(NO_OPPO_PER_MIN), 
      SD_NO_OPPO_PER_MIN = sd(NO_OPPO_PER_MIN), 
      #DISTANCE
      AVE_SHORT_DIST_PERMIN = mean(SHORT_DIST_PERMIN), 
      SD_SHORT_DIST_PERMIN = sd(SHORT_DIST_PERMIN), 
      AVE_MEDIUM_DIST_PERMIN = mean(MEDIUM_DIST_PERMIN), 
      SD_MEDIUM_DIST_PERMIN = sd(MEDIUM_DIST_PERMIN), 
      AVE_LONG_DIST_PERMIN = mean(LONG_DIST_PERMIN), 
      SD_LONG_DIST_PERMIN = sd(LONG_DIST_PERMIN),
      AVE_NO_DIST = mean(DIST_NO),
      SD_NO_DIST = sd(DIST_NO),
      #RECEIVER DENSITY
      AVE_UNCONTESTED_PERMIN = mean(UNCONTESTED_PERMIN),
      SD_UNCONTESTED_PERMIN = sd(UNCONTESTED_PERMIN),
      AVE_EVEN_PERMIN = mean(EVEN_PERMIN),
      SD_EVEN_PERMIN = sd(EVEN_PERMIN),
      AVE_SUPERIOR_PERMIN = mean(SUPERIOR_PERMIN),
      SD_SUPERIOR_PERMIN = sd(SUPERIOR_PERMIN),
      AVE_INFERIOR_PERMIN = mean(INFERIOR_PERMIN),
      SD_INFERIOR_PERMIN = sd(INFERIOR_PERMIN),
      AVE_LEADING_PERMIN = mean(LEADING_PERMIN),
      SD_LEADING_PERMIN = sd(LEADING_PERMIN),
      AVE_SHOT_GOAL_PERMIN = mean(SHOT_GOAL_PERMIN),
      SD_SHOT_GOAL_PERMIN = sd(SHOT_GOAL_PERMIN),
      AVE_NO_RECIEVER_PERMIN = mean(NO_RECIEVER_PERMIN),
      SD_NO_RECIEVER_PERMIN = sd(NO_RECIEVER_PERMIN),
      AVE_RECIVEER_NO = mean(RECIEVE_NO),
      SD_RECIVEER_NO = sd(RECIEVE_NO),
      #OUTCOME
      AVE_EFFECTIVE_PERMIN = mean(EFFECTIVE_PERMIN),
      SD_EFFECTIVE_PERMIN = sd(EFFECTIVE_PERMIN),
      AVE_INEFFECTIVE_PERMIN = mean(INEFFECTIVE_PERMIN),
      SD_INEFFECTIVE_PERMIN = sd(INEFFECTIVE_PERMIN),
      AVE_NEUTRAL_PERMIN = mean(NEUTRAL_PERMIN),
      SD_NEUTRAL_PERMIN = sd(NEUTRAL_PERMIN)
    ) %>%
    mutate_if(is.numeric, round, digits = 2)
  

write_csv(df.descriptive, "DESCRIPTIVE DATA2.csv")

