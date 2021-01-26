# CONFIG ----
library('readxl')
library(dplyr)
library(tidyr)
library(car)
library(googlesheets4)

gs4_auth(
  cache = ".secrets",
  email = "bartek.granat@gmail.com"
)

# QUESTION 1 ----

# read data directly from google sheets
df1 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1qAykeCMSr4gbopXvPJqUrm2VhtgVyWnQmgnQOLL9dYo/edit#gid=0", 
   sheet = 'q1',
   col_names = c('answer','timestamp','market_knowledge','ml_knowledge','phone_purchase','ids','nick')
)
# split answers into features
df1 <- df1 %>%
  separate(
    answer, 
    c("feature_1", "feature_2", "feature_3"), 
    "-"
  )
# aggreagte data
result1 <- df1 %>% 
  select(c("feature_1", "feature_2", "feature_3")) %>% 
  gather(feature_nr, feature) %>%
  drop_na() %>%
  select(feature) %>%
  group_by(feature) %>%
  summarise('freq' = n()) %>% 
  mutate(freq_scaled = freq/max(freq))
# save results
saveRDS(result1, '../data/annotations/result_question_1.rds')

# QUESTION 2 ----
# read data directly from google sheets
df2 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1qAykeCMSr4gbopXvPJqUrm2VhtgVyWnQmgnQOLL9dYo/edit#gid=0", 
  sheet = 'q2',
  col_names = F
)
# separate phones ids
df2 <- df2 %>%
  separate(
    ...9, 
    c("id_1", "id_2", 'id_3', 'id_4'), 
    "-"
  )

col_names <- c(
  'question',
  'id'
)

# gather answers for each telephone
result2 <- rbind(
  df2 %>% select(1, id = id_1) %>% setNames(col_names),
  df2 %>% select(2, id = id_2) %>% setNames(col_names),
  df2 %>% select(3, id = id_3) %>% setNames(col_names)
) 
# separate order
result2 <- result2 %>%
  separate(
    question, 
    c("1", "2", '3','4','5','6','7','8'), 
    "-"
  )
# process results to numeric values
result2_proc <- data.frame()

for(i in 1:nrow(result2)) {
  row <- result2[i,]
  importance <- 1:8
  names(importance) <- row[1:8]
  res_row <- cbind(t(importance), row[9]) %>% select(brand, back_camera_mpix, front_camera_mpix, battery_mAh, flash_gb, ram_gb, diag, resolution_Mpx, id)
  result2_proc <- rbind(result2_proc, res_row)
}

saveRDS(result2_proc, '../data/annotations/result_question_2.rds')

# QUESTION 3 ----

# read data directly from google sheets
df3 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1qAykeCMSr4gbopXvPJqUrm2VhtgVyWnQmgnQOLL9dYo/edit#gid=0", 
  sheet = 'q3',
  col_names = F
)

df3 <- df3 %>%
  separate(
    ...21, 
    c("id_1", "id_2"), 
    "-"
  )

col_names <- c(
  'brand',
  'back_camera_mpix',
  'front_camera_mpix',
  'battery_mAh',
  'flash_gb',
  'ram_gb',
  'diag',
  'resolution_Mpx',
  'id'
)

result3 <- rbind(
  df3 %>% select(1:8, id = id_1) %>% setNames(col_names),
  df3 %>% select(9:16, id = id_2) %>% setNames(col_names)
) 

result3 <- result3 %>% 
  mutate_all(function(x) recode(x, '"Highly decreases"=-2; "Slightly decreases"=-1; "Neutral"=0; "Slightly increases"=1; "Highly increases"=2;'))

saveRDS(result3, '../data/annotations/result_question_3.rds')
