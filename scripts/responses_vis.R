# CONFIG ----
library('readxl')
library(dplyr)
library(tidyr)
library(car)
library(data.table)
library(googlesheets4)
library(ggplot2)
library(gridExtra)
library(grid)


gs4_auth(
  cache = ".secrets",
  email = "bartek.granat@gmail.com"
)


##### script for visualization of form results ----


# read data directly from google sheets
df1 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1qAykeCMSr4gbopXvPJqUrm2VhtgVyWnQmgnQOLL9dYo/edit#gid=0", 
  sheet = 'q1',
  col_names = c('answer','timestamp','market_knowledge','ml_knowledge','phone_purchase','ids','nick')
)

df1 <- readRDS('charts/df1.rds')

df_personal <- df1[c('ml_knowledge','phone_purchase','market_knowledge')] %>% as.data.table()

p1 <- ggplot(df_personal) + 
  geom_bar(aes(x = factor(market_knowledge, 
              levels=c("Fundamental", 
                       "Novice",
                       "Intermediate", 
                       "Advanced",
                       "Professional")))) + 
  theme_bw() +
  labs(x = 'Market Knowledge Level', 'Number of respondents') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 
p2 <- ggplot(df_personal) + 
  geom_bar(aes(x = factor(ml_knowledge, 
                          levels=c("Fundamental", 
                                   "Novice",
                                   "Intermediate", 
                                   "Advanced",
                                   "Professional")))) + 
  theme_bw() +
  labs(x = 'ML Knowledge Level', 'Number of respondents') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p3 <- ggplot(df_personal) + 
  geom_bar(aes(x = factor(phone_purchase, 
                          levels=c("Less than 6 months ago", 
                                   "Between 1 years and 6 months ago",
                                   "Between 2 years and 1 year ago", 
                                   "More than 2 years ago")))) + 
  theme_bw() +
  labs(x = 'Time of purchase', 'Number of respondents') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

grid.arrange(p1, p2, nrow = 1, top=textGrob("Distributions of respondents knowledge levels",gp=gpar(fontsize=20,font=3)))

grid.arrange(p3, nrow = 1, top=textGrob("Distribution of respondents phone purchase date",gp=gpar(fontsize=20,font=3)))


cor(as.numeric(factor(df_personal$market_knowledge, 
           levels=c("Fundamental", 
                    "Novice",
                    "Intermediate", 
                    "Advanced",
                    "Professional"))), 
    as.numeric(factor(df_personal$ml_knowledge, 
           levels=c("Fundamental", 
                    "Novice",
                    "Intermediate", 
                    "Advanced",
                    "Professional"))))

