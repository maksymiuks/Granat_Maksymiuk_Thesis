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

p1 <- 
  ggplot(df_personal) + 
  geom_bar(aes(x = factor(market_knowledge, 
              levels=c("Fundamental", 
                       "Novice",
                       "Intermediate", 
                       "Advanced",
                       "Professional"))),
           color = "#371ea3",
           fill = "#8bdcbe") + 
  theme_bw() +
  DALEX::theme_drwhy() +
  scale_y_continuous(limits = c(0, 20)) +
  labs(x = 'Market Knowledge Level', y = 'Number of respondents') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 
p2 <- ggplot(df_personal) + 
  geom_bar(aes(x = factor(ml_knowledge, 
                          levels=c("Fundamental", 
                                   "Novice",
                                   "Intermediate", 
                                   "Advanced",
                                   "Professional"))),
           color = "#371ea3",
           fill = "#8bdcbe") + 
  theme_bw() +
  DALEX::theme_drwhy() +
  scale_y_continuous(limits = c(0, 20)) +
  labs(x = 'ML Knowledge Level', y = 'Number of respondents') +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p3 <- ggplot(df_personal) + 
  geom_bar(aes(x = factor(phone_purchase, 
                          levels=c("Less than 6 months ago", 
                                   "Between 1 years and 6 months ago",
                                   "Between 2 years and 1 year ago", 
                                   "More than 2 years ago"))),
           color = "#371ea3",
           fill = "#8bdcbe") + 
  theme_bw() +
  DALEX::theme_drwhy() +
  scale_y_continuous(limits = c(0, 20)) +
  labs(x = 'Time of purchase', y = 'Number of respondents') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

p4 <- grid.arrange(p1, p2, nrow = 1)

ggsave("../plots/knowledge_levels2.png", plot = p4, height = 5, width = 10)

p5 <- grid.arrange(p3, nrow = 1)

ggsave("../plots/time_of_purchase2.png", plot = p5, height = 5, width = 5)


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

