phones <- read.csv("../data/phones/phones.csv", stringsAsFactors = TRUE)
phones$resolution_Mpx <- phones$height_px*phones$width_px/1000000
phones <- phones[,-c(9, 10)]

summary(phones)

library(ggplot2)
library(DALEX)

dbar_plot_data <- data.frame(brand = forcats::fct_lump(phones$brand, 9))

brand <- ggplot(dbar_plot_data, aes(x = brand)) +
  geom_histogram(color = "#371ea3", fill = "#8bdcbe", stat = "count") +
  labs(subtitle = "Bar plot for brand variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

back_camera <- ggplot(data = phones, aes(x = back_camera_mpix)) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  labs(subtitle = "Histogram for back_camera_mpix variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy()
  
front_camera <- ggplot(data = phones, aes(x = front_camera_mpix)) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  labs(subtitle = "Histogram for front_camera_mpix variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy()

battery_mAh <- ggplot(data = phones, aes(x = battery_mAh)) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  labs(subtitle = "Histogram for battery_mAh variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy()

ram_gb <- ggplot(data = phones, aes(x = ram_gb)) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  labs(subtitle = "Histogram for ram_gb variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy()

flash_gb <- ggplot(data = phones, aes(x = flash_gb)) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  labs(subtitle = "Histogram for flash_gb variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy()

diag <- ggplot(data = phones, aes(x = diag)) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  labs(subtitle = "Histogram for diag variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy()

resolution_Mpx <- ggplot(data = phones, aes(x = resolution_Mpx)) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  labs(subtitle = "Histogram for resolution_Mpx variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_bw() +
  theme_drwhy()

price  <- ggplot(data = phones, aes(x = price )) +
  geom_histogram(bins = 10, color = "#371ea3", fill = "#8bdcbe") +
  theme_bw() +
  labs(subtitle = "Histogram for price variable") +
  scale_y_continuous(limits = c(0, 210)) +
  theme_drwhy()

data_vis <- gridExtra::grid.arrange(back_camera, brand, front_camera, battery_mAh, ram_gb, flash_gb, flash_gb,
                        diag, resolution_Mpx, price, ncol = 2)

ggsave("../plots/data_vist.png", plot = data_vis, height = 12, width = 6)
