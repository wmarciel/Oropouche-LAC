#Figure S3.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)


##### Figure S3. panel A ######

df_S3A <- read.csv("Figure S3A.csv")

df_S3A$date <- as.Date(df_S3A$date, format = "%m/%d/%y")

P_S3A <- ggplot(df_S3A, aes(x=date, y=mean_7, group = 1)) +
          geom_line(size = 0.25) +
          scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
          geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
          labs(x = "Year", y = "Suitability index - index P") +
          scale_y_continuous(limits = c(0, 18), breaks = seq(0, 15, by = 5)) +
          theme_classic()
P_S3A


##### Figure S3. panel B #####

df_S3B <- read.csv("Figure S3B.csv")

P_S3B <- ggplot(df_S3B, aes(x = factor(Year), y = n_suitable_days, fill = Season)) +
          geom_bar(stat = "identity") +
          scale_y_continuous(limits = c(0, 366), breaks = seq(0, 360, by = 30)) +
          scale_fill_manual(
            values = c("Wet" = "#519d78", "Dry" = "#8BCF8B")) +
          geom_hline(yintercept = 180, linetype = "dashed", color = "black") +
          labs(x = "Year", y = "Number of suitable days", 
               fill = "Season") +
          theme_classic() +
          theme(legend.direction = "horizontal",
                legend.position = c(0.2, 0.95))
P_S3B