#Figure 4.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)

##### Figure 4. panel A ######

df_4A <- read.csv("Figure 4A.csv")

P_4A <- ggplot(df_4A, aes(x = Year_median, y = AttackRate_median)) +
        geom_point(size = 3, color = "black") +  
        geom_errorbar(aes(ymin = AttackRate_lower, ymax = AttackRate_upper), width = 0.1, color = "black") + # vertical whisker
        geom_segment(aes(x = Year_lower, xend = Year_upper, y = AttackRate_median, yend = AttackRate_median), size = 0.7, color = "black") +
        labs(x = "Year", y = "OROV - Attack Rate (%)") +
        scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
        scale_x_continuous(limits = c(1960, 2025), breaks = seq(1960, 2025, by = 10)) +
        geom_vline(xintercept = 2023, color = "darkgray", linetype = "dashed", size = 0.5) +
        theme_classic()
P_4A


##### Figure 4. panel B #####

load("Figure 4B.RData")

P_4B <- ggplot(df_4B, aes(x = Mean_age, y = Observed)) +
  geom_point(size = 3) +                                   
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +  
  labs(x = "Age Group", y = "OROV IgG detection (%)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
  theme_classic()
P_4B 

P_4B_1 <- P_4B +
          geom_line(data = df_4B_Modelestimate, aes(x = Age, y = Mean), color = "#45567A", size = 1, inherit.aes = FALSE) +
          geom_ribbon(data = df_4B_Modelestimate, aes(x = Age, ymin = `Lower.2.5%`, ymax = `Upper.97.5%`), 
                      fill = "#656f83", alpha = 0.15, inherit.aes = FALSE) 
P_4B_1


##### Figure 4. panel C #####

df_4C <- read.csv("Figure 4C.csv")

P_4C <- ggplot(df_4C, aes(x = Year_median, y = AttackRate_median)) +
        geom_point(size = 3, color = "black") +  
        geom_errorbar(aes(ymin = AttackRate_lower, ymax = AttackRate_upper), width = 0.1, color = "black") + # vertical whisker
        geom_segment(aes(x = Year_lower, xend = Year_upper, y = AttackRate_median, yend = AttackRate_median), size = 0.7, color = "black") +
        labs(x = "Year", y = "OROV - Attack Rate (%)") +
        scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
        scale_x_continuous(limits = c(1960, 2025), breaks = seq(1960, 2025, by = 10)) +
        geom_vline(xintercept = 2015, color = "darkgray", linetype = "dashed", size = 0.5) +
        theme_classic()
P_4C