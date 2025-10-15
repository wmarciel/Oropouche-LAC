#Figure 2.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)

##### Figure 2. panel A ######

df_2A <- read.csv("Figure 2A.csv")
                  
df_2A$Category <- factor(df_2A$Category, levels = c("Nov 2023", "Jun 2024", "Nov 2024"))
                  
P_2A <- ggplot(data5, aes(x = Category, y = Estimate)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
        labs(y = "OROV IgG detection (%)", x = NULL) +
        scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
        theme_classic()
P_2A


##### Figure 2. panel B ######

df_2B <- read.csv("Figure 2B.csv")

df_2B$Category <- factor(df_2B$Category, levels = c("Nov 2023", "Jun 2024", "Nov 2024"))

P_2B <- ggplot(df_2B, aes(x = Category, y = Estimate)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
        labs(y = "OROV IgM detection (%)", x = NULL) +
        scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
        theme_classic()
P_2B


##### Figure 2. panel C #####

df_2C <- read.csv("Figure 2C.csv")

df_2C$IgG <- factor(df_2C$IgG, levels = c("Positive", "Undetermined", "Negative"))

P_2C <- ggplot(df_2C, aes(x = IgG, y = `RU.mL.IgG`)) +
        geom_point(aes(fill = `FRNT.result` == "Positive"), 
                   shape = 21, color = "black", 
                   position = position_jitter(width = 0.2), size = 3.5, alpha = 0.7) +
        scale_fill_manual(values = c("TRUE" = "#69a547", "FALSE" = "#5e8dc5")) +  # TRUE = Positive, FALSE = Negative
        labs(x = NULL, y = "OROV IgG - RU/ml", fill = "FRNT result") +
        geom_hline(yintercept = 22, linetype = "dashed", color = "black") + #The cutoff for IgG Positive
        geom_hline(yintercept = 16, linetype = "dashed", color = "black") + #The cutoff for IgG negative
        theme_classic() +
        theme(legend.position = c(0.9, 0.9)) 

P_2C


##### Figure 2. panel D #####

load("Figure 2D.RData")

#1. November 2023 serosurvey
P_2D_Nov23 <- ggplot(df_2D_Nov2023_seroprevalence, aes(x = mean_age, y = observed)) +
  geom_point(size = 3) +                                   # dots for mean
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # error bars
  labs(title = "Nov 2023", x = "Age Group", y = "OROV IgG detection (fraction)") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  theme_classic()
P_2D_Nov23

P_2D_Nov23_1 <- P_2D_Nov23 +
                geom_line(data = df_2D_Nov2023_Rsero, aes(x = Age, y = mean), color = "#45567A", size = 1, inherit.aes = FALSE) +
                geom_ribbon(data = df_2D_Nov2023_Rsero, aes(x = Age, ymin = `lower.2.5%`, ymax = `upper.97.5%`), 
                            fill = "#849EB9", alpha = 0.15, inherit.aes = FALSE) 

P_2D_Nov23_1


#2. June 2024 serosurvey
P_2D_Jun24 <- ggplot(df_2D_Jun2024_seroprevalence, aes(x = mean_age, y = observed)) +
              geom_point(size = 3) +                                   # dots for mean
              geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # error bars
              labs(title = "Jun 2024", x = "Age Group", y = "OROV IgG detection (fraction)") +
              scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.2)) +
              scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
              theme_classic()
P_2D_Jun24

P_2D_Jun24_1 <- P_2D_Jun24 +
                geom_line(data = df_2D_Jun2024_Rsero, aes(x = Age, y = mean), color = "#45567A", size = 1, inherit.aes = FALSE) +
                geom_ribbon(data = df_2D_Jun2024_Rsero, aes(x = Age, ymin = `lower.2.5%`, ymax = `upper.97.5%`), 
                            fill = "#849EB9", alpha = 0.15, inherit.aes = FALSE) 

P_2D_Jun24_1


#3. November 2024 serosurvey
P_2D_Nov24 <- ggplot(df_2D_Nov2024_seroprevalence, aes(x = mean_age, y = observed)) +
              geom_point(size = 3) +                                   # dots for mean
              geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # error bars
              labs(title = "Nov 2024", x = "Age Group", y = "OROV IgG detection (fraction)") +
              scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.2)) +
              scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
              theme_classic()
P_2D_Nov24

P_2D_Nov24_1 <- P_2D_Nov24 +
                geom_line(data = df_2D_Nov2024_Rsero, aes(x = Age, y = mean), color = "#45567A", size = 1, inherit.aes = FALSE) +
                geom_ribbon(data = df_2D_Nov2024_Rsero, aes(x = Age, ymin = `lower.2.5%`, ymax = `upper.97.5%`), 
                            fill = "#849EB9", alpha = 0.15, inherit.aes = FALSE) 

P_2D_Nov24_1