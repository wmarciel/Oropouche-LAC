#Figure 3.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)

##### Figure 3. panel C ######

df_3C_preoutbreak <- read_excel("Figure 3C.xlsx", sheet = "Pre-outbreak")

P_3C_pre <- ggplot(df_3C_preoutbreak, aes(x = Group, y = `RU/mL IgG`, fill = Group)) +
            geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.6) +
            geom_jitter(shape = 21, fill = "#c4c8d6", color = "black",    
                        position = position_jitter(width = 0.15),
                        size = 3.5, alpha = 0.8) +
            scale_fill_manual(values = c("<50" = "#e4dfcc", "≥50" = "#888365")) +
            labs(title = "Pre-outbreak", x = "Age Group", y = "OROV IgG - RU/ml", fill = NULL) +
            geom_hline(yintercept = 22, linetype = "dashed", color = "darkgray") +
            scale_y_continuous(limits = c(0, 215), breaks = seq(0, 200, by = 50)) +
            theme_classic() +
            theme(legend.position = "none") +
            annotate("text", x = 1.5, y = 210, label = "p = 0.2586", size = 4) #Wilcoxon rank sum test (Lines# 30)
P_3C_pre

wilcox.test(`RU/mL IgG` ~ Group, data = df_3C_preoutbreak)
# Wilcoxon rank sum test with continuity correction
# data:  RU/mL IgG by Group
# W = 768.5, p-value = 0.2586
# alternative hypothesis: true location shift is not equal to 0


#2. Post-outbreak
df_3C_postoutbreak <- read_excel("Figure 3C.xlsx", sheet = "Post-outbreak")

P_3C_post <- ggplot(df_3C_postoutbreak, aes(x = Group, y = `RU/mL IgG`, fill = Group)) +
              geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.6) +
              geom_jitter(shape = 21, fill = "#c4c8d6", color = "black",    
                          position = position_jitter(width = 0.15),
                          size = 3.5, alpha = 0.8) +
              scale_fill_manual(values = c("<50" = "#e4dfcc", "≥50" = "#888365")) +
              labs(title = "Post-outbreak", x = "Age Group", y = "OROV IgG - RU/ml", fill = NULL) +
              geom_hline(yintercept = 22, linetype = "dashed", color = "darkgray") +
              scale_y_continuous(limits = c(0, 215), breaks = seq(0, 200, by = 50)) +
              theme_classic() +
              theme(legend.position = "none") +
              annotate("text", x = 1.5, y = 210, label = "p = 0.2227", size = 4) #Wilcoxon rank sum test (Lines# 312)
P_3C_post

wilcox.test(`RU/mL IgG` ~ Group, data = df_3C_postoutbreak)
# Wilcoxon rank sum test with continuity correction
# data:  RU/mL IgG by Group
# W = 4715.5, p-value = 0.2227
# alternative hypothesis: true location shift is not equal to 0


##### Figure 3. panel E ######

df_3E <- read.csv("Figure 3E.csv")

df_3E$Type <- factor(df_3E$Type, levels = c("IgG Positive", "IgG Negative", "IgM Positive"))

P_3E <- ggplot(df_3E, aes(x = Category, y = Value, group = Donor.ID)) +
        geom_line(color = "gray") +  
        geom_point(aes(fill = Type), shape = 21, color = "black", size = 5) +
        scale_fill_manual(values = c("IgG Negative" = "#cccccc", "IgG Positive" = "#ebc74c", "IgM Positive" = "#6a5522"),
                          name = NULL) +
        geom_hline(yintercept = 22, linetype = "dashed", color = "gray") +
        labs(x = NULL, y="RU/mL IgG value", color = NULL) +
        scale_y_continuous(limits = c(0, 225), breaks = seq(0, 200, by = 50)) +
        theme_classic() +
        theme(legend.position=c(0.4, 0.95), 
              legend.direction = "horizontal")
P_3E