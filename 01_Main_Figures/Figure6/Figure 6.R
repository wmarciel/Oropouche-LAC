#Figure 6.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)


##### Figure 6. panel A ######

#Figure 6A: 1) Arbovirus surveillance capacity index (0-1) pre-outbreak
#           2) Arbovirus surveillance capacity index (0-1) post-outbreak

df_6A_preoutbreak <- read_excel("Figure 6A.xlsx", sheet = "Pre-outbreak")
df_6A_postoutbreak <- read_excel("Figure 6A.xlsx", sheet = "Post-outbreak")

#1. Pre-outbreak
df_6A_preoutbreak$Country <- factor(df_6A_preoutbreak$Country, levels = c("Brazil", "Peru", "Bolivia", "Colombia", "Venezuela", 
                                                                          "Ecuador", "Guyana", "Suriname", "French Guiana"))
P_6A_1 <- ggplot(df_6A_preoutbreak, aes(x = Country, y = median)) +
          geom_point(size = 4, color = "#0273b2") +
          geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "gray40") +
          geom_hline(yintercept = 0.2246, linetype = "dashed", color = "gray") + #South America average
          scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by =0.2)) +
          labs(title = "Pre-outbreak", y = "Surveillance capacity index", x = NULL) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
P_6A_1


#2. Post-outbreak
df_6A_postoutbreak$Country <- factor(df_6A_postoutbreak$Country, levels = c("Brazil", "Peru", "Cuba","Bolivia", 
                                                                            "Colombia", "Ecuador", "Panama", "Dominican Republic",
                                                                            "Guyana", "Barbados"))
P_6A_2 <- ggplot(df_6A_postoutbreak, aes(x = Country, y = median)) +
          geom_point(size = 4, color = "#439243") +
          geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "gray40") +
          geom_hline(yintercept = 0.23365, linetype = "dashed", color = "gray") + #Latin America and Caribbean average
          scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by =0.2)) +
          labs(title = "Post-outbreak", y = "Surveillance capacity index", x = NULL) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
P_6A_2


##### Figure 6. panel B #####

#Figure 6B: 1) Optimal travel time to healthcare services pre-outbreak
#           2) Optimal travel time to healthcare services post-outbreak

df_6B_preoutbreak <- read_excel("Figure 6B.xlsx", sheet = "Pre-outbreak")
df_6B_postoutbreak <- read_excel("Figure 6B.xlsx", sheet = "Post-outbreak")

#1. Pre-outbreak
df_6B_preoutbreak$Country <- factor(df_6B_preoutbreak$Country, levels = c("Brazil", "Peru", "Bolivia", "Colombia", "Venezuela", "Ecuador", "Guyana", "Suriname", "French Guiana"))

P_6B_1 <- ggplot(df_6B_preoutbreak, aes(x = Country, y = median)) +
          geom_point(size = 4, color = "#0273b2") +
          geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "gray40") +
          geom_hline(yintercept = 2.1732, linetype = "dashed", color = "gray") + #South America average
          scale_y_continuous(limits = c(0, 48), breaks = seq(0, 48, by = 12)) +
          labs(title = "Pre-outbreak", y = "Travel time in hours", x = NULL) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
P_6B_1

#2. Post-outbreak
df_6B_postoutbreak$Country <- factor(df_6B_postoutbreak$Country, levels = c("Brazil", "Peru", "Cuba","Bolivia", 
                                                                            "Colombia", "Ecuador", "Panama", "Dominican Republic","Guyana", "Barbados"))
P_6B_2 <- ggplot(df_6B_postoutbreak, aes(x = Country, y = median)) +
          geom_point(size = 4, color = "#439243") +
          geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "gray40") +
          geom_hline(yintercept = 1.8835, linetype = "dashed", color = "gray") + #Latin America and Caribbean average
          scale_y_continuous(limits = c(0, 48), breaks = seq(0, 48, by = 12)) +
          labs(title = "Post-outbreak", y = "Travel time in hours", x = NULL) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
P_6B_2
