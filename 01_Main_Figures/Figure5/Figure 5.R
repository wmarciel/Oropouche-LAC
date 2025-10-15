#Figure 5.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)

##### Figure 5. panel A ######

#Figure 5A: 1) Map of the outbreaks extracted from the existing literature 
#           2) Distribution of the outbreaks between 1960s and 2020s by country 

load("Figure 5A_maps_1.RData") #data for creating the base maps
load("Figure 5A_maps_2.RData") #data of the outbreak locations
df_5A <- read.csv("Figure 5A.csv") #data for the outbreaks by decades

df_outbreakpoints_sf$Freq_cat <- factor(df_outbreakpoints_sf$Freq_cat, levels = c("Single", "Multiple"))

P_5A <- ggplot() +
        geom_sf(data = americas, fill = "#F2F2F2", color = "#B3B3B3", size = 1) +  # base map
        geom_sf(data = brazil_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = peru_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = frenchguiana_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = panama_simpl, fill = NA, color = "black", size = 0.05) +
        theme_void() +
        labs(title = NULL)
P_5A

P_5A_1 <- P_5A + 
          geom_sf(data = df_outbreakpoints_sf, aes(fill = Freq_cat), shape = 21, size = 3.5, color = "black") +
          scale_fill_manual(values = c("Multiple" = "#615e49", "Single" = "#e4dfcc")) +
          guides(fill = guide_legend(title = "Outbreaks"))  
P_5A_1

P_5A_2 <- ggplot(df_5A, aes(x= Decade, y = Freq, fill = Country)) +
          geom_bar(stat = "identity") +
          labs (x = NULL, y = "Number of recorded outbreaks", fill = NULL) +
          scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by =2)) +
          scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
          scale_fill_manual(values = c("Panama" = "#a7a285",
                                       "Brazil" = "#e4dfcc",
                                       "Peru" = "#888365",
                                       "French Guiana" = "#c7c1a6"))+
          theme_classic() +
          theme(legend.direction = "horizontal",
                legend.position = c(0.4, 0.95))
P_5A_2


##### Figure 5. panel B #####

#Figure 5B: 1) Map of the serological studies extracted from the existing literature 
#           2) Distribution of the serological studies between 1960s and 2020s by country 

load("Figure 5B_maps_1.RData") #data for creating the base maps
load("Figure 5B_maps_2.RData") #data of the serological surveys locations
df_5B <- read.csv("Figure 5B.csv") #data for the serological surveys by decades

df_surveypoints_sf$Freq_cat <- factor(df_surveypoints_sf$Freq_cat, levels = c("Single", "Multiple"))

P_5B <- ggplot() +
        geom_sf(data = americas, fill = "#F2F2F2", color = "#B3B3B3", size = 1) + # base map
        geom_sf(data = brazil_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = peru_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = bolivia_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = colombia_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = costa_rica_simpl, fill = NA, color = "black", size = 0.05) +
        geom_sf(data = ecuador_simpl, fill = NA, color = "black", size = 0.05) +
        theme_void() +
        labs(title = NULL)
P_5B

P_5B_1 <- P_5B + 
          geom_sf(data = df_surveypoints_sf, aes(fill = Freq_cat), shape = 21, size = 3.5, color = "black") +
          scale_fill_manual(values = c("Multiple" = "#9b772C", "Single" = "#f6db89")) +
          guides(fill = guide_legend(title = "Serosurvey"))  
P_5B_1

P_5B_2 <- ggplot(df_5B, aes(x= Decade, y = Freq, fill = Country)) +
          geom_bar(stat = "identity") +
          labs (x = NULL, y = "Number of serosurvey", fill = NULL) +
          scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by =10)) +
          scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
          scale_fill_manual(values = c("Bolivia" = "#fff1c3",
                                       "Brazil" = "#f6db89",
                                       "Colombia" = "#ebc74c",
                                       "Costa Rica" = "#ca9e2c", 
                                       "Ecuador" = "#9b772c", 
                                       "Peru" = "#6a5522"))+
          theme_classic() +
          theme(legend.direction = "horizontal",
                legend.position = c(0.35, 0.9))
P_5B_2


##### Figure 5. panel C #####

df_5C <- read.csv("Figure 5C.csv")

df_5C$Status <- factor(df_5C$Status, levels = c("pre-outbreak", "post-outbreak"))

P_5C <- ggplot(df_5C, aes(x = Status, y = Prevalence, fill = Status)) +
        geom_boxplot(outlier.shape = NA) +  
        theme_classic() +
        theme(legend.position = c(0.3, 0.95), 
              legend.direction = "horizontal") +
        labs(x = NULL, y = "Seroprevalence (%)", fill = NULL) +
        scale_fill_manual(values = c("#0072b2", "#449343")) + 
        annotate("text", x = 1.3, y = 70, 
                 label = "p < 0.0001", size = 3.5) #Wilcoxon rank sum test (Lines# 118)

P_5C

wilcox.test(Prevalence ~ Status, data = df_5C)
# Wilcoxon rank sum test with continuity correction
# data:  Prevalence by Status
# W = 436, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0



##### Figure 5. panel D #####

df_5D <- read.csv("Figure 5D.csv")

df_5D$Country <- factor(df_5D$Country, levels = c("Brazil", "Peru", "Bolivia", "Venezuela", "Colombia",
                                                  "Ecuador", "Guyana", "Suriname", "French Guiana"))

P_5D <- ggplot(df_5D, aes(x = Country, y = disease_burden_round)) +  # x = each row as a factor
        geom_bar(stat = "identity", fill = "#0072b2") +  # bars with heights from A
        geom_errorbar(aes(ymin = disease_burden_round, ymax = disease_burden_round_Q3), width = 0.2) +  # error bars using B
        labs(title = "Pre-outbreak", x = NULL, y = "OROV infections") +
        scale_y_continuous(limits = c(0, 511340),             
                           breaks = seq(0, 500000, by = 100000))+  # Breaks every 1,000,000
        theme_classic() +
        theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
P_5D


##### Figure 5. panel E #####

df_5E <- read.csv("Figure 5E.csv")

df_5E$Country <- factor(df_5E$Country, levels = c("Brazil", "Cuba","Peru", "Bolivia", "Colombia",  
                                                  "Ecuador", "Panama","Dominican Republic","Guyana", "Barbados"))

P_5E <- ggplot(df_5E, aes(x = Country, y = disease_burden_round)) +  # x = each row as a factor
        geom_bar(stat = "identity", fill = "#449343") +  # bars with heights from A
        geom_errorbar(aes(ymin = disease_burden_round_Q1, ymax = disease_burden_round_Q3), width = 0.2) +  # error bars using B
        labs(title = "Post-outbreak", x = NULL, y = "OROV infections") +
        scale_y_continuous(limits = c(0, 12000000),
                           breaks = seq(0, 12000000, by = 1000000))+  # Breaks every 1,000,000
        theme_classic() +
        theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
P_5E