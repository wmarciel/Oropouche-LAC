#Figure S1.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)


##### Figure S1. panel A ######

df_S1A <- read.csv("Figure S1A.csv")

df_S1A$Category <- factor(df_S1A$Category, levels = c("Manaus","Amazonas","Brazil"))

P_S1A <- ggplot(df_S1A, aes(x = as.factor(Category), y = Value, color = Category)) +
          geom_point(position = position_dodge(width = 0.5), size = 2.5) +
          geom_errorbar(
            aes(ymin = Lower_95, ymax = Upper_95),
            position = position_dodge(width = 0.5),
            width = 0.3) +
          scale_y_continuous(limits = c(0, 0.154), breaks = seq(0, 0.15, by = 0.05)) +
          scale_color_manual(values = c("Brazil" = "black",
                                        "Amazonas"       = "#5e8fc6",
                                        "Manaus"   = "#6ba945"))+
          labs(x = "Arboviruses", y = "Ttesting positivity rate", color = NULL) +
          theme_classic() +
          theme(legend.position = c(0.8, 0.9))
P_S1A



##### Figure S1. panels B & C ######

#Figure S1B: surveillance capacity index for Manaus (city), Amazonas (state), and Brazil (national)
#Figure S1C: optimal travel time (hours) for Manaus (city), Amazonas (state), and Brazil (national)

df_S1BC <- read.csv("Figure S1BC.csv")

df_S1BC$Location <- factor(df_S1BC$Location, levels = c("Brazil", "Amazonas", "Manaus"))
df_S1BC$Group <- factor(df_S1BC$Group, levels = c("Relative surveillance capacity", "Optimal travel time"))

#Having two panels side by side
P_S1BC <- ggplot(df_S1BC, aes(x = Location, y = Median)) +
          geom_point(size = 3, color = "black") +
          geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
          facet_wrap(~ Group, scales = "free_y") +
          theme_classic() +
          labs(y = "Surveillance capacity index", title = NULL, x = NULL)
P_S1BC