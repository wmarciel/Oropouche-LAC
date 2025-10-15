#Figure 1.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)

##### Figure 1. panel A ######

df_1A <- read.csv("Figure 1A.csv")

df_1A$Category <- factor(df_1A$Category, 
                         levels = c("Manaus", "Amazonas", "Brazil"))

breaks_year_start <- unique(df_1A$Epi_week[grep("_01$", df_1A$Epi_week)])

P_1A <- ggplot(df_1A, aes(x = Epi_week, y = Incidence, 
                        color = Category, group = Category)) +
        geom_line(linewidth = 0.5) +
        scale_x_discrete(breaks = breaks_year_start,
                         labels = sub("_01$", "", breaks_year_start)) +
        labs(x = "Year (Epidemiological Week)", 
             y = "Incidence of Oropouche fever", color = NULL) +
        theme_classic() +
        scale_color_manual(values = c("Brazil" = "#4d4d4d",
                                      "Amazonas"       = "#5d8ec7",
                                      "Manaus"   = "#6aa947"))+
        theme(legend.position = c(0.4, 0.95),   
              legend.direction = "horizontal")  
P_1A


##### Figure 1. panel B #####

df_1B <- read_excel("Figure 1B.xlsx")

P_1B <- ggplot(df_1B, aes(x = POP, y = INC_OROV)) +
        geom_point(aes(fill = City_status), shape = 21, 
                   color = "black", size = 4, alpha = 0.6) +
        scale_fill_manual(values = c("Capital" = "#518942", 
                                     "Non-capital" = "#bebebe")) +
        scale_x_continuous(limits = c(0, 12000000), 
                           breaks = seq(0, 12000000, by = 2000000)) +
        scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
        geom_text(aes(label = ifelse(NM_MUN %in% c("Manaus (AM)", "Belém (PA)", 
                                                   "São Paulo (SP)", 
                                                   "Rio de Janeiro (RJ)", 
                                                   "Serra (ES)", 
                                                   "Teresina (PI)"), 
                                     NM_MUN, "")),
                               hjust = 0.5, vjust = -1.5, size = 3) +
         labs(y = "Incidence of Oropouche fever", 
              x = "Municipality population size", fill = NULL) +
         theme_classic() +
         theme(legend.position = c(0.85, 0.9))
P_1B


##### Figure 1. panel C #####

df_1C <- readRDS("Figure 1C.rds")

# 1. plot the map from December to May (wet season)
P_1C_wet <- ggplot(df_1C) +
        geom_sf(aes(fill = `Wet season`), color = "black", size = 0.1) +
        scale_fill_manual(
          values = c(">1-2"     = "#c9e3f4",
                     ">2-3"  = "#9dc3e6",
                     ">3-4" = "#5d8ec7",
                     ">4-5"   = "#2f66a6",
                     ">5-6" = "#224883",
                     ">6-8" = "#1d2a4e"),
          na.value = "grey90",name = "Index P") +
        labs(title = "December to May",subtitle = "2014-2024") +
        theme_minimal() +
        theme(legend.position = "right",
              plot.title = element_text(face = "bold"),
              panel.grid = element_blank())
P_1C_wet

# 2. add the location of Manaus city to the existing plot
point_df <- data.frame( lon = -60.0217,   # longitude 
                        lat = -3.1190,    # latitude 
                        name = "Manaus")

# 3. convert to sf with same CRS as map (SIRGAS 2000, EPSG:4674)
point_sf <- st_as_sf(point_df, coords = c("lon", "lat"), crs = 4674)

P_1C_wetseason <- P_1C_wet +
                  geom_sf(data = point_sf, color = "white", 
                          size = 3, shape = 21, fill = "white") +
                  annotate("text", x = -60.0217, y = -3.1190, label = "Manaus", 
                           hjust = 0.5, vjust = -1.0, size = 4, color = "white")
P_1C_wetseason

# 4. plot the map from June to November (dry season)
P_1C_dryseason <- ggplot(df_1C) +
                  geom_sf(aes(fill = `Dry season`), color = "black", size = 0.1)+
                  scale_fill_manual(
                    values = c(">1-2"     = "#c9e3f4",
                               ">2-3"  = "#9dc3e6",
                               ">3-4" = "#5d8ec7",
                               ">4-5"   = "#2f66a6",
                               ">5-6" = "#224883",
                               ">6-8" = "#1d2a4e"),
                    na.value = "grey90",name = "Index P") +
                  labs(title = "June to November",subtitle = "2014-2024") +
                  theme_minimal() +
                  theme(legend.position = "right",
                        plot.title = element_text(face = "bold"),
                        panel.grid = element_blank())
P_1C_dryseason


##### Figure 1. panel D #####

df_1D <- read.csv("Figure 1D.csv")

P_1D <- ggplot(df_1D, aes(x = Year, y = Median)) +
        geom_point(size = 3, color = "black") +            # median as dot
        geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                      width = 0.2, color = "black") +      # 95% CI as error bar
        scale_x_continuous(breaks = 2014:2024) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray") +
        scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
        labs(y = "Odds Ratio of the number of suitable days") +
        theme_classic() 
P_1D