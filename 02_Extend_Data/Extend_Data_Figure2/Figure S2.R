#Figure S2.

library(pacman)
pacman::p_load(sf, raster, dplyr, readxl, ggplot2, ggpubr, tidyverse, hrbrthemes, tmap,
               ggbreak, ggridges, tidyr, geobr, openxlsx, lubridate, biscale, cowplot, 
               scales, corrplot, ggcorrplot,stringr, RColorBrewer, ggpattern, patchwork, 
               terra, raster, Rsero)


##### Figure S2. all panels ######

#Figure S2: monthly index P plot for Amazonas state at the municipality level (n=62)

load("Figure S2.RData")

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

plots <- list()
for (m in months) {
  fill_col <- paste0(m, "_cat")
  plots[[m]] <- ggplot(spatial_indexP_AM) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "black", size = 0.01) +
    scale_fill_manual(
      values = c("<1"     = "white", #when index-P less than 1, then it is unsuitable
                 ">1-2"  = "#c9e3f4",
                 ">2-3" = "#9dc3e6",
                 ">3-4"   = "#5d8dc7",
                 ">4-5" = "#2f66a6",
                 ">5-6" = "#224883",
                 ">6-7"  = "#1d2a4e", #here, >6-7 and >7-8 used the same color
                 ">7-8" = "#1d2a4e"),
      na.value = "grey90",
      name = "Index P") +
    labs(title = m) +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"),
          panel.grid = element_blank())
}

# Example for accessing monthly plots (Jan to Dec)
plots$Jan  
plots$Feb  
plots$Mar  
plots$Apr  
plots$May  
plots$Jun  
plots$Jul  
plots$Aug  
plots$Sep  
plots$Oct  
plots$Nov  
plots$Dec  
