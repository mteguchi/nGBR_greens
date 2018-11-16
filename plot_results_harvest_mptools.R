#plot_results_climate_mptools


# Tomo Eguchi
# 1 February 2017

rm(list=ls())
library(ggplot2)
library(reshape2)
library(mptools)
library(dplyr)

source("nGBR_greens_fcns.R")

results.dir <- 'data/Harvest Models/'

results.file <- "Harvest_6000AF__1.0_SD10.mp"
#results.file <- "Harvest_6000AF_1.04_SD10_02012017.mp"
#results.file <- "Harvest_3000AF_SA_1.0_SD10.mp"

title.txt <- strsplit(results.file, ".mp")[[1]]
stageNames.title <- c('Hatchling', 'Pelagic juvenile',
                      'Benthic juvenile', 'Subadult',
                      'Maturing adult', 'Adult', "All")

stages <- c('ne', 'pj', 'bj', 'sa', 'ma', 'ad', "all")

out.list <- prep_mpfile(results.dir, results.file)

Region.names <- levels(out.list$dat$region)


# plots <- vector(mode = "list",
#                 length = length(stageNames.title) )

k <- k1 <- 1
for (k1 in 1:length(stages)){
  
  df2 <- out.list$df1[, c(stages[k1], "time", "region", "sex")]
  
  colnames(df2) <- c('abundance', 'time', "region", 'sex')
  
  for (k in 1:length(Region.names)){
    
    plot.R <- ggplot(data = filter(df2, region == Region.names[k])) +
      labs(title = paste0(title.txt,  
                          ': ', Region.names[k], 
                          " (", stageNames.title[k1], ")"),
           x = 'Time', y = 'log10(Abundance)') +
      geom_point(aes(x = time,
                     y = log10(abundance),
                     shape = sex),
                 size = 3) +
      scale_shape_manual(breaks = c('Female', 'Male'),
                         values = c("Female" = 16, 
                                    "Male" = 17)) + 
      #scale_shape(solid = TRUE) +
      
      theme(legend.position = c(0.9, 0.8),
            plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0('figures/', title.txt, "_", stageNames.title[k1],
                  "_", Region.names[k], '.png'),
           dpi = 600)
  }
  
}


