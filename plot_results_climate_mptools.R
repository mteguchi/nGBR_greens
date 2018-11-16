#plot_results_climate_mptools


# Tomo Eguchi
# 1 February 2017

# run extract_results_climate_mptools.R first to extract all age classes
#plot_results_climate_mptools


# Tomo Eguchi
# 1 February 2017

rm(list=ls())
library(ggplot2)
library(reshape2)
library(mptools)
library(dplyr)

source("nGBR_greens_fcns.R")

results.dir <- 'data/Climate Models/'

#results.file <- "Extreme_Climate_M30F_harvest3000af3000sa.mp"
results.file <- "Moderate_Climate_M10F_02012017.mp"
file.base <- title.txt <- strsplit(results.file, ".mp")[[1]][1]

stageNames.title <- c('Hatchling', 'Pelagic juvenile',
                      'Benthic juvenile', 'Subadult',
                      'Maturing adult', 'Adult', "All")

stages <- c('ne', 'pj', 'bj', 'sa', 'ma', 'ad', "all")

out.list <- prep_mpfile(results.file)

Region.names <- levels(out.list$dat$region)

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

