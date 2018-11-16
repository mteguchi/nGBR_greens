#extract_results_base_mptools.R

# Uses mptools to extract data from .mp files for making plots

# Tomo Eguchi
# 1 February 2017

rm(list = ls())

library(ggplot2)
library(reshape2)
library(mptools)

sysInfo <- Sys.info()
runDate <- Sys.Date()
tBegin <- Sys.time()

#nPops <- 4
#results.dir <- 'data/Base Models/'
#results.file <- 'Base1.0_SD10_02012017.mp'

# results.dir <- 'data/Harvest Models/'
# results.file <- 'Harvest_6000AF__1.0_SD10.mp'

results.dir <- 'data/Climate Models/'
#results.file <- "Extreme_Climate_add_30000neM.mp"
results.file <- "Extreme_Climate_add_60000neM.mp"


file.base <- unlist(strsplit(results.file, split = '.mp'))
results.all <- results(paste0(results.dir, results.file))

# total abundance
All.N <- data.frame(results.all$results[, , "ALL"])
All.N$time <- 1:dim(All.N)[1]
R1.N <- data.frame(results.all$results[, , "Region 1"])
R1.N$time <- 1:dim(All.N)[1]
R2.N <- data.frame(results.all$results[, , "Region 2"])
R2.N$time <- 1:dim(All.N)[1]
R3.N <- data.frame(results.all$results[, , "Region 3"])
R3.N$time <- 1:dim(All.N)[1]
R4.N <- data.frame(results.all$results[, , "Region 4 (Unknow"])
R4.N$time <- 1:dim(All.N)[1]

all.df <- list('Base - All' = All.N, 'Base - Region 1' = R1.N,
               'Base - Region 2' = R2.N, 'Base - Region 3' = R3.N,
               'Base - Region 4' = R4.N)

plots <- vector(mode = "list",
                length = length(all.df) )
k <- 1
for (k in 1:length(all.df)){
  plots[[k]] <- ggplot(data = all.df[[k]]) +
    labs(title = names(all.df)[k],
         x = 'Time', y = 'Abundance') +
    geom_ribbon(aes(x=time, ymax = log10(max), ymin = log10(min)),
                alpha = 0.5, fill = 'gray') +
    geom_line(aes(x = time, y = log10(mean)),
              size = 3, color = 'black') +
    geom_line(aes(x = time, y = log10(min)), size = 2) +
    geom_line(aes(x = time, y = log10(max)), size = 2) +

    theme(legend.position = c(0.9, 0.5),
          plot.title = element_text(hjust = 0.5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))

  ggsave(plot = plots[[k]],
         filename = paste0('figures/', file.base, '_',
                           gsub(' ', '_', names(all.df)[k]), '.png'),
         dpi = 1200)

}




