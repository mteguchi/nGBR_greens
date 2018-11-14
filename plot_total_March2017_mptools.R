#extract_results_base_mptools.R

# Uses mptools to extract data from .mp files for making plots
# run extract_results_climate_maptools.R first to create _stages.txt
# file.

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
# results.dir <- 'data/Base Models/'
# results.file <- 'Extreme_Climate.mp'
results.dir <- 'data/Climate Models/'
#results.file <- 'Extreme_Climate_M55F.mp'
#results.file <- 'Extreme_Climate_M30F_harvest.mp'
#results.file <- 'Moderate_Climate 10FperM.mp'
results.file <- 'Extreme_Climate_M30F_harvest3000af3000sa.mp'
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

all.df <- list('ALL' = All.N, 'Region 1' = R1.N,
               'Region 2' = R2.N, 'Region 3' = R3.N,
               'Region 4' = R4.N)

plots <- vector(mode = "list",
                length = length(all.df) )
k <- 1
for (k in 1:length(all.df)){
  plots[[k]] <- ggplot(data = all.df[[k]]) +
    labs(title = names(all.df)[k],
         x = 'Time (yr)', y = 'Abundance') +
    geom_ribbon(aes(x=time, ymax = max, ymin = min),
                alpha = 0.5, fill = 'gray') +
    geom_line(aes(x = time, y = mean),
              size = 3, color = 'black') +
    geom_line(aes(x = time, y = min), size = 2) +
    geom_line(aes(x = time, y = max), size = 2) +

    theme(legend.position = c(0.9, 0.5),
          plot.title = element_text(hjust = 0.5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))

  # ggsave(plot = plots[[k]],
  #        filename = paste0('figures/', file.base, '_',
  #                          gsub(' ', '_', names(all.df)[k]), '.png'),
  #        dpi = 1200)

}

# adult plots:
stage.data <- read.delim(file = paste0(results.dir, file.base, '_stages.txt'),
                         sep = ',', header = F)

colnames(stage.data) <- c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf',
                          'nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')
stage.data.region <- list(stage.data[1:42,], stage.data[43:84,],
                          stage.data[85:126,], stage.data[127:168,])

#stage.data$time <- seq(from = 0, to = (nrow(stage.data)-1), by = 1)
Region.names <- c('Region 1', 'Region 2', 'Region 3', 'Region 4')
k <- 1
for (k in 1:length(stage.data.region)){
  plot.data <- stage.data.region[[k]][,c('adf', 'adm')]
  colnames(plot.data) <- c('Female', 'Male')
  plot.data$time <- seq(from=0, to=209,by=5)
  plot.data.1 <- melt(data = plot.data, id = 'time',
                      value.name = 'abundance')
  colnames(plot.data.1) <- c('time', 'Sex', 'abundance')
  p1 <- ggplot(data = plot.data.1) +
    labs(title = paste0('Adult - ', Region.names[k]),
         x = 'Time (yr)', y = 'Abundance') +
    geom_point(aes(x = time,
                   y = abundance,
                   shape = Sex),
               size = 3) +
    scale_shape_manual(breaks = c('Female', 'Male'),
                       values = c(17, 15)) +  # filled triangle and filled square
    #geom_line(aes(x = time, y = adm),
    #          size = 3, color = 'black') +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.position = c(0.9, 0.5))

  ggsave(plot = p1,
         filename = paste0('figures/', file.base, '_Region', k,
                           '_adult.png'),
         dpi = 1200)

  # p2 <- ggplot(data = stage.data) +
  #   labs(title = 'Adult female',
  #        x = 'Time (yr)', y = 'Abundance') +
  #   geom_line(aes(x = time, y = adf),
  #             size = 3, color = 'black') +
  #   theme(plot.title = element_text(hjust = 0.5),
  #         axis.title = element_text(size = 12),
  #         axis.text = element_text(size = 12))
  #
  # ggsave(plot = p2,
  #        filename = paste0('figures/', file.base, '_adultFemale.png'),
  #        dpi = 1200)
  #
}
