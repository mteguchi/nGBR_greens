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
library(dplyr)

sysInfo <- Sys.info()
runDate <- Sys.Date()
tBegin <- Sys.time()

#nPops <- 4
#results.dir <- 'data/Base Models/'
#results.file <- "Base1.0_SD10.mp"  # these don't have stages... Use plot_results_base_mptools.R

# results.file <- 'Extreme_Climate.mp'
results.dir <- 'data/Climate Models/'

# uncomment one of the following lines and source this script
#results.file <- 'Extreme_Climate_M55F.mp'
#results.file <- 'Extreme_Climate_M30F_harvest.mp'
#results.file <- 'Moderate_Climate 10FperM.mp'
results.file <- 'Extreme_Climate_M30F_harvest3000af3000sa.mp'

climate.txt <- strsplit(results.file, "_")[[1]][1]

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
R4.N <- data.frame(results.all$results[, , "Region 4 (Unknow"]) # keep (Unknow - that's the column name
R4.N$time <- 1:dim(All.N)[1]

all.df <- list('All' = All.N, 'Region 1' = R1.N,
               'Region 2' = R2.N, 'Region 3' = R3.N,
               'Region 4' = R4.N)

plots <- vector(mode = "list",
                length = length(all.df) )
k <- 1
for (k in 1:length(all.df)){
  plots[[k]] <- ggplot(data = all.df[[k]]) +
    labs(title = paste(climate.txt, "-", names(all.df)[k]),
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

  ggsave(plot = plots[[k]],
         filename = paste0('figures/', file.base, '_',
                           gsub(' ', '_', names(all.df)[k]), '.png'),
         dpi = 1200)

}

# adult plots:
stage.data <- read.delim(file = paste0(results.dir, file.base, '_stages.txt'),
                         sep = ',', header = F)

if (ncol(stage.data) == 1)
  stage.data <- read.delim(file = paste0(results.dir, file.base, '_stages.txt'),
                           sep = " ", header = F)


colnames(stage.data) <- c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf',
                          'nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')
stage.data <- stage.data[,!is.na(names(stage.data))]

stage.data %>% 
  mutate(nefm = nef + nem, pjfm = pjf + pjm, 
         bjfm = bjf + bjm, safm = saf + sam, 
         mafm = maf + mam, adfm = adf + adm) -> stage.data

stage.data.region <- list(stage.data[1:42,], 
                          stage.data[43:84,],
                          stage.data[85:126,], 
                          stage.data[127:168,])

stage.data.all <- stage.data.region[[1]] + stage.data.region[[2]] +
  stage.data.region[[1]] + stage.data.region[[2]]
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
                       values = c("Female" = 16, 
                                  "Male" = 17)) + #geom_line(aes(x = time, y = adm),
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

# For adults
plot.data <- stage.data.all[,c('adf', 'adm')]
colnames(plot.data) <- c('Female', 'Male')
plot.data$time <- seq(from=0, to=209,by=5)
plot.data.1 <- melt(data = plot.data, id = 'time',
                    value.name = 'abundance')
colnames(plot.data.1) <- c('time', 'Sex', 'abundance')

p1 <- ggplot(data = plot.data.1) +
  labs(title = "Adult - All",
       x = 'Time (yr)', y = 'Abundance') +
  geom_point(aes(x = time,
                 y = abundance,
                 shape = Sex),
             size = 3) +
  scale_shape_manual(breaks = c('Female', 'Male'),
                     values = c("Female" = 16, 
                                "Male" = 17)) +
  #geom_line(aes(x = time, y = adm),
  #          size = 3, color = 'black') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = c(0.9, 0.5))


ggsave(plot = p1,
       filename = paste0('figures/', file.base, "_AllRegions",
                         '_adult.png'),
       dpi = 1200)

# for all of them, but separate sex
stage.data.all %>% select(c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf')) %>%
  rowSums() -> female.counts
all.females <- data.frame(abundance = female.counts,
                          sex = "Female",
                          time = seq(from = 0, to = 209, by = 5))

stage.data.all %>% select(c('nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')) %>%
  rowSums() -> male.counts
all.males <- data.frame(abundance = male.counts,
                        sex = "Male",
                        time = seq(from = 0, to = 209, by = 5))

plot.data.2 <- rbind(all.females, all.males)

p2 <- ggplot(data = plot.data.2) +
  labs(title = paste(climate.txt, "- All regions and age classes"),
       x = 'Time (yr)', y = 'Abundance') +
  geom_point(aes(x = time,
                 y = abundance,
                 shape = sex),
             size = 3) +
  scale_shape_manual(breaks = c('Female', 'Male'),
                     values = c("Female" = 16, 
                                "Male" = 17)) +  #geom_line(aes(x = time, y = adm),
  #          size = 3, color = 'black') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = c(0.9, 0.5))


ggsave(plot = p2,
       filename = paste0('figures/', file.base, "_AllRegions",
                         '_AllAges.png'),
       dpi = 1200)
