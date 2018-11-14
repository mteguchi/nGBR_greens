#plot_results_climate_mptools


# Tomo Eguchi
# 1 February 2017

# run extract_results_climate_mptools.R first to extract all age classes

rm(list=ls())
library(ggplot2)
library(reshape2)

results.dir <- 'data/Climate Models/'

# data were extracted from the .mp files using Matlab:
# now it is doen in extract_results_climate_mptools.R
stageNames <- c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf',
                'nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')

stageNames.title <- c('Hatchling', 'Pelagic juvenile',
                      'Benthic juvenile', 'Subadult',
                      'Maturing adult', 'Adult')

F0 <- dir(results.dir)
#F1 <- F0[grep('_02012017_', F0)]
#F1 <- F0[grep('Extreme_Climate_M55F', F0)]
F1 <- F0[grep('Extreme_Climate_M30F_harvest3000', F0)]
F1 <- F1[grep('.csv', F1)]
# dat <- vector(mode = "list",
#               length = length(F1))

plots <- vector(mode = "list",
                length = length(F1) )
k <- 1
for (k in 1:length(F1)){
  # pop <- unlist(strsplit(unlist(strsplit(unlist(strsplit(F1[k],
  #                                                        '_02012017_'))[2],
  #                                        '.csv')), 'pop'))[2]
  pop <- unlist(strsplit(unlist(unlist(strsplit(F1[k], '.csv'))), 'pop'))[2]

  datafile <- paste0(results.dir, F1[k])
  dat0 <- read.csv(file = datafile, header = F)
  colnames(dat0) <- stageNames
  dat <- dat0[3:dim(dat0)[1],]  # why start from 3? matches with 0 to 200 by 5
  dat$time <- seq(from = 0, to = 200, by = 5)
  #df1 <- dat[[k]]

  ext <- ifelse(length(grep("Extreme", F1[k])) == 1, 'Ext', 'Mod')

  if (length(grep('M4F', F1[k])) == 1){
    trtmt <- '-M4F'
  } else if (length(grep('M5F', F1[k])) == 1){
    trtmt <- '-M5F'
  } else if (length(grep('M60F', F1[k])) == 1){
    trtmt <- '-M60F'
  } else if (length(grep('M65F', F1[k])) == 1){
    trtmt <- '-M65F'
  } else if (length(grep('30000neM_neF_', F1[k])) == 1){
    trtmt <- '+30000neM+30000neF'
  } else if (length(grep('60000neM', F1[k])) == 1){
    trtmt <- '+60000neM'
  } else if (length(grep('30000neM_', F1[k])) == 1){
    trtmt <- '+30000neM'
  } else if (length(grep('M30F_harvest3000am3000af', F1[k])) == 1){
    trtmt <- 'M30F_H3000am3000af'
  } else if (length(grep('M30F_harvest3000af3000sa', F1[k])) == 1){
    trtmt <- 'M30F_H3000af3000sa'
  }

  df1.f <- dat[, c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf', 'time')]
  df1.f$sex <- "Female"
  colnames(df1.f) <- c('ne', 'pj', 'bj', 'sa', 'ma', 'ad', 'time', 'sex')

  df1.m <- dat[, c('nem', 'pjm', 'bjm', 'sam', 'mam', 'adm', 'time')]
  df1.m$sex <- "Male"
  colnames(df1.m) <- c('ne', 'pj', 'bj', 'sa', 'ma', 'ad', 'time', 'sex')

  df1 <- rbind(df1.f, df1.m)
  df1$sex <- as.factor(df1$sex)
  k1 <- 1
  for (k1 in 1:6){
    if (k1 == 6){
      df2 <- df1[, c(k1, 7, 8)]

      colnames(df2) <- c('abundance', 'time', 'sex')

      plots[[k]] <- ggplot(data = df2) +
        labs(title = paste0(trtmt,  ': Region ', pop),
             x = 'Time', y = 'Abundance') +
        geom_point(aes(x = time,
                       y = abundance,
                       shape = sex),
                   size = 3) +
        scale_shape_manual(breaks = c('Female', 'Male'),
                           values = c(17, 15)) +  # filled triangle and filled square

        #scale_shape(solid = TRUE) +

        theme(legend.position = c(0.9, 0.5),
              plot.title = element_text(hjust = 0.5))

      # ggsave(paste0('figures/', stageNames.title[k1], '_Region', pop,
      #               '_Climate_', trtmt, '_Feb2017.png'),
      #        dpi = 600)
      ggsave(paste0('figures/', stageNames.title[k1], '_Region', pop,
                    '_Climate_', trtmt, '_Aug2017.png'),
             dpi = 600)


    }

  }

}

