

rm(list = ls())

library(ggplot2)
library(reshape2)
#library(mptools)

sysInfo <- Sys.info()
runDate <- Sys.Date()
tBegin <- Sys.time()

# Change the directory structure according to the computer
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

F0 <- dir('data/Climate Models/')
F1 <- F0[grep('.csv', F0)]

# data were extracted from the .mp files using Matlab:
stageNames <- c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf',
                'nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')


dat <- vector(mode = "list",
              length = length(F1))

plots <- vector(mode = "list",
                length = length(F1) )

for (k in 1:length(F1)){
  datafile <- paste0('data/Climate Models/', F1[k])
  dat0 <- read.csv(file = datafile, header = F)
  colnames(dat0) <- stageNames
  dat[[k]] <- dat0[3:dim(dat0)[1],]
  dat[[k]]$time <- seq(from = 0, to = 200, by = 5)
  df1 <- dat[[k]]

  ext <- ifelse(length(grep("Extreme", F1[k])) == 1, 'Ext', 'Mod')

  if (length(grep('M4F', F1[k])) == 1){
    trtmt <- '-M4F'
  } else if (length(grep('M5F', F1[k])) == 1){
    trtmt <- '-M5F'
  } else if (length(grep('M60F', F1[k])) == 1){
    trtmt <- '-M60F'
  } else if (length(grep('M65F', F1[k])) == 1){
    trtmt <- '-M65F'
  } else if (length(grep('30000neM_neF_p', F1[k])) == 1){
    trtmt <- '+30000neM+30000neF'
  } else if (length(grep('60000neM', F1[k])) == 1){
    trtmt <- '+60000neM'
  } else if (length(grep('30000neM_p', F1[k])) == 1){
    trtmt <- '+30000neM'
  }

  plots[[k]] <- ggplot(data = df1) +
    labs(title = paste0(ext, trtmt)) +
    geom_point(aes(x = time,
                   y = adf), size = 3)+
    geom_point(aes(x = time, y = adm))

  #plots[[k]]
  ggsave(paste0('figures/',
               unlist(strsplit(F1[k], '.csv')), '.png'),
         dpi = 600)
}


