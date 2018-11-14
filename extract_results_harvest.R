

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

F0 <- dir('data/Harvest Models/')
F1 <- F0[grep('.csv', F0)]

# data were extracted from the .mp files using Matlab:
stageNames <- c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf',
                'nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')

# stageNames.title <- c('hatchling (F)', 'pelagic juvenile (F)',
#                       'benthic juvenile (F)', 'subadult (F)',
#                       'maturing adult (F)', 'adult (F)',
#                       'hatchling (M)', 'pelagic juvenile (M)',
#                       'benthic juvenile (M)', 'subadult (M)',
#                       'maturing adult (M)', 'adult (M)')


dat <- vector(mode = "list",
              length = length(F1))

plots <- vector(mode = "list",
                length = length(F1) )

for (k in 1:length(F1)){
  datafile <- paste0('data/Harvest Models/', F1[k])
  dat0 <- read.csv(file = datafile, header = F)
  colnames(dat0) <- stageNames
  dat[[k]] <- dat0[3:dim(dat0)[1],]
  dat[[k]]$time <- seq(from = 0, to = 200, by = 5)
  df1 <- dat[[k]]
  fname <- unlist(strsplit(F1[k], '.csv'))
  pname <- substr(fname, start = (nchar(fname)-3),
                  stop = nchar(fname))

  if (length(grep('3000AF_1000AM', F1[k])) == 1){
    trtmt <- 'Hvt-3000AdF, 3000AdM'
  } else if (length(grep('3000AF_300AM', F1[k])) == 1) {
    trtmt <- 'Hvt-3000AdF, 300AdM'
  } else if (length(grep('3000AF_AM', F1[k])) == 1){
    trtmt <- 'Hvt-3000AdF, 3000AdM'
  } else if (length(grep('4000AF', F1[k])) == 1){
    trtmt <- 'Hvt-4000AdF'
  } else if (length(grep('6000AF__1.0', F1[k])) == 1){
    trtmt <- 'Hvt-6000AdF'
  } else if (length(grep('6000AF_1.04', F1[k])) == 1){
    trtmt <- 'Mvt(+7)-6000AdF'
  } else if (length(grep('3000AF_SA', F1[k])) == 1){
    trtmt <- 'Hvt-3000AdF, 3000SubAd'
  }

  plots[[k]] <- ggplot(data = df1) +
    labs(title = paste(trtmt, pname, sep = " : "),
         y = 'Adult',
         x = 'Time') +

    geom_rect(aes(xmin = 20, xmax = 120, ymin = 0, ymax = Inf),
              alpha = 0.1) +

    geom_point(aes(x = time,
                   y = adf),
               size = 5) +
    geom_point(aes(x = time,
                   y = adm),
               shape = 17,
               color = 'black',
               fill = 'black',
               size = 5)

  ggsave(paste0('figures/',
                unlist(strsplit(F1[k], '.csv')), '.png'),
         dpi = 600)
}





