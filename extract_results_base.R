

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

# Sets of parameters
lambdas <- c('1.0', '1.04')
SDs <- c('05', '10', '15')

lambda.label <- c('1', '7')

# data were extracted from the .mp files using Matlab:
stageNames <- c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf',
                'nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')

stageNames.title <- c('Hatchling', 'Pelagic juvenile',
                      'Benthic juvenile', 'Subadult',
                      'Maturing adult', 'Adult')


dat <- vector(mode = "list",
              length = length(lambdas)*length(SDs)*4)
scenarioNames <- vector(mode = "character",
                        length = length(lambdas)*length(SDs)*4)
c <- 1

for (k in 1:length(lambdas)){
  for (k1 in 1:length(SDs)){
    for (k2 in 1:4){
      datafile <- paste0('data/Base Models/Base', lambdas[k],
                         '_SD', SDs[k1], '_pop', k2, '.csv')
      dat0 <- read.csv(file = datafile, header = F)
      colnames(dat0) <- stageNames
      dat[[c]] <- dat0[3:dim(dat0)[1],]
      scenarioNames[c] <- paste0('pop', k2, '_',
                                 lambdas[k], '_SD', SDs[k1])
      c <- c + 1
    }
  }
}

# Now extract the same stages into different dataframes
dat1 <- dat2 <- vector(mode = "list",
                       length = length(stageNames))

for (k3 in 1:length(stageNames)){
  dat1[[k3]] <- do.call(cbind.data.frame,
                        lapply(dat,
                               FUN = function(x) x[,stageNames[k3]]))

  colnames(dat1[[k3]]) <- scenarioNames
  dat1[[k3]]$time <- seq(from = 0, to = 200, by = 5)

  # melt the dataframes for easier plotting
  dat2[[k3]] <- melt(dat1[[k3]], id = 'time')
  colnames(dat2[[k3]]) <- c("time", "scenarios", "abundance")
}


# for pop1 decreasing
for (k1 in 1:4){
  for (k2 in 1:length(lambdas)){
    # per stage
    plots <- vector(mode = "list",
                    length = length(stageNames) )

    for (k in 1:6){
      df0.f <- dat2[[k]]
      df0.m <- dat2[[k+6]]

      df1.f <- df0.f[grep(paste0('pop', k1, '_', lambdas[k2], '_'),
                          df0.f$scenarios, fixed = FALSE),]
      df1.f$sex <- "Female"
      df1.m <- df0.m[grep(paste0('pop', k1, '_', lambdas[k2], '_'),
                          df0.m$scenarios, fixed = FALSE),]
      df1.m$sex <- "Male"

      df1 <- rbind(df1.f, df1.m)
      df1$sex <- as.factor(df1$sex)

      plots[[k]] <- ggplot(data = df1) +
        labs(title = paste0(stageNames.title[k],
                            ' Base +', lambda.label[k2],
                            ': Region ', k1),
             x = 'Time', y = 'Abundance') +
        geom_jitter(aes(x = time,
                        y = abundance,
                        shape = factor(scenarios),
                        color = sex),
                    size = 3) +

        scale_shape_discrete(name = "CV",
                             labels = c("5%", "10%", "15%"),
                             solid = FALSE) +

        theme(legend.position = c(0.9, 0.5))
      ggsave(paste0('figures/', stageNames.title[k], '_Region', k1,
                    '_Base_', lambda.label[k2], '.png'),
             dpi = 600)

    }

  }

}





