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
results.dir <- 'data/Base Models/'
results.file <- 'Base1.0_SD10_02012017.mp'
results.all <- results(paste0(results.dir, results.file))

# find average stage abundances:
# open file:
tmp1 <- readLines(paste0(results.dir, results.file))

idx0 <- grep('Constraints Matrix', tmp1)  # for initial abundance
idx1 <- grep('Average stage abundances', tmp1)  # beginning
idx2 <- grep('End of file', tmp1)

dim.stage.data <- as.numeric(unlist(strsplit(tmp1[(idx1+1)], " +")))
N0 <- matrix(as.numeric(unlist(strsplit(tmp1[(idx0+40):(idx0+43)],
                                        " +"))),
             ncol = 12, byrow = T)

stage.data <- matrix(as.numeric(unlist(strsplit(tmp1[(idx1+2):(idx2-1)],
                                                " +"))),
                     ncol = 12, byrow = T)

idx3 <- seq(from = 1, to = nrow(stage.data), by = dim.stage.data[2])
k0 <- 1
for (k0 in 1:dim.stage.data[1]){  # per pop
  tmp <- rbind(N0[k0,],
               stage.data[idx3[k0]:(idx3[k0]+dim.stage.data[2]-1),])
  write.table(tmp, file = paste0('data/Base Models/',
                               unlist(strsplit(results.file,
                                               split = '.mp'))[1],
                               '_pop', k0, '.csv'),
            sep = ",", quote = F, row.names = F, col.names = F)

}






