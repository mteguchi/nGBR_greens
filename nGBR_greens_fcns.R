



prep_mpfile <- function(results.dir, results.file){
  file.base <- unlist(strsplit(results.file, split = '.mp'))
  results.all <- results(paste0(results.dir, results.file))
  
  # total and per region abundance - mean, SD, min, max
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
  
  # Per region and per stage abundance have been extracted using extract_results_X_mptools.R scripts.
  stageNames <- c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf',
                  'nem', 'pjm', 'bjm', 'sam', 'mam', 'adm')
  
  R1.N.stages <- read.csv(file = paste0(results.dir, file.base, "_pop1.csv"), 
                          header = FALSE)
  colnames(R1.N.stages) <- stageNames
  R1.N.stages %>% mutate(time = seq(from = 0, to = 210, by = 5),
                         region = "Region_1") -> R1.N.stages
  
  R2.N.stages <- read.csv(file = paste0(results.dir, file.base, "_pop2.csv"), 
                          header = FALSE)
  colnames(R2.N.stages) <- stageNames
  R2.N.stages %>% mutate(time = seq(from = 0, to = 210, by = 5),
                         region = "Region_2") -> R2.N.stages
  
  R3.N.stages <- read.csv(file = paste0(results.dir, file.base, "_pop3.csv"), 
                          header = FALSE)
  colnames(R3.N.stages) <- stageNames
  R3.N.stages %>% mutate(time = seq(from = 0, to = 210, by = 5),
                         region = "Region_3") -> R3.N.stages
  
  R4.N.stages <- read.csv(file = paste0(results.dir, file.base, "_pop4.csv"), 
                          header = FALSE)
  colnames(R4.N.stages) <- stageNames
  R4.N.stages %>% mutate(time = seq(from = 0, to = 210, by = 5),
                         region = "Region_4") -> R4.N.stages
  
  All.N.stages <- R1.N.stages[, 1:12] + 
    R2.N.stages[, 1:12] + 
    R3.N.stages[, 1:12] + 
    R4.N.stages[, 1:12] 
  
  All.N.stages %>% mutate(time = seq(from = 0, to = 210, by = 5),
                          region = "All") -> All.N.stages
  
  dat <- rbind(R1.N.stages, R2.N.stages,
               R3.N.stages, R4.N.stages, 
               All.N.stages)
  
  dat$region <- as.factor(dat$region)
  
  df1.f <- dat[, c('nef', 'pjf', 'bjf', 'saf', 'maf', 'adf', 
                   'time', "region")]
  df1.f$sex <- "Female"
  colnames(df1.f) <- c('ne', 'pj', 'bj', 'sa', 'ma', 'ad', 
                       'time', "region", 'sex')
  
  df1.m <- dat[, c('nem', 'pjm', 'bjm', 'sam', 'mam', 'adm',
                   'time', "region")]
  df1.m$sex <- "Male"
  colnames(df1.m) <- c('ne', 'pj', 'bj', 'sa', 'ma', 'ad',
                       'time', "region", 'sex')
  
  df1 <- rbind(df1.f, df1.m)
  df1$sex <- as.factor(df1$sex)
  df1$all <- rowSums(df1[, 1:6])
 
  return(list(df1 = df1,
              dat = dat) )
}



