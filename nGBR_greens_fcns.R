
# Functions to compute survival and transition probabilities using variable stage durations. p. 162 on Caswell's book. 
# 
# # # Create a function to define Lefkovitch matrix using variable stage duration model. 
# # Ts is the stage durations of various juvenile stages in years (T in Caswell's book) - the last juvenile stage is to be maturing adults, so their fecundity is not zero.
# 
# VT is a vector of variances of stage durations
# 
# f is a vector of fecundities, which include the survival of adults  
# and proportion that breeds: only two are allowed, where only maturing adults (MA) and 
# adults (A) breed. For the maturing adults, the survival rate is incorporated (multiplied) 
# during the calculation. 
# 
# phi.1 is the survival rate of the first year (eggs, hatchlings, and first year)
# 
# phi.J includes pelagic juvenile, neritic juvenile, subadult, and maturing adult stages. 
# 

define.matrix.varstage <- function(Ts, VT, f, phi.1, phi.J, phi.A, lambda){
  
  d.lambda <- 0.5
  
  while(d.lambda > 0.001){
    a <- log(lambda/phi.J)
    
    # gam is a vector of length = length(VT) - the number of juvenile age classes, 
    # including maturing adults
    gam <- (1/Ts) * exp(-a * ((Ts/2) - (VT/(2*Ts))))
    
    M.dim <- length(phi.J) + length(phi.A) + 1
    # (juvenile + subadult stages + maturing adults) + adult stages (length of survival) + first year
    M <- matrix(data = 0, 
                nrow = M.dim, 
                ncol = M.dim)
    
    # the maturing adult stage needs to reproduce (Caswell 2001, p. 165)
    # fertility needs adult survival for the post-breeding census. 
    M[1, M.dim] <- f[length(f)] * phi.A   # adults
    
    # fertility for maturing adults needs the maturing adult stage transition and
    # survival and maturing adults' fecundity and non-transition. M.dim - 2 is the 
    # last element of gam and phi.J
    M[1, (M.dim - 1)] <- f[length(f)] * gam[M.dim - 2] * phi.J[M.dim - 2]  +
      f[length(f) - 1] * phi.J[M.dim - 2] * (1 - gam[M.dim - 2])
    
    # subadults need maturing adults fecundity and adult stage transition
    M[1, (M.dim - 2)] <- f[length(f) - 1] * gam[M.dim - 3] * phi.J[M.dim - 3]
    
    # first-year survival
    M[2, 1] <- phi.1
    
    # adult survival at the end
    M[M.dim, M.dim] <- phi.A
    
    # fill in two rows for each column staying (1 - gam) and leaving (gam)
    l <- 1  
    for (j in 2:(M.dim-1)){
      M[j, j] <- phi.J[j-1] * (1 - gam[j-1])
      M[(j+1), j] <- phi.J[j-1] * gam[j-1]
      
    }
    
    # compute eigenvalues
    eig1 <- eigen(M)
    
    d.lambda <- abs(eig1$values[1] - lambda)
    lambda <- eig1$values[1]
  }
  
  
  # return stuff.
  return(list(a = a,
              gam = gam,
              phi.J = phi.J,
              M = M,
              eigen1 = eig1$values[1]))
}


obj.fcn.varstage <- function(x){
  M <- define.matrix.varstage(Ts = in.list$Ts, 
                              VT = in.list$VT, 
                              f = in.list$f, 
                              phi.1 = in.list$phi.1, 
                              phi.J = x, 
                              phi.A = in.list$phi.A,
                              lambda = 1.0)
  
  eig.1 <- M$eigen1
  
  return(abs(eig.1 - in.list$lambda))
}




# Create a function to define Lefkovitch matrix using the negative binomial stage duration model. 
# Ts is the stage durations of various juvenile stages in years (T in Caswell's book) - the last juvenile stage is to be maturing adults, so their fecundity is not zero.
# 
# VT is a vector of variances of stage durations
# 
# f is a vector of fertilities (this excludes the survival of adult): only two are allowed, where only maturing adults (MA) and adults (A) breed. 
# 
# phi.1 is the survival rate of the first year (eggs, hatchlings, and first year)
# 
# phi.J includes pelagic juvenile, neritic juvenile, subadult, and maturing adult stages. 
# 
# p.breed is a vector of proportions of females laying eggs - same length as f

define.matrix.negbin <- function(Ts, VT, f, phi.1, phi.J, phi.A, p.breed){
  gam <- Ts/(VT + Ts)   # probability of moving to the next stage.
  k <- round((Ts^2) / (VT + Ts))
  M.dim <- sum(k) + 2
  
  # juvenile + subadult stages + maturing adults (ks) + adult stages (length of fecundity) + first year
  M <- matrix(data = 0, 
              nrow = M.dim, 
              ncol = M.dim)
  
  # all pseudo stages for the maturing adult stage need to reproduce (Caswell 2001, p. 165)
  # fertility needs adult survival for the post-breeding census
  M[1, M.dim] <- f[2] * phi.A * p.breed[2]  # adults
  
  # fertility for maturing adults needs the final juvenile stage suvival
  M[1, (M.dim - k[length(Ts)]):(M.dim - 1)] <- f[1] * phi.J[length(phi.J)] * p.breed[1] 
  
  # first-year survival
  M[2, 1] <- phi.1
  
  # adult survival at the end
  M[M.dim, M.dim] <- phi.A
  
  # fill in two rows for each column staying (1 - gam) and leaving (gam)
  l <- 1  
  for (j in 1:length(k)){
    for (i in 1:k[j]){
      M[(l + 1), (l + 1)] <- phi.J[j] * (1 - gam[j])
      M[(l + 2), (l + 1)] <- phi.J[j] * gam[j]
      l <- l + 1
    }
    
  }
  
  # compute eigenvalues
  eig1 <- eigen(M)
  
  # return stuff.
  return(list(k = k,
              gam = gam,
              phi.J = phi.J,
              M = M,
              eigen1 = eig1$values[1]))
}

# Using Rsolnp package - this seems to work better. in.list needs to be available in the workspace.

obj.fcn <- function(x){
  M <- define.matrix.negbin(Ts = in.list$Ts, 
                            VT = in.list$VT, 
                            f = in.list$f, 
                            phi.1 = in.list$phi.1, 
                            phi.J = x, 
                            phi.A = in.list$phi.A,
                            p.breed = in.list$p.breed)
  
  eig.1 <- M$eigen1
  
  return(abs(eig.1 - in.list$lambda))
}

# Differences of survival rates between two subsequent stages
# are always negative = increasing survival rates
eval_g0 <- function(x){
  
  return(c(x[1] - x[2], x[2] - x[3], x[3] - x[4]))
}

# the inequality constraints didn't work so well in nloptr package.
# estim <- nloptr(x0 = c(0.5, 0.7, 0.9),
#                 eval_f = obj.fcn,
#                 eval_g_ineq = eval_g0,
#                 lb = c(0,0,0), 
#                 ub = c(1.0,1.0,1.0),
#                 opts = list("algorithm" = "NLOPT_LN_COBYLA",
#                             "xtol_abs" = c(1.0e-18, 1.0e-18, 1.0e-18),
#                             "maxeval" = 50000))

#"NLOPT_LN_NELDERMEAD" - worked for without contraint
#"NLOPT_GN_AGS" - does not work
#"NLOPT_GN_ORIG_DIRECT" - didn't work so well. 
#                            ,
#                            "maxeval" = 10000

# Tried to use matrix algebra but didn't work so well in nloptr package... 
# eval_g0 <- function(x){
#   return(matrix(c(1,-1,0,0,1,-1), nrow = 2, ncol = 3) %*% c(x[1], x[2], x[3]))
# }


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



