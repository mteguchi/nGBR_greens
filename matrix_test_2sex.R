# Moderate matrix from NF

M_f <- matrix(data = c(0,0,0,1.22,42.18,100.65,
                       0.4,0.63428,0,0,0,0,
                       0,0.09297,0.7918,0,0,0,
                       0,0,0.02373,0.81877,0,0,
                       0,0,0,0.03989,0.75785,0,
                       0,0,0,0,0.11436,0.95),
              byrow = T, nrow = 6)

eigen(M_f)


M_m <- matrix(data = c(0,0,0,0,0,0,
                       0.4,0.63428,0,0,0,0,
                       0,0.09297,0.7918,0,0,0,
                       0,0,0.02373,0.81877,0,0,
                       0,0,0,0.03989,0.75785,0,
                       0,0,0,0,0.11436,0.95),
              byrow = T, nrow = 6)

M1_fm <- cbind(M_f, matrix(data = 0, nrow = 6, ncol = 6))
M2_fm <- cbind(matrix(data = c(0,0,0,0,12.60339, 13.7275,
                               0,0,0,0,0,0,
                               0,0,0,0,0,0,
                               0,0,0,0,0,0,
                               0,0,0,0,0,0,
                               0,0,0,0,0,0),
                      byrow = T, nrow = 6), M_m)

M_fm <- rbind(M1_fm, M2_fm)

Ft  <- matrix(data = NA, nrow = 6, ncol = 1000)
Ft[, 1] <- rep(100, 6)

Nt <- matrix(data = NA, nrow = 12, ncol = 1000)
Nt[,1] <- rep(100, 12)
for (t in 2:ncol(Nt)){
  Nt[, t] <- M_fm %*% Nt[,t-1]
  Ft[, t] <- M_f %*% Ft[, t-1]
}

N_sum <- colSums(Nt)
plot(N_sum)

obsd_r <- log(N_sum[ncol(Nt)]/N_sum[1])/ncol(Nt)

# this is not the same as, but close to, the dominant eigenvalue. 

F_sum <- colSums(Ft)
obsd_r_f <- log(F_sum[ncol(Ft)]/F_sum[1])/ncol(Ft)
