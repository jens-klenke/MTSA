# Exercise Sheet 2, Task 1
# Computing the (lagged) covariance matrices based on Example 2.6,
# simulating a trajectory and estimating sample moments

# First define parameters/coefficients:
Phi <- matrix(data = c(0.2, -0.6, 0.3, 1.1), byrow = FALSE, nrow = 2) # VAR coefficients
Sigma_a <- matrix(data = c(1, 0.8, 0.8, 2.0), byrow = FALSE, nrow = 2) # innovations' covariances
k <- 2 # dimension of z

### a) + b): Covariance and Correlation Matrices
#------------------------------
# compute Kronecker product of phi
Phi_kron <- kronecker(X = Phi, Y = Phi) # kronecker product
Phi %x% Phi # alternative command
dim(Phi_kron) # turns out it's k² in every dimension :)

# Going for the covariance matrix Gamma_0
Ident <- diag(k^2) # identity matrix with the same dimensions as Phi_kron
Gamma0.vec <- solve(Ident - Phi_kron) %*% as.vector(Sigma_a) # as.vector works like the "vec" operator
Gamma0.mat <- matrix(data = Gamma0.vec, nrow = 2, byrow = FALSE)
Gamma0.mat

# Proceeding to Gamma_1
Gamma1.mat <- Phi %*% Gamma0.mat
Gamma1.mat

# compute further (lagged) cross-covariance-functions
library(expm) # to compute phi^(p) neatly
ccovf.list <- list() # preparing the variables to store the matrices into: covariance
ccorf.list <- list() # """ : correlation
D.inv <- solve(sqrt(Gamma0.mat * diag(k))) # obtaining standard deviations for the single variables (parts of z) and putting them in a diagonal matrix
for (i in 1:10){
  ccovf.list[[i]] <- Phi%^%i %*% Gamma0.mat
  ccorf.list[[i]] <- D.inv %*% ccovf.list[[i]] %*% D.inv
}
ccovf.list # cross lagged covariances
ccorf.list # cross lagged correlations

Rho0.mat <- D.inv %*% Gamma0.mat %*% D.inv
#------------------------------

### c) + d): Simulating a Trajectory
#------------------------------
# i) Set sample size and further coefficients
T <- 300
# ii) The Basis: Innovations
library(MASS)
set.seed(2^9-1) # for replication: the 'random' numbers drawn up from this point will always be the same (given you use the same command to draw them....)
a <- mvrnorm(n = T, mu = c(0,0), Sigma = Sigma_a)
# iii) Writing a function to generate 'z' realisations from 'a' using Phi
var1gen <- function(coef.mat, z.lag, innovation){
  z.current <- coef.mat %*% z.lag + innovation # z_(t) = phi * z_(t-1) + a_(t)
  return(z.current)
}
# iv) Prepare variable for 'z'
z <- ts(data = matrix(data = NA, nrow = (T), ncol = k)
        ,start = 1, names = c("z_1", "z_2"))
# v) Set starting values for z (at mean + innovation)
z[1,] <- c(0,0) + a[1,]
head(z) # making sure it did work
# vi) Generate z repeatedly and store it
for (i in 2:(T)){
  z[i,] <- var1gen(coef.mat = Phi, z.lag = z[(i-1),], innovation = a[i,])
}


# Note that it is generally advised to discard the first few observations to eliminate the influence of the arbitrary starting point!
# In our case we skipped this step to keep things short and clear.

# The ordinary 'plot' command works because 'z' is already a 'ts'-class object (among other classes) and NOT a data frame!
plot(z)
#------------------------------

### Part 3: Estimate Sample Moments from Simulated Data
#------------------------------
# i) demeaning the multivariate time series first: (if you are puzzled, please have another look at the slides)
mu <- colMeans(z)
# Method 1: iteration
#z.demeaned <- t(apply(X = z, MARGIN = c(1), FUN = function(x) x - mu)) # iterating over the rows and subtracting the mean vector everytime
# Method 2: subsetting
z.demeaned <- cbind(z[,1]-mu[1], z[,2]-mu[2]) # subtracting the column means from each column individually and combining the columns afterwards

# ii) estimate Gamma_0 (showing the normalisation step -- people had questions about it regarding the Ljung-Box test)
Gamma0.hat <- t(z.demeaned) %*% z.demeaned / (T-1) # sample covariance with correction for degrees of freedom (we've demeaned the series!)
# please also note that now the first entry is transposed! (z.demeaned is a data matrix and not a random vector anymore!)
standardisation <- matrix(data = c(Gamma0.hat[1,1], # just another way to program it, maybe it is more visible what's inside D.inv %*% D.inv now
                                sqrt( Gamma0.hat[1,1] * Gamma0.hat[2,2]),
                                sqrt(Gamma0.hat[2,2] * Gamma0.hat[1,1]),
                                Gamma0.hat[2,2]),
                       nrow = 2, byrow = FALSE) 
Rho0.hat <- Gamma0.hat / standardisation
Rho0.hat # quite comparable to what is expected (population equivalent is in slides)

# comparing population moments with sample moments:
mu - c(0,0)
Gamma0.hat - Gamma0.mat
Rho0.hat - Rho0.mat
# What's the best way to reduce the gap? What influence does Sigma_a have?

# for the lazy ones:
cov(z.demeaned)
cor(z.demeaned)
#------------------------------


