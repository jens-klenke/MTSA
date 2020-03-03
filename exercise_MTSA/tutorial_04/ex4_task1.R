# Exercise Sheet 4
# 13-11-2019

library(MTS)

# Task 1
#-------------------
# Preparation: Define matrices
phi_1 <- matrix(data = c(0.75, -0.25, 0, 0.5), nrow = 2)
phi_0 <- c(1, 0)
Sigma_a <- matrix(data = c(1, 0, 0, 1), nrow = 2)
mean <- solve(diag(2) - phi_1) %*% phi_0 # litte reminder
# b)
phi_1.adj <- matrix(data = c(0.5, 0.25, 0, 0.75), nrow = 2) # coefficients of the adjoint matrix. don't forget the lag operator!
# c)
(diag(2) - phi_1.adj) %*% (diag(2) - phi_1) # pre-multiplying the adjoint matrix; no difference between the procedures
# e)
T <- 10^3
burn_in <- 250 # extra periods, the first 250 are then cut away from the simulated to reduce the impact of starting point selection
set.seed(42)
var1_data <- VARMAsim(nobs = T, arlags = c(1), malags = NULL, cnst = phi_0, phi = phi_1, skip = burn_in, sigma = Sigma_a)
plot.ts(var1_data$series)
# f)
var1_fit <- VAR(x = var1_data$series, p = c(1), include.mean = TRUE) # least squares estimation
mse_var <- colMeans(var1_fit$residuals^2) # MSEs of two sequences of residuals (a_1, a_2)
# g)
z1_fit <- arima(x = var1_data$series[,1], order = c(2,0,1), include.mean = TRUE)
z2_fit <- arima(x = var1_data$series[,2], order = c(2,0,1), include.mean = TRUE, optim.control = list(maxit = 10^3))
mse_arma <- c( mean(z1_fit$residuals^2), mean(z2_fit$residuals^2) ) # (a_1, a_2)
# h)
mse_arma / mse_var # element-wise ratio of MSEs

# Further look: comparing AR(1) coefficients
t(cbind(var1_fit$coef[2:3,1], var1_fit$coef[2:3,2])) # autoregressive coefficients from the VAR (main diagonal of phi_1)
t(cbind(z1_fit$coef[1:2], z2_fit$coef[1:2])) # autoregressive coefficients from the implied models for z_1 and z_2
# matrices were transposed to fit the formula z_t = phi_0 + phi_1 * z_t-1 + a_t !
#-------------------

# Task 2
#-------------------
# a)
# estimating the VAR regression by regression
z1_reg <- lm(var1_data$series[-1,1] ~ var1_data$series[-T,]) # row 1
z2_reg <- lm(var1_data$series[-1,2] ~ var1_data$series[-T,]) # row 2
ols_coef_matrix <- cbind(z1_reg$coefficients, z2_reg$coefficients) # putting the coefficients together
# comparing the coefficients
ols_coef_matrix
var1_fit$coef # this is supposed to match the matrix from above
#-------------------

# For your interest:
# Just fitting AR(1) models instead of VAR(1)
z1_arfit <- arima(x = var1_data$series[,1], order = c(1,0,0), include.mean = TRUE)
z2_arfit <- arima(x = var1_data$series[,2], order = c(1,0,0), include.mean = TRUE)
cbind(z1_arfit$coef[1], z2_arfit$coef[1])
cbind(var1_fit$coef[2,1], var1_fit$coef[3,2])


