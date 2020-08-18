# Exercise Sheet 3, Task 3
# Computations for solutions
library(MASS)

# a)
# (reverse) characteristic polynomial: 1 - z + 0.21 z^2 - 0.025 z^3 = 0
roots <- polyroot(c(1, -1, 0.21, 0.025))
roots # there are some imaginary parts attached to it
length(Re(roots)^2 + Im(roots)^2 > 1) # count how many roots lie outside the unit circle

# Alternative: VAR(1) approach and eigenvalues.
phi_1 <- matrix(data = c(0.5, 0.4, 0.1, 0.5), nrow = 2)
phi_2 <- matrix(data = c(0, 0.25, 0, 0), nrow = 2)
I2x2 <- diag(2)
O2x2 <- matrix(data = rep(0, 4), nrow = 2)
Phi <- rbind( cbind(phi_1, phi_2), cbind(I2x2, O2x2) )
Phi
var1.eigen <- eigen(Phi)
length(Re(var1.eigen$values)^2 + Im(var1.eigen$values)^2 < 1) # How many eigenvalues lie inside the unit circle?


# b) 
phi_0 <- c(2,1)
mu <- solve((I2x2 - phi_1 - phi_2)) %*% phi_0
fractions(mu)

# e)
# Alternative solution to b) using the VAR(1) representation:
mu2 <- solve(diag(4) - Phi) %*% c(phi_0, rep(0,2))
fractions(mu2)

# f)
Sigma_a <- diag(2)
Sigma_b <- rbind( cbind(Sigma_a, O2x2), cbind(O2x2, O2x2) )
Gamma0ast.vec <- solve(diag(16) - Phi %x% Phi) %*% as.vector(Sigma_b)
Gamma0ast.mat <- matrix(data = Gamma0ast.vec, nrow = 4)
Gamma0ast.mat




A <- matrix(c(0.75, 0, -0.25, 0.5 ), ncol = 2, byrow = TRUE)
B <- matrix(c(2.285714, -0.6857143, -0.6857143, 1.752381 ), ncol = 2, byrow = TRUE)



A%*%B