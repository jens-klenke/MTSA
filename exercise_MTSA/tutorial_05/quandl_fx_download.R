# Quandl Script -> Download FX data 
# MTSA 2019/20
# Week 5

library(Quandl)
# 1. Download and prepare data
FX.Ja   <- Quandl("FRED/DEXJPUS", start_date = "1998-12-30", end_date = "2018-12-31", type = "xts")   # Download daily data on Japan/US FX rates
FX.Eu   <- Quandl("FRED/DEXUSEU", start_date = "1998-12-30", end_date = "2018-12-31", type = "xts")  # Download daily data on Euro/US FX rates
# Compute Growth rates
lr.Eu <- diff(log(FX.Eu[, 1]))[-1]   # leave out NA in first component via [-1]
lr.Ja <- diff(log(FX.Ja[, 1]))[-1]    # leave out NA in first component via [-1]

V         <- merge.xts(lr.Eu, lr.Ja, all=FALSE)  # only use data when both log-returns are available
date      <- index(V)                 # save dates for later use
fx_series <- coredata(V)          # raw log-returns
N         <- length(date)

# a) Apply Ljung-Box test on fx_series


# b) What order do the information criteria suggest?


# c) Fit a VAR(1), inspect Sigma_a


# d) Apply the Ljung-Box test on the residuals (degrees of freedom!) Comment.


# e) Repeat using the squared residuals. Also consult information criteria. Does it look sensible to fit a VAR? 


# f) Can we rule out weak stationarity just by our findings regarding the correlation structures?


