# Solution to Exercise Sheet 1, Task 5
# 23-10-2019

# 1. Load the package
library(MTS)

# import the dataset to variables
data("mts-examples")

# it's a data frame :)
class(tenstocks)

# a) test all three series individually
mq(x = tenstocks$JPM, lag = 20) # no adjustments needed since we have not estimated any model here
mq(x = tenstocks$MS, lag = 20)
mq(x = tenstocks$GS, lag = 20)
# => No p-value below 5%.  There seems to be no relevant autocorrelation pattern in any of the series.

# b) perform the multivariate test
mq(x = cbind(tenstocks$JPM, tenstocks$MS, tenstocks$GS), lag = 20)
# => all p-values are below 5%. Since the time series are not much autocorrelated (univariate!),
# there must be cross-correlations which cause hypothesis to be rejected
# -> there is a dynamic pattern which might be explained using multivariate time series models!
ccf(x = tenstocks$JPM, y = tenstocks$MS, lag.max = 20)
ccf(y = tenstocks$JPM, x = tenstocks$MS, lag.max = 20) # remember task 4!
ccf(x = tenstocks$JPM, y = tenstocks$GS, lag.max = 20)
ccf(x = tenstocks$MS, y = tenstocks$GS, lag.max = 20)
# keep it mind that the Ljung-Box test does not take \rho_0 into consideration!
plot.ts(cbind(tenstocks$JPM, tenstocks$MS, tenstocks$GS)) 
# this command treats the data as time series and can even cope with multivariate time series

