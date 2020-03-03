#packages <- c('MTS', 'fGarch', 'forecast', 'tseries', 'fUnitRoots', 'urca')
#install.packages(packages)

library(xts)
library(tidyverse)

#lecture 1 examples

da <- read.table(here::here("00_data/q-gdpunemp.txt"), header = T) # load data
head(da)

gdp <- ts(da$gdp, start = c(1948,1) , frequency = 4)
unrate <- ts(da$rate , start = c(1948,1), frequency = 4)

plot(diff(log(gdp)), ylab = 'GDP Growth' )
plot(unrate, ylab = 'unemployment rate')

gas.data <- read.csv('data/w-gasreg.csv')
oil.data <- read.csv('data/w-wti.csv')

date_gas_oil <- as.Date(gas.data$DATE, '%m/%d/%Y')

gas <- xts(x = gas.data$GASREGCOVW, order.by = date_gas_oil)
oil <- xts(x = oil.data$WCOILWTICO, order.by = date_gas_oil)

data <- cbind( gas, oil)
data <- cbind.data.frame(date_gas_oil, gas, oil)
dim(data)
head(data)

ggplot(data = data, aes(x = date_gas_oil, y =log(gas)))+
  geom_line(color = "#00AFBB", size = 1.2)

ggplot(data = data, aes(x = date_gas_oil, y =log(oil)))+
  geom_line(color = "#E7B800", size = 1.2)

df <- data

df <- df %>%  mutate(
  gas = log(data$gas),
  oil = log(data$oil))


dff <- df%>% 
  select(date_gas_oil, gas, oil) %>%
  gather(key = "variable", value = "value", -date_gas_oil)


ggplot(dff, aes(x = date_gas_oil, y = value))+
  geom_line(aes(color = variable), size = 1)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()