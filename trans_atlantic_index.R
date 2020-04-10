library(lubridate)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)

setwd('~/value_at_risk_portfolio/large_cap-export')

#############
### Construct blended index and indecies through S&P and Euro Stox 
############

# STOXX50E$STOXX50E.Adjusted <- read.csv('./^STOXX50E.csv', header=TRUE, row.names="Date") #Euro Stoxx 50
# GSPC$GSPC.Adjusted <- read.csv('./^GSPC.csv', header=TRUE, row.names="Date") #S&P 500 

endDate   = Sys.time()
tickers <- c("^STOXX50E", "^GSPC")
getSymbols(tickers, from='1960-01-04', to = endDate)

blended_prices <- na.omit(merge(Ad(STOXX50E), Ad(GSPC)))
### Rate of Change or momentum
blended_returns <- ROC(blended_prices, type = 'discrete')

#Euro Stoxx 50
STOXX50E$STOXX50E.Adjusted <- as.numeric(STOXX50E$STOXX50E.Adjusted)  
#S&P 500 
GSPC$GSPC.Adjusted <- as.numeric(GSPC$GSPC.Adjusted)  

summary(STOXX50E$STOXX50E.Adjusted)   #Factors besides 'Open' have null values
summary(GSPC$GSPC.Adjusted)

length(STOXX50E$STOXX50E.Adjusted)          # lengths are not equal by two days
length(GSPC$GSPC.Adjusted)

#series_transatlantic = (STOXX50E$STOXX50E.Adjusted + GSPC$GSPC.Adjusted) / 2 
series_transatlantic_1 = .5*(STOXX50E$STOXX50E.Adjusted) + .5*(GSPC$GSPC.Adjusted)

ret_transatlantic <- ROC(series_transatlantic_1, type = 'discrete')

### Euro Stoxx 50 density, histogram, line
g <- ggplot(STOXX50E, aes(x = STOXX50E.Adjusted, y = STOXX50E[,0]))
g <- ggplot(STOXX50E, aes(x = STOXX50E[,0], y = STOXX50E.Adjusted))
g + geom_density()
g + geom_histogram()
g + geom_line()

### S&P 500 density, histogram, line
g <- ggplot(GSPC, aes(x = GSPC.Adjusted))
g + geom_density()

df_transatlantic = data.frame(series_transatlantic_1, ret_transatlantic) 
colnames(df_transatlantic) <- c('Adjusted_Price_Blended', 'Returns_Blended')

plot(series_transatlantic_1, auto.grid = TRUE)
g <- ggplot(series_transatlantic_1, aes(x = Date, y = Adjusted_Blended))
g + geom_density()
g + geom_line()

summary(STOXX50E)
summary(GSPC)

setwd('~/value_at_risk_portfolio/large_cap-export/value_at_risk_portfolio_views')
write.csv(df_transatlantic, file = "transatlantic.csv")




