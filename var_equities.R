library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(cvar)

########################################
# Trading portfolio(ETrade) equity value at risk 
########################################
# amount = Quantity * price 
# sum(amount) 
# calculate VaR .95 

#equity prices 
# setwd('~/value_at_risk_portfolio/large_cap-export')     # yahoo finance sets, set working path 
# files_equities = list.files(pattern = "._[1-9].csv")    # take in numerous view to get each total VaR
# files_equities =lapply(files_equities, function(x) read.csv(x)) 

#combining equity portfilio list (class(files_equities)) 
# files_equities = do.call(rbind, files_equities)

setwd('~/value_at_risk_portfolio/large_cap-export/value_at_risk_portfolio_views')
# setwd('/Users/darienbouzakis/RStudio')

# Quantity, price, market value, includes options
portfolio_lots_1 <- read.csv('PortfolioDownload.csv', header = F)  
View(portfolio_lots_1)
#Equity only view
portfolio_lots_2 <- read.csv('PortfolioDownload-2.csv', header = F)  
View(portfolio_lots_2)
portfolio_lots_3 <- read.csv('PortfolioDownload-3.csv', header = F)  
portfolio_lots = lapply(portfolio_lots, function(x) read.csv(x)) 
View(portfolio_lots_3)
# portfolio_lots <- read.csv('Portfolio.csv', header = F)   
date <- Sys.time()

portfolio_pnl <- portfolio_lots[c(1,6)]
# Dollar total 
daily_pnl <- data.frame(portfolio_pnl[75,2])

names(daily_pnl) = c('PnL')
daily_pnl_df <- data.frame(daily_pnl)
daily_pnl_df <- data.frame('Date' = date, 'PnL' = daily_pnl_df)
df_ts <- as.xts(daily_pnl_df, order.by = as.Date(daily_pnl_df$Date))

## open a connection to write to csv then write daily Date and VaR
## write to an Excel file when application is available on local 
pnl_to_var <- write_csv(daily_pnl_df, 'PnL_to_VaR')
# write.table(daily_pnl_df, 'PnL_to_VaR.csv', sep = ',', col.names = !file.exists('PnL_to_VaR'), append = T)

## Test var method 
# new <- data.frame('Date' = c('2019/1/3', '2019/1/2', '2019/1/1' ), 'Daily PnL' = c(1, -.02, .1))
# new[,2]
# combination of date and values that work

### Calculation
df_ts[1,2]
df_ts = as.numeric(df_ts[,2])
PerformanceAnalytics::VaR(df_ts, p=.95, method = 'historical')


########################################
# Equity Example
########################################
##HL
# options("getSymbols.warning4.0"=FALSE)
# env_hl <-new.env()
startDate = as.Date('1960-01-04')
endDate   = Sys.time()
HL_prices <- Ad(getSymbols("HL" , from=startDate, to=endDate, auto.assign=F))
HL_returns <- dailyReturn(HL_prices)
VaR(HL_returns, p = 0.95, method='historical')
VaR(HL_returns, p = 0.99, method="historical")

# No dividend for a stock index, but useful for the individual stocks
getDividends('HL',env=env_hl, src='yahoo', from=startDate,
             to=endDate,auto.assign=T)
# No split for a stock index, but useful for the individual stocks
getSplits('HL',env=hl, src='yahoo', from=startDate,to=endDate,auto.assign=T)

tail(hl$HL)


########################################
# Equities 
########################################

endDate   = Sys.time()
tickers <- c("HL", "IGOI", "NBGIF", "SQBG", "TWTR", "UTSI")
weights <- c(.2, .2, .2, .2, .1, .1)              # Find weights or calculate quantities through portfolio View

getSymbols(tickers, from='1960-01-04', to = endDate)
Port.prices <- na.omit(merge(Ad(HL), Ad(IGOI), Ad(NBGIF), Ad(SQBG), Ad(TWTR), Ad(UTSI)))
Port.returns <- ROC(Port.prices, type= "discrete")[-1,]
colnames(Port.returns) <- tickers
sapply(Port.prices, sd)

hl_ret <- ROC(HL$HL.Adjusted, type = "discrete")[-1,]
#### Loss range at 99% ####
## VaR() is not accepting Port.returns object 
Equi_hist <- VaR(Port.returns, p=0.99, weights = NULL, portfolio_method = "single", method = "historical")
plot(Port.returns, type = "l", pch = 16, cex = 0.8,  col = gray(0.2, 0.5), 
     ylab = "Returns", main = "99% VaR Forecasting", xaxt = "n")

df_port_returns <- data.frame(Port.returns)
# add ticker value to each stock return - seperate
# rbind back together into a ticker named dataframe 

write.csv(df_port_returns, 'port_returns.csv')

hist(Port.prices$HL.Adjusted, col ='red')
hist(Port.prices$IGOI.Adjusted, col = 'blue')
hist(Port.prices$NBGIF.Adjusted, col = 'yellow')
hist(Port.prices$SQBG.Adjusted, col = 'blue')
hist(Port.prices$TWTR.Adjusted, col = 'blue')

g <-  Port.returns %>% ggplot(aes (x=  , fill=value)) +
      geom_histogram(color='#e9ecef', alpha=0.6, position = 'identity') 
g     
autoplot(Port.returns, geom = 'line')
chart.VaRSensitivity(Port.returns$IGOI)

Equi_gaus <- VaR(Port.returns, p=0.99, weights = NULL, portfolio_method = "single", method = "gaussian")
Equi_mod <- VaR(Port.returns, p=0.99, weights = NULL, portfolio_method = "single", method = "modified")
Equi_all_VaR <- data.frame(rbind(Equi_hist, Equi_gaus, Equi_mod))
rownames(Equi_all_VaR) = c('Hist', 'Gaus', 'Mod')
Equi_all_VaR$Type <- c('Hist', 'Gaus', 'Mod')
Equi_all_VaR

PortVaR_hist <- VaR(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "historical")
PortVaR_gaus <- VaR(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "gaussian")
PortVaR_mod <- VaR(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "modified")
All_VaR <- data.frame(rbind(PortVaR_hist$hVaR, PortVaR_gaus$VaR, PortVaR_mod$MVaR))
rownames(All_VaR) <- c('Hist', 'Gaus', 'Mod')
# colname(All_VaR$hVaR.99.) = 'Portfolio'
All_VaR

colnames(Equi_all_VaR$Porfolio) = 'Portfolio' 
Equi_VaR_Port <- Equi_all_VaR
Equi_VaR_Port
# Equi_all_VaR <- select(Equi_all_VaR, -Port)

#### CVaR average expected loss @99% ####
cond_var <- CVaR(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "historical")
CVaR(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "gaussian")

CVaR(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "gaussian")
ETL(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "gaussian")
ES(Port.returns, p=0.99, weights = weights, portfolio_method = "component", method = "gaussian")

library(reshape2)
class(Equi_all_VaR$Porfolio)
melt(Equi_all_VaR)

charts.PerformanceSummary(Port.returns, main = "Cumulative Returns", geometric = T)
# https://www.youtube.com/watch?v=0N7XBk7YA5A



########################################
# Equity files 
########################################
 setwd('~/value_at_risk_portfolio/large_cap-export')
for (i in tickers) {
  write.csv(i, file = paste(i, '_series'))
}
View(tickers)

write.csv(as.data.frame(HL),'HL_series.csv')
write.csv(as.data.frame(IGOI),'IGOI_series.csv')
write.csv(as.data.frame(NBGIF),'NBGIF_series.csv')
write.csv(as.data.frame(SQBG),'SQBG_series.csv')
write.csv(as.data.frame(TWTR),'TWTR_series.csv')
write.csv(as.data.frame(UTSI),'UTSI_series.csv')

new_df <- data.frame(Company = character(), 
           Date = as.Date(character()),
           Open = numeric(),
           High = numeric(),
           Low = numeric(), 
           Close = numeric(),
           Volume = numeric(), 
           Adjusted = numeric())
