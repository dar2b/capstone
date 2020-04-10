library(quantmod)
library(PerformanceAnalytics)

setwd('~/value_at_risk_portfolio/large_cap-export/value_at_risk_portfolio_views')

# CAPM/Farma-French with Betas for each equity
endDate   = Sys.time()
tickers <- c("BTZ","HL", "IGOI", "NBGIF", "SQBG", "TWTR", "UTSI")
getSymbols(tickers, from='1960-01-04', to = endDate)

### Pricing series and returns' volitility 
port_prices <- na.omit(merge(Ad(BTZ), Ad(HL), Ad(IGOI), Ad(NBGIF), Ad(SQBG), Ad(TWTR), Ad(UTSI)))
port_returns <- ROC(port_prices, type = 'discrete')
btz_ret <- ROC(na.omit(Ad(BTZ)))
plot(Ad(BTZ), auto.grid = TRUE)
plot(btz_ret, auto.grid = TRUE)
hl_ret <- ROC(na.omit(Ad(HL)))
plot(Ad(HL), auto.grid = TRUE)
plot(hl_ret, auto.grid = TRUE)
igoi_ret <- ROC(na.omit(Ad(IGOI)))
plot(Ad(IGOI), auto.grid = TRUE)
plot(igoi_ret, auto.grid = TRUE)
nbgif_ret <- ROC(na.omit(Ad(NBGIF)))
plot(Ad(NBGIF), auto.grid = TRUE)
plot(nbgif_ret, auto.grid = TRUE)
sqbg_ret <- ROC(na.omit(Ad(SQBG)))
plot(Ad(SQBG), auto.grid = TRUE)
plot(sqbg_ret, auto.grid = TRUE)
twtr_ret <- ROC(na.omit(Ad(TWTR)))
plot(Ad(TWTR), auto.grid = TRUE)
plot(twtr_ret, auto.grid = TRUE)
ustsu_ret <- ROC(na.omit(Ad(USTSU)))
plot(Ad(UTSI), auto.grid = TRUE)
plot(ustsu_ret, auto.grid = TRUE)
plot(UTSI, auto.grid = TRUE)

###### "Market" - Transatlantic Indice ####
# Betas
# reading in file since there are no object between classes
transatlantic <- read.csv('transatlantic.csv', header = TRUE)  
data.frame(transatlantic, index)
rownames(transatlantic) <- transatlantic$X
transatlantic$X <- NULL 
transatlantic <- as.xts(transatlantic)

plot(port_prices$HL.Adjusted)

# ???? merge/join on date for xts objects 
data.frame(rbind(hl_date$HL.Adjusted, transatlantic$Returns_Blended))
join(data.frame(btz_ret$BTZ.Adjusted), data.frame(transatlantic$Returns_Blended), type = "inner")
merge((btz_ret$BTZ.Adjusted),(transatlantic$Returns_Blended))
merge((hl_date$HL.Adjusted),(transatlantic$Returns_Blended))  # not enought random access memory for RAM 
# merge(data.frame(hl_date$HL.Adjusted), data.frame(transatlantic$Returns_Blended))
# merge(data.frame(hl_date$HL.Adjusted), data.frame(transatlantic$Returns_Blended))
# merge(data.frame(hl_date$HL.Adjusted), data.frame(transatlantic$Returns_Blended))

library(plyr)
lst <- list(data.frame(hl_date$HL.Adjusted), data.frame(transatlantic$Returns_Blended))
df_lst <- do.call(rbind.fill, lst)
View(df_lst)
## use list to combine 
#https://ryouready.wordpress.com/2009/01/23/r-combining-vectors-or-data-frames-of-unequal-length-into-one-data-frame/

model = lm( df_lst$HL.Adjusted ~ df_lst$transatlantic.Returns_Blended, data = df_lst)
model = lm()
