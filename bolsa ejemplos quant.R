getSymbols('SBUX', src='google')
getSymbols('SPY',src='google')

head(SBUX)

tail(SBUX)
class(SBUX)


head(SPY)

tail(SPY)
class(SPY)


SPY <- data.frame(SPY) 
summary(SPY)


SBUX <- data.frame(SBUX) 
summary(SBUX)

basket_symbols <- c('YELP', 'AMZN', 'AAPL')
getSymbols(basket_symbols, src='google')

basket <- data.frame(as.xts(merge(YELP, AMZN, AAPL)))
head(basket,2)


getSymbols('BMV:LIVEPOLC-1',src='google')

head(`BMV:LIVEPOLC-1`)

tail(`BMV:LIVEPOLC-1`)
chartSeries(`BMV:LIVEPOLC-1`, theme='white')


