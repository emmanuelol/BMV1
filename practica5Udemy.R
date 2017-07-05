library(quantmod)
library(TTR)
library(binhf)

liverpool<-getSymbols('BMV:LIVEPOLC-1', src='google',auto.assign = FALSE) 
elektra <-getSymbols('BMV:ELEKTRA',src = 'google', auto.assign = FALSE)

#renombrar datos de bolsa a nombres mas cortos

colnames(liverpool) <- gsub("BMV:LIVEPOLC-1", "liverpool", colnames(liverpool)) 
colnames(elektra)<-gsub("BMV:ELEKTRA","elektra",colnames(elektra))

chartSeries(liverpool$liverpool.Close, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

chartSeries(elektra, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

elektra.EMA.50<- EMA(elektra$elektra.Close, n=50, ) 
elektra.EMA.200<- EMA(elektra$elektra.Close, n=200, )  
addTA(elektra.EMA.50 - elektra.EMA.200,col='blue', type='h',legend="50-200 MA")

#liverpool

chartSeries(liverpool, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=50, ) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=200, ) 
addTA(liverpool.EMA.50 - liverpool.EMA.200, col='blue', type='h',legend="50-200 MA")

chartSeries(liverpool, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

liverpool.EMA.10 <- EMA(liverpool$liverpool.Close, n=10, ) 
liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=50, ) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=200, ) 
Fast.Diff <- liverpool.EMA.10 - liverpool.EMA.50
Slow.Diff <- liverpool.EMA.50 - liverpool.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA")
addTA(Slow.Diff, col='red', type='h',legend="50-200 MA")

#liverpool
tail(as.numeric(Fast.Diff))
tail(shift(v=as.numeric(Fast.Diff), places=1, dir="right")) 




# look for long entries
Long_Trades <- ifelse(
  Slow.Diff  > 0 &
    Fast.Diff  > 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") < 0, liverpool$liverpool.Close, NA)

# look for long exits (same thing but inverse signts)
Short_Trades <- ifelse(
  Slow.Diff  < 0 &
    Fast.Diff  < 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") > 0, liverpool$liverpool.Close, NA)

plot(liverpool)
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)


#elektra

chartSeries(elektra, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

elektra.EMA.10 <- EMA(elektra$elektra.Close, n=10, ) 
elektra.EMA.50 <- EMA(elektra$elektra.Close, n=50, ) 
elektra.EMA.200 <- EMA(elektra$elektra.Close, n=200, ) 
Fast.Diff <- elektra.EMA.10 - elektra.EMA.50
Slow.Diff <- elektra.EMA.50 - elektra.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA")
addTA(Slow.Diff, col='red', type='h',legend="50-200 MA")

tail(as.numeric(Fast.Diff))
tail(shift(v=as.numeric(Fast.Diff), places=1, dir="right")) 




# look for long entries
Long_Trades <- ifelse(
  Slow.Diff  > 0 &
    Fast.Diff  > 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") < 0, elektra$elektra.Close, NA)

# look for long exits (same thing but inverse signts)
Short_Trades <- ifelse(
  Slow.Diff  < 0 &
    Fast.Diff  < 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") > 0, elektra$elektra.Close, NA)

plot(elektra)
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)
