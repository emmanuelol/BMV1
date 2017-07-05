library(quantmod)
library(TTR)
library(binhf)

#acciones mexicanas de liverpool y elektra

liverpool<-getSymbols('BMV:LIVEPOLC-1', src='google',auto.assign = FALSE) 
elektra <-getSymbols('BMV:ELEKTRA',src = 'google', auto.assign = FALSE)

#renombrar datos de bolsa a nombres mas cortos

colnames(liverpool) <- gsub("BMV:LIVEPOLC-1", "liverpool", colnames(liverpool)) 
colnames(elektra)<-gsub("BMV:ELEKTRA","elektra",colnames(elektra))


#haciendo mas lentos los EMA y agregando reglas para elegir mejor los largos y cortos

##  if no position: red-1 < red and blue-1 < 0 and blue > 0 go long
##if long: blue < 0 exit long
##
##if no position: red-1 > red and blue-1 > 0 and blue < 0 go short
##if short: blue > 0 exit short

### elektra
chartSeries(elektra, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

elektra.EMA.10 <- EMA(elektra$elektra.Close, n=20, ) 
elektra.EMA.50 <- EMA(elektra$elektra.Close, n=100, ) 
elektra.EMA.200 <- EMA(elektra$elektra.Close, n=300, ) 
Fast.Diff <- elektra.EMA.10 - elektra.EMA.50
Slow.Diff <- elektra.EMA.50 - elektra.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA")
addTA(Slow.Diff, col='red', type='h',legend="50-200 MA")


EMA.Fast <- EMA(elektra$elektra.Close, n=20, ) 
EMA.Medium <- EMA(elektra$elektra.Close, n=100, ) 
EMA.Slow <- EMA(elektra$elektra.Close, n=300, ) 
Fast.Diff <- EMA.Fast - EMA.Medium
Slow.Diff <- EMA.Medium - EMA.Slow

# look for trades
Long_Trades <- ifelse(
  shift(v=as.numeric(Slow.Diff), places=1, dir="right") < Slow.Diff &
    Fast.Diff  > 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") < 0, elektra$elektra.Close, NA)

Short_Trades <- ifelse(
  shift(v=as.numeric(Slow.Diff), places=1, dir="right") > Slow.Diff &
    Fast.Diff  < 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") > 0, elektra$elektra.Close, NA)

plot(elektra)

points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)



### liverpool


chartSeries(liverpool, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

liverpool.EMA.10 <- EMA(liverpool$liverpool.Close, n=20, ) 
liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=100, ) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=300, ) 
Fast.Diff <- liverpool.EMA.10 - liverpool.EMA.50
Slow.Diff <- liverpool.EMA.50 - liverpool.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA")
addTA(Slow.Diff, col='red', type='h',legend="50-200 MA")


EMA.Fast <- EMA(liverpool$liverpool.Close, n=20, ) 
EMA.Medium <- EMA(liverpool$liverpool.Close, n=100, ) 
EMA.Slow <- EMA(liverpool$liverpool.Close, n=300, ) 
Fast.Diff <- EMA.Fast - EMA.Medium
Slow.Diff <- EMA.Medium - EMA.Slow

# look for trades
Long_Trades <- ifelse(
  shift(v=as.numeric(Slow.Diff), places=1, dir="right") < Slow.Diff &
    Fast.Diff  > 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") < 0, liverpool$liverpool.Close, NA)

Short_Trades <- ifelse(
  shift(v=as.numeric(Slow.Diff), places=1, dir="right") > Slow.Diff &
    Fast.Diff  < 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") > 0, liverpool$liverpool.Close, NA)

plot(liverpool)

points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)
