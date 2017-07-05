library(quantmod)
library(TTR)
library(binhf)

#acciones mexicanas de liverpool y elektra

liverpool<-getSymbols('BMV:LIVEPOLC-1', src='google',auto.assign = FALSE) 
elektra <-getSymbols('BMV:ELEKTRA',src = 'google', auto.assign = FALSE)

#renombrar datos de bolsa a nombres mas cortos

colnames(liverpool) <- gsub("BMV:LIVEPOLC-1", "liverpool", colnames(liverpool)) 
colnames(elektra)<-gsub("BMV:ELEKTRA","elektra",colnames(elektra))


# remove any NAs 
liverpool <- liverpool[!(rowSums(is.na(liverpool))),]
elektra <- elektra[!(rowSums(is.na(elektra))),]


chartSeries(liverpool, theme="white", TA="addSMA(50, col='black');addSMA(200, col='blue');addADX(n = 14, maType='EMA', wilder=TRUE)", subset='2013::')
chartSeries(elektra, theme="white", TA="addSMA(50, col='black');addSMA(200, col='blue');addADX(n = 14, maType='EMA', wilder=TRUE)", subset='2013::')

# VWAMP osea volumen contra precio 

VWAP.Slow <- VWAP(price=elektra$elektra.Close, volume=elektra$elektra.Volume, n=100)
VWAP.Fast <- VWAP(price=elektra$elektra.Close, volume=elektra$elektra.Volume, n=20)
VWAP.Diff <- VWAP.Fast- VWAP.Slow

chartSeries(elektra, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue')")

chartSeries(elektra, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue');
            addADX(n = 14, maType='EMA', wilder=TRUE)")

ADX.20 <- ADX(elektra,n=14)

# look for long entries
Long_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff> 0, elektra$elektra.Close, NA)

# look for long entries
Short_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff < 0, elektra$elektra.Close, NA)

plot(elektra)

points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)



# Liverpool

VWAP.Slow <- VWAP(price=liverpool$liverpool.Close, volume=liverpool$liverpool.Volume, n=100)
VWAP.Fast <- VWAP(price=liverpool$liverpool.Close, volume=liverpool$liverpool.Volume, n=20)
VWAP.Diff <- VWAP.Fast- VWAP.Slow

chartSeries(liverpool, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue');
            addADX(n = 14, maType='EMA', wilder=TRUE)")

ADX.20 <- ADX(liverpool,n=14)

# look for long entries
Long_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff> 0, liverpool$liverpool.Close, NA)

# look for long entries
Short_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff < 0, liverpool$liverpool.Close, NA)

plot(liverpool)

## Warning in plot.xts(liverpool): only the univariate series will be plotted

points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)


