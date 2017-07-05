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


#liverpool
chartSeries(liverpool, theme="white", TA="addRSI(n=100);addCCI(n=100);addROC(n=100)", subset='2017')
chartSeries(liverpool, theme="white", TA="addCCI(n=100);addEMA(n=50,col='blue');addEMA(n=200,col='red')")


# create a slow ema difference
##CCI

liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=50) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=200) 
Slow.Diff <- liverpool.EMA.50 - liverpool.EMA.200
CCI.IND <- CCI(HLC=liverpool[,c("liverpool.High","liverpool.Low","liverpool.Close")],n=100)

# look for long entries
Long_Trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places=1, dir="right") > CCI.IND &
    CCI.IND < 100 & 
    Slow.Diff > 0, liverpool$liverpool.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places=1, dir="right") < CCI.IND &
    CCI.IND > -100 & 
    Slow.Diff < 0, liverpool$liverpool.Close, NA)

plot(liverpool)

## Warning in plot.xts(liverpool): only the univariate series will be plotted

points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)

## chaikin volatility

chartSeries(liverpool, theme="white", TA="addChVol(n=100);")

chartSeries(liverpool, theme="white", TA="addCCI(n=100);addEMA(n=50,col='blue');addEMA(n=200,col='red');addChVol(n=100);")

# create a slow ema difference
liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=50) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=200) 
Slow.Diff <- liverpool.EMA.50 - liverpool.EMA.200
CCI.IND <- CCI(HLC=liverpool[,c("liverpool.High","liverpool.Low","liverpool.Close")],n=100)
CV.IND <- chaikinVolatility(HL=liverpool[,c("liverpool.High","liverpool.Low")], n=100)

# look for long entries
Long_Trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places=1, dir="right") > CCI.IND &
    CCI.IND < 100 & 
    CV.IND < 0 & 
    Slow.Diff > 0, liverpool$liverpool.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places=1, dir="right") < CCI.IND &
    CCI.IND > -100 & 
    CV.IND < 0 & 
    Slow.Diff < 0, liverpool$liverpool.Close, NA)

plot(liverpool)

## Warning in plot.xts(liverpool): only the univariate series will be plotted

points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)


### RSI

chartSeries(liverpool, theme="white", TA="addRSI(n=100);", subset='2017')
chartSeries(liverpool, theme="white", TA=NULL, subset='2017')

RSI.Fast <- RSI(price=liverpool$liverpool.Close,n=10)
RSI.Slow <- RSI(price=liverpool$liverpool.Close,n=30)
RSI.Diff <- RSI.Fast-RSI.Slow
addTA(RSI.Diff, col='blue', type='h',legend="RSI Diff")

# create a slow ema difference
liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=50) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=200) 
Slow.Diff <- liverpool.EMA.50 - liverpool.EMA.200

RSI.IND <- RSI(price=liverpool$liverpool.Close,n=30)

# look for long entries
Long_Trades <- ifelse(
  RSI.Diff  < 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") > 0  &
    Slow.Diff > 0, liverpool$liverpool.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  RSI.Diff  > 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") < 0  &
    Slow.Diff < 0, liverpool$liverpool.Close, NA)

plot(liverpool, main='RSI')

## Warning in plot.xts(liverpool, main = "RSI"): only the univariate series will be
## plotted

points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)

### combine RSI and CV

chartSeries(liverpool, theme="white", TA="addRSI(n=100);addChVol(n=100);", subset='2017')
chartSeries(liverpool, theme="white", TA=NULL, subset='2017')
RSI.Fast <- RSI(price=liverpool$liverpool.Close,n=10)
RSI.Slow <- RSI(price=liverpool$liverpool.Close,n=30)
RSI.Diff <- RSI.Fast-RSI.Slow
addTA(RSI.Diff, col='blue', type='h',legend="RSI Diff")

# create a slow ema difference
liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=50) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=200) 
Slow.Diff <- liverpool.EMA.50 - liverpool.EMA.200
CV.IND <- chaikinVolatility(HL=liverpool, n=100)
RSI.IND <- RSI(price=liverpool$liverpool.Close,n=30)

# look for long entries
Long_Trades <- ifelse(
  RSI.Diff  < 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") > 0  &
    CV.IND < -0.1 &
    Slow.Diff > 0, liverpool$liverpool.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  RSI.Diff  > 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") < 0  &
    CV.IND < -0.1 &
    Slow.Diff < 0, liverpool$liverpool.Close, NA)

plot(liverpool, main='RSI')
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)



#Elektra 
chartSeries(elektra, theme="white", TA="addRSI(n=100);addCCI(n=100);addROC(n=100)")

chartSeries(elektra, theme="white", TA="addRSI(n=100);addChVol(n=100);")


# create a slow ema difference
elektra.EMA.50 <- EMA(elektra$elektra.Close, n=50) 
elektra.EMA.200 <- EMA(elektra$elektra.Close, n=200) 
Slow.Diff <- elektra.EMA.50 - elektra.EMA.200

RSI.Fast <- RSI(price=elektra$elektra.Close,n=10)
RSI.Slow <- RSI(price=elektra$elektra.Close,n=30)
RSI.Diff <- RSI.Fast-RSI.Slow

CV.IND <- chaikinVolatility(HL=elektra, n=100)

# look for long entries
Long_Trades <- ifelse(
  CV.IND < -0.1 &
    RSI.Diff  < 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") > 0  &
    shift(v=as.numeric(RSI.Diff ), places=2, dir="right") < 0  &
    Slow.Diff > 0, elektra$elektra.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  CV.IND < -0.1 &
    RSI.Diff  > 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") < 0  &
    shift(v=as.numeric(RSI.Diff ), places=2, dir="right") > 0  &
    Slow.Diff < 0, elektra$elektra.Close, NA)

plot(elektra, main='RSI')

## Warning in plot.xts(elektra, main = "RSI"): only the univariate series will be
## plotted

points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch