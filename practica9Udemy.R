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


## Elektra

SMA.Fast <- SMA(elektra$elektra.Close, n=20)
SMA.Medium <- SMA(elektra$elektra.Close, n=100) 
SMA.Slow <- SMA(elektra$elektra.Close, n=200) 
fast_detrend_ma <- SMA.Fast - SMA.Medium
slow_detrend_ma <- SMA.Medium - SMA.Slow

# look for long entries
elektra$Long_Trades <- ifelse(
  slow_detrend_ma  > 0 &
    fast_detrend_ma  > 0 &
    shift(v=as.numeric(fast_detrend_ma), places=1, dir="right") < 0, elektra$elektra.Close, NA)
# exits for longs
elektra$Long_Trades <- ifelse(fast_detrend_ma  < 0, -1 * elektra$elektra.Close, elektra$Long_Trades)
elektra$Long_Trades[is.na(elektra$Long_Trades)] <- 0

# look for short entries
elektra$Short_Trades <- ifelse(
  slow_detrend_ma  < 0 &
    fast_detrend_ma < 0 &
    shift(v=as.numeric(fast_detrend_ma), places=1, dir="right") > 0, elektra$elektra.Close, NA)
# exits for longs
elektra$Short_Trades <- ifelse(fast_detrend_ma  > 0, -1 * elektra$elektra.Close, elektra$Short_Trades)
elektra$Short_Trades[is.na(elektra$Short_Trades)] <- 0

ProfitLoss_Calculator(elektra)


## Liverpool

SMA.Fast <- SMA(liverpool$liverpool.Close, n=20)
SMA.Medium <- SMA(liverpool$liverpool.Close, n=100) 
SMA.Slow <- SMA(liverpool$liverpool.Close, n=200) 
fast_detrend_ma <- SMA.Fast - SMA.Medium
slow_detrend_ma <- SMA.Medium - SMA.Slow

# look for long entries
liverpool$Long_Trades <- ifelse(
  slow_detrend_ma  > 0 &
    fast_detrend_ma  > 0 &
    shift(v=as.numeric(fast_detrend_ma), places=1, dir="right") < 0, liverpool$liverpool.Close, NA)
# exits for longs
liverpool$Long_Trades <- ifelse(fast_detrend_ma  < 0, -1 * liverpool$liverpool.Close, liverpool$Long_Trades)
liverpool$Long_Trades[is.na(liverpool$Long_Trades)] <- 0

# look for short entries
liverpool$Short_Trades <- ifelse(
  slow_detrend_ma  < 0 &
    fast_detrend_ma < 0 &
    shift(v=as.numeric(fast_detrend_ma), places=1, dir="right") > 0, liverpool$liverpool.Close, NA)
# exits for longs
liverpool$Short_Trades <- ifelse(fast_detrend_ma  > 0, -1 * liverpool$liverpool.Close, liverpool$Short_Trades)
liverpool$Short_Trades[is.na(liverpool$Short_Trades)] <- 0

ProfitLoss_Calculator(liverpool)

### agregando parametros como cci rsi  cv

chartSeries(liverpool, theme="white", TA="addChVol(n=50);addROC(n=10)")

 ,# create a slow ema difference
liverpool.EMA.50 <- EMA(liverpool$liverpool.Close, n=50) 
liverpool.EMA.200 <- EMA(liverpool$liverpool.Close, n=200) 
Slow.Diff <- liverpool.EMA.50 - liverpool.EMA.200
ROC.IND <- ROC(x=liverpool$liverpool.Close, n=10)
CV.IND <- chaikinVolatility(HL=liverpool, n=20)

# look for long entries
liverpool$Long_Trades <- ifelse(
  ROC.IND > 0.1 &
    CV.IND > 0 & CV.IND < 0.1 &
    Slow.Diff > 0, liverpool$liverpool.Close, NA)

# exits
liverpool$Long_Trades <- ifelse(Slow.Diff < 0, -1 * liverpool$liverpool.Close, liverpool$Long_Trades)
liverpool$Long_Trades[is.na(liverpool$Long_Trades)] <- 0

# look for short entries
liverpool$Short_Trades <- ifelse(
  ROC.IND < -0.1 &
    CV.IND > 0 & CV.IND < 0.1 &
    Slow.Diff < 0, liverpool$liverpool.Close, NA)

# exits for shorts
liverpool$Short_Trades <- ifelse(Slow.Diff  > 0, -1 * liverpool$liverpool.Close, liverpool$Short_Trades)
liverpool$Short_Trades[is.na(liverpool$Short_Trades)] <- 0

ProfitLoss_Calculator(liverpool)