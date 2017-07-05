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

# merge
basket <- data.frame(as.xts(merge(liverpool,elektra)))

## eligiendo solo las columnas de cierre
basket <- basket[,names(basket)[grepl(x=names(basket), pattern='Close')]]


## arbitraje simple por correlacion

basket_years <- unique(substr(rownames(basket), start=1, stop=4))
basket_months <- unique(substr(rownames(basket), start=1, stop=7))
small_basket <- basket[,names(basket)[grepl(x=names(basket), pattern='liverpool|elektra')]]
liverpool_elektra <- c()
for (yearmonth in basket_years) {
  temp_df <- small_basket[substr(rownames(basket), start=1, stop=4)==yearmonth,]
  liverpool_elektra <- cbind(liverpool_elektra, cor(temp_df$liverpool.Close, temp_df$elektra.Close))
}

small_basket_correlations <- data.frame(rbind(liverpool_elektra))
colnames(small_basket_correlations) <- basket_years

par(mfrow=c(2,1))
plot(as.Date(row.names(basket)), basket$liverpool.Close, col='red', 
     type='l', ylab="price", xlab='')
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$elektra.Close, col='gray', type='l', xaxt="n",yaxt="n",ylab="", xlab='time')
legend("topright",col=c("red","gray"),lty=1,legend=c("liverpool","elektra"))

plot(type='l', col='darkgreen', x=as.factor(names(small_basket_correlations)),  y=as.numeric(small_basket_correlations[1,]))
lines(type='l', col='darkgreen', x=as.factor(names(small_basket_correlations)), 
      y=as.numeric(small_basket_correlations[1,]))



###

EMA.Fast <- EMA(liverpool$liverpool.Close, n=30) 
EMA.Medium <- EMA(liverpool$liverpool.Close, n=100) 
EMA.Slow <- EMA(liverpool$liverpool.Close, n=200) 
EMA_Diff_Fast <- EMA.Fast - EMA.Medium
EMA_Diff_Slow <- EMA.Medium - EMA.Slow

chartSeries(liverpool, theme="white", TA="addEMA(n=100, col='red');addEMA(n=200, col='blue')")



elektra$elektra.movement <- EMA(ifelse(ClCl(elektra)  > 0, 1, -1),50)
liverpool$liverpool.movement <- EMA(ifelse(ClCl(liverpool) > 0, 1, -1),50)

plot(as.numeric(liverpool$liverpool.movement ), col='black', ylab="movement",  main='liverpool-elektra', type = 'l')
abline(h=0, col='red')
par(new=TRUE)
plot(as.numeric(elektra$elektra.movement ), col='gray', xaxt="n",yaxt="n",ylab="", xlab='time', type='l')
abline(h=0, col='green')



par(mfrow=c(2,1))
diff <- as.numeric(liverpool$liverpool.movement)-as.numeric(elektra$elektra.movement)


plot(diff, 
     col='black', type='l', xlab='time', ylab='difference',
     main='liverpool-elektra')
abline(h=0, col='green')
plot(EMA_Diff_Slow)
abline(h=0, col='green')

# look for long entries
Long_Trades <- ifelse(
  EMA_Diff_Slow  > 0 &
    EMA_Diff_Fast  > 0 &
    elektra$elektra.movement > 0 &
    liverpool$liverpool.movement > 0 &
    (liverpool$liverpool.movement - elektra$elektra.movement) < 0 & 
    (liverpool$liverpool.movement - elektra$elektra.movement) > -0.01, liverpool$liverpool.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  EMA_Diff_Slow < 0 &
    EMA_Diff_Fast  < 0 &
    elektra$elektra.movement < 0 &
    liverpool$liverpool.movement < 0 &
    (liverpool$liverpool.movement - elektra$elektra.movement) > 0 & 
    (liverpool$liverpool.movement - elektra$elektra.movement) < 0.01, liverpool$liverpool.Close, NA)

plot(liverpool, main='liverpool')

## Warning in plot.xts(liverpool, main = "liverpool"): only the univariate series will be
## plotted

points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)


