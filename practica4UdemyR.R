library(quantmod)
getSymbols(c('BMV:LIVEPOLC-1','BMV:ELEKTRA'), src='google') 

#renombrar datos de bolsa a nombres mas cortos
liverpool<-`BMV:LIVEPOLC-1`
colnames(liverpool) <- gsub("BMV:LIVEPOLC-1", "liverpool", colnames(liverpool)) 

plot(liverpool$liverpool.Close)


period <- 100
price_vector <- liverpool$liverpool.Close
moving_average_vector <- c(rep(NA, period))
for (ind in seq((period+1),(length(price_vector))) ){
  moving_average_vector <- c(moving_average_vector, mean(price_vector[(ind-period):ind]))
}

#par(mfrow=c(2,1))
plot(liverpool$liverpool.Close)
plot(moving_average_vector, type='l', col='red', lwd=3, main = paste('SMA', period))

length(moving_average_vector)


# # pass it back to our time series object
 liverpool$liverpool.Close.SMA <- moving_average_vector
# 
 plot(liverpool$liverpool.Close)
 lines(liverpool$liverpool.Close.SMA, type='l', col='red', lwd=3)

 #manera tradicional
 chartSeries(liverpool,theme = 'white')
 addSMA(n=100)
 