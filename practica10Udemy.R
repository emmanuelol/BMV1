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

### graficar dos o mas compañias

plot(as.Date(row.names(basket)), basket$elektra.Close, col="blue", type='l', ylab="", xlab="")
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$liverpool.Close, col='green', type='l', 
     xaxt="n", yaxt='n', xlab="",ylab="")
axis(4)
legend("topleft",col=c("blue","green"),lty=1,legend=c("Elektra","Liverpool"))


## medir sincronizacion 
movement_elektra <- ifelse(ClCl(elektra)[-1] > 0, 1, -1)
movement_liverpool <- ifelse(ClCl(liverpool)[-1] > 0, 1, -1)

sum(movement_elektra == movement_liverpool) / length(movement_liverpool)
