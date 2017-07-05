library(quantmod)
library(TTR)
library(binhf)

#acciones mexicanas de liverpool y elektra

liverpool<-getSymbols('BMV:LIVEPOLC-1', src='google',auto.assign = FALSE) 
elektra <-getSymbols('BMV:ELEKTRA',src = 'google', auto.assign = FALSE)
famsa<-getSymbols('BMV:GFAMSAA',src='google', auto.assign = FALSE)
walmart<-getSymbols('BMV:WALMEX',src='google',auto.assign = FALSE)
sanborns<-getSymbols('BMV:GSANBORB-1',src='google',auto.assign = FALSE)
carso<-getSymbols('BMV:GCARSOA1',src='google',auto.assign = FALSE)


#renombrar datos de bolsa a nombres mas cortos

colnames(liverpool) <- gsub("BMV:LIVEPOLC-1", "liverpool", colnames(liverpool)) 
colnames(elektra)<-gsub("BMV:ELEKTRA","elektra",colnames(elektra))
colnames(famsa)<-gsub("BMV:GFAMSAA","famsa",colnames(famsa))
colnames(walmart)<-gsub("BMV:WALMEX","walmart",colnames(walmart))
colnames(sanborns)<-gsub("BMV:GSANBORB-1","sanborns",colnames(sanborns))
colnames(carso)<-gsub("BMV:GCARSOA1","carso",colnames(carso))


# remove any NAs 
liverpool <- liverpool[!(rowSums(is.na(liverpool))),]
elektra <- elektra[!(rowSums(is.na(elektra))),]
famsa<-famsa[!(rowSums(is.na(famsa))),]
walmart <- walmart[!(rowSums(is.na(walmart))),]
sanborns <- sanborns[!(rowSums(is.na(sanborns))),]
carso <- carso[!(rowSums(is.na(carso))),]

# merge
basket <- data.frame(as.xts(merge(liverpool,elektra,famsa,walmart,sanborns,carso)))

## eligiendo solo las columnas de cierre
basket <- basket[,names(basket)[grepl(x=names(basket), pattern='Close')]]



results <- c()
for (basket_name in names(basket)) {
  result <- round(as.numeric(cor(basket)[,basket_name]),2)
  results <- rbind(results, c(basket_name,result))
}
results <- data.frame(results)
names(results)[-1] <- names(basket)
results



# time for a correlation function
Get_Column_Correlations <- function(objDF){
  results <- c()
  for (col_name in names(objDF)) {
    result <- round(as.numeric(cor(objDF)[,col_name]),2)
    results <- rbind(results, c(col_name,result))
  }
  results <- data.frame(results)
  names(results)[-1] <- names(objDF)
  return (results)
}

# time for a correlation function
Get_Column_Correlations <- function(objDF){
  results <- c()
  for (col_name in names(objDF)) {
    result <- round(as.numeric(cor(objDF)[,col_name]),2)
    results <- rbind(results, c(col_name,result))
  }
  results <- data.frame(results)
  names(results)[-1] <- names(objDF)
  return (results)
}


Get_Column_Correlations(basket[as.Date(rownames(basket)) >= '2017-01-01',])[,c('X1','liverpool.Close')]
