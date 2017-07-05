library(quantmod)
library(TTR)
getSymbols('BMV:ALFAA',src='google')
ALFA<-`BMV:ALFAA`
chartSeries(ALFA,theme = 'white')
addSMA(n=200)
addROC(n=200)
chartSeries(ALFA,theme = 'white',TA="addVo();addBBands();addCCI()",subset = '2017')
chartSeries(ALFA, theme=chartTheme('white'), up.col="black", dn.col="black")
ALFA.EMA.20<- EMA(ALFA$'BMV:ALFAA.Close', n=20) 
ALFA.EMA.100<- EMA(ALFA$'BMV:ALFAA.Close', n=100) 
addTA(ALFA.EMA.20, on=1, col = "red")

#Liverpool

getSymbols('BMV:LIVEPOLC-1',src='google')
liverpool<-`BMV:LIVEPOLC-1`
chartSeries(liverpool,theme = 'white')
addSMA(n=200)
addROC(n=200)
chartSeries(liverpool,theme = 'white',TA="addVo();addBBands();addCCI()",subset = '2017')
chartSeries(liverpool, theme=chartTheme('white'), up.col="black", dn.col="black")
liverpool.EMA.20<- EMA(liverpool$'BMV:LIVEPOLC-1.Close', n=20) 
liverpool.EMA.100<- EMA(liverpool$'BMV:LIVEPOLC-1.Close', n=100) 
#addTA(liverpool.EMA.20, on=1, col = "red")
addTA(liverpool.EMA.20-liverpool.EMA.100,col='blue', type='h',legend="20-100 MA")