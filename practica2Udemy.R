library(quantmod)
getSymbols('BMV:LIVEPOLC-1',src='google')
dim(`BMV:LIVEPOLC-1`)

plot(`BMV:LIVEPOLC-1`)
names(`BMV:LIVEPOLC-1`)
# lineChart(`BMV:LIVEPOLC-1`,line.type = 'h', theme = 'white', TA = NULL)
#chart with volume
lineChart(`BMV:LIVEPOLC-1`,line.type = 'h', theme = 'white')

barChart(`BMV:LIVEPOLC-1`,line.type = 'h', TA = NULL)

candleChart(`BMV:LIVEPOLC-1`, TA = NULL,subset = '2017-01::2017-05')