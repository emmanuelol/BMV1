library(quantmod)
library(TTR)

Liverpool & Elektra
========================================================
```{r echo=TRUE, results='hide', message=FALSE} 
library(quantmod)
getSymbols(c('BMV:LIVEPOLC-1','BMV:ELEKTRA'), src='google') 

```
My setup on Liverpool

```{r}
liverpool<-`BMV:LIVEPOLC-1`
chartSeries(liverpool, theme="white", TA="addVo();addBBands();addCCI()", subset='2017')
```
My setup on Elektra
```{r}
elektra<-BMV:ELEKTRA
chartSeries(liverpool, theme="white", TA="addVo();addBBands();addCCI()", subset='2017')
```




