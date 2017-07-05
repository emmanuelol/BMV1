ProfitLoss_Calculator <- function(objDF) {
  # make column names generic so they can handle any symbol
  colnames(objDF) <- c("Open", "High", "Low", "Close", "Volume", "Long_Trades", "Short_Trades")
  
  current_long <- 0
  current_short <- 0
  
  for (ind in seq(1,nrow(objDF))) {
    if (objDF$Long_Trades[ind] !=0) {
      # first trade should be an entry, last trade an exit
      if ((current_long==0 & objDF$Long_Trades[ind] > 0) | (current_long !=0)) {
        # next trade should be opposite sign of previous trade (entry -> exit)
        if (sign(objDF$Long_Trades[ind]) != sign(current_long)) {
          current_long <- as.numeric(objDF$Long_Trades[ind])
          print(paste('Long', current_long))
        }
      }
      if (current_long != as.numeric(objDF$Long_Trades[ind]))
        objDF$Long_Trades[ind] <- 0
    }
    if (objDF$Short_Trades[ind] !=0) {
      # first trade should be an entry
      if ((current_short==0 & objDF$Short_Trades[ind] > 0) | (current_short !=0)) {
        # next trade should be opposite sign of previous trade (entry -> exit)
        if (sign(objDF$Short_Trades[ind]) != sign(current_short)) {
          current_short <- as.numeric(objDF$Short_Trades[ind])
          print(paste('Short', current_short))
        }
      }
      if (current_short != as.numeric(objDF$Short_Trades[ind]))
        objDF$Short_Trades[ind] <- 0
    }
  }
  
  # trim to be even, if not add last close held in chart
  if ((!length(objDF$Long_Trades[objDF$Long_Trades != 0])%% 2) == 0)
    objDF$Long_Trades[length(objDF$Close)] <- -1 * objDF$Close[length(objDF$Close)]
  if ((!length(objDF$Short_Trades[objDF$Short_Trades != 0])%% 2) == 0)
    objDF$Short_Trades[length(objDF$Close)] <- -1 * objDF$Close[length(objDF$Close)]
  
  print(paste('Final Longs:',round(sum(objDF$Long_Trades * -1),2)))
  print(paste('Final Shorts:',round(sum(objDF$Short_Trades),2)))
  
  
  # plot trade entries and exits
  par(mfrow=c(2,1))
  plot(objDF, main='Long Trades')
  points(ifelse(objDF$Long_Trades > 0, objDF$Long_Trades, NA), col='green', cex=1.5, pch=16)
  points(ifelse(objDF$Long_Trades < 0, objDF$Long_Trades * -1, NA), col='red', cex=1.5, pch=15)
  
  plot(objDF, main='Short Trades')
  points(ifelse(objDF$Short_Trades > 0, objDF$Short_Trades, NA), col='green', cex=1.5, pch=16)
  points(ifelse(objDF$Short_Trades < 0, objDF$Short_Trades * -1, NA), col='red', cex=1.5, pch=15)
}