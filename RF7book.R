symbolsToday<-c("AAPL")
symbol<-symbolsToday[1]
quote<-0
for (j in 1:length(symbol)){
  STOCK<-stock(RUN = FALSE)
  STOCKtd<-STOCK[between(STOCK$pCHANGE,drange()[1],drange()[2]),]
  STOCKtd<-STOCKtd[rev(order(STOCKtd$row)),]
  STOCKtd<-STOCKtd[!is.na(STOCKtd$pCHANGE),]
  STOCKtd$lag[is.na(STOCKtd$lag)]<-sign(quote)
  STOCKtd$running<-cummean(STOCKtd$lag)
  print(mean(STOCKtd$running))
}
