symbolsToday<-c("VRSK")
symbol<-symbolsToday[1]
quote<--6.6
range<-(drange(bump = 0)[1]*100):(drange(bump = 0)[2]*100)
BOOK<-matrix(data = NA,nrow = length(range),ncol = 2)
for (k in range){
  quote<-k/100
  STOCK<-stock(STOCK=get(symbol),RUN = FALSE)
  STOCKtd<-STOCK[between(STOCK$pCHANGE,drange()[1],drange()[2]),]
  STOCKtd<-STOCKtd[rev(order(STOCKtd$row)),]
  STOCKtd<-STOCKtd[!is.na(STOCKtd$pCHANGE),]
  STOCKtd$lag[is.na(STOCKtd$lag)]<-sign(quote)
  STOCKtd$running<-cummean(STOCKtd$lag)
  BOOK[k-min(range),1]<-quote
  BOOK[k-min(range),2]<-mean(STOCKtd$running)
  # print(quote)
  # print(mean(STOCKtd$running))
}
View(BOOK)
