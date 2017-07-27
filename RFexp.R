
DIA2<-DIA
DIA2$pCHANGE<-((DIA2$DIA.Close)-lag(DIA2$DIA.Close))/lag(DIA2$DIA.Close)*100
SPY2<-SPY
SPY2$pCHANGE<-((SPY2$SPY.Close)-lag(SPY2$SPY.Close))/lag(SPY2$SPY.Close)*100
CORR1R<-NULL
CORR2R<-NULL
for (j in 1:length(symbols)){
  STOCK<-na.approx(get(symbols[j]))
  colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
  STOCK$pCHANGE<-((STOCK$close)-lag(STOCK$close))/lag(STOCK$close)*100
  CORR<-as.matrix(cbind(lag(STOCK$pCHANGE),STOCK$pCHANGE,DIA2$pCHANGE,SPY2$pCHANGE))
  CORR2<-as.matrix(cbind(lag(sign(STOCK$pCHANGE)),sign(STOCK$pCHANGE),sign(DIA2$pCHANGE),sign(SPY2$pCHANGE)))
  print(symbols[j])
  CORR1R<-rbind(CORR1R,c(symbols[j],cor(CORR,use = "complete.obs")[1,]))
  CORR2R<-rbind(CORR2R,c(symbols[j],cor(CORR2,use = "complete.obs")[1,]))
}

cor(CORR,use = "complete.obs")[1,]
rbind(cor(CORR,use = "complete.obs")[1,],cor(CORR,use = "complete.obs")[1,])

Last<-h
STOCK<-get(symbols[j])
colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
if(length(STOCK$open)<Last){
  Last<-length(STOCK$open)
}
STOCK<-STOCK[(length(STOCK$open)-Last):length(STOCK$open),]
STOCK$row<-1:length(STOCK$open)
STOCK$pCHANGE<-((STOCK$close)-lag(STOCK$close))/lag(STOCK$close)*100
STOCK$UP<-0
STOCK$UP[sign(STOCK$pCHANGE)==1]<-1
streak<-data.frame(unclass(rle(as.vector(STOCK$UP))))
streak$values[streak$values==0]<--1
streak$tvalue<-streak$lengths*streak$values
y<-as.vector(NULL)
for(i in 1:length(streak$lengths)){
  y<-c(y,(sign(streak$tvalue[i]):streak$tvalue[i]))
}
STOCK<-cbind(STOCK,y)
