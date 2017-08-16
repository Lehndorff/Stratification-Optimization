drange<-function(bound=.2,bump=.075){
  if (abs(quote)<.1){quote=.1*sign(quote)}
  return(sort(c(quote*(1-bound)+(-bump*sign(quote)),quote*(1+bound)+(bump*sign(quote)))))
}
stock<-function(STOCK=get(symbols[j]),Last=h,RUN=TRUE){
  colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    if(length(STOCK$open)<Last){Last<-length(STOCK$open)}
    STOCK<-STOCK[(length(STOCK$open)-Last):length(STOCK$open),]
    STOCK$row<-1:length(STOCK$open)
    STOCK$pCHANGE<-((STOCK$close)-lag(STOCK$close))/lag(STOCK$close)*100
    STOCK$UP<-0
    STOCK$UP[sign(STOCK$pCHANGE)==1]<-1
    if (RUN == TRUE){
      streak<-data.frame(unclass(rle(as.vector(STOCK$UP))))
      streak$values[streak$values==0]<--1
      streak$tvalue<-streak$lengths*streak$values
      y<-as.vector(NULL)
      for(i in 1:length(streak$lengths)){
        y<-c(y,(streak$values[i]:streak$tvalue[i]))
      }
      STOCK<-cbind(STOCK,y)
    }
    STOCK<-as.data.frame(STOCK)
    STOCK$lag<-lead(STOCK$UP,1)
    STOCK$rflag<-lead(STOCK$pCHANGE)
    colnames(STOCK)[colnames(STOCK)=="..2"]<-"streak"
    return(STOCK)
}

runtd<-function(){
  STOCKstreak$streak==(qSTOCK$sign+(STOCK$streak[length(STOCK$streak)]*as.numeric(sign(STOCK$streak[length(STOCK$streak)])==qSTOCK$sign)))
}

STREAK<-function(){
  y<-as.vector(NULL)
  for(i in 1:length(streak$lengths)){
      streak$values[i]:streak$tvalue[i]
  }
  return(y)
}
symbols2<-symbolsSP
for (j in 1:length(symbols2)){
  check<-nrow(get(symbols2[j]))
  if (check<100){
  symbols2<-symbols2[!(symbols2 %in% symbols2[j])]
  }
}

Historyrun$conc<-paste(Historyrun$symb,Historyrun$date)
Historyday$conc<-paste(Historyday$symb,Historyday$date)
test<-left_join(Historyday,Historyrun,by="conc")
table(test$sign.y)
