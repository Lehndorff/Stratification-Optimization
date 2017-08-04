drange<-function(bound=.2,bump=.075){
  if (abs(quote)<.1){quote=.1*sign(quote)}
  return(sort(c(quote*(1-bound)+(-bump*sign(quote)),quote*(1+bound)+(bump*sign(quote)))))
}
stock<-function(STOCK=get(symbols[j]),Last=h){
  colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    if(length(STOCK$open)<Last){Last<-length(STOCK$open)}
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
      y<-c(y,(sign(streak$tvalue[i]):streak$tvalue[i]))
  }
  return(y)
}

ptm<-proc.time()
for (j in 1:length(symbols)){
  STOCK<-stock()
}
(proc.time()-ptm)
  
ptm<-proc.time()
for(k in 1:500){
STOCK<-stock()
  }
(proc.time()-ptm)
