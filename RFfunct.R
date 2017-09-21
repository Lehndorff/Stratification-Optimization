drange<-function(bound=.2,bump=.075){
  if (abs(quote)<.1){quote=.1*sign(quote)}
  return(sort(c(quote*(1-bound)+(-bump*sign(quote)),quote*(1+bound)+(bump*sign(quote)))))
}
stock<-function(STOCK=get(symbols[j]),Last=h,RUN=TRUE,bit=0,id=FALSE){
  STOCK<-na.omit(STOCK)
  colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
  if(length(STOCK$open)<Last){Last<-length(STOCK$open)}
  STOCK<-STOCK[(length(STOCK$open)-Last):length(STOCK$open),]
  STOCK$row<-1:length(STOCK$open)
  STOCK<-STOCK[STOCK$row>max(0,STOCK$row[STOCK$volume<=1]),]
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
  STOCK$diff<-lag(STOCK$close)*STOCK$pCHANGE/100
  STOCK$difflag<-lead(STOCK$diff,1)
  STOCK<-STOCK[1:(nrow(STOCK)-bit),]
  STOCK$date<-row.names(STOCK)
  if (id==TRUE){
    STOCK$id<-paste(symbols[j],STOCK$date)
  }
  return(STOCK)
}

runtd<-function(){
  STOCKstreak$streak==(qSTOCK$sign+(STOCK$streak[length(STOCK$streak)]*as.numeric(sign(STOCK$streak[length(STOCK$streak)])==qSTOCK$sign)))
}

runtd2<-function(){
  (qSTOCK$sign+(STOCK$streak[length(STOCK$streak)]*as.numeric(sign(STOCK$streak[length(STOCK$streak)])==qSTOCK$sign)))
}

STREAK<-function(){
  y<-as.vector(NULL)
  for(i in 1:length(streak$lengths)){
      streak$values[i]:streak$tvalue[i]
  }
  return(y)
}
symbols2<-symbolsSP
for (j in 1:length(symbolsSP)){
  stockcheck<-get(symbolsSP[j])
  colnames(stockcheck)<-c("open","high","low","close","volume","adjusted")
  stockcheck$row<-1:length(stockcheck$open)
  if (nrow(stockcheck)<507 | stockcheck$volume[length(stockcheck$volume)]<=1|sum(is.na(stockcheck))>0|(length(stockcheck$row)-max(0,stockcheck$row[stockcheck$volume<=1]))<507){
  symbols2<-symbols2[!(symbols2 %in% symbolsSP[j])]
  }
}
Historyday<-Historyday[Historyday$symb %in% symbols2,]
STOCK$ave50<-NA
for (i in 50:length(STOCK$close)){
  STOCK$ave50[i]<-mean(STOCK$close[(i-49):i])
}
STOCK$bullsi<-sign(STOCK$close-STOCK$ave50)
STOCK$bull<-STOCK$close-STOCK$ave50
STOCK$peak<-0
STOCK$peak[findPeaks(STOCK$close)]<-1
STOCK$vall<-0
STOCK$vall[findValleys(STOCK$close)]<-1

for (j in 1:length(symbols)){
  STOCK<-stock(RUN = TRUE,Last = 3000)
  STOCK$date<-row.names(STOCK)
  Test<-left_join(STOCK,left_join(STOCKDIA,STOCKSPY,by = "date"),by = "date")
  print(symbols[j])
  print(cor(Test$rflag[2:(length(Test$pCHANGE)-1)],Test$pCHANGE.x[2:(length(Test$pCHANGE)-1)]))
  print(cor(Test$rflag[2:(length(Test$pCHANGE)-1)],Test$pCHANGE.y[2:(length(Test$pCHANGE)-1)]))
}
