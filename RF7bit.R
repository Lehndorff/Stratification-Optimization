symbols <-symbols2
l<-match("MMM",symbols)
for (i in l:length(symbols)){
  loadSymbols(Symbols = symbols[i])
  print(i)
}
ptm<-proc.time()
Historyday<-NULL
RunOn<-FALSE
for (z in 10:2){
  bit<-z
  PctupDay<-NULL
  q<-as.data.frame(matrix(data = NA, nrow=length(symbols),ncol = 5))
  names(q)<-c("V1","Last","Change","Ctype","sign")
  q$V1<-symbols
  for (j in 1:length(symbols)){
    STOCK<-na.approx(get(symbols[j]))
    colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    q$Change[j]<-STOCK$close-lag(STOCK$close)[nrow(STOCK)-bit+1]
    q$Last[j]<-lag(STOCK$close)[nrow(STOCK)-bit+1]
  }
  rownames(q)<-q$V1
  q$sign<-sign(q$Change)
  for (j in 1:length(symbols)){
    # Runpct<-NULL
    Daypct<-NULL
    # Runrf<-NULL
    Dayrf<-NULL
    # RunDen<-NULL
    DayDen<-NULL
    # Run<-NULL
    STOCK<-stock(RUN = FALSE)
    qSTOCK<-q[q$V1%in%symbols[j],]
    quote<-qSTOCK$Change/qSTOCK$Last*100
    STOCKtd<-STOCK[between(STOCK$pCHANGE,drange()[1],drange()[2]),]
    STOCKtd<-STOCKtd[rev(order(STOCKtd$row)),]
    STOCKtd<-STOCKtd[!is.na(STOCKtd$pCHANGE),]
    STOCKtd$lag[is.na(STOCKtd$lag)]<-sign(quote)
    STOCKtd$running<-cummean(STOCKtd$lag)
    PctupDay<-c(PctupDay,mean(STOCKtd$running,na.rm = TRUE))
    Day<-c(Day,round(quote,3))
  }
  PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay))
  names(PctupDay)<-c("symb","Day","Pctup")
  PctupDay[,c("Day","Pctup")]<-sapply(PctupDay[,c("Day","Pctup")],as.character)
  PctupDay[,c("Day","Pctup")]<-sapply(PctupDay[,c("Day","Pctup")],as.numeric)
  
  symbolsWatch<-symbols
  # symbolsWatch<-PctupDay$symb[PctupDay$Pctup>.7]
  WatchDay<-PctupDay[PctupDay$symb%in%symbolsWatch,]
  
  Tomorrow<-as.data.frame(matrix(data = NA, nrow=length(symbols),ncol = 4))
  names(Tomorrow)<-c("V1","Last","Change","sign")
  Tomorrow$V1<-symbols
  for (j in 1:length(symbols)){
    STOCK<-na.approx(get(symbols[j]))
    colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    Tomorrow$Change[j]<-STOCK$close-lag(STOCK$close)[nrow(STOCK)-bit+2]
    Tomorrow$Last[j]<-lag(STOCK$close)[nrow(STOCK)-bit+2]
  }
  rownames(Tomorrow)<-Tomorrow$V1
  Tomorrow$sign<-sign(Tomorrow$Change)
  TomorrowWatch<-Tomorrow[Tomorrow$V1 %in% symbolsWatch,]
  TomorrowWatch<-TomorrowWatch[order(TomorrowWatch$V1),]
  Printday<-cbind(merge(select(WatchDay,symb,Day,Pctup),select(TomorrowWatch,V1,Last,Change,sign),by.x="symb",by.y = "V1"), date=row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  Historyday<-rbind(Historyday,Printday)
  print(row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  print(sum(Printday$sign[Printday$sign==1])/length(Printday$sign))
}
(proc.time()-ptm)/60

HistDayAgg<-Historyday%>%group_by(sign)%>%summarise(mean=mean(Pctup),sd=mean(sd),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistDayAgg2<-Historyday%>%group_by(date)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistDayAgg3<-Historyday%>%group_by(symb)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())

hist(Historyday$Pctup[Historyday$sign==-1],breaks = 0:100*.01)
table(Historyday$sign[Historyday$Pctup>.8])
