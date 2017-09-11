symbols <-symbols2
l<-match("MMM",symbols)
for (i in l:length(symbols)){
  loadSymbols(Symbols = symbols[i])
  print(i)
}
ptm<-proc.time()
Historyday<-NULL
Historyrun<-NULL
RunOn<-FALSE
for (z in 507:253){
  bit<-z
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
  PctupDay<-NULL
  PctupRun<-NULL
  Pctdn<-NULL
  Pctrn<-NULL
  Day<-NULL
  Run<-NULL
  for (j in 1:length(symbols)){
    STOCK<-stock(Last = 3000)
    STOCK<-STOCK[1:(nrow(STOCK)-bit),]
    qSTOCK<-q[q$V1%in%symbols[j],]
    quote<-qSTOCK$Change/qSTOCK$Last*100
    STOCKtd<-STOCK[between(STOCK$pCHANGE,drange()[1],drange()[2]),]
    STOCKtd<-STOCKtd[rev(order(STOCKtd$row)),]
    STOCKtd<-STOCKtd[!is.na(STOCKtd$pCHANGE),]
    STOCKtd$lag[is.na(STOCKtd$lag)]<-sign(quote)
    STOCKtd$running<-cummean(STOCKtd$lag)
    STOCKtr<-STOCK[STOCK$streak==runtd2(),]
    STOCKtr<-STOCKtr[rev(order(STOCKtr$row)),]
    STOCKtr<-STOCKtr[!is.na(STOCKtr$pCHANGE),]
    STOCKtr$lag[is.na(STOCKtr$lag)]<-sign(quote)
    STOCKtr$running<-cummean(STOCKtr$lag)
    PctupDay<-c(PctupDay,(mean(STOCKtd$running,na.rm = TRUE)-mean(STOCKtd$lag)))
    PctupRun<-c(PctupRun,(mean(STOCKtr$running,na.rm = TRUE)-mean(STOCKtr$lag)))
    Pctrn<-c(Pctrn,length(STOCKtr$running))
    Pctdn<-c(Pctdn,length(STOCKtd$running))
    Day<-c(Day,round(quote,3))
    Run<-c(Run,runtd2())
  }

  PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay,Pctdn))
  names(PctupDay)<-c("symb","Day","Pctup","Pctdn")
  PctupDay[,c("Day","Pctup","Pctdn")]<-sapply(PctupDay[,c("Day","Pctup","Pctdn")],as.character)
  PctupDay[,c("Day","Pctup","Pctdn")]<-sapply(PctupDay[,c("Day","Pctup","Pctdn")],as.numeric)
  
  PctupRun<-as.data.frame(cbind(symbols,Run,PctupRun,Pctrn))
  names(PctupRun)<-c("symb","Run","Pctup","Pctrn")
  PctupRun[,c("Run","Pctup","Pctrn")]<-sapply(PctupRun[,c("Run","Pctup","Pctrn")],as.character)
  PctupRun[,c("Run","Pctup","Pctrn")]<-sapply(PctupRun[,c("Run","Pctup","Pctrn")],as.numeric)

  symbolsWatch<-symbols
  # symbolsWatch<-PctupDay$symb[PctupDay$Pctup>.7]
  WatchDay<-PctupDay[PctupDay$symb%in%symbolsWatch,]
  WatchRun<-PctupRun[PctupRun$symb%in%symbolsWatch,]

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
  Printday<-cbind(merge(WatchDay,TomorrowWatch,by.x="symb",by.y = "V1"), date=row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  Printrun<-cbind(merge(WatchRun,TomorrowWatch,by.x="symb",by.y = "V1"), date=row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  Historyrun<-rbind(Historyrun,Printrun)
  Historyday<-rbind(Historyday,Printday)
  print(row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  print(sum(Printday$sign[Printday$sign==1])/length(Printday$sign))
}
(proc.time()-ptm)/60

Historyday$conc<-paste(Historyday$symb,Historyday$date)
Historyrun$conc<-paste(Historyrun$symb,Historyrun$date)
Historyfull<-left_join(Historyday,Historyrun,by="conc")

HF2<-Historyfull[Historyfull$Pctup.x>.5&Historyfull$Pctup.y>.5,]

table(HF2$sign.x[(HF2$Pctup.x+HF2$Pctup.y>1.1)&(HF2$Pctdn+HF2$Pctrn>20)])

table(round(HF2$Pctup.x[HF2$sign.x==-1&HF2$Pctrn>5&HF2$Pctdn>5],1),
  round(HF2$Pctup.y[HF2$sign.x==-1&HF2$Pctrn>5&HF2$Pctdn>5],1))

HistDayAgg<-Historyday%>%group_by(sign)%>%summarise(mean=mean(Pctup),sd=mean(sd,na.rm=TRUE),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistDayAgg2<-Historyday%>%group_by(date)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistDayAgg3<-Historyday%>%group_by(symb)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())

hist(Historyday$Pctup[Historyday$sign==-1],breaks = 0:100*.01)
table(Historyday$sign[Historyday$Pctup>.65])

for (i in 10:90*.01){
  HD2<-Historyday[(Historyday$Pctup>i),]
  HistDayAgg<-HD2%>%group_by(sign)%>%summarise(Pctup=mean(Pctup),sd=mean(Pctsd,na.rm=TRUE),min=mean(Pctmin),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
  HistDayAgg2<-HD2%>%group_by(date)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),Pctup=mean(Pctup),sd=mean(Pctsd,na.rm=TRUE),min=mean(Pctmin),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
  HistDayAgg3<-HD2%>%group_by(symb)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),Pctup=mean(Pctup),sd=mean(Pctsd,na.rm=TRUE),min=mean(Pctmin),Day=mean(Day),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
  print(i)
  print(length(unique(HD2$date)))
  print(sum(Historyday$sign[Historyday$Pctup>i]==1)/sum(Historyday$Pctup>i))
  # print(sum(Historyday$sign[Historyday$Pctup>i&Historyday$Pctmin>.5]==1)/sum((Historyday$Pctup>i&Historyday$Pctmin>.5)))
  print(as.numeric(sum(HistDayAgg2$prof)/max(HistDayAgg2$In)*100))
  print(max(HistDayAgg2$In))
  print(min(HistDayAgg2$prof))
}

i<-.75
table(Historyday$sign[Historyday$Pctup>i])
sum(Historyday$sign[Historyday$Pctup>i]==1)/sum(Historyday$Pctup>i)

symbolsdayup<-NULL
for (j in 1:length(symbols)){
  subs<-subset(Historyday,symb==symbols[j])
  x<-probitmfx(formula = as.numeric(sign==1) ~ Pctup+Pctdn,data = subs,robust = TRUE)
  print(x)
  if (abs(x$mfxest[7])<.05&x$mfxest[1]>0){
    # symbolsdayup<-c(symbolsdayup,symbols[j])
    # print(symbols2[j])
    # print(x)
  }
}

