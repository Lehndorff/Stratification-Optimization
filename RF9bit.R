library(quantmod)
library(dplyr)
library(data.table)
library(beepr)
library(mfx)
library(TTR)
getSymbols(c("DIA","SPY"))
symbolsSP<-as.vector(read.csv("~/desktop/symbolsSP.csv")[,2])
symbols <-symbolsSP
l<-match("MMM",symbols)
for (i in l:length(symbols)){
  loadSymbols(Symbols = symbols[i])
  print(i)
}
for (j in l:length(symbols)){
  check<-nrow(get(symbols[j]))
  if (check<1000){
    print(symbols[j])
    print(check)
    loadSymbols(symbols[j])
  }
}
DIAST<-stock(STOCK = get("DIA"), Last = 3000)
SPYST<-stock(STOCK = get("SPY"), Last = 3000)

comb<-left_join(DIAST,SPYST,by = "date")
table(comb$UP.x==comb$UP.y)

aaplcomb<-left_join(comb,stock(STOCK = get("AAPL"),Last = 30000),by="date")

lagxup<-NULL
lagupx<-NULL
for (j in 1:length(symbols)){
  STOCK<-stock(Last = 3000)
  combstock<-left_join(STOCK,comb,by = "date")
  print(symbols[j])
  lagxup<-c(lagxup,round(sum(combstock$pCHANGE-combstock$pCHANGE.x,na.rm = TRUE),3))
  lagupx<-c(lagupx,round(sum(combstock$pCHANGE-combstock$pCHANGE.y,na.rm = TRUE),3))
}

Result<-cbind(symbols,as.data.frame(cbind(lagxup,lagupx)))


probitmfx(lag ~ UP+UP.y+streak+streak.y+streak.x+UP.x, data = aaplcomb)

SPYST$meanstc50<-mean(SPYST$pCHANGE)
for(k in 50:nrow(SPYST)){
  SPYST$meanstc50[k]<-(mean(SPYST$close[(k-49):k])-SPYST$close[k])
}
plot(SPYST$meanstc50)
table(DIAST$lag,round(DIAST$meanst50,1))

ptm<-proc.time()
Historyday<-NULL
Historyrun<-NULL
for (z in 253:2){
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
    PctupDay<-c(PctupDay,weighted.mean(STOCKtd$lag,w=STOCKtd$row))
    PctupRun<-c(PctupRun,(mean(STOCKtr$lag,na.rm = TRUE)))
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

dsymbs<-NULL
usymbs<-NULL
symbols<-symbols2
for (j in 1:length(symbols)){
  STOCK<-stock(STOCK = get(symbols[j]),Last = 5007, bit = 253)
  STOCK$diff<-lag(STOCK$close)*STOCK$pCHANGE/100
  STOCK$difflag<-lead(STOCK$diff,1)
  STOCKagg<-STOCK%>%group_by(down=(UP==0))%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=mean(rflag,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
  STOCKagg2<-STOCK%>%group_by(streak)%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=mean(rflag,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
  # STOCKagg<-STOCK%>%group_by(down=(UP==0))%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=weighted.mean(rflag,w=row,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
  # STOCKagg2<-STOCK%>%group_by(streak)%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=weighted.mean(rflag,w=row,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
  # STOCKagg3<-STOCK%>%group_by(day=round(pCHANGE,1))%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=mean(rflag,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
  if (sign(STOCKagg$rday[1])>sign(STOCKagg$rday[2])&abs(STOCKagg$rday[1]-STOCKagg$rday[2])>.4){
    print(paste(symbols[j],"UP",(STOCKagg$rday[1]-STOCKagg$rday[2])))
    usymbs<-c(usymbs,symbols[j])
  }
  if (sign(STOCKagg$rday[1])<sign(STOCKagg$rday[2])&abs(STOCKagg$rday[1]-STOCKagg$rday[2])>.4){
    print(paste(symbols[j],"DOWN",(STOCKagg$rday[1]-STOCKagg$rday[2]),(STOCKagg$up[2]-STOCKagg$up[1])))
    dsymbs<-c(dsymbs,symbols[j])
  }
}
symbols<-dsymbs
for (j in 1:length(symbols)){
  STOCK<-stock(STOCK = get(symbols[j]),Last = 252, bit = 0)
  STOCK$diff<-lag(STOCK$close)*STOCK$pCHANGE/100
  STOCK$difflag<-lead(STOCK$diff,1)
  STOCKagg<-STOCK%>%group_by(down=(UP==0))%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=mean(rflag,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
  STOCKagg2<-STOCK%>%group_by(streak)%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=mean(rflag,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
  print(paste(symbols[j],STOCKagg$dayn[2],(STOCKagg$dayn[2]-STOCKagg$dayn[1])))
}

retdsym<-NULL
retusym<-NULL
for (t in 507:253){
  print(t)
  for (j in 1:length(symbols)){
    STOCK<-stock(STOCK = get(symbols[j]),Last = t+253, bit = t,id=TRUE)
    # STOCK$diff<-lag(STOCK$close)*STOCK$pCHANGE/100
    # STOCK$difflag<-lead(STOCK$diff,1)  
    STOCKagg<-STOCK%>%group_by(down=(UP==0))%>%summarise(n=n(),up=mean(lag,na.rm=TRUE),rday=mean(rflag,na.rm=TRUE),nday=mean(difflag,na.rm=TRUE),dayn=sum(difflag,na.rm=TRUE),tchng=sum(diff,na.rm=TRUE))
    if (sign(STOCKagg$rday[1])>sign(STOCKagg$rday[2])&abs(STOCKagg$rday[1]-STOCKagg$rday[2])>.4&last(STOCK$UP)==1){
      retusym<-c(retusym,last(STOCK$id))
    }
    if (sign(STOCKagg$rday[1])<sign(STOCKagg$rday[2])&abs(STOCKagg$rday[1]-STOCKagg$rday[2])>.4&last(STOCK$UP)==0){
      retdsym<-c(retdsym,last(STOCK$id))
    }
  }
}

ALLST<-NULL
for (j in 1:length(symbols)){
  ALLST<-bind_rows(ALLST,stock(STOCK=get(symbols[j]),Last = 1000,id=TRUE,symb=TRUE))
  print(j)
}

ALLyessymb<-ALLST[ALLST$id%in%yessymb,]
ALLretu<-ALLST[ALLST$id%in%retusym,]
ALLretdagg<-ALLretd%>%group_by(date)%>%summarise(In=sum(close),ret=sum(difflag))
ALLretuagg<-ALLretu%>%group_by(date)%>%summarise(In=sum(close),ret=sum(difflag))

yessymb<-NULL
ALLYES<-NULL
for (j in 1:length(symbols)){
  STOCK<-stock(STOCK = get(symbols[j]),Last = 3000,id=TRUE,symb = TRUE)
  STOCK[is.na(STOCK)]<-0
  STOCK2<-STOCK%>%group_by(UP)%>%mutate(rfall=cummean(rflag),lagmean=cummean(lag),diffmean=cummean(diff),rfall253=SMA(rflag,n=153),lagmean253=SMA(lag, n=153),diffmean253=SMA(diff,n=153))
  # STOCK2$YES<-((STOCK2$rfall>0.1)&(STOCK2$lagmean>.5)&(STOCK2$diffmean>0)&(STOCK2$rfall253>0.2)&(STOCK2$lagmean253>.5)&(STOCK2$diffmean253>0))
  # sum(STOCK2$YES[STOCK2$row>(nrow(STOCK2)-253)],na.rm = TRUE)
  # yessymb<-c(yessymb,STOCK2$id[STOCK2$YES&STOCK2$row>(nrow(STOCK2)-253)])
  ALLYES<-bind_rows(ALLYES,STOCK2)
  print(j)
}

sum(subset(ALLYES,rfall>-900&lagmean>0&diffmean>0&rfall253>.175&lagmean253>.505&diffmean253>0&substr(date,1,4)=="2016"&close<500)$difflag,na.rm = TRUE)
SUBYES<-subset(ALLYES,rfall>-900&lagmean>0&diffmean>0&rfall253>.175&lagmean253>.505&diffmean253>0&substr(date,1,4)=="2016"&close<500)%>%group_by(date)%>%summarise(In=sum(close),ret=sum(difflag,na.rm=TRUE))
range(SUBYES$In)
sum(SUBYES$ret)/max(SUBYES$In)
table(subset(ALLYES,rfall>-.9&lagmean>.5&diffmean>0&rfall253>.175&lagmean253>.5&diffmean253>0&substr(date,1,4)=="2017"&symb!="PCLN")$symb)
length(unique(SUBYES$date))

summary(lm(rflag ~ rfall + lagmean + diffmean + rfall253 + lagmean253 + diffmean253, data = ALLYES))


ALLyessymb<-ALLST[ALLST$id%in%yessymb,]
ALLyessymbagg<-ALLyessymb%>%group_by(date)%>%summarise(In=sum(close),ret=sum(difflag,na.rm=TRUE))
sum(ALLyessymb$difflag,na.rm = TRUE)
range(ALLyessymbagg$In)
ALLyessymbagg2<-ALLyessymb%>%group_by(symb)%>%summarise(In=sum(close),ret=sum(difflag,na.rm=TRUE))
