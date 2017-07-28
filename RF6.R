library(quantmod)
library(dplyr)
library(data.table)
library(beepr)
symbolsETF<-c("DIA","SPY","FAS","QQQ","SPXL","SOCL","IHF","GXF","IGV","BJK","RYH","MTK","IYH","XLF","XLV","FEZ","RPG","ITB","ITA","VONG","PPA","IWF","VOOG","SPXS","UDOW","SH")
symbolsDOW<-c("AAPL","AXP","BA","CAT","CSCO","CVX","KO","DD","XOM","GE","GS","HD","IBM","INTC",
  "JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX","V","VZ","WMT","DIS")
symbols<-"DIA"
symbols <-symbolsSP
# symbols<-unique(c(symbolsSP,SymbolsNAS))
l<-1
l<-match("MMM",symbols)
for (i in l:length(symbols)){
  loadSymbols(Symbols = symbols[i])
  print(i)
}
beep()
q<-getQuote(symbols)
q$V1<-rownames(q)
q$Ctype<-cut((q$Change/(q$Change+q$Last)*100),breaks = c(-Inf,-5,-2,-1,-.75,-.5,-.25,-.1,0,.1,.25,.5,.75,1,2,5,Inf),
      labels = c("-5","-2","-1","-.75","-.5","-.25","-.1","-0","0",".1",".25",".5",".75","1","2","5"))
q$sign<-sign(q$Change)
# Marks<-(65*1:45)
Marks<-c(65,124,189,253,337,420,505,1008,1511,3000)
# Marks<-c(65,124)
PctupRun<-NULL
PctupDay<-NULL
PctrfRun<-NULL
PctrfDay<-NULL
DenRun<-NULL
DenDay<-NULL
for (h in Marks){
  Runpct<-NULL
  Daypct<-NULL
  Runrf<-NULL
  Dayrf<-NULL
  RunDen<-NULL
  DayDen<-NULL
  Day<-NULL
  Run<-NULL
  for (j in 1:length(symbols)){
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
    STOCK<-as.data.frame(STOCK)
    STOCK$lag<-lead(STOCK$UP,1)
    STOCK$rflag<-lead(STOCK$pCHANGE)
    STOCK$Ctype<-as.vector(cut(STOCK$pCHANGE,breaks = c(-Inf,-5,-2,-1,-.75,-.5,-.25,-.1,0,.1,.25,.5,.75,1,2,5,Inf),
      labels = c("-5","-2","-1","-.75","-.5","-.25","-.1","-0","0",".1",".25",".5",".75","1","2","5")))
    colnames(STOCK)[colnames(STOCK)=="..2"]<-"streak"
    STOCKstreak<-STOCK%>%group_by(streak)%>%summarise(strkrf=mean(rflag,na.rm=TRUE),strkup=sum(lag,na.rm=TRUE),strkn=n(),strkpct=strkup/strkn)
    STOCKday<-STOCK%>%group_by(Ctype)%>%summarise(dayrf=mean(rflag,na.rm=TRUE),dayup=sum(lag,na.rm=TRUE),dayn=n(),daypct=dayup/dayn)
    STOCKday$Ctype[is.na(STOCKday$Ctype)]<-"X"
    qSTOCK<-q[q$V1%in%symbols[j],]
    Run<-c(Run,as.numeric(STOCKstreak[STOCKstreak$streak==(qSTOCK$sign+(STOCK$streak[length(STOCK$streak)]*as.numeric(sign(STOCK$streak[length(STOCK$streak)])==qSTOCK$sign))),"streak"]))
    Day<-c(Day,as.vector(qSTOCK$Ctype))
    Runpct<-c(Runpct,as.numeric(STOCKstreak[STOCKstreak$streak==(qSTOCK$sign+(STOCK$streak[length(STOCK$streak)]*as.numeric(sign(STOCK$streak[length(STOCK$streak)])==qSTOCK$sign))),"strkpct"]))
    Daypct<-c(Daypct,as.numeric(STOCKday[STOCKday$Ctype==qSTOCK$Ctype,"daypct"]))
    Runrf<-c(Runrf,as.numeric(STOCKstreak[STOCKstreak$streak==(qSTOCK$sign+(STOCK$streak[length(STOCK$streak)]*as.numeric(sign(STOCK$streak[length(STOCK$streak)])==qSTOCK$sign))),"strkrf"]))
    Dayrf<-c(Dayrf,as.numeric(STOCKday[STOCKday$Ctype==qSTOCK$Ctype,"dayrf"]))
    RunDen<-c(RunDen,as.numeric(STOCKstreak[STOCKstreak$streak==(qSTOCK$sign+(STOCK$streak[length(STOCK$streak)]*as.numeric(sign(STOCK$streak[length(STOCK$streak)])==qSTOCK$sign))),"strkn"]))
    DayDen<-c(DayDen,as.numeric(STOCKday[STOCKday$Ctype==qSTOCK$Ctype,"dayn"]))
  }
  PctupRun<-cbind(PctupRun,Runpct)
  PctupDay<-cbind(PctupDay,Daypct)
  PctrfRun<-cbind(PctrfRun,Runrf)
  PctrfDay<-cbind(PctrfDay,Dayrf)
  DenRun<-cbind(DenRun,RunDen)
  DenDay<-cbind(DenDay,DayDen)
  print(h)
}
cols<-paste0(Marks,sep="x")
PctupRun<-as.data.frame(cbind(symbols,Run,PctupRun))
names(PctupRun)<-c("symb","Run",cols)
PctupRun[,cols]<-sapply(PctupRun[,cols],as.character)
PctupRun[,cols]<-sapply(PctupRun[,cols],as.numeric)
PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay))
names(PctupDay)<-c("symb","Day",cols)
PctupDay[,cols]<-sapply(PctupDay[,cols],as.character)
PctupDay[,cols]<-sapply(PctupDay[,cols],as.numeric)
PctrfRun<-as.data.frame(cbind(symbols,Run,PctrfRun))
names(PctrfRun)<-c("symb","Run",cols)
PctrfRun[,cols]<-sapply(PctrfRun[,cols],as.character)
PctrfRun[,cols]<-sapply(PctrfRun[,cols],as.numeric)
PctrfDay<-as.data.frame(cbind(symbols,Day,PctrfDay))
names(PctrfDay)<-c("symb","Day",cols)
PctrfDay[,cols]<-sapply(PctrfDay[,cols],as.character)
PctrfDay[,cols]<-sapply(PctrfDay[,cols],as.numeric)
DenRun<-as.data.frame(cbind(symbols,Run,DenRun))
names(DenRun)<-c("symb","Run",cols)
DenRun[,cols]<-sapply(DenRun[,cols],as.character)
DenRun[,cols]<-sapply(DenRun[,cols],as.numeric)
DenDay<-as.data.frame(cbind(symbols,Day,DenDay))
names(DenDay)<-c("symb","Day",cols)
DenDay[,cols]<-sapply(DenDay[,cols],as.character)
DenDay[,cols]<-sapply(DenDay[,cols],as.numeric)

PctupRun$min<-0
PctupDay$min<-0
PctupRun$max<-0
PctupDay$max<-0
PctupRun$mean<-0
PctupDay$mean<-0
for (j in 1:length(symbols)){
  PctupRun$min[j]<-min(as.numeric(PctupRun[j,cols]),na.rm=TRUE)
  PctupDay$min[j]<-min(as.numeric(PctupDay[j,cols]),na.rm=TRUE)
  PctupRun$max[j]<-max(as.numeric(PctupRun[j,cols]),na.rm=TRUE)
  PctupDay$max[j]<-max(as.numeric(PctupDay[j,cols]),na.rm=TRUE)
  PctupRun$mean[j]<-mean(as.numeric(PctupRun[j,cols]),na.rm=TRUE)
  PctupDay$mean[j]<-mean(as.numeric(PctupDay[j,cols]),na.rm=TRUE)
}
PctupRun$min[is.infinite(PctupRun$min)]<-0
PctupDay$min[is.infinite(PctupDay$min)]<-0
PctupRun$max[is.infinite(PctupRun$max)]<-0
PctupDay$max[is.infinite(PctupDay$max)]<-0
PctupRun$mean[is.nan(PctupRun$mean)]<-0
PctupDay$mean[is.nan(PctupDay$mean)]<-0

checkUR<-as.vector(PctupRun$symb[PctupRun$min>.5&PctupRun$max>=.6&(PctupRun$min==PctupRun$`3000x`|PctupRun$max==PctupRun$`65x`)])
checkUD<-as.vector(PctupDay$symb[PctupDay$min>.5&PctupDay$max>=.6&(PctupDay$min==PctupDay$`3000x`|PctupDay$max==PctupDay$`65x`)])
# checkRR<-as.vector()
# checkRD<-as.vector()
checkDR<-as.vector(DenRun$symb[DenRun$`65x`>=8&DenRun$`3000x`>=50])
checkDD<-as.vector(DenDay$symb[DenDay$`65x`>=8&DenDay$`3000x`>=50])
checkU<-checkUR[checkUR%in%checkUD]
checkD<-checkDD[checkDD%in%checkDR]  
symbolsWatchRD<-checkU[checkU%in%checkD]
symbolsWatchD<-checkUD[checkUD%in%checkDD]
symbolsWatchR<-checkUR[checkUR%in%checkDR]

WatchDay<-PctupDay[PctupDay$symb%in%symbolsWatchD,]
WatchRun<-PctupRun[PctupRun$symb%in%symbolsWatchR,]
WatchDay$both<-0
WatchRun$both<-0
WatchDay$both[WatchDay$symb%in%symbolsWatchRD]<-1
WatchRun$both[WatchRun$symb%in%symbolsWatchRD]<-1
