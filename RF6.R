library(quantmod)
library(dplyr)
library(data.table)
library(beepr)
symbolsETF<-c("DIA","SPY","FAS","QQQ","SPXL","SOCL","IHF","GXF","IGV","BJK","RYH","MTK","IYH","XLF","XLV","FEZ","RPG","ITB","ITA","VONG","PPA","IWF","VOOG","SPXS","UDOW","SH")
symbolsDOW<-c("AAPL","AXP","BA","CAT","CSCO","CVX","KO","DD","XOM","GE","GS","HD","IBM","INTC",
  "JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX","V","VZ","WMT","DIS")
symbolsSP<-as.vector(read.csv("~/desktop/symbolsSP.csv")[,2])
symbolsH<-c("AVB","PNW","SO","TSS","WAT","CTSH","KO","DIA","SPY")
symbols<-"COO"
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
    STOCK<-stock()
    STOCKstreak<-STOCK%>%group_by(streak)%>%summarise(strkrf=mean(rflag,na.rm=TRUE),strkup=sum(lag,na.rm=TRUE),strkn=n(),strkpct=strkup/strkn)
    qSTOCK<-q[q$V1%in%symbols[j],]
    quote<-qSTOCK$Change/qSTOCK$Last*100
    Run<-c(Run,sum(as.numeric(STOCKstreak$streak[runtd()])))
    Day<-c(Day,round(quote,3))
    Runpct<-c(Runpct,sum(as.numeric(STOCKstreak$strkpct[runtd()])))
    Daypct<-c(Daypct,mean(STOCK$lag[between(STOCK$pCHANGE,drange()[1],drange()[2])],na.rm = TRUE))
    Runrf<-c(Runrf,sum(as.numeric(STOCKstreak$strkrf[runtd()])))
    Dayrf<-c(Dayrf,mean(STOCK$rflag[between(STOCK$pCHANGE,drange()[1],drange()[2])],na.rm = TRUE))
    RunDen<-c(RunDen,sum(as.numeric(STOCKstreak$strkn[runtd()])))
    DayDen<-c(DayDen,sum(!is.na(STOCK$lag[between(STOCK$pCHANGE,drange()[1],drange()[2])])))
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

checkUR<-as.vector(PctupRun$symb[PctupRun$min>.5&PctupRun$`65x`>.65&(PctupRun$`65x`-PctupRun$`124x`)>-.05&PctupRun$max>=.65&(PctupRun$min==PctupRun$`3000x`|PctupRun$max==PctupRun$`65x`)])
checkUD<-as.vector(PctupDay$symb[PctupDay$min>.5&PctupDay$`65x`>.65&(PctupDay$`65x`-PctupDay$`124x`)>-.05&PctupDay$max>=.65&(PctupDay$min==PctupDay$`3000x`|PctupDay$max==PctupDay$`65x`)])
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
