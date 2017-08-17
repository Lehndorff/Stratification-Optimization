library(quantmod)
library(dplyr)
library(data.table)
library(beepr)
symbolsDS<-c("DIA","SPY")
symbols <-symbols2
l<-match("MMM",symbols)
for (i in l:length(symbols)){
  loadSymbols(Symbols = symbols[i])
  print(i)
}
ptm<-proc.time()
Marks<-c(65,124,189,253,337,420,505,1008,1511,3000)
Historyday<-NULL
Historyrun<-NULL
RunOn<-TRUE
reset<-1
for (z in 33:26){
  bit<-z
  PctupRun<-NULL
  PctupDay<-NULL
  PctrfRun<-NULL
  PctrfDay<-NULL
  DenRun<-NULL
  DenDay<-NULL
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
    STOCK<-stock(RUN = RunOn)
    if(RunOn==TRUE){STOCKstreak<-STOCK%>%group_by(streak)%>%summarise(strkrf=mean(rflag,na.rm=TRUE),strkup=sum(lag,na.rm=TRUE),strkn=n(),strkpct=strkup/strkn)}
    qSTOCK<-q[q$V1%in%symbols[j],]
    quote<-qSTOCK$Change/qSTOCK$Last*100
    if(RunOn==TRUE){
      Run<-c(Run,sum(as.numeric(STOCKstreak$streak[runtd()])))
      Runpct<-c(Runpct,sum(as.numeric(STOCKstreak$strkpct[runtd()])))
      Runrf<-c(Runrf,sum(as.numeric(STOCKstreak$strkrf[runtd()])))
      RunDen<-c(RunDen,sum(as.numeric(STOCKstreak$strkn[runtd()])))
    }
    Day<-c(Day,round(quote,3))
    Daypct<-c(Daypct,mean(STOCK$lag[between(STOCK$pCHANGE,drange()[1],drange()[2])],na.rm = TRUE))
    Dayrf<-c(Dayrf,mean(STOCK$rflag[between(STOCK$pCHANGE,drange()[1],drange()[2])],na.rm = TRUE))
    DayDen<-c(DayDen,sum(!is.na(STOCK$lag[between(STOCK$pCHANGE,drange()[1],drange()[2])])))
  }
  if(RunOn==TRUE){
    PctupRun<-cbind(PctupRun,Runpct)
    PctrfRun<-cbind(PctrfRun,Runrf)
    DenRun<-cbind(DenRun,RunDen)
  }
  PctupDay<-cbind(PctupDay,Daypct)
  PctrfDay<-cbind(PctrfDay,Dayrf)
  DenDay<-cbind(DenDay,DayDen)
  # print(h)
}
cols<-paste0(Marks,sep="x")
if(RunOn==TRUE){
  PctupRun<-as.data.frame(cbind(symbols,Run,PctupRun))
  names(PctupRun)<-c("symb","Run",cols)
  PctupRun[,cols]<-sapply(PctupRun[,cols],as.character)
  PctupRun[,cols]<-sapply(PctupRun[,cols],as.numeric)
  PctrfRun<-as.data.frame(cbind(symbols,Run,PctrfRun))
  names(PctrfRun)<-c("symb","Run",cols)
  PctrfRun[,cols]<-sapply(PctrfRun[,cols],as.character)
  PctrfRun[,cols]<-sapply(PctrfRun[,cols],as.numeric)
  DenRun<-as.data.frame(cbind(symbols,Run,DenRun))
  names(DenRun)<-c("symb","Run",cols)
  DenRun[,cols]<-sapply(DenRun[,cols],as.character)
  DenRun[,cols]<-sapply(DenRun[,cols],as.numeric)
}
PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay))
names(PctupDay)<-c("symb","Day",cols)
PctupDay[,cols]<-sapply(PctupDay[,cols],as.character)
PctupDay[,cols]<-sapply(PctupDay[,cols],as.numeric)
PctrfDay<-as.data.frame(cbind(symbols,Day,PctrfDay))
names(PctrfDay)<-c("symb","Day",cols)
PctrfDay[,cols]<-sapply(PctrfDay[,cols],as.character)
PctrfDay[,cols]<-sapply(PctrfDay[,cols],as.numeric)
DenDay<-as.data.frame(cbind(symbols,Day,DenDay))
names(DenDay)<-c("symb","Day",cols)
DenDay[,cols]<-sapply(DenDay[,cols],as.character)
DenDay[,cols]<-sapply(DenDay[,cols],as.numeric)

if(RunOn==TRUE){
  PctupRun$min<-0
  PctupRun$max<-0
  PctupRun$mean<-0
  PctupRun$sd<-0
}
PctupDay$min<-0
PctupDay$max<-0
PctupDay$mean<-0
PctupDay$sd<-0
for (j in 1:length(symbols)){
  if(RunOn==TRUE){
    PctupRun$min[j]<-min(as.numeric(PctupRun[j,cols]),na.rm=TRUE)
    PctupRun$max[j]<-max(as.numeric(PctupRun[j,cols]),na.rm=TRUE)
    PctupRun$mean[j]<-mean(as.numeric(PctupRun[j,cols]),na.rm=TRUE)
    PctupRun$sd[j]<-sd(as.numeric(PctupRun[j,cols]),na.rm=TRUE)
  }
  PctupDay$min[j]<-min(as.numeric(PctupDay[j,cols]),na.rm=TRUE)
  PctupDay$max[j]<-max(as.numeric(PctupDay[j,cols]),na.rm=TRUE)
  PctupDay$mean[j]<-mean(as.numeric(PctupDay[j,cols]),na.rm=TRUE)
  PctupDay$sd[j]<-sd(as.numeric(PctupDay[j,cols]),na.rm=TRUE)
}
if(RunOn==TRUE){
  PctupRun$min[is.infinite(PctupRun$min)]<-0
  PctupRun$max[is.infinite(PctupRun$max)]<-0
  PctupRun$mean[is.nan(PctupRun$mean)]<-0
  PctupRun$sd[is.nan(PctupRun$sd)]<-0
}
PctupDay$min[is.infinite(PctupDay$min)]<-0
PctupDay$max[is.infinite(PctupDay$max)]<-0
PctupDay$mean[is.nan(PctupDay$mean)]<-0
PctupDay$sd[is.nan(PctupDay$sd)]<-0

if(RunOn==TRUE){
  checkUR<-as.vector(PctupRun$symb[PctupRun$min>.5&PctupRun$`65x`>.65&(PctupRun$`65x`-PctupRun$`124x`)>-.05&PctupRun$max>=.65&(PctupRun$min==PctupRun$`3000x`|PctupRun$max==PctupRun$`65x`)])
  checkDR<-as.vector(DenRun$symb[DenRun$`65x`>=8&DenRun$`3000x`>=50])
  # checkU<-checkUR[checkUR%in%checkUD]
  symbolsWatchR<-checkUR[checkUR%in%checkDR]
  WatchRun<-PctupRun[PctupRun$symb%in%symbolsWatchR,]
}
checkUD<-as.vector(PctupDay$symb[PctupDay$min>.5&PctupDay$`65x`>.65&(PctupDay$`65x`-PctupDay$`124x`)>-.05&PctupDay$max>=.65&(PctupDay$min==PctupDay$`3000x`|PctupDay$max==PctupDay$`65x`)])
checkDD<-as.vector(DenDay$symb[DenDay$`65x`>=8&DenDay$`3000x`>=50])
# checkD<-checkDD[checkDD%in%checkDR]  
# symbolsWatchRD<-checkU[checkU%in%checkD]
symbolsWatchD<-checkUD[checkUD%in%checkDD]
WatchDay<-PctupDay[PctupDay$symb%in%symbolsWatchD,]

  
  Tomorrow<-as.data.frame(matrix(data = NA, nrow=length(symbols),ncol = 5))
  names(Tomorrow)<-c("V1","Last","Change","Ctype","sign")
  Tomorrow$V1<-symbols
  for (j in 1:length(symbols)){
    STOCK<-na.approx(get(symbols[j]))
    colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    Tomorrow$Change[j]<-STOCK$close-lag(STOCK$close)[nrow(STOCK)-bit+2]
    Tomorrow$Last[j]<-lag(STOCK$close)[nrow(STOCK)-bit+2]
  }
  rownames(Tomorrow)<-Tomorrow$V1
  Tomorrow$sign<-sign(Tomorrow$Change)
  if(RunOn==TRUE){
    TomorrowWatchR<-Tomorrow[Tomorrow$V1 %in% symbolsWatchR,]
    TomorrowWatchR<-TomorrowWatchR[order(TomorrowWatchR$V1),]
    Printrun<-cbind(merge(select(WatchRun,symb,Run,min,max,mean,sd),select(TomorrowWatchR,V1,Last,Change,sign),by.x="symb",by.y = "V1"), date=row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
    Historyrun<-rbind(Historyrun,Printrun)
  }
  TomorrowWatchD<-Tomorrow[Tomorrow$V1 %in% symbolsWatchD,]
  TomorrowWatchD<-TomorrowWatchD[order(TomorrowWatchD$V1),]
  Printday<-cbind(merge(select(WatchDay,symb,Day,min,max,mean,sd),select(TomorrowWatchD,V1,Last,Change,sign),by.x="symb",by.y = "V1"), date=row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  Historyday<-rbind(Historyday,Printday)
  
  print(row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  print(sum(Printday$sign[Printday$sign==1])/length(Printday$sign))
  print(sum(Printrun$sign[Printrun$sign==1])/length(Printrun$sign))
  # print("Day")
  # print(sum(sign(as.numeric(as.vector(TomorrowWatchD$sign)))>0)/length(TomorrowWatchD$sign))
  # Dayup<-(Dayup+sum(sign(as.numeric(as.vector(TomorrowWatchD$sign)))>0))*reset
  # Daytot<-(Daytot+length(TomorrowWatchD$sign))*reset
  # Dayprof<-(Dayprof+sum(TomorrowWatchD$Change))*reset
  # print(sum(sign(PctupDay$mean-.5)==sign(Tomorrow$Change)))
  # Dayzmean<-(Dayzmean+sum(sign(PctupDay$mean-.5)==sign(Tomorrow$Change)))*reset
  # Dayzmin<-(Dayzmin+sum(sign(PctupDay$min-.5)==sign(Tomorrow$Change)))*reset
  # Dayzmax<-(Dayzmax+sum(sign(PctupDay$max-.5)==sign(Tomorrow$Change)))*reset
  # Dayz65x<-(Dayz65x+sum(sign(PctupDay$`65x`-.5)==sign(Tomorrow$Change)))*reset
  # Dayz3000x<-(Dayz3000x+sum(sign(PctupDay$`3000x`-.5)==sign(Tomorrow$Change)))*reset
  # print(sum(TomorrowWatchD$Change))
  # print("Run")
  # Runup<-(Runup+sum(sign(as.numeric(as.vector(TomorrowWatchR$sign)))>0))*reset
  # Runtot<-(Runtot+length(TomorrowWatchR$sign))*reset
  # Runprof<-(Runprof+sum(TomorrowWatchR$Change))*reset
  # print(sum(sign(as.numeric(as.vector(TomorrowWatchR$sign)))>0)/length(TomorrowWatchR$sign))
  # print(sum(sign(PctupRun$mean-.5)==sign(Tomorrow$Change)))
  # Runzmean<-(Runzmean+sum(sign(PctupRun$mean-.5)==sign(Tomorrow$Change)))*reset
  # Runzmin<-(Runzmin+sum(sign(PctupRun$min-.5)==sign(Tomorrow$Change)))*reset
  # Runzmax<-(Runzmax+sum(sign(PctupRun$max-.5)==sign(Tomorrow$Change)))*reset
  # Runz65x<-(Runz65x+sum(sign(PctupRun$`65x`-.5)==sign(Tomorrow$Change)))*reset
  # Runz3000x<-(Runz3000x+sum(sign(PctupRun$`3000x`-.5)==sign(Tomorrow$Change)))*reset
  # print(sum(TomorrowWatchR$Change))
  # print(sum(sign(PctupDay$mean-.5)==sign(Tomorrow$Change))>=sum(sign(PctupRun$mean-.5)==sign(Tomorrow$Change)))

  # print((proc.time()-ptm)/60)
}
(proc.time()-ptm)/60
beep(2)

HistDayAgg<-Historyday%>%group_by(sign)%>%summarise(min=mean(min),max=mean(max),mean=mean(mean),sd=mean(sd),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistDayAgg2<-Historyday%>%group_by(date)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistDayAgg3<-Historyday%>%group_by(symb)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistRunAgg<-Historyrun%>%group_by(sign)%>%summarise(min=mean(min),max=mean(max),mean=mean(mean),sd=mean(sd),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistRunAgg2<-Historyrun%>%group_by(date)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
HistRunAgg3<-Historyrun%>%group_by(symb)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())

Historyrun$conc<-paste(Historyrun$symb,Historyrun$date)
Historyday$conc<-paste(Historyday$symb,Historyday$date)
Histboth<-left_join(Historyday,Historyrun,by="conc")
table(Histboth$sign.y)
