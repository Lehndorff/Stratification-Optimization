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
    Day<-c(Day,as.numeric(as.vector(qSTOCK$Ctype)))
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
PctupRun<-as.data.frame(cbind(symbols,Run,PctupRun))
names(PctupRun)<-c("symb","Run",as.character(Marks))
PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay))
names(PctupDay)<-c("symb","Day",as.character(Marks))
PctrfRun<-as.data.frame(cbind(symbols,Run,PctrfRun))
names(PctrfRun)<-c("symb","Run",as.character(Marks))
PctrfDay<-as.data.frame(cbind(symbols,Day,PctrfDay))
names(PctrfDay)<-c("symb","Day",as.character(Marks))
DenRun<-as.data.frame(cbind(symbols,Run,DenRun))
names(DenRun)<-c("symb","Run",as.character(Marks))
DenDay<-as.data.frame(cbind(symbols,Day,DenDay))
names(DenDay)<-c("symb","Day",as.character(Marks))

  
  