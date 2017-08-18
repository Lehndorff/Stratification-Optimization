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
q$sign<-sign(q$Change)

# PctupRun<-NULL
PctupDay<-NULL
# PctrfRun<-NULL
PctrfDay<-NULL
# DenRun<-NULL
DenDay<-NULL
Day<-NULL
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
  # STOCKstreak<-STOCK%>%group_by(streak)%>%summarise(strkrf=mean(rflag,na.rm=TRUE),strkup=sum(lag,na.rm=TRUE),strkn=n(),strkpct=strkup/strkn)

  # Run<-c(Run,sum(as.numeric(STOCKstreak$streak[runtd()])))
  # Day<-c(Day,round(quote,3))
  # Runpct<-c(Runpct,sum(as.numeric(STOCKstreak$strkpct[runtd()])))
  # Daypct<-c(Daypct,mean(STOCK$lag[between(STOCK$pCHANGE,drange()[1],drange()[2])],na.rm = TRUE))
  # Runrf<-c(Runrf,sum(as.numeric(STOCKstreak$strkrf[runtd()])))
  # Dayrf<-c(Dayrf,mean(STOCK$rflag[between(STOCK$pCHANGE,drange()[1],drange()[2])],na.rm = TRUE))
  # RunDen<-c(RunDen,sum(as.numeric(STOCKstreak$strkn[runtd()])))
  # DayDen<-c(DayDen,sum(!is.na(STOCK$lag[between(STOCK$pCHANGE,drange()[1],drange()[2])])))
}

PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay))
names(PctupDay)<-c("symb","Day","Pctup")
PctupDay[,c("Day","Pctup")]<-sapply(PctupDay[,c("Day","Pctup")],as.character)
PctupDay[,c("Day","Pctup")]<-sapply(PctupDay[,c("Day","Pctup")],as.numeric)

