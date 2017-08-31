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
symbols <-symbols2
# symbols<-unique(c(symbolsSP,SymbolsNAS))
l<-1
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
beep()
q<-getQuote(symbols)
q$V1<-rownames(q)
q$sign<-sign(q$Change)

PctupDay<-NULL
Pctupmin<-NULL
Pctsd<-NULL
Day<-NULL
for (j in 1:length(symbols)){
  STOCK<-stock(RUN = FALSE,Last = 3000)
  qSTOCK<-q[q$V1%in%symbols[j],]
  quote<-qSTOCK$Change/qSTOCK$Last*100
  STOCKtd<-STOCK[between(STOCK$pCHANGE,drange()[1],drange()[2]),]
  STOCKtd<-STOCKtd[rev(order(STOCKtd$row)),]
  STOCKtd<-STOCKtd[!is.na(STOCKtd$pCHANGE),]
  STOCKtd$lag[is.na(STOCKtd$lag)]<-sign(quote)
  STOCKtd$running<-cummean(STOCKtd$lag)
  PctupDay<-c(PctupDay,mean(STOCKtd$running,na.rm = TRUE))
  Pctupmin<-c(Pctupmin,min(STOCKtd$running,na.rm = TRUE))
  Pctsd<-c(Pctsd,sd(STOCKtd$running,na.rm = TRUE))
  Day<-c(Day,round(quote,3))
}

PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay,Pctupmin,Pctsd))
names(PctupDay)<-c("symb","Day","Pctup","Pctmin","Pctsd")
PctupDay[,c("Day","Pctup","Pctmin","Pctsd")]<-sapply(PctupDay[,c("Day","Pctup","Pctmin","Pctsd")],as.character)
PctupDay[,c("Day","Pctup","Pctmin","Pctsd")]<-sapply(PctupDay[,c("Day","Pctup","Pctmin","Pctsd")],as.numeric)

PctupDay<-NULL
PctupRun<-NULL
Pctsd<-NULL
Day<-NULL
Run<-NULL
for (j in 1:length(symbols)){
  STOCK<-stock(Last = 3000)
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
  PctupDay<-c(PctupDay,mean(STOCKtd$running,na.rm = TRUE))
  PctupRun<-c(PctupRun,mean(STOCKtr$running,na.rm = TRUE))
  # Pctsd<-c(Pctsd,sd(STOCKtd$running,na.rm = TRUE))
  Day<-c(Day,round(quote,3))
  Run<-c(Run,runtd2())
}

PctupDay<-as.data.frame(cbind(symbols,Day,PctupDay))
names(PctupDay)<-c("symb","Day","Pctup")
PctupDay[,c("Day","Pctup")]<-sapply(PctupDay[,c("Day","Pctup")],as.character)
PctupDay[,c("Day","Pctup")]<-sapply(PctupDay[,c("Day","Pctup")],as.numeric)

PctupRun<-as.data.frame(cbind(symbols,Run,PctupRun))
names(PctupRun)<-c("symb","Run","Pctup")
PctupRun[,c("Run","Pctup")]<-sapply(PctupRun[,c("Run","Pctup")],as.character)
PctupRun[,c("Run","Pctup")]<-sapply(PctupRun[,c("Run","Pctup")],as.numeric)

find<-c(STOCK$UP[(length(STOCK$UP)-3):length(STOCK$UP)],as.numeric(qSTOCK$sign==1))
STOCK$match<-0
for (j in 1:length(STOCK$UP)){
  if(sum(as.vector(STOCK$UP[j:(j+4)])==find)==5){
    STOCK$match[j+4]<-1
  }
}
table(STOCK$match)
table(STOCK$lag[STOCK$match==1])

Streak<-NULL
pchange<-NULL
Streakz<-NULL
pchangez<-NULL
for (j in 1:length(symbols2)){
  STOCK<-stock(Last = 3000)
  qSTOCK<-q[q$V1%in%symbols[j],]
  quote<-qSTOCK$Change/qSTOCK$Last*100
  # probit<-glm(data = STOCK, lag ~ streak+pCHANGE, family = binomial(link = probit))
  print(symbols[j])
  # stargazer(probit,type = "text")
  x<-probitmfx(formula = lag ~ streak+pCHANGE,data = STOCK,robust = TRUE)
  Streak<-c(Streak,x$mfxest[1])
  pchange<-c(pchange,x$mfxest[2])
  Streakz<-c(Streakz,x$mfxest[5])
  pchangez<-c(pchangez,x$mfxest[6])
}
regress<-as.data.frame(cbind(symbols,Streak,Streakz,pchange,pchangez))
regress[,c("symbols","Streak","Streakz","pchange","pchangez")]<-sapply(regress[,c("symbols","Streak","Streakz","pchange","pchangez")],as.character)
regress[,c("Streak","Streakz","pchange","pchangez")]<-sapply(regress[,c("Streak","Streakz","pchange","pchangez")],as.numeric)

x<-probitmfx(formula = lag ~ streak+pCHANGE,data = STOCK,robust = TRUE)
as.table(x[1])
table(STOCK$lag,STOCK$streak)
