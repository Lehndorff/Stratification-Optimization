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

ptm<-proc.time()
PctupGen<-NULL
GenDen<-NULL
for (j in 1:length(symbols)){
  STOCK<-stock(RUN = FALSE,Last = 3000)
  STOCK$match<-0
  qSTOCK<-q[q$V1%in%symbols[j],]
  find<-c(STOCK$UP[(length(STOCK$UP)-4):length(STOCK$UP)],as.numeric(qSTOCK$sign==1))
  for (i in 1:(length(STOCK$UP)-5)){
    if(sum(as.vector(STOCK$UP[i:(i+5)])==find)==6){
      STOCK$match[i+5]<-1
    }
  }
  PctupGen<-c(PctupGen,mean(STOCK$lag[STOCK$match==1],na.rm = TRUE))
  GenDen<-c(GenDen,sum(STOCK$match))
}

PctGen<-as.data.frame(cbind(symbols,PctupGen,GenDen))
names(PctGen)<-c("symb","Pctup","PctupDen")
PctGen[,c("Pctup","PctupDen")]<-sapply(PctGen[,c("Pctup","PctupDen")],as.character)
PctGen[,c("Pctup","PctupDen")]<-sapply(PctGen[,c("Pctup","PctupDen")],as.numeric)
proc.time()-ptm




table(STOCK$match)
table(STOCK$lag[STOCK$match==1])
