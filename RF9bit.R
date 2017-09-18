library(quantmod)
library(dplyr)
library(data.table)
library(beepr)
library(mfx)
getSymbols(c("DIA","SPY"))
symbolsSP<-as.vector(read.csv("~/desktop/symbolsSP.csv")[,2])
symbols <-symbols2
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
