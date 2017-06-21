library(quantmod)
library(dplyr)
library(data.table)
library(ggplot2)
symbols<-c("DIA","VOO","FAS","XLF")
symbols<-c("AMD","MCHP","NVDA","MU")
symbols<-c("KMI","DVN","OKE","ADP")
symbols<-c("LUV","FB", "KMX","AAPL","RTN")
symbols<-c("OKE","LUV","RTN")
symbols<-c("INCY","SHPG","GS","JPM","BIIB","AAL","REGN")
symbols<-c("IMW","ITB","XHB","XOP","ITA","XBI","EEM","SCHH","UNG","SIL")
symbols<-c("DRWI","WAC","CRK")
symbols<-c("ADP","SMH","MU","VOO","AAPL","FB","LUV","KMX","NVDA","DIA")
symbols<-c("JD","BIDU","ORLY","DBK","TKA")
symbols<-c("VRX","NVAX","AZN","COH","DRYS")
symbols<-c("ARGS","CERU","CRTN","CVM","DCTH","MBRX","RNVA","SHIP","BIOC","PULM","GSAT","MNKD","NVAX","ANY")
getSymbols(Symbols = symbols)
STOCK<-CMS
colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
STOCK$row<-1:length(STOCK$open)
STOCK$pCHANGE<-(STOCK$close-STOCK$open)/STOCK$open*100
STOCK$UP<-0
STOCK$UP[STOCK$close>STOCK$open]<-1
streak<-data.frame(unclass(rle(as.vector(STOCK$UP))))
streak$values[streak$values==0]<--1
streak$tvalue<-streak$lengths*streak$values
y<-as.vector(NULL)
for(i in 1:length(streak$lengths)){
  y<-c(y,rep(streak$tvalue[i],times=streak$lengths[i]))
}
STOCK<-cbind(STOCK,y)
STOCK$NEXT<-stats::lag(STOCK$UP,-1)
STOCK$NEXT2<-stats::lag(STOCK$UP,-2)
STOCK$DU<-0
STOCK$UU<-0
STOCK$DU[(STOCK$UP-STOCK$NEXT)==-1]<-1
STOCK$UU[(STOCK$UP == 1 & STOCK$NEXT ==1)]<-1
STOCK<-as.data.frame(STOCK)
STOCK.DOWN<-STOCK[STOCK$UP==0,]
STOCK.DOWN$row<-1:length(STOCK.DOWN$row)
STOCK.DOWN$pDU<-cumsum(STOCK.DOWN$DU)/STOCK.DOWN$row
STOCK.UP<-STOCK[STOCK$UP==1,]
STOCK.UP$row<-1:length(STOCK.UP$row)
STOCK.UP$pUU<-cumsum(STOCK.UP$UU)/STOCK.UP$row
tail(STOCK.DOWN)
tail(STOCK.UP)
y<-as.matrix(as.data.frame(table(STOCK.DOWN$..2)))
y<-cbind(y,as.numeric(y[,2])/as.numeric(y[,1]))
y<-cbind(y,cumsum(y[,3]))
y<-cbind(y,as.numeric(lag(y[,4],1))/as.numeric(y[,4]))
y2<-as.matrix(as.data.frame(table(STOCK.UP$..2)))
y2<-cbind(y2,as.numeric(y2[,2])/as.numeric(y2[,1]))
y2<-cbind(y2,rev(cumsum(rev(y2[,3]))))
y2<-cbind(y2,(as.numeric(lead(y2[,4],1)))/as.numeric(y2[,4]))
y3<-rbind(y,y2)
# View(y3)
tail(STOCK)


qplot(as.numeric(y3[,1]),abs(as.numeric(y3[,4])))
y4<-as.data.frame(y3)
y4$V6<-as.numeric(as.matrix(y4$V5))/as.numeric(as.matrix(y4$Var1))

getQuote("KMX")


table(STOCK$..2)

symbols<-c("AAPL","AXP","BA","CAT","CSCO","CVX","KO","DD","XOM","GE","GS","HD","IBM","INTC",
  "JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX","V","VZ","WMT","DIS")

asdf<-as.data.frame(merge(AAPL,CAT))
x<-as.vector(AAPL$AAPL.Open)
y<-as.vector(CAT$CAT.Open)
cor(x,y)

symbolsY<-c(
"AIZ",
"ALB",
"AVB",
"AWK",
"CMA",
"COL",
"FITB",
"GPN",
"KMB",
"LNT",
"LRCX",
"PBCT",
"PNW",
"QRVO",
"SNPS",
"SYMC",
"WU")
SDFS<-Final[Final$V1 %in% symbolsY,]
getQuote(symbolsY)
