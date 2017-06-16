library(quantmod)
library(dplyr)
library(data.table)
symbolsList<-c("CMS","TEL","D","SNPS","APH","NSC","MCO","ALLE","ESS","ADBE","RSG","BCR","NI","EIX","SPGI")
symbolsH<-c("PKI","ALB","KLAC","ADBE","BBY","HPQ","IDXX","SNPS","SPXL","DIA")
symbolsDOW<-c("AAPL","AXP","BA","CAT","CSCO","CVX","KO","DD","XOM","GE","GS","HD","IBM","INTC",
  "JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX","V","VZ","WMT","DIS")
symbols<-"FB"
symbols<-symbolsSP
loadSymbols(Symbols = symbols)
q<-getQuote(symbols)
Marks<-c(150,250,500,1000,3000)
Marks<-c(65,124,189,253,337,420,505,1008,1511,3000)
TrendpUp<-NULL
TrendpScore<-NULL
for (h in Marks){
  Results<-NULL
  Bounces<-NULL
  for (j in 1:length(symbols)){
    Last<-h
    STOCK<-get(symbols[j])
    colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    if(length(STOCK$open)<Last){
      Last<-length(STOCK$open)
    }
    STOCK<-STOCK[(length(STOCK$open)-Last):length(STOCK$open),]
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
    y<-as.matrix(as.data.frame(table(STOCK.DOWN$..2)))
    y<-cbind(y,as.numeric(y[,2])/as.numeric(y[,1]))
    y<-cbind(y,cumsum(y[,3]))
    y<-cbind(y,as.numeric(lag(y[,4],1))/as.numeric(y[,4]))
    y2<-as.matrix(as.data.frame(table(STOCK.UP$..2)))
    y2<-cbind(y2,as.numeric(y2[,2])/as.numeric(y2[,1]))
    y2<-cbind(y2,rev(cumsum(rev(y2[,3]))))
    y2<-cbind(y2,(as.numeric(lead(y2[,4],1)))/as.numeric(y2[,4]))
    y3<-rbind(y,y2)
    y3<-cbind(y3,as.numeric(y3[,5])/as.numeric(y3[,1]))
    row<-match(TRUE,(STOCK[length(STOCK$open),10]==y3[,1]))
    Result<-c(symbols[j],STOCK[length(STOCK$open),10],y3[row,5],
      sum(as.numeric(y3[abs(as.numeric(y3[,1]))<(min(abs(range(as.numeric(y3[,1])))-1)),6]),na.rm=TRUE))
    Bounce<-c(symbols[j],y3[y3[,1]==-1,c(4,5)])
    Results<-rbind(Results,Result)
    Bounces<-rbind(Bounces,Bounce)
  }
  
  q$V1<-rownames(q)
  real<-merge(Results,q)
  
  real$today<-sign(real$Change)
  real$streak<-sign(as.numeric(as.matrix(real$V2)))
  real$Next<-0
  real$V2<-as.numeric(as.matrix(real$V2))
  real$Next[real$streak==real$today]<-real$V2[real$streak==real$today]+real$today[real$streak==real$today]
  real$Next[real$Next==0]<-real$today[real$Next==0]
  
  rerun<-select(real,V1,V4,V2,V3,Next)
  rerun$Next[rerun$Next==0]<--1
  symbols2<-as.character(rerun$V1)
  Results2<-NULL
  for (j in 1:length(symbols2)){
    Last<-h
    STOCK<-get(symbols2[j])
    colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    if(length(STOCK$open)<Last){
      Last<-length(STOCK$open)
    }
    STOCK<-STOCK[(length(STOCK$open)-Last):length(STOCK$open),]
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
    y<-as.matrix(as.data.frame(table(STOCK.DOWN$..2)))
    y<-cbind(y,as.numeric(y[,2])/as.numeric(y[,1]))
    y<-cbind(y,cumsum(y[,3]))
    y<-cbind(y,as.numeric(lag(y[,4],1))/as.numeric(y[,4]))
    y2<-as.matrix(as.data.frame(table(STOCK.UP$..2)))
    y2<-cbind(y2,as.numeric(y2[,2])/as.numeric(y2[,1]))
    y2<-cbind(y2,rev(cumsum(rev(y2[,3]))))
    y2<-cbind(y2,(as.numeric(lead(y2[,4],1)))/as.numeric(y2[,4]))
    y3<-rbind(y,y2)
    y3<-cbind(y3,as.numeric(y3[,5])/as.numeric(y3[,1]))
    row<-match(TRUE,(rerun$Next[j]==y3[,1]))
    Result2<-c(symbols2[j],rerun$Next[j],y3[row,5])
    Results2<-rbind(Results2,Result2)
  }
  Results2<-as.data.frame(Results2)
  colnames(Results2)<-c("V1","Next","pNext")
  Final<-left_join(rerun,Results2,by="V1")
  Final$pNext<-as.numeric(as.matrix(Final$pNext))
  Final$pUP[sign(Final$Next.x)==1]<-Final$pNext[sign(Final$Next.x)==1]
  Final$pUP[sign(Final$Next.x)==-1]<-1-Final$pNext[sign(Final$Next.x)==-1]
  Final$upscore<-Final$pUP*as.numeric(as.matrix(Final$V4))
  TrendpUp<-cbind(TrendpUp,Final$pUP)
  TrendpScore<-cbind(TrendpScore,Final$upscore)
}
TrendpUp<-cbind(as.vector(as.matrix(Final$V1)),TrendpUp)
TrendpScore<-cbind(as.vector(as.matrix(Final$V1)),TrendpScore)
TrendpUp<-as.data.frame(TrendpUp[,1:(length(Marks)+1)])
TrendpScore<-as.data.frame(TrendpScore[,1:(length(Marks)+1)])
TrendpUp[,2:(length(Marks)+1)]<-as.numeric(as.matrix(TrendpUp[,2:(length(Marks)+1)]))
TrendpScore[,2:(length(Marks)+1)]<-as.numeric(as.matrix(TrendpScore[,2:(length(Marks)+1)]))
TrendpUp<-as.data.frame(TrendpUp)
TrendpScore<-as.data.frame(TrendpScore)
for (p in 1:length(TrendpUp$V1)){
  TrendpUp$sum[p]<-sum(TrendpUp[p,2:(length(Marks)+1)])
  TrendpScore$sum[p]<-sum(TrendpScore[p,2:(length(Marks)+1)],na.rm=TRUE)
}
TrendpUp$YES<-0
# TrendpUp$YES[TrendpUp$V2>.6&TrendpUp$V3>.6&TrendpUp$V4>.6&TrendpUp$V5>.6&TrendpUp$V6>.55]<-1
for (t in 1:length(TrendpUp$V1)){
  TrendpUp$YES[t]<-((sum(TrendpUp[t,2:8]>.6,na.rm = TRUE)+sum(TrendpUp[t,9:11]>.55,na.rm = TRUE)))*(sum(TrendpUp[t,2:11]<=.5,na.rm=TRUE)==0)
}

UpX<-as.data.frame(cbind(select(TrendpUp,V1,YES),TrendpScore$sum))
# UpX<-as.data.frame(cbind(TrendpUp[,c(1,16)],TrendpScore$sum))

UpX$Score2<-UpX$YES*UpX$`TrendpScore$sum`
# UpX$Score2<-UpX$sum*UpX$`TrendpScore$sum`
# UpX$`TrendpScore$sum`<-UpX$`TrendpScore$sum`-.001
View(UpX)
symbolsWatch<-as.vector(unique(UpX$V1[((UpX$YES>=8)+(UpX$`TrendpScore$sum`>=.13))==2]))
symbolsWatch<-symbolsWatch[!is.na(symbolsWatch)]

UpXWatch<-UpX[UpX$V1 %in% symbolsWatch,]
TrendupWatch<-TrendpUp[TrendpUp$V1 %in% symbolsWatch,]
TrendscWatch<-TrendpScore[TrendpScore$V1 %in% symbolsWatch,]
TrendupWatch<-TrendupWatch%>%group_by(V1)%>%mutate(min=min(V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,na.rm=TRUE),max=max(V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,na.rm=TRUE),mean=mean(V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,nan.rm=TRUE))

write.csv(UpXWatch,"~/Desktop/UpXWatch.csv")
write.csv(TrendupWatch,"~/desktop/TrendUp.csv")
write.csv(TrendscWatch,"~/desktop/TrendScore.csv")

UpXWToday<-merge(UpXWatch,q)
