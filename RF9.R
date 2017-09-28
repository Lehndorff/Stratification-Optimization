symbols<-symbols2
quotes<-getQuote(symbols)
quotes$symb<-row.names(quotes)
colnames(quotes)<-c("Time","close","Change","Change%","open","high","low","volume","symb")
quotes$open<-as.numeric(quotes$open)
quotes$open[is.na(quotes$open)]<-0
quotes$date<-substr(quotes$Time,1,10)
# ALLYES<-NULL
ALLTODAY<-NULL
for (j in 1:length(symbols)){
  STOCK<-stock(STOCK = get(symbols[j]),Last = 3000,id=TRUE,symb = TRUE,today = TRUE)
  STOCK[is.na(STOCK)]<-0
  STOCK2<-STOCK%>%group_by(UP)%>%mutate(rfall=cummean(rflag),lagmean=cummean(lag),rfall253=SMA(rflag,n=153),lagmean253=SMA(lag, n=153),rfall50=SMA(rflag,n=50),lagmean50=SMA(lag,n=50))
  # ALLYES<-bind_rows(ALLYES,STOCK2)
  ALLTODAY<-bind_rows(ALLTODAY,tail(STOCK2,n=1))
  print(j)
}
check<-subset(ALLTODAY,rfall253>.2&lagmean253>.5&close<1000&abs(pCHANGE)<=54.5&rfall50>.5&lagmean50>.5)
getQuote(check$symb)

SUBYES<-subset(ALLYES,rfall253>.2&lagmean253>.5&as.numeric(substr(date,1,4))>=2014&close<100&abs(pCHANGE)<=2.5&rfall50>.4&lagmean50>.5)
sum(SUBYES$difflag,na.rm = TRUE)
SUBYESagg<-SUBYES%>%group_by(substr(date,1,7))%>%summarise(In=sum(close),ret=sum(difflag,na.rm=TRUE),n=n())
range(SUBYESagg$In)
sum(SUBYESagg$ret)/max(SUBYESagg$In)
max(SUBYESagg$n)
length(SUBYESagg$date)/length(unique(ALLYES$date[substr(ALLYES$date,1,4)=="2017"]))
length(unique(SUBYES$symb))

# Yesterday<-subset(ALLYES,ALLYES$id%in%check)
TESTYES<-subset(ALLYES,substr(date,1,3)=="201"&lagmean253>.5&lagmean50>.5&abs(pCHANGE)<2.5&close<100)
Best<--100
for (a in -5:5){
  for (b in 1:1){
    for(c in -5:5){
      for (d in 3:3){
        print(paste(a,b,c,d))
        SUBYES<-subset(TESTYES,rfall253>c*.1&rfall50>a*.1)
        SUBYESagg<-SUBYES%>%group_by(date)%>%summarise(In=sum(close),ret=sum(difflag,na.rm=TRUE),n=n())
        if(max(SUBYESagg$In)<1000000&sum(SUBYES$difflag,na.rm = TRUE)/max(SUBYESagg$In,1,na.rm = TRUE)>Best&(nrow(SUBYESagg)/length(unique(TESTYES$date)))>.0){
          Best<-sum(SUBYES$difflag,na.rm = TRUE)/max(SUBYESagg$In,1,na.rm = TRUE)
          id<-paste(a,b,c,d)
        }
      }
    }
  }
}

