symbolsToday<-c("XOM")
symbol<-symbolsToday[1]
# q<-getQuote(symbol)
# q$V1<-rownames(q)
# q$Ctype<-cut((q$Change/(q$Change+q$Last)*100),breaks = c(-Inf,-5,-2,-1,-.75,-.5,-.25,-.1,0,.1,.25,.5,.75,1,2,5,Inf),
#       labels = c("-5","-2","-1","-.75","-.5","-.25","-.1","-0","0",".1",".25",".5",".75","1","2","5"))
# q$sign<-sign(q$Change)
Marks<-c(65,124,189,253,337,420,505,1008,1511,3000)
quote<--0.05
Daypct<-NULL
for (h in Marks){
  Last<-h
  STOCK<-get(symbol)
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
  STOCK$Ctypelag<-as.vector(cut(STOCK$rflag,breaks = c(-Inf,-5,-2,-1,-.75,-.5,-.25,-.1,0,.1,.25,.5,.75,1,2,5,Inf),
    labels = c("-5","-2","-1","-.75","-.5","-.25","-.1","-0","0",".1",".25",".5",".75","1","2","5")))
  colnames(STOCK)[colnames(STOCK)=="..2"]<-"streak"
  # STOCKstreak<-STOCK%>%group_by(streak)%>%summarise(strkrf=mean(rflag,na.rm=TRUE),strkup=sum(lag,na.rm=TRUE),strkn=n(),strkpct=strkup/strkn)
  # STOCKday<-STOCK%>%group_by(Ctype)%>%summarise(dayrf=mean(rflag,na.rm=TRUE),dayup=sum(lag,na.rm=TRUE),dayn=n(),daypct=dayup/dayn)
  # STOCKday$Ctype[is.na(STOCKday$Ctype)]<-"X"
  Daypct<-c(Daypct,mean(STOCK$lag[between(STOCK$pCHANGE,sort(c(quote*.8+(-.01*sign(quote)),quote*1.2+(.01*sign(quote))))[1],sort(c(quote*.8+(-.01*sign(quote)),quote*1.2+(.01*sign(quote))))[2])],na.rm = TRUE))
}
Daypct
