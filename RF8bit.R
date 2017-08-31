symbols<-symbolsGen
ptm<-proc.time()
HistoryGen<-NULL
GenL<-6
for (z in 252:2){
  bit<-z
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
  PctupGen<-NULL
  GenDen<-NULL
  for (j in 1:length(symbols)){
    STOCK<-stock(RUN = FALSE,Last = 3000,bit=bit)
    STOCK$match<-0
    qSTOCK<-q[q$V1%in%symbols[j],]
    find<-c(STOCK$UP[(length(STOCK$UP)-(GenL-2)):length(STOCK$UP)],as.numeric(qSTOCK$sign==1))
    for (i in 1:(length(STOCK$UP)-(GenL-1))){
      if(sum(as.vector(STOCK$UP[i:(i+(GenL-1))])==find)==GenL){
        STOCK$match[i+(GenL-1)]<-1
      }
    }
    PctupGen<-c(PctupGen,mean(STOCK$lag[STOCK$match==1],na.rm = TRUE))
    GenDen<-c(GenDen,sum(STOCK$match))
  }

  PctGen<-as.data.frame(cbind(symbols,PctupGen,GenDen))
  names(PctGen)<-c("symb","Pctup","PctupDen")
  PctGen[,c("Pctup","PctupDen")]<-sapply(PctGen[,c("Pctup","PctupDen")],as.character)
  PctGen[,c("Pctup","PctupDen")]<-sapply(PctGen[,c("Pctup","PctupDen")],as.numeric)

  symbolsWatch<-symbols
  # symbolsWatch<-PctupDay$symb[PctupDay$Pctup>.7]
  WatchGen<-PctGen[PctGen$symb%in%symbolsWatch,]

  Tomorrow<-as.data.frame(matrix(data = NA, nrow=length(symbols),ncol = 4))
  names(Tomorrow)<-c("V1","Last","Change","sign")
  Tomorrow$V1<-symbols
  for (j in 1:length(symbols)){
    STOCK<-na.approx(get(symbols[j]))
    colnames(STOCK)<-c("open","high","low","close","volume","adjusted")
    Tomorrow$Change[j]<-STOCK$close-lag(STOCK$close)[nrow(STOCK)-bit+2]
    Tomorrow$Last[j]<-lag(STOCK$close)[nrow(STOCK)-bit+2]
  }
  rownames(Tomorrow)<-Tomorrow$V1
  Tomorrow$sign<-sign(Tomorrow$Change)
  TomorrowWatch<-Tomorrow[Tomorrow$V1 %in% symbolsWatch,]
  TomorrowWatch<-TomorrowWatch[order(TomorrowWatch$V1),]
  PrintGen<-cbind(merge(select(WatchGen,symb,Pctup,PctupDen),select(TomorrowWatch,V1,Last,Change,sign),by.x="symb",by.y = "V1"), date=row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  HistoryGen<-rbind(HistoryGen,PrintGen)
  print(row.names(as.data.frame(STOCK))[nrow(STOCK)-bit+1])
  print(sum(PrintGen$sign[PrintGen$sign==1])/length(PrintGen$sign))
}
(proc.time()-ptm)/60

GenAgg2<-HistoryGen%>%group_by(symb,Pctup>.64,PctupDen>45)%>%summarise(up=(n()+mean(sign)*n())/(2*n()),In=sum(Last),prof=sum(Change),return=prof/In*100,n=n())
symbolsGen<-as.character(GenAgg$symb[GenAgg$`Pctup > 0.64`==TRUE&GenAgg$`PctupDen > 45`==TRUE&GenAgg$up>.67&GenAgg$n>6])
table(HistoryGen$sign[HistoryGen$Pctup>.65&HistoryGen$PctupDen>6])

cl <- makeForkCluster(100)
registerDoParallel(cl)
getDoParWorkers()
?`doParallel-package`
