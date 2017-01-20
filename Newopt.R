library(dplyr)
CE <- read.csv("~/Desktop/SampleFrame.csv", stringsAsFactors=FALSE)
CEopt <- CE[CE$PrimaryMeasure=="OtherElectric",c(1,10,12,16)]
CEopt$Strata3Work<-1
BestCV<-5
Count<-1
n<-length(CEopt$Strata)
Strata<-4
Trials <- 102000
x<-c(rep(1, (Strata-1)), rep(0, n-4))
y<-cbind(1, t(unique(replicate(Trials, base::sample(x, replace=FALSE)))))
y2<-apply(y, 1, cumsum)
system.time(
  for (i in 1:Trials){
  CEopt$Strata3Work<-y2[,i]
  OK1 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==1])/sum(CEopt$SumKWH),.125,.375)
  OK2 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==2])/sum(CEopt$SumKWH),.125,.375)
  OK3 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==3])/sum(CEopt$SumKWH),.125,.375)
  OK4 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==4])/sum(CEopt$SumKWH),.125,.375)
  if (OK1+OK2+OK3+OK4>=4){
    TotalCV <- ((((max(sd(CEopt$SumKWH[CEopt$Strata3Work==1]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==1]))*sum(CEopt$SumKWH[CEopt$Strata3Work==1]))+ ((max(sd(CEopt$SumKWH[CEopt$Strata3Work==2]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==2])*sum(CEopt$SumKWH[CEopt$Strata3Work==2]))+ ((max(sd(CEopt$SumKWH[CEopt$Strata3Work==3]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==3])*sum(CEopt$SumKWH[CEopt$Strata3Work==3]))+((max(sd(CEopt$SumKWH[CEopt$Strata3Work==4]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==4])*sum(CEopt$SumKWH[CEopt$Strata3Work==4])))/sum(CEopt$SumKWH)
    if (TotalCV<BestCV){
      BestCV<-TotalCV
      CEopt$Best <- CEopt$Strata3Work
    }
  }
  Count<-Count+1
  })

