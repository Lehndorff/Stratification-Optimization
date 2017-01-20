library(dplyr)
CE <- read.csv("~/Desktop/CE.csv", stringsAsFactors=FALSE)
View(CE)
CE$Strata2 <- CE$Strata
for (i in 1:49) {
  if(CE$Strata2[i]==99){
    CE$Strata2[i] <- 1
  }
}
if ((count(CE,CE$Strata2==1)[2,2])>1){
  SD1 <- sd(CE$SumKWH[CE$Strata2==1])
}else{
  SD1 <-0
}
TotalCV<- ((sd(CE$SumKWH[CE$Strata2==1])/mean(CE$SumKWH[CE$Strata2==1]))*sum(CE$SumKWH[CE$Strata2==1])+ (sd(CE$SumKWH[CE$Strata2==2])/mean(CE$SumKWH[CE$Strata2==2]))*sum(CE$SumKWH[CE$Strata2==2])+ (sd(CE$SumKWH[CE$Strata2==3])/mean(CE$SumKWH[CE$Strata2==3]))*sum(CE$SumKWH[CE$Strata2==3])+ (sd(CE$SumKWH[CE$Strata2==4])/mean(CE$SumKWH[CE$Strata2==4]))*sum(CE$SumKWH[CE$Strata2==4]))/sum(CE$SumKWH)
CV1 <- SD1/mean(CE$SumKWH[CE$Strata2==1])
for(i in 2:47){
  for(j in i:48){
    CEwork$WorkStrata[j]<-2
    print(CEwork$WorkStrata)
    if ((count(CEwork,CEwork$WorkStrata==1)[2,2])>1){
      SD1 <- sd(CEwork$SumKWH[CEwork$WorkStrata==1])
    }else{
      SD1 <- 0
    }
    if ((count(CEwork,CEwork$WorkStrata==2)[2,2])>1){
      SD2 <- sd(CEwork$SumKWH[CEwork$WorkStrata==2])
    }else{
      SD2 <- 0
    }
    if ((count(CEwork,CEwork$WorkStrata==3)[2,2])>1){
      SD3 <- sd(CEwork$SumKWH[CEwork$WorkStrata==3])
    }else{
      SD3 <- 0
    }
    TotalCV <- ((((SD1)/mean(CEwork$SumKWH[CEwork$WorkStrata==1]))*sum(CEwork$SumKWH[CEwork$WorkStrata==1]))+ ((SD2)/mean(CEwork$SumKWH[CEwork$WorkStrata==2])*sum(CEwork$SumKWH[CEwork$WorkStrata==2]))+ ((SD3)/mean(CEwork$SumKWH[CEwork$WorkStrata==3])*sum(CEwork$SumKWH[CEwork$WorkStrata==3])))/sum(CEwork$SumKWH)
    print(TotalCV)
    if (TotalCV<BestCV){
      BestCV<-TotalCV
      CEwork$Best2 <- CEwork$WorkStrata
    }
  }
  CEwork$WorkStrata[(i+2):48]<-3
  print(CEwork$WorkStrata)
  CEwork$WorkStrata[i]<-1
  print(CEwork$WorkStrata)
}
CEwork$WorkStrata<-CEwork$BestStrata

n<-317
x<-c(rep(1, 3), rep(0, n-4))
y<-cbind(1, t(unique(replicate(n*100, base::sample(x, replace=FALSE)))))
y2<-apply(y, 1, cumsum)

OK1 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==1])/sum(CEopt$SumKWH),.125,.375)
OK2 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==2])/sum(CEopt$SumKWH),.125,.375)
OK3 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==3])/sum(CEopt$SumKWH),.125,.375)
OK4 <- between(sum(CEopt$SumKWH[CEopt$Strata3Work==4])/sum(CEopt$SumKWH),.125,.375)

for (i in 1:length(CEopt$Percent)){
  A<-sum(CEopt$Percent[1:i])
  print(A)
  while (A<.25){
    Count<-Count+1
  }
}

while (A<.25){
  for (i in 1:length(CEopt$Percent)){
    A<-sum(CEopt$Percent[1:i])
    print(A)
    Count<-Count+1
  }
}
A<-0