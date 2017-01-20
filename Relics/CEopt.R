library(dplyr)
CE <- read.csv("~/Desktop/HVAC.csv", stringsAsFactors=FALSE)
CEopt <- CE[,c(1,10,16)]
CEopt$Strata3Work <- 3
CEopt$Strata3Work[1]<-1
CEopt$Strata3Work[2]<-2
CEopt$Reset<-CEopt$Strata3Work
BestCV<-5
for(i in 2:(length(CEopt$SumKWH)-1)){
  for(j in i:(length(CEopt$SumKWH)-1)){
    CEopt$Strata3Work[j]<-2
    print(CEopt$Strata3Work)
    if ((count(CEopt,CEopt$Strata3Work==1)[2,2])>1){
      SD1 <- sd(CEopt$SumKWH[CEopt$Strata3Work==1])
    }else{
      SD1 <- 0
    }
    if ((count(CEopt,CEopt$Strata3Work==2)[2,2])>1){
      SD2 <- sd(CEopt$SumKWH[CEopt$Strata3Work==2])
    }else{
      SD2 <- 0
    }
    if ((count(CEopt,CEopt$Strata3Work==3)[2,2])>1){
      SD3 <- sd(CEopt$SumKWH[CEopt$Strata3Work==3])
    }else{
      SD3 <- 0
    }
    TotalCV <- ((((SD1)/mean(CEopt$SumKWH[CEopt$Strata3Work==1]))*sum(CEopt$SumKWH[CEopt$Strata3Work==1]))+ ((SD2)/mean(CEopt$SumKWH[CEopt$Strata3Work==2])*sum(CEopt$SumKWH[CEopt$Strata3Work==2]))+ ((SD3)/mean(CEopt$SumKWH[CEopt$Strata3Work==3])*sum(CEopt$SumKWH[CEopt$Strata3Work==3])))/sum(CEopt$SumKWH)
    print(TotalCV)
    if (TotalCV<BestCV){
      BestCV<-TotalCV
      CEopt$Best <- CEopt$Strata3Work
    }
  }
  CEopt$Strata3Work[(i+2):(length(CEopt$SumKWH)-1)]<-3
  print(CEopt$Strata3Work)
  CEopt$Strata3Work[i]<-1
  print(CEopt$Strata3Work)
}
CEopt$Strata3Work<-CEopt$Reset
