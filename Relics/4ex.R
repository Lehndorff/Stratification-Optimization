library(dplyr)
CE <- read.csv("~/Desktop/SampleFrame.csv", stringsAsFactors=FALSE)
CEopt <- CE[CE$PrimaryMeasure=="OtherElectric",c(1,10,12,16)]
CEopt$Strata3Work <- 4
CEopt$Strata3Work[1]<-1
CEopt$Strata3Work[2]<-2
CEopt$Strata3Work[3]<-3
CEopt$Reset<-CEopt$Strata3Work
BestCV<-5
Count<-1
for(i in 2:(length(CEopt$SumKWH)-3)){
  for(j in i:(length(CEopt$SumKWH)-2)){
    CEopt$Strata3Work[j]<-2
    for (k in j:(length(CEopt$SumKWH)-2)) {
      CEopt$Strata3Work[k+1]<-3
      SD1<-max(sd(CEopt$SumKWH[CEopt$Strata3Work==1]), 0, na.rm=TRUE)
      SD2<-max(sd(CEopt$SumKWH[CEopt$Strata3Work==2]), 0, na.rm=TRUE)
      SD3<-max(sd(CEopt$SumKWH[CEopt$Strata3Work==3]), 0, na.rm=TRUE)
      SD4<-max(sd(CEopt$SumKWH[CEopt$Strata3Work==4]), 0, na.rm=TRUE)
      Mean1<-mean(CEopt$SumKWH[CEopt$Strata3Work==1])
      Mean2<-mean(CEopt$SumKWH[CEopt$Strata3Work==2])
      Mean3<-mean(CEopt$SumKWH[CEopt$Strata3Work==3])
      Mean4<-mean(CEopt$SumKWH[CEopt$Strata3Work==4])
      Sum1<-sum(CEopt$SumKWH[CEopt$Strata3Work==1])
      Sum2<-sum(CEopt$SumKWH[CEopt$Strata3Work==2])
      Sum3<-sum(CEopt$SumKWH[CEopt$Strata3Work==3])
      Sum4<-sum(CEopt$SumKWH[CEopt$Strata3Work==4])
      TotalCV <- ((SD1/Mean1*Sum1)+(SD2/Mean2*Sum2)+(SD3/Mean3*Sum3)+(SD4/Mean4*Sum4))/sum(CEopt$SumKWH)
      if (TotalCV<BestCV){
        BestCV<-TotalCV
        CEopt$Best <- CEopt$Strata3Work
      }
      Count<-Count+1
      Print(Count)
    }
  CEopt$Strata3Work[(j+2):(length(CEopt$SumKWH))]<-4
  }
  CEopt$Strata3Work[i]<-1
  }
CEopt$Strata3Work<-CEopt$Reset
