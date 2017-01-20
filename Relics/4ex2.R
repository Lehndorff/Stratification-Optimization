library(dplyr)
library(data.table)
CE <- read.csv("~/Desktop/SampleFrame_10172016.csv", stringsAsFactors=FALSE)
CEopt <- CE[CE$PrimaryMeasure=="Motor",c(1,10,12,16)]
CEopt$Strata3Work <- 4
CEopt$Strata3Work[1]<-1
CEopt$Strata3Work[2]<-2
CEopt$Strata3Work[3]<-3
CEopt$Percent <- CEopt$SumKWH/sum(CEopt$SumKWH)
CEopt$Reset<-CEopt$Strata3Work
BestCV<-5
Count<-1
Strata <-4
system.time(
for(i in 2:(length(CEopt$SumKWH)-3)){
  if (sum(CEopt$Percent[CEopt$Strata3Work==1])>(1/Strata*1.5)){
    break
  }
  for(j in i:(length(CEopt$SumKWH)-2)){
    CEopt$Strata3Work[j]<-2
    if (sum(CEopt$Percent[CEopt$Strata3Work==2])>(1/Strata*1.5)){
      break
    }
    for (k in j:(length(CEopt$SumKWH)-2)) {
      CEopt$Strata3Work[k+1]<-3
      if (sum(CEopt$Percent[CEopt$Strata3Work==3])>(1/Strata*1.5)){
        break
      }
      if (between(sum(CEopt$Percent[CEopt$Strata3Work==4]),(1/Strata*.5),(1/Strata*1.5))==1){
        TotalCV <- ((((max(sd(CEopt$SumKWH[CEopt$Strata3Work==1]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==1]))*sum(CEopt$SumKWH[CEopt$Strata3Work==1]))+ ((max(sd(CEopt$SumKWH[CEopt$Strata3Work==2]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==2])*sum(CEopt$SumKWH[CEopt$Strata3Work==2]))+ ((max(sd(CEopt$SumKWH[CEopt$Strata3Work==3]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==3])*sum(CEopt$SumKWH[CEopt$Strata3Work==3]))+((max(sd(CEopt$SumKWH[CEopt$Strata3Work==4]), 0, na.rm=TRUE))/mean(CEopt$SumKWH[CEopt$Strata3Work==4])*sum(CEopt$SumKWH[CEopt$Strata3Work==4])))/sum(CEopt$SumKWH)
        if (TotalCV<BestCV){
          BestCV<-TotalCV
          CEopt$Best <- CEopt$Strata3Work
        }
      }
      Count<-Count+1
      #print(CEopt$Strata3Work)
    }
    CEopt$Strata3Work[(j+2):(length(CEopt$SumKWH))]<-4
  }
  CEopt$Strata3Work[i]<-1
})
CEopt$Strata3Work<-CEopt$Reset
BestCV<-5
Count<-1
CEopt$Strata3Work<-CEopt$Best

subsets<-function(data=CEopt, size="SumKWH", strata="Strata3Work"){
  str1<-data[[size]][data[[strata]]==1]
  str2<-data[[size]][data[[strata]]==2]
  str3<-data[[size]][data[[strata]]==3]
  str4<-data[[size]][data[[strata]]==4]
  
  SD1<-max(sd(str1), 0, na.rm=TRUE)
  SD2<-max(sd(str2), 0, na.rm=TRUE)
  SD3<-max(sd(str3), 0, na.rm=TRUE)
  SD4<-max(sd(str4), 0, na.rm=TRUE)
  
  TotalCV<-(
    (SD1/max(mean(str1), 1, na.rm = TRUE))*sum(str1)+ 
      (SD2/max(mean(str2), 1, na.rm = TRUE))*sum(str2)+ 
      (SD3/max(mean(str3), 1, na.rm = TRUE))*sum(str3)+
      (SD4/max(mean(str4), 1, na.rm = TRUE))*sum(str4)
  )/sum(data[[size]])
  return(TotalCV)
}
