library(dplyr)
library(data.table)
library(ggplot2)
CE <- read.csv("~/Desktop/SampleFrame_12062016.csv", stringsAsFactors=FALSE)
StrataMax<-5
Enduses<-c("CustomElectric", "Motor", "CompressedAirEquip", "HVAC","T8","OtherLighting","OtherElectric","LED")
subsetsx<-function(data=CEopt, size="SumKWH", strata="Work"){
  str1<-data[[size]][data[[strata]]==1]
  str2<-data[[size]][data[[strata]]==2]
  str3<-data[[size]][data[[strata]]==3]
  str4<-data[[size]][data[[strata]]==4]
  str5<-data[[size]][data[[strata]]==5]
  str6<-data[[size]][data[[strata]]==6]
  
  SD1<-max(sd(str1), 0, na.rm=TRUE)
  SD2<-max(sd(str2), 0, na.rm=TRUE)
  SD3<-max(sd(str3), 0, na.rm=TRUE)
  SD4<-max(sd(str4), 0, na.rm=TRUE)
  SD5<-max(sd(str5), 0, na.rm=TRUE)
  SD6<-max(sd(str6), 0, na.rm=TRUE)
  
  TotalCV<-(
    (SD1/max(mean(str1), 1, na.rm = TRUE))*sum(str1)+ 
      (SD2/max(mean(str2), 1, na.rm = TRUE))*sum(str2)+ 
      (SD3/max(mean(str3), 1, na.rm = TRUE))*sum(str3)+
      (SD4/max(mean(str4), 1, na.rm = TRUE))*sum(str4)+
      (SD5/max(mean(str5), 1, na.rm = TRUE))*sum(str5)+
      (SD6/max(mean(str6), 1, na.rm = TRUE))*sum(str6)
  )/sum(data[[size]])
  return(TotalCV)
}
Strata<-2
Endusesn<-c(3)
Tolerance<-1.5
Results<-matrix(data=NA, nrow=(Strata*length(Endusesn)+1), ncol = (StrataMax+5))
Results[1,]<-c("Enduse","Strata","Iter", "BestCV","n1","n2","n3","n4","n5","Max")
Result<-1

system.time(
#for (h in 1:Endusesn){
for (h in Endusesn){
  Measure<-Enduses[h]
  CEopt <- CE[CE$PrimaryMeasure==Measure ,c(1,10)]
  CEopt$Percent <- CEopt$SumKWH/sum(CEopt$SumKWH)
  Pos<-c(1:length(CEopt$Percent))
  CEopt$Work<-Pos
  Legal<-1
    for (n in 1:Strata){
      CEopt$Work<-Pos
      CEopt$Work[n:length(CEopt$Work)]<-n
      for (a in 1:(length(CEopt$Work)-n+1)){
        CEopt$Work[a]<-1
        if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance)){
          break
        }
        if (n>2){
          CEopt$Work[(a+2):length(CEopt$Work)]<-n
        }
        if (n<=3){
          Legal<-Legal+1
        }
        if (sum(CEopt$Work)==length(CEopt$Work)){
          break
        }
        for (b in (a+1):(length(CEopt$Work)-n+2)){
          if (n<=2){
            break
          }
          CEopt$Work[b]<-2
          if (sum(CEopt$Percent[CEopt$Work==2])>(1/n*Tolerance)){
            break
          }
          if (n>3){
            CEopt$Work[(b+2):length(CEopt$Work)]<-n
          }
          if (n<=4){
            Legal<-Legal+1
          }
          for (c in (b+1):(length(CEopt$Work)-n+3)){
            if (n<=3){
              break
            }
            CEopt$Work[c]<-3
            if (sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance)){
              break
            }
            if (n>4){
              CEopt$Work[(c+2):length(CEopt$Work)]<-n
            }
            if(n<=5){
              Legal<-Legal+1
            }
            for(d in (c+1):(length(CEopt$Work)-n+4)){
              if(n<=4){
                break
              }
              CEopt$Work[d]<-4
              if (sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance)){
                break
              }
              if (n>5){
                CEopt$Work[(d+2):length(CEopt$Work)]<-n
              }
              if (n<=6){
                Legal<-Legal+1
              }
              for(e in (d+1):(length(CEopt$Work)-n+5)){
                if(n<=5){
                  break
                }
                CEopt$Work[e]<-5
                if (sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance)){
                  break
                }
                if (n>6){
                  CEopt$Work[(e+2):length(CEopt$Work)]<-n
                }
                if (n<=7){
                  Legal<-Legal+1
                }
              }
            }
          }
        }
      }
    }
  Count<-1
  Place<-1
  EstPoss<-Legal
  New<-matrix(data=99, nrow = EstPoss, ncol = (length(CEopt$Work)))
  Other<-matrix(data=99, nrow = EstPoss, ncol = 2)
    for (n in 1:Strata){
      CEopt$Work<-Pos
      CEopt$Work[n:length(CEopt$Work)]<-n
      for (a in 1:(length(CEopt$Work)-n+1)){
        CEopt$Work[a]<-1
        if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance)){
          break
        }
        if (n>2){
          CEopt$Work[(a+2):length(CEopt$Work)]<-n
        }
        if (n<=3){
          if((sum(New[Place-1,]==CEopt$Work))!=length(CEopt$Work)){
            New[Place,]<-CEopt$Work
            Other[Place,2]<-subsetsx()
            Other[Place,1]<-Place
            Place<-Place+1
          }
          Count<-Count+1
        }
        if (sum(CEopt$Work)==length(CEopt$Work)){
          break
        }
        for (b in (a+1):(length(CEopt$Work)-n+2)){
          if (n<=2){
            break
          }
          CEopt$Work[b]<-2
          if (sum(CEopt$Percent[CEopt$Work==2])>(1/n*Tolerance)){
            break
          }
          if (n>3){
            CEopt$Work[(b+2):length(CEopt$Work)]<-n
          }
          if (n<=4){
            if((sum(New[Place-1,]==CEopt$Work))!=length(CEopt$Work)){
              New[Place,]<-CEopt$Work
              Other[Place,2]<-subsetsx()
              Other[Place,1]<-Place
              Place<-Place+1
            }
            Count<-Count+1
          }
          for (c in (b+1):(length(CEopt$Work)-n+3)){
            if (n<=3){
              break
            }
            CEopt$Work[c]<-3
            if (sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance)){
              break
            }
            if (n>4){
              CEopt$Work[(c+2):length(CEopt$Work)]<-n
            }
            if(n<=5){
              if((sum(New[Place-1,]==CEopt$Work))!=length(CEopt$Work)){
                New[Place,]<-CEopt$Work
                Other[Place,2]<-subsetsx()
                Other[Place,1]<-Place
                Place<-Place+1
              }
              Count<-Count+1
            }
            for(d in (c+1):(length(CEopt$Work)-n+4)){
              if(n<=4){
                break
              }
              CEopt$Work[d]<-4
              if (sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance)){
                break
              }
              if (n>5){
                CEopt$Work[(d+2):length(CEopt$Work)]<-n
              }
              if (n<=6){
                if((sum(New[Place-1,]==CEopt$Work))!=length(CEopt$Work)){
                  New[Place,]<-CEopt$Work
                  Other[Place,2]<-subsetsx()
                  Other[Place,1]<-Place
                  Place<-Place+1
                }
                Count<-Count+1
              }
              for(e in (d+1):(length(CEopt$Work)-n+5)){
                if(n<=5){
                  break
                }
                CEopt$Work[e]<-5
                if (sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance)){
                  break
                }
                if (n>6){
                  CEopt$Work[(e+2):length(CEopt$Work)]<-n
                }
                if (n<=7){
                  if((sum(New[Place-1,]==CEopt$Work))!=length(CEopt$Work)){
                    New[Place,]<-CEopt$Work
                    Other[Place,2]<-subsetsx()
                    Other[Place,1]<-Place
                    Place<-Place+1
                  }
                  Count<-Count+1
                }
              }
            }
          }
        }
      }
    }
  CVs<-cbind(New,Other)
  CVs<-(CVs[CVs[,1]!=99,])
  Counts<-matrix(data=NA,nrow=length(New[,1]),ncol=StrataMax)
  for (i in 1:(length(New[,1]))){
    Counts[i,1]<-sum(New[i,(New[i,]==1)])
    Counts[i,2]<-sum(New[i,(New[i,]==2)])/2
    Counts[i,3]<-sum(New[i,(New[i,]==3)])/3
    Counts[i,4]<-sum(New[i,(New[i,]==4)])/4
    Counts[i,5]<-sum(New[i,(New[i,]==5)])/5
    #Counts[i,6]<-sum(New[i,(New[i,]==6)])/6
  }
  CVsCount<-cbind(New,Other,Counts)
  for (i in 1:Strata){
    Result=Result+1
    Results[Result,2:9]<-(CVsCount[(CVsCount[,(length(CEopt$Work)+2)]==(min(CVsCount[(CVsCount[,(length(CEopt$Work))]==i),(length(CEopt$Work)+2)],na.rm = TRUE))),((length(CEopt$Work)):(length(CEopt$Work)+7))])
    Results[Result,1]<-Measure
    Results[Result,10]<-max(CVsCount[CVsCount[,length(CEopt$Work)]==i,(length(CEopt$Work)+1)]) 
  }
})
