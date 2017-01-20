library(dplyr)
library(data.table)
library(ggplot2)
CE <- read.csv("~/Desktop/SampleFrame_12062016.csv", stringsAsFactors=FALSE)
StrataMax<-6
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
Strata<-5
Endusesn<-c(1:6)
times <- 10
Impro<-matrix(data=NA, nrow = 30, ncol = times*2)
for (f in 1:times){
  ToleranceSet<-1+f*.05
  Flex<-1
  Tolerance<-ToleranceSet
  Tolerance2<-1-(Tolerance-1)
  Results<-matrix(data=NA, nrow=(Strata*length(Endusesn)), ncol = (StrataMax+6))
  colnames(Results)<-c("Enduse","Strata","Iter", "BestCV","S1","S2","S3","S4","S5","S6", "Max", "Tolerance")
  Result<-0
  loc<-0
  # h<-2
  system.time(
    for (h in Endusesn){
      Measure<-Enduses[h]
      CEopt <- CE[CE$PrimaryMeasure==Measure ,c(1,10)]
      # CEopt <- CEopt[1:25,]
      CEopt$Percent <- CEopt$SumKWH/sum(CEopt$SumKWH)
      Pos<-c(1:length(CEopt$Percent))
      CEopt$Work<-Pos
      Legal<-1
      StrataSet<-Strata
      while (CEopt$Percent[1]*StrataSet >= Tolerance){
        StrataSet<-StrataSet-1
      }
      for (n in 1:StrataSet){
        CEopt$Work<-Pos
        CEopt$Work[n:length(CEopt$Work)]<-n
        Tolerance<-ToleranceSet
        Tolerance2<-1-(Tolerance-1)
        if (Flex*sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
          while (sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
            Tolerance<-Tolerance+.01
            Tolerance2<-1-(Tolerance-1)
          }
        }
        if (Flex*n==2){
          if ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
            while ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
              Tolerance<-Tolerance+.01
              Tolerance2<-1-(Tolerance-1)
            }
          }
        }
        for (a in 1:(length(CEopt$Work)-n+1)){
          CEopt$Work[a]<-1
          if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance)){
            break
          }
          if (sum(CEopt$Percent[CEopt$Work==1])<(1/n*Tolerance2)){
            next
          }
          if (n>2){
            CEopt$Work[(a+2):length(CEopt$Work)]<-n
          }
          if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance2)){
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
            if (sum(CEopt$Percent[CEopt$Work==2])<(1/n*Tolerance2)){
              next
            }
            if (n>3){
              CEopt$Work[(b+2):length(CEopt$Work)]<-n
            }
            if (sum(CEopt$Percent[CEopt$Work==2])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2)){
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
              if (sum(CEopt$Percent[CEopt$Work==3])<(1/n*Tolerance2)){
                next
              }
              if (n>4){
                CEopt$Work[(c+2):length(CEopt$Work)]<-n
              }
              if(sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2)){
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
                if (sum(CEopt$Percent[CEopt$Work==4])<(1/n*Tolerance2)){
                  next
                }
                if (n>5){
                  CEopt$Work[(d+2):length(CEopt$Work)]<-n
                }
                if (sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2)){
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
                  if (sum(CEopt$Percent[CEopt$Work==5])<(1/n*Tolerance2)){
                    next
                  }
                  if (n>6){
                    CEopt$Work[(e+2):length(CEopt$Work)]<-n
                  }
                  if (sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==6])>(1/n*Tolerance2)){
                    Legal<-Legal+1
                  }
                }
              }
            }
          }
        }
        # print(Legal-1)
      }
      Count<-1
      Place<-1
      EstPoss<-Legal
      New<-matrix(data=99, nrow = EstPoss, ncol = (length(CEopt$Work)))
      Other<-matrix(data=99, nrow = EstPoss, ncol = 2)
      for (n in 1:StrataSet){
        loc <- loc + 1
        CEopt$Work<-Pos
        CEopt$Work[n:length(CEopt$Work)]<-n
        Tolerance<-ToleranceSet
        Tolerance2<-1-(Tolerance-1)
        if (Flex*sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
          while (sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
            Tolerance<-Tolerance+.01
            Tolerance2<-1-(Tolerance-1)
          }
        }
        if (Flex*n==2){
          if ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
            while ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
              Tolerance<-Tolerance+.01
              Tolerance2<-1-(Tolerance-1)
            }
          }
        }
        Results[loc,12]<-Tolerance
        for (a in 1:(length(CEopt$Work)-n+1)){
          CEopt$Work[a]<-1
          if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance)){
            break
          }
          if (sum(CEopt$Percent[CEopt$Work==1])<(1/n*Tolerance2)){
            next
          }
          if (n>2){
            CEopt$Work[(a+2):length(CEopt$Work)]<-n
          }
          if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance2)){
            New[Place,]<-CEopt$Work
            Other[Place,2]<-subsetsx()
            Other[Place,1]<-Place
            Place<-Place+1
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
            if (sum(CEopt$Percent[CEopt$Work==2])<(1/n*Tolerance2)){
              next
            }
            if (n>3){
              CEopt$Work[(b+2):length(CEopt$Work)]<-n
            }
            if (sum(CEopt$Percent[CEopt$Work==2])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2)){
              New[Place,]<-CEopt$Work
              Other[Place,2]<-subsetsx()
              Other[Place,1]<-Place
              Place<-Place+1
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
              if (sum(CEopt$Percent[CEopt$Work==3])<(1/n*Tolerance2)){
                next
              }
              if (n>4){
                CEopt$Work[(c+2):length(CEopt$Work)]<-n
              }
              if(sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2)){
                New[Place,]<-CEopt$Work
                Other[Place,2]<-subsetsx()
                Other[Place,1]<-Place
                Place<-Place+1
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
                if (sum(CEopt$Percent[CEopt$Work==4])<(1/n*Tolerance2)){
                  next
                }
                if (n>5){
                  CEopt$Work[(d+2):length(CEopt$Work)]<-n
                }
                if (sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2)){
                  New[Place,]<-CEopt$Work
                  Other[Place,2]<-subsetsx()
                  Other[Place,1]<-Place
                  Place<-Place+1
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
                  if (sum(CEopt$Percent[CEopt$Work==5])<(1/n*Tolerance2)){
                    next
                  }
                  if (n>6){
                    CEopt$Work[(e+2):length(CEopt$Work)]<-n
                  }
                  if (sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==6])>(1/n*Tolerance2)){
                    New[Place,]<-CEopt$Work
                    Other[Place,2]<-subsetsx()
                    Other[Place,1]<-Place
                    Place<-Place+1
                    Count<-Count+1
                  }
                }
              }
            }
          }
        }
        Results[loc,11]<-Place-1
      }
      CVs<-cbind(New,Other)
      CVs<-(CVs[CVs[,1]!=99,])
      Counts<-matrix(data=NA,nrow=length(New[,1]),ncol=StrataMax)
      # for (i in 1:(length(New[,1]))){
      #   Counts[i,1]<-sum(New[i,(New[i,]==1)])
      #   Counts[i,2]<-sum(New[i,(New[i,]==2)])/2
      #   Counts[i,3]<-sum(New[i,(New[i,]==3)])/3
      #   Counts[i,4]<-sum(New[i,(New[i,]==4)])/4
      #   Counts[i,5]<-sum(New[i,(New[i,]==5)])/5
      #   Counts[i,6]<-sum(New[i,(New[i,]==6)])/6
      # }
      CVsCount<-cbind(New,Other,Counts)
      for (i in 1:StrataSet){
        Result=Result+1
        Results[Result,2:10]<-(CVsCount[(CVsCount[,(length(CEopt$Work)+2)]==(min(CVsCount[(CVsCount[,(length(CEopt$Work))]==i),(length(CEopt$Work)+2)],na.rm = TRUE))),((length(CEopt$Work)):(length(CEopt$Work)+8))])
        Results[Result,1]<-Measure
        #Results[Result,11]<-max(CVsCount[CVsCount[,length(CEopt$Work)]==i,(length(CEopt$Work)+1)])
      }
    })
  Impro[,2*f]<-Results[,4]
  Impro[,2*f-1]<-Results[,12]
}

