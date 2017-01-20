library(dplyr)
library(data.table)
library(ggplot2)
# CE <- read.csv("~/Desktop/SampleFrame_10172016.csv", stringsAsFactors=FALSE)
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

# Optimization inputs; # of Strata, which End Uses, Sum kWh variation tolerance, intial Critial Value and Percision
Strata<-5
Endusesn<-c(7:8)
Critical<-1.284
Precision<-.2
times <- 10
Impro<-matrix(data=NA, nrow = Strata*length(Endusesn), ncol = times*2)
Clock<-vector(mode = "numeric",length = (times*2))
###
A<-as.numeric(proc.time())
End<-c()
for (f in 1:times){
  ToleranceSet<-1+f*.05
  Tolerance<-ToleranceSet
  Tolerance2<-1-(Tolerance-1)
  Options<-matrix(data=NA, nrow=(Strata*length(Endusesn)), ncol = (StrataMax+8))
  colnames(Options)<-c("Enduse","Strata","Iter", "BestCV","S1","S2","S3","S4","S5","S6", "Max", "Tolerance","InfSamp","FinSamp")
  Result<-0
  loc<-0
  for (h in Endusesn){
    Measure<-Enduses[h]
    if(f == 1){
      End<-c(End, rep(Measure, times = Strata))
    }
    CEopt <- CE[CE$PrimaryMeasure==Measure ,c("CProjectID","SumKWH")]
    CEopt$Percent <- CEopt$SumKWH/sum(CEopt$SumKWH)
    Length<-length(CEopt$Percent)
    Pos<-c(1:Length)
    CEopt$Work<-Pos
    EstPoss<-1
    StrataSet<-Strata
    if (Measure=="LED" && Strata>4){
      StrataSet<-4
    }
    while (CEopt$Percent[1]*StrataSet >= Tolerance){
      StrataSet<-StrataSet-1
    }
    for (n in 1:StrataSet){
      CEopt$Work<-Pos
      CEopt$Work[n:Length]<-n
      if (Measure=="LED" && Strata >= 4){
        ToleranceSet<-1.1
      }
      Tolerance<-ToleranceSet
      Tolerance2<-1-(Tolerance-1)
      if (sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
        while (sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
          Tolerance<-Tolerance+.01
          Tolerance2<-1-(Tolerance-1)
        }
      }
      if (n==2){
        if ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
          while ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
            Tolerance<-Tolerance+.01
            Tolerance2<-1-(Tolerance-1)
          }
        }
      }
      for (a in 1:(Length-n+1)){
        CEopt$Work[a]<-1
        if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance)){
          break
        }
        if (sum(CEopt$Percent[CEopt$Work==1])<(1/n*Tolerance2)){
          next
        }
        if (n>2){
          CEopt$Work[(a+2):Length]<-n
        }
        if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance2)){
          EstPoss<-EstPoss+1
        }
        if (sum(CEopt$Work)==Length){
          break
        }
        for (b in (a+1):(Length-n+2)){
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
            CEopt$Work[(b+2):Length]<-n
          }
          if (sum(CEopt$Percent[CEopt$Work==2])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2)){
            EstPoss<-EstPoss+1
          }
          for (c in (b+1):(Length-n+3)){
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
              CEopt$Work[(c+2):Length]<-n
            }
            if(sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2)){
              EstPoss<-EstPoss+1
            }
            for(d in (c+1):(Length-n+4)){
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
                CEopt$Work[(d+2):Length]<-n
              }
              if (sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2)){
                EstPoss<-EstPoss+1
              }
              for(e in (d+1):(Length-n+5)){
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
                  CEopt$Work[(e+2):Length]<-n
                }
                if (sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==6])>(1/n*Tolerance2)){
                  EstPoss<-EstPoss+1
                }
              }
            }
          }
        }
      }
    }
    Place<-1
    New<-matrix(data=99, nrow = EstPoss, ncol = (Length))
    Other<-matrix(data=99, nrow = EstPoss, ncol = 2)
    for (n in 1:StrataSet){
      loc <- loc + 1
      CEopt$Work<-Pos
      CEopt$Work[n:Length]<-n
      Tolerance<-ToleranceSet
      Tolerance2<-1-(Tolerance-1)
      if (sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
        while (sum(CEopt$Percent[1:2])>1/n*Tolerance && CEopt$Percent[1]<1/n*Tolerance2){
          Tolerance<-Tolerance+.01
          Tolerance2<-1-(Tolerance-1)
        }
      }
      if (n==2){
        if ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
          while ((sum(CEopt$Percent[1:3])<1/n*Tolerance2) && (sum(CEopt$Percent[1:4])>1/n*Tolerance)){
            Tolerance<-Tolerance+.01
            Tolerance2<-1-(Tolerance-1)
          }
        }
      }
      Options[loc,12]<-Tolerance
      for (a in 1:(Length-n+1)){
        CEopt$Work[a]<-1
        if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance)){
          break
        }
        if (sum(CEopt$Percent[CEopt$Work==1])<(1/n*Tolerance2)){
          next
        }
        if (n>2){
          CEopt$Work[(a+2):Length]<-n
        }
        if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*Tolerance2)){
          New[Place,]<-CEopt$Work
          Other[Place,2]<-subsetsx()
          Other[Place,1]<-Place
          Place<-Place+1
        }
        if (sum(CEopt$Work)==Length){
          break
        }
        for (b in (a+1):(Length-n+2)){
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
            CEopt$Work[(b+2):Length]<-n
          }
          if (sum(CEopt$Percent[CEopt$Work==2])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2)){
            New[Place,]<-CEopt$Work
            Other[Place,2]<-subsetsx()
            Other[Place,1]<-Place
            Place<-Place+1
          }
          for (c in (b+1):(Length-n+3)){
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
              CEopt$Work[(c+2):Length]<-n
            }
            if(sum(CEopt$Percent[CEopt$Work==3])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2)){
              New[Place,]<-CEopt$Work
              Other[Place,2]<-subsetsx()
              Other[Place,1]<-Place
              Place<-Place+1
            }
            for(d in (c+1):(Length-n+4)){
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
                CEopt$Work[(d+2):Length]<-n
              }
              if (sum(CEopt$Percent[CEopt$Work==4])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2)){
                New[Place,]<-CEopt$Work
                Other[Place,2]<-subsetsx()
                Other[Place,1]<-Place
                Place<-Place+1
              }
              for(e in (d+1):(Length-n+5)){
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
                  CEopt$Work[(e+2):Length]<-n
                }
                if (sum(CEopt$Percent[CEopt$Work==5])>(1/n*Tolerance2) && sum(CEopt$Percent[CEopt$Work==6])>(1/n*Tolerance2)){
                  New[Place,]<-CEopt$Work
                  Other[Place,2]<-subsetsx()
                  Other[Place,1]<-Place
                  Place<-Place+1
                }
              }
            }
          }
        }
      }
      Options[loc,11]<-Place-1
    }
    Counts<-matrix(data=NA,nrow=length(New[,1]),ncol=StrataMax)
    for (i in 1:(length(New[,1]))){
      for (j in 1:StrataMax){
        Counts[i,j]<-sum(New[i,(New[i,]==j)])/j
      }
    }
    CVsCount<-cbind(New,Other,Counts)
    for (i in 1:StrataSet){
      Result=Result+1
      Options[Result,2:10]<-(CVsCount[(CVsCount[,(Length+2)]==(min(CVsCount[(CVsCount[,(Length)]==i),(Length+2)],na.rm = TRUE))),((Length):(Length+8))])
      Options[Result,1]<-Measure
    }
  }
  for (i in 1:length(Options[,1])) {
    Options[i,13]<-round((((Critical*as.numeric(Options[i,4]))/Precision)^2),0)
    Options[i,14]<-ceiling(as.numeric(Options[i,13])/(1+as.numeric(Options[i,13])/sum(as.numeric(Options[i,5:10]))))
  }
  Impro[,2*f]<-Options[,14]
  Impro[,2*f-1]<-Options[,4]
  B<-as.numeric(proc.time())
  Clock[2*f]<-(B[3]-A[3])
  print(f)
}
Clock<-c(0,Clock)
ELAP<-2*c(1:times)
for (c in ELAP){
  Clock[c]<-Clock[c+1]-Clock[c-1]
  Clock[c-1]<-NA
}
Clock<-Clock[2:length(Clock)]
Impro<-rbind(Impro,Clock)
End<-c(End, NA)
Impro<-cbind(End, Impro)
View(Impro)
# write.csv(Impro,file = "~/Desktop/Impro2.csv",quote = TRUE)
