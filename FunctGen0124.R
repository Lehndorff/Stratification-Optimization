library(dplyr)
library(data.table)
# Functions
subsetsx<-function(data=Dataopt, size=StratVar, strata="Work"){
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
StrataOpt<-function(){
  StrataMax<-6
  Enduses<-unique(EndUseID)
  x<-proc.time()
  Tolerance<-ToleranceSet
  Tolerance2<-1-(Tolerance-1)
  Options<-matrix(data=NA, nrow=(Strata*length(Endusesn)), ncol = (StrataMax+8))
  colnames(Options)<-c("Enduse","Strata","Iter", "BestCV","S1","S2","S3","S4","S5","S6", "Max", "Tolerance","InfSamp","FinSamp")
  Result<-0
  loc<-0
  for (h in Endusesn){
    Measure<-Enduses[h]
    Dataopt <- Data[EndUseID==Measure ,c(ID,StratVar)]
    Dataopt$Percent <- Dataopt[,StratVar]/sum(Dataopt[,StratVar])
    Length<-length(Dataopt$Percent)
    Pos<-c(1:Length)
    Dataopt$Work<-Pos
    EstPoss<-1
    StrataSet<-Strata
    if (Measure=="LED" && Strata>4){
      StrataSet<-4
    }
    if(Measure=="OtherElectric" && Strata>4){
      StrataSet<-4
    }
    while (Dataopt$Percent[1]*StrataSet >= Tolerance){
      StrataSet<-StrataSet-1
    }
    for (n in 1:StrataSet){
      Dataopt$Work<-Pos
      Dataopt$Work[n:Length]<-n
      if (Measure=="LED" && Strata >= 4){
        ToleranceSet<-1.1
      }
      Tolerance<-ToleranceSet
      Tolerance2<-1-(Tolerance-1)
      if (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
        while (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
          Tolerance<-Tolerance+.01
          Tolerance2<-1-(Tolerance-1)
        }
      }
      if (n==2){
        if ((sum(Dataopt$Percent[1:3])<1/n*Tolerance2) && (sum(Dataopt$Percent[1:4])>1/n*Tolerance)){
          while ((sum(Dataopt$Percent[1:3])<1/n*Tolerance2) && (sum(Dataopt$Percent[1:4])>1/n*Tolerance)){
            Tolerance<-Tolerance+.01
            Tolerance2<-1-(Tolerance-1)
          }
        }
      }
      for (a in 1:(Length-n+1)){
        Dataopt$Work[a]<-1
        if (sum(Dataopt$Percent[Dataopt$Work==1])>(1/n*Tolerance)){
          break
        }
        if (sum(Dataopt$Percent[Dataopt$Work==1])<(1/n*Tolerance2)){
          next
        }
        if (n>2){
          Dataopt$Work[(a+2):Length]<-n
        }
        if (sum(Dataopt$Percent[Dataopt$Work==1])>(1/n*Tolerance2)){
          EstPoss<-EstPoss+1
        }
        if (sum(Dataopt$Work)==Length){
          break
        }
        for (b in (a+1):(Length-n+2)){
          if (n<=2){
            break
          }
          Dataopt$Work[b]<-2
          if (sum(Dataopt$Percent[Dataopt$Work==2])>(1/n*Tolerance)){
            break
          }
          if (sum(Dataopt$Percent[Dataopt$Work==2])<(1/n*Tolerance2)){
            next
          }
          if (n>3){
            Dataopt$Work[(b+2):Length]<-n
          }
          if (sum(Dataopt$Percent[Dataopt$Work==2])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance2)){
            EstPoss<-EstPoss+1
          }
          for (c in (b+1):(Length-n+3)){
            if (n<=3){
              break
            }
            Dataopt$Work[c]<-3
            if (sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance)){
              break
            }
            if (sum(Dataopt$Percent[Dataopt$Work==3])<(1/n*Tolerance2)){
              next
            }
            if (n>4){
              Dataopt$Work[(c+2):Length]<-n
            }
            if(sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance2)){
              EstPoss<-EstPoss+1
            }
            for(d in (c+1):(Length-n+4)){
              if(n<=4){
                break
              }
              Dataopt$Work[d]<-4
              if (sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance)){
                break
              }
              if (sum(Dataopt$Percent[Dataopt$Work==4])<(1/n*Tolerance2)){
                next
              }
              if (n>5){
                Dataopt$Work[(d+2):Length]<-n
              }
              if (sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance2)){
                EstPoss<-EstPoss+1
              }
              for(e in (d+1):(Length-n+5)){
                if(n<=5){
                  break
                }
                Dataopt$Work[e]<-5
                if (sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance)){
                  break
                }
                if (sum(Dataopt$Percent[Dataopt$Work==5])<(1/n*Tolerance2)){
                  next
                }
                if (n>6){
                  Dataopt$Work[(e+2):Length]<-n
                }
                if (sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==6])>(1/n*Tolerance2)){
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
      Dataopt$Work<-Pos
      Dataopt$Work[n:Length]<-n
      Tolerance<-ToleranceSet
      Tolerance2<-1-(Tolerance-1)
      if (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
        while (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
          Tolerance<-Tolerance+.01
          Tolerance2<-1-(Tolerance-1)
        }
      }
      if (n==2){
        if ((sum(Dataopt$Percent[1:3])<1/n*Tolerance2) && (sum(Dataopt$Percent[1:4])>1/n*Tolerance)){
          while ((sum(Dataopt$Percent[1:3])<1/n*Tolerance2) && (sum(Dataopt$Percent[1:4])>1/n*Tolerance)){
            Tolerance<-Tolerance+.01
            Tolerance2<-1-(Tolerance-1)
          }
        }
      }
      Options[loc,12]<-Tolerance
      for (a in 1:(Length-n+1)){
        Dataopt$Work[a]<-1
        if (sum(Dataopt$Percent[Dataopt$Work==1])>(1/n*Tolerance)){
          break
        }
        if (sum(Dataopt$Percent[Dataopt$Work==1])<(1/n*Tolerance2)){
          next
        }
        if (n>2){
          Dataopt$Work[(a+2):Length]<-n
        }
        if (sum(Dataopt$Percent[Dataopt$Work==1])>(1/n*Tolerance2)){
          New[Place,]<-Dataopt$Work
          Other[Place,2]<-subsetsx()
          Other[Place,1]<-Place
          Place<-Place+1
        }
        if (sum(Dataopt$Work)==Length){
          break
        }
        for (b in (a+1):(Length-n+2)){
          if (n<=2){
            break
          }
          Dataopt$Work[b]<-2
          if (sum(Dataopt$Percent[Dataopt$Work==2])>(1/n*Tolerance)){
            break
          }
          if (sum(Dataopt$Percent[Dataopt$Work==2])<(1/n*Tolerance2)){
            next
          }
          if (n>3){
            Dataopt$Work[(b+2):Length]<-n
          }
          if (sum(Dataopt$Percent[Dataopt$Work==2])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance2)){
            New[Place,]<-Dataopt$Work
            Other[Place,2]<-subsetsx()
            Other[Place,1]<-Place
            Place<-Place+1
          }
          for (c in (b+1):(Length-n+3)){
            if (n<=3){
              break
            }
            Dataopt$Work[c]<-3
            if (sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance)){
              break
            }
            if (sum(Dataopt$Percent[Dataopt$Work==3])<(1/n*Tolerance2)){
              next
            }
            if (n>4){
              Dataopt$Work[(c+2):Length]<-n
            }
            if(sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance2)){
              New[Place,]<-Dataopt$Work
              Other[Place,2]<-subsetsx()
              Other[Place,1]<-Place
              Place<-Place+1
            }
            for(d in (c+1):(Length-n+4)){
              if(n<=4){
                break
              }
              Dataopt$Work[d]<-4
              if (sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance)){
                break
              }
              if (sum(Dataopt$Percent[Dataopt$Work==4])<(1/n*Tolerance2)){
                next
              }
              if (n>5){
                Dataopt$Work[(d+2):Length]<-n
              }
              if (sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance2)){
                New[Place,]<-Dataopt$Work
                Other[Place,2]<-subsetsx()
                Other[Place,1]<-Place
                Place<-Place+1
              }
              for(e in (d+1):(Length-n+5)){
                if(n<=5){
                  break
                }
                Dataopt$Work[e]<-5
                if (sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance)){
                  break
                }
                if (sum(Dataopt$Percent[Dataopt$Work==5])<(1/n*Tolerance2)){
                  next
                }
                if (n>6){
                  Dataopt$Work[(e+2):Length]<-n
                }
                if (sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==6])>(1/n*Tolerance2)){
                  New[Place,]<-Dataopt$Work
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
  y<-proc.time()
  print(y-x)
}

# Define Data (e.g. read.csv("~/Desktop/SampleFrame_12062016.csv", stringsAsFactors=FALSE) )
Data <- read.csv("~/Desktop/SampleFrame_12062016.csv", stringsAsFactors=FALSE)

# Define end-use identifier (e.g. Data$PrimaryMeasure)
EndUseID<-Data$PrimaryMeasure

# Confirm end-uses
unique(EndUseID)

# Define quantitative variable to be stratified (e.g. "SumKWH")
StratVar<-"SumKWH"

# Define unique project identifier (e.g. "CProjectID")
ID<-"CProjectID"

# Optimization inputs; # of Strata up to 6 (e.g. 4), which end-uses (by unique(EnduseID) ordinal e.g. c(1:6,9)), 
# StratVar variation tolerance (e.g. ), intial critial value (e.g. 1.645) and initial percision (e.g. .1)
Strata<-4
Endusesn<-c(1:3,5)
ToleranceSet<-1.3
Critical<-1.645
Precision<-.1
###

StrataOpt()
