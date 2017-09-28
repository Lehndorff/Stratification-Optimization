library(dplyr)
library(data.table)
library(ggplot2)
library(devtools)
library(evergreen)
# Data <- read.csv("~/Desktop/SampleFrame_10172016.csv", stringsAsFactors=FALSE)
# Data <- read.csv("~/Desktop/SampleFrame_12062016.csv", stringsAsFactors=FALSE)
Data <- read.csv("~/Desktop/ALTSampleFrame_092617.csv", stringsAsFactors = FALSE)
StrataMax<-6
EndUseID<-Data$PrimaryMeasure
Enduses<-unique(EndUseID)
print(Enduses)
KWHEU<-c("CustomElectric","OtherKWH","T8","Motor","LED","OtherLighting","CompressedAirEquip")
MCFEU<-c("CustomGas","BoilersandBoilerControls","HVAC","OtherGas","SmartBuildings")
ID<-"C.Project.ID"
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
InfSamp<-function(CV){
  round((Critical*as.numeric(CV)/Precision)^2,0)
}
FinSamp<-function(InfSamp,Total){
  ceiling(as.numeric(InfSamp)/(1+as.numeric(InfSamp)/sum(as.numeric(Total))))
}

# Optimization inputs; # of Strata, which End Uses, Sum kWh variation tolerance, intial Critial Value and Percision
Strata<-5
StratVar<-"SumMCF"
Endusesn<-c(1,3:5)
MaxCert<-0
ToleranceSet<-1.3
minTolerance<-0
Critical<-1.645
Precision<-.15
Restrictions<-1
###

for (z in 1:1){
  # x<-proc.time()
  Certopts<-0:MaxCert
  if(minTolerance==1){
    ToleranceSet<-2
  }
  Tolerance<-ToleranceSet
  Tolerance2<-1-(Tolerance-1)
  ToleranceReset<-ToleranceSet
  Options<-matrix(data=NA, nrow=(Strata*length(Endusesn)), ncol = (StrataMax+8))
  CertOptions<-as.data.frame(matrix(data=NA, nrow=(Strata*length(Endusesn)), ncol = (StrataMax+8)))
  colnames(Options)<-c("Enduse","Strata","Iter", "BestCV","S1","S2","S3","S4","S5","S6", "Max", "Tolerance","InfSamp","FinSamp")
  colnames(CertOptions)<-c("Enduse","Strata","Iter", "BestCV","S1","S2","S3","S4","S5","S6", "Max", "Tolerance","InfSamp","FinSamp")
  Result<-0
  ResultCert<-0
  loc<-0
  EstPossVec <- rep(0,times = length(Enduses))
  minTol<-c(0,0)
  r<-proc.time()
  for (h in Endusesn){
    ToleranceSet<-ToleranceReset
    Tolerance<-ToleranceSet
    Tolerance2<-1-(Tolerance-1)
    Measure<-Enduses[h]
    Dataopt <- Data[EndUseID==Measure ,c(ID,StratVar)]
    Dataopt<-Dataopt[rev(order(Dataopt[StratVar])),]
    Dataopt$Percent <- Dataopt[,StratVar]/sum(Dataopt[,StratVar])
    Dataopt$Percentile<-cumsum(Dataopt$Percent)
    Length<-length(Dataopt$Percent)
    Pos<-c(1:Length)
    Dataopt$Work<-Pos
    EstPoss<-1
    StrataSet<-Strata
    while (Dataopt$Percent[1]*StrataSet >= Tolerance & minTolerance!=1){
      StrataSet<-StrataSet-1
    }
    for (n in 1:StrataSet){
      if(minTolerance==1){
        for (m in 1:n){
          Dataopt$minTol<-abs(Dataopt$Percentile-m/n)
          minTol[m]<-min(Dataopt$minTol)
        }
        ToleranceSet<-1+(min(.5*max(minTol),.0027)+max(minTol))*n
      }
      Dataopt$Work<-Pos
      Dataopt$Work[n:Length]<-n
      if (Length>500 && n == 4 && ToleranceSet>1.1 && Restrictions == 1){
        ToleranceSet<-1.08
      }
      if (Length>500 && n > 4 && ToleranceSet>1.02 && Restrictions == 1){
        ToleranceSet<-1.03
      }
      Tolerance<-ToleranceSet
      Tolerance2<-1-(Tolerance-1)
      if (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
        while (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
          Tolerance<-Tolerance+.01
          Tolerance2<-1-(Tolerance-1)
        }
      }
      if (n==2 && length(Dataopt$Work)>2){
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
          Dataopt$Work[(a+n-1):Length]<-n
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
            Dataopt$Work[(b+n-2):Length]<-n
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
              Dataopt$Work[(c+n-3):Length]<-n
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
                Dataopt$Work[(d+n-4):Length]<-n
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
                  Dataopt$Work[(e+n-5):Length]<-n
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
    EstPossVec[h]<-EstPoss
  }
  q<-proc.time()
  print("Est. Time (s) Remaining (High)", quote = FALSE)
  print(as.numeric((q-r)*2.5)[3])
  print("Est. Time (s) Remaining (Low)", quote = FALSE)
  print(as.numeric((q-r)*2)[3])
  
  f<-proc.time()
  for (h in Endusesn){
    ToleranceSet<-ToleranceReset
    Tolerance<-ToleranceSet
    Tolerance2<-1-(Tolerance-1)
    Measure<-Enduses[h]
    Dataopt <- Data[EndUseID==Measure ,c(ID,StratVar)]
    Dataopt<-Dataopt[rev(order(Dataopt[StratVar])),]
    Dataopt$Percent <- Dataopt[,StratVar]/sum(Dataopt[,StratVar])
    Dataopt$Percentile<-cumsum(Dataopt$Percent)
    Length<-length(Dataopt$Percent)
    Pos<-c(1:Length)
    Dataopt$Work<-Pos
    StrataSet<-Strata
    while (Dataopt$Percent[1]*StrataSet >= Tolerance & minTolerance!=1){
      StrataSet<-StrataSet-1
    }
    Place<-1
    New<-matrix(data=99, nrow = EstPossVec[h], ncol = (Length))
    Other<-matrix(data=99, nrow = EstPossVec[h], ncol = 2)
    for (n in 1:StrataSet){
      if(minTolerance==1){
        for (m in 1:n){
          Dataopt$minTol<-abs(Dataopt$Percentile-m/n)
          minTol[m]<-min(Dataopt$minTol)
        }
        ToleranceSet<-1+(min(.5*max(minTol),.0027)+max(minTol))*n
      }
      loc <- loc + 1
      Dataopt$Work<-Pos
      if (Length>500 && n == 4 && ToleranceSet>1.1 && Restrictions == 1){
        ToleranceSet<-1.08
      }
      if (Length>500 && n > 4 && ToleranceSet>1.02 && Restrictions == 1){
        ToleranceSet<-1.03
      }
      Dataopt$Work[n:Length]<-n
      Tolerance<-ToleranceSet
      Tolerance2<-1-(Tolerance-1)
      if (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
        while (sum(Dataopt$Percent[1:2])>1/n*Tolerance && Dataopt$Percent[1]<1/n*Tolerance2){
          Tolerance<-Tolerance+.01
          Tolerance2<-1-(Tolerance-1)
        }
      }
      if (n==2 && length(Dataopt$Work)>2){
        if ((sum(Dataopt$Percent[1:3])<1/n*Tolerance2) && (sum(Dataopt$Percent[1:4])>1/n*Tolerance)){
          while ((sum(Dataopt$Percent[1:3])<1/n*Tolerance2) && (sum(Dataopt$Percent[1:4])>1/n*Tolerance)){
            Tolerance<-Tolerance+.01
            Tolerance2<-1-(Tolerance-1)
          }
        }
      }
      Options[loc,12]<-Tolerance
      CertOptions[loc,12]<-Tolerance
      for (a in 1:(Length-n+1)){
        Dataopt$Work[a]<-1
        if (sum(Dataopt$Percent[Dataopt$Work==1])>(1/n*Tolerance)){
          break
        }
        if (sum(Dataopt$Percent[Dataopt$Work==1])<(1/n*Tolerance2)){
          next
        }
        if (n>2){
          Dataopt$Work[(a+n-1):Length]<-n
        }
        if (sum(Dataopt$Percent[Dataopt$Work==1])>(1/n*Tolerance2) && sum(New[Place-1,])!=sum(Dataopt$Work)){
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
            Dataopt$Work[(b+n-2):Length]<-n
          }
          if (sum(Dataopt$Percent[Dataopt$Work==2])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance2) && sum(New[Place-1,])!=sum(Dataopt$Work)){
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
              Dataopt$Work[(c+n-3):Length]<-n
            }
            if(sum(Dataopt$Percent[Dataopt$Work==3])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance2) && sum(New[Place-1,])!=sum(Dataopt$Work)){
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
                Dataopt$Work[(d+n-4):Length]<-n
              }
              if (sum(Dataopt$Percent[Dataopt$Work==4])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance2) && sum(New[Place-1,])!=sum(Dataopt$Work)){
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
                  Dataopt$Work[(e+n-5):Length]<-n
                }
                if (sum(Dataopt$Percent[Dataopt$Work==5])>(1/n*Tolerance2) && sum(Dataopt$Percent[Dataopt$Work==6])>(1/n*Tolerance2) && sum(New[Place-1,])!=sum(Dataopt$Work)){
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
      CertOptions[loc,11]<-Place-1
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
    CVs2<-as.data.frame(CVsCount)
    colnames(CVs2)[Length]<-"Last"
    colnames(CVs2)[Length+2]<-"CV"
    colnames(CVs2)[Length+3]<-"Count"
    for (i in 1: StrataSet){
      ResultCert<-ResultCert+1
      if (!is.infinite(min(CVs2$CV[!is.na(match(CVs2$Count,Certopts)) & CVs2$Last==i]))){
        CertOptions[ResultCert,2:10]<-CVs2[(CVs2$CV==min(CVs2$CV[!is.na(match(CVs2$Count,Certopts)) & CVs2$Last==i])),((Length):(Length+8))]
      }
      CertOptions[ResultCert,1]<-Measure
    }
  }
  g<-proc.time()
  k<-(g-f)
  print(k)
  for (i in 1:length(Options[,1])) {
    Options[i,13]<-InfSamp(CV = Options[i,4])
    CertOptions[i,13]<-InfSamp(CV = CertOptions[i,4])
    Options[i,14]<-FinSamp(InfSamp = Options[i,13],Total = Options[i,5:10])
    CertOptions[i,14]<-FinSamp(InfSamp = CertOptions[i,13],Total = CertOptions[i,5:10])
  }
  # y<-proc.time()
  # print(y-x)
  Options2<-rbind(CertOptions, Options)
  Options2<-Options2[order(Options2$Enduse,Options2$Strata),]
  Options2<-Options2[!is.na(Options2$Strata),]
  Options2<-cbind(Options2, "Selection"=(1:length(Options2$Enduse)))
}
View(Options2)

#Sample Design inputs; Select rows from Options, Tune Critical Value and Precison
Selection<-c(5,9,15,20)
Critical<-1.282
Precision<-.2
###

for (z in 1:1){
  Results<-cbind(Options2[Selection,1:14], matrix(data=NA, nrow = length(Selection),ncol = 1))
  colnames(Results)<-c(colnames(Results[,1:14]),"Sites/Strat")
  for (i in 1:length(Results[,1])) {
    Results[i,13]<-InfSamp(CV = Results[i,4])
    Results[i,14]<-FinSamp(InfSamp = Results[i,13],Total = Results[i,5:10])
  }
  Results[,15]<-as.numeric(Results[,14])/as.numeric(Results[,2])
  SumVect<-c()
  MeanVect<-c()
  CountVect<-c()
  StratVect<-c()
  EnduseVect<-c()
  SDVect<-c()
  for (x in Selection){
    Measure <- Options2[x,1]
    Dataopt <- Data[EndUseID==Measure ,c(ID,StratVar)]
    Dataopt<-Dataopt[rev(order(Dataopt[StratVar])),]
    Dataopt$Work <- c(rep(1, times = Options2[x,5]),rep(2, times = Options2[x,6]),rep(3, times = Options2[x,7]),rep(4, times = Options2[x,8]),rep(5, times = Options2[x,9]),rep(6, times = Options2[x,10]))
    StratVect<-c(StratVect,1:Options2[x,2])
    EnduseVect<-c(EnduseVect,rep(Options2[x,1],times=Options2[x,2]))
    CountVect<-c(CountVect,Options2[x,5:10])
    SDVectNew<-c()
    for (y in 1:StrataMax){
      SumVect<-c(SumVect,sum(Dataopt[Dataopt$Work==y,StratVar]))
      MeanVect<-c(MeanVect,mean(Dataopt[Dataopt$Work==y,StratVar]))
      SDVectNew<-c(SDVectNew, sd(Dataopt[Dataopt$Work==y,StratVar]))
      if (y==StrataMax && is.na(SDVectNew[1])==1){
        SDVectNew[1]<-0
      }
    }
    SDVect<-c(SDVect,SDVectNew)
  }
  SumVect<-SumVect[SumVect>0]
  MeanVect<-MeanVect[is.nan(MeanVect)==0]
  CountVect<-CountVect[CountVect>0]
  SDVect<-SDVect[is.na(SDVect)==0]
  Design<-cbind(EnduseVect,StratVect,CountVect,MeanVect,SumVect,SDVect,matrix(data=0, nrow = length(EnduseVect),ncol = 2))
  colnames(Design)<-c("Enduse","Stratum","Count","MeanKWH",StratVar,"SD","CV","SampTar")
  Design[,7]<-as.numeric(Design[,6])/as.numeric(Design[,4])
  row.names(Design)<-1:length(Design[,1])
  print.default("Total Sites:",quote = FALSE)
  print(TotalSites<-sum(as.numeric(Results[,14])))
  print.default("Sites per Strata:",quote = FALSE)
  print(SitesperStrat<-TotalSites/length(Design[,1]))
  for (y in 1:length(Results[,1])){
    while (as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
      for (x in 1:length(Design[,1])){
        if (Results[y,1]==Design[x,1]){
          if (as.numeric(Design[x,8])<as.numeric(Design[x,3]) && as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
            Design[x,8]<-(as.numeric(Design[x,8])+1)
          }
        }
      }
    }
  }
}
View(Results)

#Fine Tune/Select Sample Sizes
Results[,14]<-c(26,8,31,18,1)

for (z in 1:1){
  Design[,8]<-0
  for (y in 1:length(Results[,1])){
    while (as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
      for (x in 1:length(Design[,1])){
        if (Results[y,1]==Design[x,1]){
          if (as.numeric(Design[x,8])<as.numeric(Design[x,3]) && as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
            Design[x,8]<-(as.numeric(Design[x,8])+1)
          }
        }
      }
    }
  }
  Results[,15]<-as.numeric(Results[,14])/as.numeric(Results[,2])
  print.default("Total Sites:",quote = FALSE)
  print(TotalSites<-sum(as.numeric(Results[,14])))
  print.default("Sites per Strata:",quote = FALSE)
  print(SitesperStrat<-TotalSites/length(Design[,1]))
}

#Optimized Stratified Sample Design
View(Design)

#Fine Tune Design
# Design[2,8]<-3
# Design[4,8]<-2

for (z in 1:1){
  print.default("Total Sites:",quote = FALSE)
  print(TotalSites<-sum(as.numeric(Design[,8])))
  print.default("Sites per Strata:",quote = FALSE)
  print(SitesperStrat<-TotalSites/length(Design[,1]))
  FinalDesign<-Design[,c(1:5,8)]
}
View(FinalDesign)
ptm<-proc.time()
proc.time()-ptm


