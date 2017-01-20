library(dplyr)
library(data.table)
library(ggplot2)
library(plot3D)
CE <- read.csv("~/Desktop/SampleFrame_10172016.csv", stringsAsFactors=FALSE)
CEopt <- CE[CE$PrimaryMeasure=="Motor",c(1,10,12,16)]
#Results<-matrix(data = NA,nrow = (StrataMax+1),ncol=8)
for (r in 1:StrataMax){
  Results[1,1]<-"Results"
  Results[(r+1),1]<-paste("CV",r)
}
CEopt$Percent <- CEopt$SumKWH/sum(CEopt$SumKWH)
Pos<-c(1:length(CEopt$Percent))
CEopt$Work<-Pos
Count<-1
Place<-1
StrataMax<-5
New<-matrix(data=99, nrow = 170000, ncol = (length(CEopt$Work)))
Other<-matrix(data=99, nrow=170000, ncol=2)
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
system.time(
  for (n in 1:StrataMax){
    CEopt$Work<-Pos
    CEopt$Work[n:length(CEopt$Work)]<-n
    for (a in 1:(length(CEopt$Work)-n+1)){
      CEopt$Work[a]<-1
      if (sum(CEopt$Percent[CEopt$Work==1])>(1/n*1.5)){
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
        #print(CEopt$Work[70:106])
      }
      if (sum(CEopt$Work)==length(CEopt$Work)){
        break
      }
      for (b in (a+1):(length(CEopt$Work)-n+2)){
        if (n<=2){
          break
        }
        CEopt$Work[b]<-2
        if (sum(CEopt$Percent[CEopt$Work==2])>(1/n*1.5)){
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
          #print(CEopt$Work[70:106])
        }
        for (c in (b+1):(length(CEopt$Work)-n+3)){
          if (n<=3){
            break
          }
          CEopt$Work[c]<-3
          if (sum(CEopt$Percent[CEopt$Work==3])>(1/n*1.5)){
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
            #print(CEopt$Work[70:106])
          }
          for(d in (c+1):(length(CEopt$Work)-n+4)){
            if(n<=4){
              break
            }
            CEopt$Work[d]<-4
            if (sum(CEopt$Percent[CEopt$Work==4])>(1/n*1.5)){
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
              #print(CEopt$Work[70:106])
            }
            for(e in (d+1):(length(CEopt$Work)-n+5)){
              if(n<=5){
                break
              }
              CEopt$Work[e]<-5
              if (sum(CEopt$Percent[CEopt$Work==5])>(1/n*1.5)){
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
                #print(CEopt$Work[70:106])
              }
            }
          }
        }
      }
    }
  })
system.time(CVs<-cbind(New,Other))
system.time(CVs<-CVs[CVs[,1]!=99,])
system.time(CVsCount<-cbind(CVs, matrix(data=NA,nrow=length(CVs[,1]),ncol=StrataMax)))
system.time(for (i in 1:StrataMax){
  Results[1,3]<-"Motor"
  Results[i+1,3]<-min(CVsCount[(CVsCount[,(length(CEopt$Work))]==i),(length(CEopt$Work)+2)],na.rm = TRUE)
})
system.time(
  for (i in 1:(length(CVsCount[,length(CEopt$Work)]))){
    CVsCount[i,(length(CEopt$Work)+3)]<-sum(New[i,(New[i,]==1)])
    CVsCount[i,(length(CEopt$Work)+4)]<-sum(New[i,(New[i,]==2)])/2
    CVsCount[i,(length(CEopt$Work)+5)]<-sum(New[i,(New[i,]==3)])/3
    CVsCount[i,(length(CEopt$Work)+6)]<-sum(New[i,(New[i,]==4)])/4
    CVsCount[i,(length(CEopt$Work)+7)]<-sum(New[i,(New[i,]==5)])/5
  })
Graph<-as.data.frame(CVsCount[,length(CEopt$Work):(length(CEopt$Work)+7)])
qplot(x=Graph[,2],y=Graph[,3],color=(Graph[,3]<.4))
qplot(x=Graph[,2],y=Graph[,8], color=(Graph[,3]<.4))
qplot(x=Graph[(Graph[,1]==4),2],y=Graph[(Graph[,1]==4),7],color=(Graph[(Graph[,1]==4),3]<.3699))
qplot(x=Graph[,2],y=Graph[,3],color=(between(Graph[,8],80,120)))

print(CVsCount[(CVsCount[,(length(CVsCount)+2)==(Results[i+1,(Measure==Results[1,])])]),((length(CEopt$Work)+3):(length(CEopt$Work)+8))])
