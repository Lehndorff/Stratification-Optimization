library(dplyr)
library(data.table)
library(ggplot2)
CE <- read.csv("~/Desktop/SampleFrame_10172016.csv", stringsAsFactors=FALSE)
CEopt <- CE[CE$PrimaryMeasure=="Motor",c(1,10,12,16)]
#Results<-matrix(data = NA,nrow = (StrataMax+1),ncol=8)
CEopt$Percent <- CEopt$SumKWH/sum(CEopt$SumKWH)
Pos<-c(1:length(CEopt$Percent))
CEopt$Work<-Pos
Count<-1
Place<-1
StrataMax<-5
TotalCV<-6
ZBestCV1<-5
ZBestCV2<-5
ZBestCV3<-5
ZBestCV4<-5
ZBestCV5<-5
New<-matrix(data=99, nrow = 170000, ncol = (length(CEopt$Work)))
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
system.time(New2<-New[(New[,length(CEopt$Work)]!=99),])
system.time(New3<-cbind(New2,matrix(data=(1:length(New2[,1])), nrow=length(New2[,1]),ncol = (StrataMax+4))))
New3[,(length(CEopt$Work)+1)]<-NA
system.time(
  for (i in 1:(length(New2[,length(CEopt$Work)]))){
    #New3[i,(length(CEopt$Work)+1)]<-((((max(sd(CEopt$SumKWH[New2[i,]==1]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==1]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==1]))+
    #(((max(sd(CEopt$SumKWH[New2[i,]==2]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==2]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==2]))+
    #(((max(sd(CEopt$SumKWH[New2[i,]==3]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==3]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==3]))+
    #(((max(sd(CEopt$SumKWH[New2[i,]==4]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==4]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==4]))+
    #(((max(sd(CEopt$SumKWH[New2[i,]==5]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==5]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==5])))/sum(CEopt$SumKWH)
    New3[i,(length(CEopt$Work)+2)]<-sum(New2[i,(New2[i,]==1)])
    New3[i,(length(CEopt$Work)+3)]<-sum(New2[i,(New2[i,]==2)])/2
    New3[i,(length(CEopt$Work)+4)]<-sum(New2[i,(New2[i,]==3)])/3
    New3[i,(length(CEopt$Work)+5)]<-sum(New2[i,(New2[i,]==4)])/4
    New3[i,(length(CEopt$Work)+6)]<-sum(New2[i,(New2[i,]==5)])/5
  })
New4<-as.data.frame(New3)
New4 <-New4 %>% group_by(V106) %>% mutate(max(V108))
system.time(
  for (k in 1:length(New2[,1])){
    if (New4[k,(length(CEopt$Work)+2)]==New4[k,(length(CEopt$Work)+10)]){
      New4[k,(length(CEopt$Work)+1)]<-((((max(sd(CEopt$SumKWH[New2[k,]==1]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==1]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==1]))+
                                         (((max(sd(CEopt$SumKWH[New2[k,]==2]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==2]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==2]))+
                                         (((max(sd(CEopt$SumKWH[New2[k,]==3]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==3]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==3]))+
                                         (((max(sd(CEopt$SumKWH[New2[k,]==4]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==4]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==4]))+
                                         (((max(sd(CEopt$SumKWH[New2[k,]==5]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==5]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==5])))/sum(CEopt$SumKWH)
    }
  })


Graph<-New4[,length(CEopt$Work):(length(CEopt$Work)+(StrataMax+2))]
Graph2<-as.data.frame(Graph)
qplot(x=Graph2[,3],y=Graph2[,2],color=Graph2[,1])


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