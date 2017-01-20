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
#system.time(New2<-unique(New,by = rows,na.rm=TRUE))
system.time(New2<-New[(New[,length(CEopt$Work)]!=99),])
system.time(New3<-cbind(New2,matrix(data=(1:length(New2[,1])), nrow=length(New2[,1]),ncol = (StrataMax+4))))
New4<-as.data.frame(New3)
system.time(
for (i in 1:(length(New2[,length(CEopt$Work)]))){
  New3[i,(length(CEopt$Work)+1)]<-((((max(sd(CEopt$SumKWH[New2[i,]==1]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==1]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==1]))+
                                     (((max(sd(CEopt$SumKWH[New2[i,]==2]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==2]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==2]))+
                                     (((max(sd(CEopt$SumKWH[New2[i,]==3]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==3]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==3]))+
                                     (((max(sd(CEopt$SumKWH[New2[i,]==4]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==4]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==4]))+
                                     (((max(sd(CEopt$SumKWH[New2[i,]==5]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[i,]==5]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[i,]==5])))/sum(CEopt$SumKWH)
  New3[i,(length(CEopt$Work)+2)]<-sum(New2[i,(New2[i,]==1)])
  New3[i,(length(CEopt$Work)+3)]<-sum(New2[i,(New2[i,]==2)])/2
  New3[i,(length(CEopt$Work)+4)]<-sum(New2[i,(New2[i,]==3)])/3
  New3[i,(length(CEopt$Work)+5)]<-sum(New2[i,(New2[i,]==4)])/4
  New3[i,(length(CEopt$Work)+6)]<-sum(New2[i,(New2[i,]==5)])/5
})
#Results<-matrix(data=NA, nrow=(StrataMax+1),ncol=8)
for (i in 1:StrataMax){
  Results[1,6]<-"OtherLighting"
  Results[i+1,6]<-min(New3[(New3[,(length(CEopt$Work))]==i),(length(CEopt$Work)+1)],na.rm = TRUE)
}
for (r in 1:StrataMax){
  Results[1,1]<-"Results"
  Results[(r+1),1]<-paste("CV",r)
}
Graph<-New3[,length(CEopt$Work):(length(CEopt$Work)+(StrataMax+2))]
plot.default(Graph[,3],Graph[,2])
Graph2<-as.data.frame(Graph)

(sum(New[Count-1,]==CEopt$Work))!=length(CEopt$Work)

count(New2,New2[,1]==1)
sum(New2[,(New2[2,]==1)])
qplot(x=Graph2[,3],y=Graph2[,2],color=Graph2[,1])
qplot(x=Graph2[2:15352,4],y=Graph2[2:15352,2],color=Graph2[2:15352,1])
qplot(x=Graph2[,8],y=Graph2[,2],color=Graph2[,7]==50)
75.967+2.514+1.593+408.554
New3[,(length(CEopt$Work)+1)]<-NA
system.time(
for (f in 1:StrataMax){
  for (k in 1:length(New2[,1])){
    if (New3[k,(length(CEopt$Work))]!=f){
      break
    }
    if (New3[k,(length(CEopt$Work)+2)]>((f-1)/f*max(New3[(New3[,length(CEopt$Work)]==f),length(CEopt$Work)]))){
      New3[k,(length(CEopt$Work)+1)]<-((((max(sd(CEopt$SumKWH[New2[k,]==1]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==1]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==1]))+
                                          (((max(sd(CEopt$SumKWH[New2[k,]==2]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==2]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==2]))+
                                         (((max(sd(CEopt$SumKWH[New2[k,]==3]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==3]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==3]))+
                                         (((max(sd(CEopt$SumKWH[New2[k,]==4]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==4]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==4]))+
                                          (((max(sd(CEopt$SumKWH[New2[k,]==5]), 0, na.rm=TRUE))/max(mean(CEopt$SumKWH[New2[k,]==5]), 1, na.rm = TRUE))*sum(CEopt$SumKWH[New2[k,]==5])))/sum(CEopt$SumKWH)
    }
  }
})
for (k in 1:length(New2[,1])){
  for (f in 1:StrataMax){
    if (New3[k,(length(CEopt$Work)+2)]==f){
      if(New3[k,(length(CEopt$Work)+StrataMax+2)]>((f-1)/f)*max(New3[(New3[,length(CEopt$Work)]==f),(length(CEopt$Work)+StrataMax+2)])){
        print(New3[k,113])
      }
    }
  }
}
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
system.time(
for (i in 1:(length(New2[,length(CEopt$Work)]))){
  New3[i,(length(CEopt$Work)+1)]<-subsets()
})