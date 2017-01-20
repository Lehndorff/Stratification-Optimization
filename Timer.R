library(dplyr)
library(data.table)
CE <- read.csv("~/Desktop/SampleFrame_10172016.csv", stringsAsFactors=FALSE)
CEopt <- CE[CE$PrimaryMeasure=="CustomElectric",c(1,10,12,16)]
CEopt$Percent <- CEopt$SumKWH/sum(CEopt$SumKWH)
Pos<-c(1:length(CEopt$Percent))
CEopt$Work<-Pos
Count<-1
Place<-1
StrataMax<-5
New<-matrix(data=99, nrow = 12500000, ncol = (length(CEopt$Work)))
system.time(
  for (n in 1:StrataMax){
    CEopt$Work<-Pos
    CEopt$Work[n:length(CEopt$Work)]<-n
    for (a in 1:(length(CEopt$Work)-n+1)){
      CEopt$Work[a]<-1
      if (n>2){
        CEopt$Work[(a+2):length(CEopt$Work)]<-n
      }
      if (n<3){
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
        if (n>3){
          CEopt$Work[(b+2):length(CEopt$Work)]<-n
        }
        if (n<4){
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
          if (n>4){
            CEopt$Work[(c+2):length(CEopt$Work)]<-n
          }
          if(n<5){
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
            if (n>5){
              CEopt$Work[(d+2):length(CEopt$Work)]<-n
            }
            if (n<6){
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
  })