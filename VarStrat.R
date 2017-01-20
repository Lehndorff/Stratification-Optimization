pos<-c(1:50)
Work<-pos
StrataMax<-7
Work<-rep(1, times=50)
for (i in 1:StrataMax){
  Strata<-i
  if (pos[i]>=Strata){
    Work[i:length(Work)]<-Strata
  }else{
    Work[i]<-pos[i]
  }
  print(Work)
}

for (n in 1:StrataMax){
  Work[n:length(Work)]<-n
  for (f in n:(length(Work)-Strata+n)){
    Work[f]<-n
    print(Work)
  }
}

for (n in 1:StrataMax){
  Work[n:length(Work)]<-n
  if ((Work[(length(Work)-n+1)])!=1){
    for (f in n:length(Work)){
      Work[f]<-(n-1)
      print(Work)
    }
  }
}

cumsum(CEopt$Percent)
A<-sum(cumsum(CEopt$Percent)<.25)
sum(cumsum(CEopt$Percent[(sum(cumsum(CEopt$Percent)<.25)):length(CEopt$Percent)])<.25)
B<-sum(cumsum(CEopt$Percent[A:length(CEopt$Percent)])<.25)
C<-sum(cumsum(CEopt$Percent[B:length(CEopt$Percent)])<.25)
sum(CEopt$Strata3Work==2)
