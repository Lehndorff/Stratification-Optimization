library(dplyr)
CE <- read.csv("~/Desktop/OtherLighting.csv", stringsAsFactors=FALSE)
CEopt <- CE[,c(1,10,12,16)]

# Set strata to something more interesting than the default
CEopt$Strata3Work<-CEopt$Strata

# Define functions for each calculation approach that looks promising
original.cv<-function(data=CEopt, size="SumKWH", strata="Strata3Work"){
TotalCV<-((((max(sd(data[[size]][data[[strata]]==1]), 0, na.rm=TRUE))/max(mean(data[[size]][data[[strata]]==1]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==1]))+ 
          (((max(sd(data[[size]][data[[strata]]==2]), 0, na.rm=TRUE))/max(mean(data[[size]][data[[strata]]==2]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==2]))+ 
          (((max(sd(data[[size]][data[[strata]]==3]), 0, na.rm=TRUE))/max(mean(data[[size]][data[[strata]]==3]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==3]))+
          (((max(sd(data[[size]][data[[strata]]==4]), 0, na.rm=TRUE))/max(mean(data[[size]][data[[strata]]==4]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==4]))
        )/sum(data[[size]])
  return(TotalCV)
}

stip.paren<-function(data=CEopt, size="SumKWH", strata="Strata3Work"){
  TotalCV<-(
              (max(sd(data[[size]][data[[strata]]==1]), 0, na.rm=TRUE)/max(mean(data[[size]][data[[strata]]==1]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==1])+ 
              (max(sd(data[[size]][data[[strata]]==2]), 0, na.rm=TRUE)/max(mean(data[[size]][data[[strata]]==2]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==2])+ 
              (max(sd(data[[size]][data[[strata]]==3]), 0, na.rm=TRUE)/max(mean(data[[size]][data[[strata]]==3]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==3])+
              (max(sd(data[[size]][data[[strata]]==4]), 0, na.rm=TRUE)/max(mean(data[[size]][data[[strata]]==4]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==4])
  )/sum(data[[size]])
  return(TotalCV)
}

split.up<-function(data=CEopt, size="SumKWH", strata="Strata3Work"){
  SD1<-max(sd(data[[size]][data[[strata]]==1]), 0, na.rm=TRUE)
  SD2<-max(sd(data[[size]][data[[strata]]==2]), 0, na.rm=TRUE)
  SD3<-max(sd(data[[size]][data[[strata]]==3]), 0, na.rm=TRUE)
  SD4<-max(sd(data[[size]][data[[strata]]==4]), 0, na.rm=TRUE)
  
  TotalCV<-(
      (SD1/max(mean(data[[size]][data[[strata]]==1]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==1])+ 
      (SD2/max(mean(data[[size]][data[[strata]]==2]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==2])+ 
      (SD3/max(mean(data[[size]][data[[strata]]==3]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==3])+
      (SD4/max(mean(data[[size]][data[[strata]]==4]), 1, na.rm = TRUE))*sum(data[[size]][data[[strata]]==4])
  )/sum(data[[size]])
  return(TotalCV)
}

aggregate.str<-function(size=CEopt$SumKWH, strata=CEopt$Strata3Work){
  data<-data.frame(size, strata)
  
  descriptives<-
    data %>% group_by(strata) %>% summarize(
      sum=sum(size),
      str=(max(sd(size), 0, na.rm = TRUE)/max(mean(size), 1, na.rm = TRUE))*sum(size)
  )

  TotalCV<-sum(descriptives$str)/sum(descriptives$sum)
  return(TotalCV)
}

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

library(compiler)
comp.subsets<-cmpfun(subsets)

alt.subsets<-function(data=CEopt, size="SumKWH", strata="Strata3Work"){
  str1<-data[[size]][data[[strata]]==1]
  str2<-data[[size]][data[[strata]]==2]
  str3<-data[[size]][data[[strata]]==3]
  str4<-data[[size]][data[[strata]]==4]
  
  TotalCV<-(
      (max(sd(str1), 0, na.rm=TRUE)/max(mean(str1), 1, na.rm = TRUE))*sum(str1)+ 
      (max(sd(str2), 0, na.rm=TRUE)/max(mean(str2), 1, na.rm = TRUE))*sum(str2)+ 
      (max(sd(str3), 0, na.rm=TRUE)/max(mean(str3), 1, na.rm = TRUE))*sum(str3)+
      (max(sd(str4), 0, na.rm=TRUE)/max(mean(str4), 1, na.rm = TRUE))*sum(str4)
    )/sum(data[[size]])
  return(TotalCV)
}


# Make sure all of the functions give the same result
c(original.cv(), stip.paren(), split.up(), aggregate.str(), subsets(), comp.subsets(), alt.subsets())

# Compare their speed with 100 trial runs
install.packages('microbenchmark')
library(microbenchmark)
compare <- microbenchmark(original=original.cv(), 
                          fewer.parentheses=stip.paren(), 
                          split.into.steps=split.up(),
                          agg.calc=aggregate.str(),
                          subset.first=subsets(),
                          cmp.subset.first=comp.subsets(),
                          alt.subset.first=alt.subsets(),
                          times = 100)
library(ggplot2)
autoplot(compare)
compare
