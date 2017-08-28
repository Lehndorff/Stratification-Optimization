# Try to do this all with packages: readr, haven, and dplyr. 
library("dplyr")
library("readr")
library("haven")
# Import NEEA RBSA data from CSV and SPSS datasets into R.
meter<-read_csv("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/RBSA Metering Data/Raw Data/RBSA_METER_DATA.csv")
geo<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFMaster_housegeometry.sav")
demog<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SF_ri_demog.sav")
HVAC<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/HVACcooling.sav")
HVACprim<-HVAC[HVAC$HVACPrimaryCooling==1,]
HVACprim<-HVACprim[!duplicated(cbind(HVACprim$siteid,HVACprim$HVACPrimaryCooling)),]

# Merge all of the site characteristics files, one line per site in the final product
Merge<-left_join(geo,left_join(demog,HVACprim,by="siteid"), by = "siteid")

# Filter the table of sites to drop some specific building types, renters, etc.
Sites<-Merge[Merge$SFBuildingType=="Single Family, Detached"&Merge$ResInt_HomeOwnership=="Own/Buying"&Merge$SFFloors=="1",]
Sites<-Sites[!is.na(Sites$siteid),]

# Use that filtered site table to drop the same households from the metering data, retaining only those sites that met your criteria (without appending any variables)
meter_filtered<-meter[meter$siteid %in% Sites$siteid,]

# Merge the site sqft and primary cooling type with all the metering data
x<-Merge[,c("siteid","SummarySketchSqFt_Calculated","HVACType")]
x$siteid<-as.integer(x$siteid)
meter_merge<-left_join(meter,x,by = "siteid")

# Aggregate the metering data
meteragg<-meter_merge %>% group_by(siteid) %>% summarise(sqft=mean(SummarySketchSqFt_Calculated,na.rm=TRUE),PCT=unique(HVACType),AODT=mean(ODT,na.rm=TRUE),ACkWh=sum(AC,na.rm=TRUE))
meter_merge2<-meter_merge[(substr(meter_merge$time,3,5)=="JUL"|substr(meter_merge$time,3,5)=="AUG"),c("siteid","time","AC","ODT")]
meter_merge2$hour<-substr(meter_merge2$time,9,13)
meteragg2<-meter_merge2%>%group_by(hour)%>%summarise(ODT=mean(ODT,na.rm=TRUE),ACkWh=mean(AC,na.rm=TRUE))
