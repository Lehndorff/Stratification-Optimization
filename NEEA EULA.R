# Try to do this all with packages: readr, haven, and dplyr. 
library("dplyr")
library("readr")
library("haven")
# Import NEEA RBSA data from CSV and SPSS datasets into R.
meter<-read_csv("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/RBSA Metering Data/Raw Data/RBSA_METER_DATA.csv")
geo<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFMaster_housegeometry.sav")
demog<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SF_ri_demog.sav")
HVAC<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/HVACcooling.sav")
heating<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/HVACheating.sav")
cooling<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/HVACcooling.sav")
weights<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFmaster_populations.sav")
state<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFmaster_location.sav")
heatmerge<-left_join(heating,left_join(state,weights,by="siteid"), by = "siteid")
coolmerge<-left_join(cooling,left_join(state,weights,by="siteid"), by = "siteid")

heatingagg<-heatmerge%>%group_by(state,(heat_clim_zone==1),HVACType)%>%summarise(n=sum(svy_wt))
coolingagg<-coolmerge%>%group_by(state,(cool_clim_zone==1),HVACType)%>%summarise(n=sum(svy_wt))

heatingagg2<-heatmerge%>%group_by(state,(heat_clim_zone==1))%>%summarise(n=sum(svy_wt))
coolingagg2<-coolmerge%>%group_by(state,(cool_clim_zone==1))%>%summarise(n=sum(svy_wt))

heatweight<-svydesign(ids = ~1, data=merge,weights = merge$svy_wt)
heatweight<-wpct(merge,na.rm = TRUE)

heatweight2<-as.svrepdesign(design=heatweight)
