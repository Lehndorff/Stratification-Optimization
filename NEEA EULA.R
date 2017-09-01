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

heattype<-heating%>%group_by(siteid)%>%summarise(bb=sum(ElecResistQuantity[HVACType=="baseboard"],na.rm=TRUE),faf=length(siteid[HVACType=="faf"]),pih=sum(ElecResistQuantity[HVACType=="pluginheater"],na.rm=TRUE),hp=length(siteid[HVACType=="heatpump"]),htst=length(siteid[HVACType=="htstove"]),fp=length(siteid[HVACType=="fireplace"]),
  hpdf=length(siteid[HVACType=="heatpumpdualfuel"]),boil=length(siteid[HVACType=="boiler"]),dhp=length(siteid[HVACType=="dhp"]),gshp=length(siteid[HVACType=="gshp"]))

cooltype<-cooling%>%group_by(siteid)%>%summarise(hp=length(siteid[HVACType=="heatpump"]),ws=sum(UnitACQuantity[HVACType=="windowshaker"],na.rm=TRUE),cac=length(siteid[HVACType=="centralAC"]),hpdf=length(siteid[HVACType=="heatpumpdualfuel"]),
  ptac=sum(UnitACQuantity[HVACType=="PTAC"],na.rm=TRUE),evap=length(siteid[HVACType=="evapcooler"]),gshp=length(siteid[HVACType=="gshp"]),dhp=length(siteid[HVACType=="dhp"]))

heatmerge<-left_join(heattype,left_join(state,weights,by="siteid"), by = "siteid")
coolmerge<-left_join(cooltype,left_join(state,weights,by="siteid"), by = "siteid")

heatingagg<-heatmerge%>%group_by(state,(heat_clim_zone==1))%>%summarise(bb=weighted.mean(bb,w = svy_wt),faf=weighted.mean(faf,w = svy_wt),pih=weighted.mean(pih,w = svy_wt),hp=weighted.mean(hp,w = svy_wt),htst=weighted.mean(htst,w = svy_wt),
  fp=weighted.mean(fp,w = svy_wt),hpdf=weighted.mean(hpdf,w = svy_wt),boil=weighted.mean(boil,w = svy_wt),dhp=weighted.mean(dhp,w = svy_wt),gshp=weighted.mean(gshp,w = svy_wt),n())
coolingagg<-coolmerge%>%group_by(state,(cool_clim_zone==1))%>%summarise(hp=weighted.mean(hp,w = svy_wt),ws=weighted.mean(ws,w = svy_wt),cac=weighted.mean(cac,w = svy_wt),hpdf=weighted.mean(hpdf,w = svy_wt),ptac=weighted.mean(ptac,w = svy_wt),
  evap=weighted.mean(evap,w = svy_wt),gshp=weighted.mean(gshp,w = svy_wt),dhp=weighted.mean(dhp,w = svy_wt),n=n())

heatingagg2<-heatmerge%>%group_by(state,(heat_clim_zone==1))%>%summarise(n=n(),bb=sum(bb),faf=sum(faf),pih=sum(pih),hp=sum(hp),htst=sum(htst),fp=sum(fp),hpdf=sum(hpdf),boil=sum(boil),dhp=sum(dhp),gshp=sum(gshp))
coolingagg2<-coolmerge%>%group_by(state,(cool_clim_zone==1))%>%summarise(n=n(),hp=sum(hp),ws=sum(ws),cac=sum(cac),hpdf=sum(hpdf),ptac=sum(ptac),evap=sum(evap),gshp=sum(gshp),dhp=sum(dhp))

heatweight<-svydesign(ids = ~1, data=merge,weights = merge$svy_wt)
heatweight<-wpct(merge,na.rm = TRUE)

heatweight2<-as.svrepdesign(design=heatweight)
