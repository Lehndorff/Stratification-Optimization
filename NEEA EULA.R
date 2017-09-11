# Try to do this all with packages: readr, haven, and dplyr. 
library("dplyr")
library("readr")
library("haven")
library("evergreen")
# Import NEEA RBSA data from CSV and SPSS datasets into R.
# meter<-read_csv("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/RBSA Metering Data/Raw Data/RBSA_METER_DATA.csv")
meterinfo<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/NEEA.RBSA.Metering.Data.Clean.UNIQUEID.HVAConly.sav")
read.csv.folder("/volumes/Projects/Common Data Files/NEEA RBSA/Exported to CSV",remove = "*.csv$")
geo<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFMaster_housegeometry.sav")
demog<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SF_ri_demog.sav")
HVAC<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/HVACcooling.sav")
heating<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/HVACheating.sav")
cooling<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/HVACcooling.sav")
weights<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFmaster_populations.sav")
state<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFmaster_location.sav")
wh<-read_spss("/Volumes/Projects/419006 - SCE AMI Billing Regression/Data/NEEA RBSA/Analysis/SPSS Data/SFwheater.sav")

# data for Mfr. homes
# heating<-MH_HVACheating
# cooling<-MH_HVACcooling
# state<-MHMaster_location
# weights<-MHMaster_populations
# wh<-MHwheater
# cooling$HVACType[cooling$HVACType=="centralac"]<-"centralAC"
# cooling$HVACType[cooling$HVACType=="ptac"]<-"PTAC"
# names(state)[names(state)=="US_state"]<-"state"

# aggregate to site level by; 1) total count of technology at site, and 2) whether or not the technology is present at the site
whtype<-wh%>%group_by(siteid)%>%summarise(hp=length(siteid[WHHeatPump=="True"]))
heattype<-heating%>%group_by(siteid)%>%summarise(bb=sum(ElecResistQuantity[HVACType=="baseboard"],na.rm=TRUE),faf=length(siteid[HVACType=="faf"]),pih=sum(ElecResistQuantity[HVACType=="pluginheater"],na.rm=TRUE),hp=length(siteid[HVACType=="heatpump"]),htst=length(siteid[HVACType=="htstove"]),fp=length(siteid[HVACType=="fireplace"]),
  hpdf=length(siteid[HVACType=="heatpumpdualfuel"]),boil=length(siteid[HVACType=="boiler"]),dhp=length(siteid[HVACType=="dhp"]),gshp=length(siteid[HVACType=="gshp"]))
heattype2<-heating%>%group_by(siteid)%>%summarise(bb=as.numeric(sum(ElecResistQuantity[HVACType=="baseboard"],na.rm=TRUE)>0),faf=as.numeric(length(siteid[HVACType=="faf"])>0),pih=as.numeric(sum(ElecResistQuantity[HVACType=="pluginheater"],na.rm=TRUE)>0),hp=as.numeric(length(siteid[HVACType=="heatpump"])>0),htst=as.numeric(length(siteid[HVACType=="htstove"])>0),fp=as.numeric(length(siteid[HVACType=="fireplace"])>0),
  hpdf=as.numeric(length(siteid[HVACType=="heatpumpdualfuel"])>0),boil=as.numeric(length(siteid[HVACType=="boiler"])>0),dhp=as.numeric(length(siteid[HVACType=="dhp"])>0),gshp=as.numeric(length(siteid[HVACType=="gshp"])>0))
cooltype<-cooling%>%group_by(siteid)%>%summarise(hp=length(siteid[HVACType=="heatpump"]),ws=sum(UnitACQuantity[HVACType=="windowshaker"],na.rm=TRUE),cac=length(siteid[HVACType=="centralAC"]),hpdf=length(siteid[HVACType=="heatpumpdualfuel"]),
  ptac=sum(UnitACQuantity[HVACType=="PTAC"],na.rm=TRUE),evap=length(siteid[HVACType=="evapcooler"]),gshp=length(siteid[HVACType=="gshp"]),dhp=length(siteid[HVACType=="dhp"]))
cooltype2<-cooling%>%group_by(siteid)%>%summarise(hp=as.numeric(length(siteid[HVACType=="heatpump"])>0),ws=as.numeric(sum(UnitACQuantity[HVACType=="windowshaker"],na.rm=TRUE)>0),cac=as.numeric(length(siteid[HVACType=="centralAC"])>0),hpdf=as.numeric(length(siteid[HVACType=="heatpumpdualfuel"])>0),
  ptac=as.numeric(sum(UnitACQuantity[HVACType=="PTAC"],na.rm=TRUE)>0),evap=as.numeric(length(siteid[HVACType=="evapcooler"])>0),gshp=as.numeric(length(siteid[HVACType=="gshp"])>0),dhp=as.numeric(length(siteid[HVACType=="dhp"])>0))

# merges for saturations as a percentage of homes with 1 or more heating/cooling equipment
# heatmerge<-left_join(heattype,left_join(state,weights,by="siteid"), by = "siteid")
# heatmerge2<-left_join(heattype2,left_join(state,weights,by="siteid"), by = "siteid")
# coolmerge<-left_join(cooltype,left_join(state,weights,by="siteid"), by = "siteid")
# coolmerge2<-left_join(cooltype2,left_join(state,weights,by="siteid"), by = "siteid")
# whmerge<-left_join(whtype,left_join(state,weights,by="siteid"), by = "siteid")
# metermerge<-left_join(meterinfoall,left_join(state,weights,by="siteid"),by = "siteid")

# merges for saturations as a percentage of all homes
heatmerge<-left_join(left_join(state,weights,by="siteid"),heattype, by = "siteid")
heatmerge[is.na(heatmerge)]<-0
heatmerge2<-left_join(left_join(state,weights,by="siteid"),heattype2, by = "siteid")
heatmerge2[is.na(heatmerge2)]<-0
coolmerge<-left_join(left_join(state,weights,by="siteid"),cooltype, by = "siteid")
coolmerge[is.na(coolmerge)]<-0
coolmerge2<-left_join(left_join(state,weights,by="siteid"),cooltype2, by = "siteid")
coolmerge2[is.na(coolmerge2)]<-0
whmerge<-left_join(left_join(state,weights,by="siteid"), whtype,by = "siteid")
whmerge[is.na(whmerge)]<-0
metermerge<-left_join(left_join(state,weights,by="siteid"), meterinfo,by = "siteid")
metermerge[is.na(metermerge)]<-0

# state and climate zone level aggregation for: 1) total count, and 2) presence
heatingagg<-heatmerge%>%group_by(state,(heat_clim_zone==1))%>%summarise(bb=weighted.mean(bb,w = svy_wt),faf=weighted.mean(faf,w = svy_wt),pih=weighted.mean(pih,w = svy_wt),hp=weighted.mean(hp,w = svy_wt),
  htst=weighted.mean(htst,w = svy_wt),fp=weighted.mean(fp,w = svy_wt),hpdf=weighted.mean(hpdf,w = svy_wt),boil=weighted.mean(boil,w = svy_wt),dhp=weighted.mean(dhp,w = svy_wt),gshp=weighted.mean(gshp,w = svy_wt),n())
coolingagg<-coolmerge%>%group_by(state,(cool_clim_zone==1))%>%summarise(hp=weighted.mean(hp,w = svy_wt),ws=weighted.mean(ws,w = svy_wt),cac=weighted.mean(cac,w = svy_wt),hpdf=weighted.mean(hpdf,w = svy_wt),
  ptac=weighted.mean(ptac,w = svy_wt),evap=weighted.mean(evap,w = svy_wt),gshp=weighted.mean(gshp,w = svy_wt),dhp=weighted.mean(dhp,w = svy_wt),n=n())
heatingagg2<-heatmerge2%>%group_by(state,(heat_clim_zone==1))%>%summarise(bb=weighted.mean(bb,w = svy_wt),faf=weighted.mean(faf,w = svy_wt),pih=weighted.mean(pih,w = svy_wt),hp=weighted.mean(hp,w = svy_wt),
  htst=weighted.mean(htst,w = svy_wt),fp=weighted.mean(fp,w = svy_wt),hpdf=weighted.mean(hpdf,w = svy_wt),boil=weighted.mean(boil,w = svy_wt),dhp=weighted.mean(dhp,w = svy_wt),gshp=weighted.mean(gshp,w = svy_wt),n())
coolingagg2<-coolmerge2%>%group_by(state,(cool_clim_zone==1))%>%summarise(hp=weighted.mean(hp,w = svy_wt),ws=weighted.mean(ws,w = svy_wt),cac=weighted.mean(cac,w = svy_wt),hpdf=weighted.mean(hpdf,w = svy_wt),
  ptac=weighted.mean(ptac,w = svy_wt),evap=weighted.mean(evap,w = svy_wt),gshp=weighted.mean(gshp,w = svy_wt),dhp=weighted.mean(dhp,w = svy_wt),n=n())
whagg<-whmerge%>%group_by(state,(heat_clim_zone==1))%>%summarise(heatpump=weighted.mean(hp,w = svy_wt),n=n(),hpc=sum(hp))

# counts (numerator) of technology
heatingagg3<-heatmerge%>%group_by(state,(heat_clim_zone==1))%>%summarise(n=n(),bb=sum(bb),faf=sum(faf),pih=sum(pih),hp=sum(hp),htst=sum(htst),fp=sum(fp),hpdf=sum(hpdf),boil=sum(boil),dhp=sum(dhp),gshp=sum(gshp))
coolingagg3<-coolmerge%>%group_by(state,(cool_clim_zone==1))%>%summarise(n=n(),hp=sum(hp),ws=sum(ws),cac=sum(cac),hpdf=sum(hpdf),ptac=sum(ptac),evap=sum(evap),gshp=sum(gshp),dhp=sum(dhp))
heatingagg4<-heatmerge2%>%group_by(state,(heat_clim_zone==1))%>%summarise(n=n(),bb=sum(bb),faf=sum(faf),pih=sum(pih),hp=sum(hp),htst=sum(htst),fp=sum(fp),hpdf=sum(hpdf),boil=sum(boil),dhp=sum(dhp),gshp=sum(gshp))
coolingagg4<-coolmerge2%>%group_by(state,(cool_clim_zone==1))%>%summarise(n=n(),hp=sum(hp),ws=sum(ws),cac=sum(cac),hpdf=sum(hpdf),ptac=sum(ptac),evap=sum(evap),gshp=sum(gshp),dhp=sum(dhp))

# counts of metered units
meteringoagg<-metermerge%>%group_by(state)%>%summarise(hpwh=sum(hWhHPWH))
meterinfoagg<-metermerge%>%group_by(state)%>%summarise(hp=sum(kWhHPflag),dhp=sum(kWhDHPflag),ac=sum(kWhACflag),bb=sum(kWhERflag),faf=sum(ElecFurnaceflag),gshp=sum(kWhGSHPflag), n=n())
meterinfoagg2<-metermerge%>%group_by(heat_clim_zone,cool_clim_zone)%>%summarise(hp=sum(kWhHPflag),dhp=sum(kWhDHPflag),ac=sum(kWhACflag),bb=sum(kWhERflag),faf=sum(ElecFurnaceflag),gshp=sum(kWhGSHPflag))
meteraggcount<-meterinfo%>%group_by(siteid)%>%mutate(count=sum(kWhHPflag,kWhDHPflag,kWhACflag,kWhERflag,ElecFurnaceflag,na.rm=TRUE))

# write.csv(heatingagg,"~/desktop/HA.csv")
# write.csv(heatingagg2,"~/desktop/HA2.csv")
# write.csv(heatingagg3,"~/desktop/HA3.csv")
# write.csv(coolingagg,"~/desktop/CA.csv")
# write.csv(coolingagg2,"~/desktop/CA2.csv")
# write.csv(coolingagg3,"~/desktop/CA3.csv")

# for multifamily, one equipment type at a time
MFunit_master$idunit<-paste(MFunit_master$siteid,MFunit_master$unit_it,sep = "-")
equip<-MFunit_wheater
names(equip)[6]<-"count"
equip$count<-1
equip$idunit<-paste(equip$siteid,equip$unit_it,sep = "-")
equipsite<-equip%>%group_by(idunit)%>%summarise(count=sum(count,na.rm=TRUE))
equipsite$yes<-as.numeric(equipsite$count>0)

equipjoin<-left_join(MFunit_master,equipsite,by="idunit")
equipjoin$count[is.na(equipjoin$count)]<-0
equipjoin$yes[is.na(equipjoin$yes)]<-0
equipagg<-equipjoin%>%group_by(Site_State)%>%summarise(mcount=weighted.mean(count,w=Site_pWeight),scount=sum(count),house=weighted.mean(yes,w=Site_pWeight),shouse=sum(yes))

write.csv(equipagg,"~/desktop/EA.csv")

# for cross frequencies of single family technology types
heatcool<-full_join(heattype2,cooltype2,by="siteid")
heatcool[is.na(heatcool)]<-0
heatcool$bb[heatcool$bb==1]<-"bb"
heatcool$faf[heatcool$faf==1]<-"faf"
heatcool$hp.x[heatcool$hp.x==1]<-"hp.h"
heatcool$dhp.x[heatcool$dhp.x==1]<-"dhp.h"
heatcool$gshp.x[heatcool$gshp.x==1]<-"gshp.h"
heatcool$hpdf.x[heatcool$hpdf.x==1]<-"hpdf.h"
heatcool$hp.y[heatcool$hp.y==1]<-"hp.c"
heatcool$cac[heatcool$cac==1]<-"cac"
heatcool$dhp.y[heatcool$dhp.y==1]<-"dhp.c"
heatcool$gshp.y[heatcool$gshp.y==1]<-"gshp.c"
heatcool$hpdf.y[heatcool$hpdf.y==1]<-"hpdf.c"
heatcool$conc<-paste(heatcool$bb,heatcool$faf,heatcool$hp.x,heatcool$dhp.x,heatcool$gshp.x,heatcool$hpdf.x,heatcool$hp.y,heatcool$cac,heatcool$dhp.y,heatcool$gshp.y,heatcool$hpdf.y,sep = "-")
heatcoolagg<-heatcool%>%group_by(conc)%>%summarise(n=n())
heatcoolagg<-heatcool%>%group_by(conc)%>%summarise(n=n(),bb=max(bb),faf=max(faf),hph=max(hp.x),dhpx=max(dhp.x),
  hpc=max(hp.y),cac=max(cac),dhpc=max(dhp.y))
