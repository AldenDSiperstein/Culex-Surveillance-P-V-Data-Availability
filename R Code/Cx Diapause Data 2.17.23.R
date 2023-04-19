# code by: Laura Pomeroy
# last updated: 2/17/23

library("lubridate")
library("maps")

########
# data #
########

setwd("~/Documents/Personnel/Alden Siperstein")

data=read.csv("Culex_Diapause_Cleaned_Data_6_Jan_2023_ADS.csv",header=T)

data1920=data[which(mdy(data[,1])<"2020-06-01"),] # pulling out all data from the 2019-2020 sampling season (collection dates 2019-09-27 through 2020-05-16)
data2021=data[which(mdy(data[,1])>"2020-06-01" & mdy(data[,1])<"2021-09-01"),] # pulling out all data from the 2020-2021 sampling season (collection dates 2020-10-03 through 2021-05-16)
data2122=data[which(mdy(data[,1])>"2021-09-01"),] # pulling out all data from the 2021-2022 sampling season (collection dates 2021-10-04 through 2022-05-12)

############################################
# mosquito species and abundance (Table 1) #
############################################

mosSp=function(dates,data){

  mosResults=matrix(NA,ncol=7,nrow=length(dates))
  
  for (i in 1:(length(dates))){
    ind=which(mdy(data[,1])==dates[i])
    species=data[ind,11]
    mosResults[i,1]=length(which(species=="Cx. erraticus"))
    mosResults[i,2]=length(which(species=="Cx. pipiens"))
    mosResults[i,3]=length(which(species=="Cx. restuans"))
    mosResults[i,4]=length(which(species=="Cx. salinarius"))
    mosResults[i,5]=length(which(species=="Cx. territans"))
    mosResults[i,6]=length(which(species==c("Cx. pipiens or Cx. restuans")))+length(which(species==c("Culex")))+length(which(species==c("Cx. restuans or Cx. salinarius")))
    mosResults[i,7]=length(which(species=="Non-Culex or poorly IDed sample"))
  }
  return(mosResults)
}  

allDates=sort(unique(mdy(data$Collection_Date_.M.D.Y.)))
species1922=mosSp(allDates,data)
apply(species1922,2,sum) # mosquito species and abundance over the entire study 

dates1920=sort(unique(mdy(data1920$Collection_Date_.M.D.Y.)))
species1920=mosSp(dates1920,data1920)
apply(species1920,2,sum) # mosquito species and abundance from the 2019-2020 sampling season (collection dates 2019-09-27 through 2020-05-16)

dates2021=sort(unique(mdy(data2021$Collection_Date_.M.D.Y.)))
species2021=mosSp(dates2021,data2021)
apply(species2021,2,sum) # mosquito species and abundance from the 2020-2021 sampling season (collection dates 2020-10-03 through 2021-05-16)

dates2122=sort(unique(mdy(data2122$Collection_Date_.M.D.Y.)))
species2122=mosSp(dates2122,data2122)
apply(species2122,2,sum) # mosquito species and abundance from the 2021-2022 sampling season (collection dates 2021-10-04 through 2022-05-12)

############################################
# times data collected (Figures 1a and 1b) #
############################################

pdf("Figure1ab.pdf",4,6)

par(mfrow=c(3,1))
par(mar = c(2.5, 4, 0.3, 0.1))        # Reduce space around plots

plot(dates2122,species2122[,2],pch=20,cex=2,type="h",ylim=c(0,200),lwd=4,ylab=expression(italic("Cx. pipiens")),xlab="")
plot(dates2122,species2122[,3],pch=20,cex=2,ylim=c(0,200),col="blue3",type="h",lwd=4,ylab=expression(italic("Cx. restuans")),xlab="")
plot(dates2122,species2122[,1],pch=20,cex=2,ylim=c(0,200),col="gray",type="h",lwd=4,ylab=expression(italic("Cx. erraticus")),xlab="")

dev.off()

############################################
# sites data collected (Figures 1c and 1d) #
############################################

sites2122=unique(cbind(data2122[,3],data2122[,4]))
sites2122results=matrix(NA,nrow=17,ncol=4)

for (i in 1:17){
  ind=which(cbind(data2122[,3],data2122[,4])==sites2122[i])
  sites2122results[i,1]=length(ind)
  sites2122results[i,2]=sum(data2122[ind,11]=="Cx. pipiens")
  sites2122results[i,3]=sum(data2122[ind,11]=="Cx. restuans")
  sites2122results[i,4]=sum(data2122[ind,11]=="Cx. erraticus")
}

pdf("Figure1cd.pdf",4,6)

par(mfrow=c(3,1))
par(mar = c(0.1, 0.1, 0.1, 0.1))        # Reduce space around plots

map('county', region = 'ohio,franklin')	
points(unique(cbind(data2122[,4],data2122[,3])),pch=20,cex=sites2122results[,2]/100,col="black")

map('county', region = 'ohio,franklin')	
points(unique(cbind(data2122[,4],data2122[,3])),pch=20,cex=sites2122results[,3]/100,col="blue3")

map('county', region = 'ohio,franklin')	
points(unique(cbind(data2122[,4],data2122[,3])),pch=20,cex=sites2122results[,4]/100,col="gray")

dev.off()

#################################################
# diapause dates for pip, rest, & err (Table 2) #
#################################################

sort(mdy(data[which(data[,11]=="Cx. pipiens" & data[,15]=="D"),][,1]))
sort(mdy(data[which(data[,11]=="Cx. pipiens" & data[,15]=="ND"),][,1]))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. pipiens" & data1920[,15]=="D"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. pipiens" & data2021[,15]=="D"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. pipiens" & data2122[,15]=="D"),][,1])))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. pipiens" & data1920[,15]=="ND"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. pipiens" & data2021[,15]=="ND"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. pipiens" & data2122[,15]=="ND"),][,1])))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. restuans" & data1920[,15]=="D"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. restuans" & data2021[,15]=="D"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. restuans" & data2122[,15]=="D"),][,1])))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. restuans" & data1920[,15]=="ND"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. restuans" & data2021[,15]=="ND"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. restuans" & data2122[,15]=="ND"),][,1])))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. erraticus" & data1920[,15]=="D"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. erraticus" & data2021[,15]=="D"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. erraticus" & data2122[,15]=="D"),][,1])))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. erraticus" & data1920[,15]=="ND"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. erraticus" & data2021[,15]=="ND"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. erraticus" & data2122[,15]=="ND"),][,1])))

###############################################
# gravid dates for pip, rest, & err (Table 2) #
###############################################

# data[,12] has "gravid" and "Gravid" as entries; change all to "Gravid" for consistency
data[which(data[,12]=="gravid"),12]="Gravid"

sort(mdy(data[which(data[,11]=="Cx. pipiens" & data[,12]=="Gravid"),][,1]))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. pipiens" & data1920[,12]=="Gravid"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. pipiens" & data2021[,12]=="Gravid"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. pipiens" & data2122[,12]=="Gravid"),][,1])))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. restuans" & data1920[,12]=="Gravid"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. restuans" & data2021[,12]=="Gravid"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. restuans" & data2122[,12]=="Gravid"),][,1])))

unique(sort(mdy(data1920[which(data1920[,11]=="Cx. erraticus" & data1920[,12]=="Gravid"),][,1])))
unique(sort(mdy(data2021[which(data2021[,11]=="Cx. erraticus" & data2021[,12]=="Gravid"),][,1])))
unique(sort(mdy(data2122[which(data2122[,11]=="Cx. erraticus" & data2122[,12]=="Gravid"),][,1])))

#####################################################
# proportion of pip in diapause by site (Figure 3a) #
#####################################################

propData=data2122[which(data2122[,11]=="Cx. pipiens" & data2122[,19]=="Culvert"),c(1,2,15)]
propData=propData[-which(propData[,3]==""),]

# 20 sites names (because of typos) but can be accurately regrouped in to the following 9 sites:
propData[grep("A - Smith Park", propData[,2]),2]="Smith"
propData[grep("C - Overbrook Drive", propData[,2]),2]="Overbrook"
propData[grep("C - Overbrooke", propData[,2]),2]="Overbrook"
propData[grep("C - Overbrook Dr", propData[,2]),2]="Overbrook"
propData[grep("C - Overbrook", propData[,2]),2]="Overbrook"
propData[grep("E - Woodward Nature Preserve", propData[,2]),2]="Woodward"
propData[grep("E - Woodward", propData[,2]),2]="Woodward"
propData[grep("I - Glen Echo Park", propData[,2]),2]="GlenEcho"
propData[grep("I- Glen Echo", propData[,2]),2]="GlenEcho"
propData[grep("I - Glen Echo", propData[,2]),2]="GlenEcho"
propData[grep("J - Walcutt Run", propData[,2]),2]="Walcutt"
propData[grep("J - Walcutt", propData[,2]),2]="Walcutt"
propData[grep("K - Storage Site", propData[,2]),2]="Storage"
propData[grep("L - OSU Sewer", propData[,2]),2]="OSU"         
propData[grep("M - Wolfe Park", propData[,2]),2]="Wolfe"
propData[grep("M - Wolfe Pk", propData[,2]),2]="Wolfe"
propData[grep("M - Wolfe park", propData[,2]),2]="Wolfe"        
propData[grep("N - Alum Creek", propData[,2]),2]="AlumCreek"
propData[grep("N - Alum Creek 2", propData[,2]),2]="AlumCreek"
propData[grep("N - Alum Creek 2", propData[,2]),2]="AlumCreek"   

# finding number of trapped mosquitoes in diapause, total counts of mosquitoes trapped, and number of sites for EACH DAY

propDates=sort(unique(mdy(propData[,1])))
diaCount=rep(NA,length(propDates))
totCount=rep(NA,length(propDates))
nSites=rep(NA,length(propDates))

for (i in 1:(length(propDates))){
  index=which(mdy(propData[,1])==propDates[i])
  diaCount[i]=length(which(propData[index,3]=="D"))
  totCount[i]=length(propData[index,3])
  nSites[i]=length(unique(propData[index,2]))
}

wks=as.character(isoweek(propDates))
numCul=tapply(nSites, wks, sum)[unique(wks)] 

prop=(tapply(diaCount, wks, mean)[unique(wks)])/(tapply(totCount, wks, mean)[unique(wks)])
# weeks go from 40-52 and then 1-19 except data from 49, 51, 3, 5, 7 are missing (NA)
# these missing weeks need NAs inserted between prop[9] and prop[10], prop[10] and prop[11], 
# prop[13] and prop[14], prop[14] and prop[15], and prop[15] and prop[16]

plotPoints=c(prop[1:9],mean(prop[9:10]),prop[10],mean(prop[10:11]),prop[11:13],mean(prop[13:14]),prop[14],mean(prop[14:15]),prop[15],mean(prop[15:16]),prop[16:27])
weightPoints=c(numCul[1:9],NA,numCul[10],NA,numCul[11:13],NA,numCul[14],NA,numCul[15],NA,numCul[16:27])

par(mfrow=c(1,1))

pdf("Figure 3a.pdf")
par(mar = c(4.5, 4.1, 1, 2.1))        # Reduce space around plots
plot(plotPoints,type="l",ylab="Proportion Diapausing",xlab="Month",xaxt = "n",main=expression(italic("Cx. pipiens")))
axis(1, at = c(1,5,9,14,18,22,27,31),labels=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May"))
polygon(x=c(22,22,27,27),y=c(-0.5,1.5,1.5,-0.5),col=adjustcolor("gray",alpha.f=0.6),border=adjustcolor("gray",alpha.f=0.8))
points(plotPoints,pch=20,cex=weightPoints/2)
legend("bottomleft",c("3 sites","6 sites","9 sites"),pch=c(20,20,20),pt.cex=c(1.5,3,4.5),horiz=T)
dev.off()

diaCountA=tapply(diaCount, wks, sum)[unique(wks)]
diaCountA=c(diaCountA[1:9],NA,diaCountA[10],NA,diaCountA[11:13],NA,diaCountA[14],NA,diaCountA[15],NA,diaCountA[16:27])
totCountA=tapply(totCount, wks, sum)[unique(wks)] 
totCountA=c(totCountA[1:9],NA,totCountA[10],NA,totCountA[11:13],NA,totCountA[14],NA,totCountA[15],NA,totCountA[16:27])
pipdataForAlden=cbind(diaCountA,totCountA,plotPoints,weightPoints)
rownames(pipdataForAlden)=c(40:52,1:19)
colnames(pipdataForAlden)=c("Count in Diapause","Total Count","Proportion in Diapause","Number of Sites")
write.csv(pipdataForAlden,"Cx. pipiens Proportion Diapause.csv")

#####################################################
# proportion of err in diapause by site (Figure 3b) #
#####################################################

propData=data2122[which(data2122[,11]=="Cx. erraticus" & data2122[,19]=="Culvert"),c(1,2,15)]
propData=propData[-which(propData[,3]==""),]

# 20 sites names (because of typos) but can be accurately regrouped in to the following 9 sites:
propData[grep("A - Smith Park", propData[,2]),2]="Smith"
propData[grep("C - Overbrook Drive", propData[,2]),2]="Overbrook"
propData[grep("C - Overbrooke", propData[,2]),2]="Overbrook"
propData[grep("C - Overbrook Dr", propData[,2]),2]="Overbrook"
propData[grep("C - Overbrook", propData[,2]),2]="Overbrook"
propData[grep("E - Woodward Nature Preserve", propData[,2]),2]="Woodward"
propData[grep("E - Woodward", propData[,2]),2]="Woodward"
propData[grep("I - Glen Echo Park", propData[,2]),2]="GlenEcho"
propData[grep("I- Glen Echo", propData[,2]),2]="GlenEcho"
propData[grep("I - Glen Echo", propData[,2]),2]="GlenEcho"
propData[grep("J - Walcutt Run", propData[,2]),2]="Walcutt"
propData[grep("J - Walcutt", propData[,2]),2]="Walcutt"
propData[grep("K - Storage Site", propData[,2]),2]="Storage"
propData[grep("L - OSU Sewer", propData[,2]),2]="OSU"         
propData[grep("M - Wolfe Park", propData[,2]),2]="Wolfe"
propData[grep("M - Wolfe Pk", propData[,2]),2]="Wolfe"
propData[grep("M - Wolfe park", propData[,2]),2]="Wolfe"        
propData[grep("N - Alum Creek", propData[,2]),2]="AlumCreek"
propData[grep("N - Alum Creek 2", propData[,2]),2]="AlumCreek"
propData[grep("N - Alum Creek 2", propData[,2]),2]="AlumCreek"   

# finding number of trapped mosquitoes in diapause, total counts of mosquitoes trapped, and number of sites for EACH DAY

propDates=sort(unique(mdy(propData[,1])))
diaCount=rep(NA,length(propDates))
totCount=rep(NA,length(propDates))
nSites=rep(NA,length(propDates))

for (i in 1:(length(propDates))){
  index=which(mdy(propData[,1])==propDates[i])
  diaCount[i]=length(which(propData[index,3]=="D"))
  totCount[i]=length(propData[index,3])
  nSites[i]=length(unique(propData[index,2]))
}

wks=as.character(isoweek(propDates))
numCul=tapply(nSites, wks, sum)[unique(wks)] 

prop=(tapply(diaCount, wks, mean)[unique(wks)])/(tapply(totCount, wks, mean)[unique(wks)])
# weeks go from 40-52 and then 1-19 except data from 49, 51, 3, 5, 7 are missing (NA)
# these missing weeks need NAs inserted between prop[9] and prop[10], prop[10] and prop[11], 
# prop[13] and prop[14], prop[14] and prop[15], and prop[15] and prop[16]

plotPoints=c(prop[1:9],mean(prop[9:10]),prop[10],mean(prop[10:11]),prop[11:13],mean(prop[13:14]),prop[14],mean(prop[14:15]),prop[15],mean(prop[15:16]),prop[16:27])
weightPoints=c(numCul[1:9],NA,numCul[10],NA,numCul[11:13],NA,numCul[14],NA,numCul[15],NA,numCul[16:27])

par(mfrow=c(1,1))

pdf("Figure 3b.pdf")
par(mar = c(4.5, 4.1, 1, 2.1))        # Reduce space around plots
plot(plotPoints,type="l",ylab="Proportion Diapausing",xlab="Month",xaxt = "n",main=expression(italic("Cx. erraticus")))
axis(1, at = c(1,5,9,14,18,22,27,31),labels=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May"))
polygon(x=c(27,27,31,31),y=c(-0.5,1.5,1.5,-0.5),col=adjustcolor("gray",alpha.f=0.6),border=adjustcolor("gray",alpha.f=0.8))
points(plotPoints,pch=20,cex=weightPoints/2)
legend("bottomleft",c("3 sites","6 sites","9 sites"),pch=c(20,20,20),pt.cex=c(1.5,3,4.5),horiz=T)
dev.off()

diaCountA=tapply(diaCount, wks, sum)[unique(wks)]
diaCountA=c(diaCountA[1:9],NA,diaCountA[10],NA,diaCountA[11:13],NA,diaCountA[14],NA,diaCountA[15],NA,diaCountA[16:27])
totCountA=tapply(totCount, wks, sum)[unique(wks)] 
totCountA=c(totCountA[1:9],NA,totCountA[10],NA,totCountA[11:13],NA,totCountA[14],NA,totCountA[15],NA,totCountA[16:27])
errdataForAlden=cbind(diaCountA,totCountA,propDiaA,numCulA)
rownames(errdataForAlden)=c(40:52,1:19)
colnames(errdataForAlden)=c("Count in Diapause","Total Count","Proportion in Diapause","Number of Sites")
write.csv(errdataForAlden,"Cx. erraticus Proportion Diapause.csv")

############################################################
# proportion of pip & rest in diapause by site (Figure S2) #
############################################################

# two plots: one culvert; one not culvert. put both species on each graph as different colors. graph a line for each site

# site choices:
# field sites/above ground: "Smith", "Webster", "Overbrook", "Antrim", "Woodward", "Cherrybottom", "Mock", "Maloney", "Glen Echo"
# culverts/below ground: "Smith", "Overbrook", "Woodward", "Glen Echo", "OSU", "Walcutt", "Alum", "Wolfe", "Storage"

compLoc=function(species,site,loc,data){
  
  speciesInd=which(data[,11]==species)
  speciesData=data[speciesInd,]
  
  indCul=c(which(speciesData[,5]=="Culvert"),which(speciesData[,5]=="culvert"))
  indAbove=c(which(speciesData[,5]=="BG Sentinel"),which(speciesData[,5]=="Gravid"),which(speciesData[,5]=="BG"),which(speciesData[,5]=="Resting"),which(speciesData[,5]=="CDC"),which(speciesData[,5]=="gravid"))

  if(loc=="culvert"){
    ind=indCul
    } else {
    ind=indAbove
    }

  speciesLocdata=speciesData[ind,]
  
  speciesLocSitedata=speciesLocdata[grep(site,speciesLocdata[,2]),]
  
  dates=sort(unique(mdy(data[,1])))
  
  results=matrix(NA,nrow=length(dates),ncol=3)
  colnames(results)=c("D","ND","Total")
  
  for (i in 1:length(dates)){
    dateInd=which(mdy(speciesLocSitedata[,1])==dates[i])
    tempdata=speciesLocSitedata[dateInd,]
    results[i,1]=length(which(tempdata=="D"))
    results[i,2]=length(which(tempdata=="ND"))
  }
  
  results[,3]=results[,1]+results[,2]
  return(results)
  
}

# GRAPH 1: Culvert; Cx. Pipiens

smithCulPIP=compLoc("Cx. pipiens","Smith", "culvert",data2122)
overbrookCulPIP=compLoc("Cx. pipiens","Overbrook", "culvert",data2122)
woodwardCulPIP=compLoc("Cx. pipiens", "Woodward", "culvert",data2122)
glenechoCulPIP=compLoc("Cx. pipiens", "Glen Echo", "culvert",data2122)
osuCulPIP=compLoc("Cx. pipiens","OSU", "culvert",data2122)
walcuttCulPIP=compLoc("Cx. pipiens","Walcutt", "culvert",data2122)
alumCulPIP=compLoc("Cx. pipiens", "Alum", "culvert",data2122)
wolfeCulPIP=compLoc("Cx. pipiens", "Wolfe", "culvert",data2122)
storageCulPIP=compLoc("Cx. pipiens", "Storage", "culvert",data2122)

pdf("FigureS2.pdf",5,10)
par(mfrow=c(2,1))
par(mar = c(3, 4, 1, 1))        # Reduce space around plots

ind=which(smithCulPIP[,1]/smithCulPIP[,3]<=1 & smithCulPIP[,1]/smithCulPIP[,3]>=0)
plot(dates2122[ind],smithCulPIP[ind,1]/smithCulPIP[ind,3],pch=20,xlab="Date",ylab="Proportion of Cx. pipiens in Diapause",cex=1)
lines(smooth.spline(dates2122[ind],smithCulPIP[ind,1]/smithCulPIP[ind,3],df=3),lwd=3)

ind=which(overbrookCulPIP[,1]/overbrookCulPIP[,3]<=1 & overbrookCulPIP[,1]/overbrookCulPIP[,3]>=0)
points(dates2122[ind],overbrookCulPIP[ind,1]/overbrookCulPIP[ind,3],pch=20,col="orange",cex=1)
lines(smooth.spline(dates2122[ind],overbrookCulPIP[ind,1]/overbrookCulPIP[ind,3],df=3),lwd=3,col="orange")

ind=which(woodwardCulPIP[,1]/woodwardCulPIP[,3]<=1 & woodwardCulPIP[,1]/woodwardCulPIP[,3]>=0)
points(dates2122[ind],woodwardCulPIP[ind,1]/woodwardCulPIP[ind,3],pch=20,col="green3",cex=1)
lines(smooth.spline(dates2122[ind],woodwardCulPIP[ind,1]/woodwardCulPIP[ind,3],df=3),lwd=3,col="green3")

ind=which(glenechoCulPIP[,1]/glenechoCulPIP[,3]<=1 & glenechoCulPIP[,1]/glenechoCulPIP[,3]>=0)
points(dates2122[ind],glenechoCulPIP[ind,1]/glenechoCulPIP[ind,3],pch=20,col="gray",cex=1)
lines(smooth.spline(dates2122[ind],glenechoCulPIP[ind,1]/glenechoCulPIP[ind,3],df=3),lwd=3,col="gray")

ind=which(osuCulPIP[,1]/osuCulPIP[,3]<=1 & osuCulPIP[,1]/osuCulPIP[,3]>=0)
points(dates2122[ind],osuCulPIP[ind,1]/osuCulPIP[ind,3],pch=20,col="red")
lines(smooth.spline(dates2122[ind],osuCulPIP[ind,1]/osuCulPIP[ind,3],df=3),lwd=3,col="red")

ind=which(walcuttCulPIP[,1]/walcuttCulPIP[,3]<=1 & walcuttCulPIP[,1]/walcuttCulPIP[,3]>=0)
points(dates2122[ind],walcuttCulPIP[ind,1]/walcuttCulPIP[ind,3],pch=20,col="yellow")
lines(smooth.spline(dates2122[ind],walcuttCulPIP[ind,1]/walcuttCulPIP[ind,3],df=3),col="yellow",lwd=3)

ind=which(alumCulPIP[,1]/alumCulPIP[,3]<=1 & alumCulPIP[,1]/alumCulPIP[,3]>=0)
points(dates2122[ind],alumCulPIP[ind,1]/alumCulPIP[ind,3],pch=20,col="blue")
lines(smooth.spline(dates2122[ind],alumCulPIP[ind,1]/alumCulPIP[ind,3],df=3),lwd=3,col="blue")

ind=which(wolfeCulPIP[,1]/wolfeCulPIP[,3]<=1 & wolfeCulPIP[,1]/wolfeCulPIP[,3]>=0)
points(dates2122[ind],wolfeCulPIP[ind,1]/wolfeCulPIP[ind,3],pch=20,col="purple")
lines(smooth.spline(dates2122[ind],wolfeCulPIP[ind,1]/wolfeCulPIP[ind,3],df=3),lwd=3,col="purple")

ind=which(storageCulPIP[,1]/storageCulPIP[,3]<=1 & storageCulPIP[,1]/storageCulPIP[,3]>=0)
points(dates2122[ind],storageCulPIP[ind,1]/storageCulPIP[ind,3],pch=20,col="pink2")
lines(smooth.spline(dates2122[ind],storageCulPIP[ind,1]/storageCulPIP[ind,3],df=3),lwd=3,col="pink2")

#legend("bottomleft",c("Smith culvert","Overbrook culvert","Woodward culvert","Glen Echo culvert"),pch=rep(20,4),lty=rep(1,4),col=c("black","orange","green3","gray"))

# GRAPH 2: Culvert; Cx. Restuans

smithCulREST=compLoc("Cx. restuans","Smith", "culvert",data2122)
overbrookCulREST=compLoc("Cx. restuans","Overbrook", "culvert",data2122)
woodwardCulREST=compLoc("Cx. restuans", "Woodward", "culvert",data2122)
glenechoCulREST=compLoc("Cx. restuans", "Glen Echo", "culvert",data2122)
osuCulREST=compLoc("Cx. restuans","OSU", "culvert",data2122)
walcuttCulREST=compLoc("Cx. restuans","Walcutt", "culvert",data2122)
alumCulREST=compLoc("Cx. restuans", "Alum", "culvert",data2122)
wolfeCulREST=compLoc("Cx. restuans", "Wolfe", "culvert",data2122)
storageCulREST=compLoc("Cx. restuans", "Storage", "culvert",data2122)

#par(mar = c(3, 4, 1, 1))        # Reduce space around plots

#ind=which(glenechoCulREST[,1]/glenechoCulREST[,3]<=1 & glenechoCulREST[,1]/glenechoCulREST[,3]>=0)
#plot(dates2122[ind],glenechoCulREST[ind,1]/glenechoCulREST[ind,3],pch=20,col="gray",xlab="Date",ylab="Proportion of Cx. restuans in Diapause",cex=1)
#lines(smooth.spline(dates2122[ind],glenechoCulREST[ind,1]/glenechoCulREST[ind,3],df=3),lwd=3,col="gray")

#ind=which(overbrookCulREST[,1]/overbrookCulREST[,3]<=1 & overbrookCulREST[,1]/overbrookCulREST[,3]>=0)
#points(dates2122[ind],overbrookCulREST[ind,1]/overbrookCulREST[ind,3],pch=20,col="orange",cex=1)
#lines(smooth.spline(dates2122[ind],overbrookCulREST[ind,1]/overbrookCulREST[ind,3],df=3),lwd=3,col="orange")

#ind=which(woodwardCulREST[,1]/woodwardCulREST[,3]<=1 & woodwardCulREST[,1]/woodwardCulREST[,3]>=0)
#points(dates2122[ind],woodwardCulREST[ind,1]/woodwardCulREST[ind,3],pch=20,col="green3",cex=1)
#lines(smooth.spline(dates2122[ind],woodwardCulREST[ind,1]/woodwardCulREST[ind,3],df=3),lwd=3,col="green3")

#ind=which(smithCulREST[,1]/smithCulREST[,3]<=1 & smithCulREST[,1]/smithCulREST[,3]>=0)
#points(dates2122[ind],smithCulREST[ind,1]/smithCulREST[ind,3],pch=20,col="black",cex=1)
#lines(smooth.spline(dates2122[ind],smithCulREST[ind,1]/smithCulREST[ind,3],df=3),lwd=3)

#ind=which(osuCulREST[,1]/osuCulREST[,3]<=1 & osuCulREST[,1]/osuCulREST[,3]>=0)
#points(dates2122[ind],osuCulREST[ind,1]/osuCulREST[ind,3],pch=20,col="red")
#lines(smooth.spline(dates2122[ind],osuCulREST[ind,1]/osuCulREST[ind,3],df=3),lwd=3,col="red")

#ind=which(walcuttCulREST[,1]/walcuttCulREST[,3]<=1 & walcuttCulREST[,1]/walcuttCulREST[,3]>=0)
#points(dates2122[ind],walcuttCulREST[ind,1]/walcuttCulREST[ind,3],pch=20,col="yellow")
#lines(smooth.spline(dates2122[ind],walcuttCulREST[ind,1]/walcuttCulREST[ind,3],df=3),col="yellow",lwd=3)

#ind=which(alumCulREST[,1]/alumCulREST[,3]<=1 & alumCulREST[,1]/alumCulREST[,3]>=0)
#points(dates2122[ind],alumCulREST[ind,1]/alumCulREST[ind,3],pch=20,col="blue")
#lines(smooth.spline(dates2122[ind],alumCulREST[ind,1]/alumCulREST[ind,3],df=3),lwd=3,col="blue")

#ind=which(wolfeCulREST[,1]/wolfeCulREST[,3]<=1 & wolfeCulREST[,1]/wolfeCulREST[,3]>=0)
#points(dates2122[ind],wolfeCulREST[ind,1]/wolfeCulREST[ind,3],pch=20,col="purple")
#lines(smooth.spline(dates2122[ind],wolfeCulREST[ind,1]/wolfeCulREST[ind,3],df=3),lwd=3,col="purple")

#ind=which(storageCulREST[,1]/storageCulREST[,3]<=1 & storageCulREST[,1]/storageCulREST[,3]>=0)
#points(dates2122[ind],storageCulREST[ind,1]/storageCulREST[ind,3],pch=20,col="pink2")
#lines(smooth.spline(dates2122[ind],storageCulREST[ind,1]/storageCulREST[ind,3],df=3),lwd=3,col="pink2")

#legend("bottomleft",c("Smith culvert","Overbrook culvert","Woodward culvert","Glen Echo culvert"),pch=rep(20,4),lty=rep(1,4),col=c("black","orange","green3","gray"))

# GRAPH 3: Culvert; Cx. erraticus

smithCulERR=compLoc("Cx. erraticus","Smith", "culvert",data2122)
overbrookCulERR=compLoc("Cx. erraticus","Overbrook", "culvert",data2122)
woodwardCulERR=compLoc("Cx. erraticus", "Woodward", "culvert",data2122)
glenechoCulERR=compLoc("Cx. erraticus", "Glen Echo", "culvert",data2122)
osuCulERR=compLoc("Cx. erraticus","OSU", "culvert",data2122)
walcuttCulERR=compLoc("Cx. erraticus","Walcutt", "culvert",data2122)
alumCulERR=compLoc("Cx. erraticus", "Alum", "culvert",data2122)
wolfeCulERR=compLoc("Cx. erraticus", "Wolfe", "culvert",data2122)
storageCulERR=compLoc("Cx. erraticus", "Storage", "culvert",data2122)

par(mar = c(3, 4, 1, 1))        # Reduce space around plots

ind=which(glenechoCulERR[,1]/glenechoCulERR[,3]<=1 & glenechoCulERR[,1]/glenechoCulERR[,3]>=0)
plot(dates2122[ind],glenechoCulERR[ind,1]/glenechoCulERR[ind,3],pch=20,col="gray",xlab="Date",ylab="Proportion of Cx. erraticus in Diapause",cex=1)
lines(smooth.spline(dates2122[ind],glenechoCulERR[ind,1]/glenechoCulERR[ind,3],df=3),lwd=3,col="gray")

ind=which(overbrookCulERR[,1]/overbrookCulERR[,3]<=1 & overbrookCulERR[,1]/overbrookCulERR[,3]>=0)
points(dates2122[ind],overbrookCulERR[ind,1]/overbrookCulERR[ind,3],pch=20,col="orange",cex=1)
lines(smooth.spline(dates2122[ind],overbrookCulERR[ind,1]/overbrookCulERR[ind,3],df=3),lwd=3,col="orange")

ind=which(woodwardCulERR[,1]/woodwardCulERR[,3]<=1 & woodwardCulERR[,1]/woodwardCulERR[,3]>=0)
points(dates2122[ind],woodwardCulERR[ind,1]/woodwardCulERR[ind,3],pch=20,col="green3",cex=1)
lines(smooth.spline(dates2122[ind],woodwardCulERR[ind,1]/woodwardCulERR[ind,3],df=3),lwd=3,col="green3")

ind=which(smithCulERR[,1]/smithCulERR[,3]<=1 & smithCulERR[,1]/smithCulERR[,3]>=0)
points(dates2122[ind],smithCulERR[ind,1]/smithCulERR[ind,3],pch=20,col="black",cex=1)
lines(smooth.spline(dates2122[ind],smithCulERR[ind,1]/smithCulERR[ind,3],df=3),lwd=3)

ind=which(osuCulERR[,1]/osuCulERR[,3]<=1 & osuCulERR[,1]/osuCulERR[,3]>=0)
points(dates2122[ind],osuCulERR[ind,1]/osuCulERR[ind,3],pch=20,col="red")
lines(smooth.spline(dates2122[ind],osuCulERR[ind,1]/osuCulERR[ind,3],df=3),lwd=3,col="red")

ind=which(walcuttCulERR[,1]/walcuttCulERR[,3]<=1 & walcuttCulERR[,1]/walcuttCulERR[,3]>=0)
points(dates2122[ind],walcuttCulERR[ind,1]/walcuttCulERR[ind,3],pch=20,col="yellow")
lines(smooth.spline(dates2122[ind],walcuttCulERR[ind,1]/walcuttCulERR[ind,3],df=3),col="yellow",lwd=3)

ind=which(alumCulERR[,1]/alumCulERR[,3]<=1 & alumCulERR[,1]/alumCulERR[,3]>=0)
points(dates2122[ind],alumCulERR[ind,1]/alumCulERR[ind,3],pch=20,col="blue")
lines(smooth.spline(dates2122[ind],alumCulERR[ind,1]/alumCulERR[ind,3],df=3),lwd=3,col="blue")

ind=which(wolfeCulERR[,1]/wolfeCulERR[,3]<=1 & wolfeCulERR[,1]/wolfeCulERR[,3]>=0)
points(dates2122[ind],wolfeCulERR[ind,1]/wolfeCulERR[ind,3],pch=20,col="purple")
lines(smooth.spline(dates2122[ind],wolfeCulERR[ind,1]/wolfeCulERR[ind,3],df=3),lwd=3,col="purple")

ind=which(storageCulERR[,1]/storageCulERR[,3]<=1 & storageCulERR[,1]/storageCulERR[,3]>=0)
points(dates2122[ind],storageCulERR[ind,1]/storageCulERR[ind,3],pch=20,col="pink2")
lines(smooth.spline(dates2122[ind],storageCulERR[ind,1]/storageCulERR[ind,3],df=3),lwd=3,col="pink2")

dev.off()
