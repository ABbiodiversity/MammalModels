#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (OSM)

# Title:            Estimate OSM BADR Treatment Effects
# Description:      Uses both OSM BADR camera data from 2021 as well as relevant EH data
# Author:           Dave J Huggard, Marcus Becker

# Previous scripts: 01_osm-calculate-density-by-location.R

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(stringr)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Import data

# Species densities
d1 <- read_csv(paste0(g_drive, "results/density/deployments/osm_all-years_density_wide_2023-03-07.csv")) |>
  # Remove ACME locations
  filter(!str_detect(project, "ACME"))

# OSM deployment treatment metadata
s <- read_csv(paste0(g_drive, "projects/osm-badr-site-selection/osm_2021_deployment-metadata.csv"))

# Lure effects
lure <- read_csv(paste0(g_drive, "data/processed/lure/Lure effect from MS for OSM May 2022.csv")) |>
  # Remove periods to use same names as density file
  mutate(common_name = str_remove_all(Species, "\\."))

# Processed main camera density file
dataset.out<-paste0(g_drive, "data/lookup/R Dataset SpTable for ABMI North mammal coefficients 2022.RData")
load(dataset.out)

# Supplemental camera deployment treatment metadata: EH & OG
s1 <- read_csv(paste0(g_drive, "projects/osm-badr-site-selection/supplemental/final/supplemental-osm-treatments_EH_2022-06-16.csv"))
s2 <- read.csv(paste0(g_drive, "projects/osm-badr-site-selection/supplemental/final/supplemental-osm-treatments_OG_2022-06-16.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Join deployment metadata to species densities
d2 <- s |>
  # Join does include the cameras paired with ACME (no relevant metadata)
  left_join(d1, by = c("project", "location")) |>
  # Remove Wetland cameras
  filter(!vegetation == "wetland") |>
  # Clean up fine scale edge treatment specification
  mutate(fine_scale = str_remove(fine_scale, " metres"),
         fine_scale = ifelse(fine_scale == "200" | fine_scale == "600", "300", fine_scale))

colnames(d2) <- gsub("_summer","Summer", colnames(d2))
colnames(d2) <- gsub("_winter","Winter", colnames(d2))
colnames(d2) <- gsub("\\.", "", colnames(d1))

# Summarise number of OSM cameras (sub-samples) per treatment and vegetation type (2021)
d2 |>
  group_by(vegetation, treatment, fine_scale) |>
  tally() |>
  arrange(treatment, fine_scale, vegetation)

# Add ABMI EH and OG deployments
s <- bind_rows(s1, s2) |>
  # Add a "-1" to the end of OG 2018
  mutate(location = ifelse(project == "ABMI Off-Grid Monitoring 2018", paste0(location, "-1"), location)) |>
  left_join(d, by = c("location", "project")) |>
  # Remove records with no density information
  filter(!is.na(location_project)) |>
  arrange(location, desc(project)) |>
  group_by(location) |>
  filter(row_number() == 1)


# Make the column names compatible
names(d2)<-gsub("\\.","",names(d2))
names(d)<-gsub("Winter","_winter",names(d))
names(d)<-gsub("Summer","_summer",names(d))
names(d)[which(names(d)=="Lured")]<-"lure"
names(d)[which(names(d)=="_winterDays")]<-"winter"
names(d)[which(names(d)=="_summerDays")]<-"summer"
d$landscape_unit<-"9999"  # Not assigned for ABMI sites
d$jem<-substr(d$location,1,3)  # The "JEM" unit for ABMI sites is just the site (with OG sites added to the nearest site).  This line only works because all ABMI sites in OSM have 3 digits (i.e., >=100, <1000)
d$jem<-ifelse(substr(d$location,1,7)=="OG-ABMI",substr(d$location,9,11),d$jem)  # This line only works because the only OG sites are OG-ABMI and the nearest ABMI sites has 3 digits in all cases
d$camera<-9999
# Standardize the edge distances to 4 classes
fs1<-c("10","100","100","100","300","300","30","300","300","300","300","30","300","Off","On")[match(d$fine_scale,c("10 metres","100 metres","150 metres","170 metres","200 metres","250 metres","30 metres","300 metres","350 metres","400 metres","450 metres","50 metres","600 metres","Off","On"))]  # Check with table(d$fine_scale,fs1) and sum(is.na(fs1))
d$fine_scale<-fs1
# And use the simpler JEM treatment names
t1<-c("reference","low activity well pads","plant/mine buffer","road buffer","dense linear features","high activity in situ")[match(d$treatment,c("Low Disturbance/Reference","Low Activity Well Pads","Plant/Mine Buffer","Roads","Dense Linear Features","High Activity Insitu Well Pads"))]  # NOTE: No pre-insitu among ABMI sites?
t1<-ifelse(is.na(t1),as.character(d$treatment),t1)  # This is for the cases that were right to begin with
d$treatment<-t1
# And standardize vegetation
d$vegetation<-tolower(d$vegetation)
# Correct some mistakes in meta-data
d$fine_scale<-ifelse(is.na(d$fine_scale) & d$vegetation!="wetland","Off",d$fine_scale)
d$fine_scale<-ifelse(d$treatment=="reference",NA,as.character(d$fine_scale))  # These are all "Off" in the ABMI meta-data, all NA in OSM data
d$fine_scale<-ifelse(d$treatment=="road buffer" & d$fine_scale=="Off","300",as.character(d$fine_scale))
# Then add to BADR cameras
d<-rbind(d2,d[,colnames(d2)])

# Summarize number of cameras (before these are combined into replicates below)
write.table(table(paste(d$treatment,d$vegetation),ifelse(is.na(d$fine_scale),"NA",as.character(d$fine_scale))), file="C:/Dave/ABMI/Cameras/2022 analysis/OSM 2022/Table of fine scale treatment by jem treatmentXveg including extra ABMI - n cameras.csv",sep=",",col.names=NA)

# Set densities with <20 operating days in season to NA
i<-which(d$summer<20)
j<-which(regexpr("_summer",names(d))>0)
d[i,j]<-NA
i<-which(d$winter<20)
j<-which(regexpr("_winter",names(d))>0)
d[i,j]<-NA

# Analyzable species - >15 occurrences
sp.first<-which(names(d)=="Beaver_summer")
sp.last<-which(names(d)=="WoodlandCaribou_winter")
occ1<-colSums(sign(d[,sp.first:sp.last]),na.rm=TRUE)
occ2<-by(occ1,substr(names(occ1),1,nchar(names(occ1))-7),sum)  # Using simple sum of summer and winter (whether or not they were at the same deployment)
occ<-as.numeric(occ2)
names(occ)<-names(occ2)
SpTable<-names(occ)[occ>15]
SpTable<-c("BlackBear","CanadaLynx","Coyote","Fisher","GrayWolf","Marten","Moose","SnowshoeHare","WhitetailedDeer","WoodlandCaribou")  # Excluding a few species that get added only because of the ABMI cameras
sp.names<-c("Black Bear","Lynx","Coyote","Fisher","Wolf","Marten","Moose","Snowshoe Hare","White-tailed Deer","Woodland Caribou")

# Calculate average density for the two seasons
for (sp in 1:length(SpTable)) {
  sp.summer<-paste(SpTable[sp],"summer",sep="_")
  sp.winter<-paste(SpTable[sp],"winter",sep="_")
  d[,SpTable[sp]]<-ifelse(is.na(d[,sp.winter]) | SpTable[sp]=="Black.Bear",d[,sp.summer],ifelse(is.na(d[,sp.summer]),d[,sp.winter],(d[,sp.summer]+d[,sp.winter])/2))  # Use summer if winter=NA (or bear), winter if summer=NA, average if both not NA
}

# Figure out variance of log abundance-given-presence, using all (individual) cameras
log.var<-NULL
for (sp in 1:length(SpTable)) {
  dx<-data.frame(Treat=paste(d$treatment,d$fine_scale,d$vegetation),Camera=d$camera,Count=d[,SpTable[sp]]/ifelse(!is.na(d$lure) & d$lure=="Yes",lure$TA[lure$Species==SpTable[sp]],1))  # Count includes direct lure adjustment
  dx1<-dx[dx$Count>0,]
  m<-lm(log(Count)~Treat,data=dx1)  # Using residual after means for treatment X fine_scale X vegetation
  log.var[sp]<-(summary(m)$sigma)^2  # Residual variance
}

# Set up summaries
LU.list<-sort(unique(d$landscape_unit))
d$LU.jem<-paste(d$landscape_unit,d$jem,sep="_")
LU.jem.list<-sort(unique(d$LU.jem))
d$LU.jem.fine<-paste(d$landscape_unit,d$jem,d$fine_scale,sep="_")
LU.jem.fine.list<-sort(unique(d$LU.jem.fine))

# First, average any subsamples within LU.jem.fine
# At this point, just doing basic summary, but including n.lured deployments in that LU.jem.fine, to correct (with distribution) during summaries below
for (i in 1:length(LU.jem.fine.list)) {
  d1<-d[d$LU.jem.fine==LU.jem.fine.list[i],]
  q1<-colMeans(d1[,SpTable],na.rm=TRUE)
  q2<-colSums(sign(d1[,SpTable]),na.rm=TRUE)
  names(q2)<-paste(names(q2),"npres",sep=".")
  q<-data.frame(project=d1$project[1],landscape_unit=d1$landscape_unit[1],jem=d1$jem[1],vegetation=d1$vegetation[1],treatment=d1$treatment[1],fine_scale=d1$fine_scale[1],LU.jem=d1$LU.jem[1],LU.jem.fine=d1$LU.jem.fine[1],
                n=nrow(d1),n.lure=sum(!is.na(d1$lure) & d1$lure=="Yes"),t(q1),t(q2))  # Assuming lure=NA (wetlands) means No lure
  if (i==1) {
    d2<-q
  } else {
    d2<-rbind(d2,q)
  }
}

d<-d2  # This is the basic file to use for analyses
d<-d[!is.na(d$BlackBear),]  # Two edge distances in LU 3 JEM 1F1 have no info

# Figure out expected abundance-given-presence when there are no occurrences
agp0<-NULL
for (sp in 1:length(SpTable)) {
  d1<-data.frame(n=d$n,Count=d[,SpTable[sp]],nPres=d[,paste(SpTable[sp],"npres",sep=".")])
  x<-d1$nPres/d1$n
  y<-d1$Count*d1$n/d1$nPres  # AGP - will be NA when nPres=0
  m<-lm(log(y)~x,weights=d1$n)
  agp0[sp]<-exp(predict(m,newdata=data.frame(x=0)))
  if(coef(m)[2]<0) agp0[sp]<-exp(mean(log(y),na.rm=TRUE))  # For case that line slopes wrong way.  Using geometric mean here to match regression
}




