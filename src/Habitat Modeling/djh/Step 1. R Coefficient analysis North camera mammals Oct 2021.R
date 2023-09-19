# R ABMI coefficients analysis for mammals in the north, using camera results
# This is step 1, process data files to be used by step 2 (the actual modeling).  Step 3, a bootstrapped version, has not been updated for the camera mammals, due to lack of any interest in those results
# The data file has one row per camera, with summaries of the species' photos, series and duration, as well as seasonal times camera was operating and detection distances per species group.  Pre-processed in R.
# Also, processing of km2 grid for the province is done separately, mainly because of memory limits

datafile<-"C:/Dave/ABMI/Data/Mammals/2021/abmi-cmu-nwsar-bg_all-years_density_wide_2021-11-04.csv"  # Processed density files from Marcus for various projects, combined
vegfile<-"C:/Dave/ABMI/Data/Site info/2021/abmi-cmu-nwsar-bg_all-years_veghf-soilhf-detdistveg_2021-10-06.csv"  #  File simplified and corrected from GIS raw point summary, and information added from images for missed deployments.  Note: this version may or may not have all previous years' corrections
HFgroupfile<-"C:/Dave/ABMI/Data/Site info/2020/lookup-hf-class.csv"  # Lookup table for HF to HF group - modifed based on May 29 2020 table from Peter (which didn't contain all the categories here)
pmfile<-"C:/Dave/ABMI/Cameras/Coefficients/2020/Analysis north/Prediction matrix for mammal models NORTH 2020.csv"  # Prediction matrix - shows which (grouped) veg types and HF to use to predict which coefficients.  Including for P/A A|P models. Needs to be edited in Excel when veg or HF groupings change in models.
siteinfofile<-"C:/Dave/ABMI/Data/Site info/Site summary with climate.csv"
deploymentlocationsfile<-"C:/Dave/ABMI/Data/Mammals/2021/Deployment locations all Nov 2021.csv"  # Needed to fill in missing nearest sites below - missing AUB, but Sask not used anyway
lurefile<-"C:/Dave/ABMI/Data/Mammals/2021/abmi-cmu-nwsar-bg_all-years_lure_2021-11-04.csv"  # Lure info not included in dataset any more
dataset.out<-"C:/Dave/ABMI/Cameras/Coefficients/2021/Analysis north/R Dataset SpTable for ABMI North mammal coefficients 2021.RData"  # To save combined species-veg-HF file, plus SpTable.

# Read data file
d1<-read.csv(datafile,stringsAsFactors=FALSE)
# Convert colnames to same formats as in previous years, to avoid changing many lines throughout this and subsequent scripts
colnames(d1)<-gsub("_summer","Summer",colnames(d1))
colnames(d1)<-gsub("_winter","Winter",colnames(d1))
colnames(d1)<-gsub("\\.","",colnames(d1))
# Eliminate projects that don't use ABMI protocols or are otherwise weird.
project.list<-c("ABMI Ecosystem Health","ABMI Northern Focal Areas","Big Grids","CMU","Northwest Species at Risk")  # Qualifying projects, except that ABMI Ecosystem Health includes several unrelated projects (in the data file that I used) - those are excluded below
d1$Use<-0
for (i in 1:length(project.list))	d1$Use<-ifelse(substr(d1$project,1,nchar(project.list[i]))==project.list[i],1,d1$Use)  # Mark to use if in qualifying project
# Then take out weird projects included as part of ABMI Ecosystem Health project.  This may change in new datafile with updated project names.
d1$Use<-ifelse(substr(d1$location,1,5)=="OG-EI",0,d1$Use)  # Excluding all EI, even though interior deployment were used previously - just easier here
d1$Use<-ifelse(substr(d1$location,1,7)=="OG-CITS",0,d1$Use)
d1$Use<-ifelse(substr(d1$location,1,5)=="OG-RIVR",0,d1$Use)
d1<-d1[d1$Use==1,]

# Read in Veg + HF, process
v<-read.csv(vegfile,stringsAsFactors=FALSE)
names(v)[which(names(v)=="VEGHFAGEclass")]<-"VegHF"  # Use previous easier name
v<-v[!is.na(v$VegHF),]  # Can't use deployments with no veg info
v$VegHF<-ifelse(substr(v$location,1,1)=="W" & substr(v$location,1,3)!="WAB","WetlandMargin",v$VegHF)  # Set all ABMI-W sites (now called just W#, so be careful searching) to WetlandMargin - specific stratum targeted by that study's design
# Set up columns, 1 per veg or HF type - making a wide-form version of the point veg+HF file so that the veg can be processed and analysed the same way I did for all the other taxa.  Note that ALL HF types in v$VegHF have to be in HFgl$HF_GROUP, except CC types, which are handled separately
VegHF.list<-sort(unique(v$VegHF))
q<-array(0,c(nrow(v),length(VegHF.list)))
colnames(q)<-VegHF.list
v<-cbind(v,q)
for (i in VegHF.list) v[,i]<-ifelse(v$VegHF==i,1,0)  # Assign proportion of area = 1 to appropriate veg or HF column for each site

# Group HF types
HFgl<-read.csv(HFgroupfile)  # HF group lookup
HFgroups<-unique(HFgl$UseInAnalysis)
for (i in HFgroups) {
  HFtypesingroup<-colnames(v)[na.omit(match(HFgl$HF_GROUP[HFgl$UseInAnalysis==i],colnames(v)))]
  if (i=="HFor") HFtypesingroup<-c(HFtypesingroup,names(v)[substr(names(v),1,2)=="CC"])  # Add the individual cutblock types to HFor group
  if (length(HFtypesingroup)>1) {
    v<-cbind(v,rowSums(v[,HFtypesingroup])) # Add a column for each HF group, summing the component HF types
  } else {
    v<-cbind(v,v[,HFtypesingroup])  # If there is only one HF type in that HF group, just add it
  }
  if (length(HFtypesingroup)==0) HFgroups<-HFgroups[-which(HFgroups==i)]  # Remove that group from the list if there are one of that HF type in the camera sample
}
names(v)<-c(names(v)[1:(ncol(v)-length(HFgroups))],as.character(HFgroups))
# And broader groups - SuccAlien
HFgroups<-unique(HFgl$SuccAlien)
for (i in HFgroups) {
  HFtypesingroup<-colnames(v)[na.omit(match(HFgl$HF_GROUP[HFgl$SuccAlien==i],colnames(v)))]
  if (i=="Succ") HFtypesingroup<-c(HFtypesingroup,names(v)[substr(names(v),1,2)=="CC"])  # Add the individual cutblock types to Succ group
  if (length(HFtypesingroup)>1) {
    v<-cbind(v,rowSums(v[,HFtypesingroup])) # Add a column for each HF group, summing the component HF types
  } else {
    v<-cbind(v,v[,HFtypesingroup])  # If there is only one HF type in that HF group, just add it
  }
  if (length(HFtypesingroup)==0) HFgroups<-HFgroups[-which(HFgroups==i)]  # Remove that group from the list if there are one of that HF type in the camera sample
}
names(v)<-c(names(v)[1:(ncol(v)-length(HFgroups))],as.character(HFgroups))
# And broader groups - NonlinLin
HFgroups<-unique(HFgl$NonlinLin)
for (i in HFgroups) {
  HFtypesingroup<-colnames(v)[na.omit(match(HFgl$HF_GROUP[HFgl$NonlinLin==i],colnames(v)))]
  if (i=="Nonlin") HFtypesingroup<-c(HFtypesingroup,names(v)[substr(names(v),1,2)=="CC"])  # Add the individual cutblock types to Nonlin group
  if (length(HFtypesingroup)>1) {
    v<-cbind(v,rowSums(v[,HFtypesingroup])) # Add a column for each HF group, summing the component HF types
  } else {
    v<-cbind(v,v[,HFtypesingroup])  # If there is only one HF type in that HF group, just add it
  }
}
names(v)<-c(names(v)[1:(ncol(v)-length(HFgroups))],as.character(HFgroups))
# Get rid of 3 deployments (* 2 years) intentionally put on game trails (as part of the CMU trail comparison - keep the others, because they represent linear HF)
v<-v[substr(v$VegHF,1,9)!="GameTrail",]
# Check for na in (any) veg class - these deployments should have been removed at start: sum(is.na(rowSums(v[,6:ncol(v)])))
# Also check that the max of broad HF classes is never more than 1 - if so, there is some problem in the lookup (often a duplicated name): table(v$Succ); table(v$Alien); table(v$Lin); table(v$Nonlin)

# Add lat long, natural regions, etc. to sites
# First add in nearest.sites for non-ABMI studies that do not have that info in the deployment name
s1<-read.csv(deploymentlocationsfile,stringsAsFactors=FALSE)  # Summarized from larger meta-data file - changes include adding nearest site where obvious from Deployment name, changing column names back to original names
s1$Year<-NULL  # No need for Year in this file - just creates duplicates in merge below
names(s1)[which(names(s1)=="Site.Name")]<-"location"
s1$Lat<-as.numeric(as.character(s1$Public.Latitude))
s1$Long<-as.numeric(as.character(s1$Public.Longitude))
# Do corrections for old/unique naming systems, where possible
s1$location<-gsub("-000","-",s1$location)
s1$location<-gsub("-00","-",s1$location)
s1$location<-gsub("-0","-",s1$location)
s1$location<-ifelse(substr(s1$location,1,5)=="ABMI-",substr(s1$location,6,nchar(s1$location)),s1$location)  # Remove initial "ABMI-" because not used in density file locations (but leave in other cases, like "OG-ABMI-...")
s1<-s1[duplicated(s1$location)==FALSE,]  # Get rid of duplicated locations
d1<-merge(d1,s1[,c("location","NearestSite","Long","Lat")],all.x=TRUE)  # Check for loss of info: d1$location[is.na(d1$NearestSite)] - AUB grid, a few LID, a few others
d1<-d1[!is.na(d1$NearestSite),]  # Get rid of those deployments without nearest site info or lat/long
d1$NearestSite<-as.numeric(as.character(d1$NearestSite))  # Warning message about NA's is okay - trying to change some "" records for NA's here
s<-read.csv(siteinfofile)  # To get ABMI site lat longs, and then natural regions via nearest ABMI sites
names(s)[which(names(s)=="PUBLIC_LONGITUDE")]<-"Long"
names(s)[which(names(s)=="PUBLIC_LATTITUDE")]<-"Lat"
names(s)[which(names(s)=="NATURAL_REGIONS")]<-"NR"
names(s)[which(names(s)=="NATURAL_SUBREGIONS")]<-"NSR"
names(s)[which(names(s)=="LANDUSE_FRAMEWORK")]<-"LUF"
for (i in 1:nrow(d1)) {  # Fill in missing nearest ABMI sites
  if (is.na(d1$NearestSite[i]) & !is.na(d1$Lat[i])) d1$NearestSite[i]<-s$SITE_ID[which.min(sqrt((d1$Long[i]-s$Long)^2 + (d1$Lat[i]-s$Lat)^2))]  # The nearest site based on lat long (without worrying about different km-scaling of lat and long here)
}
# Fill in missing lat longs
i<-which(is.na(d1$Lat))
d1$Lat[i]<-s$Lat[match(d1$NearestSite[i],s$SITE_ID)]
d1$Long[i]<-s$Long[match(d1$NearestSite[i],s$SITE_ID)]
q<-merge(d1,s[,c("SITE_ID","NR","NSR","LUF","AHM","PET","FFP","MAP","MAT","MCMT","MWMT")],by.x="NearestSite",by.y="SITE_ID")  # Check for loss of sites
q$TrueLat<-q$Lat
q$Lat<-ifelse(q$Lat<51.5,51.5,q$Lat)  # Truncated latitude for spatial modeling
# Then merge in veg
v$location_project<-paste(v$location,v$project,sep="_")
q0<-merge(q,v[,-(1:4)],by="location_project")  # Check for loss of sites.  These are CMU sites in Saskatchewan or new CMU sites where the point veg+HF hasn't been done or was lost.  Plus a few oddball sites.
d<-q0

# Combine indistinguishable wetland classes and group age classes
# Note: All age class 0 have been allocated an age class by looking at the images, unless there were no images at that deployment (in which case, they should not be in d any more anyway)
i<-which(substr(names(d),1,8)=="TreedBog")
d$TreedBog<-rowSums(d[,i])
i<-which(substr(names(d),1,8)=="TreedFen")
d$TreedFen<-rowSums(d[,i])
d$ShrubbyBogFen<-d$ShrubbyBog+d$ShrubbyFen
i<-which(substr(names(d),1,10)=="TreedSwamp")
d$TreedSwamp<-rowSums(d[,i])
for (i1 in i[order(-i)]) d[,i1]<-NULL # Get rid of these so they are not used by mistake
i<-which(substr(names(d),1,7)=="CCDecid" | substr(names(d),1,7)=="CCMixed")
d$CCDecidMixed<-rowSums(d[,i])
i<-which(substr(names(d),1,8)=="CCSpruce")
d$CCSpruce<-rowSums(d[,i])
i<-which(substr(names(d),1,6)=="CCPine")
d$CCPine<-rowSums(d[,i])
d$CCConif<-d$CCSpruce+d$CCPine
d$Marsh<-d$Marsh+d$GraminoidFen  # Change these all to marsh  Note: Could also be GrassyBog, if this isn't converted to another type in the original data file
d$GraminoidFen<-rep(0,nrow(d))  # Set to 0 so not two types for those sites  (and also GrassyBog if that is included in previous line)
# Truncate age distributions at class 8 (so this class becomes >140yr), because no ABMI data in older pine, deciduous, or mixed, and marginal in other upland and wet conifers.  Also CC's
# Check for other stand types with age 9 data in future iterations
d$Spruce8<-d$Spruce8+d$Spruce9
d$Spruce9<-NULL  # Get rid of those old classes to avoid confusion
d$Decid8<-d$Decid8+d$Decid9
d$Decid9<-NULL  # Get rid of those old classes to avoid confusion
d$Pine8<-d$Pine8+d$Pine9
d$Pine9<-NULL  # Get rid of those old classes to avoid confusion
d$Mixedwood8<-d$Mixedwood8+d$Mixedwood9
d$Mixedwood9<-NULL  # Get rid of those old classes to avoid confusion
d$TreedFen8<-d$TreedFen8+d$TreedFen9
d$TreedFen9<-NULL  # Get rid of those old classes to avoid confusion
d$TreedBog8<-d$TreedBog8+d$TreedBog9
d$TreedBog9<-NULL  # Get rid of those old classes to avoid confusion
# The combinations below may become unnecessary when more sites are surveyed
d$CCPine1<-d$CCPine1+d$CCPine2+d$CCPine3  # Too few of class 2 and 3.  Check for other classes in updates
d$CCPine2<-NULL  # Get rid of those old classes to avoid confusion
d$CCPine3<-NULL  # Get rid of those old classes to avoid confusion
d$CCMixedwood2<-d$CCMixedwood2+d$CCMixedwood3+d$CCMixedwood4  # Too few of class 3 and 4. Check for other classes in updates
d$CCMixedwood3<-NULL  # Get rid of those old classes to avoid confusion
d$CCMixedwood4<-NULL  # Get rid of those old classes to avoid confusion
d$CCSpruce2<-d$CCSpruce2+d$CCSpruce3+d$CCSpruce4  # Too few of class 3 and 4. Check for other classes in updates
d$CCSpruce3<-NULL  # Get rid of those old classes to avoid confusion
d$CCSpruce4<-NULL  # Get rid of those old classes to avoid confusion
# Make combined classes for entire stand types
d$Spruce<-d$SpruceR+d$Spruce1+d$Spruce2+d$Spruce3+d$Spruce4+d$Spruce5+d$Spruce6+d$Spruce7+d$Spruce8
d$Pine<-d$PineR+d$Pine1+d$Pine2+d$Pine3+d$Pine4+d$Pine5+d$Pine6+d$Pine7+d$Pine8  # Check if all these are in input data - causes error if some type is not found in input file - and check that there are no new ones in future iterations
d$Decid<-d$DecidR+d$Decid1+d$Decid2+d$Decid3+d$Decid4+d$Decid5+d$Decid6+d$Decid7+d$Decid8  # Check if all these are in input data - causes error if some type is not found in input file
d$Mixedwood<-d$MixedwoodR+d$Mixedwood2+d$Mixedwood3+d$Mixedwood4+d$Mixedwood5+d$Mixedwood6+d$Mixedwood7+d$Mixedwood8
d$TreedBogFen<-d$TreedFen+d$TreedBogR+d$TreedBog1+d$TreedBog2+d$TreedBog3+d$TreedBog4+d$TreedBog5+d$TreedBog6+d$TreedBog7+d$TreedBog8

# Add combined veg or HF variables for more general models
d$DecidMixed<-d$Decid+d$Mixedwood
d$UpCon<-d$Spruce+d$Pine
d$CCAll<-d$CCDecidMixed+d$CCSpruce
d$Cult<-d$CultivationCrop+d$CultivationRoughPasture+d$CultivationTamePasture
d$RurUrbInd<-d$Rural+d$Urban+d$Industrial+d$HardLin
d$SoftLin<-d$EnSoftLin+d$EnSeismic+d$TrSoftLin
d$ShrubbyWet<-d$ShrubbyBogFen+d$ShrubbySwamp
d$OpenWet<-d$ShrubbyWet+d$Marsh+d$GraminoidFen
d$TreedWet<-d$TreedBogFen+d$TreedSwamp
d$GrassShrub<-d$GrassHerb+d$Shrub
d$TreedAll<-d$DecidMixed+d$UpCon+d$TreedWet
d$OpenAll<-d$GrassShrub+d$OpenWet
d$Boreal<-d$TreedAll+d$OpenWet  # Everything except GrassShrub
d$THF<-d$Alien+d$Succ
d$Lowland<-d$TreedWet+d$OpenWet
d$UplandForest<-d$UpCon+d$DecidMixed  # Upland, except GrassShrub
d$Upland<-1-d$Lowland-d$THF-d$WetlandMargin
# CHECK for max of some of these groups being >1 - indicates duplication somewhere, which needs to be corrected

# Remove sites not in North (boreal or foothills)
d$UseAsNorth<-ifelse(d$NR=="Boreal" | d$NR=="Canadian Shield" | d$NR=="Foothills" | d$NR=="Rocky Mountain" | d$NR=="Parkland","Y","N")
d$UseAsNorth<-ifelse(d$Water==1,"N",d$UseAsNorth)
d<-d[d$UseAsNorth=="Y",]

# Add NSR grouping
d$NSR1<-c(rep("Parkland",3),"DryMixedwood","CentralMixedwood",rep("Foothills",2),rep("North",4),rep("Shield",3),rep("Mountain",3))[match(d$NSR,c("Central Parkland","Foothills Parkland","Peace River Parkland","Dry Mixedwood","Central Mixedwood",
                                                                                                                                                 "Lower Foothills","Upper Foothills","Lower Boreal Highlands","Upper Boreal Highlands","Boreal Subarctic","Northern Mixedwood","Athabasca Plain","Kazan Uplands","Peace-Athabasca Delta","Montane","Subalpine","Alpine"))]

# Add Lured info
lure<-read.csv(lurefile)
lure<-lure[duplicated(lure$location_project)==FALSE,]  # Not sure why there are duplicated records here
names(lure)[which(names(lure)=="lure")]<-"Lured"  # Keep the same as previous version, for subsequent scripts
q<-merge(d,lure[,c("location_project","Lured")],all.x=TRUE)  # Check for NAs in Lured - figure out correct value and add to lure file
d<-q

# Add weights for each record - because some sites sampled >1 time
# Note: currently no down-weighting of deployments that are in dense grids of deployments
# Eliminate summer records for any deployment with <10 days summer sampling, and ditto for winter.  And eliminate entirely any deployments that don't qualify in either season
names(d)[which(names(d)=="summer")]<-"SummerDays"  # Using previous name, because too late in the day to change this through the scripts!
names(d)[which(names(d)=="winter")]<-"WinterDays"  # Using previous name, because too late in the day to change this through the scripts!
d<-d[d$SummerDays>=10 | d$WinterDays>=10,]
i<-which(d$SummerDays<10)
j<-which(regexpr("Summer",names(d))>0)
j<-j[-1]  # First hit is for days - don't remove
d[i,j]<-NA  # Convert SummerDays to NA if SummerDays is <10 (and not already NA)
i<-which(d$WinterDays<10)
j<-which(regexpr("Summer",names(d))>0)
j<-j[-1]  # First hit is for days - don't remove
d[i,j]<-NA
# Then weights based on number of qualifying visits (for each season)
q<-by(ifelse(d$SummerDays>=10,1,0),d$location,sum)
d$wt.s<-1/as.numeric(q[match(d$location,names(q))])
d$wt.s<-ifelse(d$wt.s==Inf,0,d$wt.s)  # no weight to locations that have never been sampled (10+ days) in summer
q<-by(ifelse(d$WinterDays>=10,1,0),d$location,sum)
d$wt.w<-1/as.numeric(q[match(d$location,names(q))])
d$wt.w<-ifelse(d$wt.w==Inf,0,d$wt.w)  # no weight to locdeploymentsation that have never been sampled (10+ days) in winter

# Set up list of species to analyse, separately for summer and winter
# Includes main analysis list as SpTable, and larger group to do use/availability for as SpTable.ua
FirstSpCol.s<-which(names(d)=="BadgerSummer")  # Find species names.  Check if species list changes
LastSpCol.s<-which(names(d)=="WoodlandCaribouSummer")
FirstSpCol.w<-which(names(d)=="BadgerWinter")  # Find species names
LastSpCol.w<-which(names(d)=="WoodlandCaribouWinter")
# Summer species table
SpTable.s<-SpTable.s.ua<-names(d)[FirstSpCol.s:LastSpCol.s] # All speciesXseasons
SpTable.s<-SpTable.s[regexpr("Summer",SpTable.s)>0]  # and Summer only
SpTable.s.ua<-SpTable.s.ua[regexpr("Summer",SpTable.s.ua)>0]  # and Summer only
occ1.s<-NULL  # Figure out total number of occurrences of each species
for (i in 1:length(SpTable.s)) occ1.s[i]<-sum(sign(d[,SpTable.s[i]])*d$wt.s,na.rm=TRUE)
SpTable.s<-SpTable.s[-which(occ1.s<20)]  # Omit species with <20 occurrences
occ1.s<-NULL  # Figure out total number of occurrences of each species
for (i in 1:length(SpTable.s.ua)) occ1.s[i]<-sum(sign(d[,SpTable.s.ua[i]])*d$wt.s,na.rm=TRUE)  # Do not exclude parkland here,  - now used in use/availability in north (otherwise, no figures for veg ua of parkland species)
SpTable.s.ua<-SpTable.s.ua[-which(occ1.s<3)]  # Omit species with <3 occurrences for use/availability summaries
# and winter species table
SpTable.w<-SpTable.w.ua<-names(d)[FirstSpCol.w:LastSpCol.w] # All speciesXseasons
SpTable.w<-SpTable.w[regexpr("Winter",SpTable.w)>0]  # and Winter only
SpTable.w.ua<-SpTable.w.ua[regexpr("Winter",SpTable.w.ua)>0]  # and Winter only
occ1.w<-NULL  # Figure out total number of occurrences of each species
for (i in 1:length(SpTable.w)) occ1.w[i]<-sum(sign(d[,SpTable.w[i]])*d$wt.w,na.rm=TRUE)
SpTable.w<-SpTable.w[-which(occ1.w<20)]  # Omit species with <20 occurrences
occ1.w<-NULL  # Figure out total number of occurrences of each species
for (i in 1:length(SpTable.w.ua)) occ1.w[i]<-sum(sign(d[,SpTable.w.ua[i]])*d$wt.w,na.rm=TRUE)  # Do not exclude parkland here,  - now used in use/availability in north (otherwise, no figures for veg ua of parkland species)
SpTable.w.ua<-SpTable.w.ua[-which(occ1.w<3)]  # Omit species with <3 occurrences for use/availability summaries

# Read prediction matrix into pm, so that it will be available in next step
pm<-read.csv(pmfile)

save(file=dataset.out,d,FirstSpCol.s,LastSpCol.s,FirstSpCol.w,LastSpCol.w,SpTable.s,SpTable.s.ua,SpTable.w,SpTable.w.ua,pm)


# Extra section to add 150m veg+HF to the datafile
v<-read.csv("C:/Dave/ABMI/Data/Site info/2021/abmi-nwsar-bg_all-years_veghf-150m-buffer_2021-11-04.csv")  # Combined from wide-form summary tables provided by Marcus
v[,3:ncol(v)]<-v[,3:ncol(v)]/rowSums(v[,3:ncol(v)])  # Convert to proportions
# Process 150m veg+HF variables into broader categories
HFgroupfile<-"C:/Dave/ABMI/Data/Site info/2020/lookup-hf-class.csv"  # Lookup table for HF to HF group - modifed based on May 29 2020 table from Peter (which didn't contain all the categories here)
HFgl<-read.csv(HFgroupfile)  # HF group lookup
# SuccAlien
HFgroups<-unique(HFgl$SuccAlien)
for (i in HFgroups) {
  HFtypesingroup<-colnames(v)[na.omit(match(HFgl$HF_GROUP[HFgl$SuccAlien==i],colnames(v)))]
  if (i=="Succ") HFtypesingroup<-c(HFtypesingroup,names(v)[substr(names(v),1,2)=="CC"])  # Add the individual cutblock types to Succ group
  if (length(HFtypesingroup)>1) {
    v<-cbind(v,rowSums(v[,HFtypesingroup])) # Add a column for each HF group, summing the component HF types
  } else {
    v<-cbind(v,v[,HFtypesingroup])  # If there is only one HF type in that HF group, just add it
  }
  if (length(HFtypesingroup)==0) HFgroups<-HFgroups[-which(HFgroups==i)]  # Remove that group from the list if there are one of that HF type in the camera sample
}
names(v)<-c(names(v)[1:(ncol(v)-length(HFgroups))],paste("x",as.character(HFgroups),sep=""))  # Leading "x" to distinguish 150m veg from point veg
# NonlinLin
HFgroups<-unique(HFgl$NonlinLin)
for (i in HFgroups) {
  HFtypesingroup<-colnames(v)[na.omit(match(HFgl$HF_GROUP[HFgl$NonlinLin==i],colnames(v)))]
  if (i=="Nonlin") HFtypesingroup<-c(HFtypesingroup,names(v)[substr(names(v),1,2)=="CC"])  # Add the individual cutblock types to Nonlin group
  if (length(HFtypesingroup)>1) {
    v<-cbind(v,rowSums(v[,HFtypesingroup])) # Add a column for each HF group, summing the component HF types
  } else {
    v<-cbind(v,v[,HFtypesingroup])  # If there is only one HF type in that HF group, just add it
  }
}
names(v)<-c(names(v)[1:(ncol(v)-length(HFgroups))],paste("x",as.character(HFgroups),sep=""))  # Leading "x" to distinguish 150m veg from point veg
v$xTHF<-v$xAlien+v$xSucc
# And native veg groups
i<-which(substr(names(v),1,5)=="Decid" | substr(names(v),1,5)=="Mixed")
v$xDecid<-rowSums(v[,i])  # Leading "x" to distinguish 150m veg from point veg
i<-which(substr(names(v),1,4)=="Pine" | substr(names(v),1,6)=="Spruce")
v$xUpCon<-rowSums(v[,i])
i<-which(substr(names(v),1,5)=="Treed")
v$xLowTreed<-rowSums(v[,i])
i<-which(names(v) %in% c("GraminoidFen","Marsh","ShrubbyBog","ShrubbyFen","ShrubbySwamp","Water") == TRUE)
v$xLowOpen<-rowSums(v[,i])
i<-which(names(v) %in% c("Shrub","GrassHerb","Bare","SnowIce") == TRUE)
v$xUpOpen<-rowSums(v[,i])
i<-which(substr(names(v),nchar(names(v)),nchar(names(v))) %in% c("5","6","7","8","9") == TRUE)
v$xOld<-rowSums(v[,i])
i<-which(substr(names(v),nchar(names(v)),nchar(names(v))) %in% c("R","1","2") == TRUE)  # NOTE: This is including cutblocks
v$xYoung<-rowSums(v[,i])
v$xUpland<-v$xUpCon+v$xDecid+v$xUpOpen
v$xLowland<-v$xLowTreed+v$xLowOpen

v$location_project<-paste(v$location,v$project,sep="_")

# Merge 150m veg+HF with dataset, check, save
q<-merge(d,v[,c("location_project","xAlien","xSucc","xNonlin","xLin","xTHF","xDecid","xUpCon","xLowTreed","xLowOpen","xUpOpen","xOld","xYoung","xUpland","xLowland")],by="location_project",all.x=TRUE)  # Check for problems
d<-q
save(file=dataset.out,d,FirstSpCol.s,LastSpCol.s,FirstSpCol.w,LastSpCol.w,SpTable.s,SpTable.s.ua,SpTable.w,SpTable.w.ua,pm)

