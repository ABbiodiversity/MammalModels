# Initial step to process 1km2 province-wide grid info file - veg + HF for NORTH

km2hffile<-"C:/Dave/ABMI/Data/Km2 grid/2020/veg-hf_grid_v61hf2016v3WildFireUpTo2016.Rdata" # 1km2 grid veg+hf R object from Peter
km2infofile<-"C:/Dave/ABMI/Data/Km2 grid/2017/kgrid_table_km.Rdata"   # 1km2 grid info from Daiyuan processed by Peter - lat long, natural region, subregion, climate variables, etc.
HFgroupfile<-"C:/Dave/ABMI/Data/Site info/2020/lookup-hf-class.csv"  # Lookup table for HF to HF group from Peter.  This needs to be modified to contain the HF type groupings that we want coefficients for this year (changes frequently)

km2file.out<-"C:/Dave/ABMI/Data/Km2 grid/2020/R km2 grid current and backfilled processed 2016 NORTH June 2020.Rdata"  # File to save processed km2 grid files

# Note: Don't need to add the combined HF type variables to KM2, because coefficients are generated for each age class, or each HF type - even if these are the same as other coefficients because there is no data to support estimating them separately
# Load km2 grid veg+HF and convert to data frame (and hope memory doesn't explode)
load(km2hffile)  # dd_kgrid with [["veg_current"]] and [["veg_reference"]] relevant here
veg_current<-dd_kgrid[["veg_current"]]
veg_reference<-dd_kgrid[["veg_reference"]]
rm(dd_kgrid)  # To save memory
gc()

# Process current landbase first (reference landbase repeats these steps below)
# Convert to data.frame, and convert areas to proportions of the 1km2 raster
veg_current<-data.frame(as.matrix(veg_current))  # Convert to data.frame
km2.area<-rowSums(veg_current)
for (i in seq(1,nrow(veg_current),10000)) {  # Convert to proportions in chunks, otherwise memory fails
  print(paste(i,nrow(veg_current),date()))
  j<-ifelse(i+9999>nrow(veg_current),nrow(veg_current),i+9999)
  veg_current[i:j,]<-veg_current[i:j,]/km2.area[i:j]
}
veg_current$LinkID<-rownames(veg_current)

# Add in info about km2 rasters - lat, long, natural region and subregion, LUF
load(km2infofile)  # kgrid with info for km2 rasters
names(kgrid)[which(names(kgrid)=="POINT_X")]<-"Long"
names(kgrid)[which(names(kgrid)=="POINT_Y")]<-"TrueLat"
kgrid$Lat<-ifelse(kgrid$TrueLat<51.5,51.5,kgrid$TrueLat)  # Modified latitude when applying spatial models, to reduce influence of sites in southern foothills and Cypress hills
kgrid<-data.frame(LinkID=rownames(kgrid),kgrid[,c("Lat","Long","TrueLat","NRNAME","NSRNAME","LUF_NAME","AHM","PET","FFP","MAP","MAT","MCMT","MWMT")],pAspen=kgrid$pAspen)
names(kgrid)[which(names(kgrid)=="NRNAME")]<-"NR"  # Simpler and consistent with last year
names(kgrid)[which(names(kgrid)=="NSRNAME")]<-"NSR"
names(kgrid)[which(names(kgrid)=="LUF_NAME")]<-"LUF"
km2<-merge(veg_current,kgrid,by="LinkID")  # Check for data loss - dimensions of km2 should be the same as veg_current
rm(veg_current)
gc()

# Reduce to just km2 rasters in the North plus parkland to allow feathering with results from South based on pAspen
km2<-km2[km2$NR!="Grassland",]  # Everything except grassland currently used in North

# Combine categories in km2 grid data to match available coefficients
# Note: Using this year's names here, so need to change all subsequent scripts (e.g., "Decid" instead of "Deciduous", etc)
i<-which(substr(colnames(km2),1,10)=="TreedSwamp")  # No age class modeling for these types
km2$TreedSwamp<-rowSums(km2[,i])
i<-which(substr(colnames(km2),1,8)=="TreedFen")  # No age class modeling for these types
km2$TreedFen<-rowSums(km2[,i])
km2$TreeShrubSwamp<-km2$TreedSwamp+km2$ShrubbySwamp
km2$NonTreeFenMarsh<-km2$ShrubbyFen+km2$GraminoidFen+km2$Marsh
# Don't have to make combined types for broader veg groupings, because model predictions made for each fine type, but do need to collapse oldest age classes since age class 9 is not modeled
km2$Spruce8<-km2$Spruce8+km2$Spruce9
km2$Pine8<-km2$Pine8+km2$Pine9
km2$Decid8<-km2$Decid8+km2$Decid9
km2$Mixedwood8<-km2$Mixedwood8+km2$Mixedwood9
km2$TreedBog8<-km2$TreedBog8+km2$TreedBog9
km2$TreedFen8<-km2$TreedFen8+km2$TreedFen9
km2$Spruce9<-km2$Pine9<-km2$Decid9<-km2$Mixedwood9<-km2$TreedBog9<-km2$TreedFen9<-NULL  # Get rid of those old classes to avoid confusion

# Group HF types
HFgl<-read.csv(HFgroupfile)  # HF group lookup
HFgroups<-unique(HFgl$UseInAnalysis)
for (i in HFgroups) {  # For each HF group used in analysis, figure out which finer HF types are included and sum the proportions for those
  HFtypesingroup<-colnames(km2)[na.omit(match(HFgl$HF_GROUP[HFgl$UseInAnalysis==i],colnames(km2)))]
  if (i=="HFor") HFtypesingroup<-c(HFtypesingroup,names(km2)[substr(names(km2),1,2)=="CC"])  # Add the individual cutblock types to HFor group.  Assumes all forestry types start with "CC"
  if (length(HFtypesingroup)>1) {
    km2<-cbind(km2,rowSums(km2[,HFtypesingroup])) # Add a column for each HF group, summing the component HF types
  } else {
    km2<-cbind(km2,km2[,HFtypesingroup])  # If there is only one HF type in that HF group, just add it
  }
}
names(km2)<-c(names(km2)[1:(ncol(km2)-length(HFgroups))],as.character(HFgroups))  # and give the newly created HF groups their proper names

# Add WetlandMargin type (always =0, because doesn't currently exist in GIS).  Needed just to make 1km2 predictions work in later steps
km2$WetlandMargin<-0

# And do the same to reference km2 grid file
# Convert veg_reference to data.frame
veg_reference<-data.frame(as.matrix(veg_reference))  # Convert to data.frame
km2.area<-rowSums(veg_reference)
for (i in seq(1,nrow(veg_reference),10000)) {  # Convert to proportions in chunks, otherwise memory fails
  print(paste(i,nrow(veg_reference),date()))
  j<-ifelse(i+9999>nrow(veg_reference),nrow(veg_reference),i+9999)
  veg_reference[i:j,]<-veg_reference[i:j,]/km2.area[i:j]
}
veg_reference$LinkID<-rownames(veg_reference)

# Add in info about km2 rasters - lat, long, natural region and subregion, LUF
km2.b<-merge(veg_reference,kgrid,by="LinkID")  # Check for data loss - dimensions of km2.b should be the same as veg_reference
rm(veg_reference,kgrid)
gc()

# Reduce to just km2 rasters in the North plus parkland to allow feathering with results from South based on pAspen
km2.b<-km2.b[km2.b$NR!="Grassland",]  # Everything except grassland currently used.

# Combine categories in km2 gird data to match available coefficients
# Note: Using this year's names here, so need to change all subsequent scripts (e.g., "Decid" instead of "Deciduous", etc)
i<-which(substr(colnames(km2.b),1,10)=="TreedSwamp")  # No age class modeling for these types
km2.b$TreedSwamp<-rowSums(km2.b[,i])
i<-which(substr(colnames(km2.b),1,8)=="TreedFen")  # No age class modeling for these types
km2.b$TreedFen<-rowSums(km2.b[,i])
km2.b$TreeShrubSwamp<-km2.b$TreedSwamp+km2.b$ShrubbySwamp
km2.b$NonTreeFenMarsh<-km2.b$ShrubbyFen+km2.b$GraminoidFen+km2.b$Marsh
# Don't have to make combined types, because model predictions made for each fine type, but do need to collapse oldest age classes
km2.b$Spruce8<-km2.b$Spruce8+km2.b$Spruce9
km2.b$Pine8<-km2.b$Pine8+km2.b$Pine9
km2.b$Decid8<-km2.b$Decid8+km2.b$Decid9
km2.b$Mixedwood8<-km2.b$Mixedwood8+km2.b$Mixedwood9
km2.b$TreedBog8<-km2.b$TreedBog8+km2.b$TreedBog9
km2.b$TreedFen8<-km2.b$TreedFen8+km2.b$TreedFen9
km2.b$Spruce9<-km2.b$Pine9<-km2.b$Decid9<-km2.b$Mixedwood9<-km2.b$TreedBog9<-km2.b$TreedFen9<-NULL  # Get rid of those old classes to avoid confusion

# Don't need to group HF types, because no HF in reference map

# Add WetlandMargin type (always =0, because doesn't exist in GIS)
km2.b$WetlandMargin<-0

max(apply(km2[,2:122],2,max))  # Make sure this = 1.  If it is >1, then some type(s) have been double-counted somewhere.  Need to fix that here, otherwise bad things happen later!

# Extra variables to use NSR groupings in modeling
km2$NSR1Parkland<-km2$NSR1DryMixedwood<-km2$NSR1CentralMixedwood<-km2$NSR1Foothills<-km2$NSR1North<-km2$NSR1Shield<-km2$NSR1Mountain<-0

i<-which(km2$NSR=="Central Parkland" | km2$NSR=="Foothills Parkland" | km2$NSR=="Peace River Parkland")
km2$NSR1Parkland[i]<-1

i<-which(km2$NSR=="Dry Mixedwood")
km2$NSR1DryMixedwood[i]<-1

i<-which(km2$NSR=="Central Mixedwood")
km2$NSR1CentralMixedwood[i]<-1

i<-which(km2$NSR=="Lower Foothills" | km2$NSR=="Upper Foothills")
km2$NSR1Foothills[i]<-1

i<-which(km2$NSR=="Lower Boreal Highlands" | km2$NSR=="Upper Boreal Highlands" | km2$NSR=="Boreal Subarctic" | km2$NSR=="Northern Mixedwood")
km2$NSR1North[i]<-1

i<-which(km2$NSR=="Kazan Uplands" | km2$NSR=="Peace-Athabasca Delta" | km2$NSR=="Athabasca Plain")
km2$NSR1Shield[i]<-1

i<-which(km2$NSR=="Montane" | km2$NSR=="Subalpine" | km2$NSR=="Alpine")
km2$NSR1Mountain[i]<-1

# Then back filled
km2.b$NSR1Parkland<-km2.b$NSR1DryMixedwood<-km2.b$NSR1CentralMixedwood<-km2.b$NSR1Foothills<-km2.b$NSR1North<-km2.b$NSR1Shield<-km2.b$NSR1Mountain<-0
i<-which(km2.b$NSR=="Central Parkland" | km2.b$NSR=="Foothills Parkland" | km2.b$NSR=="Peace River Parkland")
km2.b$NSR1Parkland[i]<-1
i<-which(km2.b$NSR=="Dry Mixedwood")
km2.b$NSR1DryMixedwood[i]<-1
i<-which(km2.b$NSR=="Central Mixedwood")
km2.b$NSR1CentralMixedwood[i]<-1
i<-which(km2.b$NSR=="Lower Foothills" | km2.b$NSR=="Upper Foothills")
km2.b$NSR1Foothills[i]<-1
i<-which(km2.b$NSR=="Lower Boreal Highlands" | km2.b$NSR=="Upper Boreal Highlands" | km2.b$NSR=="Boreal Subarctic" | km2.b$NSR=="Northern Mixedwood")
km2.b$NSR1North[i]<-1
i<-which(km2.b$NSR=="Kazan Uplands" | km2.b$NSR=="Peace-Athabasca Delta" | km2.b$NSR=="Athabasca Plain")
km2.b$NSR1Shield[i]<-1
i<-which(km2.b$NSR=="Montane" | km2.b$NSR=="Subalpine" | km2.b$NSR=="Alpine")
km2.b$NSR1Mountain[i]<-1

save(file=km2file.out,km2,km2.b)

