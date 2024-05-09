#-----------------------------------------------------------------------------------------------------------------------

# Project(s):       ABMI EH, ABMI OG, CMU, BG, NWSAR

# Title:            Coefficient Analysis (North) - Basic Models and Figures
# Description:
# Author:           Marcus Becker, David J. Huggard

# Previous scripts: 01, 02, 03

#-----------------------------------------------------------------------------------------------------------------------

library(mgcv)  # For binomial GAM
library(mapproj)  # For projected maps
library(binom)  # For exact binomial confidence intervals (not currently being used)
library(pROC)  # For AUC of ROC
library(MuMIn) # For second order AIC (i.e., AICc)

# Root directory for 2022 analysis north results (Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/Results/Habitat Models/2022/analysis-north/"

# Set up file names for various outputs
fname.fig<- paste0(g_drive, "figures/best-model/")  # Start of figure file, one per species for veg+HF.  Subdirectory, species name and extension will be added
fname.map <- paste0(g_drive, "maps/")  # Start of map file, one per species for, spatial+climate map, current, reference and difference maps.  Subdirectory, species name and extension will be added.  Note: the web uses only whole-province maps, so naming here can be non-official
fname.sumout <- paste0(g_drive, "coefficient-tables/Mammal coefficients North 2022 Best model")  # For exporting coefficient tables
fname.Robjects <- paste0(g_drive, "R-objects/R objects North mammal coefficients 2022 Best model")  # Start of file name to save each species' models
fname.Robjects.sum <- paste0(g_drive, "R-objects/R objects North mammals Coefficient tables 2022 Best model.Rdata")
fname.km2summaries <- paste0(g_drive, "km2-summaries/best-model/Km2 North reference and current 2022") # Start of file name for km2 grid reference and current output for each species
fname.useavail <- paste0(g_drive, "figures/use-availability/")  # Sub-directory for use-availability figures and table

# NEW kgrid with new climate info
km2file.out <- "G:/Shared drives/ABMI Camera Mammals/data/processed/km2-grid/km2-grid-north_current-backfilled_processed_2024-05-09.Rdata"    # File with processed km2 grid files - processed in separate script.  Despite the variable name, this is an input here.
fname.150mVegModels <- paste0(g_drive, "R objects/Veg 150m best model")  # Start of file name to save best model using 150m veg as well as that model combined with best space/climate model (for separate graphing)

# File name for previously processed datafile+veg+HF, km2 grid info, SpTable, and look-up matrix for veg groups
dataset.out<-"G:/Shared drives/ABMI Camera Mammals/data/lookup/R Dataset SpTable for ABMI North mammal coefficients 2022.RData"
load(dataset.out)
SpTable.w<-SpTable.w[SpTable.w!="black_bearWinter"]  # Bears have enough occurrences in winter to meet criterion, but they are just flukey encounters - don't run winter models
SpTable<-sort(unique(c(gsub("Summer","",SpTable.s),gsub("Winter","",SpTable.w))))  # All modeled species, summer and/or winter
SpTable.ua<-sort(unique(c(gsub("Summer","",SpTable.s.ua),gsub("Winter","",SpTable.w.ua))))  # And the larger list for use-availability

#-----------------------------------------------------------------------------------------------------------------------

# Let's add the new Climate variables

load("S:/samba/abmisc/AB_data_v2023/sites/processed/climate/abmi-camera-climate_2023.Rdata")

d_new <- d |>
  select(location_project, project, location, NearestSite) |>
  mutate(year = as.character(str_extract_all(project, "\\d+")))

new_climate <- rownames_to_column(camera.climate) |>
  separate(rowname, into = c("location", "year"), sep = "_", remove = TRUE) |>
  left_join(d_new, by = c("location", "year")) |>
  select(location_project, project, location, NearestSite, year, everything()) |>
  # Lot of NAs - mostly from 2022 and 2023 projects, which aren't in the density data (yet). To look into!
  # Note: No CMU, NWSAR ...
  filter(!is.na(project)) |>
  select(-year)

# Re-load the dataset
load(dataset.out)

# Let's join these new climate variables to d
d <- d |>
  select(-c(AHM, FFP, MAP, MAT, MCMT, MWMT)) |>
  left_join(new_climate, by = c("location_project", "project", "location", "NearestSite")) |>
  filter(!is.na(bio9))

# We lost a lot of data here (no CMU) ... will figure that out later.

# Specify space/climate ("sc") model options for each species in SpTable  1=based on total abundance, 2=based on presence/absence only, 3=none.  Based on results of cross-validation at site and regional level.
#sc.option<-c(1,2,3,2,2,2,3,3,3,3,1,1,3,3,2,2,2,3,2,2,3,3)  # These are the decisions from the 2019, for the record - overruled in next line

# New SpTable
SpTable <- c("Moose", "Coyote", "White-tailed Deer", "Woodland Caribou", "Canada Lynx")

sc.option<-rep(2,length(SpTable))  # Currently running all with sc based only on presence/absence, due to problems in AGP-based models
data.frame(SpTable,sc.option)  # Check that these are in right order (if different options are being used for different species!)

# Get nice species names
taxa.file<-read.csv("G:/Shared drives/ABMI Camera Mammals/data/lookup/Mammal taxa.csv")  # To get proper species names for figures, tables
sp.names<-as.character(taxa.file$Species[match(SpTable,taxa.file$Label)]) # These are nice names, not the condensed R names
sp.names.ua<-as.character(taxa.file$Species[match(SpTable.ua,taxa.file$Label)])  # For all species, including ones with only use/availablity figures
SeasName<-c("Summer","Winter")  # For labeling figures, etc.

# Figure parameters set up based on any possible variable in the veg+HF models
fp<-read.csv("G:/Shared drives/ABMI Camera Mammals/data/lookup/Veg HF plot lookup.csv")  # Figure parameters, set up for all possible variables.  This has to be revised when different veg or HF groupings are added.
fp$col1<-as.character(fp$col1)

# Load processed km2 file
load(km2file.out)
kgrid_veg_current_north$Intercept<-1
kgrid_veg_reference_north$Intercept<-1

# Rename to Dave's name
km2 <- kgrid_veg_current_north
km2.b <- kgrid_veg_reference_north
rm(kgrid_veg_current_north, kgrid_veg_reference_north)
# Water-dominated km2 rasters to add to maps km2.water
load("G:/Shared drives/ABMI Camera Mammals/data/processed/km2-grid/R object water dominated km2.rdata")

# Set up km2 prediction matrices for space/climate mapping
# km2.sc is for climate+space effects only - for diagnostic mapping of these effects.
# Needs to include all variables used in the sc models.
# The MAT2 transformation avoids problems where MAT<0.  Keep telling this to other people using the models, because they will forget every time.
# Removed PET
km2.sc<-data.frame(km2[,c("Intercept","Lat","Long","TrueLat", "Easting", "Northing",
                          "AHM","FFP","MAP","MAT","MCMT","MWMT", "TD", "MSP", "SHM", "DD_0", "DD5", "DD_18", "NFFD", "bFFP", "eFFP",
                          "PAS", "EMT", "EXT", "Eref", "CMD", "RH", "CMI", "DD1040", "bio15", "HTV",
                          "NSR1Parkland","NSR1DryMixedwood","NSR1CentralMixedwood","NSR1Foothills","NSR1North","NSR1Shield","NSR1Mountain")],
                   # Not really sure that these are needed, but will keep anyway.
                   Lat2=km2$Lat^2,
                   Lat3=km2$Lat^3,
                   Long2=km2$Long^2,
                   LatLong=km2$Lat*km2$Long,
                   Lat2Long2=km2$Lat^2*km2$Long^2,
                   # Add the Easting/Northing ones
                   EN = km2$Easting * km2$Northing,
                   E2 = km2$Easting^2,
                   N2 = km2$Northing^2,
                   LongMAT=km2$Long*km2$MAT,
                   #MAPPET=km2$MAP*km2$PET,
                   MATAHM=km2$MAT*km2$AHM,
                   MAPFFP=km2$MAP*km2$FFP,
                   MAT2=km2$MAT*(km2$MAT+10),
                   MWMT2=km2$MWMT^2)

# Means, used to plot residual relationships (including "average" NSR group)
DF.res.null<-data.frame(Intercept=1,
                        Lat=mean(d$Lat),
                        Long=mean(d$Long),
                        AHM=mean(d$AHM),
                        FFP=mean(d$FFP),
                        MAP=mean(d$MAP),
                        MAT=mean(d$MAT),
                        MCMT=mean(d$MCMT),
                        MWMT=mean(d$MWMT),
                        Lat2=mean(d$Lat)^2,
                        Long=mean(d$Long)^2,
                        LatLong=mean(d$Lat)*mean(d$Long),
                        TD = mean(d$TD),
                        MSP = mean(d$MSP),
                        SHM = mean(d$SHM),
                        DD_0 = mean(d$DD_0),
                        DD5 = mean(d$DD5),
                        DD_18 = mean(d$DD_18),
                        NFFD = mean(d$NFFD),
                        bFFP = mean(d$bFFP),
                        eFFP = mean(d$eFFP),
                        PAS = mean(d$PAS),
                        EMT = mean(d$EMT),
                        EXT = mean(d$EXT),
                        Eref = mean(d$Eref),
                        CMD = mean(d$CMD),
                        RH = mean(d$RH),
                        CMI = mean(d$CMI),
                        DD1040 = mean(d$DD1040),
                        bio15 = mean(d$bio15),
                        HTV = mean(d$HTV),
                        MATAHM=mean(d$MAT)*mean(d$AHM),
                        MAPFFP=mean(d$MAP)*mean(d$FFP),
                        MAT2=mean(d$MAT*(d$MAT+10)),
                        MWMT2=mean(d$MWMT)^2,
                        NSR1Parkland=1/7,
                        NSR1DryMixedwood=1/7,
                        NSR1CentralMixedwood=1/7,
                        NSR1Foothills=1/7,
                        NSR1North=1/7,
                        NSR1Shield=1/7,
                        NSR1Mountain=1/7)

# Truncate mapped area to just approximate polygon around sampled points.  Polygon determined by trial and error from map.  Needs to be changed every year
jpeg(file="G:/Shared drives/ABMI Camera Mammals/Results/Habitat Models/2022/analysis-north/maps/Spatial projection area NORTH.jpg",height=600,width=400)
plot(km2$Long,km2$TrueLat,pch=16,cex=0.3)  # Northern region
points(d$Long,d$TrueLat,pch=16,cex=1,col="red")  # Sampled points
poly.long<-c(-120.1,-120.1,-115,-112.5,-112.8,-120.1,-120.1)  # These are the longitudes of the polygon that bounds the area to be excluded from mapping - 2020 version
poly.lat<-c(52,56.5,58.2,58.5,60.1,60.1,52)  # And these are the corresponding latitudes
pip<-in.out(matrix(c(poly.long,poly.lat),c(length(poly.long),2)),matrix(c(km2$Long,km2$TrueLat),c(nrow(km2),2)))
# No spatial mask is being used here, with the (few) locations in the NW now.  Check how sensible the models look for those areas
#	points(km2$Long[pip==TRUE],km2$TrueLat[pip==TRUE],col="grey80",pch=16,cex=0.3)  # Mask the area out of the plottable region with grey
#	points(d$Long,d$TrueLat,pch=16,cex=1,col="red2")  # Sampled points replotted on top
graphics.off()
#	km2.sc<-km2.sc[pip==FALSE,]  # Clip out the non-plottable region in the rasters used for space/climate model checking
km2.1<-km2  # Version to use if no non-plottable region
km2.b.1<-km2.b  # Version to use if no non-plottable region
#	km2.1<-km2[pip==FALSE,]  # And in the complete raster file of current veg+HF
#	km2.b.1<-km2.b[pip==FALSE,]  # And in the backfilled veg raster file

# Set up information for mapping - converts things to be mapped to Albers map projection co-ordinates
city.y<-c(51,53,49,56,58,55)+c(3,33,42,44,31,10)/60
city.x<- -c(114,113,112,111,117,118)-c(5,30,49,23,8,48)/60
city<-c("Calgary","Edmonton","Lethbridge","Fort McMurray","High Level","Grande Prairie")
m<-mapproject(c(km2.sc$Long,city.x,km2.water$Long),c(km2.sc$TrueLat,city.y,km2.water$Lat),projection="albers",par=c(49,60))  # These have to be projected together, otherwise get slightly different results, don't know why
km2.sc$proj.x<-m$x[1:nrow(km2.sc)]  # Projected x-value for km2 raster longitudes
km2.sc$proj.y<-m$y[1:nrow(km2.sc)]  # Projected y-value for km2 raster latitudes
m.city.x<-m$x[(nrow(km2.sc)+1):(nrow(km2.sc)+length(city))]  # Projected x for cities locations
m.city.y<-m$y[(nrow(km2.sc)+1):(nrow(km2.sc)+length(city))]  # Projected y for cities locations
m.water<-data.frame(x=m$x[(nrow(km2.sc)+length(city)+1):length(m$x)],y=m$y[(nrow(km2.sc)+length(city)+1):length(m$y)])
m.title<-mapproject(-115,60.5,projection="albers",par=c(49,60))  # Just coordinates for the map title
c1<-rev(c("#D73027","#FC8D59","#FEE090","#E0F3F8","#91BFDB","#4575B4"))  # Colour gradient for reference and current
c2<-colorRampPalette(c1, space = "rgb") # Function to interpolate among these colours for reference and current
d1<-c("#C51B7D","#E9A3C9","#FDE0EF","#E6F5D0","#A1D76A","#4D9221")  # Colour gradient for difference map
dcol2<-colorRampPalette(d1, space = "rgb") # Function to interpolate among these colours for difference map

#-----------------------------------------------------------------------------------------------------------------------

# Set up variables to store results

# Variables to use for the reference predictions
vnames.b<-c("SpruceR","Spruce1","Spruce2","Spruce3","Spruce4","Spruce5","Spruce6","Spruce7","Spruce8","PineR","Pine1","Pine2","Pine3","Pine4","Pine5","Pine6","Pine7","Pine8","DecidR","Decid1","Decid2","Decid3","Decid4","Decid5","Decid6","Decid7","Decid8",
            "MixedwoodR","Mixedwood1","Mixedwood2","Mixedwood3","Mixedwood4","Mixedwood5","Mixedwood6","Mixedwood7","Mixedwood8","TreedBogR","TreedBog1","TreedBog2","TreedBog3","TreedBog4","TreedBog5","TreedBog6","TreedBog7","TreedBog8",
            "TreedFen","TreedSwamp","Shrub","GrassHerb","GraminoidFen","Marsh","ShrubbyBog","ShrubbyFen","ShrubbySwamp","WetlandMargin")  # Variables to use in reference predictions

# .pa used for presence/absence components, .agp for abundance-given-presence, .sc for space/climate, .s for summer, .w for winter
vnames.pa<-c("SpruceR","Spruce1","Spruce2","Spruce3","Spruce4","Spruce5","Spruce6","Spruce7","Spruce8","PineR","Pine1","Pine2","Pine3","Pine4","Pine5","Pine6","Pine7","Pine8","DecidR","Decid1","Decid2","Decid3","Decid4","Decid5","Decid6","Decid7","Decid8",
             "MixedwoodR","Mixedwood1","Mixedwood2","Mixedwood3","Mixedwood4","Mixedwood5","Mixedwood6","Mixedwood7","Mixedwood8","TreedBogR","TreedBog1","TreedBog2","TreedBog3","TreedBog4","TreedBog5","TreedBog6","TreedBog7","TreedBog8",
             "TreedFen","TreedSwamp","Shrub","GrassHerb","GraminoidFen","Marsh","ShrubbyBog","ShrubbyFen","ShrubbySwamp",
             "CCSpruceR","CCSpruce1","CCSpruce2","CCSpruce3","CCSpruce4","CCPineR","CCPine1","CCPine2","CCPine3","CCPine4","CCDecidR","CCDecid1","CCDecid2","CCDecid3","CCDecid4","CCMixedwoodR","CCMixedwood1","CCMixedwood2","CCMixedwood3","CCMixedwood4",
             "Crop","TameP","RoughP","Urban","Rural","Industrial","Well","EnSoftLin","EnSeismic","TrSoftLin","HardLin","WetlandMargin")  # Names of coefficients

# Set up tables to store coefficients and their SE's - Season, species, variable
Coef.pa<-Coef.pa.se<-array(0,c(2,length(SpTable),length(vnames.pa)))
# Extra matrix for pooled coefficients for each species for 5 stand types that have ages estimated - Season, species, variable
PooledStandCoef.pa<-PooledStandCoef.pa.se<-array(0,c(2,length(SpTable),5))
dimnames(Coef.pa)<-dimnames(Coef.pa.se)<-list(c("Summer","Winter"),SpTable,vnames.pa)
dimnames(PooledStandCoef.pa)<-dimnames(PooledStandCoef.pa.se)<-list(c("Summer","Winter"),SpTable,c("Decid","Mixedwood","Pine","Spruce","TreedBog"))

# Abundance-given-presence - names of coefficients.
vnames.agp<-c("Spruce","Pine","Decid","Mixedwood","TreedBog",
              "TreedFen","TreedSwamp","Shrub","GrassHerb","GraminoidFen","Marsh","ShrubbyBog","ShrubbyFen","ShrubbySwamp",
              "CCSpruceR","CCSpruce1","CCSpruce2","CCSpruce3","CCSpruce4","CCPineR","CCPine1","CCPine2","CCPine3","CCPine4","CCDecidR","CCDecid1","CCDecid2","CCDecid3","CCDecid4","CCMixedwoodR","CCMixedwood1","CCMixedwood2","CCMixedwood3","CCMixedwood4",
              "Crop","TameP","RoughP","Urban","Rural","Industrial","Well","EnSoftLin","EnSeismic","TrSoftLin","HardLin","WetlandMargin")
Coef.agp<-Coef.agp.se<-array(0,c(2,length(SpTable),length(vnames.agp)))  # Set up tables to store coefficients and their SE's - Season, species, variable
dimnames(Coef.agp)<-dimnames(Coef.agp.se)<-list(c("Summer","Winter"),SpTable,vnames.agp)
Coef.mean<-Coef.lci<-Coef.uci<-array(0,c(2,length(SpTable),length(vnames.pa)))  # To store combined presence * abundance|presence estimates and their CI's (CI's because not symmetrical SE's) - Season, species, variable
dimnames(Coef.mean)<-dimnames(Coef.lci)<-dimnames(Coef.uci)<-list(c("Summer","Winter"),SpTable,vnames.pa)
Coef.pa.all<-Coef.pa.lci.all<-Coef.pa.uci.all<-Coef.mean.all<-Coef.lci.all<-Coef.uci.all<-array(0,c(length(SpTable),length(vnames.pa)))  # To store combined presence * abundance|presence estimates and their CI's (CI's because not symmetrical SE's) - average of Summer and Winter
colnames(Coef.pa.all)<-colnames(Coef.pa.lci.all)<-colnames(Coef.pa.uci.all)<-colnames(Coef.mean.all)<-colnames(Coef.lci.all)<-colnames(Coef.uci.all)<-vnames.pa
rownames(Coef.pa.all)<-rownames(Coef.pa.lci.all)<-rownames(Coef.pa.uci.all)<-rownames(Coef.mean.all)<-rownames(Coef.lci.all)<-rownames(Coef.uci.all)<-SpTable

# Space-climate
# Note: Not sure what to do with this.
vnames.sc<-c("Intercept","Lat","Long","LatLong","Lat2","Lat3","Long2","Lat2Long2",
             "MAT", "FFP", "TD", "MAT", "FFP", "PAS", "CMD", "AHM", "SHM", "Eref", "MAP",
             "Easting", "Northing", "E2", "N2", "EN",
             "MATAHM","MWMT","MCMT","MWMT2","MAT2","LongMAT",
             "NSR1Parkland","NSR1DryMixedwood","NSR1CentralMixedwood","NSR1Foothills","NSR1North","NSR1Shield","NSR1Mountain")

Res.coef<-array(0,c(length(SpTable),length(vnames.sc))) # Done for both seasons together
colnames(Res.coef)<-vnames.sc
rownames(Res.coef)<-SpTable
# To store lure coefficients for pres/abs, abundance | presence - Season, species
lure.pa<-lure.agp<-array(NA,c(2,length(SpTable)))
# Set the cut-off for the minimum proportion of a stand type for a site to be included in the age analysis for a stand type
# (currently the same for each stand type - could be flexible).
# Note: weighting is proportional to proportion of site, so low values can be used - those sites just won't contribute much.
# Not that any of this matters with point veg+HF; I just kept it from my scripts for other taxa to accommodate any future modeling using areas surrounding cameras ...
cutoff.pc.for.age<-0.1

#-----------------------------------------------------------------------------------------------------------------------

# Arrays to save aic weights for each age model, and for which sets of models are best.

# AIC for age models using 5 stand separately, 2 pairs + black spruce, all stands together - Season, species, age combination model
aic.age<-array(NA,c(2,length(SpTable),3))

# To save aic weights for each species, presence/absence - Season, species, model.
# Need to change the third dimension if number of models changes
aic.wt.pa.save<-array(NA,c(2,length(SpTable),17))

# To save aic weights for each species, abundance|presence.  Include null. - Season, species, model.
# Need to change the third dimension if number of models changes
aic.wt.agp.save<-array(NA,c(2,length(SpTable),18))

dimnames(aic.wt.pa.save)[[1]]<-dimnames(aic.wt.agp.save)[[1]]<-c("Summer","Winter")
dimnames(aic.wt.pa.save)[[2]]<-dimnames(aic.wt.agp.save)[[2]]<-SpTable
# This saves the AIC weights for the 3 combining age models for each species. - Season, species, model
aic.wt.age.save<-array(NA,c(2,length(SpTable),3))
# This saves the AIC weights for the spline model (relative to the null) for each of the eight stand types or combinations - Season, species, model
aic.wt.age.models.save<-array(NA,c(2,length(SpTable),8))
dimnames(aic.wt.age.save)[[1]]<-dimnames(aic.wt.age.models.save)[[1]]<-c("Summer","Winter")
dimnames(aic.wt.age.save)[[2]]<-dimnames(aic.wt.age.models.save)[[2]]<-SpTable
dimnames(aic.wt.age.save)[[3]]<-c("Separate","Intermediate","Combined")
dimnames(aic.wt.age.models.save)[[3]]<-c("Spruce","Pine","Decid","Mixedwood","TreedBog","UpCon","DecidMixed","All")
auc.fit<-NULL  # AUC for the presence/absence fit, one value for each species

# Extra list of variables for direct summary of densities by veg+HF class.  Need to eliminate un-sampled types.
# This summary is just meant as a test of the reasonableness of the fitted coefficients.
vnames.sum<-vnames.pa[vnames.pa %in% names(d)]
n.sum<-colSums(sign(d[,vnames.sum]))
vnames.sum<-vnames.sum[which(colSums(d[,vnames.sum])>0)]  # A couple veg+HF types have columns, but no samples in used dataset
n.sum<-colSums(sign(d[,vnames.sum]))
which(colSums(sign(d[d$SummerDays>=10,vnames.sum]))==0)  # Check if any of these veg+HF types were not sampled in Summer
which(colSums(sign(d[d$WinterDays>=10,vnames.sum]))==0)  # Check if any of these veg+HF types were not sampled in winter
simple.sum.mean<-simple.sum.n<-simple.sum.se<-array(NA,c(2,length(SpTable),length(vnames.sum)))  # Season, species, veg+HF type
dimnames(simple.sum.mean)<-dimnames(simple.sum.n)<-dimnames(simple.sum.se)<-list(c("Summer","Winter"),SpTable,vnames.sum)

# Setup variables to save BIC and variance components for SC and 150m models
# Not included for now.

#bic.sc150m<-array(NA,c(length(SpTable),4))  # BIC for {null, best sc model, best 150m veg model, combo model}
#var.sc150m<-array(NA,c(length(SpTable),5))  # Residual variance for {complete null with no offsets, null with offsets, best sc model, best 150m veg model, combo model}
#best.model.sc150m<-array(NA,c(length(SpTable),2))  # Store best model for {sc, 150m veg}
#rownames(bic.sc150m)<-rownames(var.sc150m)<-rownames(best.model.sc150m)<-SpTable
#colnames(bic.sc150m)<-c("NullOffsets","Best.SC","Best.150m","Combo.SC150m")
#colnames(var.sc150m)<-c("Null","NullOffsets","Best.SC","Best.150m","Combo.SC150m")
#colnames(best.model.sc150m)<-c("Best.SC","Best.150m")

# Add Year back in to datafile
d$Year<-substr(d$project,nchar(d$project)-3,nchar(d$project))
d$Year<-ifelse(d$project=="Northwest Species at Risk Program",2020,d$Year)

#-----------------------------------------------------------------------------------------------------------------------

# Modeling + Figures + Mapping

# This loops through each species in SpTable (22)

# For each species, point veg+HF models are fit for summer and winter (and best-model coefficient figures plotted)
# Then results are combined (and full coefficient figures plotted)
# Then space/climate residual models are fit (and maps are plotted)

for (sp in 1:length(SpTable)) {

  # Set up data.frame to keep counts and predictions for the species for each season
  # To use in SC modeling using residuals after year-round average prediction
  d1<-data.frame(location_project=d$location_project,
                 Lured=d$Lured,
                 count.summer=NA,count.winter=NA,p.pa.summer=NA,p.agp.summer=NA,p.summer=NA,p.pa.winter=NA,p.agp.winter=NA,p.winter=NA)

  # Loop through seasons
  for (seas in 1:2) {  # 1=Summer, 2=Winter

    if (seas==1) {  # Set season-specific variables
      SpSeas<-paste(SpTable[sp],"Summer",sep="")
      SeasDays<-d$SummerDays
      SeasWt<-d$wt.s  # Set up in first script
    }
    if (seas==2) {
      SpSeas<-paste(SpTable[sp],"Winter",sep="")
      SeasDays<-d$WinterDays
      SeasWt<-d$wt.w  # Set up in first script
    }

    if ( (seas==1 & SpSeas %in% SpTable.s==TRUE) | (seas==2 & SpSeas%in%SpTable.w==TRUE) ) {  # Only run if that species is in the species table for that season

      print(paste(sp,length(SpTable),SpTable[sp],SeasName[seas],date()))
      # Extract site descriptors and just the target species.  Assumes species columns are Sp1Summer, Sp1Winter...SpnSummer, SpnWinter
      d.sp<-d[,c(1:(FirstSpCol.s-1),(LastSpCol.w+1):ncol(d),which(colnames(d)==SpSeas))]
      # Change the species count column name to "Count"
      colnames(d.sp)[ncol(d.sp)]="Count"
      # Use locations with sampling in that season and enough days
      i<-which(!is.na(d.sp$Count) & SeasDays>10)
      d.sp<-d.sp[i,]
      SeasDays<-SeasDays[i]
      SeasWt<-SeasWt[i]
      # Calculate lure effect on pres/abs for that species
      d.sp$Lured<-as.character(d.sp$Lured)
      # Only use ongrid sites for lure calibration (paired lure/not).  These are now identified by locations starting with digits
      UseForLure<-grepl("^[[:digit:]]+",d.sp$location)
      q<-by(sign(d.sp$Count[UseForLure==TRUE]),d.sp$Lured[UseForLure==TRUE],mean)
      lure.pa[seas,sp]<-q["Yes"]/q["No"]

      # 0. Simple summary of density in each veg+HF type that has been sampled

      # Lure effect on total abundance
      lure1<-mean(d.sp$Count[UseForLure==TRUE & d.sp$Lured=="Yes"])/mean(d.sp$Count[UseForLure==TRUE & d.sp$Lured=="No"])
      for (i in 1:length(vnames.sum)) {
        x<-d.sp$Count[d.sp[,vnames.sum[i]]==1]
        l<-d.sp$Lured[d.sp[,vnames.sum[i]]==1]
        x<-ifelse(l=="Yes",x/lure1,x)
        simple.sum.mean[seas,sp,i]<-mean(x)
        simple.sum.n[seas,sp,i]<-sum(sign(x))
        if (n.sum[i]>1) simple.sum.se[seas,sp,i]<-sd(x)/sqrt(n.sum[sp])
      }

      # 1. Fit models to broad veg types and HF types

      # 1.1 Fit point veg+HF models for ** Presence/Absence ** component
      m.pa<-list(NULL)
      pCount<-sign(d.sp$Count)/ifelse(d.sp$Lured=="Yes",lure.pa[seas,sp],1)  # Standardize all to no-lure
      pCount<-pCount/max(pCount)  # This is in case the lure effect is <1
      d.sp$pCount.pa<-pCount  # Used in the age models below
      m.pa[[1]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedBog+TreedFen+TreedSwamp+GrassHerb+Shrub+Marsh+ShrubbySwamp+ShrubbyBogFen+CCDecidR+CCDecid1+CCDecid2+CCMixedwoodR+CCMixedwood1+CCMixedwood2+CCPineR+CCPine1+CCSpruceR+CCSpruce1+CCSpruce2+EnSoftLin+EnSeismic+TrSoftLin+TameP+RoughP+Well+RurUrbInd+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is crop. No UrbInd sampled
      m.pa[[2]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedBog+TreedFen+TreedSwamp+GrassHerb+Shrub+Marsh+ShrubbySwamp+ShrubbyBogFen+CCDecidMixed+CCPine+CCSpruce+EnSoftLin+EnSeismic+TrSoftLin+TameP+RoughP+Well+RurUrbInd+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is crop. No UrbInd sampled
      m.pa[[3]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedBog+TreedFen+TreedSwamp+GrassHerb+Shrub+Marsh+ShrubbySwamp+ShrubbyBogFen+CCDecidMixed+CCPine+CCSpruce+EnSoftLin+EnSeismic+TrSoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[4]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedBog+TreedFen+TreedSwamp+GrassHerb+Shrub+Marsh+ShrubbySwamp+ShrubbyBogFen+CCDecidMixed+CCPine+CCSpruce+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[5]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedBog+TreedFen+TreedSwamp+GrassHerb+Shrub+Marsh+ShrubbySwamp+ShrubbyBogFen+CCAll+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien.  Weak data for cutblocks
      m.pa[[6]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedBogFen+TreedSwamp+GrassHerb+Shrub+OpenWet+CCAll+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien.
      m.pa[[7]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedWet+GrassShrub+OpenWet+CCDecidR+CCDecid1+CCDecid2+CCMixedwoodR+CCMixedwood1+CCMixedwood2+CCPineR+CCPine1+CCSpruceR+CCSpruce1+CCSpruce2+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[8]]<-try(glm(pCount~Decid+Mixedwood+Pine+Spruce+TreedWet+GrassShrub+OpenWet+CCDecidMixed+CCConif+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[9]]<-try(glm(pCount~DecidMixed+UpCon+TreedWet+GrassShrub+OpenWet+CCDecidMixed+CCConif+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[10]]<-try(glm(pCount~DecidMixed+UpCon+TreedWet+GrassShrub+OpenWet+CCAll+SoftLin+TameP+RoughP+Well+RurUrbInd+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is crop
      m.pa[[11]]<-try(glm(pCount~DecidMixed+UpCon+TreedWet+GrassShrub+OpenWet+CCAll+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[12]]<-try(glm(pCount~DecidMixed+UpCon+TreedWet+GrassShrub+OpenWet+Succ+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[13]]<-try(glm(pCount~Upland+Lowland+CCAll+SoftLin+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[14]]<-try(glm(pCount~TreedAll+OpenAll+Succ+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[15]]<-try(glm(pCount~UplandForest+Lowland+GrassShrub+Succ+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[16]]<-try(glm(pCount~Upland+Lowland+Succ+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # Intercept is alien
      m.pa[[17]]<-try(glm(pCount~Boreal+GrassShrub+Succ+WetlandMargin+SeasDays,family="binomial",data=d.sp,weights=SeasWt))  # "Boreal" is everything native except GrassShrub. Intercept is alien

      # AIC calculation  (I'm using AIC here, because this is primarily for prediction, rather than finding a minimal best model)
      nModels.pa<-length(m.pa)
      aic.ta<-rep(999999999,(nModels.pa))
      for (i in 1:(nModels.pa)) {
        if (!is.null(m.pa[[i]]) & class(m.pa[[i]])[1]!="try-error") {  # last part is to not used non-converged models, unless none converged
          aic.ta[i]<-AICc(m.pa[[i]])
        }
      }
      aic.delta<-aic.ta-min(aic.ta)
      aic.exp<-exp(-1/2*aic.delta)
      aic.wt.pa<-aic.exp/sum(aic.exp)
      best.model.pa<-which.max(aic.wt.pa)
      aic.wt.pa.save[seas,sp,]<-aic.wt.pa

      # 1.2 Then do ** Abundance | Presence ** model

      d.p<-d.sp[d.sp$Count>0,]  # Just presence records
      d.p$SeasDays<-SeasDays[d.sp$Count>0]
      UseForLure.p<-UseForLure[d.sp$Count>0]  # Adjust abundance for lure effect using only on grid ABMI sites
      q<-by(d.p$Count[UseForLure.p==TRUE],d.p$Lured[UseForLure.p==TRUE],mean)
      lure.agp[seas,sp]<-q["Yes"]/q["No"]
      pCount<-d.p$Count/ifelse(d.p$Lured=="Yes",lure.agp[seas,sp],1)
      j<-0
      m.agp<-list(NULL)
      mnums<-NULL  # To keep track of which models were actually fit

      for (i in 1:nModels.pa) {
        x<-colSums(d.p[,attr(m.pa[[i]]$terms,"term.labels")])  # Minimum number of presence records in each veg+HF type included in the model
        if (min(x)>3 & nrow(d.p)-sum(x[-length(x)])) {  # Only use the model if each type is represented by >3 presence records (incl. intercept)
          j<-j+1
          m.agp[[j]]<-glm(m.pa[[i]]$formula,data=d.p,family=gaussian(link="log"))  # Note: Weights not being used here, because haven't tracked whether each species was present on more than one visit for revisited locations
          mnums<-c(mnums,i)
        }
      }

      m.agp[[j+1]]<-glm(pCount~SeasDays,data=d.p,family=gaussian(link="log"))  # And add a null
      mnums<-c(mnums,nModels.pa+1)  # Add the null model to list - 1 beyond the last pa model

      # AIC calculation  (I'm using AIC here, because this is primarily for prediction, rather than finding a minimal best model)
      nModels.agp<-length(m.agp)
      aic.ta<-rep(999999999,(nModels.agp))
      for (i in 1:(nModels.agp)) {
        if (!is.null(m.agp[[i]]) & class(m.agp[[i]])[1]!="try-error") {  # last part is to not used non-converged models, unless none converged
          aic.ta[i]<-AICc(m.agp[[i]])
        }
      }
      aic.delta<-aic.ta-min(aic.ta)
      aic.exp<-exp(-1/2*aic.delta)
      aic.wt.agp<-aic.exp/sum(aic.exp)
      best.model.agp<-which.max(aic.wt.agp)
      aic.wt.agp.save[seas,sp,mnums]<-aic.wt.agp

      # 1.3 Predict from models for 100% of each veg type and for each site - pres/abs - using only the best model

      Intercept<-c("Crop","Crop",rep("Alien",7),"Crop",rep("Alien",7))[best.model.pa]  # These are the intercepts - the veg+HF type that is not included in the model terms (to avoid collinearity).  Change this if models change
      terms.pa<-c(attr(m.pa[[best.model.pa]]$terms,"term.labels"),Intercept)
      Coef1.pa<-Coef1.pa.se<-rep(NA,length(terms.pa))
      names(Coef1.pa)<-names(Coef1.pa.se)<-terms.pa
      for (i in 1:length(terms.pa)) {
        pm1<-rep(0,length(terms.pa))
        pm1[i]<-1
        names(pm1)<-terms.pa
        pm1<-data.frame(t(pm1))
        p<-predict(m.pa[[best.model.pa]],newdata=data.frame(SeasDays=100,pm1),se.fit=T)
        Coef1.pa[i]<-plogis(p$fit)  # Ordinal scale
        Coef1.pa.se[i]<-p$se.fit  # logit scale
      }
      # Adjust so that mean prediction at standardized SeasDays across all qualifying sites = mean observed count.  This is to compensate for inaccuracies due to fitting SeasDays coefficients
      pCount<-sign(d.sp$Count)/ifelse(d.sp$Lured=="Yes",lure.pa[seas,sp],1)  # Standardize all to no-lure
      pCount<-pCount/max(pCount)  # This is in case the lure effect is <1
      p.adj<-predict(m.pa[[best.model.pa]],newdata=data.frame(SeasDays=100,d.sp))  # Prediction for each data point, but at standardized days
      Coef1.pa<-plogis(qlogis(Coef1.pa)+qlogis(mean(pCount))-qlogis(mean(plogis(p.adj))))  # Adjustment made on logit scale.  Var doesn't change on logit scale(?). The adjustment = qlogis(mean(pCount))-qlogis(mean(plogis(p.adj))) should be small
      # And predict for each site - used for age model below
      d.sp$p<-predict(m.pa[[best.model.pa]])  # Logit scale

      # 1.4 Predict from models for 100% of each veg type and for each site - abund|pres

      terms.pa1<-terms.pa[terms.pa!="SeasDays"]
      Coef1.agp<-Coef1.agp.se<-rep(NA,length(terms.pa1))  # For agp coefficients averaged to terms in best pa model
      names(Coef1.agp)<-names(Coef1.agp.se)<-terms.pa1
      i<-best.model.agp
      p<-predict(m.agp[[i]],newdata=data.frame(pm,SeasDays=100),se.fit=T)  # For each type
      tTypeMean.agp<-p$fit
      tTypeVar.agp<-p$se.fit^2  # Using variance, for consistency with other scripts at this point
      names(tTypeMean.agp)<-names(tTypeVar.agp)<-pm$VegType
      # Adjust so that mean prediction at standardized SeasDays across all qualifying sites = mean observed count.  This is to compensate for inaccuracies due to fitting SeasDays coefficients
      pCount<-d.p$Count/ifelse(d.p$Lured=="Yes",lure.agp[seas,sp],1)
      p.adj<-predict(m.agp[[i]],newdata=data.frame(SeasDays=100,d.sp))  # Prediction for each data point, but at standardized days
      tTypeMean.agp<-log(exp(tTypeMean.agp)+mean(pCount)-mean(exp(p.adj)))  # Var doesn't change on log scale
      # Then average those for each broader group included in the best pa model
      for (i in 1:length(terms.pa1)) {
        j<-pm$VegType[which(pm[,terms.pa1[i]]==1)]  # Names of fine hab+HF types included in that broader group
        x<-tTypeMean.agp[as.character(j)]
        x.var<-tTypeVar.agp[as.character(j)]
        Coef1.agp[i]<-mean(x)  # Simple mean, log scale
        Coef1.agp.se[i]<-sqrt(mean(x.var))  # This is for straight-up mean, log scale
      }
      # Put non-stand-age coefficients in Coef matrix (and SE's)
      Coef.agp[seas,sp,]<-exp(tTypeMean.agp[na.omit(match(dimnames(Coef.agp)[[3]],names(tTypeMean.agp)))])  # On ordinal scale
      Coef.agp.se[seas,sp,]<-sqrt(tTypeVar.agp[na.omit(match(dimnames(Coef.agp)[[3]],names(tTypeMean.agp)))])  # On log scale

      # 2. Run models for age within each stand type - pres/abs only currently

      # 2.1. Set up separate dataframes for sites containing a minimum amount of each broad stand type
      d.spruce<-d.pine<-d.decid<-d.mixed<-d.treedbog<-NULL
      for (i in 0:8) {  # Add sites with each age class of the stand type to a separate data frames for each stand type
        i1<-ifelse(i==0,"R",i)  # For variable name
        i2<-ifelse(i==0,0.5,i)  # For twenty-year age
        cn<-paste("Spruce",i1,sep="")  # Col name
        if(cn %in% names(d.sp)) if (sum(d.sp[,cn]>cutoff.pc.for.age)>0) d.spruce<-rbind(d.spruce,data.frame(pCount=d.sp[d.sp[,cn]>cutoff.pc.for.age,"pCount.pa"], age=i2, wt1=d.sp[d.sp[,cn]>cutoff.pc.for.age,cn]*SeasWt[d.sp[,cn]>cutoff.pc.for.age], p=d.sp$p[d.sp[,cn]>cutoff.pc.for.age]))  # Weight is the ppoportion of the site of that age class and stand type, multiplied by the original weight (which accounts for revisited sites)
        cn<-paste("Pine",i1,sep="")  # Col name
        if(cn %in% names(d.sp)) if (sum(d.sp[,cn]>cutoff.pc.for.age)>0) d.pine<-rbind(d.pine,data.frame(pCount=d.sp[d.sp[,cn]>cutoff.pc.for.age,"pCount.pa"], age=i2, wt1=d.sp[d.sp[,cn]>cutoff.pc.for.age,cn]*SeasWt[d.sp[,cn]>cutoff.pc.for.age], p=d.sp$p[d.sp[,cn]>cutoff.pc.for.age]))
        cn<-paste("Decid",i1,sep="")  # Col name
        if(cn %in% names(d.sp)) if (sum(d.sp[,cn]>cutoff.pc.for.age)>0) d.decid<-rbind(d.decid,data.frame(pCount=d.sp[d.sp[,cn]>cutoff.pc.for.age,"pCount.pa"], age=i2, wt1=d.sp[d.sp[,cn]>cutoff.pc.for.age,cn]*SeasWt[d.sp[,cn]>cutoff.pc.for.age], p=d.sp$p[d.sp[,cn]>cutoff.pc.for.age]))
        cn<-paste("Mixedwood",i1,sep="")  # Col name
        if(cn %in% names(d.sp)) if (sum(d.sp[,cn]>cutoff.pc.for.age)>0) d.mixed<-rbind(d.mixed,data.frame(pCount=d.sp[d.sp[,cn]>cutoff.pc.for.age,"pCount.pa"], age=i2, wt1=d.sp[d.sp[,cn]>cutoff.pc.for.age,cn]*SeasWt[d.sp[,cn]>cutoff.pc.for.age], p=d.sp$p[d.sp[,cn]>cutoff.pc.for.age]))
        cn<-paste("TreedBog",i1,sep="")  # Col name
        if(cn %in% names(d.sp)) if (sum(d.sp[,cn]>cutoff.pc.for.age)>0) d.treedbog<-rbind(d.treedbog,data.frame(pCount=d.sp[d.sp[,cn]>cutoff.pc.for.age,"pCount.pa"], age=i2, wt1=d.sp[d.sp[,cn]>cutoff.pc.for.age,cn]*SeasWt[d.sp[,cn]>cutoff.pc.for.age], p=d.sp$p[d.sp[,cn]>cutoff.pc.for.age]))
      }
      # Note: Not using grass and shrub here, because including parklands, so most of these are prairies, not recent fires
      # Combined data frames for models using more than one stand type
      d.upcon<-rbind(cbind(d.spruce,Sp="Spruce"),cbind(d.pine,Sp="Pine"))
      d.decidmixed<-rbind(cbind(d.decid,Sp="Decid"),cbind(d.mixed,Sp="Mixed"))
      d.all<-rbind(cbind(d.spruce,Sp="Spruce"),cbind(d.pine,Sp="Pine"),cbind(d.decid,Sp="Decid"),cbind(d.mixed,Sp="Mixed"),cbind(d.treedbog,Sp="TreedBog"))

      # 2.2. Then fit models of age functions
      # Note: Days not included in these models, because its effect on presence/absence is already included in the prediction p used in the offset
      m.age<-m.null.age<-list(NULL)  # Age models for each of the 5 stand types plus 3 combinations.  Null model is for AIC comparison to smoothing spline model (which can be worse, even if it has >1 equiv DF)
      m.null.age[[1]]<-gam(pCount~1+offset(p),data=d.spruce,family="binomial",weights=d.spruce$wt1)
      if (sum(sign(d.spruce$pCount))>4) {
        m.age[[1]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.spruce,family="binomial",weights=d.spruce$wt1)  # Fit spline through age data for that stand type if enough records
      } else {
        m.age[[1]]<-m.null.age[[1]]  # Use constant-only if too few records in that stand type
      }
      m.null.age[[2]]<-gam(pCount~1+offset(p),data=d.pine,family="binomial",weights=d.pine$wt1)
      if (sum(sign(d.pine$pCount))>4) {
        m.age[[2]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.pine,family="binomial",weights=d.pine$wt1)
      } else {
        m.age[[2]]<-m.null.age[[2]]  # Use constant-only if too few records in that stand type
      }
      m.null.age[[3]]<-gam(pCount~1+offset(p),data=d.decid,family="binomial",weights=d.decid$wt1)
      if (sum(sign(d.decid$pCount))>4) {
        m.age[[3]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.decid,family="binomial",weights=d.decid$wt1)
      } else {
        m.age[[3]]<-m.null.age[[3]]  # Use constant-only if too few records in that stand type
      }
      m.null.age[[4]]<-gam(pCount~1+offset(p),data=d.mixed,family="binomial",weights=d.mixed$wt1)
      if (sum(sign(d.mixed$pCount))>4) {
        m.age[[4]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.mixed,family="binomial",weights=d.mixed$wt1)
      } else {
        m.age[[4]]<-m.null.age[[4]]  # Use constant-only if too few records in that stand type
      }
      m.null.age[[5]]<-gam(pCount~1+offset(p),data=d.treedbog,family="binomial",weights=d.treedbog$wt1)
      if (sum(sign(d.treedbog$pCount))>4) {
        m.age[[5]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.treedbog,family="binomial",weights=d.treedbog$wt1)
      } else {
        m.age[[5]]<-m.null.age[[5]]  # Use constant-only if too few records in that stand type
      }
      m.null.age[[6]]<-gam(pCount~1+offset(p),data=d.upcon,family="binomial",weights=d.upcon$wt1)
      if (sum(sign(d.upcon$pCount))>4) {
        m.age[[6]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.upcon,family="binomial",weights=d.upcon$wt1)
      } else {
        m.age[[6]]<-m.null.age[[6]]  # Use constant-only if too few records in that stand type
      }
      m.null.age[[7]]<-gam(pCount~1+offset(p),data=d.decidmixed,family="binomial",weights=d.decidmixed$wt1)
      if (sum(sign(d.decidmixed$pCount))>4) {
        m.age[[7]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.decidmixed,family="binomial",weights=d.decidmixed$wt1)
      } else {
        m.age[[7]]<-m.null.age[[7]]  # Use constant-only if too few records in that stand type
      }
      m.null.age[[8]]<-gam(pCount~1+offset(p),data=d.all,family="binomial",weights=d.all$wt1)
      if (sum(sign(d.all$pCount))>4) {
        m.age[[8]]<-gam(pCount~s(sqrt(age),k=4,m=2)+offset(p),data=d.all,family="binomial",weights=d.all$wt1)
      } else {
        m.age[[8]]<-m.null.age[[8]]  # Use constant-only if too few records in that stand type
      }
      # AIC for options - each separate, pairwise combinations, combine all
      aic.age[seas,sp,1]<-AIC(m.age[[1]])+AIC(m.age[[2]])+AIC(m.age[[3]])+AIC(m.age[[4]])+AIC(m.age[[5]])-8  # Minus 8 for the 4 extra variances
      aic.age[seas,sp,2]<-AIC(m.age[[5]])+AIC(m.age[[6]])+AIC(m.age[[7]])-4
      aic.age[seas,sp,3]<-AIC(m.age[[8]])
      aic.delta<-aic.age[seas,sp,]-min(aic.age[seas,sp,])
      aic.exp<-exp(-1/2*aic.delta)
      aic.wt.age<-aic.exp/sum(aic.exp)
      aic.wt.age.models<-NULL
      for (i in 1:8) {  # AIC weight for spline (versus null) for each age grouping
        q<-c(AICc(m.age[[i]]),AICc(m.null.age[[i]]))
        q<-q-min(q)
        aic.wt.age.models[i]<-exp(-1/2*q[1])/sum(exp(-1/2*q))
      }
      names(aic.wt.age.models)<-c("Spruce","Pine","Decid","Mixedwood","TreedBog","DecidMixed","UpCon","TreedAll")

      # 2.3 Then make age predictions only if the spline model is better than the null, and only for (groupings of) stand types in the pa model

      age.flag<-rep(0,10)  # Flag for separate age predictions for each stand type {Spruce, Pine, Deciduous, Mixedwood, TreedBog, TreedWet (includes TreedSwamp), DecidMixed, UpCon, UplandForest, TreedAll) (0=no, 1=yes)
      p.age<-p.age.se<-array(NA,c(10,9))  # Predictions for 10 stand types, 9 age classes
      rownames(p.age)<-rownames(p.age.se)<-names(age.flag)<-c("Spruce","Pine","Decid","Mixedwood","TreedBog","TreedWet","DecidMixed","UpCon","UplandForest","TreedAll")
      if ("Spruce" %in% terms.pa) {  # Do spruce separately
        if (aic.wt.age.models["Spruce"]>0.5) {
          age.flag[1]<-1
          p<-predict(m.age[[1]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["Spruce"])),se.fit=T)
          p.age["Spruce",]<-plogis(p$fit)
          p.age.se["Spruce",]<-p$se.fit  # Still on link scale
        }
      }
      if ("Pine" %in% terms.pa) {  # Do pine separately
        if (aic.wt.age.models["Pine"]>0.5) {
          age.flag["Pine"]<-1
          p<-predict(m.age[[2]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["Pine"])),se.fit=T)
          p.age["Pine",]<-plogis(p$fit)
          p.age.se["Pine",]<-p$se.fit  # Still on link scale
        }
      }
      if ("Decid" %in% terms.pa) {  # Do deciduous separately
        if (aic.wt.age.models["Decid"]>0.5) {
          age.flag["Decid"]<-1
          p<-predict(m.age[[3]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["Decid"])),se.fit=T)
          p.age["Decid",]<-plogis(p$fit)
          p.age.se["Decid",]<-p$se.fit  # Still on link scale
        }
      }
      if ("Mixedwood" %in% terms.pa) {  # Do mixedwood separately
        if (aic.wt.age.models["Mixedwood"]>0.5) {
          age.flag["Mixedwood"]<-1
          p<-predict(m.age[[4]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["Mixedwood"])),se.fit=T)
          p.age["Mixedwood",]<-plogis(p$fit)
          p.age.se["Mixedwood",]<-p$se.fit  # Still on link scale
        }
      }
      if ("TreedBog" %in% terms.pa) {  # Do treedbog separately
        if (aic.wt.age.models["TreedBog"]>0.5) {
          age.flag["TreedBog"]<-1
          p<-predict(m.age[[5]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["TreedBog"])),se.fit=T)
          p.age["TreedBog",]<-plogis(p$fit)
          p.age.se["TreedBog",]<-p$se.fit  # Still on link scale
        }
      }
      if ("TreedWet" %in% terms.pa) {  # Do TreedWet separately, using TreedBog model
        if (aic.wt.age.models["TreedBog"]>0.5) {
          age.flag["TreedWet"]<-1
          p<-predict(m.age[[5]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["TreedWet"])),se.fit=T)
          p.age["TreedWet",]<-plogis(p$fit)
          p.age.se["TreedWet",]<-p$se.fit  # Still on link scale
        }
      }
      if ("UpCon" %in% terms.pa) {  # Do spruce+pine together
        if (aic.wt.age.models["UpCon"]>0.5) {
          age.flag["UpCon"]<-1
          p<-predict(m.age[[6]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["UpCon"])),se.fit=T)
          p.age["UpCon",]<-plogis(p$fit)
          p.age.se["UpCon",]<-p$se.fit  # Still on link scale
        }
      }
      if ("DecidMixed" %in% terms.pa) {  # Do deciduous+mixed together
        if (aic.wt.age.models["DecidMixed"]>0.5) {
          age.flag["DecidMixed"]<-1
          p<-predict(m.age[[7]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["DecidMixed"])),se.fit=T)
          p.age["DecidMixed",]<-plogis(p$fit)
          p.age.se["DecidMixed",]<-p$se.fit  # Still on link scale
        }
      }
      if ("UplandForest" %in% terms.pa)  { # Do those four stand types together, using "all" age model
        if (aic.wt.age.models["TreedAll"]>0.5) {
          age.flag["UplandForest"]<-1
          p<-predict(m.age[[8]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["UplandForest"])),se.fit=T)
          p.age["UplandForest",]<-plogis(p$fit)
          p.age.se["UplandForest",]<-p$se.fit  # Still on link scale
        }
      }
      if ("TreedAll" %in% terms.pa)  {  # Do all five together
        if (aic.wt.age.models["TreedAll"]>0.5) {
          age.flag["TreedAll"]<-1
          p<-predict(m.age[[8]],newdata=data.frame(age=c(0.5,1:8),p=qlogis(Coef1.pa["TreedAll"])),se.fit=T)
          p.age["TreedAll",]<-plogis(p$fit)
          p.age.se["TreedAll",]<-p$se.fit  # Still on link scale
        }
      }
      # Post-hoc smoothing of age relationships, to deal with extreme values due to linear relationships on log (=lower part of logit) scale
      for (i in 1:nrow(p.age)) {
        y1<-p.age[i,]
        y1.se<-p.age.se[i,]
        y2<-c(sum(y1[1:2])/2,y1[2:4],sum(y1[4:6])/3,sum(y1[5:7])/3,sum(y1[6:8])/3,sum(y1[7:9])/3,sum(y1[8:9])/2)
        y2.se<-c(sum(y1.se[1:2])/2,y1.se[2:4],sum(y1.se[4:6])/3,sum(y1.se[5:7])/3,sum(y1.se[6:8])/3,sum(y1.se[7:9])/3,sum(y1.se[8:9])/2)
        p.age[i,]<-y2
        p.age.se[i,]<-y2.se
      }
      # Save aic weights
      aic.wt.age.save[seas,sp,]<-aic.wt.age
      aic.wt.age.models.save[seas,sp,]<-aic.wt.age.models

      # 3. Calculate total abundance coefficients

      # 3.1 Assemble presence/absence coefficients for each veg+HF type then multiply by appropriate Coef.agp term to generate Coef.mean matrix including expanding out stand types that have ages, to populate entire Coef.pa matrix,
      for (i in 1:length(terms.pa1)) {

        j<-as.character(pm$VegType[pm[,terms.pa1[i]]==1])  # The fine veg+HF types that are covered by the (potentially broader) variable included in the best model
        for (j1 in 1:length(j)) {
          if (j[j1] %in% c("Spruce","Pine","Decid","Mixedwood","TreedBog")) {
            # Stand types that have age classes in full coefficients
            x<-paste(j[j1],c("R","1","2","3","4","5","6","7","8"),sep="")  # Names in Coef.pa and Coef.mean with ages
            if ((age.flag[terms.pa1[i]]==0)==TRUE | is.na(age.flag[terms.pa1[i]])) {   # But these broad types do not have age classes in the best model
              Coef.pa[seas,sp,x]<-Coef1.pa[terms.pa1[i]]  # Fill in all ages with value for that (broad) stand type
              Coef.pa.se[seas,sp,x]<-Coef1.pa.se[terms.pa1[i]]  # Fill in all ages with value for that (broad) stand type
              Coef.mean[seas,sp,x]<-Coef.pa[seas,sp,x]*Coef.agp[seas,sp,j[j1]]  # Only the one value for agp
              Coef.lci[seas,sp,x]<-plogis(qlogis(Coef.pa[seas,sp,x])-Coef.pa.se[seas,sp,x]*1.28) * exp(log(Coef.agp[seas,sp,j[j1]])-Coef.agp.se[seas,sp,j[j1]]*1.28)  # Using 10% intervals for each to multiply to 5% intervals (assuming independence - checked empirically)
              Coef.uci[seas,sp,x]<-plogis(qlogis(Coef.pa[seas,sp,x])+Coef.pa.se[seas,sp,x]*1.28) * exp(log(Coef.agp[seas,sp,j[j1]])+Coef.agp.se[seas,sp,j[j1]]*1.28)  # Using 10% intervals for each to multiply to 5% intervals (assuming independence - checked empirically)
            } else {  # These broad types do have age classes in the best model
              Coef.pa[seas,sp,x]<-p.age[terms.pa1[i],]  # Use separate age values for that (broad) stand type
              Coef.pa.se[seas,sp,x]<-p.age.se[terms.pa1[i],]  # Use separate age values for that (broad) stand type
              Coef.mean[seas,sp,x]<-Coef.pa[seas,sp,x]*Coef.agp[seas,sp,j[j1]]  # Only the one value for agp
              Coef.lci[seas,sp,x]<-plogis(qlogis(Coef.pa[seas,sp,x])-Coef.pa.se[seas,sp,x]*1.28) * exp(log(Coef.agp[seas,sp,j[j1]])-Coef.agp.se[seas,sp,j[j1]]*1.28)  # Using 10% intervals for each to multiply to 5% intervals (assuming independence - checked empirically)
              Coef.uci[seas,sp,x]<-plogis(qlogis(Coef.pa[seas,sp,x])+Coef.pa.se[seas,sp,x]*1.28) * exp(log(Coef.agp[seas,sp,j[j1]])+Coef.agp.se[seas,sp,j[j1]]*1.28)  # Using 10% intervals for each to multiply to 5% intervals (assuming independence - checked empirically)
            }
          } else {
            # Stand types that do not have age classes in full coefficients
            Coef.pa[seas,sp,j[j1]]<-Coef1.pa[terms.pa1[i]]  # Fill in the non-age coefficient
            Coef.pa.se[seas,sp,j[j1]]<-Coef1.pa.se[terms.pa1[i]]  # Fill in the non-age coefficient
            Coef.mean[seas,sp,j[j1]]<-Coef.pa[seas,sp,j[j1]]*Coef.agp[seas,sp,j[j1]]
            Coef.lci[seas,sp,j[j1]]<-plogis(qlogis(Coef.pa[seas,sp,j[j1]])-Coef.pa.se[seas,sp,j[j1]]*1.28) * exp(log(Coef.agp[seas,sp,j[j1]])-Coef.agp.se[seas,sp,j[j1]]*1.28)  # Using 10% intervals for each to multiply to 5% intervals (assuming independence - checked empirically)
            Coef.uci[seas,sp,j[j1]]<-plogis(qlogis(Coef.pa[seas,sp,j[j1]])+Coef.pa.se[seas,sp,j[j1]]*1.28) * exp(log(Coef.agp[seas,sp,j[j1]])+Coef.agp.se[seas,sp,j[j1]]*1.28)  # Using 10% intervals for each to multiply to 5% intervals (assuming independence - checked empirically)
          }  # End if for stand types with age classes
        }  # Next fine veg+HF type within broader class
      }  # Next broader class in best model

      # 3.2 Do any post-hoc modifications  CHECK ALL WITH NEW DATA

      # Note: Not using cutblock convergence for best-model analysis (i.e., for "honest" coefficient figures), but it is used below for the final all-coefficients exported
      if (SpTable[sp]=="Muledeer" & seas==1) {
        Coef.mean[seas,sp,c("Pine7","Pine8")]<-(Coef.mean[seas,sp,c("Pine7","Pine8")]+Coef.mean[seas,sp,"Pine6"])/2  # Average oldest pine with mature to get less extreme values (problem is due to exponential extrapolation of age relationship)
        Coef.lci[seas,sp,c("Pine7","Pine8")]<-(Coef.lci[seas,sp,c("Pine7","Pine8")]+Coef.lci[seas,sp,"Pine6"])/2  # Average oldest pine with mature to get less extreme values
        Coef.uci[seas,sp,c("Pine7","Pine8")]<-(Coef.uci[seas,sp,c("Pine7","Pine8")]+Coef.uci[seas,sp,"Pine6"])/2  # Average oldest pine with mature to get less extreme values
      }
      if (SpTable[sp]=="Muledeer" & seas==2) {
        Coef.mean[seas,sp,c("Pine7","Pine8")]<-(Coef.mean[seas,sp,c("Pine7","Pine8")]+Coef.mean[seas,sp,"Pine6"])/2  # Average oldest pine with mature to get less extreme values (problem is due to exponential extrapolation of age relationship)
        Coef.lci[seas,sp,c("Pine7","Pine8")]<-(Coef.lci[seas,sp,c("Pine7","Pine8")]+Coef.lci[seas,sp,"Pine6"])/2  # Average oldest pine with mature to get less extreme values
        Coef.uci[seas,sp,c("Pine7","Pine8")]<-(Coef.uci[seas,sp,c("Pine7","Pine8")]+Coef.uci[seas,sp,"Pine6"])/2  # Average oldest pine with mature to get less extreme values
        Coef.mean[seas,sp,c("Decid7","Decid8")]<-(Coef.mean[seas,sp,c("Decid7","Decid8")]+Coef.mean[seas,sp,"Decid6"])/2  # Average oldest deciduous with mature to get less extreme values
        Coef.lci[seas,sp,c("Decid7","Decid8")]<-(Coef.lci[seas,sp,c("Decid7","Decid8")]+Coef.lci[seas,sp,"Decid6"])/2  # Average oldest deciduous with mature to get less extreme values
        Coef.uci[seas,sp,c("Decid7","Decid8")]<-(Coef.uci[seas,sp,c("Decid7","Decid8")]+Coef.uci[seas,sp,"Decid6"])/2  # Average oldest deciduous with mature to get less extreme values
      }

      # 3.3 Predictions for each site, as offsets below

      d.sp$t.p.pa<-colSums(Coef1.pa[terms.pa1]*t(d.sp[,terms.pa1]))  # Prediction of presence/absence at each site - ordinal scale
      d.sp$t.p.agp<-colSums(Coef1.agp[terms.pa1]*t(d.sp[,terms.pa1]))  # Prediction of abundance|presence at each site - log scale
      d.sp$p.ta<-d.sp$t.p.pa*exp(d.sp$t.p.agp)  # Prediction (ordinal scale) of total abundance at each site
      # Further adjustments so that mean(Coef.mean) = mean(observed density).  Adjusts for geometric mean issue, and also for any remaining issues with Days adjustments
      pCount<-ifelse(d.sp$Lured=="Yes",d.sp$Count/(lure.pa[seas,sp]*lure.agp[seas,sp]),d.sp$Count)
      Coef.mean[seas,sp,]<-Coef.mean[seas,sp,]*mean(pCount)/mean(d.sp$p.ta)  # mean(pCount) should be very to mean(d.sp$p.ta)
      Coef.lci[seas,sp,]<-Coef.lci[seas,sp,]*mean(pCount)/mean(d.sp$p.ta)
      Coef.uci[seas,sp,]<-Coef.uci[seas,sp,]*mean(pCount)/mean(d.sp$p.ta)

      # 3.4 Store values for season

      j<-match(d.sp$location_project,d1$location_project)
      if (seas==1) {
        d1$count.summer[j]<-d.sp$Count
        d1$p.pa.summer[j]<-d.sp$t.p.pa
        d1$p.agp.summer[j]<-exp(d.sp$t.p.agp)
        d1$p.summer[j]<-d.sp$p.ta
      } else {
        d1$count.winter[j]<-d.sp$Count
        d1$p.pa.winter[j]<-d.sp$t.p.pa
        d1$p.agp.winter[j]<-exp(d.sp$t.p.agp)
        d1$p.winter[j]<-d.sp$p.ta
      }

      # 4. Coefficient figures (Only done for total abundance)

      # 4.1. Assemble values for custom figure for best model and age options
      x<-col1<-y<-y.lci<-y.uci<-w<-class<-space1<-NULL
      for (i in 1:length(terms.pa1)) {
        if (age.flag[terms.pa1[i]]==0 | is.na(age.flag[terms.pa1[i]])) {  # Variable without age info
          j<-match(terms.pa1[i],fp$Class)
          x<-c(x,fp$x[j])
          veg.to.use<-as.character(pm$VegType[pm[,terms.pa1[i]]==1])[1]  # To use Coef.mean, find the first fine veg+HF types that is included in the (potentially broader) variable being plotted
          if (veg.to.use %in% c("Spruce","Pine","Decid","Mixedwood","TreedBog")) veg.to.use<-paste(veg.to.use,"R",sep="")  # If no age relationship, use the first value for that fine stand type (don't add the R for types that don't have age classes at all)
          y<-c(y,Coef.mean[seas,sp,veg.to.use])
          y.lci<-c(y.lci,Coef.lci[seas,sp,veg.to.use])
          y.uci<-c(y.uci,Coef.uci[seas,sp,veg.to.use])
          col1<-c(col1,fp$col1[j])
          w<-c(w,fp$width[j])
          class<-c(class,terms.pa1[i])
          space1<-c(space1,fp$spaceafter[j])
        } else {  # Variable with age info
          j<-match(paste(terms.pa1[i],"R",sep=""),fp$Class):match(paste(terms.pa1[i],"8",sep=""),fp$Class)
          x<-c(x,fp$x[j])
          veg.to.use<-as.character(pm$VegType[pm[,terms.pa1[i]]==1])[1]  # To use Coef.mean, find the first fine veg+HF types that is included in the (potentially broader) variable being plotted
          veg.to.use<-paste(veg.to.use,c("R","1","2","3","4","5","6","7","8"),sep="")  # If age relationship, use each value for that fine stand type
          y<-c(y,Coef.mean[seas,sp,veg.to.use])
          y.lci<-c(y.lci,Coef.lci[seas,sp,veg.to.use])
          y.uci<-c(y.uci,Coef.uci[seas,sp,veg.to.use])
          col1<-c(col1,fp$col1[j])
          w<-c(w,fp$width[j])
          class<-c(class,as.character(fp$Class[j]))
          space1<-c(space1,fp$spaceafter[j])
        }
      }
      ord<-order(x)  # Sort all by x
      y<-y[ord]
      y.lci<-y.lci[ord]
      y.uci<-y.uci[ord]
      col1<-col1[ord]
      w<-w[ord]
      class<-class[ord]
      space1<-space1[ord]
      x<-x[ord]
      # Rectify spaces between x's
      for (i in 1:(length(x)-1)) {
        for (j in (i+1):length(x)) x[j]<-x[j]+space1[i]-(x[i+1]-x[i])  # Alter all subsequent positions accordingly
      }

      # 4.2. Make bar plot
      ymax<-min(max(y.uci,na.rm=TRUE),2*max(y,na.rm=TRUE))  # This keeps the figures readable when there are extreme UCI's
      space<-c(1,x[-1]-x[-length(x)])-0.99  # The spacing between bars
      density<-ifelse(substr(class,1,2)=="CC",50,NA)
      fname<-paste(fname.fig,"Veg+HF figure best model ",SpTable[sp]," ",SeasName[seas],".png",sep="")
      png(file=fname,width=ifelse(length(y)>5,1500,1000),height=700)
      par(mai=c(1.9,1,0.2,0.3))
      x1<-barplot(y,space=space,width=w,border="white",col="grey30",ylim=c(0,ymax),yaxt="n",ylab="Relative abundance",col.lab="grey50",cex.lab=1.2,axisnames=F)[,1]  # To get strips on CC bars
      abline(h=pretty(c(0,ymax)),col="grey80")
      x1<-barplot(y,space=space,width=w,border="white",col="grey30",ylim=c(0,ymax),yaxt="n",ylab="Relative abundance",col.lab="grey50",cex.lab=1.2,axisnames=F,add=TRUE)[,1]  # To get strips on CC bars, and to put bars in front of horizontal axis lines
      x1<-barplot(y,space=space,width=w,border="white",density=density,col=col1,ylim=c(0,ymax),yaxt="n",ylab="Relative abundance",col.lab="grey50",cex.lab=1.2,axisnames=F,add=TRUE)[,1]
      axis(side=2,tck=0.02,cex.axis=0.9,col.axis="grey50",col.ticks="grey50",las=2,at=pretty(c(0,ymax)))
      box(bty="l",col="grey50")
      for (i in 1:length(x1)) {
        lines(rep(x1[i],2),c(y[i],y.uci[i]),col=col1[i])
        lines(rep(x1[i],2),c(y[i],y.lci[i]),col="grey90")
      }
      for (i in 1:length(class)) {  # Label x axis
        if (substr(class[i],1,2)=="CC") {
          mtext(side=1,line=0,at=x1[i],"cut",col=col1[i],cex=0.8)
          if (class[i]=="CCAll") mtext(side=1,at=x1[i],line=1,"All",col=col1[i])
          if (class[i]=="CCConif") mtext(side=1,at=x1[i],line=1,"Conif",col=col1[i])
        } else {
          if (substr(class[i],nchar(class[i]),nchar(class[i])) %in% c("R","1","2","3","4","5","6","7","8")==TRUE) {
            if (substr(class[i],nchar(class[i]),nchar(class[i]))=="R") mtext(side=1,line=0,at=x1[i]-0.5,"0",col=col1[i],cex=0.8)
            if (substr(class[i],nchar(class[i]),nchar(class[i]))=="2") mtext(side=1,line=0,at=x1[i]-0.5,"20",col=col1[i],cex=0.8)
            if (substr(class[i],nchar(class[i]),nchar(class[i]))=="4") {
              mtext(side=1,line=0,at=x1[i]-0.5,"60",col=col1[i],cex=0.8)
              mtext(side=1,line=1,at=x1[i],substr(class[i],1,nchar(class[i])-1),col=col1[i],cex=1.2)
            }
            if (substr(class[i],nchar(class[i]),nchar(class[i]))=="6") mtext(side=1,line=0,at=x1[i]-0.5,"100",col=col1[i],cex=0.8)
            if (substr(class[i],nchar(class[i]),nchar(class[i]))=="8") mtext(side=1,line=0,at=x1[i]-0.5,"140",col=col1[i],cex=0.8)
          } else {
            if (class[i]=="Crop") class[i]<-"Crop+"  # Because this also includes other alienating
            mtext(side=1,at=x1[i],line=1,adj=ifelse(w[i]>2 | length(y)<9,0.5,1),las=ifelse(w[i]>2 | length(y)<9,1,2),class[i],col=col1[i],cex=1.2)
          }  # End if for aged treed type
        } # End if for CC
      }
      mtext(side=3,at=x1[1],adj=0,paste(sp.names[sp],SeasName[seas],"- North"),col="grey30",cex=1.2)
      text(max(x1),ymax*0.98,paste("Detected at",sum(sign(d.sp$Count)),"of",nrow(d.sp),SeasName[seas],"camera locations"),cex=1.1,adj=1,col="grey40") # Add sample size
      graphics.off()
    }  # End if for that species being in the species table for that season
  }  # Next season

  # 5. Combine Summer and winter coefficients and make figures

  # 5.1. Combine Summer and winter coefficients
  # Use these for mapping, below (pres/abs needed for cases where space/climate residual models are just for pres/abs)
  if (paste(SpTable[sp],"Summer",sep="") %in% SpTable.s  & paste(SpTable[sp],"Winter",sep="") %in% SpTable.w) {
    Coef.pa.all[sp,]<-(Coef.pa[1,sp,]+Coef.pa[2,sp,])/2
    Coef.mean.all[sp,]<-(Coef.mean[1,sp,]+Coef.mean[2,sp,])/2
    Coef.lci.all[sp,]<-(Coef.lci[1,sp,]+Coef.lci[2,sp,])/2
    Coef.uci.all[sp,]<-(Coef.uci[1,sp,]+Coef.uci[2,sp,])/2
  }
  if (paste(SpTable[sp],"Summer",sep="") %in% SpTable.s  & (paste(SpTable[sp],"Winter",sep="") %in% SpTable.w == FALSE)) {
    Coef.pa.all[sp,]<-Coef.pa[1,sp,]
    Coef.mean.all[sp,]<-Coef.mean[1,sp,]
    Coef.lci.all[sp,]<-Coef.lci[1,sp,]
    Coef.uci.all[sp,]<-Coef.uci[1,sp,]
  }
  if (paste(SpTable[sp],"Winter",sep="") %in% SpTable.w  & (paste(SpTable[sp],"Summer",sep="") %in% SpTable.s == FALSE)) {
    Coef.pa.all[sp,]<-Coef.pa[2,sp,]
    Coef.mean.all[sp,]<-Coef.mean[2,sp,]
    Coef.lci.all[sp,]<-Coef.lci[2,sp,]
    Coef.uci.all[sp,]<-Coef.uci[2,sp,]
  }

  # 5.2. Do cutblock convergence for class 2 and unsampled class 3 and 4
  Coef.mean.all[sp,"CCSpruce2"]<-Coef.mean.all[sp,"CCSpruce2"]*(1-0.5)+Coef.mean.all[sp,"Spruce2"]*0.5
  Coef.mean.all[sp,"CCSpruce3"]<-Coef.mean.all[sp,"CCSpruce3"]*(1-0.849)+Coef.mean.all[sp,"Spruce3"]*0.849
  Coef.mean.all[sp,"CCSpruce4"]<-Coef.mean.all[sp,"CCSpruce4"]*(1-0.96)+Coef.mean.all[sp,"Spruce4"]*0.96
  Coef.mean.all[sp,"CCPine2"]<-Coef.mean.all[sp,"CCPine2"]*(1-0.5)+Coef.mean.all[sp,"Pine2"]*0.5
  Coef.mean.all[sp,"CCPine3"]<-Coef.mean.all[sp,"CCPine3"]*(1-0.849)+Coef.mean.all[sp,"Pine3"]*0.849
  Coef.mean.all[sp,"CCPine4"]<-Coef.mean.all[sp,"CCPine4"]*(1-0.96)+Coef.mean.all[sp,"Pine4"]*0.96
  Coef.mean.all[sp,"CCMixedwood2"]<-Coef.mean.all[sp,"CCMixedwood2"]*(1-0.705)+Coef.mean.all[sp,"Mixedwood2"]*0.705
  Coef.mean.all[sp,"CCMixedwood3"]<-Coef.mean.all[sp,"CCMixedwood3"]*(1-0.912)+Coef.mean.all[sp,"Mixedwood3"]*0.912
  Coef.mean.all[sp,"CCMixedwood4"]<-Coef.mean.all[sp,"CCMixedwood4"]*(1-0.97)+Coef.mean.all[sp,"Mixedwood4"]*0.97
  Coef.mean.all[sp,"CCDecid2"]<-Coef.mean.all[sp,"CCDecid2"]*(1-0.705)+Coef.mean.all[sp,"Decid2"]*0.705
  Coef.mean.all[sp,"CCDecid3"]<-Coef.mean.all[sp,"CCDecid3"]*(1-0.912)+Coef.mean.all[sp,"Decid3"]*0.912
  Coef.mean.all[sp,"CCDecid4"]<-Coef.mean.all[sp,"CCDecid4"]*(1-0.97)+Coef.mean.all[sp,"Decid4"]*0.97
  # And CI's
  Coef.lci.all[sp,"CCSpruce2"]<-Coef.lci.all[sp,"CCSpruce2"]*(1-0.5)+Coef.lci.all[sp,"Spruce2"]*0.5
  Coef.lci.all[sp,"CCSpruce3"]<-Coef.lci.all[sp,"CCSpruce3"]*(1-0.849)+Coef.lci.all[sp,"Spruce3"]*0.849
  Coef.lci.all[sp,"CCSpruce4"]<-Coef.lci.all[sp,"CCSpruce4"]*(1-0.96)+Coef.lci.all[sp,"Spruce4"]*0.96
  Coef.lci.all[sp,"CCPine2"]<-Coef.lci.all[sp,"CCPine2"]*(1-0.5)+Coef.lci.all[sp,"Pine2"]*0.5
  Coef.lci.all[sp,"CCPine3"]<-Coef.lci.all[sp,"CCPine3"]*(1-0.849)+Coef.lci.all[sp,"Pine3"]*0.849
  Coef.lci.all[sp,"CCPine4"]<-Coef.lci.all[sp,"CCPine4"]*(1-0.96)+Coef.lci.all[sp,"Pine4"]*0.96
  Coef.lci.all[sp,"CCMixedwood2"]<-Coef.lci.all[sp,"CCMixedwood2"]*(1-0.705)+Coef.lci.all[sp,"Mixedwood2"]*0.705
  Coef.lci.all[sp,"CCMixedwood3"]<-Coef.lci.all[sp,"CCMixedwood3"]*(1-0.912)+Coef.lci.all[sp,"Mixedwood3"]*0.912
  Coef.lci.all[sp,"CCMixedwood4"]<-Coef.lci.all[sp,"CCMixedwood4"]*(1-0.97)+Coef.lci.all[sp,"Mixedwood4"]*0.97
  Coef.lci.all[sp,"CCDecid2"]<-Coef.lci.all[sp,"CCDecid2"]*(1-0.705)+Coef.lci.all[sp,"Decid2"]*0.705
  Coef.lci.all[sp,"CCDecid3"]<-Coef.lci.all[sp,"CCDecid3"]*(1-0.912)+Coef.lci.all[sp,"Decid3"]*0.912
  Coef.lci.all[sp,"CCDecid4"]<-Coef.lci.all[sp,"CCDecid4"]*(1-0.97)+Coef.lci.all[sp,"Decid4"]*0.97
  Coef.uci.all[sp,"CCSpruce2"]<-Coef.uci.all[sp,"CCSpruce2"]*(1-0.5)+Coef.uci.all[sp,"Spruce2"]*0.5
  Coef.uci.all[sp,"CCSpruce3"]<-Coef.uci.all[sp,"CCSpruce3"]*(1-0.849)+Coef.uci.all[sp,"Spruce3"]*0.849
  Coef.uci.all[sp,"CCSpruce4"]<-Coef.uci.all[sp,"CCSpruce4"]*(1-0.96)+Coef.uci.all[sp,"Spruce4"]*0.96
  Coef.uci.all[sp,"CCPine2"]<-Coef.uci.all[sp,"CCPine2"]*(1-0.5)+Coef.uci.all[sp,"Pine2"]*0.5
  Coef.uci.all[sp,"CCPine3"]<-Coef.uci.all[sp,"CCPine3"]*(1-0.849)+Coef.uci.all[sp,"Pine3"]*0.849
  Coef.uci.all[sp,"CCPine4"]<-Coef.uci.all[sp,"CCPine4"]*(1-0.96)+Coef.uci.all[sp,"Pine4"]*0.96
  Coef.uci.all[sp,"CCMixedwood2"]<-Coef.uci.all[sp,"CCMixedwood2"]*(1-0.705)+Coef.uci.all[sp,"Mixedwood2"]*0.705
  Coef.uci.all[sp,"CCMixedwood3"]<-Coef.uci.all[sp,"CCMixedwood3"]*(1-0.912)+Coef.uci.all[sp,"Mixedwood3"]*0.912
  Coef.uci.all[sp,"CCMixedwood4"]<-Coef.uci.all[sp,"CCMixedwood4"]*(1-0.97)+Coef.uci.all[sp,"Mixedwood4"]*0.97
  Coef.uci.all[sp,"CCDecid2"]<-Coef.uci.all[sp,"CCDecid2"]*(1-0.705)+Coef.uci.all[sp,"Decid2"]*0.705
  Coef.uci.all[sp,"CCDecid3"]<-Coef.uci.all[sp,"CCDecid3"]*(1-0.912)+Coef.uci.all[sp,"Decid3"]*0.912
  Coef.uci.all[sp,"CCDecid4"]<-Coef.uci.all[sp,"CCDecid4"]*(1-0.97)+Coef.uci.all[sp,"Decid4"]*0.97

  # 5.3. Figure for overall coefficients
  # Do the next three lines here to use count.all in figure
  d1$count.all<-ifelse(!is.na(d1$count.summer) & !is.na(d1$count.winter),(d1$count.summer+d1$count.winter)/2, ifelse(is.na(d1$count.summer),0,d1$count.summer)+ifelse(is.na(d1$count.winter),0,d1$count.winter))  # Use unweighted average if both available, because predictions are for equal effort (100 days) per season.  Otherwise, use just whichever season is available
  d1$p.pa.all<-ifelse(!is.na(d1$count.summer) & !is.na(d1$count.winter),(d1$p.pa.summer+d1$p.pa.winter)/2, ifelse(is.na(d1$count.summer),0,d1$p.pa.summer)+ifelse(is.na(d1$count.winter),0,d1$p.pa.winter))  # Use (average) prediction(s) for whichever season(s) have/has available count
  d1$p.all<-ifelse(!is.na(d1$count.summer) & !is.na(d1$count.winter),(d1$p.summer+d1$p.winter)/2, ifelse(is.na(d1$count.summer),0,d1$p.summer)+ifelse(is.na(d1$count.winter),0,d1$p.winter))  # Use (average) prediction(s) for whichever season(s) have/has available count
  x<-col1<-y<-y.lci<-y.uci<-w<-class<-space1<-NULL
  for (i in 1:length(vnames.agp)) {  # vnames.agp doesn't have age classes for natural stands
    if (vnames.agp[i] %in% c("Spruce","Pine","Decid","Mixedwood","TreedBog") == FALSE) {  # Variable without age info
      j<-match(vnames.agp[i],fp$Class)
      x<-c(x,fp$x[j])
      veg.to.use<-as.character(pm$VegType[pm[,vnames.agp[i]]==1])[1]  # To use Coef.mean, find the first fine veg+HF types that is included in the (potentially broader) variable being plotted
      if (veg.to.use %in% c("Spruce","Pine","Decid","Mixedwood","TreedBog")) veg.to.use<-paste(veg.to.use,"R",sep="")  # If no age relationship, use the first value for that fine stand type (don't add the R for types that don't have age classes at all)
      y<-c(y,Coef.mean.all[sp,veg.to.use])
      y.lci<-c(y.lci,Coef.lci.all[sp,veg.to.use])
      y.uci<-c(y.uci,Coef.uci.all[sp,veg.to.use])
      col1<-c(col1,fp$col1[j])
      w<-c(w,fp$width[j])
      class<-c(class,vnames.agp[i])
      space1<-c(space1,fp$spaceafter[j])
    } else {  # Variable with age info
      j<-match(paste(vnames.agp[i],"R",sep=""),fp$Class):match(paste(vnames.agp[i],"8",sep=""),fp$Class)
      x<-c(x,fp$x[j])
      veg.to.use<-as.character(pm$VegType[pm[,vnames.agp[i]]==1])[1]  # To use Coef.mean, find the first fine veg+HF types that is included in the (potentially broader) variable being plotted
      veg.to.use<-paste(veg.to.use,c("R","1","2","3","4","5","6","7","8"),sep="")  # If age relationship, use each value for that fine stand type
      y<-c(y,Coef.mean.all[sp,veg.to.use])
      y.lci<-c(y.lci,Coef.lci.all[sp,veg.to.use])
      y.uci<-c(y.uci,Coef.uci.all[sp,veg.to.use])
      col1<-c(col1,fp$col1[j])
      w<-c(w,fp$width[j])
      class<-c(class,as.character(fp$Class[j]))
      space1<-c(space1,fp$spaceafter[j])
    }
  }
  ord<-order(x)  # Sort all by x
  y<-y[ord]
  y.lci<-y.lci[ord]
  y.uci<-y.uci[ord]
  col1<-col1[ord]
  w<-w[ord]
  class<-class[ord]
  space1<-space1[ord]
  x<-x[ord]
  # Rectify spaces between x's
  for (i in 1:(length(x)-1)) {
    for (j in (i+1):length(x)) x[j]<-x[j]+space1[i]-(x[i+1]-x[i])  # Alter all subsequent positions accordingly
  }
  # Make bar plot
  ymax<-min(max(y.uci,na.rm=TRUE),2*max(y,na.rm=TRUE))  # This keeps the figures readable when there are extreme UCI's
  space<-c(1,x[-1]-x[-length(x)])-0.99  # The spacing between bars
  density<-ifelse(substr(class,1,2)=="CC",50,NA)
  fname<-paste(fname.fig,"Veg+HF figure best model ",SpTable[sp],".png",sep="")
  png(file=fname,width=ifelse(length(y)>5,1500,1000),height=700)
  par(mai=c(1.9,1,0.2,0.3))
  x1<-barplot(y,space=space,width=w,border="white",col="grey30",ylim=c(0,ymax),yaxt="n",ylab="Relative abundance",col.lab="grey50",cex.lab=1.2,axisnames=F)[,1]  # To get strips on CC bars
  abline(h=pretty(c(0,ymax)),col="grey80")
  x1<-barplot(y,space=space,width=w,border="white",col="grey30",ylim=c(0,ymax),yaxt="n",ylab="Relative abundance",col.lab="grey50",cex.lab=1.2,axisnames=F,add=TRUE)[,1]  # To get strips on CC bars, and to put bars in front of horizontal axis lines
  x1<-barplot(y,space=space,width=w,border="white",density=density,col=col1,ylim=c(0,ymax),yaxt="n",ylab="Relative abundance",col.lab="grey50",cex.lab=1.2,axisnames=F,add=TRUE)[,1]
  axis(side=2,tck=0.02,cex.axis=0.9,col.axis="grey50",col.ticks="grey50",las=2,at=pretty(c(0,ymax)))
  box(bty="l",col="grey50")
  for (i in 1:length(x1)) {
    lines(rep(x1[i],2),c(y[i],y.uci[i]),col=col1[i])
    lines(rep(x1[i],2),c(y[i],y.lci[i]),col="grey90")
  }
  for (i in 1:length(class)) {  # Label x axis
    if (substr(class[i],1,2)=="CC") {
      if(substr(class[i],nchar(class[i]),nchar(class[i]))=="2") mtext(side=1,line=0,at=x1[i],"cut",col=col1[i],cex=0.8)
      if (class[i]=="CCAll") mtext(side=1,at=x1[i],line=1,"All",col=col1[i])
      if (class[i]=="CCConif") mtext(side=1,at=x1[i],line=1,"Conif",col=col1[i])
    } else {
      if (substr(class[i],nchar(class[i]),nchar(class[i])) %in% c("R","1","2","3","4","5","6","7","8")==TRUE) {
        if (substr(class[i],nchar(class[i]),nchar(class[i]))=="R") mtext(side=1,line=0,at=x1[i]-0.5,"0",col=col1[i],cex=0.8)
        if (substr(class[i],nchar(class[i]),nchar(class[i]))=="2") mtext(side=1,line=0,at=x1[i]-0.5,"20",col=col1[i],cex=0.8)
        if (substr(class[i],nchar(class[i]),nchar(class[i]))=="4") {
          mtext(side=1,line=0,at=x1[i]-0.5,"60",col=col1[i],cex=0.8)
          mtext(side=1,line=1,at=x1[i],substr(class[i],1,nchar(class[i])-1),col=col1[i],cex=1.2)
        }
        if (substr(class[i],nchar(class[i]),nchar(class[i]))=="6") mtext(side=1,line=0,at=x1[i]-0.5,"100",col=col1[i],cex=0.8)
        if (substr(class[i],nchar(class[i]),nchar(class[i]))=="8") mtext(side=1,line=0,at=x1[i]-0.5,"140",col=col1[i],cex=0.8)
      } else {
        if (class[i]=="Crop") class[i]<-"Crop+"  # Because this also includes other alienating
        mtext(side=1,at=x1[i],line=1,adj=ifelse(w[i]>2 | length(y)<9,0.5,1),las=ifelse(w[i]>2 | length(y)<9,1,2),class[i],col=col1[i],cex=1.2)
      }  # End if for aged treed type
    } # End if for CC
  }
  mtext(side=3,at=x1[1],adj=0,paste(sp.names[sp],"- North"),col="grey30",cex=1.2)
  text(max(x1),ymax*0.98,paste("Detected at",sum(sign(d1$count.all)),"of",nrow(d1),"camera locations"),cex=1.1,adj=1,col="grey40") # Add sample size
  graphics.off()

  # 6. Residual variation due to location and climate (and 150m veg+HF)
  # These models are fit to the residual variation after the average summer and winter predictions (or whichever one(s) are available).
  # Average summer and winter counts and predictions, and add explanatory variables from d to d1.  Check first that location_projects in d1 and d are still in same order
  # There are extra lines here to do 150m veg+HF residual modeling (and combined 150m+space/climate).  This is for the test of using that 150m scale.  However, 150m data was not available for all sites, so this reduces sample size (since the same samples then need to be used for 150m and space/climate modeling, to allow AIC comparisons).
  # Therefore, the standard space/climate modeling excludes those lines - they are commented out - , so the largest dataset can be used.

  # 6.1. Fit space/climate and optional 150m veg+HF
  plot(1:nrow(d),match(d$location_project,d1$location_project),cex=0.3)  # Making sure that deployments in d and d1 are the same and in same order before cbind'ing.  Needs to be a straight 1:1 line
  d2<-cbind(d1,d[,c("Lat","TrueLat","Long", "Easting", "Northing",
                    "TD", "MSP", "SHM", "DD_0", "DD5", "DD_18", "DD18", "NFFD", "bFFP",
                    "eFFP", "FFP", "PAS", "EMT", "EXT", "Eref", "CMD", "RH", "CMI", "DD1040", "bio9", "bio15", "HTV",
                    "AHM","MAT", "MAP","MWMT","MCMT","NSR1")])  # For space/climate modeling only (no 150m options)
  d2<-d2[!is.na(rowSums(d2[,c("Lat","TrueLat","Long", "Easting", "Northing",
                              "TD", "MSP", "SHM", "DD_0", "DD5", "DD_18", "DD18", "NFFD", "bFFP",
                              "eFFP", "FFP", "PAS", "EMT", "EXT", "Eref", "CMD", "RH", "CMI", "DD1040", "bio9", "bio15", "HTV",
                              "AHM","MAT", "MAP","MWMT","MCMT")])),]

  if (sc.option[sp]==2 | sc.option[sp]==1) { # Presence/absence model only.  It is run for option 1 here solely to do fit AUC; the total abundance models replace these in the next section for option 1.
    # Best climate and spatial model is found.  If not commented out, then best 150m veg+HF, then test which/both are best
    UseForLure<-grepl("^[[:digit:]]+",d2$location)  # Only use ongrid sites for lure calibration (paired lure/not).  These are now identified by locations starting with digits
    dtemp<-d2[UseForLure==TRUE,]  # To calculate lure effect only with ABMI paired'ish sites
    lure1<-mean(sign(dtemp$count.all[dtemp$Lured=="Yes"]))/mean(sign(dtemp$count.all[dtemp$Lured=="No"]))  # Lure effect on presence/absence
    pCount1<-sign(d2$count.all)/ifelse(d2$Lured=="Yes",lure1,1)
    pCount1<-pCount1/max(pCount1)  # In case lure effect is <1
    m.sc.pa<-list(NULL)
    wt1<-d2$wt  # Need to do this to use model.matrix below for plotting
    m.sc.pa[[1]]<-try(glm(pCount1~offset(qlogis(0.998*d2$p.pa.all+0.001))-1,data=d2,family="binomial",weights=wt1))

    m.sc.pa[[2]] <- try(update(m.sc.pa[[1]], .~. + MAT))
    m.sc.pa[[3]] <- try(update(m.sc.pa[[1]], .~. + FFP))
    m.sc.pa[[4]] <- try(update(m.sc.pa[[1]], .~. + TD))
    m.sc.pa[[5]] <- try(update(m.sc.pa[[1]], .~. + MAT + TD))
    m.sc.pa[[6]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD))
    m.sc.pa[[7]] <- try(update(m.sc.pa[[1]], .~. + MAP))
    #m.sc.pa[[8]] <- try(update(m.sc.pa[[1]], .~. + MAS + PAS))
    m.sc.pa[[9]] <- try(update(m.sc.pa[[1]], .~. + CMD))
    m.sc.pa[[10]] <- try(update(m.sc.pa[[1]], .~. + AHM + SHM))
    m.sc.pa[[11]] <- try(update(m.sc.pa[[1]], .~. + CMD + SHM + Eref))
    m.sc.pa[[12]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + MAP))
    #m.sc.pa[[13]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + MAS + PAS))
    #m.sc.pa[[14]] <- try(update(m.sc.pa[[1]], .~. + MAS + PAS + CMD + Eref))
    m.sc.pa[[15]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + CMD + SHM + Eref))
    m.sc.pa[[16]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + MAP + CMD + SHM + Eref))

    m.sc.pa[[17]] <- try(update(m.sc.pa[[1]], .~. + Easting + Northing + Easting * Northing))
    m.sc.pa[[18]] <- try(update(m.sc.pa[[1]], .~. + Easting + Northing + Easting * Northing + Easting^2 + Northing^2))

    #		for (i in 1:19) m.sc.pa[[i+19]]<-update(m.sc.pa[[i]],.~.+NSR1)
    # Models with spatial and climate variables together not currently used, because highly correlated
    nModels.sc.pa<-length(m.sc.pa)
    # BIC calculation to select best covariate set Uses BIC for more conservative variable set
    bic.sc.pa<-rep(999999999,nModels.sc.pa)
    for (i in 1:(nModels.sc.pa)) {
      if (!is.null(m.sc.pa[[i]]) & class(m.sc.pa[[i]])[1]!="try-error") {
        bic.sc.pa[i]<-BIC(m.sc.pa[[i]])
      }
    }
    best.model.sc.pa<-which.min(bic.sc.pa)
    # Then 150m veg models
    #		m.150m.pa<-list(NULL)
    #		m.150m.pa[[1]]<-try(glm(pCount1~offset(qlogis(0.998*d2$p.pa.all+0.001))-1,data=d2,family="binomial",weights=wt1))
    #		m.150m.pa[[2]]<-try(update(m.150m.pa[[1]],.~.+xTHF))
    #		m.150m.pa[[3]]<-try(update(m.150m.pa[[1]],.~.+xAlien+xSucc))
    #		m.150m.pa[[4]]<-try(update(m.150m.pa[[1]],.~.+xLin+xNonlin))
    #		m.150m.pa[[5]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland))
    #		m.150m.pa[[6]]<-try(update(m.150m.pa[[1]],.~.+xUpCon+xDecid+xUpOpen+xLowland))
    #		m.150m.pa[[7]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowTreed+xLowOpen))
    #		m.150m.pa[[8]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland+xOld+xYoung))
    #		m.150m.pa[[9]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland+xAlien))
    #		m.150m.pa[[10]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland+xLin))
    #		# BIC calculation to select best covariate set Uses BIC for more conservative variable set
    #		nModels.150m.pa<-length(m.150m.pa)
    #		bic.150m.pa<-rep(999999999,nModels.150m.pa)
    #		for (i in 1:(nModels.150m.pa)) {
    #			if (!is.null(m.150m.pa[[i]]) & class(m.150m.pa[[i]])[1]!="try-error") {
    #				bic.150m.pa[i]<-BIC(m.150m.pa[[i]])
    #			}
    #		}
    #		best.model.150m.pa<-which.min(bic.150m.pa)

    #		# Check combined space/climate + 150m veg+HF model
    #		f<-paste(". ~ +",paste(attr(m.sc.pa[[best.model.sc.pa]]$terms,"term.labels"),collapse="+"),"+",paste(attr(m.150m.pa[[best.model.150m.pa]]$terms,"term.labels"),collapse="+") )
    #		f<-paste(f,"+ offset(qlogis(0.998 * d2$p.pa.all + 0.001)) - 1",sep="")
    #		m.combo<-try(update(m.sc.pa[[1]],formula=f))
    #		bic.combo<-c(bic.sc.pa[best.model.sc.pa],bic.150m.pa[best.model.150m.pa],BIC(m.combo))

    #		# Store BIC, residual variance for best models; save 150m model.  For latter summarizing variance components, which options were best for each species, etc.
    #		# For species using AGP models, these values will be overwritten in that section
    #		bic.sc150m[sp,]<-c(bic.150m.pa[1],bic.sc.pa[best.model.sc.pa],bic.150m.pa[best.model.150m.pa],BIC(m.combo))
    #		m0<-glm(pCount1~1,family="binomial",data=d2,weights=wt1)  # Null with no offset
    #		var.sc150m[sp,]<-c(summary(m0)$deviance, summary(m.sc.pa[[1]])$deviance, summary(m.sc.pa[[best.model.sc.pa]])$deviance, summary(m.150m.pa[[best.model.150m.pa]])$deviance, summary(m.combo)$deviance)
    #		best.model.sc150m[sp,]<-c(best.model.sc.pa,best.model.150m.pa)
    #		fname<-paste(fname.150mVegModels," ",SpTable[sp],".Rdata",sep="")
    #		m.sc.best<-m.sc.pa[[best.model.sc.pa]]
    #		m.150m.best<-m.150m.pa[[best.model.150m.pa]]
    #		save(file=fname,m.sc.best,m.150m.best,m.combo)
  }  # End if for sc.option=1 or 2

  # Do AUC of fit here, before the total abundance option is run for option 1
  if (sc.option[sp]==1 | sc.option[sp]==2) p<-plogis(predict(m.sc.pa[[best.model.sc.pa]]))
  if (sc.option[sp]==3) p<-plogis(d2$p.pa.all)  # The original veg+HF only prediction if there is no sc model (this prediction is on the ordinal scale, despite the "t." in the name...)
  auc.fit[sp]<-auc(roc(sign(d2$count.all),p))  # No correction for lure here.

  if (sc.option[sp]==1) { # Model full total abundance.
    # Climate and spatial variables sets are tried
    UseForLure<-grepl("^[[:digit:]]+",d2$location)  # Only use ongrid sites for lure calibration (paired lure/not).  These are now identified by locations starting with digits
    dtemp<-d2[UseForLure==TRUE,]  # To calculate lure effect only with ABMI paired'ish sites
    lure1<-mean(dtemp$count.all[dtemp$Lured=="Yes"])/mean(dtemp$count.all[dtemp$Lured=="No"])  # Lure effect on total abundance
    pCount1<-d2$count.all/ifelse(d2$Lured=="Yes",lure1,1)
    log.offset<-min(pCount1[pCount1>0])/2
    pCount1<-log(pCount1+log.offset)  # Using log(x+offset)
    log.adj<-mean(log(d2$p.all+log.offset)-pCount1)  # Compensate for geometric mean issue
    pCount1<-pCount1+log.adj
    m.sc.pa<-list(NULL)
    wt1<-d2$wt  # Need to do this to use model.matrix below for plotting
    m.sc.pa[[1]]<-try(glm(pCount1~offset(log(d2$p.all+log.offset))-1,data=d2,family=gaussian,weights=wt1))

    m.sc.pa[[2]] <- try(update(m.sc.pa[[1]], .~. + MAT))
    m.sc.pa[[3]] <- try(update(m.sc.pa[[1]], .~. + FFP))
    m.sc.pa[[4]] <- try(update(m.sc.pa[[1]], .~. + TD))
    m.sc.pa[[5]] <- try(update(m.sc.pa[[1]], .~. + MAT + TD))
    m.sc.pa[[6]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD))
    m.sc.pa[[7]] <- try(update(m.sc.pa[[1]], .~. + MAP))
    #m.sc.pa[[8]] <- try(update(m.sc.pa[[1]], .~. + MAS + PAS))
    m.sc.pa[[9]] <- try(update(m.sc.pa[[1]], .~. + CMD))
    m.sc.pa[[10]] <- try(update(m.sc.pa[[1]], .~. + AHM + SHM))
    m.sc.pa[[11]] <- try(update(m.sc.pa[[1]], .~. + CMD + SHM + Eref))
    m.sc.pa[[12]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + MAP))
    #m.sc.pa[[13]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + MAS + PAS))
    #m.sc.pa[[14]] <- try(update(m.sc.pa[[1]], .~. + MAS + PAS + CMD + Eref))
    m.sc.pa[[15]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + CMD + SHM + Eref))
    m.sc.pa[[16]] <- try(update(m.sc.pa[[1]], .~. + MAT + FFP + TD + MAP + CMD + SHM + Eref))

    m.sc.pa[[17]] <- try(update(m.sc.pa[[1]], .~. + Easting + Northing + Easting * Northing))
    m.sc.pa[[18]] <- try(update(m.sc.pa[[1]], .~. + Easting + Northing + Easting * Northing + Easting^2 + Northing^2))

    #		for (i in 1:19) m.sc.pa[[i+19]]<-update(m.sc.pa[[i]],.~.+NSR1)
    # Models with spatial and climate variables together not currently used, because highly correlated
    nModels.sc<-length(m.sc.pa)
    # BIC calculation to select best covariate set Uses BIC for more conservative variable set
    bic.sc.pa<-rep(999999999,nModels.sc)
    for (i in 1:(nModels.sc)) {
      if (!is.null(m.sc.pa[[i]]) & class(m.sc.pa[[i]])[1]!="try-error") {
        bic.sc.pa[i]<-BIC(m.sc.pa[[i]])
      }
    }
    best.model.sc.pa<-which.min(bic.sc.pa)
    #		# Then 150m veg models
    #		m.150m.pa<-list(NULL)
    #		m.150m.pa[[1]]<-try(glm(pCount1~offset(log(d2$p.all+log.offset))-1,data=d2,family=gaussian,weights=wt1))
    #		m.150m.pa[[2]]<-try(update(m.150m.pa[[1]],.~.+xTHF))
    #		m.150m.pa[[3]]<-try(update(m.150m.pa[[1]],.~.+xAlien+xSucc))
    #		m.150m.pa[[4]]<-try(update(m.150m.pa[[1]],.~.+xLin+xNonlin))
    #		m.150m.pa[[5]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland))
    #		m.150m.pa[[6]]<-try(update(m.150m.pa[[1]],.~.+xUpCon+xDecid+xUpOpen+xLowland))
    #		m.150m.pa[[7]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowTreed+xLowOpen))
    #		m.150m.pa[[8]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland+xOld+xYoung))
    #		m.150m.pa[[9]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland+xAlien))
    #		m.150m.pa[[10]]<-try(update(m.150m.pa[[1]],.~.+xUpland+xLowland+xLin))
    #		# BIC calculation to select best covariate set Uses BIC for more conservative variable set
    #		nModels.150m.pa<-length(m.150m.pa)
    #		bic.150m.pa<-rep(999999999,nModels.150m.pa)
    #		for (i in 1:(nModels.150m.pa)) {
    #			if (!is.null(m.150m.pa[[i]]) & class(m.150m.pa[[i]])[1]!="try-error") {
    #				bic.150m.pa[i]<-BIC(m.150m.pa[[i]])
    #			}
    #		}
    #		best.model.150m.pa<-which.min(bic.150m.pa)

    #		# Check combined space/climate + 150m veg+HF model
    #		f<-paste(". ~ +",paste(attr(m.sc.pa[[best.model.sc.pa]]$terms,"term.labels"),collapse="+"),"+",paste(attr(m.150m.pa[[best.model.150m.pa]]$terms,"term.labels"),collapse="+") )
    #		f<-paste(f,"+ offset(log(d2$p.all+log.offset)) - 1",sep="")
    #		m.combo<-try(update(m.sc.pa[[1]],formula=f))
    #		bic.combo<-c(bic.sc.pa[best.model.sc.pa],bic.150m.pa[best.model.150m.pa],BIC(m.combo))

    #		# Store BIC, residual variance for best models; save 150m model
    #		bic.sc150m[sp,]<-c(bic.150m.pa[1],bic.sc.pa[best.model.sc.pa],bic.150m.pa[best.model.150m.pa],BIC(m.combo))
    #		m0<-glm(pCount1~1,family=gaussian,data=d2,weights=wt1)
    #		var.sc150m[sp,]<-c(summary(m0)$deviance, summary(m.sc.pa[[1]])$deviance, summary(m.sc.pa[[best.model.sc.pa]])$deviance, summary(m.150m.pa[[best.model.150m.pa]])$deviance, summary(m.combo)$deviance)
    #		best.model.sc150m[sp,]<-c(best.model.sc.pa,best.model.150m.pa)
    #		fname<-paste(fname.150mVegModels," ",SpTable[sp],".Rdata",sep="")
    #		m.sc.best<-m.sc.pa[[best.model.sc.pa]]
    #		m.150m.best<-m.150m.pa[[best.model.150m.pa]]
    #		save(file=fname,m.sc.best,m.150m.best,m.combo)
  }  # End if for sc.option==1

  # No models if option=3

  # 6.2. Change models if necessary when a non-best model is more accurate than the best model - CHECK ALL WITH NEW DATA
  if (SpTable[sp]=="MuleDeer") best.model.sc.pa<-11
  if (SpTable[sp]=="Wolverine") best.model.sc.pa<-2
  if (SpTable[sp]=="WhitetailedJackRabbit") best.model.sc.pa<-2
  if (SpTable[sp]=="RedFox") best.model.sc.pa<-8  # Difficult spatial distribution - this gives soft approximation to overall pattern without extreme values at edges (mountains)
  if (SpTable[sp]=="GrayWolf") best.model.sc.pa<-1  # None give reasonable patterns for this widespread but rare species
  if (SpTable[sp]=="SnowshoeHare") best.model.sc.pa<-2
  if (SpTable[sp]=="WoodlandCaribou") best.model.sc.pa<-3  # The best and other good model over-predict the undersampled NW.  This option is less bad.  Shouldn't really be showing that part of the province yet...

  c1<-coef(m.sc.pa[[best.model.sc.pa]])  # Variable names in best sc model

  # And post-hoc modifications of coefficients when necessary - CHECK ALL WITH NEW DATA
  if (SpTable[sp]=="GrizzlyBear") c1<-c(c1,"NSR1North"= -16, "NSR1Shield"= -16, "NSR1CentralMixedwood" = -2)  # Add to (partially) censor those NSR1's
  if (SpTable[sp]=="Wolverine") c1<-c1/3  # This makes the spatial modeling less extreme for this rarely detected species
  if (SpTable[sp]=="Moose") c1<-c1/2  # This makes the spatial modeling less extreme for this species
  if (SpTable[sp]=="SnowshoeHare") c1<-c1/2  # This makes the spatial modeling less extreme for this species

  # 6.3 Save coefficients for the subset of climate and/or spatial variables
  #if (sc.option[sp]==1 | sc.option[sp]==2) {
    vnames<-names(c1)
    vnames1<-ifelse(vnames=="(Intercept)","Intercept",vnames)  # This is for the names used in km2.sc (where "(", "^", etc. can't be used)
    vnames1<-ifelse(vnames1=="I(Lat^2)","Lat2",vnames1)
    vnames1<-ifelse(vnames1=="I(Lat^3)","Lat3",vnames1)
    vnames1<-ifelse(vnames1=="I(Long^2)","Long2",vnames1)
    vnames1<-ifelse(vnames1=="I(MAT * (MAT + 10))","MAT2",vnames1)
    vnames1<-ifelse(vnames1=="I(MWMT^2)","MWMT2",vnames1)
    vnames1<-ifelse(vnames1=="I(Lat^2 * Long^2)","Lat2Long2",vnames1)
    vnames1<-ifelse(vnames1=="I(Long * MAT)","LongMAT",vnames1)
    vnames1<-gsub(":","",vnames1)
    Res.coef[sp,match(vnames1,colnames(Res.coef))]<-c1
  #  if (length(c1)==0) Res.coef[sp,]<-0  # For case when models were fit, but best one was null
  #}
  # If option=3, the res coefs are already 0

  # 6.4 Mapping of residual climate/spatial effect
  # This is just the additional climate and spatial effect, not the basic veg type effects
  # Done in the original non-colour-blind-friendly way, since this is just for us
  if (sc.option[sp]==1 | sc.option[sp]==2) {
    vnames<-names(c1)  # Figure out the names of the included variables in the km2 raster data frame
    vnames<-ifelse(vnames=="(Intercept)","Intercept",vnames)
    vnames<-ifelse(vnames=="I(Lat^2)","Lat2",vnames)
    vnames<-ifelse(vnames=="I(Lat^3)","Lat3",vnames)
    vnames<-ifelse(vnames=="I(Long^2)","Long2",vnames)
    vnames<-ifelse(vnames=="I(MAT * (MAT + 10))","MAT2",vnames)
    vnames<-ifelse(vnames=="I(MWMT^2)","MWMT2",vnames)
    vnames<-ifelse(vnames=="I(Lat^2 * Long^2)","Lat2Long2",vnames)
    vnames<-ifelse(vnames=="I(Long * MAT)","LongMAT",vnames)
    vnames<-gsub(":","",vnames)
    km2.sc1<-km2.sc[,vnames]
    if (sc.option[sp]==1) km2.p1<-exp(rowSums( t(c1*t(km2.sc1))))   # Predictions from just the climate and spatial part of the residual model for each km2 raster - log-link total abundance
    if (sc.option[sp]==2) km2.p1<-plogis(rowSums( t(c1*t(km2.sc1))))   # Predictions from just the climate and spatial part of the residual model for each km2 raster - logit-link presence/absence
    km2.p1<-ifelse(km2.p1>quantile(km2.p1,0.99),quantile(km2.p1,0.99),km2.p1)
    km2.p1<-km2.p1/max(km2.p1)
    if (max(km2.p1)==min(km2.p1)) {
      km2.p1<-rep(0.5,length(km2.p1))  # make all white map if null model is best
    } else {
      km2.p1<-(km2.p1-min(km2.p1))/(max(km2.p1)-min(km2.p1))
    }
    r<-ifelse(km2.p1<0.5,1,(1-km2.p1)*2)[!is.na(km2.p1)]  # RGB for map (white at no change, more green for more positive residual effect, more red for more negative
    g<-ifelse(km2.p1>0.5,1,km2.p1*2)[!is.na(km2.p1)]  # RGB for map
    b<-(1-abs(km2.p1-0.5)*2)[!is.na(km2.p1)]  # RGB for map
    fname<-paste(fname.map,"Climate and spatial/New/",SpTable[sp],".jpg",sep="")
    jpeg(filename=fname,width=5,height=8.7,units="in",res=300)
    plot(km2.sc$Long[!is.na(km2.p1)],km2.sc$TrueLat[!is.na(km2.p1)],pch=15,cex=0.4,col=rgb(r,g,b),xlab="",ylab="")
    points(km2.water$Long,km2.water$Lat,pch=15,cex=0.3,col=rgb(0.5,0.4,0.9))
    points(d1$Long,d1$TrueLat,cex=1.5*sqrt(d1$count.all)+0.2,col="yellow",lwd=2)  # Add data points, size proportional to count
    title(paste("Climate+spatial",sp.names[sp]))
    graphics.off()
  }

  # 6.5 Full maps for North
  # This includes veg types, surrounding HF and additional climate and spatial effects
  # Prediction based on veg types only.  Note: water, barren and mines not included, so they are treated as 0.
  # km2.1 is the full km2 truncated to the sampled polygon
  km2.pveg<-colSums(Coef.mean.all[sp,]*t(km2.1[,colnames(Coef.mean.all)]))
  # Prediction of residual effect (note: Uses truncated latitude "Lat", but point is plotted at true latitude)
  km2.pres<-colSums(Res.coef[sp,]*t(km2.sc[,colnames(Res.coef)]))
  # Prediction based on non-HF veg types only for reference. .b.1 is the full km2.b truncated to the sampled polygon
  km2.pveg.ref<-colSums(Coef.mean.all[sp,vnames.b]*t(km2.b.1[,vnames.b]))
  # Prediction of residual effect (note: Uses truncated latitude "Lat", but point is plotted at true latitude)
  km2.pres.ref<-colSums(Res.coef[sp,]*t(km2.sc[,colnames(Res.coef)]))

  # Use this also for no model - all 0 coefficients become 1 multipliers on exp scale
  if (sc.option[sp]==1 | sc.option[sp]==3) {
    # Using simple multiplication of residual effect, to avoid offset problems
    km2.p<-km2.pveg*exp(km2.pres)
    # Using simple multiplication of residual effect, to avoid offset problems - here, for log-linked residual total abundance predictions
    km2.p.ref<-km2.pveg.ref*exp(km2.pres.ref)
  }

  # Logit scale, and need to extract veg presence/absence and AGP components
  if (sc.option[sp]==2) {
    # Presence/absence prediction based on veg types only.  Note: water, barren and mines not included, so they are treated as 0.
    # km2.1 is the full km2 truncated to the sampled polygon
    km2.pveg.pa<-colSums(Coef.pa.all[sp,]*t(km2.1[,colnames(Coef.pa.all)]))
    # This is the abundance-given-presence prediction for each km2 raster, to be multiplied by the sc-adjusted presence/absence
    km2.pveg.agp<-ifelse(km2.pveg.pa<0.0001,0,km2.pveg / km2.pveg.pa)
    # Using simple multiplication of residual effect, to avoid offset problems - here, for logit-linked residual presence/absence predictions.
    # And multiply by agp to get total abundance.
    km2.p<-plogis(qlogis(0.998*km2.pveg.pa+0.001)+km2.pres) * km2.pveg.agp
    # This is if the prediction is 0
    km2.p<-ifelse(is.na(km2.p),0,km2.p)
    # And repeat for reference
    km2.pveg.pa.ref<-colSums(Coef.pa.all[sp,vnames.b]*t(km2.b.1[,vnames.b]))   # Presence/absence prediction based on veg types only.  Note: water, barren and mines not included, so they are treated as 0.  km2.b.1 is the full km2.b truncated to the sampled polygon
    km2.pveg.agp.ref<-ifelse(km2.pveg.pa.ref<0.0001,0,km2.pveg.ref/km2.pveg.pa.ref)  # This is the abundance-given-presence prediction for each km2 raster, to be multiplied by the sc-adjusted presence/absence
    km2.p.ref<-plogis(qlogis(0.998*km2.pveg.pa.ref+0.001)+km2.pres.ref) * km2.pveg.agp.ref  # Using simple multiplication of residual effect, to avoid offset problems - here, for logit-linked residual presence/absence predictions. And multiply by agp to get total abundance.
    km2.p.ref<-ifelse(is.na(km2.p.ref),0,km2.p.ref)  # This is if the prediction is 0
  }

  x.ref1<-km2.p.ref  # Colour gradient direct with predicted abundance
  x.curr1<-km2.p
  #	x.curr1<-ifelse(x.curr1<min(pCount),min(pCount),x.curr1)  # Some extreme low values otherwise
  #	x.ref1<-ifelse(x.ref1<min(pCount),min(pCount),x.ref1)
  x.ref.trunc<-ifelse(x.ref1>quantile(c(x.ref1,x.curr1),0.99,na.rm=T),quantile(c(x.ref1,x.curr1),0.99,na.rm=T),x.ref1)  # Clip to 99 percentile, to prevent colour scaling problems with a few high values
  x.curr.trunc<-ifelse(x.curr1>quantile(c(x.ref1,x.curr1),0.99,na.rm=T),quantile(c(x.ref1,x.curr1),0.99,na.rm=T),x.curr1)  # Clip to 99 percentile, to prevent colour scaling problems with a few high values
  x.curr<-x.curr.trunc/max(c(x.ref.trunc,x.curr.trunc),na.rm=T)
  x.ref<-x.ref.trunc/max(c(x.ref.trunc,x.curr.trunc),na.rm=T)
  # 6.5.1 Current
  c3<-c2(1000)[1+999*x.curr]
  fname<-paste(fname.map,"Current/",SpTable[sp],".jpg",sep="")
  jpeg(file=fname,width=600,height=1000)
  plot(km2.sc$proj.x,km2.sc$proj.y,pch=15,cex=0.2,col=c3,xaxt="n",yaxt="n",xlab="",ylab="",bty="n",xlim=range(c(km2.sc$proj.x,m.water$x)),ylim=range(c(km2.sc$proj.y,m.water$y)))
  points(m.water$x,m.water$y,pch=15,cex=0.2,col=rgb(0.4,0.3,0.8))
  #points(km2.sc$proj.x[km2$NatRegion=="Rocky Mountain"],km2.sc$proj.y[km2$NatRegion=="Rocky Mountain"],pch=15,cex=0.2,col="lightcyan4")
  #text(-0.025,-0.745,"Insufficient \n   data",col="white",cex=0.9)
  mtext(side=3,at=m.title$x,paste(sp.names[sp],"Current"),adj=0.5,cex=1.4,col="grey40")
  mtext(side=3,at=m.title$x,line=-1,paste("Detected at",sum(sign(d1$count.all)),"of",nrow(d),"camera locations"),adj=0.5,cex=1.2,col="grey40")
  points(m.city.x,m.city.y,pch=18,col="grey10")
  text(m.city.x,m.city.y,city,cex=0.8,adj=-0.1,col="grey10")
  graphics.off()
  # 6.5.2 Reference
  c3<-c2(1000)[1+999*x.ref]
  fname<-paste(fname.map,"Reference/",SpTable[sp],".jpg",sep="")
  jpeg(file=fname,width=600,height=1000)
  plot(km2.sc$proj.x,km2.sc$proj.y,pch=15,cex=0.2,col=c3,xaxt="n",yaxt="n",xlab="",ylab="",bty="n",xlim=range(c(km2.sc$proj.x,m.water$x)),ylim=range(c(km2.sc$proj.y,m.water$y)))
  points(m.water$x,m.water$y,pch=15,cex=0.2,col=rgb(0.4,0.3,0.8))
  #points(km2$proj.x[km2$NatRegion=="Rocky Mountain"],km2$proj.y[km2$NatRegion=="Rocky Mountain"],pch=15,cex=0.2,col="lightcyan4")
  #text(-0.025,-0.745,"Insufficient \n   data",col="white",cex=0.9)
  mtext(side=3,at=m.title$x,paste(sp.names[sp],"Reference"),adj=0.5,cex=1.4,col="grey40")
  mtext(side=3,at=m.title$x,line=-1,paste("Detected at",sum(sign(d1$count.all)),"of",nrow(d),"camera locations"),adj=0.5,cex=1.2,col="grey40")
  points(m.city.x,m.city.y,pch=18,col="grey10")
  text(m.city.x,m.city.y,city,cex=0.8,adj=-0.1,col="grey10")
  graphics.off()
  # 6.5.3 Difference
  trunc<-quantile(c(km2.p,km2.p.ref),0.99,na.rm=T)
  diff<-(ifelse(km2.p>trunc,trunc,km2.p)-ifelse(km2.p.ref>trunc,trunc,km2.p.ref))/trunc
  diff<-sign(diff)*abs(diff)^0.5
  d3<-dcol2(1000)[500+499*diff]
  fname<-paste(fname.map,"Difference/",SpTable[sp],".jpg",sep="")
  jpeg(file=fname,width=600,height=1000)
  plot(km2.sc$proj.x,km2.sc$proj.y,pch=15,cex=0.2,col=d3,xaxt="n",yaxt="n",xlab="",ylab="",bty="n",xlim=range(c(km2.sc$proj.x,m.water$x)),ylim=range(c(km2.sc$proj.y,m.water$y)))
  points(m.water$x,m.water$y,pch=15,cex=0.2,col=rgb(0.4,0.3,0.8))
  #points(km2$proj.x[km2$NatRegion=="Rocky Mountain"],km2$proj.y[km2$NatRegion=="Rocky Mountain"],pch=15,cex=0.2,col="lightcyan4")
  #text(-0.025,-0.745,"Insufficient \n   data",col="white",cex=0.9)
  mtext(side=3,at=m.title$x,paste(sp.names[sp],"Difference"),adj=0.5,cex=1.4,col="grey40")
  mtext(side=3,at=m.title$x,line=-1,paste("Detected at",sum(sign(d1$count.all)),"of",nrow(d),"camera locations"),adj=0.5,cex=1.2,col="grey40")
  points(m.city.x,m.city.y,pch=18,col="grey10")
  text(m.city.x,m.city.y,city,cex=0.8,adj=-0.1,col="grey10")
  graphics.off()

  # 7. Output by species
  # km2 raster reference and current abundance
  q<-data.frame(LinkID=km2.1$LinkID,Ref=km2.p.ref,Curr=km2.p)
  fname<-paste(fname.km2summaries," ",SpTable[sp],".csv",sep="")
  write.table(q,file=fname,sep=",",row.names=FALSE)

  #  Save AIC wts for each species - models themselves not being saved in R format, because haven't been using
  fname<-paste(fname.Robjects," ",SpTable[sp],".Rdata",sep="")
  save(file=fname,aic.wt.pa.save,aic.wt.agp.save,aic.age,aic.wt.age.models.save,aic.wt.age.save)

}  # Next species

#-----------------------------------------------------------------------------------------------------------------------






