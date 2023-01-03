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

# Root directory for 2022 analysis north results (Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/results/habitat-models/2022/analysis-north/"

# Set up file names for various outputs
fname.fig<-"figures/best-model/"  # Start of figure file, one per species for veg+HF.  Subdirectory, species name and extension will be added
fname.map<-"maps/"  # Start of map file, one per species for, spatial+climate map, current, reference and difference maps.  Subdirectory, species name and extension will be added.  Note: the web uses only whole-province maps, so naming here can be non-official
fname.sumout<-"coefficient-tables/Mammal coefficients North 2022 Best model"  # For exporting coefficient tables
fname.Robjects<-"R-objects/R objects North mammal coefficients 2022 Best model"  # Start of file name to save each species' models
fname.Robjects.sum<-"R-objects/R objects North mammals Coefficient tables 2022 Best model.Rdata"
fname.km2summaries<-"km2-summaries/best-model/Km2 North reference and current 2022" # Start of file name for km2 grid reference and current output for each species
fname.useavail<-"figures/use-availability/"  # Subdirectory for use-availability figures and table

km2file.out<-"G:/Shared drives/ABMI Camera Mammals/data/processed/km2-grid/km2-grid-north_current-backfilled_processed.Rdata"    # File with processed km2 grid files - processed in separate script.  Despite the variable name, this is an input here.
fname.150mVegModels<-"R objects/Veg 150m best model"  # Start of file name to save best model using 150m veg as well as that model combined with best space/climate model (for separate graphing)

# File name for previously processed datafile+veg+HF, km2 grid info, SpTable, and look-up matrix for veg groups
dataset.out<-"G:/Shared drives/ABMI Camera Mammals/data/lookup/R Dataset SpTable for ABMI North mammal coefficients 2021.RData"
load(dataset.out)
SpTable.w<-SpTable.w[SpTable.w!="black_bearWinter"]  # Bears have enough occurrences in winter to meet criterion, but they are just flukey encounters - don't run winter models
SpTable<-sort(unique(c(gsub("Summer","",SpTable.s),gsub("Winter","",SpTable.w))))  # All modeled species, summer and/or winter
SpTable.ua<-sort(unique(c(gsub("Summer","",SpTable.s.ua),gsub("Winter","",SpTable.w.ua))))  # And the larger list for use-availability

# Specify space/climate ("sc") model options for each species in SpTable  1=based on total abundance, 2=based on presence/absence only, 3=none.  Based on results of cross-validation at site and regional level.
sc.option<-c(1,2,3,2,2,2,3,3,3,3,1,1,3,3,2,2,2,3,2,2,3,3)  # These are the decisions from the 2019, for the record - overruled in next line
sc.option<-rep(2,length(sc.option))  # Currently running all with sc based only on presence/absence, due to problems in AGP-based models
data.frame(SpTable,sc.option)  # Check that these are in right order (if different options are being used for different species!)

# Get nice species names
taxa.file<-read.csv("G:/Shared drives/ABMI Camera Mammals/data/lookup/Mammal taxa.csv")  # To get proper species names for figures, tables
sp.names<-as.character(taxa.file$Species[match(SpTable,taxa.file$Label)]) # These are nice names, not the condensed R names
sp.names.ua<-as.character(taxa.file$Species[match(SpTable.ua,taxa.file$Label)])  # For all species, including ones with only use/availablity figures
SeasName<-c("Summer","Winter")  # For labeling figures, etc.

# Figure parameters set up based on any possible variable in the veg+HF models
# Don't have this?
fp<-read.csv("C:/Dave/ABMI/Cameras/Coefficients/2020/Analysis north/Veg HF plot lookup.csv")  # Figure parameters, set up for all possible variables.  This has to be revised when different veg or HF groupings are added.
fp$col1<-as.character(fp$col1)

# Load processed km2 file
load(km2file.out)
kgrid_veg_current_north$Intercept<-1
kgrid_veg_reference_north$Intercept<-1
load("G:/Shared drives/ABMI Camera Mammals/data/processed/km2-grid/R object water dominated km2.rdata")  # Water-dominated km2 rasters to add to maps km2.water

# Set up km2 prediction matrices for space/climate mapping
km2.sc<-data.frame(km2[,c("Intercept","Lat","Long","TrueLat","AHM","PET","FFP","MAP","MAT","MCMT","MWMT","NSR1Parkland","NSR1DryMixedwood","NSR1CentralMixedwood","NSR1Foothills","NSR1North","NSR1Shield","NSR1Mountain")],
                   Lat2=km2$Lat^2,Lat3=km2$Lat^3,Long2=km2$Long^2,LatLong=km2$Lat*km2$Long,Lat2Long2=km2$Lat^2*km2$Long^2,LongMAT=km2$Long*km2$MAT,MAPPET=km2$MAP*km2$PET,MATAHM=km2$MAT*km2$AHM,MAPFFP=km2$MAP*km2$FFP,
                   MAT2=km2$MAT*(km2$MAT+10),MWMT2=km2$MWMT^2)  # km2.sc is for climate+space effects only - for diagnostic mapping of these effects.   Needs to include all variables used in the sc models.  The MAT2 transformation avoids problems where MAT<0.  Keep telling this to other people using the models, because they will forget every time.

DF.res.null<-data.frame(Intercept=1,Lat=mean(d$Lat),Long=mean(d$Long),AHM=mean(d$AHM),PET=mean(d$PET),FFP=mean(d$FFP),MAP=mean(d$MAP),MAT=mean(d$MAT),MCMT=mean(d$MCMT),MWMT=mean(d$MWMT),
                        Lat2=mean(d$Lat)^2,Long=mean(d$Long)^2,LatLong=mean(d$Lat)*mean(d$Long),MAPPET=mean(d$MAP)*mean(d$PET),MATAHM=mean(d$MAT)*mean(d$AHM),MAPFFP=mean(d$MAP)*mean(d$FFP),MAT2=mean(d$MAT*(d$MAT+10)),MWMT2=mean(d$MWMT)^2,
                        NSR1Parkland=1/7,NSR1DryMixedwood=1/7,NSR1CentralMixedwood=1/7,NSR1Foothills=1/7,NSR1North=1/7,NSR1Shield=1/7,NSR1Mountain=1/7)  # Means, used to plot residual relationships (including "average" NSR group)

# Truncate mapped area to just approximate polygon around sampled points.  Polygon determined by trial and error from map.  Needs to be changed every year
jpeg(file="C:/Dave/ABMI/Cameras/Coefficients/2021/Analysis north/Maps/Spatial projection area NORTH.jpg",height=600,width=400)
dplot(km2$Long,km2$TrueLat,pch=16,cex=0.3)  # Northern region
points(d$Long,d$TrueLat,pch=16,cex=1,col="red")  # Sampled points
poly.long<-c(-120.1,-120.1,-115,-112.5,-112.8,-120.1,-120.1)  # These are the longitudes of the polygon that bounds the area to be excluded from mapping - 2020 version
poly.lat<-c(52,56.5,58.2,58.5,60.1,60.1,52)  # And these are the corresponding latitudes
pip<-in.out(matrix(c(poly.long,poly.lat),c(length(poly.long),2)),matrix(c(km2$Long,km2$TrueLat),c(nrow(km2),2)))
# No spatial mask is being used here, with the (few) locations in the NW now.  Check how sensible the models lok for those areas
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
Coef.pa<-Coef.pa.se<-array(0,c(2,length(SpTable),length(vnames.pa)))  # Set up tables to store coefficients and their SE's - Season, species, variable
PooledStandCoef.pa<-PooledStandCoef.pa.se<-array(0,c(2,length(SpTable),5))  # Extra matrix for pooled coefficients for each species for 5 stand types that have ages estimated - Season, species, variable
dimnames(Coef.pa)<-dimnames(Coef.pa.se)<-list(c("Summer","Winter"),SpTable,vnames.pa)
dimnames(PooledStandCoef.pa)<-dimnames(PooledStandCoef.pa.se)<-list(c("Summer","Winter"),SpTable,c("Decid","Mixedwood","Pine","Spruce","TreedBog"))
vnames.agp<-c("Spruce","Pine","Decid","Mixedwood","TreedBog",
              "TreedFen","TreedSwamp","Shrub","GrassHerb","GraminoidFen","Marsh","ShrubbyBog","ShrubbyFen","ShrubbySwamp",
              "CCSpruceR","CCSpruce1","CCSpruce2","CCSpruce3","CCSpruce4","CCPineR","CCPine1","CCPine2","CCPine3","CCPine4","CCDecidR","CCDecid1","CCDecid2","CCDecid3","CCDecid4","CCMixedwoodR","CCMixedwood1","CCMixedwood2","CCMixedwood3","CCMixedwood4",
              "Crop","TameP","RoughP","Urban","Rural","Industrial","Well","EnSoftLin","EnSeismic","TrSoftLin","HardLin","WetlandMargin")  # Names of coefficients
Coef.agp<-Coef.agp.se<-array(0,c(2,length(SpTable),length(vnames.agp)))  # Set up tables to store coefficients and their SE's - Season, species, variable
dimnames(Coef.agp)<-dimnames(Coef.agp.se)<-list(c("Summer","Winter"),SpTable,vnames.agp)
Coef.mean<-Coef.lci<-Coef.uci<-array(0,c(2,length(SpTable),length(vnames.pa)))  # To store combined presence * abundance|presence estimates and their CI's (CI's because not symmetrical SE's) - Season, species, variable
dimnames(Coef.mean)<-dimnames(Coef.lci)<-dimnames(Coef.uci)<-list(c("Summer","Winter"),SpTable,vnames.pa)
Coef.pa.all<-Coef.pa.lci.all<-Coef.pa.uci.all<-Coef.mean.all<-Coef.lci.all<-Coef.uci.all<-array(0,c(length(SpTable),length(vnames.pa)))  # To store combined presence * abundance|presence estimates and their CI's (CI's because not symmetrical SE's) - average of Summer and Winter
colnames(Coef.pa.all)<-colnames(Coef.pa.lci.all)<-colnames(Coef.pa.uci.all)<-colnames(Coef.mean.all)<-colnames(Coef.lci.all)<-colnames(Coef.uci.all)<-vnames.pa
rownames(Coef.pa.all)<-rownames(Coef.pa.lci.all)<-rownames(Coef.pa.uci.all)<-rownames(Coef.mean.all)<-rownames(Coef.lci.all)<-rownames(Coef.uci.all)<-SpTable
vnames.sc<-c("Intercept","Lat","Long","LatLong","Lat2","Lat3","Long2","Lat2Long2","PET","AHM","MAT","FFP","MAP","MAPFFP","MAPPET","MATAHM","MWMT","MCMT","MWMT2","MAT2","LongMAT","NSR1Parkland","NSR1DryMixedwood","NSR1CentralMixedwood","NSR1Foothills","NSR1North","NSR1Shield","NSR1Mountain")
Res.coef<-array(0,c(length(SpTable),length(vnames.sc))) # Done for both seasons together
colnames(Res.coef)<-vnames.sc
rownames(Res.coef)<-SpTable
lure.pa<-lure.agp<-array(NA,c(2,length(SpTable)))  # To store lure coefficients for pres/abs, abundance | presence - Season, species
cutoff.pc.for.age<-0.1  # Set the cut-off for the minimum proportion of a stand type for a site to be included in the age analysis for a stand type (currently the same for each stand type - could be flexible).  Note: weighting is proportional to proportion of site, so low values can be used - those sites just won't contribute much.  Not that any of this matters with point veg+HF; I just kept it from my scripts for other taxa to accomodate any future modeling using areas surrounding cameras ...

# Arrays to save aic weights for each age model, and for which sets of models are best.
aic.age<-array(NA,c(2,length(SpTable),3))  # AIC for age models using 5 stand separately, 2 pairs + black spruce, all stands together - Season, species, age combination model
aic.wt.pa.save<-array(NA,c(2,length(SpTable),17))  # To save aic weights for each species, presence/absence - Season, species, model.  Need to change the third dimension if number of models changes
aic.wt.agp.save<-array(NA,c(2,length(SpTable),18))  # To save aic weights for each species, abundance|presence.  Include null. - Season, species, model.  Need to change the third dimension if number of models changes
dimnames(aic.wt.pa.save)[[1]]<-dimnames(aic.wt.agp.save)[[1]]<-c("Summer","Winter")
dimnames(aic.wt.pa.save)[[2]]<-dimnames(aic.wt.agp.save)[[2]]<-SpTable
aic.wt.age.save<-array(NA,c(2,length(SpTable),3))  # This saves the AIC weights for the 3 combining age models for each species. - Season, species, model
aic.wt.age.models.save<-array(NA,c(2,length(SpTable),8))  # This saves the AIC weights for the spline model (relative to the null) for each of the eight stand types or combinations - Season, species, model
dimnames(aic.wt.age.save)[[1]]<-dimnames(aic.wt.age.models.save)[[1]]<-c("Summer","Winter")
dimnames(aic.wt.age.save)[[2]]<-dimnames(aic.wt.age.models.save)[[2]]<-SpTable
dimnames(aic.wt.age.save)[[3]]<-c("Separate","Intermediate","Combined")
dimnames(aic.wt.age.models.save)[[3]]<-c("Spruce","Pine","Decid","Mixedwood","TreedBog","UpCon","DecidMixed","All")
auc.fit<-NULL  # AUC for the presence/absence fit, one value for each species

# Extra list of variables for direct summary of densities by veg+HF class.  Need to eliminate unsampled types.  This summary is just meant as a test of the reasonableness of the fitted coefficients.
vnames.sum<-vnames.pa[vnames.pa %in% names(d)]
n.sum<-colSums(sign(d[,vnames.sum]))
vnames.sum<-vnames.sum[which(colSums(d[,vnames.sum])>0)]  # A couple veg+HF types have columns, but no samples in used dataset
n.sum<-colSums(sign(d[,vnames.sum]))
which(colSums(sign(d[d$SummerDays>=10,vnames.sum]))==0)  # Check if any of these veg+HF types were not sampled in Summer
which(colSums(sign(d[d$WinterDays>=10,vnames.sum]))==0)  # Check if any of these veg+HF types were not sampled in winter
simple.sum.mean<-simple.sum.n<-simple.sum.se<-array(NA,c(2,length(SpTable),length(vnames.sum)))  # Season, species, veg+HF type
dimnames(simple.sum.mean)<-dimnames(simple.sum.n)<-dimnames(simple.sum.se)<-list(c("Summer","Winter"),SpTable,vnames.sum)

# Setup variables to save BIC and variance components for SC and 150m models
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




