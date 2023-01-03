# R Make table of camera deployments in files needed for habitat modeling
# Includes look-up for new deployment-project naming and old deployment-year system

root <- "G:/Shared drives/ABMI Camera Mammals/"

d.dens<-read.csv(paste0(root, "results/density/deployments/abmi-cmu-nwsar-bg_all-years_density_wide_2021-11-04.csv",stringsAsFactors=FALSE)  # Wideform density for all deployments.  Compiled from separate files provided by Marcus
d.pveg<-read.csv("C:/Dave/ABMI/Data/Site info/2021/abmi-cmu-nwsar-bg_all-years_veghf-soilhf-detdistveg_2021-10-06.csv",stringsAsFactors=FALSE)  # Point veg file, including manual corrections.  Updated by Marcus from previous May 2020 version
d.lure<-read.csv("C:/Dave/ABMI/Data/Mammals/2021/abmi-cmu-nwsar-bg_all-years_lure_2021-11-04.csv",stringsAsFactors=FALSE)  # Yes/No lure status. From Marcus
d.loc<-read.csv("C:/Dave/ABMI/Data/Mammals/2021/Deployment locations all Nov 2021.csv",stringsAsFactors=FALSE)  # Deployment locations (for nearest ABMI site). To be updated
d.veg150<-read.csv("C:/Dave/ABMI/Data/Site info/2021/abmi-nwsar-bg_all-years_veghf-150m-buffer_2021-11-04.csv",stringsAsFactors=FALSE)  # 150m veg+HF.  Processed at end of step 1 modeling script, based on files from Peter
#d.soil150<-read.csv("C:/Dave/ABMI/Data/Site info/2020/Camera ARU 150m veg+HF from Peter converted to standard deployment names.csv",stringsAsFactors=FALSE)  # 150m soil+HF.  Processed at end of step 1 modeling script, based on files from Peter
d.lf<-read.csv("C:/Dave/ABMI/Data/Site info/2021/OffGridSites_AB_land_facets_20210804.csv",stringsAsFactors=FALSE)  # Land facets info from Eric (not just off-grids, despite file name)

# Make standard columns for "location" (used to be "Deployment" for camera files and various names from GIS/Peter), "project", and "year" (used to be "Year")
d.dens$year<-as.numeric(substr(d.dens$project,nchar(d.dens$project)-3,nchar(d.dens$project)))
d.dens$year<-ifelse(d.dens$project=="Northwest Species at Risk Program",2020,d.dens$year)  # Check for any still missing
d.dens$location_year<-paste(d.dens$location,d.dens$year,sep="_")  # To be able to link to files that don't have "project"

d.pveg$year<-as.numeric(substr(d.pveg$project,nchar(d.pveg$project)-3,nchar(d.pveg$project)))  # Check that this covers all
d.pveg$year<-ifelse(d.pveg$project=="Northwest Species at Risk Program",2020,d.pveg$year)  # Check for any still missing
d.pveg$location_year<-paste(d.pveg$location,d.pveg$year,sep="_")  # To be able to link to files that don't have "project"

d.lure$year<-as.numeric(substr(d.lure$project,nchar(d.lure$project)-3,nchar(d.lure$project)))  # Check that this covers all
d.lure$year<-ifelse(d.lure$project=="Northwest Species at Risk Program",2020,d.lure$year)  # Check for any still missing
d.lure$location_year<-paste(d.lure$location,d.lure$year,sep="_")  # To be able to link to files that don't have "project"

names(d.loc)[which(names(d.loc)=="Site.Name")]<-"location"
names(d.loc)[which(names(d.loc)=="Year")]<-"year"
d.loc$location<-ifelse(substr(d.loc$location,1,5)=="ABMI-",substr(d.loc$location,6,nchar(d.loc$location)),d.loc$location)  # Remove initial "ABMI-" because not used in density file locations (but leave in other cases, like "OG-ABMI-...")
# Do corrections for old/unique naming systems, where possible
d.loc$location<-gsub("-000","-",d.loc$location)
d.loc$location<-gsub("-00","-",d.loc$location)
d.loc$location<-gsub("-0","-",d.loc$location)
d.loc$location_year<-paste(d.loc$location,d.loc$year,sep="_")  # To be able to link to files that don't have "project"

d.veg150$year<-as.numeric(substr(d.veg150$project,nchar(d.veg150$project)-3,nchar(d.veg150$project)))  # Check that this covers all
d.veg150$year<-ifelse(d.veg150$project=="Northwest Species at Risk Program",2020,d.veg150$year)  # Check for any still missing
d.veg150$location_year<-paste(d.veg150$location,d.veg150$year,sep="_")  # To be able to link to files that don't have "project"

# d.soil150$year<-as.numeric(substr(d.soil150$project,nchar(d.soil150$project)-3,nchar(d.soil150$project)))  # Check that this covers all
# d.soil150$year<-ifelse(d.soil150$project=="Northwest Species at Risk Program",2020,d.soil150$year)  # Check for any still missing
# d.soil150$location_year<-paste(d.soil150$location,d.soil150$year,sep="_")  # To be able to link to files that don't have "project"

names(d.lf)[which(names(d.lf)=="Site")]<-"location"
names(d.lf)[which(names(d.lf)=="Year")]<-"year"
d.lf$location_year<-paste(d.lf$location,d.lf$year,sep="_")  # To be able to link to files that don't have "project"

# Then make a table of all deployment_year values, with associated project when available, and record which deployment_years occur in which files
# First, add "project" to location_year, where that is known
dy.all<-sort(unique(c(d.dens$location_year,d.pveg$location_year,d.lure$location_year,d.loc$location_year,d.veg150$location_year,d.lf$location_year)))  # d.soil150$location_year not included
dy.all<-ifelse(substr(dy.all,1,1)=="0",substr(dy.all,2,nchar(dy.all)),dy.all)  # Strip off leading 0's because not used in some formats and hard to add in if not used
dy.all<-ifelse(substr(dy.all,1,1)=="0",substr(dy.all,2,nchar(dy.all)),dy.all)  # Strip off leading 0's because not used in some formats and hard to add in if not used
dy.all<-ifelse(substr(dy.all,1,1)=="0",substr(dy.all,2,nchar(dy.all)),dy.all)  # Strip off leading 0's because not used in some formats and hard to add in if not used
dy.all<-sort(unique(dy.all))
d<-data.frame(location_year=dy.all,location=substr(dy.all,1,nchar(dy.all)-5),year=substr(dy.all,nchar(dy.all)-3,nchar(dy.all)))
dy.proj1<-d.dens$project[match(d$location_year,d.dens$location_year)]  # Compile project names from three files with this info
dy.proj2<-d.pveg$project[match(d$location_year,d.pveg$location_year)]  # Compile project names from three files with this info
dy.proj3<-d.lure$project[match(d$location_year,d.lure$location_year)]  # Compile project names from three files with this info
dy.proj<-ifelse(is.na(dy.proj1),ifelse(is.na(dy.proj2),dy.proj3,dy.proj2),dy.proj1)  # Use whichever is not NA
d$project<-dy.proj
# Then tabulate which files have which deployment years
d$Dens<-ifelse(is.na(match(d$location_year,d.dens$location_year)),"","Y")
d$PVeg<-ifelse(is.na(match(d$location_year,d.pveg$location_year)),"","Y")
d$Lure<-ifelse(is.na(match(d$location_year,d.lure$location_year)),"","Y")
d$Loc<-ifelse(is.na(match(d$location_year,d.loc$location_year)),"","Y")
d$Veg150<-ifelse(is.na(match(d$location_year,d.veg150$location_year)),"","Y")
# d$Soil150<-ifelse(is.na(match(d$location_year,d.soil150$location_year)),"","Y")
d$LF<-ifelse(is.na(match(d$location_year,d.lf$location_year)),"","Y")
# Don't need location for deployments that have an ABMI site number in their name, so fill that in.
d$Loc<-ifelse( (!is.na(as.numeric(substr(d$location,1,1))) | substr(d$location,1,2)=="OG") & d$Loc=="","(Y)",d$Loc)  # Mark those as "(Y)"
# Get rid of locations with veg150 or soil150 or land facet info that don't have densities, point veg, or lure info - these are likely just ARU-only locations
i<-which((d$Veg150=="Y" |  d$LF=="Y") & (d$Dens=="" & d$PVeg=="" & d$Lure==""))  # d$Soil150=="Y" | omitted
d<-d[-i,]

# Some summaries
x1<-d[,5:10]
x1$Loc<-ifelse(x1$Loc=="(Y)","Y",x1$Loc)
x<-rowSums(x1=="Y")
table(x)
#d$location_year[x==1]  # Check that these are deployments that are not being used
d<-d[x>1,]  # Then delete those
x<-x[x>1]

# Export (to pull out locations needing info in Excel)
write.table(d,file="C:/Dave/ABMI/Data/Mammals/2021/Location year project by data file for camera modeling Nov 2021.csv",sep=",",row.names=FALSE)



