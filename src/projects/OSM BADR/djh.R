#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (Oilsands Monitoring)

# Title:            Estimate Treatment Effects
# Description:

# Author:           Marcus Becker, David J. Huggard

# Previous scripts: None

# Last updated:     March 2024

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# From Marcus - summarized to densities (not by season anymore)
d1 <- read.csv(paste0(g_drive, "Results/Density/Deployments/og-sup_all-years_density_wide_2024-03-14.csv"), stringsAsFactors = FALSE)

# 2022 OSM BADR sites NOT to include
df_2022_bad <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/osm2022.csv")) |>
  select(project, location, Useable)

# Remove unusable sites - these are those that had camera failures in 2022
d1 <- d1 |>
  left_join(df_2022_bad, by = c("project", "location")) |>
  mutate(Useable = ifelse(is.na(Useable), TRUE, Useable)) |>
  filter(!Useable == FALSE) |>
  select(-Useable)

# Metadata treatment info for each OSM deployment in 2021
s <- read.csv(paste0(g_drive, "projects/osm-badr-site-selection/osm_2021_deployment-metadata.csv"),stringsAsFactors=FALSE)

# Lure information OSM 2022
lure_22 <- read_csv(paste0(g_drive, "Projects/OSM BADR/osm_2022_lure.csv"))

# Metadata treatment info for each OSM deployment in 2022
df_meta_22 <- read_csv(paste0(g_drive, "Projects/OSM BADR/osm_2022_deployment-metadata.csv")) |>
  mutate(camera = str_sub(location, -3, -1)) |>
  left_join(lure_22, by = "location") |>
  select(-notes)

s <- bind_rows(s, df_meta_22)

# Modified from MS summary, to include expansion factor to adjust for greater than expected year (=habitat, weather, timing, etc.) effects
lure<-read.csv(paste0(g_drive, "data/processed/lure/Lure effect from MS for OSM May 2022.csv"),stringsAsFactors=FALSE)

d2<-merge(s,d1)
d2$jem<-ifelse(d2$jem=="1.00E+01","1E1",d2$jem)  # Fix Excel stupidity
d2$jem<-ifelse(d2$jem=="1.00E+02","1E2",d2$jem)  # Fix Excel stupidity
d2$jem<-ifelse(d2$jem=="2.00E+01","2E1",d2$jem)  # Fix Excel stupidity
d2$jem<-ifelse(d2$jem=="2.00E+02","2E2",d2$jem)  # Fix Excel stupidity
d2$jem<-ifelse(d2$jem=="3.00E+01","3E1",d2$jem)  # Fix Excel stupidity
d2$fine_scale<-gsub(" metres","",d2$fine_scale)  # Skip the " metres" part
d2$fine_scale<-ifelse(d2$fine_scale=="200" | d2$fine_scale=="600","300",d2$fine_scale)  # Treat the oddball 200 and 600m distances as 300m

# Summarize number of cameras (before these are combined into replicates below) - BADR only here
#write.table(table(paste(d2$treatment,d2$vegetation),ifelse(is.na(d2$fine_scale),"NA",as.character(d2$fine_scale))), file="C:/Dave/ABMI/Cameras/2022 analysis/OSM 2022/Table of fine scale treatment by jem treatmentXveg including extra ABMI - n cameras BADR.csv",sep=",",col.names=NA)

# Add ABMI on-grid and off-grid deployments
dataset.out<-paste0(g_drive, "data/lookup/R Dataset SpTable for ABMI North mammal coefficients 2022.RData")  # Processed main camera density file
load(dataset.out)
s1<-read.csv(paste0(g_drive, "projects/osm-badr-site-selection/supplemental/final/supplemental-osm-treatments_EH_2022-06-16.csv"),stringsAsFactors=FALSE)  # On-grid (ecosystem health) deployments from Marcus with OSM treatment info
s2<-read.csv(paste0(g_drive, "projects/osm-badr-site-selection/supplemental/final/supplemental-osm-treatments_OG_2022-06-16.csv"),stringsAsFactors=FALSE)  # Off-grid (ecosystem health) deployments from Marcus with OSM treatment info
# Fix some off-grid deployment names
i<-which(is.na(match(s2$location,d$location)))
s2$location[i]<-paste(s2$location[i],"-1",sep="")  # This fixes most of the problems.  Just omit the other few.
# Merge density and OSM BADR info
s<-rbind(s1,s2)
d<-merge(d,s,by="location")
d$project.y<-NULL
names(d)[which(names(d)=="project.x")]<-"project"
# There are some revisited sites, and some duplicate records - use just the most recent one
d<-d[order(d$project,decreasing=TRUE),]
d<-d[duplicated(d$location)==FALSE,]
# Make the column names compatible
names(d2)<-gsub("\\.","",names(d2))
names(d)<-gsub("Winter","_winter",names(d))
names(d)<-gsub("Summer","_summer",names(d))
names(d)[which(names(d)=="Lured")]<-"lure"
names(d)[which(names(d)=="_winterDays")]<-"winter"
names(d)[which(names(d)=="_summerDays")]<-"summer"
d$landscape_unit<-9999  # Not assigned for ABMI sites
d$jem<-substr(d$location,1,3)  # The "JEM" unit for ABMI sites is just the site (with OG sites added to the nearest site).  This line only works because all ABMI sites in OSM have 3 digits (i.e., >=100, <1000)
d$jem<-ifelse(substr(d$location,1,7)=="OG-ABMI",substr(d$location,9,11),d$jem)  # This line only works because the only OG sites are OG-ABMI and the nearest ABMI sites has 3 digits in all cases
d$camera<-"9999"
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

names(d)<-gsub("\\ ","",names(d))
names(d)<-gsub("\\-","",names(d))
col <- intersect(colnames(d), colnames(d2))
d <- d |>
  select(all_of(col)) |>
  bind_rows(d2)
# Then add to BADR cameras
# d<-rbind(d2,d[,colnames(d2)])

# Summarize number of cameras (before these are combined into replicates below)
#write.table(table(paste(d$treatment,d$vegetation),ifelse(is.na(d$fine_scale),"NA",as.character(d$fine_scale))), file="C:/Dave/ABMI/Cameras/2022 analysis/OSM 2022/Table of fine scale treatment by jem treatmentXveg including extra ABMI - n cameras.csv",sep=",",col.names=NA)

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
  dx<-data.frame(Treat=paste(d$treatment,d$fine_scale,d$vegetation),
                 Camera=d$camera,
                 Count=d[,SpTable[sp]]/ifelse(!is.na(d$lure) & d$lure=="Yes",lure$TA[lure$Species==SpTable[sp]],1))  # Count includes direct lure adjustment
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

# Summaries to figure out analyses
# Number of replicates for fine scale treatments by treatment+vegetation type
write.table(table(paste(d$treatment,d$vegetation),ifelse(is.na(d$fine_scale),"NA",as.character(d$fine_scale))), file=paste0(g_drive, "Projects/OSM BADR/Table of fine scale treatment by jem treatmentXveg including extra ABMI - n replicates.csv"),sep=",",col.names=NA)

# JEM treatment X fine scale treatment figures
d$TreatType<-ifelse(regexpr("buffer",d$treatment)>0,"Buffer","HF")  # Separate graphs for buffer-distance treatments and on/off HF treatments
# Treatments with on/off HF
treat.list<-c("dense linear features NA","dense linear features Off","dense linear features On","high activity in situ NA","high activity in situ Off","high activity in situ On",
              "low activity well pads NA","low activity well pads Off","low activity well pads On","pre-insitu NA","reference NA")  # Pasted treatment, fine_scale
#jem.name<-c("","DLF","","","HAIS","","","LAWP","","Pre","Ref")  # Labels for JEM treatments, in same order as above
#fine.name<-c("wet","off","on","wet","off","on","wet","off","on","","")  # Labels for fine scale treatments, in same order as above
treat.x<-c(3.4,3,3.25,5.4,5,5.25,4.4,4,4.25,2,1)  # x-axis position of treatments, in same order as above
veg.list<-c("decidmix40","treedlow20","wetland")
veg.x.offset<-c(0,0.05,0.15)  # x offset for veg types
veg.col<-c("#FF880033","#33AA3333","#0000DD33")
x.tick<-c(1.05,2.05,3.05,3.3,3.55,4.05,4.3,4.55,5.05,5.3,5.55)  # For labels of each fine treatment within JEM treatment
x.label<-c("","","Off","On","Wet","Off","On","Wet","Off","On","Wet")
x.jem<-c(1.05,2.05,3.3,4.3,5.3)
jem.label<-c("Ref","Pre-","Linear","Low well","High in situ")
niter<-3000  # Monte Carlo runs per treatment, including number of elements used to represent corrected density distribution below
mu.true<-log(seq(0.001,20,length.out=niter))  # True mean for likelihood-based CIs.  Evenly distributed on the ordinal scale, so likelihoods-->probabilities for each bin
p.true<-seq(0.0005,0.9995,length.out=niter)  # Possible true p's for likelihood-based CI's
z.dens<-qnorm(seq(0.025,0.975,length.out=niter),0,1)  # Standard normal deviates for density distribution - evenly distributed between 95% CI's
x.hf<-array(NA,c(length(SpTable),length(treat.list),length(veg.list),4))  # To store the values for later use: Species, treatment, veg, {n replicate units, mean, lci, uci}
dimnames(x.hf)<-list(SpTable,treat.list,veg.list,c("n","Mean","LCI","UCI"))

pinterp<-function(x,p,ptarg)  {
  # Interpolates x values based on a second cumulative (generally = probability) distribution, for 1 or more target values of the second distribution (e.g. quantiles)
  # x are the values to interpolate, p is the CUMULATIVE probability distribution (incl. 0 and 1), ptarg is the target probability
  xinterp<-NULL
  for (j in 1:length(ptarg)) {
    x<-c(x,x[length(x)])  # Add upper end
    p<-c(p,p[length(p)])
    i<-length(p[p<=ptarg[j]])
    interp.ratio<-(ptarg[j]-p[i])/(p[i+1]-p[i])
    xinterp[j]<-x[i]*(1-interp.ratio)+x[i+1]*interp.ratio
  }
  xinterp
}

# This is the On/Off treatments.

for (sp in 1:length(SpTable)) {
  d1<-d[d$TreatType=="HF",c(1:10,which(names(d)==SpTable[sp]),which(names(d)==paste(SpTable[sp],"npres",sep=".")))]
  names(d1)[ncol(d1)-1]<-"Density"
  names(d1)[ncol(d1)]<-"nPres"
  jem.fine<-paste(d1$treatment,d1$fine_scale)
  # Create distribution of densities to account for (uncertain) lure effect
  lure.factor<-lure[lure$Species==SpTable[sp],c("TA","TA.lci","TA.uci")]  # This is only being done with TA (not presence/absence component) to keep this somewhat tractable
  lure.exp<-lure$Expansion[lure$Species==SpTable[sp]]
  lure.factor$TA.lci<-lure.factor$TA.lci/(lure.exp*1.96/1.65)  # Adjust for expansion factor and fact that CI's are 90% but using 95% below.  Multiplicative, because ratio
  lure.factor$TA.uci<-lure.factor$TA.uci*(lure.exp*1.96/1.65)  # Adjust for expansion factor and fact that CI's are 90% but using 95% below.  Multiplicative, because ratio
  factor1<-lure.factor$TA-1  # To use for prorating adjustment by proportion of lured deployments within unit
  factor1.lci<-lure.factor$TA.lci-1
  factor1.uci<-lure.factor$TA.uci-1
  dens.adj<-1+outer(d1$n.lure/d1$n,factor1+c(z.dens[1:(niter/2)]*(factor1-factor1.lci)/1.96,z.dens[(niter/2+1):niter]*(factor1.uci-factor1)/1.96))  # Using separate lower and upper half-normal distributions here
  dens.dist<-d1$Density/dens.adj  # This is now 3000 points for each sample unit representing the density of the species there
  # Calculate binomial X log-normal likelihood-based CI's here, incl. adding the uncertainty for the lure correction
  lci<-uci<-xmean<-nrep<-array(NA,c(length(treat.list),length(veg.list)))  # To save CI values and simple (lure-corrected) mean and number of replicate units for each combination of treatment and vegetation, to plot
  #	x1<-log(d1$Density[d1$Density>0]/(1+(lure.factor$TA-1)*d1$n.lure[d1$Density>0]/d1$n[d1$Density>0]))  # To calculate the variance of the log-abundance-given-presence.  This is based on all the density values, so using just the direct means here
  for (i in 1:length(treat.list)) {
    for (j in 1:length(veg.list)) {
      x<-dens.dist[jem.fine==treat.list[i] & d1$vegetation==veg.list[j],]  # Extract the density values with their distributions caused by the uncertain lure adjustment
      nrep[i,j]<-ifelse(is.null(nrow(x)),1,nrow(x))
      x.nPres<-d1$nPres[jem.fine==treat.list[i] & d1$vegetation==veg.list[j]]  # To figure out average number of cameras with occurrences of the species per replicate in that treatmentXveg
      nPres<-mean(x.nPres[x.nPres>0])
      log.var1<-log.var[sp]/nPres  # Overall variance of log abundance-given-presence accounting for the average number of cameras with presences per replicate in this treatmentXveg
      if (length(x)>0) {
        if (sum(x)>0) {  # At least one occurrence, so therefore AGP samples
          # Log-normal likelihood
          if (length(x)==niter)  {  # Single row (sample)
            logx.mean<-sample(log(x[x>0]))  # 1 value for each niter. sample here so that these are independent of mu.true
            lik.lognorm<-exp(-1*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).  This now incorporates variation in density too.
            n.pres<-sum(sum(x)>0)  # Observed number of presences for binomial likelihood
            n.abs<-sum(sum(x)==0)  # and absences
          } else {  # Multiple rows
            if (sum(rowSums(x)>0)==1)  { # Case where only one of those rows has non-0 values
              logx.mean<-sample(log(x[rowSums(x)>0]))  # 1 value for each niter. sample here so that these are independent of mu.true
              lik.lognorm<-exp(-1*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).  This now incorporates variation in density too.
            } else {  # Case where 2 or more rows have non-0 values
              logx.mean<-sample(colMeans(log(x[rowSums(x)>0,])))  # 1 value for each niter. sample here so that these are independent of mu.true
              lik.lognorm<-exp(-nrow(x)*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).  This now incorporates variation in density too.
            }
            # Binomial likelihood
            n.pres<-sum(rowSums(x)>0)  # Observed number of presences for binomial likelihood
            n.abs<-sum(rowSums(x)==0)  # and absences
          }
        } else {  # No occurrences, so use default agp0
          log.var1<-log.var[sp]
          logx.mean<-log(agp0[sp])
          lik.lognorm<-exp(-1*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).
          n.pres<-0
          n.abs<-nrow(x)
          n.abs<-ifelse(is.null(n.abs),1,n.abs)  # This is for the case where there is only one sample, so no dimensions to x
        }
        lik.lognorm<-lik.lognorm/sum(lik.lognorm)  # Convert to probabilities (over just that range)
        lik.binom<-p.true^n.pres*(1-p.true)^n.abs  # Binomial likelihood of each value of (true) p
        lik.binom<-lik.binom/sum(lik.binom)  # Convert to probabilities
        # Then do Monte Carlo simulations from the two distributions independently to generate composite distribution
        lik.lognorm<-c(lik.lognorm[1],lik.lognorm)  # To make interpolation work
        lik.binom<-c(lik.binom[1],lik.binom)  # To make interpolation work
        cump.lognorm<-c(0,cumsum(lik.lognorm))
        cump.binom<-c(0,cumsum(lik.binom))
        z1<-runif(niter,0,1)
        z2<-runif(niter,0,1)
        lognorm.mc<-pinterp(mu.true,cump.lognorm,z1)
        binom.mc<-pinterp(p.true,cump.binom,z2)
        # Adjust for geometric mean effect and calculate quantiles
        adj<-ifelse(n.pres==0,1,mean(x)/mean(binom.mc*exp(lognorm.mc)))  # NOTE: Can't do this for 0's - using
        lci[i,j]<-ifelse(n.pres==0,0,quantile(adj*binom.mc*exp(lognorm.mc),0.05))
        uci[i,j]<-quantile(adj*binom.mc*exp(lognorm.mc),0.95)
      } # End if for >0 point
      xmean[i,j]<-mean(x)  # Simple mean of (lure-corrected) densities
    }  # Next veg type
  } # Next treatment
  # Then do figure(s)
  fname<-paste(g_drive, "results/osm/figures/2022/Figure JEM x on off x veg ",SpTable[sp],".png",sep="")
  png(file=fname,height=600,width=600)
  i<-match(jem.fine,treat.list)
  j<-match(d1$vegetation,veg.list)
  x.point<-treat.x[i]+veg.x.offset[j]
  c1<-veg.col[j]
  dens.adj<-d1$Density/(1+(d1$n.lure/d1$n*(lure.factor$TA-1)))
  plot(jitter(x.point),dens.adj,pch=18,cex=1.0,col=c1,xlab="",ylab="Density",xaxt="n",ylim=c(0,max(c(uci,dens.adj),na.rm=TRUE)))
  axis(side=1,at=x.tick,lab=rep("",length(x.tick)),tck=0.015)
  mtext(side=1,at=x.tick,adj=0.5,line=0.6,x.label)
  mtext(side=1,at=x.jem,adj=0.5,line=1.8,cex=1.3,jem.label)
  mtext(side=3,at=1,adj=0,cex=1.4,sp.names[sp])
  for (i in 1:length(treat.list)) {
    for (j in 1:length(veg.list)) {
      x.point<-treat.x[i]+veg.x.offset[j]
      c1<-veg.col[j]
      for (k in 1:15) {  # To make the transparent colours dark
        lines(rep(x.point,2),c(lci[i,j],uci[i,j]),col=c1,lwd=ifelse(lci[i,j]==0,1,2))
        points(x.point,xmean[i,j],pch=15,cex=2,col=c1)
      }
    }  # Next veg j
  }  # Next treat i
  graphics.off()
  # Version with sqrt-transformation on y-axis
  fname<-paste(g_drive, "results/osm/figures/2022/Figure JEM x on off x veg ",SpTable[sp]," SQRT version.png",sep="")
  png(file=fname,height=600,width=600)
  i<-match(jem.fine,treat.list)
  j<-match(d1$vegetation,veg.list)
  x.point<-treat.x[i]+veg.x.offset[j]
  c1<-veg.col[j]
  dens.adj<-d1$Density/(1+(d1$n.lure/d1$n*(lure.factor$TA-1)))
  plot(jitter(x.point),sqrt(dens.adj),pch=18,cex=1.3,col=c1,xlab="",ylab="Density",xaxt="n",ylim=c(0,sqrt(max(c(uci,dens.adj),na.rm=TRUE))),yaxt="n")
  y.pretty<-pretty(c(0,max(c(uci,dens.adj),na.rm=TRUE)),n=8)
  axis(side=2,at=sqrt(y.pretty),lab=rep("",length(y.pretty)),cex.axis=1.3,tck=1,col="grey85")
  axis(side=2,at=sqrt(y.pretty),lab=y.pretty,cex.axis=1.3,tck=0.015)
  axis(side=1,at=x.tick,lab=rep("",length(x.tick)),tck=0.015)
  mtext(side=1,at=x.tick,adj=0.5,line=0.6,x.label)
  mtext(side=1,at=x.jem,adj=0.5,line=1.8,cex=1.3,jem.label)
  mtext(side=3,at=1,adj=0,cex=1.4,sp.names[sp])
  for (i in 1:length(treat.list)) {
    for (j in 1:length(veg.list)) {
      x.point<-treat.x[i]+veg.x.offset[j]
      c1<-veg.col[j]
      for (k in 1:15) {  # To make the transparent colours dark
        lines(rep(x.point,2),sqrt(c(lci[i,j],uci[i,j])),col=c1,lwd=ifelse(lci[i,j]==0,1,2))
        points(x.point,sqrt(xmean[i,j]),pch=15,cex=2,col=c1)
      }
    }  # Next veg j
  }  # Next treat i
  graphics.off()
  # Keep estimates for JEM-level summaries below
  x.hf[sp,,,1]<-nrep
  x.hf[sp,,,2]<-xmean
  x.hf[sp,,,3]<-lci
  x.hf[sp,,,4]<-uci
}  # Next species

str(x.hf)
# Black Bear
blackbear_mean <- x.hf[1, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Black Bear")

blackbear_lci <- x.hf[1, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Black Bear")

blackbear_uci <- x.hf[1, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Black Bear")

blackbear <- blackbear_mean |>
  left_join(blackbear_lci) |>
  left_join(blackbear_uci)

# Canada Lynx
cl_mean <- x.hf[2, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Canada Lynx")

cl_lci <- x.hf[2, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Canada Lynx")

cl_uci <- x.hf[2, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Canada Lynx")

cl <- cl_mean |>
  left_join(cl_lci) |>
  left_join(cl_uci)

# Coyote
coyote_mean <- x.hf[3, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Coyote")

coyote_lci <- x.hf[3, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Coyote")

coyote_uci <- x.hf[3, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Coyote")

coyote <- coyote_mean |>
  left_join(coyote_lci) |>
  left_join(coyote_uci)

# Fisher
fisher_mean <- x.hf[4, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Fisher")

fisher_lci <- x.hf[4, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Fisher")

fisher_uci <- x.hf[4, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Fisher")

fisher <- fisher_mean |>
  left_join(fisher_lci) |>
  left_join(fisher_uci)

# GrayWolf
graywold_mean <- x.hf[5, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Gray Wolf")

graywold_lci <- x.hf[5, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Gray Wolf")

graywold_uci <- x.hf[5, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Gray Wolf")

graywold <- graywold_mean |>
  left_join(graywold_lci) |>
  left_join(graywold_uci)

# Marten
Marten_mean <- x.hf[6, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Marten")

Marten_lci <- x.hf[6, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Marten")

Marten_uci <- x.hf[6, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Marten")

Marten <- Marten_mean |>
  left_join(Marten_lci) |>
  left_join(Marten_uci)

# Moose
Moose_mean <- x.hf[7, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Moose")

Moose_lci <- x.hf[7, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Moose")

Moose_uci <- x.hf[7, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Moose")

Moose <- Moose_mean |>
  left_join(Moose_lci) |>
  left_join(Moose_uci)

# SnowshoeHare
snowshoehare_mean <- x.hf[8, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Snowshoe Hare")

snowshoehare_lci <- x.hf[8, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Snowshoe Hare")

snowshoehare_uci <- x.hf[8, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Snowshoe Hare")

snowshoehare <- snowshoehare_mean |>
  left_join(snowshoehare_lci) |>
  left_join(snowshoehare_uci)

# WhitetailedDeer
wtd_mean <- x.hf[9, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "White-tailed Deer")

wtd_lci <- x.hf[9, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "White-tailed Deer")

wtd_uci <- x.hf[9, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "White-tailed Deer")

wtd <- wtd_mean |>
  left_join(wtd_lci) |>
  left_join(wtd_uci)

# Woodland Caribou
caribou_mean <- x.hf[10, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Woodland Caribou")

caribou_lci <- x.hf[10, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Woodland Caribou")

caribou_uci <- x.hf[10, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "situ NA|pads NA|features NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Woodland Caribou")

caribou <- caribou_mean |>
  left_join(caribou_lci) |>
  left_join(caribou_uci)

# Join all together
all_on_off <- bind_rows(
  caribou, wtd, Marten, Moose, fisher, graywold, snowshoehare, blackbear, cl, coyote) |>
  select(common_name, treatment, vegetation, mean_density, lci_density, uci_density) |>
  mutate(treatment = str_remove(treatment, " NA"))

# Save csv
write_csv(all_on_off, paste0(g_drive, "results/osm/2021-2022_osm-on-off_treatment_results.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# JEM treatment X edge distance buffer
d$TreatType<-ifelse(regexpr("buffer",d$treatment)>0,"Buffer","HF")  # Separate graphs for buffer-distance treatments and on/off HF treatments
# Treatments with buffers
treat.list<-c("road buffer 10","road buffer 30","road buffer 100","road buffer 300","plant/mine buffer 10","plant/mine buffer 30","plant/mine buffer 100","plant/mine buffer 300")  # Pasted treatment, fine_scale
treat.x<-c(1,1.1,1.2,1.3,2.2,2.3,2.4,2.5)  # x-axis position of treatments, in same order as above.  Additional value will be added for veg types
veg.list<-c("decidmix40","treedlow20")
veg.x.offset<-c(0,0.5)  # x offset for veg types
veg.col<-c("#FF880033","#33AA3333")
x.tick<-c(1,1.1,1.2,1.3,1.5,1.6,1.7,1.8,2.2,2.3,2.4,2.5,2.7,2.8,2.9,3)  # For labels of each buffer distance within JEM treatment
x.label<-rep(c("10","30","100","300"),4)
x.jem<-c(1.4,2.6)
jem.label<-c("Road","Plant/mine")
niter<-3000  # Monte Carlo runs per treatment, including number of elements used to represent corrected density distribution below
mu.true<-log(seq(0.001,20,length.out=niter))  # True mean for likelihood-based CIs.  Evenly distributed on the ordinal scale, so likelihoods-->probabilities for each bin
p.true<-seq(0.0005,0.9995,length.out=niter)  # Possible true p's for likelihood-based CI's
z.dens<-qnorm(seq(0.025,0.975,length.out=niter),0,1)  # Standard normal deviates for density distribution - evenly distributed between 95% CI's
x.dist<-array(NA,c(length(SpTable),length(treat.list),length(veg.list),4))  # To store the values for later use: Species, treatment, veg, {n replicate units, mean, lci, uci}
dimnames(x.dist)<-list(SpTable,treat.list,veg.list,c("n","Mean","LCI","UCI"))


for (sp in 1:length(SpTable)) {
  d1<-d[d$TreatType=="Buffer",c(1:10,which(names(d)==SpTable[sp]),which(names(d)==paste(SpTable[sp],"npres",sep=".")))]
  names(d1)[ncol(d1)-1]<-"Density"
  names(d1)[ncol(d1)]<-"nPres"
  jem.fine<-paste(d1$treatment,d1$fine_scale)
  # Create distribution of densities to account for (uncertain) lure effect
  lure.factor<-lure[lure$Species==SpTable[sp],c("TA","TA.lci","TA.uci")]  # This is only being done with TA (not presence/absence component) to keep this somewhat tractable
  lure.exp<-lure$Expansion[lure$Species==SpTable[sp]]
  lure.factor$TA.lci<-lure.factor$TA.lci/(lure.exp*1.96/1.65)  # Adjust for expansion factor and fact that CI's are 90% but using 95% below.  Multiplicative, because ratio
  lure.factor$TA.uci<-lure.factor$TA.uci*(lure.exp*1.96/1.65)  # Adjust for expansion factor and fact that CI's are 90% but using 95% below.  Multiplicative, because ratio
  factor1<-lure.factor$TA-1  # To use for prorating adjustment by proportion of lured deployments within unit
  factor1.lci<-lure.factor$TA.lci-1
  factor1.uci<-lure.factor$TA.uci-1
  dens.adj<-1+outer(d1$n.lure/d1$n,factor1+c(z.dens[1:(niter/2)]*(factor1-factor1.lci)/1.96,z.dens[(niter/2+1):niter]*(factor1.uci-factor1)/1.96))  # Using separate lower and upper half-normal distributions here
  dens.dist<-d1$Density/dens.adj  # This is now 3000 points for each sample unit representing the density of the species there
  # Calculate binomial X log-normal likelihood-based CI's here, incl. adding the uncertainty for the lure correction
  lci<-uci<-xmean<-nrep<-array(NA,c(length(treat.list),length(veg.list)))  # To save CI values and simple (lure-corrected) mean and number of replicate units for each combination of treatment and vegetation, to plot
  #	x1<-log(d1$Density[d1$Density>0]/(1+(lure.factor$TA-1)*d1$n.lure[d1$Density>0]/d1$n[d1$Density>0]))  # This is based on all the density values, so using just the direct means here
  for (i in 1:length(treat.list)) {
    for (j in 1:length(veg.list)) {
      x<-dens.dist[jem.fine==treat.list[i] & d1$vegetation==veg.list[j],]  # Extract the density values with their distributions caused by the uncertain lure adjustment
      nrep[i,j]<-ifelse(is.null(nrow(x)),1,nrow(x))
      x.nPres<-d1$nPres[jem.fine==treat.list[i] & d1$vegetation==veg.list[j]]  # To figure out average number of cameras with occurrences of the species per replicate in that treatmentXveg
      nPres<-mean(x.nPres[x.nPres>0])
      log.var1<-log.var[sp]/nPres  # Overall variance of log abundance-given-presence accounting for the average number of cameras with presences per replicate in this treatmentXveg
      if (length(x)>0)  {
        if (sum(x)>0) {
          # Log-normal likelihood
          if (length(x)==niter)  {  # Single row
            logx.mean<-sample(log(x[x>0]))  # 1 value for each niter. sample here so that these are independent of mu.true
            lik.lognorm<-exp(-1*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).  This now incorporates variation in density too.
            n.pres<-sum(sum(x)>0)  # Observed number of presences for binomial likelihood
            n.abs<-sum(sum(x)==0)  # and absences
          } else {  # Multiple rows
            if (sum(rowSums(x)>0)==1)  { # Case where only one of those rows has non-0 values
              logx.mean<-sample(log(x[rowSums(x)>0]))  # 1 value for each niter. sample here so that these are independent of mu.true
              lik.lognorm<-exp(-1*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).  This now incorporates variation in density too.
            } else {  # Case where 2 or more rows have non-0 values
              logx.mean<-sample(colMeans(log(x[rowSums(x)>0,])))  # 1 value for each niter. sample here so that these are independent of mu.true
              lik.lognorm<-exp(-nrow(x)*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).  This now incorporates variation in density too.
            }
            # Binomial likelihood
            n.pres<-sum(rowSums(x)>0)  # Observed number of presences for binomial likelihood
            n.abs<-sum(rowSums(x)==0)  # and absences
          }
        } else {  # No occurrences, so use default agp0
          log.var1<-log.var[sp]
          logx.mean<-log(agp0[sp])
          lik.lognorm<-exp(-1*(log.var1+(logx.mean-mu.true)^2)/(2*log.var1))  # Likelihood for each value of mu (log-normal).
          n.pres<-0
          n.abs<-nrow(x)
          n.abs<-ifelse(is.null(n.abs),1,n.abs)  # This is for the case where there is only one sample, so no dimensions to x
        }
        lik.lognorm<-lik.lognorm/sum(lik.lognorm)  # Convert to probabilities (over just that range)
        lik.binom<-p.true^n.pres*(1-p.true)^n.abs  # Binomial likelihood of each value of (true) p
        lik.binom<-lik.binom/sum(lik.binom)  # Convert to probabilities
        # Then do Monte Carlo simulations from the two distributions independently to generate composite distribution
        lik.lognorm<-c(lik.lognorm[1],lik.lognorm)  # To make interpolation work
        lik.binom<-c(lik.binom[1],lik.binom)  # To make interpolation work
        cump.lognorm<-c(0,cumsum(lik.lognorm))
        cump.binom<-c(0,cumsum(lik.binom))
        z1<-runif(niter,0,1)
        z2<-runif(niter,0,1)
        lognorm.mc<-pinterp(mu.true,cump.lognorm,z1)
        binom.mc<-pinterp(p.true,cump.binom,z2)
        # Adjust for geometric mean effect and calculate quantiles
        adj<-ifelse(n.pres==0,1,mean(x)/mean(binom.mc*exp(lognorm.mc)))  # NOTE: Can't do this for 0's - using
        lci[i,j]<-ifelse(n.pres==0,0,quantile(adj*binom.mc*exp(lognorm.mc),0.05, na.rm = TRUE))
        uci[i,j]<-quantile(adj*binom.mc*exp(lognorm.mc),0.95, na.rm = TRUE)
      } # End if for >1 point
      xmean[i,j]<-mean(x)  # Simple mean of (lure-corrected) densities
    }  # Next veg type
  } # Next treatment
  # Then do figure(s)
  fname<-paste(g_drive, "results/osm/figures/2022/Figure JEM x buffer distance x veg ",SpTable[sp],".png",sep="")
  png(file=fname,height=600,width=600)
  i<-match(jem.fine,treat.list)
  j<-match(d1$vegetation,veg.list)
  x.point<-treat.x[i]+veg.x.offset[j]
  c1<-veg.col[j]
  dens.adj<-d1$Density/(1+(d1$n.lure/d1$n*(lure.factor$TA-1)))
  plot(jitter(x.point),dens.adj,pch=18,cex=1.3,col=c1,xlab="",ylab="Density",xaxt="n",ylim=c(0,max(c(uci,dens.adj),na.rm=TRUE)))
  axis(side=1,at=x.tick,lab=rep("",length(x.tick)),tck=0.015)
  mtext(side=1,at=x.tick,adj=0.5,line=0.6,x.label)
  mtext(side=1,at=x.jem,adj=0.5,line=1.8,cex=1.3,jem.label)
  mtext(side=3,at=1,adj=0,cex=1.4,sp.names[sp])
  for (i in 1:length(treat.list)) {
    for (j in 1:length(veg.list)) {
      x.point<-treat.x[i]+veg.x.offset[j]
      c1<-veg.col[j]
      for (k in 1:15) {  # To make the transparent colours dark
        lines(rep(x.point,2),(c(lci[i,j],uci[i,j])),col=c1,lwd=ifelse(lci[i,j]==0,1,2))
        points(x.point,xmean[i,j],pch=15,cex=2,col=c1)
      }
    }  # Next veg j
  }  # Next treat i
  graphics.off()
  # Version with sqrt-transformation on y-axis
  fname<-paste(g_drive, "results/osm/figures/2022/Figure JEM x buffer distance x veg ",SpTable[sp]," SQRT version.png",sep="")
  png(file=fname,height=600,width=600)
  i<-match(jem.fine,treat.list)
  j<-match(d1$vegetation,veg.list)
  x.point<-treat.x[i]+veg.x.offset[j]
  c1<-veg.col[j]
  dens.adj<-d1$Density/(1+(d1$n.lure/d1$n*(lure.factor$TA-1)))
  plot(jitter(x.point),sqrt(dens.adj),pch=18,cex=1.3,col=c1,xlab="",ylab="Density",xaxt="n",ylim=c(0,sqrt(max(c(uci,dens.adj),na.rm=TRUE))),yaxt="n")
  y.pretty<-pretty(c(0,max(c(uci,dens.adj),na.rm=TRUE)),n=8)
  axis(side=2,at=sqrt(y.pretty),lab=rep("",length(y.pretty)),cex.axis=1.3,tck=1,col="grey85")
  axis(side=2,at=sqrt(y.pretty),lab=y.pretty,cex.axis=1.3,tck=0.015)
  axis(side=1,at=x.tick,lab=rep("",length(x.tick)),tck=0.015)
  mtext(side=1,at=x.tick,adj=0.5,line=0.6,x.label)
  mtext(side=1,at=x.jem,adj=0.5,line=1.8,cex=1.3,jem.label)
  mtext(side=3,at=1,adj=0,cex=1.4,sp.names[sp])
  for (i in 1:length(treat.list)) {
    for (j in 1:length(veg.list)) {
      x.point<-treat.x[i]+veg.x.offset[j]
      c1<-veg.col[j]
      for (k in 1:15) {  # To make the transparent colours dark
        lines(rep(x.point,2),sqrt(c(lci[i,j],uci[i,j])),col=c1,lwd=ifelse(lci[i,j]==0,1,2))
        points(x.point,sqrt(xmean[i,j]),pch=15,cex=2,col=c1)
      }
    }  # Next veg j
  }  # Next treat i
  graphics.off()
  # Keep estimates for JEM-level summaries below
  x.dist[sp,,,1]<-nrep
  x.dist[sp,,,2]<-xmean
  x.dist[sp,,,3]<-lci
  x.dist[sp,,,4]<-uci
}  # Next species

str(x.dist)

# Black Bear
blackbear_mean <- x.dist[1, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Black Bear")

blackbear_lci <- x.dist[1, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Black Bear")

blackbear_uci <- x.dist[1, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Black Bear")

blackbear <- blackbear_mean |>
  left_join(blackbear_lci) |>
  left_join(blackbear_uci)

# Canada Lynx
cl_mean <- x.dist[2, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Canada Lynx")

cl_lci <- x.dist[2, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Canada Lynx")

cl_uci <- x.dist[2, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Canada Lynx")

cl <- cl_mean |>
  left_join(cl_lci) |>
  left_join(cl_uci)

# Coyote
coyote_mean <- x.dist[3, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Coyote")

coyote_lci <- x.dist[3, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Coyote")

coyote_uci <- x.dist[3, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Coyote")

coyote <- coyote_mean |>
  left_join(coyote_lci) |>
  left_join(coyote_uci)

# Fisher
fisher_mean <- x.dist[4, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Fisher")

fisher_lci <- x.dist[4, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Fisher")

fisher_uci <- x.dist[4, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Fisher")

fisher <- fisher_mean |>
  left_join(fisher_lci) |>
  left_join(fisher_uci)

# GrayWolf
graywold_mean <- x.dist[5, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Gray Wolf")

graywold_lci <- x.dist[5, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Gray Wolf")

graywold_uci <- x.dist[5, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Gray Wolf")

graywold <- graywold_mean |>
  left_join(graywold_lci) |>
  left_join(graywold_uci)

# Marten
Marten_mean <- x.dist[6, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Marten")

Marten_lci <- x.dist[6, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Marten")

Marten_uci <- x.dist[6, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Marten")

Marten <- Marten_mean |>
  left_join(Marten_lci) |>
  left_join(Marten_uci)

# Moose
Moose_mean <- x.dist[7, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Moose")

Moose_lci <- x.dist[7, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Moose")

Moose_uci <- x.dist[7, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Moose")

Moose <- Moose_mean |>
  left_join(Moose_lci) |>
  left_join(Moose_uci)

# SnowshoeHare
snowshoehare_mean <- x.dist[8, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Snowshoe Hare")

snowshoehare_lci <- x.dist[8, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Snowshoe Hare")

snowshoehare_uci <- x.dist[8, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Snowshoe Hare")

snowshoehare <- snowshoehare_mean |>
  left_join(snowshoehare_lci) |>
  left_join(snowshoehare_uci)

# WhitetailedDeer
wtd_mean <- x.dist[9, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "White-tailed Deer")

wtd_lci <- x.dist[9, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "White-tailed Deer")

wtd_uci <- x.dist[9, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "White-tailed Deer")

wtd <- wtd_mean |>
  left_join(wtd_lci) |>
  left_join(wtd_uci)

# Woodland Caribou
caribou_mean <- x.dist[10, , 1:2, 2] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "mean_density") |>
  mutate(common_name = "Woodland Caribou")

caribou_lci <- x.dist[10, , 1:2, 3] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "lci_density") |>
  mutate(common_name = "Woodland Caribou")

caribou_uci <- x.dist[10, , 1:2, 4] |>
  as.data.frame() |>
  rownames_to_column(var = "treatment") |>
  filter(!str_detect(treatment, "NA")) |>
  pivot_longer(cols = c(decidmix40, treedlow20), names_to = "vegetation", values_to = "uci_density") |>
  mutate(common_name = "Woodland Caribou")

caribou <- caribou_mean |>
  left_join(caribou_lci) |>
  left_join(caribou_uci)

# Join all together
all_buffer <- bind_rows(
  caribou, wtd, Marten, Moose, fisher, graywold, snowshoehare, blackbear, cl, coyote) |>
  select(common_name, treatment, vegetation, mean_density, lci_density, uci_density)

# Save csv
write_csv(all_buffer, paste0(g_drive, "results/osm/2021-2022_osm_buffer_treatment_results.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Individual points for plotting

dens.adj<-d1$Density/(1+(d1$n.lure/d1$n*(lure.factor$TA-1)))

jem_density <- d |>
  pivot_longer(cols = c(BlackBear:WoodlandCaribou), names_to = "common_name", values_to = "density") |>
  left_join(lure, by = c("common_name" = "Species")) |>
  mutate(density_adj = density / (1 + ((n.lure / n) * (TA - 1)))) |>
  select(jem, vegetation, treatment, fine_scale, type = TreatType, common_name, density, density_adj)

write_csv(jem_density, paste0(g_drive, "data/processed/osm/2021-2022_osm_mean_jem_density_values.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Do JEM-level summaries
# This uses a stratified mean, with strata weights based on area of each fine_scale treatment type in the JEM units (separately by veg type)
s.hf<-read.csv("C:/Dave/ABMI/Data/Site info/2022/OSM BADR cameras summary_for_denselinear_lowhighactivity.csv")  # Dense linear and wellpad
s.ref<-read.csv("C:/Dave/ABMI/Data/Site info/2022/OSM BADR cameras summary_for_LowDisturbance_Reference.csv")  # Reference
s.road<-read.csv("C:/Dave/ABMI/Data/Site info/2022/OSM BADR cameras summary_for_road_plantmine.csv")  # Road and plant distances
# First, make tables of the proportion of JEM treatment unit areas in each fine_scale treatment, by veg type
# Make table of areas in same format as estimates: [treat,veg]
#1. On-off HF treatments
treat.list<-c("dense linear features Off","dense linear features On","high activity in situ Off","high activity in situ On",
              "low activity well pads Off","low activity well pads On")  # Pasted treatment, fine_scale
veg.list<-c("decidmix40","treedlow20")  # Note: No info on wetland
area.hf<-array(NA,c(length(treat.list),length(veg.list)))
dimnames(area.hf)<-list(treat.list,veg.list)
# Go through treatments individually
s1<-s.hf[s.hf$Treatment_Polygon=="Dense Linear Features",]
area.hf[rownames(area.hf)=="dense linear features Off",]<-colSums(s1[s1$denselinear_FTY==FALSE,c("A_decidmix40","A_treedlow20")])
area.hf[rownames(area.hf)=="dense linear features On",]<-colSums(s1[s1$denselinear_FTY==TRUE,c("A_decidmix40","A_treedlow20")])
s1<-s.hf[s.hf$Treatment_Polygon=="High Activity Insitu Well Pads",]
area.hf[rownames(area.hf)=="high activity in situ Off",]<-colSums(s1[s1$wellsite_FTY==FALSE,c("A_decidmix40","A_treedlow20")])
area.hf[rownames(area.hf)=="high activity in situ On",]<-colSums(s1[s1$wellsite_FTY==TRUE,c("A_decidmix40","A_treedlow20")])
s1<-s.hf[s.hf$Treatment_Polygon=="Low Activity Well Pads",]
area.hf[rownames(area.hf)=="low activity well pads Off",]<-colSums(s1[s1$wellsite_FTY==FALSE,c("A_decidmix40","A_treedlow20")])
area.hf[rownames(area.hf)=="low activity well pads On",]<-colSums(s1[s1$wellsite_FTY==TRUE,c("A_decidmix40","A_treedlow20")])
# And table with strata weights (proportions of area of fine-scale types within each treatment and veg type)
p.area.hf<-array(NA,c(3,2,2))  # 3 on/off HF JEM treatments, 2 on/off fine_scale, 2 veg types
dimnames(p.area.hf)<-list(c("Dense Linear Features","High Activity Insitu Well Pads","Low Activity Well Pads"),c("Off","On"),veg.list)
p.area.hf[1,,1]<-area.hf[1:2,1]/sum(area.hf[1:2,1])
p.area.hf[1,,2]<-area.hf[1:2,2]/sum(area.hf[1:2,2])
p.area.hf[2,,1]<-area.hf[3:4,1]/sum(area.hf[3:4,1])
p.area.hf[2,,2]<-area.hf[3:4,2]/sum(area.hf[3:4,2])
p.area.hf[3,,1]<-area.hf[5:6,1]/sum(area.hf[5:6,1])
p.area.hf[3,,2]<-area.hf[5:6,2]/sum(area.hf[5:6,2])
#2. Buffer distance treatments
treat.list<-c("road buffer 10","road buffer 30","road buffer 100","road buffer 300","plant/mine buffer 10","plant/mine buffer 30","plant/mine buffer 100","plant/mine buffer 300")  # Pasted treatment, fine_scale
veg.list<-c("decidmix40","treedlow20")  # Note: No info on wetland
area.dist<-array(NA,c(length(treat.list),length(veg.list)))
dimnames(area.dist)<-list(treat.list,veg.list)
# Go through treatments individually
s1<-s.road[s.road$Treatment_Polygon=="Roads",]
area.dist[rownames(area.dist)=="road buffer 10",]<-colSums(s1[s1$buffer_road=="0-20m",c("A_decidmix40","A_treedlow20")])
area.dist[rownames(area.dist)=="road buffer 30",]<-colSums(s1[s1$buffer_road=="20-50m",c("A_decidmix40","A_treedlow20")])
area.dist[rownames(area.dist)=="road buffer 100",]<-colSums(s1[s1$buffer_road=="50-200m",c("A_decidmix40","A_treedlow20")])
area.dist[rownames(area.dist)=="road buffer 300",]<-colSums(s1[s1$buffer_road=="NAN",c("A_decidmix40","A_treedlow20")])
s1<-s.road[s.road$Treatment_Polygon=="Plant/Mine Buffer",]
area.dist[rownames(area.dist)=="plant/mine buffer 10",]<-colSums(s1[s1$buffer_plant=="0-20m",c("A_decidmix40","A_treedlow20")])
area.dist[rownames(area.dist)=="plant/mine buffer 30",]<-colSums(s1[s1$buffer_plant=="20-50m",c("A_decidmix40","A_treedlow20")])
area.dist[rownames(area.dist)=="plant/mine buffer 100",]<-colSums(s1[s1$buffer_plant=="50-200m",c("A_decidmix40","A_treedlow20")])
area.dist[rownames(area.dist)=="plant/mine buffer 300",]<-colSums(s1[s1$buffer_plant=="NAN",c("A_decidmix40","A_treedlow20")])
# And table with strata weights (proportions of area of fine-scale types within each treatment and veg type)
p.area.dist<-array(NA,c(2,4,2))  # 2 on/off HF JEM treatments, 4 distances, 2 veg types
dimnames(p.area.dist)<-list(c("Roads","Plant/Mine Buffer"),c("10m","30m","100m","300m"),veg.list)
p.area.dist[1,,1]<-area.dist[1:4,1]/sum(area.dist[1:4,1])
p.area.dist[1,,2]<-area.dist[1:4,2]/sum(area.dist[1:4,2])
p.area.dist[2,,1]<-area.dist[5:8,1]/sum(area.dist[5:8,1])
p.area.dist[2,,2]<-area.dist[5:8,2]/sum(area.dist[5:8,2])
# Then do stratified means
JEM1<-array(NA,c(length(SpTable),7,2,3))  # Species, {3 On/off HF treatments + 2 Distance treatments + 2 no-fine-scale treatments}, veg types, stratified {mean, LCI, UCI}
dimnames(JEM1)<-list(SpTable,c("Dense Linear Features","High Activity Insitu Well Pads","Low Activity Well Pads","Roads","Plant/Mine Buffer","Pre-Insitu","Low Disturbance/Reference"),veg.list,c("Mean","LCI","UCI"))
for (sp in 1:length(SpTable)) {
  # Dense linear
  JEM1[sp,"Dense Linear Features",,"Mean"]<-colSums(x.hf[sp,2:3,1:2,"Mean"] * p.area.hf["Dense Linear Features",,])
  JEM1[sp,"Dense Linear Features",,"LCI"]<-colSums(x.hf[sp,2:3,1:2,"LCI"] * p.area.hf["Dense Linear Features",,])
  JEM1[sp,"Dense Linear Features",,"UCI"]<-colSums(x.hf[sp,2:3,1:2,"UCI"] * p.area.hf["Dense Linear Features",,])
  # In situ
  JEM1[sp,"High Activity Insitu Well Pads",,"Mean"]<-colSums(x.hf[sp,5:6,1:2,"Mean"] * p.area.hf["High Activity Insitu Well Pads",,])
  JEM1[sp,"High Activity Insitu Well Pads",,"LCI"]<-colSums(x.hf[sp,5:6,1:2,"LCI"] * p.area.hf["High Activity Insitu Well Pads",,])
  JEM1[sp,"High Activity Insitu Well Pads",,"UCI"]<-colSums(x.hf[sp,5:6,1:2,"UCI"] * p.area.hf["High Activity Insitu Well Pads",,])
  # Low activity
  JEM1[sp,"Low Activity Well Pads",,"Mean"]<-colSums(x.hf[sp,8:9,1:2,"Mean"] * p.area.hf["Low Activity Well Pads",,])
  JEM1[sp,"Low Activity Well Pads",,"LCI"]<-colSums(x.hf[sp,8:9,1:2,"LCI"] * p.area.hf["Low Activity Well Pads",,])
  JEM1[sp,"Low Activity Well Pads",,"UCI"]<-colSums(x.hf[sp,8:9,1:2,"UCI"] * p.area.hf["Low Activity Well Pads",,])
  # Roads
  JEM1[sp,"Roads",,"Mean"]<-colSums(x.dist[sp,1:4,1:2,"Mean"] * p.area.dist["Roads",,])
  JEM1[sp,"Roads",,"LCI"]<-colSums(x.dist[sp,1:4,1:2,"LCI"] * p.area.dist["Roads",,])
  JEM1[sp,"Roads",,"UCI"]<-colSums(x.dist[sp,1:4,1:2,"UCI"] * p.area.dist["Roads",,])
  # Plant/Mine Buffer
  JEM1[sp,"Plant/Mine Buffer",,"Mean"]<-colSums(x.dist[sp,5:8,1:2,"Mean"] * p.area.dist["Plant/Mine Buffer",,])
  JEM1[sp,"Plant/Mine Buffer",,"LCI"]<-colSums(x.dist[sp,5:8,1:2,"LCI"] * p.area.dist["Plant/Mine Buffer",,])
  JEM1[sp,"Plant/Mine Buffer",,"UCI"]<-colSums(x.dist[sp,5:8,1:2,"UCI"] * p.area.dist["Plant/Mine Buffer",,])
  # Pre-Insitu
  JEM1[sp,"Pre-Insitu",,"Mean"]<-x.hf[sp,10,1:2,"Mean"]
  JEM1[sp,"Pre-Insitu",,"LCI"]<-x.hf[sp,10,1:2,"LCI"]
  JEM1[sp,"Pre-Insitu",,"UCI"]<-x.hf[sp,10,1:2,"UCI"]
  # Reference
  JEM1[sp,"Low Disturbance/Reference",,"Mean"]<-x.hf[sp,11,1:2,"Mean"]
  JEM1[sp,"Low Disturbance/Reference",,"LCI"]<-x.hf[sp,11,1:2,"LCI"]
  JEM1[sp,"Low Disturbance/Reference",,"UCI"]<-x.hf[sp,11,1:2,"UCI"]
  # Figure
  # Version with sqrt-transformation on y-axis
  fname<-paste("C:/Dave/ABMI/Cameras/2022 analysis/OSM 2022/Results with ABMI/Figure JEM Treatment ",SpTable[sp]," SQRT version.png",sep="")
  png(file=fname,height=600,width=600)
  x.point<-c(1:7,(1:7+0.3))
  x.tick<-(1:7)+0.15
  jem.label<-c("Ref","Pre-","Linear","Low well","High in situ","Roads","Plant buffer")
  ymax<-sqrt(min(c(max(JEM1[sp,,,],na.rm=TRUE), max(3*JEM1[sp,,,"Mean"]))))  # Truncate y-axis at 3x maximum mean
  dplot(0,0,typ="n",xlim=c(0.8,7.5),ylim=c(0,ymax),yaxt="n",xaxt="n",ylab="Density",xlab="JEM Treatment")
  y.pretty<-pretty(c(0,ymax^2),n=8)
  axis(side=2,at=sqrt(y.pretty),lab=rep("",length(y.pretty)),cex.axis=1.3,tck=1,col="grey85")
  axis(side=2,at=sqrt(y.pretty),lab=y.pretty,cex.axis=1.3,tck=0.015)
  axis(side=1,at=x.tick,lab=rep("",length(x.tick)),tck=0.015)
  mtext(side=1,at=x.tick,adj=0.5,line=0.6,jem.label,cex=1.2)
  mtext(side=3,at=1,adj=0,cex=1.4,sp.names[sp])
  for (j in 1:length(veg.list)) {
    x.point<-1:7+(j-1)*0.3
    c1<-veg.col[j]
    for (k in 1:15) {  # To make the transparent colours dark
      for (treat in c(7,6,1,3,2,4,5)) {
        lines(rep(x.point[treat],2),sqrt(JEM1[sp,treat,j,2:3]),col=c1,lwd=2)
        points(x.point[treat],sqrt(JEM1[sp,treat,j,1]),pch=15,cex=2,col=c1)
      }
    }
  }  # Next veg j
  graphics.off()
}  # Next species
write.table(cbind(p.area.hf[,,1],p.area.hf[,,2]),file="C:/Dave/ABMI/Cameras/2022 analysis/OSM 2022/Results with ABMI/Area of fine_scale strata on- vs off-HF treatments.csv",sep=",",col.names=NA)
write.table(cbind(p.area.dist[,,1],p.area.dist[,,2]),file="C:/Dave/ABMI/Cameras/2022 analysis/OSM 2022/Results with ABMI/Area of fine_scale strata distance treatments.csv",sep=",",col.names=NA)




