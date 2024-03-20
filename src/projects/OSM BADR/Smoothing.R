# R Explore smoothing for OSM cameras

library(mgcv)  # For GAM
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

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Summary results come from Marcus in .csv form.  Convert to array like 2022 to use same script below
# This may have to be changed in the future if results are in different format
n<-read.csv(paste0(g_drive, "Projects/OSM BADR/Total Number of Camera Per OSM Treatment and Vegetation Type.csv"),stringsAsFactors=FALSE)  # Sample size incl ABMI, from Marcus. "treatment1" added to use same treatment names as main results files
# HF treatments
d.hf<-read.csv(paste0(g_drive, "Results/OSM BADR/2021-2022_osm-on-off_treatment_results_new.csv"),stringsAsFactors=FALSE)
SpTable<-sort(unique(d.hf$common_name))
hf.list<-sort(unique(d.hf$treatment))
veg.list<-sort(unique(d.hf$vegetation))
x.hf<-array(NA,c(length(SpTable),length(hf.list),length(veg.list),4))  # Density by species, hf treatment, vegetation type, {n, mean, lci, uci}
dimnames(x.hf)<-list(SpTable,hf.list,veg.list,c("n","Mean","LCI","UCI"))
for (sp in 1:length(SpTable)) {
  for (hf in 1:length(hf.list)) {
    for (veg in 1:length(veg.list)) {
      x.hf[sp,hf,veg,1]<-n$n[n$treatment1==hf.list[hf] & tolower(n$vegetation)==veg.list[veg]]
      x.hf[sp,hf,veg,2]<-d.hf$mean_density[d.hf$common_name==SpTable[sp] & d.hf$treatment==hf.list[hf] & d.hf$vegetation==veg.list[veg]]
      x.hf[sp,hf,veg,3]<-d.hf$lci_density[d.hf$common_name==SpTable[sp] & d.hf$treatment==hf.list[hf] & d.hf$vegetation==veg.list[veg]]
      x.hf[sp,hf,veg,4]<-d.hf$uci_density[d.hf$common_name==SpTable[sp] & d.hf$treatment==hf.list[hf] & d.hf$vegetation==veg.list[veg]]
    }  # Next veg v
  }  # Next treatment hf
}  # Next sp
# Distance treatments
d.dist<-read.csv(paste0(g_drive, "Results/OSM BADR/2021-2022_osm_buffer_treatment_results_new.csv"),stringsAsFactors=FALSE)
d.dist$lci_density[is.na(d.dist$lci_density)]<-0.1234388  # Single NA - using value from last year
d.dist$uci_density[is.na(d.dist$uci_density)]<-1.9516508  # Single NA - using value from last year
SpTable<-sort(unique(d.dist$common_name))
hf.list<-c("road buffer 10","road buffer 30","road buffer 100","road buffer 300","plant/mine buffer 10","plant/mine buffer 30","plant/mine buffer 100","plant/mine buffer 300")  # Manual, to put the distances in numerical (rather than alphabetical) order
veg.list<-sort(unique(d.dist$vegetation))
x.dist<-array(NA,c(length(SpTable),length(hf.list),length(veg.list),4))  # Density by species, hf treatment, vegetation type, {n, mean, lci, uci}
dimnames(x.dist)<-list(SpTable,hf.list,veg.list,c("n","Mean","LCI","UCI"))
for (sp in 1:length(SpTable)) {
  for (hf in 1:length(hf.list)) {
    for (veg in 1:length(veg.list)) {
      x.dist[sp,hf,veg,1]<-n$n[n$treatment1==hf.list[hf] & tolower(n$vegetation)==veg.list[veg]]
      x.dist[sp,hf,veg,2]<-d.dist$mean_density[d.dist$common_name==SpTable[sp] & d.dist$treatment==hf.list[hf] & d.dist$vegetation==veg.list[veg]]
      x.dist[sp,hf,veg,3]<-d.dist$lci_density[d.dist$common_name==SpTable[sp] & d.dist$treatment==hf.list[hf] & d.dist$vegetation==veg.list[veg]]
      x.dist[sp,hf,veg,4]<-d.dist$uci_density[d.dist$common_name==SpTable[sp] & d.dist$treatment==hf.list[hf] & d.dist$vegetation==veg.list[veg]]
    }  # Next veg v
  }  # Next treatment hf
}  # Next sp

# Edge distance
combo.roads<-combo.plant<-array(NA,c(length(SpTable),4,3))  # To save results for species, 4 distances, {mean, 5%, 95%} (combining veg strata)
dimnames(combo.roads)<-dimnames(combo.plant)<-list(SpTable,c("0-20m","20-50m","50-200m",">200m"),c("Mean","q5","q95"))
for (sp in 1:length(SpTable)) {
  d1<-x.dist[sp,,,]
  DLR<-sum(d1[,"decidmix40","Mean"])/sum(d1[,"treedlow20","Mean"])  # deciduous:lowland ratio.  Analysis assumes same relationships in each, just a multiplicative offset
  d<-d1
  d[,"decidmix40",c("Mean","LCI","UCI")]<-d1[,"decidmix40",c("Mean","LCI","UCI")]/sqrt(DLR)  # Adjust everything to geometric mean of deciduous and lowland values for summaries.  Geometric because the correction is multiplicative, to retain the relative differences among distances
  d[,"treedlow20",c("Mean","LCI","UCI")]<-d1[,"treedlow20",c("Mean","LCI","UCI")]*sqrt(DLR)  # Adjust everything to geometric mean of deciduous and lowland values for summaries.  Geometric because the correction is multiplicative, to retain the relative differences among distances
  # Roads
  x<-rep(c(10,30,100,300),2)
  y<-as.vector(d[c("road buffer 10","road buffer 30","road buffer 100","road buffer 300"),,"Mean"])
  #	m<-gam(y~s(x,k=3,m=2),family="gaussian(log)")  # Smoothed fit through data.  Not used because some erratic results with limited sample sizes and horrible camera distributions
  y<-ifelse(y==0,min(y[y>0])/2,y)  # Need this to fit log-gaussian when there can be 0 values
  wt<-ifelse(x==10 | x==300,2,1)  # Weighted to help improve fit when there are wonky values in the middle (esp. zeroes)
  m<-glm(y~log(x),family=gaussian("log"),weights=wt)  # Alternative linear with log-distance
  p<-predict(m,newdata=data.frame(x=c(10,30,100,300)),se.fit=TRUE)
  # For each distance, combined distribution of data and smooth
  xd.log<-seq(log(0.001),log(max(d[,,"UCI"])),length.out=1000)  # Values to calculate combined likelihoods at - log scale
  combo<-array(NA,c(4,3))  # 4 distances, {max Lik,5%, 95%}
  for (i in 1:4) {  # Go through each edge distance
    mean.log1<-ifelse(d[i,1,"Mean"]==0,min(xd.log),log(d[i,1,"Mean"]))
    var.log1<-((d[i,1,"UCI"]-d[i,1,"LCI"])/1.65/2)^2 / exp(mean.log1)^2  # Variance based on 90% CIs, delta transformation to log scale.  Decid
    mean.log2<-ifelse(d[i,2,"Mean"]==0,min(xd.log),log(d[i,2,"Mean"]))
    var.log2<-((d[i,2,"UCI"]-d[i,2,"LCI"])/1.65/2)^2 / exp(mean.log2)^2  # Variance based on 90% CIs, delta transformation to log scale.  Lowland
    L1<-dnorm(xd.log,mean.log1,sqrt(var.log1))
    L2<-dnorm(xd.log,mean.log2,sqrt(var.log2))
    LP<-dnorm(xd.log,p$fit[i],p$se.fit[i])
    L.combo<-L1*L2*LP/max(L1*L2*LP)
    p.combo<-L.combo/sum(L.combo)
    combo[i,1]<-xd.log[which.max(p.combo)]
    combo[i,2:3]<-pinterp(xd.log,cumsum(p.combo),c(0.05,0.95))
    if (sp==1 & i==2)  {  # Example figure of combining likelihoods
      png(file=paste0(g_drive, "Results/OSM BADR/Smoothed edge distance likelihood example.png"),width=600,height=500)
      plot((xd.log),p.combo,xaxt="n",xlab="Density",ylab="Likelihood (sum=1)",typ="l",lwd=3,xlim=c(log(0.01),log(0.8)))
      axis(side=1,at=log(c(0.01,0.02,0.03,0.05,0.1,0.2,0.3,0.4,0.6,0.8,1)),rep("",11),tck=0.015,cex.axis=1.3)
      mtext(side=1,line=0.5,at=log(c(0.01,0.02,0.03,0.05,0.1,0.2,0.3,0.4,0.6,0.8,1)),c(0.01,0.02,0.03,0.05,0.1,0.2,0.3,0.4,0.6,0.8,1),cex=1.3)
      lines((xd.log),L1/sum(L1),col="orange",lwd=2)
      lines((xd.log),L2/sum(L2),col="green2",lwd=2)
      lines((xd.log),LP/sum(LP),col="cyan3",lwd=2)
      text(0.4,0.009,adj=0,"Combined",cex=1.4)
      text(0.4,0.0083,adj=0,"Smoothed Curve",cex=1.3,lwd=2,col="cyan3")
      text(0.4,0.0076,adj=0,"Standardized Deciduous",cex=1.3,col="orange")
      text(0.4,0.0069,adj=0,"Standardized Treed Lowland",cex=1.3,lwd=2,col="green2")
      mtext(side=3,at=log(0.01),"Likelihoods: Black bear Road at 30m",adj=0,cex=1.3)
      graphics.off()
    }  # End if for example figure
  }
  combo.roads[sp,,]<-exp(combo)
  fname<-paste(paste0(g_drive, "Results/OSM BADR/Figures/Smoothed/Smoothed figure Road distance "),SpTable[sp],".png",sep="")
  png(file=fname,width=500,height=500)
  ymax<-max(d1[1:4,,2:4])
  ymax<-ifelse(ymax>2*max(d1[1:4,,2]),max(d1[1:4,,2])*2,ymax)
  plot(c(10,30,100,300)-2,d1[c("road buffer 10","road buffer 30","road buffer 100","road buffer 300"),1,"Mean"],pch=18,cex=2,col=rgb(0.95,0.8,0.0),ylim=c(0,ymax),ylab="Relative density",xlab="Distance from road (m)",xaxt="n",yaxs="i")  # Using the actual data points here, not centred to geometric mean
  axis(side=1,at=c(10,30,100,300),lab=rep("",4),tck=0.015,cex.axis=1.3)
  mtext(side=1,line=0.5,at=c(10,30,100,300),c(10,30,100,300),cex=1.3)
  points(c(10,30,100,300)+2,d1[c("road buffer 10","road buffer 30","road buffer 100","road buffer 300"),2,"Mean"],pch=18,cex=2,col=rgb(0.75,0.92,0.4))  # Using the actual data points here, not centred to geometric mean
  for (i in 1:4) {
    lines(rep(c(10,30,100,300)[i],2)-2,d1[c("road buffer 10","road buffer 30","road buffer 100","road buffer 300")[i],1,c("LCI","UCI")],col=rgb(0.95,0.8,0.0))   # Using the actual data points here, not centred to geometric mean
    lines(rep(c(10,30,100,300)[i],2)+2,d1[c("road buffer 10","road buffer 30","road buffer 100","road buffer 300")[i],2,c("LCI","UCI")],col=rgb(0.75,0.92,0.4))   # Using the actual data points here, not centred to geometric mean
  }
  p1<-predict(m,newdata=data.frame(x=10:300),se.fit=TRUE)
  lines(10:300,exp(p1$fit),col="lightblue2")
  lines(10:300,exp(p1$fit+1.65*p1$se.fit),col="lightblue2",lty=2)
  lines(10:300,exp(p1$fit-1.65*p1$se.fit),col="lightblue2",lty=2)
  points(c(10,30,100,300),exp(combo[,1]),pch=18,cex=2,col="black")
  for (i in 1:4) {
    lines(rep(c(10,30,100,300)[i],2),exp(combo[i,2:3]),col="black")
  }
  mtext(side=3,at=10,adj=0,SpTable[sp],cex=1.4)
  graphics.off()

  # Plants/mines
  x<-rep(c(10,30,100,300),2)
  y<-as.vector(d[c("plant/mine buffer 10","plant/mine buffer 30","plant/mine buffer 100","plant/mine buffer 300"),,"Mean"])
  #	m<-gam(y~s(x,k=3,m=2),family="gaussian(log)")  # Smoothed fit through data.  Not used because some erratic results with limited sample sizes and horrible camera distributions
  y<-ifelse(y==0,min(y[y>0])/2,y)  # Need this to fit log-gaussian when there can be 0 values
  wt<-ifelse(x==10 | x==300,2,1)  # Weighted to help improve fit when there are wonky values in the middle (esp. zeroes)
  m<-glm(y~log(x),family=gaussian("log"),weights=wt)  # Alternative linear with log-distance
  p<-predict(m,newdata=data.frame(x=c(10,30,100,300)),se.fit=TRUE)
  # For each distance, combined distribution of data and smooth
  xd.log<-seq(log(0.001),log(max(d[,,"UCI"])),length.out=1000)  # Values to calculate combined likelihoods at - log scale
  combo<-array(NA,c(4,3))  # 4 distances, {max Lik,5%, 95%}
  for (i in 1:4) {
    mean.log1<-ifelse(d[i+4,1,"Mean"]==0,min(xd.log),log(d[i+4,1,"Mean"]))
    var.log1<-((d[i+4,1,"UCI"]-d[i+4,1,"LCI"])/1.65/2)^2 / exp(mean.log1)^2  # Variance based on 90% CIs, delta transformation to log scale.  Decid
    mean.log2<-ifelse(d[i+4,2,"Mean"]==0,min(xd.log),log(d[i+4,2,"Mean"]))
    var.log2<-((d[i+4,2,"UCI"]-d[i+4,2,"LCI"])/1.65/2)^2 / exp(mean.log2)^2  # Variance based on 90% CIs, delta transformation to log scale.  Lowland
    L1<-dnorm(xd.log,mean.log1,sqrt(var.log1))
    L2<-dnorm(xd.log,mean.log2,sqrt(var.log2))
    LP<-dnorm(xd.log,p$fit[i],p$se.fit[i])
    L.combo<-L1*L2*LP/max(L1*L2*LP)
    p.combo<-L.combo/sum(L.combo)
    combo[i,1]<-xd.log[which.max(p.combo)]
    combo[i,2:3]<-pinterp(xd.log,cumsum(p.combo),c(0.05,0.95))
  }
  combo.plant[sp,,]<-exp(combo)
  fname<-paste(paste0(g_drive, "Results/OSM BADR/Figures/Smoothed/Smoothed figure PlantMine distance "),SpTable[sp],".png",sep="")
  png(file=fname,width=500,height=500)
  ymax<-max(d1[5:8,,2:4])
  ymax<-ifelse(ymax>2*max(d1[5:8,,2]),max(d1[5:8,,2])*2,ymax)
  plot(c(10,30,100,300)-2,d1[5:8,1,"Mean"],pch=18,cex=2,col=rgb(0.95,0.8,0.0),ylim=c(0,ymax),ylab="Relative density",xlab="Distance from plant/mine (m)",xaxt="n",yaxs="i")  # Using the actual data points here, not centred to geometric mean
  axis(side=1,at=c(10,30,100,300),lab=rep("",4),tck=0.015,cex.axis=1.3)
  mtext(side=1,line=0.5,at=c(10,30,100,300),c(10,30,100,300),cex=1.3)
  points(c(10,30,100,300)+2,d1[5:8,2,"Mean"],pch=18,cex=2,col=rgb(0.75,0.92,0.4))  # Using the actual data points here, not centred to geometric mean
  for (i in 1:4) {
    lines(rep(c(10,30,100,300)[i],2)-2,d1[i+4,1,c("LCI","UCI")],col=rgb(0.95,0.8,0.0))   # Using the actual data points here, not centred to geometric mean
    lines(rep(c(10,30,100,300)[i],2)+2,d1[i+4,2,c("LCI","UCI")],col=rgb(0.75,0.92,0.4))   # Using the actual data points here, not centred to geometric mean
  }
  p1<-predict(m,newdata=data.frame(x=10:300),se.fit=TRUE)
  lines(10:300,exp(p1$fit),col="lightblue2")
  lines(10:300,exp(p1$fit+1.65*p1$se.fit),col="lightblue2",lty=2)
  lines(10:300,exp(p1$fit-1.65*p1$se.fit),col="lightblue2",lty=2)
  points(c(10,30,100,300),exp(combo[,1]),pch=18,cex=2,col="black")
  for (i in 1:4) {
    lines(rep(c(10,30,100,300)[i],2),exp(combo[i,2:3]),col="black")
  }
  mtext(side=3,at=10,adj=0,SpTable[sp],cex=1.4)
  graphics.off()

}  # Next sp

# On/off-HF
combo.hf<-array(NA,c(length(SpTable),7,3))  # To save results for species, 8 on/off-HF treatments, {mean, 5%, 95%}
dimnames(combo.hf)<-list(SpTable,c("reference","dense linear features Off","dense linear features On","low activity well pads Off","low activity well pads On","high activity in situ Off","high activity in situ On"),c("Mean","q5","q95"))
for (sp in 1:length(SpTable)) {
  d1<-x.hf[sp,c("reference","dense linear features Off","dense linear features On","low activity well pads Off","low activity well pads On","high activity in situ Off","high activity in situ On"),,]  # MAke sure order is correct
  DLR<-sum(d1[,"decidmix40","Mean"])/sum(d1[,"treedlow20","Mean"])  # deciduous:lowland ratio.  Analysis assumes same relationships in each, just a multiplicative offset
  d<-d1
  d[,"decidmix40",c("Mean","LCI","UCI")]<-d1[,"decidmix40",c("Mean","LCI","UCI")]/sqrt(DLR)  # Adjust everything to geometric mean of deciduous and lowland values for summaries.  Geometric because the correction is multiplicative, to retain the relative differences among distances
  d[,"treedlow20",c("Mean","LCI","UCI")]<-d1[,"treedlow20",c("Mean","LCI","UCI")]*sqrt(DLR)  # Adjust everything to geometric mean of deciduous and lowland values for summaries.  Geometric because the correction is multiplicative, to retain the relative differences among distances

  # On and off-HF treatments
  y.off<-as.vector(d[c("reference","dense linear features Off","low activity well pads Off","high activity in situ Off"),,"Mean"])
  y.off<-ifelse(y.off==0,min(y.off[y.off>0])/2,y.off)  # Need this to fit log-gaussian when there can be 0 values
  m.off<-glm(y.off~1,family=gaussian("log"))
  p.off<-predict(m.off,newdata=data.frame(x=0),se.fit=TRUE)
  y.on<-as.vector(d[c("dense linear features On","low activity well pads On","high activity in situ On"),,"Mean"])
  y.on<-ifelse(y.on==0,min(y.on[y.on>0])/2,y.on)  # Need this to fit log-gaussian when there can be 0 values
  m.on<-glm(y.on~1,family=gaussian("log"))
  p.on<-predict(m.on,newdata=data.frame(x=0),se.fit=TRUE)
  # For each distance, combined distribution of data and mean
  xd.log<-seq(log(0.001),log(max(d[,,"UCI"])),length.out=1000)  # Values to calculate combined likelihoods at - log scale
  combo<-array(NA,c(7,3))  # 7 treatments, {max Lik,5%, 95%}
  for (i in 1:7) {
    mean.log1<-ifelse(d[i,1,"Mean"]==0,min(xd.log),log(d[i,1,"Mean"]))
    var.log1<-((d[i,1,"UCI"]-d[i,1,"LCI"])/1.65/2)^2 / exp(mean.log1)^2  # Variance based on 90% CIs, delta transformation to log scale.  Decid
    mean.log2<-ifelse(d[i,2,"Mean"]==0,min(xd.log),log(d[i,2,"Mean"]))
    var.log2<-((d[i,2,"UCI"]-d[i,2,"LCI"])/1.65/2)^2 / exp(mean.log2)^2  # Variance based on 90% CIs, delta transformation to log scale.  Lowland
    L1<-dnorm(xd.log,mean.log1,sqrt(var.log1))
    L2<-dnorm(xd.log,mean.log2,sqrt(var.log2))
    if (i %in% c(1,2,4,6) == TRUE) {  # Off-HF treatments use that mean
      LP<-dnorm(xd.log,p.off$fit,p.off$se.fit)
    } else {  # On-HF treatments use that mean
      LP<-dnorm(xd.log,p.on$fit,p.on$se.fit)
    }
    L.combo<-L1*L2*LP/max(L1*L2*LP)
    p.combo<-L.combo/sum(L.combo)
    combo[i,1]<-xd.log[which.max(p.combo)]
    combo[i,2:3]<-pinterp(xd.log,cumsum(p.combo),c(0.05,0.95))
  }
  combo.hf[sp,,]<-exp(combo)
  fname<-paste(paste0(g_drive, "Results/OSM BADR/Figures/Smoothed/Smoothed figure On off HF "),SpTable[sp],".png",sep="")
  png(file=fname,width=600,height=500)
  ymax<-max(d1[,,2:4])
  ymax<-ifelse(ymax>2*max(d1[,,2]),max(d1[,,2])*2,ymax)
  x<-c(3,5,6,8,9,11,12)  # This is just for plotting
  plot(x-0.05,d1[,1,"Mean"],pch=18,cex=2,col=rgb(0.95,0.8,0.0),ylim=c(0,ymax),ylab="Relative density",xlab="",xaxt="n",yaxs="i")  # Using the actual data points here, not centred to geometric mean
  axis(side=1,at=x,lab=rep("",length(x)),tck=0.015,cex.axis=1.3)
  mtext(side=1,line=0.4,at=c(5,6,8,9,11,12),c("Off","On","Off","On","Off","On"),cex=1)
  mtext(side=1,line=1.5,at=c(3,5.5,8.5,11.5),c("Ref","Linear","Low Well","High in Situ"),cex=1.3)
  points(x+0.05,d1[,2,"Mean"],pch=18,cex=2,col=rgb(0.75,0.92,0.4))  # Using the actual data points here, not centred to geometric mean
  for (i in 1:7) {
    lines(rep(x[i],2)-0.05,d1[i,1,c("LCI","UCI")],col=rgb(0.95,0.8,0.0))   # Using the actual data points here, not centred to geometric mean
    lines(rep(x[i],2)+0.05,d1[i,2,c("LCI","UCI")],col=rgb(0.75,0.92,0.4))   # Using the actual data points here, not centred to geometric mean
  }
  lines(c(0,5.5),rep(exp(p.off$fit),2),col="lightcyan2")
  lines(c(0,5.5),rep(exp(p.off$fit+1.65*p.off$se.fit),2),col="lightcyan2",lty=2)
  lines(c(0,5.5),rep(exp(p.off$fit-1.65*p.off$se.fit),2),col="lightcyan2",lty=2)
  lines(c(7.5,8.5),rep(exp(p.off$fit),2),col="lightcyan2")
  lines(c(7.5,8.5),rep(exp(p.off$fit+1.65*p.off$se.fit),2),col="lightcyan2",lty=2)
  lines(c(7.5,8.5),rep(exp(p.off$fit-1.65*p.off$se.fit),2),col="lightcyan2",lty=2)
  lines(c(10.5,11.5),rep(exp(p.off$fit),2),col="lightcyan2")
  lines(c(10.5,11.5),rep(exp(p.off$fit+1.65*p.off$se.fit),2),col="lightcyan2",lty=2)
  lines(c(10.5,11.5),rep(exp(p.off$fit-1.65*p.off$se.fit),2),col="lightcyan2",lty=2)
  lines(c(5.5,6.5),rep(exp(p.on$fit),2),col="lightpink2")
  lines(c(5.5,6.5),rep(exp(p.on$fit+1.65*p.on$se.fit),2),col="lightpink2",lty=2)
  lines(c(5.5,6.5),rep(exp(p.on$fit-1.65*p.on$se.fit),2),col="lightpink2",lty=2)
  lines(c(8.5,9.5),rep(exp(p.on$fit),2),col="lightpink2")
  lines(c(8.5,9.5),rep(exp(p.on$fit+1.65*p.on$se.fit),2),col="lightpink2",lty=2)
  lines(c(8.5,9.5),rep(exp(p.on$fit-1.65*p.on$se.fit),2),col="lightpink2",lty=2)
  lines(c(11.5,12.5),rep(exp(p.on$fit),2),col="lightpink2")
  lines(c(11.5,12.5),rep(exp(p.on$fit+1.65*p.on$se.fit),2),col="lightpink2",lty=2)
  lines(c(11.5,12.5),rep(exp(p.on$fit-1.65*p.on$se.fit),2),col="lightpink2",lty=2)
  points(x,exp(combo[,1]),pch=18,cex=2,col="black")
  for (i in 1:7) {
    lines(rep(x[i],2),exp(combo[i,2:3]),col="black")
  }
  mtext(side=3,at=3,adj=0,SpTable[sp],cex=1.4)
  graphics.off()
}  # Next sp

# Export results
#SpToReport<-c("BlackBear","CanadaLynx","Coyote","Moose","SnowshoeHare","WhitetailedDeer")
SpToReport<-SpTable
for (sp1 in 1:length(SpToReport)) {
  sp<-match(SpToReport[sp1],SpTable)
  if (sp1==1) {
    q.roads<-data.frame(Sp=SpTable[sp],Dist=dimnames(combo.roads)[[2]],combo.roads[sp,,])
  } else {
    q.roads<-rbind(q.roads,data.frame(Sp=SpTable[sp],Dist=dimnames(combo.roads)[[2]],combo.roads[sp,,]))
  }
  if (sp1==1) {
    q.plant<-data.frame(Sp=SpTable[sp],Dist=dimnames(combo.plant)[[2]],combo.plant[sp,,])
  } else {
    q.plant<-rbind(q.plant,data.frame(Sp=SpTable[sp],Dist=dimnames(combo.plant)[[2]],combo.plant[sp,,]))
  }
  if (sp1==1) {
    q.hf<-data.frame(Sp=SpTable[sp],Treat=dimnames(combo.hf)[[2]],combo.hf[sp,,])
  } else {
    q.hf<-rbind(q.hf,data.frame(Sp=SpTable[sp],Treat=dimnames(combo.hf)[[2]],combo.hf[sp,,]))
  }
}
write.table(q.roads,file= paste0(g_drive, "Results/OSM BADR/OSM mammals 2021 2022 Smoothed Road distance.csv"),sep=",",row.names=FALSE)
write.table(q.plant,file= paste0(g_drive, "Results/OSM BADR/OSM mammals 2021 2022 Smoothed Plant mine distance.csv"),sep=",",row.names=FALSE)
write.table(q.hf,file=paste0(g_drive, "Results/OSM BADR/OSM mammals 2021 2022 Smoothed On off HF.csv"),sep=",",row.names=FALSE)










