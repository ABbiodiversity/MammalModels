# R Precison analysis for OSM BADR estimates

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

dplot<-function(x=x,y=y,cex=1.5,pch=5,...) {  # Plot with my preferred defaults
  # Plot with my default preferences
  plot(x,y,bty="n",tck=0.01,cex.axis=1.3,cex.lab=1.6,cex=cex,pch=pch,...)
  box(bty="l",lwd=2)
}

load(paste0(g_drive, "Results/OSM BADR/R objects OSM camera summaries Sept 2022.RData"))
# Saved in analysis script.
# x.hf for HF JEMs, x.dist for distance-buffer JEMS, [also JEM1 for overall (stratified) JEM estimates but that isn't used here]
# x.hf has dimensions [species, JEM-HF treatment, veg stratum, {n, mean, LCI, UCI}].  Treatments include various "NA" versions, which are valid in the non-HF JEMs (ref, pre-insitu) but are unneeded wetland treatments in other JEMs.  veg stratum includes unneeded "wetland"
# x.dist has dimensions [species, JEM-distance treatment, veg stratum, {n, mean, LCI, UCI}].

# New objects
load(paste0(g_drive, "Results/OSM BADR/R objects OSM camera summaries March 2024.RData"))

# Compile CI half-widths, logSE, and n for each HF-veg for all species
SpTable<-dimnames(x.hf2)[[1]]
vegHF<-paste(rep(dimnames(x.hf2)[[2]][c(1,2,3,4,5,6,8)],2),
             rep(dimnames(x.hf2)[[3]][1:2],each=7),sep="_")

#SpTable<-dimnames(x.hf)[[1]]
#vegHF<-paste(rep(dimnames(x.hf)[[2]][c(2,3,5,6,8,9,11)],2), rep(dimnames(x.hf)[[3]][1:2],each=7),sep="_")


# (Awkwardly) pulling out just the main JEM-HF treatments in the two main veg strata (omitting wetlands and associated HF=NA treatments).
# Change if treatment results summaries change
#i2<-rep(c(2,3,5,6,8,9,11),2)  # Indices for vegHF on second dimension of arrays  # Change if treatment results summaries change
i2 <- rep(c(1, 2, 3, 4, 5, 6, 8), 2)
i3<-rep(1:2,each=7)  # Indices for vegHF on third dimension of arrays  # Change if treatment results summaries change
CI.HF<-mean.HF<-logSE.HF<-array(NA,c(length(SpTable),length(vegHF)))  # CI half-width, mean, logSE
dimnames(CI.HF)<-dimnames(mean.HF)<-dimnames(logSE.HF)<-list(SpTable,vegHF)
# Number of *samples* per vegHF.
# Note definition of "sample" used in original analysis (can be >1 camera, incl. for ABMI sites and duplicated cameras at some BADR points).
# Note: The "diag" here and below are because R didn't subset the array the way I wanted, and I gave up trying to make it behave
n.HF<-diag(x.hf2[1,i2,i3,1])
names(n.HF)<-vegHF
for (sp in 1:length(SpTable)) {
  CI.HF[sp,]<-(diag(x.hf2[sp,i2,i3,4])-diag(x.hf2[sp,i2,i3,3]))/2  # CI half-width
  logSE.HF[sp,]<-(log(diag(x.hf2[sp,i2,i3,4]))-log(diag(x.hf2[sp,i2,i3,2])) + log(diag(x.hf2[sp,i2,i3,2]))-log(diag(x.hf2[sp,i2,i3,3])) )/1.65/2  # logSE, taken as the simple average of (logUCI-logmean)/1.65 and (logmean-logLCI)/1.65
  mean.HF[sp,]<-diag(x.hf2[sp,i2,i3,2])  # Mean
}

# And for each dist-veg
vegdist<-paste(rep(dimnames(x.dist2)[[2]],2), rep(dimnames(x.dist2)[[3]][1:2],each=dim(x.dist2)[2]),sep="_") # (Awkwardly) pulling out just the main JEM-dist treatments in the two main veg strata (omitting wetlands and associated HF=NA treatments).  Change if treatment results summaries change
i2<-rep(1:dim(x.dist2)[2],2)  # Indices for vegdist on second dimension of arrays # Change if treatment results summaries change
i3<-rep(1:2,each=dim(x.dist2)[2])  # Indices for vegdist on third dimension of arrays # Change if treatment results summaries change
CI.dist<-mean.dist<-logSE.dist<-array(NA,c(length(SpTable),length(vegdist)))  # CI half-width, mean
dimnames(CI.dist)<-dimnames(mean.dist)<-dimnames(logSE.dist)<-list(SpTable,vegdist)
n.dist<-diag(x.dist2[1,i2,i3,1])  # Number of *samples* per vegdist.  Note definition of "sample" used in original analysis (can be >1 camera, incl. for ABMI sites and duplicated cameras at some BADR points)
names(n.dist)<-vegdist
for (sp in 1:length(SpTable)) {
  CI.dist[sp,]<-(diag(x.dist2[sp,i2,i3,4])-diag(x.dist2[sp,i2,i3,3]))/2  # CI half-width
  logSE.dist[sp,]<-(log(diag(x.dist2[sp,i2,i3,4]))-log(diag(x.dist2[sp,i2,i3,2])) + log(diag(x.dist2[sp,i2,i3,2]))-log(diag(x.dist2[sp,i2,i3,3])) )/1.65/2  # logSE, taken as the simple average of (logUCI-logmean)/1.65 and (logmean-logLCI)/1.65
  mean.dist[sp,]<-diag(x.dist2[sp,i2,i3,2])  # Mean
}

# Combine
CI.all<-cbind(CI.HF,CI.dist)
logSE.all<-cbind(logSE.HF,logSE.dist)
mean.all<-cbind(mean.HF,mean.dist)
n.all<-c(n.HF,n.dist)

# VERSION USING logSE plot
m<-list(NULL)  # To save models
n.prec<-array(NA,c(length(SpTable),2,6))  # Sites needed for each species, with 1 or 2+ cameras, for CI half width/mean of {1.5,1.25,1,0.75,0.5}
dimnames(n.prec)<-list(SpTable,c("1cam","2cam"),c("CI1.5","CI1.25","CI1","CI0.75","CI0.5", "CI0.25"))
for (sp in 1:length(SpTable)) {
  i<-which(!is.na(logSE.all[sp,]) & n.all>1)  # Only plot and summarize non-0 treatments with >1 sample
  y<-logSE.all[sp,i] # Summarizing logSE (for count'ish data)
  col1<-ifelse(regexpr("decid",names(CI.all[sp,i]))>0,"#FF880077","#33AA3377")
  pch1<-ifelse(regexpr("buffer",names(CI.all[sp,i]))>0,15,18)
  cex1<-ifelse(pch1==15,2,2.5)  # Squares are too big...
  ncams<-ifelse(regexpr("buffer",names(CI.all[sp,i]))<=0 | regexpr("road buffer 300_decid",names(CI.all[sp,i]))>0,"2cam","1cam")  # Number of cameras per site, roughly - 1 for buffer treatments, except 300m from roads in decid where there are also ABMI sites; 2+ otherwise
  fname<-paste(paste0(g_drive, "Results/OSM BADR/Figures/Precision/logSE vs n for ",SpTable[sp],".png",sep=""))
  png(file=fname,width=500,height=500)
  dplot(log(n.all[i]),y,pch=pch1,cex=cex1,col=col1,xlab="Sample size",ylab="logSE",xaxt="n",yaxt="n",xlim=log(c(2,108)))
  axis(side=1,at=log(c(1,2,3,5,10,20,30,50,100)),c(1,2,3,5,10,20,30,50,100),tck=1,cex.axis=1.3,col="grey85")
  axis(side=2,at=(seq(0.0,2,0.2)),seq(0,2,0.2),tck=1,cex.axis=1.3,col="grey85")
  axis(side=1,at=log(c(1,2,3,5,10,20,30,50,100)),c(1,2,3,5,10,20,30,50,100),tck=0.015,cex.axis=1.3)
  axis(side=2,at=(seq(0.0,2,0.2)),seq(0,2,0.2),tck=0.015,cex.axis=1.3)
  points(log(n.all[i]),y,pch=pch1,cex=cex1,col=col1)
  lognall<-log(n.all[i])
  m[[sp]]<-lm(log(y)~lognall+ncams)
  p1<-exp(predict(m[[sp]],newdata=data.frame(lognall=log(2:200),ncams="1cam")))
  lines(log(2:200),p1,lwd=2,col="red4")
  p2<-exp(predict(m[[sp]],newdata=data.frame(lognall=log(2:200),ncams="2cam")))
  lines(log(2:200),p2,lwd=2,col="blue3")
  mtext(side=3,at=min(log(n.all[i])),adj=0,SpTable[sp],cex=1.4)
  graphics.off()
  # Save number of sites needed to reach different precision levels (with 1 vs 2+ cameras per site).  These are pulled out using interpolation from predictions of the empirical regression.
  p1<-predict(m[[sp]],newdata=data.frame(lognall=log(2:200),ncams="1cam"))
  p2<-predict(m[[sp]],newdata=data.frame(lognall=log(2:200),ncams="2cam"))
  n.prec[sp,1,]<-as.numeric(lapply(c(1.5,1.25,1,0.75,0.5, 0.25),function(x) which.min(abs(log(x)-p1)))) + 2  # Value of n where model-predicted log(CI half-width/mean) is closect to target log(CI half-width/mean) - for 1 camera/site.  +1 of the "+2" is because the predicitons start at n=2, and the other +1 is to round up to the higher whole sample size
  n.prec[sp,2,]<-as.numeric(lapply(c(1.5,1.25,1,0.75,0.5, 0.25),function(x) which.min(abs(log(x)-p2)))) + 2  # Value of n where model-predicted log(CI half-width/mean) is closect to target log(CI half-width/mean) - for 2+ cameras per site
}  # Next sp
# Save number of sites needed to reach different precision levels
q<-data.frame(n.prec[,1,])
colnames(q)<-paste(colnames(q),"1cam",sep="_")
q1<-data.frame(n.prec[,2,])
colnames(q1)<-paste(colnames(q1),"2cam",sep="_")
q<-cbind(q,q1)
write.table(q,file=paste0(g_drive, "Results/OSM BADR/Sites needed to reach precision targets OSM camera mammals.csv"),sep=",",col.names=NA)

# Graphs of 90% CI width around a mean of 1 at different n, 1 and 2+ cameras/site
n.list<-c(3,5,7,10,20,30,50,100)
y.ticks<-c(0.2,0.33,0.5,0.66,0.8,1,1.25,1.5,2,3,5)  # Version for log y-axis
y.ticks<-c(0.2,0.5,0.8,1,1.2,1.5,2,3,4,5)  # Version for ordinal y-axis
for (sp in 1:length(SpTable)) {
  p1<-exp(predict(m[[sp]],newdata=data.frame(lognall=log(n.list),ncams="1cam")))
  p2<-exp(predict(m[[sp]],newdata=data.frame(lognall=log(n.list),ncams="2cam")))
  lci1<-exp(-p1*1.65)  # Assuming mean of 1 (0 on log scale)
  uci1<-exp(p1*1.65)  # Assuming mean of 1 (0 on log scale)
  lci2<-exp(-p2*1.65)  # Assuming mean of 1 (0 on log scale)
  uci2<-exp(p2*1.65)  # Assuming mean of 1 (0 on log scale)
  fname<-paste(paste0(g_drive, "Results/OSM BADR/Figures/Precision/CI examples vs n ",SpTable[sp],".png",sep=""))
  png(file=fname,height=600,width=1000)
  par(mfrow=c(1,2))
  # 1 camera/site
  dplot(log(n.list),rep(1,8),pch=18,cex=2,xaxt="n",yaxt="n",xlab="Sample size",ylab="90% CI on mean=1",ylim=(c(0.2,5)))
  abline(0,0)
  axis(side=1,at=log(n.list),n.list,tck=0.01,cex.axis=1.3)
  axis(side=2,at=(y.ticks),rep("",length(y.ticks)),tck=1,col="grey85",cex.axis=1.3)
  axis(side=2,at=(y.ticks),y.ticks,tck=0.01,cex.axis=1.3,las=2)
  points(log(n.list),rep(1,8),pch=18,cex=2)
  for (i in 1:8) {
    lines(rep(log(n.list)[i],2),(c(lci1[i],uci1[i])),lwd=2)
  }
  mtext(side=3,at=log(3),adj=0,paste(SpTable[sp],": 1 camera/site",sep=""),cex=1.5)
  # 2+ cameras/site
  dplot(log(n.list),rep(1,8),pch=18,cex=2,xaxt="n",yaxt="n",xlab="Sample size",ylab="90% CI on mean=1",ylim=(c(0.2,5)))
  abline(0,0)
  axis(side=1,at=log(n.list),n.list,tck=0.01,cex.axis=1.3)
  axis(side=2,at=(y.ticks),rep("",length(y.ticks)),tck=1,col="grey85",cex.axis=1.3)
  axis(side=2,at=(y.ticks),y.ticks,tck=0.01,cex.axis=1.3,las=2)
  points(log(n.list),rep(1,8),pch=18,cex=2)
  for (i in 1:8) {
    lines(rep(log(n.list)[i],2),(c(lci2[i],uci2[i])),lwd=2)
  }
  mtext(side=3,at=log(3),adj=0,paste(SpTable[sp],": 2+ cameras/site",sep=""),cex=1.5)
  graphics.off()
}  # Next sp

# Graphs of the expected SE for a difference between two treatments as a function of the sample size of each
for (sp in 1:length(SpTable)) {
  p1<-exp(predict(m[[sp]],newdata=data.frame(lognall=log(2:100),ncams="1cam")))
  p2<-exp(predict(m[[sp]],newdata=data.frame(lognall=log(2:100),ncams="2cam")))
  p1.diff<-array(NA,c(length(p1),length(p1)))
  for (i in 1:length(p1)) p1.diff[i,]<-sqrt(p1[i]^2+p1^2)  # logSE of difference, following rule of summing variance
  p1.diff.ratio<-exp(p1.diff*1.65)
  p2.diff<-array(NA,c(length(p2),length(p2)))
  for (i in 1:length(p2)) p2.diff[i,]<-sqrt(p2[i]^2+p2^2)  # logSE of difference, following rule of summing variance
  p2.diff.ratio<-exp(p2.diff*1.65)
  fname<-paste(paste0(g_drive, "Results/OSM BADR/Figures/Precision/Significantly detectable ratio versus n ",SpTable[sp],".png",sep=""))
  png(file=fname,height=500,width=900)
  par(mfrow=c(1,2))
  # 1 camera/site
  contour(log(2:100),log(2:100),p1.diff.ratio,xaxt="n",yaxt="n",labcex=1.2,xlab="Sample size 1",ylab="Sample size 2",cex.lab=1.4,levels=c(40,30,20,10,8,6,5,4,3,2.5,2,1.5,1))
  axis(side=1,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=1,col="grey90",cex.axis=1.3)
  axis(side=1,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=0.01,cex.axis=1.3)
  axis(side=2,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=1,col="grey90",cex.axis=1.3)
  axis(side=2,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=0.01,cex.axis=1.3)
  mtext(side=3,at=log(2),adj=0,paste(SpTable[sp],": 1 camera/site",sep=""),cex=1.5)
  # 2+ cameras/site
  contour(log(2:100),log(2:100),p2.diff.ratio,xaxt="n",yaxt="n",labcex=1.2,xlab="Sample size 1",ylab="Sample size 2",cex.lab=1.4,levels=c(40,30,20,10,8,6,5,4,3,2.5,2,1.5,1))
  axis(side=1,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=1,col="grey90",cex.axis=1.3)
  axis(side=1,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=0.01,cex.axis=1.3)
  axis(side=2,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=1,col="grey90",cex.axis=1.3)
  axis(side=2,at=log(c(2,3,5,10,20,30,50,100)),c(2,3,5,10,20,30,50,100),tck=0.01,cex.axis=1.3)
  mtext(side=3,at=log(2),adj=0,paste(SpTable[sp],": 2+ cameras/site",sep=""),cex=1.5)
  graphics.off()
}  # Next sp




