#-----------------------------------------------------------------------------------------------------------------------

# Title:       Tidy data
# Date:        June 2023
# Author:      Marcus Becker

# Description:


#-----------------------------------------------------------------------------------------------------------------------

# Modeling

library(MuMIn)

# Load data
d <- read.csv(paste0(root, "supplemental/assumptions-tests/investigation-behaviour-by-veghf/processed/all-species-behaviour_wide_2020-10-12.csv"))

d <- df_behaviour

# Investigate combos
d_subset <- d %>%
  mutate(total_time_O = ifelse(common_name == "Snowshoe Hare" & VegHF1 == "YoungTreedWet",
                               total_time, total_time_O),
         total_time_L = ifelse(common_name == "Snowshoe Hare" & VegHF1 == "YoungTreedWet",
                               0, total_time_L)) |>
  group_by(common_name, VegHF1) |>
  add_count() |>
  filter(n >= 10,
         common_name != "Wolverine")

# Issues with tibbles
d_subset <- data.frame(d_subset)

# Let's build a for loop!

# Sp groups
species <- unique(d_subset$common_name)

# Loop through each species group, try each model variation, make predictions, output table
for (sp in 1:length(species)) {

  # print(paste(sp, length(species), species[species], date()))

  # Filter for just 1 species at a time
  d.sp <- d_subset[d_subset$common_name == species[sp], ]

  # Calculate proporation of time investigating, investigating + direct travel, investigating + direct travel + lingering
  d.sp$pInvest <- (d.sp$total_time_IP + d.sp$total_time_IC) / d.sp$total_time
  d.sp$pInvestDirect <- (d.sp$total_time_IP + d.sp$total_time_IC + d.sp$total_time_DP + d.sp$total_time_DC) / (d.sp$total_time + 0.001)
  d.sp$pInvestDirectLinger <- (d.sp$total_time_IP + d.sp$total_time_IC + d.sp$total_time_DP + d.sp$total_time_DC + d.sp$total_time_L) / (d.sp$total_time + 0.001)

  # Weights
  eff.wt <- d.sp$total_time / sum(d.sp$total_time) * nrow(d.sp)

  # Modeling
  m.pI <- m.pID<-m.pIDL<-list(NULL)
  m.pI[[1]] <- glm(pInvest~1,family="binomial",weights=eff.wt,data=d.sp)
  m.pI[[2]] <- glm(pInvest~VegHF1,family="binomial",weights=eff.wt,data=d.sp)
  m.pID[[1]] <- glm(pInvestDirect~1,family="binomial",weights=eff.wt,data=d.sp)
  m.pID[[2]] <- glm(pInvestDirect~VegHF1,family="binomial",weights=eff.wt,data=d.sp)
  m.pIDL[[1]] <- glm(pInvestDirectLinger~1,family="binomial",weights=eff.wt,data=d.sp)
  m.pIDL[[2]] <- glm(pInvestDirectLinger~VegHF1,family="binomial",weights=eff.wt,data=d.sp)
  aic.pI<-c(AICc(m.pI[[1]]),AICc(m.pI[[2]]))
  best.model.pI<-which.min(aic.pI)
  aic.pID<-c(AICc(m.pID[[1]]),AICc(m.pID[[2]]))
  best.model.pID<-which.min(aic.pID)
  aic.pIDL<-c(AICc(m.pIDL[[1]]),AICc(m.pIDL[[2]]))
  best.model.pIDL<-which.min(aic.pIDL)

  # Predictions
  veg.list<-sort(unique(d.sp$VegHF1))
  pI<-predict(m.pI[[best.model.pI]],newdata=data.frame(VegHF1=veg.list),se.fit=TRUE)
  pID<-predict(m.pID[[best.model.pID]],newdata=data.frame(VegHF1=veg.list),se.fit=TRUE)
  pIDL<-predict(m.pIDL[[best.model.pIDL]],newdata=data.frame(VegHF1=veg.list),se.fit=TRUE)
  q<-data.frame(VegHF=veg.list,nSeries=as.numeric(table(as.character(d.sp$VegHF1))),pInvest=plogis(pI$fit),pInvest.logitSE=pI$se.fit,pInvest.LCI=plogis(pI$fit-1.65*pI$se.fit),pInvest.UCI=plogis(pI$fit+1.65*pI$se.fit),
                pInvestDirect=plogis(pID$fit),pInvestDirect.logitSE=pID$se.fit,pInvestDirect.LCI=plogis(pID$fit-1.65*pID$se.fit),pInvestDirect.UCI=plogis(pID$fit+1.65*pID$se.fit),
                pInvestDirectLinger=plogis(pIDL$fit),pInvestDirectLinger.logitSE=pIDL$se.fit,pInvestDirectLinger.LCI=plogis(pIDL$fit-1.65*pIDL$se.fit),pInvestDirectLinger.UCI=plogis(pIDL$fit+1.65*pIDL$se.fit))

  # Save models
  fname <- paste0(g_drive, "data/supplemental/assumptions-tests/investigation-behaviour-by-veghf/results/models/behaviour model ", species[sp], ".RData", sep = "")
  save(file = fname, q, m.pI, m.pID, m.pIDL, best.model.pI, best.model.pID, best.model.pIDL)

  x <- t(cbind(q$pInvest, q$pInvestDirect, q$pInvestDirectLinger))

  png(file = paste0(g_drive, "results/figures/Investigation/", species[sp], " - investigation time with direct and linger.png"),
      width = 600,
      height = 600)

  par(mar = c(9, 5, 5, 2))

  b <- barplot(x, beside = TRUE, col = c("cornflowerblue", "blue", "darkblue"),
               ylab = "Proportion of time", ylim = c(0, 1),
               main = species[sp])

  mtext(side = 1, "Light = investigating time only; medium = invest + direct travel; dark = invest + direct + linger", line = 7)

  text(b[2,],rep(-0.01,nrow(q)),q$VegHF,adj=1,srt=45,xpd=TRUE)
  for (i in 1:nrow(q)) {
    lines(rep(b[1,i],2),c(q$pInvest.LCI[i],q$pInvest[i]),col="white")
    lines(rep(b[1,i],2),c(q$pInvest.UCI[i],q$pInvest[i]),col="cornflowerblue")
    lines(rep(b[2,i],2),c(q$pInvestDirect.LCI[i],q$pInvestDirect[i]),col="white")
    lines(rep(b[2,i],2),c(q$pInvestDirect.UCI[i],q$pInvestDirect[i]),col="blue")
    lines(rep(b[3,i],2),c(q$pInvestDirectLinger.LCI[i],q$pInvestDirectLinger[i]),col="white")
    lines(rep(b[3,i],2),c(q$pInvestDirectLinger.UCI[i],q$pInvestDirectLinger[i]),col="darkblue")
  }
  graphics.off()

}

#-----------------------------------------------------------------------------------------------------------------------

