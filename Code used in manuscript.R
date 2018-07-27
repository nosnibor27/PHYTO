##############################################################################
#
# Code used for Phytopathology manuscript titled:
# Utility of a multilevel modelling approach to investigate differences in isolation frequency of Fusarium culmorum in agricultural soil across the Inland Pacific Northwest
#
# Revision Date: 2018-06-26 version 1.0.0
# 
#   Section 1:   Loading in dataframe for current study
#   Section 2:   Processing variables and creating features
#   Section 3:   Loading in dataframe with data from Backhouse et al. (2004)
#   Section 4:   Creating Figure 1
#   Section 5:   Running model using Eq. [1]
#   Section 6:   Code to produce pairs plots
#   Section 7:   Creating Figure 2
#   Section 8:   Creating Figure 3
#   Section 9:   Creating Figure 4
#   Section 10:  Testing sensitivity to priors for variance parameters
#   Section 11:  Creating Supplemental Figure 1
#   Section 12:  Running model using Eq. [2] with annual precipitaiton
#   Section 13:  Running model using Eq. [2] with annual evapotranspiration
#   Section 14:  Running model using Eq. [2] with annual growing degree-days
#   Section 15:  Running models using Eq. [3] with dataset from Backhouse et al. (2004)
#   Section 16:  Creating Figure 5
#   Section 17:  Posterior field offsets based on model used
#   Section 18:  Addressing temporal correlation by changing definition of sampling iteration
#   Section 19:  Creating Supplemental Figure 2
#   Section 20:  Making predictions using Eq. [4]
#   Section 21:  Creating Figure 6
#
# Author: Andrew Lloyd Robinson
#
##############################################################################

#dependent packages
library(rethinking)
#increasing max print for summaries of model parameters
options(max.print = 1000000)
#Section 1: Loading in dataframes----
#loading in data frame
# field       - field number
# quadrat     - quadrat number
# dilution.1  - soil dilution factor first replicate
# total.1     - total soil Fusaria first replicate
# culmorum.1  - total F. culmorum first replicate
# dilution.2  - soil dilution factor second replicate
# total.2     - total soil Fusaria second replicate
# culmorum.2  - total F. culmorum second replicate
# culmorum    - total F. culmorum per quadrat by summing culmorum.1 and culmorum.2
# total       - total soil Fusaria per quadrat by summing total.1 and total.2
af = read.csv(file = "soilpop.csv", stringsAsFactors = FALSE)
#Section 2: Processing variables and creating features----
#adding a reference column
af$number <- as.integer(rep(seq(1,72,1),each=9))
af$acre <- as.integer(rep(seq(1,81,1),8))
#adding proportion
af$prop <- af$culmorum/af$total
#annual rainfall fields 1:9
annual.precip <- c(661.5669368,721.3644632,578.1862053,554.1764842,498.9040711,455.9379342,391.1394974,393.6508053,379.4867026)
#formatting to match dataframe
precip.seq <- rep(annual.precip,8)
af$precip.seq  <- rep(precip.seq,each=9)
af$precip.seq.s <- (af$precip.seq-mean(af$precip.seq))/sd(af$precip.seq)
#annual evapotranspiration fields 1:9
annual.evap <- c(996.0147474,973.3092553,986.0191658,992.7352079,1032.804297,1090.76265,1104.833661,1118.045392,1087.534961)
#formatting to match dataframe
evap.seq <- rep(annual.evap,8)
af$evap.seq  <- rep(evap.seq,each=9)
af$evap.seq.s <- (af$evap.seq-mean(af$evap.seq))/sd(af$evap.seq)
#annual growing degree days fields 1:9
annual.gdd <- c(3419.356404,3334.144521,3419.783534,3432.88703,3480.269411,3673.363232,3632.976053,3716.581403,3473.457341)
#formatting to match dataframe
gdd.seq <- rep(annual.gdd,8)
af$gdd.seq  <- rep(gdd.seq,each=9)
af$gdd.seq.s <- (af$gdd.seq-mean(af$gdd.seq))/sd(af$gdd.seq)
#Section 3: Loading in dataframe with data from Backhouse et al. (2004)----
# F..pse      - total Fusaria
# F..cul      - total F. culmorum
bf = read.csv(file = "dbackhouse2004.csv", stringsAsFactors = FALSE)
#adding field number
bf$field <- seq(1,163,1)
#standardizing variables
bf$precip.s <- (bf$precip-mean(bf$precip))/sd(bf$precip)
bf$gdd.s <- (bf$gdd-mean(bf$gdd))/sd(bf$gdd)
bf$prop <- bf$F..cul/bf$F..pse
#Section 4: Creating Figure 1----
par(mfrow=c(3,2))
plot(af$field,af$prop,pch=16,col=col.alpha("black",alpha=0.2),main="A",ylim=c(0,1),ylab=expression("Proportion "~italic("F. culmorum")),xlab="Field",axes=FALSE)
box()
axis(1,at=seq(1,9,1),las=1)
axis(2,at=seq(0,1,0.2),las=2)
plot(bf$field,bf$prop,pch=16,col=col.alpha("black",alpha=0.2),main="B",ylim=c(0,1),ylab=expression("Proportion "~italic("F. culmorum")),xlab="Field",axes=FALSE)
box()
axis(1,at=seq(0,163,40),las=1)
axis(2,at=seq(0,1,0.2),las=1)
plot(af$precip.seq,af$prop,pch=16,col=col.alpha("black",alpha=0.2),main="C",ylim=c(0,1),xlim=c(300,800),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")))
plot(bf$precip,bf$prop,pch=16,col=col.alpha("black",alpha=0.2),main="D",ylim=c(0,1),xlim=c(300,800),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")))
plot(af$gdd.seq,af$prop,pch=16,col=col.alpha("black",alpha=0.2),main="E",ylim=c(0,1),xlim=c(3200,3800),xlab="Annual growing degree-days (GDD)",ylab=expression("Proportion "~italic("F. culmorum")))
plot(bf$gdd,bf$prop,pch=16,col=col.alpha("black",alpha=0.2),main="F",ylim=c(0,1),xlim=c(1000,1800),xlab="Annual growing degree-days (GDD)",ylab=expression("Proportion "~italic("F. culmorum")))
#Section 5: Running model using Eq. [1]----
#list for Stan
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre)
#varying intercepts model equation
m.1 <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat],
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    a~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 6000,
  chains = 4,
  cores = 4 
)
#parameter summary used to produce Table 3 and 4, Rhat is formal diagnostic for convergence
precis(m.1,depth=2,prob = 0.95)
#extracting samples
post.1 <- extract.samples(m.1,n=20000)
#Section 6: Code to produce pairs plots----
#comparing fields

#comparing quatrats within field 1
pairs(post.1$a_quadrat[1:1000,1:9])
#comparing quatrats within field 2
pairs(post.1$a_quadrat[1:1000,10:18])
#comparing quatrats within field 3
pairs(post.1$a_quadrat[1:1000,19:27])
#comparing quatrats within field 4
pairs(post.1$a_quadrat[1:1000,28:36])
#comparing quatrats within field 5
pairs(post.1$a_quadrat[1:1000,37:45])
#comparing quatrats within field 6
pairs(post.1$a_quadrat[1:1000,46:54])
#comparing quatrats within field 7
pairs(post.1$a_quadrat[1:1000,55:63])
#comparing quatrats within field 8
pairs(post.1$a_quadrat[1:1000,64:72])
#comparing quatrats within field 9
pairs(post.1$a_quadrat[1:1000,73:81])
#comparing iterations 
pairs(post.1$a_iteration[1:1000,1:24])
pairs(post.1$a_iteration[1:1000,25:48])
pairs(post.1$a_iteration[1:1000,49:72])

windows(100,100)
par(mar=c(0,0,0,0))
pairs(post.1$a_iteration[1:1000,],gap=0)

#Section 7: Creating Figure 2----
#simulating counts of F. culmorum from the posterior distribution
m.1.sim <- sim(m.1,n=1000)
#making matrix of x values
m <- matrix(0,1000,9)
m[,1] = 1
m[,2] = 2
m[,3] = 3
m[,4] = 4
m[,5] = 5
m[,6] = 6
m[,7] = 7
m[,8] = 8
m[,9] = 9
#season names for titles
seasons <- as.vector(c("Jun 2016","Sep 2016","Dec 2016","Mar 2017","Jun 2017","Sep 2017","Dec 2017","Mar 2018"))
#plotting
par(mfrow=c(9,8))
par(oma=c(4,4,2,1))
ls <- c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(1,648,81)
ns <- c("Field 1"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(seasons[n],line=0,side=3)
  mtext(ns[n],line=-1.5,side=3)
}
#field 2
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(10,648,81)
ns <- c("Field 2"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
#field 3
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(19,648,81)
ns <- c("Field 3"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
#field 4
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(28,648,81)
ns <- c("Field 4"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
#field 5
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(37,648,81)
ns <- c("Field 5"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
#field 6
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(46,648,81)
ns <- c("Field 6"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
#field 7
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(55,648,81)
ns <- c("Field 7"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
#field 8
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(64,648,81)
ns <- c("Field 8"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=FALSE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
#field 9
par(mar=c(0.5,0.5,0.5,0.5))
fs <- seq(73,648,81)
ns <- c("Field 9"," "," "," "," "," "," "," "," ")
for(n in 1:8){
  plot(0,type="n",xlim=c(1,9),ylim=c(0,40),xlab=" ",ylab=" ",axes=FALSE)
  box()
  axis(1,at=seq(1,9,1),las=1,labels=TRUE)
  axis(2,at=seq(0,40,10),las=1,labels=ls[n])
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("orange",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=16,col=col.alpha("blue",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
mtext("Quadrat number", side = 1, outer = TRUE, line = 2)
mtext(expression("Counts of "~italic("F. culmorum")), side = 2, outer = TRUE, line = 2)

#Section 8: Creating Figure 3----
#dataframe with marginal field offsets
eq.1.f <- data.frame(post.1$a_field)
par(mfrow=c(2,2))
par(cex=1.5)
plot(0,type="n",xlim=c(0,10),ylim=c(-4,4),xlab="Field",ylab="Log odds",axes=FALSE,main="A")
box()
axis(1,at=seq(1,9,1),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=1,y=eq.1.f[n,1],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=2,y=eq.1.f[n,2],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=3,y=eq.1.f[n,3],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=4,y=eq.1.f[n,4],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=5,y=eq.1.f[n,5],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=6,y=eq.1.f[n,6],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=7,y=eq.1.f[n,7],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=8,y=eq.1.f[n,8],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=9,y=eq.1.f[n,9],pch=16,col=col.alpha("black",alpha=0.1))
}
abline(h=0)
plot(0,type="n",xlim=c(350,750),ylim=c(-4,4),xlab="Annual precipitation (mm)",ylab="Log odds",axes=FALSE,main="B")
box()
axis(1,at=seq(350,750,50),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=annual.precip[1],y=eq.1.f[n,1],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[2],y=eq.1.f[n,2],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[3],y=eq.1.f[n,3],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[4],y=eq.1.f[n,4],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[5],y=eq.1.f[n,5],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[6],y=eq.1.f[n,6],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[7],y=eq.1.f[n,7],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[8],y=eq.1.f[n,8],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[9],y=eq.1.f[n,9],pch=16,col=col.alpha("black",alpha=0.1))
}
abline(h=0)
plot(0,type="n",xlim=c(950,1150),ylim=c(-4,4),xlab="Annual evapotranspiration (mm)",ylab="Log odds",axes=FALSE,main="C")
box()
axis(1,at=seq(950,1150,50),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=annual.evap[1],y=eq.1.f[n,1],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[2],y=eq.1.f[n,2],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[3],y=eq.1.f[n,3],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[4],y=eq.1.f[n,4],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[5],y=eq.1.f[n,5],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[6],y=eq.1.f[n,6],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[7],y=eq.1.f[n,7],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[8],y=eq.1.f[n,8],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[9],y=eq.1.f[n,9],pch=16,col=col.alpha("black",alpha=0.1))
}
abline(h=0)
plot(0,type="n",xlim=c(3300,3800),ylim=c(-4,4),xlab="Annual growing degree-days (GDD)",ylab="Log odds",axes=FALSE,main="D")
box()
axis(1,at=seq(3300,3800,100),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=annual.gdd[1],y=eq.1.f[n,1],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[2],y=eq.1.f[n,2],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[3],y=eq.1.f[n,3],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[4],y=eq.1.f[n,4],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[5],y=eq.1.f[n,5],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[6],y=eq.1.f[n,6],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[7],y=eq.1.f[n,7],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[8],y=eq.1.f[n,8],pch=16,col=col.alpha("black",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[9],y=eq.1.f[n,9],pch=16,col=col.alpha("black",alpha=0.1))
}
abline(h=0)
#Section 9: Creating Figure 4----
#dataframe with marginal quadrat offsets
eq.1.q <- data.frame(post.1$a_quadrat)
#sequence of relative elevations
rel.elev <- as.vector(c(-1.919444444,3.205555556,6.805555556,-2.669444444,1.655555556,7.305555556,-10.69444444,-5.169444444,1.480555556,0.236111111,-1.063888889,-1.238888889,2.861111111,1.136111111,0.561111111,3.461111111,-2.538888889,-3.413888889,1.286111111,4.236111111,5.436111111,-2.263888889,-1.563888889,-1.288888889,-3.663888889,-2.038888889,-0.138888889,4.05,0.8,-1.025,4.525,-1.3,-3.725,2.425,-2,-3.75,1.052777778,-6.672222222,-13.67222222,3.027777778,-0.497222222,-1.647222222,0.552777778,7.477777778,10.37777778,13.63611111,9.636111111,5.411111111,6.236111111,0.786111111,-5.063888889,-6.688888889,-10.31388889,-13.63888889,11.53611111,3.811111111,-4.563888889,8.061111111,0.436111111,-7.663888889,4.236111111,-3.713888889,-12.13888889,-12.50555556,-2.155555556,5.369444444,-7.405555556,1.519444444,8.194444444,-4.680555556,3.294444444,8.369444444,0.361111111,-0.388888889,-0.288888889,0.436111111,-0.138888889,-0.213888889,0.061111111,0.036111111,0.136111111))
#index where 95% HDPI excludes 0
q.pos <- as.vector(c(7,8,9,10,11,26,30,34,35,38,39,42,43,50,53,54,56,63))
q.neg <- as.vector(c(5,28,31,37,40,45,48,49))
par(cex=1.5)
plot(0,type="n",xlim=c(-15,15),ylim=c(-4,4),xlab="Relative elevation (m)",ylab="Log odds",axes=FALSE)
box()
axis(1,at=seq(-15,15,5),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:18){
  for(i in 1:1000){
    points(x=rel.elev[q.pos[n]],y=eq.1.q[i,q.pos[n]],pch=16,col=col.alpha("green3",alpha=0.1))
  }
}
for(n in 1:8){
  for(i in 1:1000){
    points(x=rel.elev[q.neg[n]],y=eq.1.q[i,q.neg[n]],pch=16,col=col.alpha("red3",alpha=0.1))
  }
}
abline(h=0)
abline(v=0)
#Section 10: Testing sensitivity to priors for variance parameters----
#list for Stan
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre)
#varying intercepts model equation, increased all variances by an order of magnitude
m.1.s <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat],
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    a~dnorm(0,100),
    sigma_field~dcauchy(0,10),
    sigma_iteration~dcauchy(0,10),
    sigma_quadrat~dcauchy(0,10)
  ),
  data = test.list,
  warmup = 1000,
  iter = 6000,
  chains = 4,
  cores = 4 
)
#parameter summary
precis(m.1.s,depth=2,prob = 0.95)
#Section 11: Creating Supplemental Figure 1----
#collecting output
output.1 <- precis(m.1,depth=2,prob = 0.95)
output.2 <- precis(m.1.s,depth=2,prob = 0.95)
#plotting comparison
par(cex=1.5)
plot(output.1@output$Mean,output.2@output$Mean,pch=16,col=col.alpha("black",alpha=0.5),xlab="Posterior parameter mean (Eq. 1)",ylab="Posterior parameter mean (Eq.1 using 10X prior variance)")
abline(a=0,b=1)
#Section 12: Running model using Eq. [2] with annual precipitaiton----
#list for Stan
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  precip=af$precip.seq.s)
#annual precipitation model equation
m.2 <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 6000 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.2,depth=2,prob=0.95)
#Section 13: Running model using Eq. [2] with annual evapotranspiration----
#list for Stan
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  evap=af$evap.seq.s)
#annual evapotranspiration model equation
m.3 <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*evap,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 6000 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.3,depth=2,prob=0.95)
#Section 14: Running model using Eq. [2] with annual growing degree-days----
#list for Stan
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  gdd=af$gdd.seq.s)
#annual growing degree-days model equation
m.4 <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*gdd,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 6000 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.4,depth=2,prob=0.95)
#Section 15: Running models using Eq. [3] with dataset from Backhouse et al. (2004)----
#list for model
test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  precip=bf$precip.s)

#varying intercepts model for annual precipitation
m.5 <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*precip,
    a_field[field]~dnorm(0,10),
    c(a,b)~dnorm(0,10)
  ),
  data = test.list,
  warmup = 1000,
  iter = 6000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.5,depth=2,prob = 0.95)
#list for model
test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  gdd=bf$gdd.s)
#varying intercepts model for annual growing degree days
m.6 <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*gdd,
    a_field[field]~dnorm(0,10),
    c(a,b)~dnorm(0,10)
  ),
  data = test.list,
  warmup = 1000,
  iter = 6000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.6,depth=2,prob = 0.95)
#Section 16: Creating Figure 5----
#extracting samples
post.2 <- extract.samples(m.2,n=20000)
post.3 <- extract.samples(m.3,n=20000)
post.4 <- extract.samples(m.4,n=20000)
post.5 <- extract.samples(m.5,n=20000)
post.6 <- extract.samples(m.6,n=20000)
label.ref <- as.vector(c("Annual Precipitation",
                      "Annual Precipitation (Backhouse et al. 2004)",
                      "Annual growing degree-days",
                      "Annual growing degree-days (Backhouse et al. 2004)",
                      "Annual evapotranspiration"))
#plotting
par(mfrow=c(1,2))
par(cex=1.5)
plot.new()
plot(0,type="n",xlim=c(-6,6),ylim=c(0,6),xlab="Posterior distribution of slope parameter",ylab=" ",axes=FALSE)
box()
axis(1,at=seq(-6,6,1),las=1)
axis(2,at=seq(1,5,1),las=1,labels=rev(label.ref))
abline(v=0)
points(y=rep(5,1000),x=post.2$b[1:1000],pch=16,col=col.alpha("black",alpha=0.2))
points(y=rep(4,1000),x=post.5$b[1:1000],pch=16,col=col.alpha("black",alpha=0.2))
points(y=rep(3,1000),x=post.4$b[1:1000],pch=16,col=col.alpha("black",alpha=0.2))
points(y=rep(2,1000),x=post.6$b[1:1000],pch=16,col=col.alpha("black",alpha=0.2))
points(y=rep(1,1000),x=post.3$b[1:1000],pch=16,col=col.alpha("black",alpha=0.2))
#Section 17: Posterior field offsets based on model used----
par(mfrow=c(2,2))
par(cex=1.5)
#eq 1
plot(0,type="n",xlim=c(1,9),ylim=c(-4,4),xlab="Field",ylab="Log odds",main="A",axes=FALSE)
box()
for(n in 1:9){
  points(x=rep(n,100),y=post.1$a_field[1:100,n],pch=16,col=col.alpha("black",0.2))
}
axis(1,at=seq(1,9,1),las=1)
axis(2,at=seq(-4,4,1),las=1)
abline(h=0)
#eq 2 with precip
plot(0,type="n",xlim=c(1,9),ylim=c(-4,4),xlab="Field",ylab="Log odds",main="B",axes=FALSE)
box()
for(n in 1:9){
  points(x=rep(n,100),y=post.2$a_field[1:100,n],pch=16,col=col.alpha("black",0.2))
}
axis(1,at=seq(1,9,1),las=1)
axis(2,at=seq(-4,4,1),las=1)
abline(h=0)
#eq 2 with gdd
plot(0,type="n",xlim=c(1,9),ylim=c(-4,4),xlab="Field",ylab="Log odds",main="C",axes=FALSE)
box()
for(n in 1:9){
  points(x=rep(n,100),y=post.4$a_field[1:100,n],pch=16,col=col.alpha("black",0.2))
}
axis(1,at=seq(1,9,1),las=1)
axis(2,at=seq(-4,4,1),las=1)
abline(h=0)
#eq 2 with evap
plot(0,type="n",xlim=c(1,9),ylim=c(-4,4),xlab="Field",ylab="Log odds",main="D",axes=FALSE)
box()
for(n in 1:9){
  points(x=rep(n,100),y=post.3$a_field[1:100,n],pch=16,col=col.alpha("black",0.2))
}
axis(1,at=seq(1,9,1),las=1)
axis(2,at=seq(-4,4,1),las=1)
abline(h=0)
#Section 18: Adressing temporal correlation by changing definition of sampling iteration----
#adding temporal term
af$temporal <- rep(seq(1,8,1),each=81)
#list for Stan
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$temporal,
                  quadrat=af$acre)
#varying intercepts model equation
m.1.temp <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat],
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    a~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 6000,
  chains = 4,
  cores = 4 
)
#parameter summary
precis(m.1.temp,depth=2,prob = 0.95)
#adding seasonal term
seasons <- rep(seq(1,4,1),each=81)
af$seasonal <- c(seasons,seasons)
#list for Stan
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$seasonal,
                  quadrat=af$acre)
#varying intercepts model equation
m.1.seas <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat],
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    a~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 6000,
  chains = 4,
  cores = 4 
)
#parameter summary
precis(m.1.seas,depth=2,prob = 0.95)
#Section 19: Creating Supplemental Figure 2----
par(mfrow=c(2,3))
plot(precis(m.1,pars="a_field",depth=2),main="A")
plot(precis(m.1.temp,pars="a_field",depth=2),main="B")
plot(precis(m.1.seas,pars="a_field",depth=2),main="C")
plot(precis(m.1,pars="a_iteration",depth=2),main="D")
plot(precis(m.1.temp,pars="a_iteration",depth=2),main="E")
plot(precis(m.1.seas,pars="a_iteration",depth=2),main="F")
#Section 20: Making predictions using Eq. [4]----
#extracting posterior slope and intercept
m.p.us <- data.frame(A=post.2$a,B=post.2$b)
m.p.aus <- data.frame(A=post.5$a,B=post.2$b)
m.gdd.us <- data.frame(A=post.4$a,B=post.4$b)
m.gdd.aus <- data.frame(A=post.6$a,B=post.6$b)
#US dataset
precip.model.us <- function(x){
  z <- (x-mean(af$precip.seq))/sd(af$precip.seq)
  predictions <- logistic(m.p.us$A + m.p.us$B*z)
}
gdd.model.us <- function(x){
  z <- (x-mean(af$gdd.seq))/sd(af$gdd.seq)
  predictions <- logistic(m.gdd.us$A + m.gdd.us$B*z)
}
#AUS dataset
precip.model.aus <- function(x){
  z <- (x-mean(bf$precip))/sd(bf$precip)
  predictions <- logistic(m.p.aus$A + m.p.aus$B*z)
}
gdd.model.aus <- function(x){
  z <- (x-mean(bf$gdd))/sd(bf$gdd)
  predictions <- logistic(m.gdd.aus$A + m.gdd.aus$B*z)
}
#Section 21: Creating Figure 6----
par(mfrow=c(2,2))
par(cex=1.5)
plot(0,type="n",xlim=c(450,750),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main="A")
for(n in seq(450,750,10)){
  est <- precip.model.us(n)
  points(x=rep(n,1000),y=est[1:1000],pch=16,col=col.alpha("black",0.1))
}
plot(0,type="n",xlim=c(3300,3700),ylim=c(0,0.5),xlab="Annual growing degree-days (GDD)",ylab=expression("Proportion "~italic("F. culmorum")),main="B")
for(n in seq(3300,3700,10)){
  est <- gdd.model.us(n)
  points(x=rep(n,1000),y=est[1:1000],pch=16,col=col.alpha("black",0.1))
}
plot(0,type="n",xlim=c(300,700),ylim=c(0,0.1),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main="C")
for(n in seq(300,700,10)){
  est <- precip.model.aus(n)
  points(x=rep(n,1000),y=est[1:1000],pch=16,col=col.alpha("black",0.1))
}
plot(0,type="n",xlim=c(1000,1600),ylim=c(0,0.1),xlab="Annual growing degree-days (GDD)",ylab=expression("Proportion "~italic("F. culmorum")),main="D")
for(n in seq(1000,1600,10)){
  est <- gdd.model.aus(n)
  points(x=rep(n,1000),y=est[1:1000],pch=16,col=col.alpha("black",0.1))
}

