##############################################################################
#
# Code used for Phytopathology manuscript titled:
# Utility of a multilevel modelling approach to investigate differences in isolation frequency of Fusarium culmorum in agricultural soil across the Inland Pacific Northwest
#
# Revision Date: 2018-10-17
# 
#   Section 1:   Loading in dataframe for current study
#   Section 2:   Processing variables and creating features
#   Section 3:   Loading in dataframe with data from Backhouse et al. (2004)
#   Section 4:   Creating Figure 1
#   Section 5:   Creating Figure 2
#   Section 6:   Running model using Eq. [1]
#   Section 7:   Testing sensitivity to priors for variance parameters
#   Section 8:   Creating Supplemental Figure 1
#   Section 9:   Adressing temporal correlation by changing definition of sampling iteration
#   Section 10:  Creating Supplemental Figure 2
#   Section 11:  Creating Figure 3
#   Section 12:  Creating Figure 4
#   Section 13:  Creating Supplemental Figure 3
#   Section 14:  All model combinarions with climate terms using Eq. [2] through Eq. [5]
#   Section 15:  Australian data
#   Section 16:  Generating model predictions
#   Section 17:  Creating Figure 5
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
par(cex(1.5))
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
#Section 5: Creating Figure 2----
par(mfrow=c(2,2))

plot(x=af$evap.seq,y=af$precip.seq,pch=16,col="black",main="A",xlab="Annual evapotranspiration (mm)",ylab="Annual precipitation (mm)")
abline(lm(af$precip.seq~af$evap.seq))
mtext(text=paste("R = ",round(cor(annual.evap,annual.precip),digits=2)),side=3,line=-2)

plot(x=af$gdd.seq,y=af$precip.seq,pch=16,col="black",main="B",xlab="Annual growing degree-days (GDD)",ylab="Annual precipitation (mm)")
abline(lm(af$precip.seq~af$gdd.seq))
mtext(text=paste("R = ",round(cor(annual.gdd,annual.precip),digits=2)),side=3,line=-2)

plot(x=af$evap.seq,y=af$gdd.seq,pch=16,col="black",main="C",xlab="Annual evapotranspiration (mm)",ylab="Annual growing degree-days (GDD)")
abline(lm(af$gdd.seq~af$evap.seq))
mtext(text=paste("R = ",round(cor(annual.gdd,annual.evap),digits=2)),side=3,line=-2)

plot(x=bf$gdd,y=bf$precip,pch=16,col="black",main="D",xlab="Annual growing degree-days (GDD)",ylab="Annual precipitation (mm)")
abline(lm(bf$precip~bf$gdd))
mtext(text=paste("R = ",round(cor(bf$precip,bf$gdd),digits=2)),side=3,line=-2)
#Section 6: Running model using Eq. [1]----
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
#Section 7: Testing sensitivity to priors for variance parameters----
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
#Section 8: Creating Supplemental Figure 1----
#collecting output
output.1 <- precis(m.1,depth=2,prob = 0.95)
output.2 <- precis(m.1.s,depth=2,prob = 0.95)
#plotting comparison
par(cex=1.5)
plot(output.1@output$Mean,output.2@output$Mean,pch=16,col=col.alpha("black",alpha=0.5),xlab="Posterior parameter mean (Eq. 1)",ylab="Posterior parameter mean (Eq.1 using 10X prior variance)")
abline(a=0,b=1)
#Section 9: Adressing temporal correlation by changing definition of sampling iteration----
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
#Section 10: Creating Supplemental Figure 2----
par(mfrow=c(2,3))
plot(precis(m.1,pars="a_field",depth=2),main="A")
mtext("Parameter",side=2,line=2.5)
plot(precis(m.1.temp,pars="a_field",depth=2),main="B")
mtext("Parameter",side=2,line=3)
plot(precis(m.1.seas,pars="a_field",depth=2),main="C")
mtext("Parameter",side=2,line=3)
plot(precis(m.1,pars="a_iteration",depth=2),main="D")
mtext("Parameter",side=2,line=2.5)
plot(precis(m.1.temp,pars="a_iteration",depth=2),main="E")
mtext("Parameter",side=2,line=3)
plot(precis(m.1.seas,pars="a_iteration",depth=2),main="F")
mtext("Parameter",side=2,line=3)
#Section 11: Creating Figure 3----
#simulating counts of F. culmorum from the posterior distribution, remove # to view differences in sampling iteration
m.1.sim <- sim(m.1,n=1000)
#m.1.sim <- sim(m.1.temp,n=1000)
#m.1.sim <- sim(m.1.seas,n=1000)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
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
  points(x=m[1:100,1:9],y=m.1.sim[1:100,fs[n]:(fs[n]+8)],pch=16,col=col.alpha("grey60",alpha=0.1))
  points(af$culmorum[fs[n]:(fs[n]+8)],pch=3,col=col.alpha("black",alpha=1),cex=1.5)
  mtext(ns[n],line=-1.5,side=3)
}
mtext("Quadrat number", side = 1, outer = TRUE, line = 2)
mtext(expression("Counts of "~italic("F. culmorum")), side = 2, outer = TRUE, line = 2)
#Section 12: Creating Figure 4----
#dataframe with marginal field offsets
eq.1.f <- data.frame(post.1$a_field)
par(mfrow=c(2,2))
plot(0,type="n",xlim=c(0,10),ylim=c(-4,4),xlab="Field",ylab="Log odds",axes=FALSE,main="A")
box()
axis(1,at=seq(1,9,1),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=1,y=eq.1.f[n,1],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=2,y=eq.1.f[n,2],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=3,y=eq.1.f[n,3],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=4,y=eq.1.f[n,4],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=5,y=eq.1.f[n,5],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=6,y=eq.1.f[n,6],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=7,y=eq.1.f[n,7],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=8,y=eq.1.f[n,8],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=9,y=eq.1.f[n,9],pch=16,col=col.alpha("grey60",alpha=0.1))
}
abline(h=0)
plot(0,type="n",xlim=c(350,750),ylim=c(-4,4),xlab="Annual precipitation (mm)",ylab="Log odds",axes=FALSE,main="B")
box()
axis(1,at=seq(350,750,50),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=annual.precip[1],y=eq.1.f[n,1],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[2],y=eq.1.f[n,2],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[3],y=eq.1.f[n,3],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[4],y=eq.1.f[n,4],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[5],y=eq.1.f[n,5],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[6],y=eq.1.f[n,6],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[7],y=eq.1.f[n,7],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[8],y=eq.1.f[n,8],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.precip[9],y=eq.1.f[n,9],pch=16,col=col.alpha("grey60",alpha=0.1))
}
abline(h=0)
plot(0,type="n",xlim=c(950,1150),ylim=c(-4,4),xlab="Annual evapotranspiration (mm)",ylab="Log odds",axes=FALSE,main="C")
box()
axis(1,at=seq(950,1150,50),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=annual.evap[1],y=eq.1.f[n,1],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[2],y=eq.1.f[n,2],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[3],y=eq.1.f[n,3],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[4],y=eq.1.f[n,4],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[5],y=eq.1.f[n,5],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[6],y=eq.1.f[n,6],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[7],y=eq.1.f[n,7],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[8],y=eq.1.f[n,8],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.evap[9],y=eq.1.f[n,9],pch=16,col=col.alpha("grey60",alpha=0.1))
}
abline(h=0)
plot(0,type="n",xlim=c(3300,3800),ylim=c(-4,4),xlab="Annual growing degree-days (GDD)",ylab="Log odds",axes=FALSE,main="D")
box()
axis(1,at=seq(3300,3800,100),las=1)
axis(2,at=seq(-4,4,1),las=1)
for(n in 1:1000){
  points(x=annual.gdd[1],y=eq.1.f[n,1],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[2],y=eq.1.f[n,2],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[3],y=eq.1.f[n,3],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[4],y=eq.1.f[n,4],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[5],y=eq.1.f[n,5],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[6],y=eq.1.f[n,6],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[7],y=eq.1.f[n,7],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[8],y=eq.1.f[n,8],pch=16,col=col.alpha("grey60",alpha=0.1))
}
for(n in 1:1000){
  points(x=annual.gdd[9],y=eq.1.f[n,9],pch=16,col=col.alpha("grey60",alpha=0.1))
}
abline(h=0)
#Section 13: Creating Supplemental Figure 3----
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
#Section 14: All model combinarions with climate terms using Eq. [2] through Eq. [5]----
#linear P only
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  precip=af$precip.seq.s)
m.l.p <- map2stan(
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
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.p,depth=2,prob=0.95)
#linear E only
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  evap=af$evap.seq.s)
m.l.e <- map2stan(
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
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.e,depth=2,prob=0.95)
#linear G only
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  gdd=af$gdd.seq.s)
m.l.g <- map2stan(
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
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.g,depth=2,prob=0.95)
#linear P and E
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  precip=af$precip.seq.s,
                  evap=af$evap.seq.s)
m.l.p.e <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip + c*evap,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.p.e,depth=2,prob=0.95)
#linear P and E with interaction
m.l.i.p.e <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip + c*evap + d*precip*evap,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c,d)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.i.p.e,depth=2,prob=0.95)
#linear P and G
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  precip=af$precip.seq.s,
                  gdd=af$gdd.seq.s)
m.l.p.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip + c*gdd,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.p.g,depth=2,prob=0.95)
#linear P and G with interaction
m.l.i.p.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip + c*gdd + d*precip*gdd,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c,d)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.i.p.g,depth=2,prob=0.95)
#linear E and G
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  evap=af$evap.seq.s,
                  gdd=af$gdd.seq.s)
m.l.e.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*evap + c*gdd,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.e.g,depth=2)
#linear E and G with interaction
m.l.i.e.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*evap + c*gdd + d*evap*gdd,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c,d)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.l.i.e.g,depth=2)
#poly P only
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  precip=af$precip.seq.s)
m.p.p <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip + c*precip^2,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.p.p,depth=2,prob=0.95)
#poly E only
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  evap=af$evap.seq.s)
m.p.e <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*evap + c*evap^2,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.p.e,depth=2,prob=0.95)
#poly G only
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  gdd=af$gdd.seq.s)
m.p.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*gdd + c*gdd^2,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.p.g,depth=2,prob=0.95)
#poly P and E
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  precip=af$precip.seq.s,
                  evap=af$evap.seq.s)
m.p.p.e <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip + c*evap + d*precip^2 + e*evap^2 + f*precip*evap,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c,d,e,f)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.p.p.e,depth=2,prob=0.95)
#poly P and G
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  precip=af$precip.seq.s,
                  gdd=af$gdd.seq.s)
m.p.p.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*precip + c*gdd + d*precip^2 + e*gdd^2 + f*precip*gdd,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c,d,e,f)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.p.p.g,depth=2,prob=0.95)
#poly E and G
test.list <- list(C=af$culmorum,
                  N=af$total,
                  field=af$field,
                  iteration=af$number,
                  quadrat=af$acre,
                  evap=af$evap.seq.s,
                  gdd=af$gdd.seq.s)
m.p.e.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + a_iteration[iteration] + a_quadrat[quadrat] + b*evap + c*gdd + d*evap^2 + e*gdd^2 + f*gdd*evap,
    a_field[field]~dnorm(0,sigma_field),
    a_iteration[iteration]~dnorm(0,sigma_iteration),
    a_quadrat[quadrat]~dnorm(0,sigma_quadrat),
    c(a,b,c,d,e,f)~dnorm(0,10),
    sigma_field~dcauchy(0,1),
    sigma_iteration~dcauchy(0,1),
    sigma_quadrat~dcauchy(0,1)
  ),
  data = test.list ,
  warmup = 1000 ,
  iter = 3500 ,
  chains = 4 ,
  cores = 4 
)
#parameter summary
precis(m.p.e.g,depth=2)
#Section 15: Australian data----

test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  precip=bf$precip.s)
m.au.p <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*precip,
    a_field[field]~dnorm(0,sigma_field),
    c(a,b)~dnorm(0,10),
    sigma_field ~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 3500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.au.p,depth=2,prob = 0.95)
test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  gdd=bf$gdd.s)
m.au.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*gdd,
    a_field[field]~dnorm(0,sigma_field),
    c(a,b)~dnorm(0,10),
    sigma_field ~ dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 3500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.au.g,depth=2,prob = 0.95)
test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  precip=bf$precip.s,
                  gdd=bf$gdd.s)
m.au.p.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*precip + c*gdd,
    a_field[field]~dnorm(0,sigma_field),
    c(a,b,c)~dnorm(0,10),
    sigma_field ~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 3500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.au.p.g,depth=2,prob = 0.95)
m.au.p.g.i <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*precip + c*gdd + d*precip*gdd,
    a_field[field]~dnorm(0,sigma_field),
    c(a,b,c,d)~dnorm(0,10),
    sigma_field ~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 3500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.au.p.g.i,depth=2,prob = 0.95)
test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  precip=bf$precip.s)
m.au.p.p <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*precip + c*precip^2,
    a_field[field]~dnorm(0,sigma_field),
    c(a,b,c)~dnorm(0,10),
    sigma_field ~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 3500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.au.p.p,depth=2,prob = 0.95)
test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  gdd=bf$gdd.s)
m.au.p.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*gdd + c*gdd^2,
    a_field[field]~dnorm(0,sigma_field),
    c(a,b,c)~dnorm(0,10),
    sigma_field ~ dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 3500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.au.p.g,depth=2,prob = 0.95)
test.list <- list(C=bf$F..cul,
                  N=bf$F..pse,
                  field=bf$field,
                  precip=bf$precip.s,
                  gdd=bf$gdd.s)
m.au.p.p.g <- map2stan(
  alist(
    C~dbinom(N,P),
    logit(P) <- a + a_field[field] + b*precip + c*gdd + d*precip^2 + e*gdd^2 + f*precip*gdd,
    a_field[field]~dnorm(0,sigma_field),
    c(a,b,c,d,e,f)~dnorm(0,10),
    sigma_field ~dcauchy(0,1)
  ),
  data = test.list,
  warmup = 1000,
  iter = 3500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99)
)
#parameter summary
precis(m.au.p.p.g,depth=2,prob = 0.95)
#Section 16: Generating model predictions----
#extracting samples from all models
#current study
post.m.l.p <- extract.samples(m.l.p,10000)
post.m.l.e <- extract.samples(m.l.e,10000)
post.m.l.g <- extract.samples(m.l.g,10000)
post.m.l.p.e <- extract.samples(m.l.p.e,10000)
post.m.l.p.g <- extract.samples(m.l.p.g,10000)
post.m.l.e.g <- extract.samples(m.l.e.g,10000)
post.m.l.i.p.e <- extract.samples(m.l.i.p.e,10000)
post.m.l.i.p.g <- extract.samples(m.l.i.p.g,10000)
post.m.l.i.e.g <- extract.samples(m.l.i.e.g,10000)
post.m.p.p <- extract.samples(m.p.p,10000)
post.m.p.e <- extract.samples(m.p.e,10000)
post.m.p.g <- extract.samples(m.p.g,10000)
post.m.p.p.e <- extract.samples(m.p.p.e,10000)
post.m.p.p.g <- extract.samples(m.p.p.g,10000)
post.m.p.e.g <- extract.samples(m.p.e.g,10000)
#Australian data
post.m.au.p <- extract.samples(m.au.p,10000)
post.m.au.g <- extract.samples(m.au.g,10000)
post.m.au.p.g <- extract.samples(m.au.p.g,10000)
post.m.au.p.g.i <- extract.samples(m.au.p.g.i,10000)
post.m.au.p.p <- extract.samples(m.au.p.p,10000)
post.m.au.p.g <- extract.samples(m.au.p.g,10000)
post.m.au.p.p.g <- extract.samples(m.au.p.p.g,10000)
#simulating from linear model
sim_field_linear <- function(n,f,q,i,a,b){
  V <- seq(-2,2,0.25)
  sim_field <- rnorm(1,0,f[n])
  sim_quadrat <- rnorm(1,0,q[n])
  sim_iteration <- rnorm(1,0,i[n])
  prediction <- logistic(a[n]+sim_field+sim_quadrat+sim_iteration + b[n]*V)
  return(prediction)
}
#simulating from polynomial model
sim_field_poly <- function(n,f,q,i,a,b,c){
  V <- seq(-2,2,0.25)
  sim_field <- rnorm(1,0,f[n])
  sim_quadrat <- rnorm(1,0,q[n])
  sim_iteration <- rnorm(1,0,i[n])
  prediction <- logistic(a[n]+sim_field+sim_quadrat+sim_iteration + b[n]*V + c[n]*V^2)
  return(prediction)
}
#simulating from linear model
au_sim_field_linear <- function(n,f,a,b){
  V <- seq(-2,2,0.25)
  sim_field <- rnorm(1,0,f[n])
  prediction <- logistic(a[n]+sim_field + b[n]*V)
  return(prediction)
}
#simulating from polynomial model
au_sim_field_poly <- function(n,f,a,b,c){
  V <- seq(-2,2,0.25)
  sim_field <- rnorm(1,0,f[n])
  prediction <- logistic(a[n]+sim_field + b[n]*V + c[n]*V^2)
  return(prediction)
}
#Section 17: Creating Figure 5----
#Part A
par(mfrow=c(2,4))
par(oma=c(4,4,2,1))
par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.p$sigma_field,
                                                          q=post.m.l.p$sigma_quadrat,
                                                          i=post.m.l.p$sigma_iteration,
                                                          a=post.m.l.p$a,
                                                          b=post.m.l.p$b),
                      col=col.alpha("black",0.1))
mtext("P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_poly(n,
                                                        f=post.m.p.p$sigma_field,
                                                        q=post.m.p.p$sigma_quadrat,
                                                        i=post.m.p.p$sigma_iteration,
                                                        a=post.m.p.p$a,
                                                        b=post.m.p.p$b,
                                                        c=post.m.p.p$c),
                      col=col.alpha("black",0.1))
mtext("P^2 + P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.p.e$sigma_field,
                                                          q=post.m.l.p.e$sigma_quadrat,
                                                          i=post.m.l.p.e$sigma_iteration,
                                                          a=post.m.l.p.e$a,
                                                          b=post.m.l.p.e$b),
                      col=col.alpha("black",0.1))
mtext("P + E",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.p.g$sigma_field,
                                                          q=post.m.l.p.g$sigma_quadrat,
                                                          i=post.m.l.p.g$sigma_iteration,
                                                          a=post.m.l.p.g$a,
                                                          b=post.m.l.p.g$b),
                      col=col.alpha("black",0.1))
mtext("P + G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$precip.seq)-2*sd(af$precip.seq),mean(af$precip.seq)+2*sd(af$precip.seq),sd(af$precip.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.i.p.e$sigma_field,
                                                          q=post.m.l.i.p.e$sigma_quadrat,
                                                          i=post.m.l.i.p.e$sigma_iteration,
                                                          a=post.m.l.i.p.e$a,
                                                          b=post.m.l.i.p.e$b),
                      col=col.alpha("black",0.1))
mtext("P + E + P*E",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$precip.seq)-2*sd(af$precip.seq),mean(af$precip.seq)+2*sd(af$precip.seq),sd(af$precip.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.i.p.g$sigma_field,
                                                          q=post.m.l.i.p.g$sigma_quadrat,
                                                          i=post.m.l.i.p.g$sigma_iteration,
                                                          a=post.m.l.i.p.g$a,
                                                          b=post.m.l.i.p.g$b),
                      col=col.alpha("black",0.1))
mtext("P + G + P*G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$precip.seq)-2*sd(af$precip.seq),mean(af$precip.seq)+2*sd(af$precip.seq),sd(af$precip.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_poly(n,
                                                        f=post.m.p.p.e$sigma_field,
                                                        q=post.m.p.p.e$sigma_quadrat,
                                                        i=post.m.p.p.e$sigma_iteration,
                                                        a=post.m.p.p.e$a,
                                                        b=post.m.p.p.e$b,
                                                        c=post.m.p.p.e$d),
                      col=col.alpha("black",0.1))
mtext("P^2 + E^2 + P*E + P + E",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$precip.seq)-2*sd(af$precip.seq),mean(af$precip.seq)+2*sd(af$precip.seq),sd(af$precip.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_poly(n,
                                                        f=post.m.p.p.g$sigma_field,
                                                        q=post.m.p.p.g$sigma_quadrat,
                                                        i=post.m.p.p.g$sigma_iteration,
                                                        a=post.m.p.p.g$a,
                                                        b=post.m.p.p.g$b,
                                                        c=post.m.p.p.g$d),
                      col=col.alpha("black",0.1))
mtext("P^2 + G^2 + P*G + P + G",side=3,line=-1.5)
mtext("Annual precipitation (mm)", side = 1, outer = TRUE, line = 2)
mtext(expression("Counts of "~italic("F. culmorum")), side = 2, outer = TRUE, line = 2)
#Part B
par(mfrow=c(2,4))
par(oma=c(4,4,2,1))
par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.e$sigma_field,
                                                          q=post.m.l.e$sigma_quadrat,
                                                          i=post.m.l.e$sigma_iteration,
                                                          a=post.m.l.e$a,
                                                          b=post.m.l.e$b),
                      col=col.alpha("black",0.1))
mtext("E",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_poly(n,
                                                        f=post.m.p.e$sigma_field,
                                                        q=post.m.p.e$sigma_quadrat,
                                                        i=post.m.p.e$sigma_iteration,
                                                        a=post.m.p.e$a,
                                                        b=post.m.p.e$b,
                                                        c=post.m.p.e$c),
                      col=col.alpha("black",0.1))
mtext("E^2 + E",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.p.e$sigma_field,
                                                          q=post.m.l.p.e$sigma_quadrat,
                                                          i=post.m.l.p.e$sigma_iteration,
                                                          a=post.m.l.p.e$a,
                                                          b=post.m.l.p.e$c),
                      col=col.alpha("black",0.1))
mtext("E + P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=FALSE)
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.e.g$sigma_field,
                                                          q=post.m.l.e.g$sigma_quadrat,
                                                          i=post.m.l.e.g$sigma_iteration,
                                                          a=post.m.l.e.g$a,
                                                          b=post.m.l.e.g$b),
                      col=col.alpha("black",0.1))
mtext("E + G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$evap.seq)-2*sd(af$evap.seq),mean(af$evap.seq)+2*sd(af$evap.seq),sd(af$evap.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.i.p.e$sigma_field,
                                                          q=post.m.l.i.p.e$sigma_quadrat,
                                                          i=post.m.l.i.p.e$sigma_iteration,
                                                          a=post.m.l.i.p.e$a,
                                                          b=post.m.l.i.p.e$c),
                      col=col.alpha("black",0.1))
mtext("E + P + E*P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$evap.seq)-2*sd(af$evap.seq),mean(af$evap.seq)+2*sd(af$evap.seq),sd(af$evap.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_linear(n,
                                                          f=post.m.l.i.e.g$sigma_field,
                                                          q=post.m.l.i.e.g$sigma_quadrat,
                                                          i=post.m.l.i.e.g$sigma_iteration,
                                                          a=post.m.l.i.e.g$a,
                                                          b=post.m.l.i.e.g$b),
                      col=col.alpha("black",0.1))
mtext("E + G + E*G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$evap.seq)-2*sd(af$evap.seq),mean(af$evap.seq)+2*sd(af$evap.seq),sd(af$evap.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_poly(n,
                                                        f=post.m.p.p.e$sigma_field,
                                                        q=post.m.p.p.e$sigma_quadrat,
                                                        i=post.m.p.p.e$sigma_iteration,
                                                        a=post.m.p.p.e$a,
                                                        b=post.m.p.p.e$c,
                                                        c=post.m.p.p.e$e),
                      col=col.alpha("black",0.1))
mtext("E^2 + P^2 + E*P + E + P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual evapotranspiration (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(af$evap.seq)-2*sd(af$evap.seq),mean(af$evap.seq)+2*sd(af$evap.seq),sd(af$evap.seq)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=sim_field_poly(n,
                                                        f=post.m.p.p.g$sigma_field,
                                                        q=post.m.p.p.g$sigma_quadrat,
                                                        i=post.m.p.p.g$sigma_iteration,
                                                        a=post.m.p.p.g$a,
                                                        b=post.m.p.p.g$b,
                                                        c=post.m.p.p.g$c),
                      col=col.alpha("black",0.1))
mtext("E^2 + G^2 + E*G + E + G",side=3,line=-1.5)
mtext("Annual evapotranspiration (mm)", side = 1, outer = TRUE, line = 2)
mtext(expression("Counts of "~italic("F. culmorum")), side = 2, outer = TRUE, line = 2)
#Part C
par(mfrow=c(1,5))
par(oma=c(4,4,2,1))
par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$precip)-2*sd(bf$precip),mean(bf$precip)+2*sd(bf$precip),sd(bf$precip)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_linear(n,
                                                             f=post.m.au.p$sigma_field,
                                                             a=post.m.au.p$a,
                                                             b=post.m.au.p$b),
                      col=col.alpha("black",0.1))
mtext("P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$precip)-2*sd(bf$precip),mean(bf$precip)+2*sd(bf$precip),sd(bf$precip)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_poly(n,
                                                           f=post.m.au.p.p$sigma_field,
                                                           a=post.m.au.p.p$a,
                                                           b=post.m.au.p.p$b,
                                                           c=post.m.au.p.p$c),
                      col=col.alpha("black",0.1))
mtext("P^2 + P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$precip)-2*sd(bf$precip),mean(bf$precip)+2*sd(bf$precip),sd(bf$precip)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_linear(n,
                                                             f=post.m.au.p.g$sigma_field,
                                                             a=post.m.au.p.g$a,
                                                             b=post.m.au.p.g$b),
                      col=col.alpha("black",0.1))
mtext("P + G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$precip)-2*sd(bf$precip),mean(bf$precip)+2*sd(bf$precip),sd(bf$precip)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_linear(n,
                                                             f=post.m.au.p.g.i$sigma_field,
                                                             a=post.m.au.p.g.i$a,
                                                             b=post.m.au.p.g.i$b),
                      col=col.alpha("black",0.1))
mtext("P + G + P*G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$precip)-2*sd(bf$precip),mean(bf$precip)+2*sd(bf$precip),sd(bf$precip)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_poly(n,
                                                           f=post.m.au.p.p.g$sigma_field,
                                                           a=post.m.au.p.p.g$a,
                                                           b=post.m.au.p.p.g$b,
                                                           c=post.m.au.p.p.g$d),
                      col=col.alpha("black",0.1))
mtext("P^2 + G^2 + P*G + P + G",side=3,line=-1.5)
mtext("Annual precipitation (mm)", side = 1, outer = TRUE, line = 2)
mtext(expression("Counts of "~italic("F. culmorum")), side = 2, outer = TRUE, line = 2)
#Part D
par(mfrow=c(1,5))
par(oma=c(4,4,2,1))
par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual growing degree-days (GDD)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$gdd)-2*sd(bf$gdd),mean(bf$gdd)+2*sd(bf$gdd),sd(bf$gdd)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_linear(n,
                                                             f=post.m.au.g$sigma_field,
                                                             a=post.m.au.g$a,
                                                             b=post.m.au.g$b),
                      col=col.alpha("black",0.1))
mtext("G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual growing degree-days (GDD)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$gdd)-2*sd(bf$gdd),mean(bf$gdd)+2*sd(bf$gdd),sd(bf$gdd)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_poly(n,
                                                           f=post.m.au.p.g$sigma_field,
                                                           a=post.m.au.p.g$a,
                                                           b=post.m.au.p.g$b,
                                                           c=post.m.au.p.g$c),
                      col=col.alpha("black",0.1))
mtext("G^2 + G",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$gdd)-2*sd(bf$gdd),mean(bf$gdd)+2*sd(bf$gdd),sd(bf$gdd)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_linear(n,
                                                             f=post.m.au.p.g$sigma_field,
                                                             a=post.m.au.p.g$a,
                                                             b=post.m.au.p.g$c),
                      col=col.alpha("black",0.1))
mtext("G + P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual growing degree-days (GDD)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$gdd)-2*sd(bf$gdd),mean(bf$gdd)+2*sd(bf$gdd),sd(bf$gdd)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_linear(n,
                                                             f=post.m.au.p.g.i$sigma_field,
                                                             a=post.m.au.p.g.i$a,
                                                             b=post.m.au.p.g.i$c),
                      col=col.alpha("black",0.1))
mtext("G + P + G*P",side=3,line=-1.5)
plot(0,type="n",xlim=c(-2,2),ylim=c(0,0.5),xlab="Annual precipitation (mm)",ylab=expression("Proportion "~italic("F. culmorum")),main=" ",axes=FALSE)
box()
axis(1,at=seq(-2,2,1),las=1,labels=round(seq(mean(bf$gdd)-2*sd(bf$gdd),mean(bf$gdd)+2*sd(bf$gdd),sd(bf$gdd)),digits=0))
axis(2,at=seq(0,0.5,0.1),las=2,labels=FALSE)
for(n in 1:100) lines(x=seq(-2,2,0.25),y=au_sim_field_poly(n,
                                                           f=post.m.au.p.p.g$sigma_field,
                                                           a=post.m.au.p.p.g$a,
                                                           b=post.m.au.p.p.g$c,
                                                           c=post.m.au.p.p.g$e),
                      col=col.alpha("black",0.1))
mtext("G^2 + P^2 + G*P + G + P",side=3,line=-1.5)
mtext("Annual growing degree-days (GDD)", side = 1, outer = TRUE, line = 2)
mtext(expression("Counts of "~italic("F. culmorum")), side = 2, outer = TRUE, line = 2)
