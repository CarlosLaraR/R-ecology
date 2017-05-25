###############################################################################
#####*****Plant - Pollinator interactions on the roof of Spain (Teide)****#####
###############################################################################
###Author: Carlos Lara Romero March 2017
 rm(list=ls())
setwd("C:/Users/carlos/Documents/SCIENCE/PROYECTOS/IMEDEA/altitudinal_gradient/Analisis")
getwd()
dir()
library(stringr)
library(bipartite)
library(lme4)
library(reshape)
library(lsmeans)
orden<-read.table("orden&familia.txt", header=T)
load("analysis_teide.R")
save.image("analysis_teide.R")


######################### 1. Data preparation############################

f_acro<-function(df){
a.ge<- str_sub(df$genero, 1, 2)
a.sp<- str_sub(df$sp, 1, 3)
acro<-paste(a.ge,a.sp, sep="")
return(data.frame(acro))

}
###1.1 Rajada

dat.raj<-read.table("rajada.txt", header=T, sep="\t")

#Deleting records with no visits (fl_visitas == 0)
dat.raj<-dat.raj[!dat.raj$fl_visitas==0,]
acro<-f_acro(dat.raj)
count<-rep(1,nrow(dat.raj))
dat.raj<-cbind(dat.raj, acro, count)
dat.raj<-merge(dat.raj, orden, by="familia", all.y=FALSE)
dat.raj<-levels(dat.raj)

#Blanca

dat.blan<-read.table("blanca.txt", header=T, sep="\t")
dat.blan<-dat.blan[!dat.blan$fl_visitas==0,]
dat.blan<-droplevels(dat.blan)
acro<-f_acro(dat.blan)
count<-rep(1,nrow(dat.blan))
dat.blan<-cbind(dat.blan, acro, count)
dat.blan<-dat.blan[!dat.blan$acro=="",]  #removing data with no insect identification
dat.blan<-merge(dat.blan, orden, by="familia", all.y=FALSE)
dat.blan<-droplevels(dat.blan)
levels(dat.blan$genero)

#refugio

dat.refu<-read.table("refugio.txt",header=T, sep="\t")
dat.refu<-dat.refu[!dat.refu$fl_visitas==0,]
dat.refu<-droplevels(dat.refu)
acro<-f_acro(dat.refu)
count<-rep(1,nrow(dat.refu))
dat.refu<-cbind(dat.refu, acro, count)
dat.refu<-dat.refu[!dat.refu$acro=="",]  #removing data with no insect identification
dat.refu<-merge(dat.refu, orden, by="familia", all.y=FALSE)
dat.refu<-droplevels(dat.refu)
levels(dat.refu$genero)

#torre

dat.torre<-read.table("torre.txt", header=T, sep="\t")
dat.torre<-dat.torre[!dat.torre$fl_visitas==0,]
dat.torre<-droplevels(dat.torre)
acro<-f_acro(dat.torre)
count<-rep(1,nrow(dat.torre))
dat.torre<-cbind(dat.torre, acro, count)
dat.torre<-dat.torre[!dat.torre$acro=="",]  #removing data with no insect identification
dat.torre<-merge(dat.torre, orden, by="familia", all.y=FALSE)
dat.torre<-droplevels(dat.torre)
levels(dat.torre$genero)

#All populations combined in one data frame

dat.tot<-rbind(dat.raj, dat.blan, dat.refu, dat.torre)
 dat.tot$n_espigas[is.na(dat.tot$n_espigas)] <- 0   #Set NA to 0
 dat.tot$n_flores[is.na(dat.tot$n_flores)] <- 0     #Set NA to 0
 fl_tot<- ifelse(dat.tot$n_flores ==0,   dat.tot$n_espigas,dat.tot$n_flores)
 dat.tot<-cbind(dat.tot, fl_tot)
  fl_visitas_w<- dat.tot$fl_visitas/dat.tot$fl_tot
dat.tot<-cbind(dat.tot, fl_visitas_w)

#List of animals

list.animals<-data.frame(dat.tot[!duplicated(dat.tot$acro),c(16,1,11,12,14)]  )
list.animals<-write.table(list.animals, "list.animals.txt")
spps.plants<- dat.tot[!duplicated(dat.tot$planta),7]
spps.animals<-list.animals$acro

######################### 2. Descriptive statistics############################

###2.1 Rajada

visitas.raj<-aggregate(count~orden*acro, dat.raj, sum)
count2<-rep(1, nrow(visitas.raj))
visitas.raj<-cbind(visitas.raj, count2)
riqueza.raj<-aggregate(count2~orden, visitas.raj, sum)   
riqueza.raj.tot<-sum(riqueza.raj$count2)   #68 spps
riqueza.raj.family<-aggregate(count~ orden*familia, dat.raj, sum)
visitas.raj.tot<-sum(visitas.raj$count)   #1011
visitas.raj.tot.order<-aggregate(count~orden, visitas.raj, sum)
int.raj<-data.frame(paste(dat.raj$planta,dat.raj$acro, sep="") )
length(int.raj[!duplicated(int.raj), ]   ) #163 

###2.2 Blanca

visitas.blan<-aggregate(count~orden*acro, dat.blan, sum)
count2<-rep(1, nrow(visitas.blan))
visitas.blan<-cbind(visitas.blan, count2)
riqueza.blan<-aggregate(count2~orden, visitas.blan, sum) 
riqueza.blan.tot<-sum(riqueza.blan$count2) #96 spps
visitas.blan.tot<-sum(visitas.blan$count)  #1650 
visitas.blan.tot.order<-aggregate(count~orden, visitas.blan, sum)
int.blan<-data.frame(paste(dat.blan$planta,dat.blan$acro, sep="") )
length(int.blan[!duplicated(int.blan), ]   ) #236
###2.3 Refugio

visitas.refu<-aggregate(count~orden*acro, dat.refu, sum)
count2<-rep(1, nrow(visitas.refu))
visitas.refu<-cbind(visitas.refu, count2)   
riqueza.refu<-aggregate(count2~orden, visitas.refu, sum)   
riqueza.refu.tot<-sum(riqueza.refu$count2)        #50 spps
visitas.refu.tot<-sum(visitas.refu$count)     #422
visitas.refu.tot.order<-aggregate(count~orden, visitas.refu, sum) 
int.refu<-data.frame(paste(dat.refu$planta,dat.refu$acro, sep="") )
length(int.refu[!duplicated(int.refu), ]   ) #82 

###2.4 Torre

visitas.torre<-aggregate(count~orden*acro, dat.torre, sum)
count2<-rep(1, nrow(visitas.torre))
visitas.torre<-cbind(visitas.torre, count2)
riqueza.torre<-aggregate(count2~orden, visitas.torre, sum) 
riqueza.torre.tot<-sum(riqueza.torre$count2)    #17spps
visitas.torre.tot<-sum(visitas.torre$count)    #45 visits
visitas.torre.tot.order<-aggregate(count~orden, visitas.torre, sum) 

int.torre<-data.frame(paste(dat.torre$planta,dat.torre$acro, sep="") )
length(int.torre[!duplicated(int.torre), ]    )       #17 interacciones

### 2.5 Richness per order

visitas.teide<-read.table("visitas_orden_teide.txt", header=T) #data from "visitas.*pop.tot.order"
riqueza.teide<-read.table("riqueza_orden_teide.txt", header=T)  #data from "riqueza.*pop"

par(mfcol=c(2,2), mar=c(4,4.5,2,1))
b<-barplot(riqueza.teide$rajada, col=c(2,3,7,4,8,5,6), main="2400 m", ylab="richness (n)", ylim=c(0,30))
legend(5.5,30, inset=0.03,levels(riqueza.teide$orden),fill=c(2,3,7,4,8,5,6), bty="n", trace=TRUE, cex=0.8)
barplot(riqueza.teide$refu, col=c(2,3,7,4,8,5,6), main="3200 m",xlab="Insect order", ylab="richness (n)")
barplot(riqueza.teide$blanca, col=c(2,3,7,4,8,5,6), main="2700 m", ylab="richness (n)")
barplot(riqueza.teide$torre, col=c(2,3,7,4,8,5,6), main="3500 m",xlab="Insect order", ylab="richness (n)")

colnames(riqueza.teide)<-c("orden","2400", "2700","3200","3500")
colnames(visitas.teide)<-c("orden","2400", "2700","3200","3500")

par(mfrow=c(1,2), mar=c(2.5,3.2,0.8,0.2))
b2<-barplot(as.matrix(riqueza.teide[1:5,2:5]),col=gray.colors(5, 0,0.9),ylab="Flower-visitor richness (n)", ylim=c(0,100), xlab="Elevation (m)",cex.axis=0.8, cex.lab=0.8, cex.names=0.8, border=NA) 
#legend(3,100, inset=0.03,legend=c("Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera"),fill=gray.colors(5, 0,1), bty="n", trace=TRUE, cex=0.7)

b2<-barplot(as.matrix(visitas.teide[1:5,2:5]),col=gray.colors(5, 0,0.9),ylab="Number of visits(n)", ylim=c(0,2000), xlab="Elevation (m)",cex.axis=0.8, cex.lab=0.8, cex.names=0.8, border=NA) 
legend(2.3,2000, inset=0.03,legend=c("Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera"),fill=gray.colors(5, 0,0.9), bty="n", trace=TRUE, cex=0.7)
 
### 2.5 Richness per family & order
riqueza.family<-aggregate(count~ orden*familia*localidad, dat.tot, sum )
count1<-rep(1, nrow(riqueza.family))
riqueza.family<-cbind(riqueza.family,count1)
riqueza.family<-aggregate(count1~ orden*localidad, riqueza.family, sum )
riqueza.family<-riqueza.family[order(riqueza.family$orden),]
elevation<-ifelse(riqueza.family$localidad=="rajada",2400,ifelse(riqueza.family$localidad=="blanca", 2700,ifelse(riqueza.family$localidad=="Refugio", 3200,3500)))
riqueza.family=cbind(riqueza.family,elevation)

par(mfrow=c(2,2), mar=c(3,2.5,0.5,0.5))
barplot(riqueza.family[1:4,3],  ylim=c(0,20),col=gray.colors(4, 0.3,0.9),ylab="Flower-visitor richness (n)", xlab="Elevation (m)",cex.axis=0.8, cex.lab=0.8, cex.names=0.8,cex.main=0.8,   main="Coleoptera", border=NA)
barplot(riqueza.family[5:8,3],  ylim=c(0,20),  col=gray.colors(4, 0.3,0.9), xlab="Elevation (m)",cex.axis=0.8, cex.lab=0.8, cex.names=0.8,cex.main=0.8,   main="Diptera", border=NA)
barplot(riqueza.family[13:16,3],  ylim=c(0,20),  col=gray.colors(4, 0.3,0.9),ylab="Flower-visitor richness (n)", xlab="Elevation (m)",cex.axis=0.8, cex.lab=0.8, cex.names=0.8,  names.arg=riqueza.family[1:4,4],cex.main=0.8, main="Himenoptera", border=NA)
barplot(riqueza.family[17:20,3],  ylim=c(0,20),  col=gray.colors(4, 0.3,0.9), xlab="Elevation (m)",cex.axis=0.8, cex.lab=0.8, cex.names=0.8,  names.arg=riqueza.family[1:4,4],cex.main=0.8, main="Lepidoptera", border=NA)


######################### 3. Bipartite analysis ############################

###########3.1 Construction of flower-visitation networks

###3.1.1 Binary networks 

web.tot<-(table(dat.tot$planta,dat.tot$orden)>0)*1    #bipartite plant-insect
web.tot.order<-(table(dat.tot$planta,dat.tot$orden)>0)*1  #bipartite plant-insect order
plotweb(web.tot )  #ybig=1.6, text.rot=90, high.spacing=0.02, low.spacing=0.09, labsize=1.10
plotweb(web.tot.order )    f
  
###3.1.2 Quantitative networks using as a link weight the method "M3" proposed by Castro-Urgal et al 2012 Basic App Ecol 

#M1: no. of visits
fl_visitas_w1<- aggregate(fl_visitas~planta*acro,dat.tot,sum)
library(reshape)
web.tot.w1<-cast(fl_visitas_w1, formula=planta~acro,  sum)  #Cast a molten data frame into the reshaped or aggregated form you want
web.tot.w1<-data.frame(web.tot.w1[,-1])
rownames(web.tot.w1)<-levels(dat.tot$planta)

#M3: no. of visits per flower/time
#duration of all observation was 15 minutes. Then we did not weighted the matrix by time

f_web<-function(df){
fl_visitas_w3<- aggregate(fl_visitas_w~planta*acro,df,sum)
web.tot.w3<-cast(fl_visitas_w3, formula=planta~acro,  sum)  #Cast a molten data frame into the reshaped or aggregated form you want
web.tot.w3<-data.frame(web.tot.w3[,-1])
 rownames(web.tot.w3)<-levels(df$planta)
 return(web.tot.w3)
 }

web.tot.w3<-f_web(dat.tot)

par(mfrow=c(1,1), mar=c(4,4,1,2))
plotweb(web.tot.w1)  
plotweb(web.tot.w3) 

#Rajada
dat.raj<-dat.tot[dat.tot$localidad=="rajada",]  
dat.raj<-droplevels(dat.raj)
web.raj<-f_web(dat.raj)
plotweb(web.raj)

#Blanca
dat.blan<-dat.tot[dat.tot$localidad=="blanca",]  
dat.blan<-droplevels(dat.blan)
web.blan<-f_web(dat.blan)
plotweb(web.blan)

#Refugio
dat.refu<-dat.tot[dat.tot$localidad=="Refugio",]  
dat.refu<-droplevels(dat.refu)
web.refu<-f_web(dat.refu)
plotweb(web.refu)

#Torre 4
dat.torre<-dat.tot[dat.tot$localidad=="Torre_4",]  
dat.torre<-droplevels(dat.torre)
web.torre<-f_web(dat.torre)
plotweb(web.torre,high.spacing=0.02)

##############3.2 Estimation of network metrics

###3.2.1 Networklevel metrics

networklevel.raj<-networklevel(web.raj)
networklevel.blan<-networklevel(web.blan)
networklevel.refu<-networklevel(web.refu)
networklevel.torre<-networklevel(web.torre)


#Z-score H2

nulls.raj1 <- nullmodel(web.raj, N=100, method="r2d")
h2.raj.random <- sapply(nulls.raj1, H2fun)
h2.raj.m<- mean(h2.raj.random[1,])
h2.raj.sd<- sd(h2.raj.random[1,])
h2.raj<-H2fun(web.raj, H2_integer=FALSE)

nulls.blan1<- nullmodel(web.blan,  N=100, method="r2d")
h2.blan.random <- sapply(nulls.blan1, H2fun)
h2.blan.m<- mean(h2.blan.random[1,])
h2.blan.sd<- sd(h2.blan.random[1,])
h2.blan<-H2fun(web.blan, H2_integer=FALSE)
         
nulls.refu1 <- nullmodel(web.refu, N=100, method="r2d")
h2.refu.random <- sapply(nulls.refu1, H2fun)
h2.refu.m<- mean(h2.refu.random[1,])
h2.refu.sd<- sd(h2.refu.random[1,])
h2.refu<-H2fun(web.refu, H2_integer=FALSE)


(h2.raj[1]-h2.raj.m)/h2.raj.sd
 (h2.blan[1]-h2.blan.m)/h2.blan.sd
(h2.refu[1]-h2.refu.m)/h2.refu.sd
 
nm<- vegan::nullmodel(matrix, method="r2")  #vegan
sm<-simulate(nm, nsim=1)

###3.2.1 Spcies level metrics

###Bipartite
specieslevel.raj<-specieslevel(web.raj)
specieslevel.blan<-specieslevel(web.blan)
specieslevel.refu<-specieslevel(web.refu)
specieslevel.torre<-specieslevel(web.torre)   #Error

###Core-periphery analysis

#Rajada

web.raj0<-(table(dat.raj$planta,dat.raj$acro)>0)*1
ki.raj.a<-apply(web.raj0,2,sum)
kmean.raj.a<-mean(ki.raj.a)
ksd.raj.a<-sd(ki.raj.a)
k.raj.a<- (ki.raj.a-kmean.raj.a)/ksd.raj.a
which(k.raj.a>1)

ki.raj.p<-apply(web.raj0,1,sum)
kmean.raj.p<-mean(ki.raj.p)
ksd.raj.p<-sd(ki.raj.p)
k.raj.p<- (ki.raj.p-kmean.raj.p)/ksd.raj.p
which(k.raj.p>1)

#Blanca
web.blan0<-(table(dat.blan$planta,dat.blan$acro)>0)*1 
ki.blan.a<-apply(web.blan0,2,sum)
kmean.blan.a<-mean(ki.blan.a)
ksd.blan.a<-sd(ki.blan.a)
k.blan.a<- (ki.blan.a-kmean.blan.a)/ksd.blan.a
which(k.blan.a>1)

ki.blan.p<-apply(web.blan0,1,sum)
kmean.blan.p<-mean(ki.blan.p)
ksd.blan.p<-sd(ki.blan.p)
k.blan.p<- (ki.blan.p-kmean.blan.p)/ksd.blan.p
which(k.blan.p>1)

#Refugio 
web.refu0<-(table(dat.refu$planta,dat.refu$acro)>0)*1 
ki.refu.a<-apply(web.refu0,2,sum)
kmean.refu.a<-mean(ki.refu.a)
ksd.refu.a<-sd(ki.refu.a)
k.refu.a<- (ki.refu.a-kmean.refu.a)/ksd.refu.a
which(k.refu.a>1)

ki.refu.p<-apply(web.refu0,1,sum)
kmean.refu.p<-mean(ki.refu.p)
ksd.refu.p<-sd(ki.refu.p)
k.refu.p<- (ki.refu.p-kmean.refu.p)/ksd.refu.p
which(k.refu.p>1)

#torre
web.torre0<-(table(dat.torre$planta,dat.torre$acro)>0)*1 
ki.torre.a<-apply(web.torre0,2,sum)
kmean.torre.a<-mean(ki.torre.a)
ksd.torre.a<-sd(ki.torre.a)
k.torre.a<- (ki.torre.a-kmean.torre.a)/ksd.torre.a
which(k.torre.a>1)

ki.torre.p<-apply(web.torre0,1,sum)
kmean.torre.p<-mean(ki.torre.p)
ksd.torre.p<-sd(ki.torre.p)
k.torre.p<- (ki.torre.p-kmean.torre.p)/ksd.torre.p
which(k.torre.p>1)

f_kdf<-function(algo.a, algo.b, elev){
ka<-data.frame(algo.a)
colnames(ka)<-"k"
spp<-rownames(ka)
kspa<-cbind(ka,spp)
kb<-data.frame(algo.b)
colnames(kb)<-"k"
spp<-rownames(kb)
kspb<-cbind(kb,spp)
kdf<-rbind(kspa,kspb)
elevation<-rep(elev,   nrow(kdf))
kdf<-cbind(kdf,elevation)
return(kdf)
}

ki.raj<-f_kdf(k.raj.a, k.raj.p, "2400")
ki.blan<-f_kdf(k.blan.a, k.blan.p, "2700")
ki.refu<-f_kdf(k.refu.a, k.refu.p, "3200")
k<-rbind(ki.raj,ki.blan,ki.refu)

##Mean and SE for each population and trophic level

high.raj<-apply(specieslevel.raj[[1]], 2,mean)
low.raj<-apply(specieslevel.raj[[2]], 2,mean)

high.blan<-apply(specieslevel.blan[[1]], 2,mean)
low.blan<-apply(specieslevel.blan[[2]], 2,mean)

high.refu<-apply(specieslevel.refu[[1]], 2,mean)
low.refu<-apply(specieslevel.refu[[2]], 2,mean)

e.high.raj<-apply(specieslevel.raj[[1]], 2, function (x) sd(x)/sqrt(length(x)))
e.low.raj<-apply(specieslevel.raj[[2]], 2,function (x) sd(x)/sqrt(length(x)))

e.high.blan<-apply(specieslevel.blan[[1]], 2,function (x) sd(x)/sqrt(length(x)))
e.low.blan<-apply(specieslevel.blan[[2]], 2,function (x) sd(x)/sqrt(length(x)))

e.high.refu<-apply(specieslevel.refu[[1]], 2,function (x) sd(x)/sqrt(length(x)))
e.low.refu<-apply(specieslevel.refu[[2]], 2,function (x) sd(x)/sqrt(length(x)))

##Metrics for each species and population

#df.animals
raj<-rep("2400",  nrow(specieslevel.raj[[1]])    )
blan<-rep("2700",  nrow(specieslevel.blan[[1]])    )
refu<-rep("3200",  nrow(specieslevel.refu[[1]])    )
elevation<-c(raj,blan,refu)
df.animals<-rbind(specieslevel.raj[[1]],specieslevel.blan[[1]],specieslevel.refu[[1]])
df.animals<-cbind(df.animals,elevation)
spp<- c(rownames(specieslevel.raj[[1]]),rownames(specieslevel.blan[[1]]),rownames(specieslevel.refu[[1]]))
df.animals<-cbind(data.frame(spp),df.animals)
write.table(df.animals, "specieslevel_animals.txt", row.names=F) 

#df.plants
raj<-rep("2400",  nrow(specieslevel.raj[[2]])    )
blan<-rep("2700",  nrow(specieslevel.blan[[2]])    )
refu<-rep("3200",  nrow(specieslevel.refu[[2]])    )
elevation<-c(raj,blan,refu)
df.plants<-rbind(specieslevel.raj[[2]],specieslevel.blan[[2]],specieslevel.refu[[2]])
df.plants<-cbind(df.plants,elevation)
spp<- c(rownames(specieslevel.raj[[2]]),rownames(specieslevel.blan[[2]]),rownames(specieslevel.refu[[2]]))
df.plants<-cbind(data.frame(spp),df.plants)
write.table(df.plants, "specieslevel_plants.txt", row.names=F) 

######################### 4. Betapart analysis ############################

library(vegan) #Nullmodels
library(betapart)
 
 ###########4.1 Observed beta diveristy

C<-(table(dat.tot$localidad,dat.tot$acro)>0)*1
core<-betapart.core(C)
(jac<-beta.multi(core, index.family="jac"))
(sor<-beta.multi(core, index.family="sor"))

dat12<- subset(dat.tot, localidad=="blanca" |  localidad=="rajada")
dat12<-droplevels(dat12)
C12 <-(table(dat12$localidad,dat12$acro)>0)*1
core12<-betapart.core(C12)
(sor12<-beta.multi(core12, index.family="sor"))

dat13<- subset(dat.tot, localidad=="blanca" |  localidad=="Refugio")
dat13<-droplevels(dat13)
C13 <-(table(dat13$localidad,dat13$acro)>0)*1
core13<-betapart.core(C13)
(sor13<-beta.multi(core13, index.family="sor"))

dat14<- subset(dat.tot, localidad=="blanca" |  localidad=="Torre_4")
dat14<-droplevels(dat14)
C14 <-(table(dat14$localidad,dat14$acro)>0)*1
core14<-betapart.core(C14)
(sor14<-beta.multi(core14, index.family="sor"))

dat23<- subset(dat.tot, localidad=="rajada" |  localidad=="Refugio")
dat23<-droplevels(dat23)
C23 <-(table(dat23$localidad,dat23$acro)>0)*1
core23<-betapart.core(C23)
(sor23<-beta.multi(core23, index.family="sor"))

dat24<- subset(dat.tot, localidad=="rajada" |  localidad=="Torre_4")
dat24<-droplevels(dat24)
C24 <-(table(dat24$localidad,dat24$acro)>0)*1
core24<-betapart.core(C24)
(sor24<-beta.multi(core24, index.family="sor"))

dat34<- subset(dat.tot, localidad=="Refugio" |  localidad=="Torre_4")
dat34<-droplevels(dat34)
C34 <-(table(dat34$localidad,dat34$acro)>0)*1
core34<-betapart.core(C34)
(sor34<-beta.multi(core34, index.family="sor"))

###########4.2 Community data matrix randomization

nruns=1000

f_random=function(matrix){
nm<- vegan::nullmodel(matrix, method="r2")  #vegan
sm<-simulate(nm, nsim=1)
sm.core=betapart.core(sm[,,1])
sm.multi=beta.multi(sm.core, index.family="sor")
return(sm.multi)
}


#Expected

R.animals<- t(replicate(nruns,f_random(C)))
sor.mean<- mean(as.numeric(R.animals[,3]))
sor.sd<- sd(as.numeric(R.animals[,3]))
sim.mean<- mean(as.numeric(R.animals[,1]))
sim.sd<- sd(as.numeric(R.animals[,1]))
sne.mean<- mean(as.numeric(R.animals[,2]))
sne.sd<- sd(as.numeric(R.animals[,2]))

###########4.3 Z-Score

(sor$beta.SIM-sim.mean)/sim.sd
(sor$beta.SNE-sne.mean)/sne.sd
(sor$beta.SOR-sor.mean)/sor.sd

                                     
######################### 5. Betalink analysis ############################

setwd("C:/Users/carlos/Documents/SCIENCE/PROYECTOS/IMEDEA/altitudinal_gradient/Analisis/betalink")

###Loading functions

#library(betalink)

source("aggregate.metaweb.r")
source("beta.os_prime.r")
source("betalink.r")
source("betalink.dist.r")
source("betalink.plot.r")
source("extract.localwebs.r")
source("generate.metaweb.r")
source("measures.r")
source("null.connectance.r")
source("null.degrees.r")
source("null.from_template.r")
source("null.metaweb.r")
source ("betalink.plot_Cuentame_modelo.r")

########### 5.1Data analysis

realizations = list(web.raj0, web.blan0, web.refu0, web.torre0)
meta<- aggregate.metaweb(realizations)  #The object returned is a list with three elements, web, template, and cooc. All three objects are matrices, which correspond to different informations about the metaweb. 

#Betalink()
				#U  (betadiv de especies del nivel trófico superior)     	
				#L  (betadiv de especies del nivel trófico inferior)
				#S  (betadiv de especies totales)
				#OS (betadiv de interacciones debida a las parejas de especies compartidas)
				#WN (betadiv de interacciones)
				#ST (betadiv de interacciones debida a las parejas de especies no compartidas)
				#contrib (Proporción de WN debida

beta12<-betalink(web.raj0 , web.blan0, bf=B01)
beta13<-betalink(web.raj0 , web.refu0, bf=B01)  
beta14<-betalink(web.raj0 , web.torre0, bf=B01)
beta23<-betalink(web.blan0 , web.refu0, bf=B01)  
beta24<-betalink(web.blan0 , web.torre0, bf=B01) 
beta34<-betalink(web.refu0 , web.torre0, bf=B01) 

###Plotting rsults

#pdf("betalink.pdf")
par(mfcol=c(1,6), mar=c(2,0,2,0))
beta12.plot<-betalink.plot_Cuentame_modelo(web.raj0 , web.blan0)
beta13.plot<-betalink.plot_Cuentame_modelo(web.raj0 , web.refu0)  
beta14.plot<-betalink.plot_Cuentame_modelo(web.raj0 , web.torre0)
beta23.plot<-betalink.plot_Cuentame_modelo(web.blan0 , web.refu0)  
beta24.plot<-betalink.plot_Cuentame_modelo(web.blan0 , web.torre0) 
beta34.plot<-betalink.plot_Cuentame_modelo(web.refu0 , web.torre0) 

#dev.off()

beta12.s=beta12.plot$negros
beta12.nf=sum(beta12.plot$azules,beta12.plot$naranjas)
beta12.nt=sum(beta12.plot$verdes,beta12.plot$rojos)

beta13.s=beta13.plot$negros
beta13.nf=sum(beta13.plot$azules,beta13.plot$naranjas)
beta13.nt=sum(beta13.plot$verdes,beta13.plot$rojos)

beta14.s=beta14.plot$negros
beta14.nf=sum(beta14.plot$azules,beta14.plot$naranjas)
beta14.nt=sum(beta14.plot$verdes,beta14.plot$rojos)

beta23.s=beta23.plot$negros
beta23.nf=sum(beta23.plot$azules,beta23.plot$naranjas)
beta23.nt=sum(beta23.plot$verdes,beta23.plot$rojos)

beta24.s=beta24.plot$negros
beta24.nf=sum(beta24.plot$azules,beta24.plot$naranjas)
beta24.nt=sum(beta24.plot$verdes,beta24.plot$rojos)

beta34.s=beta34.plot$negros
beta34.nf=sum(beta34.plot$azules,beta34.plot$naranjas)
beta34.nt=sum(beta34.plot$verdes,beta34.plot$rojos)

matrix.beta=matrix(c(beta12.s,beta12.nf,beta12.nt, beta13.s, beta13.nf, beta13.nt,beta14.s,beta14.nf,beta14.nt,beta23.s,beta23.nf,beta23.nt,beta24.s,beta24.nf,beta24.nt,beta34.s,beta34.nf,beta34.nt), ncol=6)
 colnames(matrix.beta)<-c("1-2","1-3","1-4","2-3","2-4","3-4")
rownames(matrix.beta)<-c("Shared","Non-shared turnover", "Non-shares prefererence")
par(mfrow=c(1,1), mar=c(4,4,0.8,0.1)) #bottom, left, top, and right 
bp<-barplot(matrix.beta[,c(1,2,4)],col= gray.colors(3), beside=T,xaxt = "n", xlab="Elevation (m) of the two study sites compared", ylim=c(0,200), ylab="Interactions (N)") 
legend(0.5,200, inset=0.03,c("Shared","Non-shared due to species turnover","Non-shared due to changes in species preferences"),
 fill=gray.colors(3), bty="n", trace=TRUE, cex=0.7)
 axis(1, at=c(2.5,6.5,10.5),line=0.2,lty=1, cex=0.6,col="white",labels=c("2400-2700","2400-3200","2700-3200") )

text(x=2.5, y=130, "Bwn=0.57, p<0.05", cex=0.8)
text(x=6.5, y=148, "Bwn=0.68, p<0.05", cex=0.8)
text(x=10.5, y=188, "Bwn=0.69, p<0.05", cex=0.8)

#With all comparisons

bp<-barplot(matrix.beta,col= gray.colors(3), beside=T,xaxt = "n", xlab="Elevation (m) of the two study sites compared", ylim=c(0,250), ylab="Interactions (N)") 
legend(0.5,250, inset=0.03,c("Shared","Non-shared due to species turnover","Non-shared due to changes in species preferences"),
 fill=gray.colors(3), bty="n", trace=TRUE, cex=0.7)
   axis(1, at=c(2.5,6.5,10.5,14.5,18.5,22.5),line=0.2,lty=1, cex=0.6,col="white",labels=c("2400-2700","2400-3200","2400-3500","2700-3200","2700-3500","3200-3500") )
   
   
###########5.2 Community data randomization

f_random2=function(m1, m2, list.plants, list.animals){
rownames(m1) <- sample(list.plants,nrow(m1))
colnames(m1) <- sample(list.animals,ncol(m1))
rownames(m2) <- sample(list.plants,nrow(m2))
colnames(m2) <- sample(list.animals,ncol(m2))
nm1 <- vegan::nullmodel(m1,method="r2") #vegan
sm1 <- simulate(nm1,nsim=1) #vegan
nm2 <- vegan::nullmodel(m2,method="r2") #vegan
sm2 <- simulate(nm2,nsim=1) #vegan
betalink<-betalink.plot_Cuentame_modelo(sm1[,,1],sm2[,,1])
betalink2<-betalink(sm1[,,1],sm2[,,1])
betalink3=rbind(betalink$negros,sum(betalink$azules,betalink$naranjas),sum(betalink$verdes,betalink$rojos),betalink2[5])
return(betalink3)
}


f_random3=function(m1, m2, list.plants, list.animals){
rownames(m1) <- sample(list.plants,nrow(m1))
colnames(m1) <- sample(list.animals,ncol(m1))
nm1 <- vegan::nullmodel(m1,method="r2") #vegan
sm1 <- simulate(nm1,nsim=1) #vegan
betalink<-betalink.plot_Cuentame_modelo(sm1[,,1],m2)
betalink2<-betalink(sm1[,,1],m2)
betalink3=rbind(betalink$negros,sum(betalink$azules,betalink$naranjas),sum(betalink$verdes,betalink$rojos),betalink2[5])
return(betalink3)
}

#Expected              
nruns=1000
R.beta12<- (replicate(nruns,f_random2(m1=web.raj0,m2=web.blan0, list.plants=spps.plants, list.animals=spps.animals)))
R.beta12.s.mean<- mean(as.numeric(R.beta12[1,,]))
R.beta12.nf.mean<- mean(as.numeric(R.beta12[2,,]))
R.beta12.nt.mean<- mean(as.numeric(R.beta12[3,,]))
 R.beta12.wn.mean<- mean(as.numeric(R.beta12[4,,]))
R.beta12.s.sd<- sd(as.numeric(R.beta12[1,,]))
R.beta12.nf.sd<- sd(as.numeric(R.beta12[2,,]))
R.beta12.nt.sd<- sd(as.numeric(R.beta12[3,,]))
R.beta12.wn.sd<- sd(as.numeric(R.beta12[4,,]))
(beta12.s-R.beta12.s.mean)/R.beta12.s.sd
(beta12.nf-R.beta12.nf.mean)/R.beta12.nf.sd
(beta12.nt-R.beta12.nt.mean)/R.beta12.nt.sd
(beta12$WN-R.beta12.wn.mean)/R.beta12.wn.sd

R.beta13<- (replicate(nruns,f_random2(web.raj0,web.refu0, spps.plants,spps.animals)))
R.beta13.s.mean<- mean(as.numeric(R.beta13[1,,]))
R.beta13.nf.mean<- mean(as.numeric(R.beta13[2,,]))
R.beta13.nt.mean<- mean(as.numeric(R.beta13[3,,]))
 R.beta13.wn.mean<- mean(as.numeric(R.beta13[4,,]))
R.beta13.s.sd<- sd(as.numeric(R.beta13[1,,]))
R.beta13.nf.sd<- sd(as.numeric(R.beta13[2,,]))
R.beta13.nt.sd<- sd(as.numeric(R.beta13[3,,]))
 R.beta13.wn.sd<- sd(as.numeric(R.beta13[4,,]))
(beta13.s-R.beta13.s.mean)/R.beta13.s.sd
(beta13.nf-R.beta13.nf.mean)/R.beta13.nf.sd
(beta13.nt-R.beta13.nt.mean)/R.beta13.nt.sd
 (beta13$WN-R.beta13.wn.mean)/R.beta13.wn.sd

R.beta14<- (replicate(nruns,f_random3(m1=web.raj0,m2=web.torre0, list.plants=spps.plants, list.animals=spps.animals)))
R.beta14.s.mean<- mean(as.numeric(R.beta14[1,,]))
R.beta14.nf.mean<- mean(as.numeric(R.beta14[2,,]))
R.beta14.nt.mean<- mean(as.numeric(R.beta14[3,,]))
 R.beta14.wn.mean<- mean(as.numeric(R.beta14[4,,]))
R.beta14.s.sd<- sd(as.numeric(R.beta14[1,,]))
R.beta14.nf.sd<- sd(as.numeric(R.beta14[2,,]))
R.beta14.nt.sd<- sd(as.numeric(R.beta14[3,,]))
 R.beta14.wn.sd<- sd(as.numeric(R.beta14[4,,]))
(beta14.s-R.beta14.s.mean)/R.beta14.s.sd
(beta14.nf-R.beta14.nf.mean)/R.beta14.nf.sd
(beta14.nt-R.beta14.nt.mean)/R.beta14.nt.sd
 (beta14$WN-R.beta14.wn.mean)/R.beta14.wn.sd


R.beta23<- (replicate(nruns,f_random2(web.blan0,web.refu0, spps.plants,spps.animals)))
R.beta23.s.mean<- mean(as.numeric(R.beta23[1,,]))
R.beta23.nf.mean<- mean(as.numeric(R.beta23[2,,]))
R.beta23.nt.mean<- mean(as.numeric(R.beta23[3,,]))
R.beta23.wn.mean<- mean(as.numeric(R.beta23[4,,]))
R.beta23.s.sd<- sd(as.numeric(R.beta23[1,,]))
R.beta23.nf.sd<- sd(as.numeric(R.beta23[2,,]))
R.beta23.nt.sd<- sd(as.numeric(R.beta23[3,,]))
R.beta23.wn.sd<- sd(as.numeric(R.beta23[4,,]))
(beta23.s-R.beta23.s.mean)/R.beta23.s.sd
(beta23.nf-R.beta23.nf.mean)/R.beta23.nf.sd
(beta23.nt-R.beta23.nt.mean)/R.beta23.nt.sd
(beta23$WN-R.beta23.wn.mean)/R.beta23.wn.sd


R.beta24<- (replicate(nruns,f_random3(m1=web.blan0,m2=web.torre0, list.plants=spps.plants, list.animals=spps.animals)))
R.beta24.s.mean<- mean(as.numeric(R.beta24[1,,]))
R.beta24.nf.mean<- mean(as.numeric(R.beta24[2,,]))
R.beta24.nt.mean<- mean(as.numeric(R.beta24[3,,]))
 R.beta24.wn.mean<- mean(as.numeric(R.beta24[4,,]))
R.beta24.s.sd<- sd(as.numeric(R.beta24[1,,]))
R.beta24.nf.sd<- sd(as.numeric(R.beta24[2,,]))
R.beta24.nt.sd<- sd(as.numeric(R.beta24[3,,]))
 R.beta24.wn.sd<- sd(as.numeric(R.beta24[4,,]))
(beta24.s-R.beta24.s.mean)/R.beta24.s.sd
(beta24.nf-R.beta24.nf.mean)/R.beta24.nf.sd
(beta24.nt-R.beta24.nt.mean)/R.beta24.nt.sd
 (beta24$WN-R.beta24.wn.mean)/R.beta24.wn.sd

 R.beta34<- (replicate(nruns,f_random3(m1=web.refu0,m2=web.torre0, list.plants=spps.plants, list.animals=spps.animals)))
R.beta34.s.mean<- mean(as.numeric(R.beta34[1,,]))
R.beta34.nf.mean<- mean(as.numeric(R.beta34[2,,]))
R.beta34.nt.mean<- mean(as.numeric(R.beta34[3,,]))
 R.beta34.wn.mean<- mean(as.numeric(R.beta34[4,,]))
R.beta34.s.sd<- sd(as.numeric(R.beta34[1,,]))
R.beta34.nf.sd<- sd(as.numeric(R.beta34[2,,]))
R.beta34.nt.sd<- sd(as.numeric(R.beta34[3,,]))
 R.beta34.wn.sd<- sd(as.numeric(R.beta34[4,,]))
(beta34.s-R.beta34.s.mean)/R.beta34.s.sd
(beta34.nf-R.beta34.nf.mean)/R.beta34.nf.sd
(beta34.nt-R.beta34.nt.mean)/R.beta34.nt.sd
 (beta34$WN-R.beta34.wn.mean)/R.beta34.wn.sd

######################### 6. Modularity analysis ############################

#Saving webs to be loaded into a external server

write.table(web.raj, "web.raj.txt")
write.table(web.blan, "web.blan.txt")
write.table(web.refu, "web.refu.txt")
write.table(web.torre, "web.torre.txt")

#Loading functions to select the best run (i.e., maximum likelihood) and extract C, Z values

f_best_modularity<-function(mod.list,...) {
results=sapply(mod.list, function(x) x@likelihood)
print(max(results))
results.max<- which (results== max(results))
results.max<-results.max[1]
mod.best<-mod.list[[results.max]]      #En la corrida anterior era el número 20 ahora es el 34
return(mod.best)
 }

################# 6.1 Rajada population

##Linux screen -S mod.rajada
##Linux Detach ctrl+a d

web.raj<-read.table("web.raj.txt", header=T)

#6.1.1 Selection of the best model

nruns=100
mod.raj<- replicate(nruns, computeModules(web.raj, steps=1E8))
save.image(file="modularity_raj.RData")
#load(file="modularity_raj.RData")
mod.raj.best<- f_best_modularity (mod.raj)

##6.1.2 Plotting the best model
pdf("plot_modularity_raj.pdf", width=20)
plotModuleWeb (mod.raj.best)
dev.off()

##6.1.3 Extracting information of the best model

printoutModuleInformation(mod.raj.best) # Extracting species of each module
cz.raj<- f_cz(mod.raj.best)             #Extracting c & z values
cz.raj[is.na(cz.raj)] <- 0   # computeModules add a NA to each species with z = 0


###Stimating Qnull and SDnull to test signification of Qobs
nulls.raj <- nullmodel(web.raj, N=100, method="r2d")
modules.nulls.raj <- sapply(nulls.raj, computeModules)
like.nulls.raj <- sapply(modules.nulls.raj, function(x) x@likelihood)
(z <- (mod.raj.best@likelihood - mean(like.nulls.raj))/sd(like.nulls.raj))

##6.1.4 Species rol

#Observed

cz.raj.l<-data.frame(czvalues(mod.raj.best, level="lower", weighted=T) )
cz.raj.h<-data.frame(czvalues(mod.raj.best, level="higher", weighted=T) )
cz.raj.l[is.na(cz.raj.l)]<-0
cz.raj.h[is.na(cz.raj.h)]<-0

critical.c.h<-quantile(cz.raj.h$c, 0.95)
critical.c.l<-quantile(cz.raj.l$c, 0.95)
critical.z.h<-quantile(cz.raj.h$z, 0.95)
critical.z.l<-quantile(cz.raj.l$z, 0.95)

hubs.raj.l<- subset(cz.raj.l, c>=critical.c.l )
hubs.raj.h<- subset(cz.raj.h, c>=critical.c.h )

################# 6.2 Blanca population
##Linux screen -S mod.blanca
##Linux Detach ctrl+a d

web.blan<-read.table("web.blan.txt", header=T)
##6.3.1 Selection of the best model

nruns=100
mod.blan<- replicate(nruns, computeModules(web.blan, steps=1E8))
save.image(file="modularity_blan.RData")
#load(file="modularity_blan.RData")
mod.blan.best<- f_best_modularity (mod.blan)

##6.3.2 Plotting the best model
pdf("plot_modularity_blan.pdf", width=16)
 plotModuleWeb (mod.blan.best)
dev.off()

##6.3.3 Extracting information of the best model

info.mod.blan.best<-printoutModuleInformation(mod.blan.best)
printoutModuleInformation(mod.blan.best) # Extracting species of each module
cz.blan<- f_cz(mod.blan.best)             #Extracting c & z values
cz.blan[is.na(cz.blan)] <- 0   # computeModules add a NA to each species with z = 0

###Stimating Qnull and SDnull to test signification of Qobs
nulls.blan <- nullmodel(web.blan, N=100, method="r2d")
modules.nulls.blan <- sapply(nulls.blan, computeModules)
like.nulls.blan <- sapply(modules.nulls.blan, function(x) x@likelihood)
(z <- (mod.blan.best@likelihood - mean(like.nulls.blan))/sd(like.nulls.blan))

##6.2.4 Species rol

#Observed

cz.blan.l<-data.frame(czvalues(mod.blan.best, level="lower", weighted=T) )
cz.blan.h<-data.frame(czvalues(mod.blan.best, level="higher", weighted=T) )
cz.blan.l[is.na(cz.blan.l)]<-0
cz.blan.h[is.na(cz.blan.h)]<-0

  critical.c.h<-quantile(cz.blan.h$c, 0.95)
critical.c.l<-quantile(cz.blan.l$c, 0.95)
critical.z.h<-quantile(cz.blan.h$z, 0.95)
critical.z.l<-quantile(cz.blan.l$z, 0.95)

hubs.blan.l<- subset(cz.blan.l, c>=critical.c.l )
hubs.blan.h<- subset(cz.blan.h, c>=critical.c.h )

################# 6.3 Refugio population

##Linux screen -S mod.refu
##Linux Detach ctrl+a d

web.refu<-read.table("web.refu.txt", header=T)
##6.3.1 Selection of the best model

nruns=100
mod.refu<- replicate(nruns, computeModules(web.refu, steps=1E8))
save.image(file="modularity_refu.RData")
#load(file="modularity_refu.RData")
mod.refu.best<- f_best_modularity (mod.refu)

##6.3.2 Plotting the best model
pdf("plot_modularity_refu.pdf", width=12)
 plotModuleWeb (mod.refu.best)
dev.off()

##6.3.3 Extracting information of the best model

info.mod.refu.best<-printoutModuleInformation(mod.refu.best)    # Extracting species of each module
cz.refu<- f_cz(mod.refu.best)             #Extracting c & z values
cz.refu[is.na(cz.refu)] <- 0  

###Stimating Qnull and SDnull to test signification of Qobs
nulls.refu1 <- nullmodel(web.refu, N=20, method="r2d")
modules.nulls.refu1 <- sapply(nulls.refu1, computeModules)
nulls.refu2 <- nullmodel(web.refu, N=20, method="r2d")
modules.nulls.refu2 <- sapply(nulls.refu2, computeModules)
nulls.refu3 <- nullmodel(web.refu, N=20, method="r2d")
modules.nulls.refu3 <- sapply(nulls.refu3, computeModules)
nulls.refu4<- nullmodel(web.refu, N=20, method="r2d")
modules.nulls.refu4 <- sapply(nulls.refu4, computeModules)
nulls.refu5 <- nullmodel(web.refu, N=20, method="r2d")
modules.nulls.refu5 <- sapply(nulls.refu5, computeModules)

like.nulls.refu <- sapply(c(modules.nulls.refu1,modules.nulls.refu2,modules.nulls.refu3,modules.nulls.refu4,modules.nulls.refu5), function(x) x@likelihood)
(z <- (mod.refu.best@likelihood - mean(like.nulls.refu))/sd(like.nulls.refu))

##6.3.4 Species rol
#Observed

cz.refu.l<-data.frame(czvalues(mod.refu.best, level="lower", weighted=T) )
cz.refu.h<-data.frame(czvalues(mod.refu.best, level="higher", weighted=T) )
cz.refu.l[is.na(cz.refu.l)]<-0

critical.c.h<-quantile(cz.refu.h$c, 0.95)
critical.c.l<-quantile(cz.refu.l$c, 0.95)
critical.z.h<-quantile(cz.refu.h$z, 0.95)
critical.z.l<-quantile(cz.refu.l$z, 0.95)

hubs.refu.l<- subset(cz.refu.l, c>=critical.c.l )
hubs.refu.h<- subset(cz.refu.h, c>=critical.c.h )

################# 6.4 Segregation of orders within modules


names(dat.tot)
key.animals<-dat.tot[,c(16,1,11,14)]
key.animals<-key.animals[!duplicated(key.animals$acro),]
colnames(key.animals)<-c("orden","familia","genero","spp")
key.plants<-dat.tot[,7]
key.plants<-key.plants[!duplicated(key.plants)]
orden.plants<-rep("plants", 11)
familia.plants<-c("Brassicaceae","Fabaceae","Asteraceae","Asteraceae","Brassicaceae","Dipsacaceae", "Caryophyllaceae","Lamiaceae","Violaceae","Fabacea", "Boraginaceae")
genero.plants<-c("Erysimum","Spartocytisus","Argyranthemum","Tolpis","Descurainia","Pterocephalus","Silene","Nepeta","Viola","Adenocarpus","Echium")
key.plants<-cbind(orden=orden.plants, familia=familia.plants, genero=genero.plants, data.frame(key.plants))
colnames(key.plants)<-c("orden","familia","genero","spp")
key.spp<-rbind(key.animals,key.plants)

modules<-read.table("modules.txt",header=T, sep="\t") #A dataframe with the distribution of species within modules
modules<-merge(modules, key.spp, by="spp", sort=F)
modules<-modules[order(modules$trophic, modules$localidad, modules$module, modules$spp),]
modules<-subset(modules, orden!="Thysanoptera")
modules<-subset(modules, orden!="Orthoptera")
modules<-subset(modules, orden!="Lepidoptera")
modules<-subset(modules, orden!="Hemiptera")
modules<-droplevels(modules)

t.modules<-table(modules$module,modules$orden)
t.modules.blan<-t.modules[1:3,1:3]

(Xsq <- chisq.test((t.modules.blan),  simulate.p.value = T))
Xsq$observed   # observed counts (same as M) 
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals


t.modules.raj<-t.modules[4:7,1:3]

(Xsq <- chisq.test((t.modules.raj),  simulate.p.value = T))
Xsq$observed   # observed counts (same as M) 
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

t.modules.refu<-t.modules[8:11,1:3]

(Xsq <- chisq.test((t.modules.refu),  simulate.p.value = T))
Xsq$observed   # observed counts (same as M) 
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

p.adjust(c(0.12, 0.03,0.66), method = "bonferroni")


################# 6.4 Segregation of plants within modules

library(lubridate)
dat.pheno<-read.table("fenologia_teide.txt", header=T, sep="\t")

date<-dat.pheno$Inicio
date<-dmy(date)
days_inicio <- yday(date) - 126 # so Jan 1 = day 0

date<-dat.pheno$Final
date<-dmy(date)
days_final <- yday(date)  -126
days_duration<- days_final - days_inicio
 
 dat.pheno<-cbind(dat.pheno, days_inicio, days_final, days_duration)
  dat.pheno<-dat.pheno[,c(2,3,4,5,6,7,8,9,1),]
  
 plot(dat.pheno$id, dat.pheno$days_inicio, type="n", xlim=c(0,90), ylim=c(0,24))
 lines(c(dat.pheno[1,6], dat.pheno[1,7]),  c(dat.pheno[1,2], dat.pheno[1,2]))
 lines(c(dat.pheno[2,6], dat.pheno[2,7]),  c(dat.pheno[2,2], dat.pheno[2,2]))
 lines(c(dat.pheno[3,6], dat.pheno[3,7]),  c(dat.pheno[3,2], dat.pheno[3,2]))
 lines(c(dat.pheno[4,6], dat.pheno[4,7]),  c(dat.pheno[4,2], dat.pheno[4,2]))
 lines(c(dat.pheno[5,6], dat.pheno[5,7]),  c(dat.pheno[5,2], dat.pheno[5,2]))
 lines(c(dat.pheno[6,6], dat.pheno[6,7]),  c(dat.pheno[6,2], dat.pheno[6,2]))
 lines(c(dat.pheno[7,6], dat.pheno[7,7]),  c(dat.pheno[7,2], dat.pheno[7,2]))
 lines(c(dat.pheno[8,6], dat.pheno[8,7]),  c(dat.pheno[8,2], dat.pheno[8,2]))
 lines(c(dat.pheno[9,6], dat.pheno[9,7]),  c(dat.pheno[9,2], dat.pheno[9,2]))
 lines(c(dat.pheno[10,6], dat.pheno[10,7]),  c(dat.pheno[10,2], dat.pheno[10,2]))
   
 lines(c(dat.pheno[11,6], dat.pheno[11,7]),  c(dat.pheno[11,2], dat.pheno[11,2]), lty=2)
 lines(c(dat.pheno[12,6], dat.pheno[12,7]),  c(dat.pheno[12,2], dat.pheno[12,2]), lty=2)
 lines(c(dat.pheno[13,6], dat.pheno[13,7]),  c(dat.pheno[13,2], dat.pheno[13,2]), lty=2)
 lines(c(dat.pheno[14,6], dat.pheno[14,7]),  c(dat.pheno[14,2], dat.pheno[14,2]), lty=2)
 lines(c(dat.pheno[15,6], dat.pheno[15,7]),  c(dat.pheno[15,2], dat.pheno[15,2]), lty=2)
 lines(c(dat.pheno[16,6], dat.pheno[16,7]),  c(dat.pheno[16,2], dat.pheno[16,2]), lty=2)
 lines(c(dat.pheno[17,6], dat.pheno[17,7]),  c(dat.pheno[17,2], dat.pheno[17,2]), lty=2)
 lines(c(dat.pheno[18,6], dat.pheno[18,7]),  c(dat.pheno[18,2], dat.pheno[18,2]), lty=2)
 lines(c(dat.pheno[19,6], dat.pheno[19,7]),  c(dat.pheno[19,2], dat.pheno[19,2]), lty=2)
   
 lines(c(dat.pheno[20,6], dat.pheno[20,7]),  c(dat.pheno[20,2], dat.pheno[20,2]), lty=3)
 lines(c(dat.pheno[21,6], dat.pheno[21,7]),  c(dat.pheno[21,2], dat.pheno[21,2]), lty=3)
 lines(c(dat.pheno[22,6], dat.pheno[22,7]),  c(dat.pheno[22,2], dat.pheno[22,2]), lty=3)
 lines(c(dat.pheno[23,6], dat.pheno[23,7]),  c(dat.pheno[23,2], dat.pheno[23,2]), lty=3)
   
 lines(c(dat.pheno[24,6], dat.pheno[24,7]),  c(dat.pheno[24,2], dat.pheno[24,2]), lty=4)
######################### 7. Models to evaluate changes on niche breadth  along elevevational gradient######################### 

# A data.frame to rule them all   :)

df.combined<-rbind(df.animals,df.plants)
trophic.p<-rep("plant",  nrow(df.plants))
trophic.a<-rep("animal", nrow(df.animals))
trophic<-c(trophic.a, trophic.p)
df.combined<-cbind(df.combined,trophic)

#####Models

##Boxplots
par(mfcol=c(4,2), mar=c(4,4,0.3,0.1))    # bottom, left, top, and right

boxplot(df.animals$normalised.degree~df.animals$elevation, ylab="Normalised degree", col="gray75")
boxplot(df.animals$d~df.animals$elevation, ylab="d", col="gray75")
boxplot(df.animals$weighted.closeness~df.animals$elevation, ylab="weighted Closeness", col="gray75")
boxplot(df.animals$species.strength~df.animals$elevation, xlab="Elevation (m)", ylab="species strength",  col="gray75")

boxplot(df.plants$normalised.degree~df.plants$elevation, col="gray90")
boxplot(df.plants$d~df.plants$elevation, col="gray90")
boxplot(df.plants$weighted.closeness~df.plants$elevation, ylim=c(0,0.4), col="gray90")
boxplot(df.plants$species.strength~df.plants$elevation, xlab="Elevation (m)", col="gray90")

##Barplots

n.mean<-aggregate(normalised.degree~elevation*trophic, df.combined, mean)
n.sd<-aggregate(normalised.degree~elevation*trophic, df.combined, sd)
n.se<-aggregate(normalised.degree~elevation*trophic, df.combined, function (x) sd(x)/sqrt(length(x)))

d.mean<-aggregate(d~elevation*trophic, df.combined, mean)
d.sd<-aggregate(d~elevation*trophic, df.combined, sd)
d.se<-aggregate(d~elevation*trophic, df.combined, function (x) sd(x)/sqrt(length(x)))

c.mean<-aggregate(weighted.closeness~elevation*trophic, df.combined, mean)
c.sd<-aggregate(weighted.closeness~elevation*trophic, df.combined, sd)
c.se<-aggregate(weighted.closeness~elevation*trophic, df.combined, function (x) sd(x)/sqrt(length(x)))

s.mean<-aggregate(species.strength~elevation*trophic, df.combined, mean)
s.sd<-aggregate(species.strength~elevation*trophic, df.combined, sd)
s.se<-aggregate(species.strength~elevation*trophic, df.combined, function (x) sd(x)/sqrt(length(x)))

par(mfcol=c(5,2), mar=c(4,3.8,0.5,0.1))    # bottom, left, top, and right
#norm degree animals

mp<-barplot(n.mean[1:3,3], ylim=c(0,0.5),names.arg=n.mean[1:3,1],  ylab="Normalised degree", col="gray75")  
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(n.mean[1,3],n.mean[1,3]+n.se[1,3])) 
lines(x=c(mp[2,1],mp[2,1]), y=c(n.mean[2,3],n.mean[2,3]+n.se[2,3])) 
lines(x=c(mp[3,1],mp[3,1]), y=c(n.mean[3,3],n.mean[3,3]+n.se[3,3])) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(n.mean[1,3]+n.se[1,3],n.mean[1,3]+n.se[1,3]))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(n.mean[2,3]+n.se[2,3],n.mean[2,3]+n.se[2,3]))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(n.mean[3,3]+n.se[3,3],n.mean[3,3]+n.se[3,3]))

#d animals
mp<-barplot(d.mean[1:3,3], ylim=c(0,0.25),names.arg=d.mean[1:3,1], ylab="Specialization, d'", col="gray75") 
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(d.mean[1,3],d.mean[1,3]+d.se[1,3])) 
lines(x=c(mp[2,1],mp[2,1]), y=c(d.mean[2,3],d.mean[2,3]+d.se[2,3])) 
lines(x=c(mp[3,1],mp[3,1]), y=c(d.mean[3,3],d.mean[3,3]+d.se[3,3])) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(d.mean[1,3]+d.se[1,3],d.mean[1,3]+d.se[1,3]))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(d.mean[2,3]+d.se[2,3],d.mean[2,3]+d.se[2,3]))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(d.mean[3,3]+d.se[3,3],d.mean[3,3]+d.se[3,3]))

#weighted closeness  animals

mp<-barplot(c.mean[1:3,3], ylim=c(0,0.030), names.arg=c.mean[1:3,1], ylab="Weighted Closeness", col="gray75")
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(c.mean[1,3],c.mean[1,3]+c.se[1,3])) 
lines(x=c(mp[2,1],mp[2,1]), y=c(c.mean[2,3],c.mean[2,3]+c.se[2,3])) 
lines(x=c(mp[3,1],mp[3,1]), y=c(c.mean[3,3],c.mean[3,3]+c.se[3,3])) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(c.mean[1,3]+c.se[1,3],c.mean[1,3]+c.se[1,3]))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(c.mean[2,3]+c.se[2,3],c.mean[2,3]+c.se[2,3]))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(c.mean[3,3]+c.se[3,3],c.mean[3,3]+c.se[3,3]))

#strength     animals
mp<-barplot(s.mean[1:3,3], ylim=c(0,0.20), names.arg=s.mean[1:3,1],  ylab="Species strength",  col="gray75")
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(s.mean[1,3],s.mean[1,3]+c.se[1,3]+0.001)) 
lines(x=c(mp[2,1],mp[2,1]), y=c(s.mean[2,3],s.mean[2,3]+c.se[2,3]+0.001)) 
lines(x=c(mp[3,1],mp[3,1]), y=c(s.mean[3,3],s.mean[3,3]+c.se[3,3]+0.001)) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(s.mean[1,3]+c.se[1,3]+0.002,s.mean[1,3]+c.se[1,3]+0.002))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(s.mean[2,3]+c.se[2,3]+0.002,s.mean[2,3]+c.se[2,3]+0.002))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(s.mean[3,3]+c.se[3,3]+0.002,s.mean[3,3]+c.se[3,3]+0.002))

#c animals
mp<-barplot(cmod.mean[1:3,3], names.arg=cmod.mean[1:3,1],  ylim=c(0,0.25),xlab="Elevation (m)",ylab="Connection, c",  col="gray75")
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(cmod.mean[1,3],cmod.mean[1,3]+cmod.se[1,3])) 
lines(x=c(mp[2,1],mp[2,1]), y=c(cmod.mean[2,3],cmod.mean[2,3]+cmod.se[2,3])) 
lines(x=c(mp[3,1],mp[3,1]), y=c(cmod.mean[3,3],cmod.mean[3,3]+cmod.se[3,3])) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(cmod.mean[1,3]+cmod.se[1,3],cmod.mean[1,3]+cmod.se[1,3]))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(cmod.mean[2,3]+cmod.se[2,3],cmod.mean[2,3]+cmod.se[2,3]))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(cmod.mean[3,3]+cmod.se[3,3],cmod.mean[3,3]+cmod.se[3,3]))
 

#norm degree plants

mp<-barplot(n.mean[4:6,3], ylim=c(0,0.60), names.arg=n.mean[1:3,1],   col="gray90")  
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(n.mean[4,3],n.mean[4,3]+n.se[4,3])) 
lines(x=c(mp[2,1],mp[2,1]), y=c(n.mean[5,3],n.mean[5,3]+n.se[5,3])) 
lines(x=c(mp[3,1],mp[3,1]), y=c(n.mean[6,3],n.mean[6,3]+n.se[6,3])) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(n.mean[4,3]+n.se[4,3],n.mean[4,3]+n.se[4,3]))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(n.mean[5,3]+n.se[5,3],n.mean[5,3]+n.se[5,3]))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(n.mean[6,3]+n.se[6,3],n.mean[6,3]+n.se[6,3]))

#d plants
mp<-barplot(d.mean[4:6,3], ylim=c(0,0.50),names.arg=d.mean[1:3,1], col="gray90") 
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(d.mean[4,3],d.mean[4,3]+d.se[4,3])) 
lines(x=c(mp[2,1],mp[2,1]), y=c(d.mean[5,3],d.mean[5,3]+d.se[5,3])) 
lines(x=c(mp[3,1],mp[3,1]), y=c(d.mean[6,3],d.mean[6,3]+d.se[6,3])) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(d.mean[4,3]+d.se[4,3],d.mean[4,3]+d.se[4,3]))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(d.mean[5,3]+d.se[5,3],d.mean[5,3]+d.se[5,3]))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(d.mean[6,3]+d.se[6,3],d.mean[6,3]+d.se[6,3]))

#weighted closeness  plants

mp<-barplot(c.mean[4:6,3], ylim=c(0,0.40), names.arg=c.mean[1:3,1],  col="gray90")
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(c.mean[4,3],c.mean[4,3]+c.se[4,3])) 
lines(x=c(mp[2,1],mp[2,1]), y=c(c.mean[5,3],c.mean[5,3]+c.se[5,3])) 
lines(x=c(mp[3,1],mp[3,1]), y=c(c.mean[6,3],c.mean[6,3]+c.se[6,3])) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(c.mean[4,3]+c.se[4,3],c.mean[4,3]+c.se[4,3]))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(c.mean[5,3]+c.se[5,3],c.mean[5,3]+c.se[5,3]))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(c.mean[6,3]+c.se[6,3],c.mean[6,3]+c.se[6,3]))

#strength     plants
mp<-barplot(s.mean[4:6,3], ylim=c(0,14), names.arg=s.mean[1:3,1],   col="gray90")
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(s.mean[4,3],s.mean[4,3]+c.se[4,3]+0.1)) 
lines(x=c(mp[2,1],mp[2,1]), y=c(s.mean[5,3],s.mean[5,3]+c.se[5,3]+0.1)) 
lines(x=c(mp[3,1],mp[3,1]), y=c(s.mean[6,3],s.mean[6,3]+c.se[6,3]+0.1)) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(s.mean[4,3]+c.se[4,3]+0.1,s.mean[4,3]+c.se[4,3]+0.1))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(s.mean[5,3]+c.se[5,3]+0.15,s.mean[5,3]+c.se[5,3]+0.15))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(s.mean[6,3]+c.se[6,3]+0.1,s.mean[6,3]+c.se[6,3]+0.1))

#c plants
mp<-barplot(cmod.mean[4:6,3], ylim=c(0,0.50), names.arg=cmod.mean[1:3,1],  xlab="Elevation (m)", col="gray90")
str(mp) # X-axis values
lines(x=c(mp[1,1],mp[1,1]), y=c(cmod.mean[4,3],cmod.mean[4,3]+cmod.se[4,3]+0.1)) 
lines(x=c(mp[2,1],mp[2,1]), y=c(cmod.mean[5,3],cmod.mean[5,3]+cmod.se[5,3]+0.1)) 
lines(x=c(mp[3,1],mp[3,1]), y=c(cmod.mean[6,3],cmod.mean[6,3]+cmod.se[6,3]+0.1)) 
lines(x=c(mp[1,1]-0.2,mp[1,1]+0.2), y=c(cmod.mean[4,3]+cmod.se[4,3]+0.1,cmod.mean[4,3]+cmod.se[4,3]+0.1))
lines(x=c(mp[2,1]-0.2,mp[2,1]+0.2), y=c(cmod.mean[5,3]+cmod.se[5,3]+0.1,cmod.mean[5,3]+cmod.se[5,3]+0.1))
lines(x=c(mp[3,1]-0.2,mp[3,1]+0.2), y=c(cmod.mean[6,3]+cmod.se[6,3]+0.1,cmod.mean[6,3]+cmod.se[6,3]+0.1))


lm.p.m<-lmer(normalised.degree~elevation*trophic+ (1|spp),data=df.combined) 
anova(lm.p.m)
car::Anova(lm.p.m)
summary(lm.p.m)
piecewiseSEM::sem.model.fits (lm.p.m) # Rm=0.07, Rc=0.60
lsmeans(lm.p.m, pairwise~elevation, adjust="tukey")

lm.p.d<-lmer(d~elevation*trophic+ (1|spp),data=df.combined )
anova(lm.p.d)
car::Anova(lm.p.d)         
summary(lm.p.d)
piecewiseSEM::sem.model.fits (lm.p.d) # Rm=0.30, Rc=0.38
lsmeans(lm.p.d, pairwise~elevation*trophic, adjust="tukey")

lm.p.wc<-lmer(weighted.closeness~elevation*trophic+ (1|spp),data=df.combined) 
anova(lm.p.wc)
car::Anova(lm.p.wc)
summary(lm.p.wc)
piecewiseSEM::sem.model.fits (lm.p.wc) # Rm=0.72, Rc=0.81
lsmeans(lm.p.wc, pairwise~elevation*trophic, adjust="tukey")

lm.p.st<-lmer(species.strength~elevation*trophic+ (1|spp),data=df.combined) 
car::Anova(lm.p.st)
piecewiseSEM::sem.model.fits (lm.p.st)   # Rm=0.62, Rc=0.90
lsmeans(lm.p.st, pairwise~elevation*trophic, adjust="tukey")

lm.p.cmod<-lmer(c~elevation*trophic+ (1|spp),data=df.combined) 
anova(lm.p.cmod)
car::Anova(lm.p.cmod)
summary(lm.p.cmod)
piecewiseSEM::sem.model.fits (lm.p.cmod) # Rm=0.07, Rc=0.60
lsmeans(lm.p.cmod, pairwise~elevation*trophic, adjust="tukey")

##Species present in all elevations.

df.elevel<-read.table("especieslevel_compartidas.txt",header=T)
c.elevel<-c(0.07,0.12,0.22,0.16,0.15,0.43,0.44,0.42,0.1,0.34,0.29,0.15)
z.elevel<-c(1.15,1.27,0,-0.56,0.71,0,-0.7,0.71,0,-1.29,0.1,0)
elevation.num<-ifelse(df.elevel$elevation=="High", "3200", ifelse(df.elevel$elevation=="Intermediate","2700","2400"))
df.elevel<-cbind(df.elevel,c=c.elevel,z=z.elevel, elevation.num)
par(mfcol=c(2,3), mar=c(4,4.5,2,1))
boxplot(df.elevel$Ndegre ~df.elevel$elevation.num, ylab="Ndegree")
boxplot(df.elevel$d~df.elevel$elevation.num, ylab="d")
boxplot(df.elevel$Wclos~df.elevel$elevation.num, ylab="Wclos")
boxplot(df.elevel$c ~df.elevel$elevation.num, ylab="Connection, c")
boxplot(df.elevel$strength ~df.elevel$elevation.num, ylab="Int. strength")
boxplot(df.elevel$z ~df.elevel$elevation.num, ylab="Participation, z")
 
(kt.m<-kruskal.test(Ndegre ~ elevation.num, data = df.elevel) ) 
(kt.d<-kruskal.test(d ~ elevation.num, data = df.elevel) )  
(kt.wc<-kruskal.test(Wclos ~ elevation.num, data = df.elevel)   ) 
(kt.s<-kruskal.test(strength ~ elevation.num, data = df.elevel)  ) 
(kt.cmod<-kruskal.test(c ~ elevation.num, data = df.elevel) )  
(kt.zmod<-kruskal.test(z ~ elevation.num, data = df.elevel)  ) 

#########################   8. Rarefaction curves    ######################### 

# Source the rarefaction function by typing
        source("http://www.jennajacobs.org/R/rarefaction.txt") #source the function from the website.

# The rarefaction function is rarefaction(x, subsample=5, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)
                where x= the datamatrix
                subsample= the interval betwwen subsamples (default is 5)  
                plot= if you want the function to plot the results
                color= if you want the plot in color (TRUE) or black and white (FALSE)
                error=  if you want the plot to show the iterative error around the mean
                legend= if you want a legend
                symbol= a vector of symbols to correspond with treatments [eg. c(1,2,3)]


dat.rarefaction<-table(dat.tot$localidad, dat.tot$acro)
library(vegan)

#Rarefaction of the dataset
dat.rarefaed<-rarefaction(dat.rarefaction, col=F) # I'm a big fan of B&W

#The rarefaction function creates 3 objects to access them type
dat.rarefaed$richness # a matrix of the mean richness at each subsample
dat.rarefaed$SE  # a matrix of the iterative SE of the mean richness at each subsample
dat.rarefaed$subsample # the subsample sizes used to find the means