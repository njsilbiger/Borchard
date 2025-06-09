### Polynesia Mana data Moorea ###

setwd("/Volumes/MORITZ/Borchard/Data/CRIOBE")


library(vegan)
library(reshape)
library(reshape2)
library(plotrix)
library(ggplot2)
library(gridExtra)


# Load table
pm<-read.table("PolyManaSubstrate.csv",header=T,sep=";",dec=",",stringsAsFactor=T)

head(pm)
str(pm)

# Remove tiahura 2001 quadrat 2: unavailable
pm<-pm[!(pm$annee %in% 2001 & pm$site %in% "tiahura34" & pm$n.quadrat %in% 2),]

### Transform to site spp matrix
data1 = pm
data.temp = melt(data1, id=c("annee","site","n.quadrat","genre"), measure.vars="recouvrement", na.rm=FALSE)
pm.matrix = cast(data.temp,annee+site+n.quadrat ~ genre, sum, add.missing = F)

table(pm.matrix$site,pm.matrix$annee) 


# Corail
corail.names<-c("Acanthastrea","Acropora","Astrea curta","Astreopora","Corail autre","Cyphastrea","Fungia","Gardineroseris","Goniastrea","Goniastrea stelligera","Herpolitha","Leptastrea","Leptoseris","Lobophyllia","Millepora","Montipora","Napopora","Pavona","Pavona varians","Pocillopora","Porites","Porites rus","Psammocora","Sandalolitha")

# Algues
algue.names<-c("Algue Asparagopsis","Algue autre","Algue Halimeda") # sans les cyanoB


# Selection des colonnes corail et algues
match.cor<-match(levels(as.factor(corail.names)),colnames(pm.matrix))
match.alg<-match(levels(as.factor(algue.names)),colnames(pm.matrix))

# Nouvelles matrices corail et algues
cor<-cbind(pm.matrix[,1:3], pm.matrix[,match.cor])
alg<-cbind(pm.matrix[,1:3], pm.matrix[,match.alg])

# Corail / algues total par transect, site, saison, année
cor.cover<-rowSums(cor[,4:27])
alg.cover<-rowSums(alg[,4:6])

# Data frames corail / algues / les deux
corail<-cbind(cor[,1:3],cor.cover)
algues<-cbind(alg[,1:3],alg.cover)
#total <-cbind(cor[,1:3],cor.cover,alg.cover)


### Mean per site
cor.moy.se<-do.call(data.frame,aggregate(corail$cor.cover,list(corail$annee,corail$site),function(x) c(mean=mean(x),sd=std.error(x))))
names(cor.moy.se)[1:4]<-c("year","site","cor.mean","cor.se")

# Moyennes et SE algues
alg.moy.se<-do.call(data.frame,aggregate(algues$alg.cover,list(algues$annee,algues$site),function(x) c(mean=mean(x),sd=std.error(x))))
names(alg.moy.se)[1:4]<-c("year","site","alg.mean","alg.se")

# Dataset total
sub.site<-cbind(cor.moy.se,alg.moy.se[3:4])


### Entire island (all sites) : not relevant because not sampled every year?
# Moyenne total Moorea corail
cor.tot<-do.call(data.frame,aggregate(cor.moy.se$cor.mean,list(cor.moy.se$year),function(x) c(mean=mean(x),sd=std.error(x))))
names(cor.tot)[1:3]<-c("year","cor.mean","cor.se")

# Moyenne total Moorea algues
alg.tot<-do.call(data.frame,aggregate(alg.moy.se$alg.mean,list(alg.moy.se$year),function(x) c(mean=mean(x),sd=std.error(x))))
names(alg.tot)[1:3]<-c("year","alg.mean","alg.se")

# Dataset total
sub.tot<-cbind(cor.tot,alg.tot[2:3])


### Graphs

# Island
p.tot<-ggplot(sub.tot)+
	geom_line(aes(x=year,y=cor.mean),colour="salmon1")+
	geom_errorbar(aes(x= year,y=cor.mean,ymin=sub.tot$cor.mean-sub.tot$cor.se,ymax=sub.tot$cor.mean+sub.tot$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=sub.tot$alg.mean-sub.tot$alg.se,ymax=sub.tot$alg.mean+sub.tot$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	scale_y_continuous(limits=c(0,100))+scale_x_continuous(limits=c(1993,2024),breaks=c(1994,2004,2014,2024),labels=c(1994,2004,2014,2024))+
	ggtitle("Moorea Polynesia Mana")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))



# Par site
p.site.ext<-ggplot(sub.site)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=sub.site$cor.mean-sub.site$cor.se,ymax=sub.site$cor.mean+sub.site$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=sub.site$alg.mean-sub.site$alg.se,ymax=sub.site$alg.mean+sub.site$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(-1,100))+
scale_x_continuous(limits=c(1993,2024),breaks=c(1994,2004,2014,2024),labels=c(1994,2004,2014,2024))+
	ggtitle("Moorea Polynesia Mana")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))
	
	
############## FISH ##############

# Load table
pm.f<-read.table("PolyManaFish.csv",header=T,sep=";",dec=",",stringsAsFactor=T)
head(pm.f)
str(pm.f)

names(pm.f)<-tolower(names(pm.f))

# Correct errors on sites
levels(pm.f$site)
levels(pm.f$site)[1]<-"Moorea Entre 2 Baies"

# Total fish abundance per transect (per 50 m2: double-check with Maggy/Gilles)
pm.sum<-aggregate(pm.f$abundance,list(pm.f$year,pm.f$site,pm.f$transect),sum)
names(pm.sum)<-c("year","site","transect","abundance")

### Mean SE coral per site and habitat
fish.mean<-do.call(data.frame,aggregate(pm.sum$abundance,list(pm.sum$year,pm.sum$site),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.mean)[1:4]<-c("year","site","abund.mean","abund.se")


### Entire island (all sites)
# Mean SE fish abundance
fish.tot<-do.call(data.frame,aggregate(fish.mean$abund.mean,list(fish.mean$year),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.tot)[1:3]<-c("year","ab.mean","ab.se")

### Graphs

# Island
p.tot<-ggplot(fish.tot)+
	geom_line(aes(x=year,y=ab.mean),colour="deepskyblue2")+
	geom_errorbar(aes(x= year,y=ab.mean,ymin=ab.mean-ab.se,ymax=ab.mean+ab.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (250 m2)")+
	scale_y_continuous(limits=c(0,2000))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2008,2012,2016,2020,2024),labels=c(2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea Polynesia Mana")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

p.tot<-ggplot(fish.mean)+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (250 m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,2000))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea Polynesia Mana")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))












