### MPA data Moorea ###

setwd("/Volumes/MORITZ/Borchard/Data/CRIOBE")


library(vegan)
library(reshape)
library(reshape2)
library(plotrix)
library(ggplot2)
library(gridExtra)


# Load table
mpa<-read.table("MPAsubstrate.csv",header=T,sep=";",dec=",",stringsAsFactor=T)
head(mpa)
str(mpa)

# Remove observations
mpa<-mpa[,-11]

names(mpa)<-tolower(names(mpa))

mpa$proportion<-mpa$proportion*100


# Correct errors in habitat
levels(mpa$habitat)
levels(mpa$habitat)[2]<-"Barrier reef"
levels(mpa$habitat)[4]<-"Fore reef"
# Correct errors in site
levels(mpa$marine.area)
levels(mpa$marine.area)[10]<-"Nuarei"
# Correct substrate name
levels(mpa$substrate)
levels(mpa$substrate)[9]<-"Corail non identifie"
levels(mpa$substrate)[33]<-"Padina"
levels(mpa$substrate)[36]<-"Pocillopora"
levels(mpa$substrate)[41]<-"Sand"
levels(mpa$substrate)[42]<-"Sargassum"
levels(mpa$substrate)[43]<-"Stegastes turf"
levels(mpa$substrate)[45]<-"Stegastes turf"


### Transform to site spp matrix
data1 = mpa
names(data1) = tolower(names(data1))
data.temp = melt(data1, id=c("campaign","year","season","date","marine.area","habitat","transect","substrate"), measure.vars="proportion", na.rm=FALSE)
mpa.matrix = cast(data.temp,campaign+year+season+date+marine.area+habitat+transect ~ substrate, sum, add.missing = F)

table(mpa.matrix$marine.area,mpa.matrix$year) # check 2009 (1 transect trop Pihaena), 2011 (3 moins Nuarei), 2012 (1 trop Tiahura), 2016 (1 trop Nuarei), 2021 (1 trop moyu ahi), 2023 (1 moins Nuarei).

#write.table(data_final,"Substrat.txt",sep="\t",dec=".",quote=F)
#cover<-read.table("Substrat.txt",header=T,sep="\t",dec=".")

# Corail
corail.names<-c("Acanthastrea","Acropora","Astreopora","Corail non identifie","Coscinaraea","Cyphastrea","Fungia","Gardineroseris","Herpolitha","Leptastrea","Leptoseris","Lobophyllia","Millepora","Montipora","Napopora","Pavona","Pocillopora","Porites","Psammocora","Synarea")

# Algues
algue.names<-c("Asparagopsis","Boodlea","Caulerpa","Dictyota","Halimeda","Macroalgae","Padina","Peyssonnelia","Sargassum","Stegastes turf","Turbinaria") # sans les cyanoB


#corail<-sub.matrix[sub$Substrate %in% levels(as.factor(corail.names)),]
#algue <-sub.matrix[sub$Substrate %in% levels(as.factor(algue.names)),]

# Selection des colonnes corail et algues
match.cor<-match(levels(as.factor(corail.names)),colnames(mpa.matrix))
match.alg<-match(levels(as.factor(algue.names)),colnames(mpa.matrix))

# Nouvelles matrices corail et algues
cor<-cbind(mpa.matrix[,1:7], mpa.matrix[,match.cor])
alg<-cbind(mpa.matrix[,1:7], mpa.matrix[,match.alg])

# Corail / algues total par transect, site, saison, année
cor.cover<-rowSums(cor[,8:27])
alg.cover<-rowSums(alg[,8:18])

# Data frames corail / algues / les deux
corail<-cbind(cor[,1:7],cor.cover)
algues<-cbind(alg[,1:7],alg.cover)
#total <-cbind(cor[,1:7],cor.cover,alg.cover)


### Mean SE coral per site and habitat
cor.moy.se<-do.call(data.frame,aggregate(corail$cor.cover,list(corail$year,corail$marine.area,corail$habitat),function(x) c(mean=mean(x),sd=std.error(x))))
names(cor.moy.se)[1:5]<-c("year","site","habitat","cor.mean","cor.se")

# Moyennes et SE algues
alg.moy.se<-do.call(data.frame,aggregate(algues$alg.cover,list(algues$year,algues$marine.area,algues$habitat),function(x) c(mean=mean(x),sd=std.error(x))))
names(alg.moy.se)[1:5]<-c("year","site","habitat","alg.mean","alg.se")

# Dataset total
sub.site<-cbind(cor.moy.se,alg.moy.se[4:5])


### Entire island (all sites)
# Moyenne total Moorea corail
cor.tot<-do.call(data.frame,aggregate(cor.moy.se$cor.mean,list(cor.moy.se$year,cor.moy.se$habitat),function(x) c(mean=mean(x),sd=std.error(x))))
names(cor.tot)[1:4]<-c("year","habitat","cor.mean","cor.se")

# Moyenne total Moorea algues
alg.tot<-do.call(data.frame,aggregate(alg.moy.se$alg.mean,list(alg.moy.se$year,alg.moy.se$habitat),function(x) c(mean=mean(x),sd=std.error(x))))
names(alg.tot)[1:4]<-c("year","habitat","alg.mean","alg.se")

# Dataset total
sub.tot<-cbind(cor.tot,alg.tot[3:4])


### Graphs

# Island
p.tot<-ggplot(sub.tot)+
	geom_line(aes(x=year,y=cor.mean,group=habitat),colour="salmon1")+
	geom_errorbar(aes(x= year,y=cor.mean,ymin=sub.tot$cor.mean-sub.tot$cor.se,ymax=sub.tot$cor.mean+sub.tot$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=habitat),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=sub.tot$alg.mean-sub.tot$alg.se,ymax=sub.tot$alg.mean+sub.tot$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~habitat,scales="free")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2008,2012,2016,2020,2024),labels=c(2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea MPA")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))



# Par site 3 habitats

# Pente externe
ext.site<-sub.site[sub.site$habitat %in% "Fore reef",] 

p.site.ext<-ggplot(ext.site)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=ext.site$cor.mean-ext.site$cor.se,ymax=ext.site$cor.mean+ext.site$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=ext.site$alg.mean-ext.site$alg.se,ymax=ext.site$alg.mean+ext.site$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(-1,100))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea MPA Fore reef")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

# Récif barrière
bar.site<-sub.site[sub.site$habitat %in% "Barrier reef",] 

p.site.bar<-ggplot(bar.site)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=bar.site$cor.mean-bar.site$cor.se,ymax=bar.site$cor.mean+bar.site$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=bar.site$alg.mean-bar.site$alg.se,ymax=bar.site$alg.mean+bar.site$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(-1,100))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea MPA Barrier reef")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

# Récif frangeant
frg.site<-sub.site[sub.site$habitat %in% "Fringing reef",] 

p.site.frg<-ggplot(frg.site)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=frg.site$cor.mean-frg.site$cor.se,ymax=frg.site$cor.mean+frg.site$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=frg.site$alg.mean-frg.site$alg.se,ymax=frg.site$alg.mean+frg.site$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(-1,100))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea MPA Fringing reef")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))


############## FISH ##############

# Load table
mpa.f<-read.table("MPAFish.csv",header=T,sep=";",dec=",",stringsAsFactor=T)
head(mpa.f)
str(mpa.f)

# Remove observations
mpa.f<-mpa.f[,-12]

names(mpa.f)<-tolower(names(mpa.f))

# Correct errors in habitat
levels(mpa.f$habitat)
levels(mpa.f$habitat)[4]<-"Fore reef"

# Total fish abundance per transect (per 50 m2: double-check with Maggy/Gilles)
mpa.sum<-aggregate(mpa.f$abundance,list(mpa.f$year,mpa.f$season,mpa.f$marine.area,mpa.f$habitat,mpa.f$transect),sum)
names(mpa.sum)<-c("year","season","site","habitat","transect","abundance")


### Mean SE coral per site and habitat
fish.mean<-do.call(data.frame,aggregate(mpa.sum$abundance,list(mpa.sum$year,mpa.sum$site,mpa.sum$habitat),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.mean)[1:5]<-c("year","site","habitat","abund.mean","abund.se")


### Entire island (all sites)
# Mean SE fish abundance
fish.tot<-do.call(data.frame,aggregate(fish.mean$abund.mean,list(fish.mean$year,fish.mean$habitat),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.tot)[1:4]<-c("year","habitat","ab.mean","ab.se")

### Graphs

# Island
p.tot<-ggplot(fish.tot)+
	geom_line(aes(x=year,y=ab.mean,group=habitat),colour="deepskyblue2")+
	geom_errorbar(aes(x= year,y=ab.mean,ymin=fish.tot$ab.mean-fish.tot$ab.se,ymax=fish.tot$ab.mean+fish.tot$ab.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (50 m2)")+
	facet_wrap(~habitat,scales="free")+
	scale_y_continuous(limits=c(0,400))+
	scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2008,2012,2016,2020,2024),labels=c(2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea MPA")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))


# Par site 3 habitats

# Pente externe
fish.ext<-fish.mean[fish.mean$habitat %in% "Fore reef",] 

p.site.ext<-ggplot(fish.ext)+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (50 m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,800))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea MPA Fore reef")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

# Barriere
fish.bar<-fish.mean[fish.mean$habitat %in% "Barrier reef",] 

p.site.ext<-ggplot(fish.bar)+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (50 m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,600))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea MPA Barrier reef")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))


# Frangeant
fish.frg<-fish.mean[fish.mean$habitat %in% "Fringing reef",] 

p.site.ext<-ggplot(fish.frg)+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (50 m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,400))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea MPA Fringing reef")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

