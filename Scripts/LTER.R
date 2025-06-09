### LTER data Moorea ###

setwd("/Volumes/MORITZ/Borchard/Data/LTER")


library(vegan)
library(reshape)
library(reshape2)
library(plotrix)
library(ggplot2)
library(gridExtra)



# Load table
lter<-read.table("MCR_LTER_Annual_Survey_Benthic_Cover_20241219.csv",sep=",",dec=".",header=T,stringsAsFactor=T)

head(lter)
str(lter)
dim(lter)

names(lter)<-tolower(names(lter))
names(lter)[9:10]<-c("substrate","cover")

levels(as.factor(lter$depth)) # 4 depths
levels(lter$habitat) # 3 habitats
levels(lter$site) # 6 sites
levels(as.factor(lter$transect)) # 5 transects
levels(as.factor(lter$quadrat)) # 10 quadrats
levels(lter$location) # all info combined

levels(lter$substrate)

### Transform to site spp matrix
data1 = lter
names(data1) = tolower(names(data1))
data.temp = melt(data1, id=c("year","site","depth","transect","quadrat","substrate"), measure.vars="cover", na.rm=FALSE)
lter.matrix = cast(data.temp,year+site+depth+transect+quadrat ~ substrate, sum, add.missing = F)

table(lter.matrix$site,lter.matrix$year) # 2005 - 2006: less sites


# Coral: use "Coral" substrate (column # 27)
lter.matrix$Coral

# Algae: sum all algae/turf, remove CCA
algae.names<-c("Acanthophora spicifera","Actinotrichia fragilis","Algal Turf","Amansia rhodantha","Amphiroa fragilissima","Asparagopsis taxiformis","Boodlea kaeneana","Caulerpa peltata","Caulerpa pickeringii","Caulerpa racemosa","Caulerpa serrulata","Chaetomorpha antennina","Chlorodesmis fastigiata","Chnoospora implexa","Cladophoropsis luxurians","Cladophoropsis membranacea","Codium geppiorum","Coelothrix irregularis","Colpomenia sinuosa","Damselfish Turf","Dichotomaria marginata","Dichotomaria obtusata","Dictyosphaeria cavernosa","Dictyosphaeria versluysii","Dictyota bartayresiana","Dictyota divaricata","Dictyota friabilis","Dictyota hamifera","Dictyota implexa","Dictyota sp.","Galaxaura filamentosa","Galaxaura rugosa","Galaxaura sp.","Gelidiella acerosa","Gelidiella sp.","Gibsmithia hawaiiensis","Halimeda discoidea","Halimeda distorta","Halimeda incrassata","Halimeda macroloba","Halimeda minima","Halimeda opuntia","Halimeda sp.","Halimeda taenicola","Hydroclathrus clathratus","Hypnea spinella","Jania sp.","Liagora ceranoides","Lobophora variegata","Martensia elegans","Microdictyon okamurae","Microdictyon umbilicatum","Neomeris vanbosseae","Padina boryana","Phyllodictyon anastomosans","Ralfsia sp.","Rhipidosiphon javensis","Sargassum pacificum","Turbinaria ornata","Valonia aegagropila","Valonia ventricosa")         

# Selection des colonnes algues
match.alg<-match(levels(as.factor(algae.names)),colnames(lter.matrix))

# Nouvelle matrice corail et algues
coral<-cbind(lter.matrix[,1:5],coral=lter.matrix$Coral)

alg<-cbind(lter.matrix[,1:5],lter.matrix[,match.alg])
alg.cover<-rowSums(alg[,6:66])
algae<-cbind(lter.matrix[,1:5],algae=alg.cover)

# Mean se per depth (10 Forereef, 17 Forereef, 2 Backreef, 6 Fringing)
# and per site and per transect and per quadrat
cor.moy.se<-do.call(data.frame,aggregate(coral$coral,list(coral$year,coral$site,coral$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(cor.moy.se)[1:5]<-c("year","site","depth","cor.mean","cor.se")

# Moyennes et SE algues
alg.moy.se<-do.call(data.frame,aggregate(algae$algae,list(algae$year,algae$site,algae$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(alg.moy.se)[1:5]<-c("year","site","depth","alg.mean","alg.se")

# Dataset total
sub.site<-cbind(cor.moy.se,alg.moy.se[4:5])


# Mean se per depth, transect, quadrat (whole sites/entire island)
# Coral
cor.tot<-do.call(data.frame,aggregate(cor.moy.se$cor.mean,list(cor.moy.se$year,cor.moy.se$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(cor.tot)[1:4]<-c("year","depth","cor.mean","cor.se")

# Algae
alg.tot<-do.call(data.frame,aggregate(alg.moy.se$alg.mean,list(alg.moy.se$year,alg.moy.se$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(alg.tot)[1:4]<-c("year","depth","alg.mean","alg.se")

# Dataset total
sub.tot<-cbind(cor.tot,alg.tot[3:4])

### Graphs

# Island
p.tot<-ggplot(sub.tot)+
	geom_line(aes(x=year,y=cor.mean,group=depth),colour="salmon1")+
	geom_errorbar(aes(x= year,y=cor.mean,ymin=sub.tot$cor.mean-sub.tot$cor.se,ymax=sub.tot$cor.mean+sub.tot$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=depth),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=sub.tot$alg.mean-sub.tot$alg.se,ymax=sub.tot$alg.mean+sub.tot$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~depth,scales="free")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(2005,2025),breaks=c(2005,2010,2015,2020,2025),labels=c(2005,2010,2015,2020,2025))+
	ggtitle("Moorea LTER")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))



# Per site 4 depths

# 17 m
site17<-sub.site[sub.site$depth==17,] 

p.site.ext<-ggplot(site17)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=site17$cor.mean-site17$cor.se,ymax= site17$cor.mean+site17$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin= site17$alg.mean-site17$alg.se,ymax= site17$alg.mean+site17$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(2005,2025),breaks=c(2005,2010,2015,2020,2025),labels=c(2005,2010,2015,2020,2025))+
	ggtitle("Moorea LTER 17 m")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

# 10 m
site10<-sub.site[sub.site$depth==10,] 

p.site.ext<-ggplot(site10)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=site10$cor.mean-site10$cor.se,ymax= site10$cor.mean+site10$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin= site10$alg.mean-site10$alg.se,ymax= site10$alg.mean+site10$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(2005,2025),breaks=c(2005,2010,2015,2020,2025),labels=c(2005,2010,2015,2020,2025))+
	ggtitle("Moorea LTER 10 m")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

# 6 m
site6<-sub.site[sub.site$depth==6,] 

p.site.ext<-ggplot(site6)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=site6$cor.mean-site6$cor.se,ymax= site6$cor.mean+site6$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin= site6$alg.mean-site6$alg.se,ymax= site6$alg.mean+site6$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(2005,2025),breaks=c(2005,2010,2015,2020,2025),labels=c(2005,2010,2015,2020,2025))+
	ggtitle("Moorea LTER 6 m")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

# 2 m
site2<-sub.site[sub.site$depth==2,] 

p.site.ext<-ggplot(site2)+
	geom_line(aes(x=year,y=cor.mean,group=site),colour="salmon1")+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=site2$cor.mean-site2$cor.se,ymax= site2$cor.mean+site2$cor.se),colour="salmon1",width=0)+
	geom_line(aes(x=year,y=alg.mean,group=site),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin= site2$alg.mean-site2$alg.se,ymax= site2$alg.mean+site2$alg.se),colour="aquamarine3",width=0)+
	xlab("") + ylab("Cover (%)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(2005,2025),breaks=c(2005,2010,2015,2020,2025),labels=c(2005,2010,2015,2020,2025))+
	ggtitle("Moorea LTER 2 m")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))




## Coral? only backreef? other dataset?
lter.cor<-read.table("MCR_LTER_Coral_Cover_Backreef_Long_20241114.csv",sep=",",dec=".",header=T,stringsAsFactor=T)

levels(lter.cor$habitat)


	
############## FISH ##############

# Load table
lter.f<-read.table("MCR_LTER_Annual_Fish_Survey_20240927.csv",header=T,sep=",",dec=",",stringsAsFactor=T)
head(lter.f)
str(lter.f)

names(lter.f)<-tolower(names(lter.f))

levels(as.factor(lter.f$swath)) # 1 and 5 m: how to account for this?? Sum? Proportion?

# Sum of abundance in both swath
# Total fish abundance per transect (per XXX m2: double-check with Peter)
lter.sum<-aggregate(lter.f$count,list(lter.f$year,lter.f$site,lter.f$depth,lter.f$transect),sum)
names(lter.sum)<-c("year","site","depth","transect","abundance")

### Mean SE coral per site and habitat
fish.mean<-do.call(data.frame,aggregate(lter.sum$abundance,list(lter.sum$year,lter.sum$site,lter.sum$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.mean)[1:5]<-c("year","site","depth","abund.mean","abund.se")


### Entire island (all sites)
# Mean SE fish abundance
fish.tot<-do.call(data.frame,aggregate(fish.mean$abund.mean,list(fish.mean$year,fish.mean$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.tot)[1:4]<-c("year","depth","ab.mean","ab.se")


### Graphs

# Island
p.tot<-ggplot(fish.tot)+
	geom_line(aes(x=year,y=ab.mean,group=depth),colour="deepskyblue2")+
	geom_errorbar(aes(x= year,y=ab.mean,ymin=ab.mean-ab.se,ymax=ab.mean+ab.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (XXX m2)")+
	facet_wrap(~depth,scales="free")+
	scale_y_continuous(limits=c(0,1000))+
scale_x_continuous(limits=c(2006,2024),breaks=c(2006,2012,2018,2024),labels=c(2006,2012,2018,2024))+
	ggtitle("Moorea LTER")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))



# Sie
p.tot<-ggplot(fish.mean[fish.mean$depth==2,])+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (XXX m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,1000))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea LTER 2")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

p.tot<-ggplot(fish.mean[fish.mean$depth==3,])+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (XXX m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,1000))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea LTER 3")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))


p.tot<-ggplot(fish.mean[fish.mean$depth==4,])+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (XXX m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,1000))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea LTER 4")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))
	
p.tot<-ggplot(fish.mean[fish.mean$depth==6,])+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (XXX m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,1000))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea LTER 6")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))
	
	
p.tot<-ggplot(fish.mean[fish.mean$depth==7,])+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (XXX m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,1000))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea LTER 7")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))

p.tot<-ggplot(fish.mean[fish.mean$depth==10,])+
	geom_line(aes(x=year,y=abund.mean,group=site),colour="deepskyblue2")+
	geom_errorbar(aes(x=year,y=abund.mean,ymin=abund.mean-abund.se,ymax=abund.mean+abund.se),colour="deepskyblue2",width=0)+
	xlab("") + ylab("Abundance (XXX m2)")+
	facet_wrap(~site,scales="free")+
	scale_y_continuous(limits=c(0,1500))+
scale_x_continuous(limits=c(2004,2024),breaks=c(2004,2014,2024),labels=c(2004,2014,2024))+
	ggtitle("Moorea LTER 10")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))
	
### Fish biomass
# Transform to number
lter.f$biomass<-as.numeric(as.character(lter.f$biomass))
# Remove -1
lter.f$biomass[lter.f$biomass==-1]<-NA

# Sum of biomass in both swath
# Total fish abundance per transect (per XXX m2: double-check with Peter)
lter.b.sum<-aggregate(lter.f$biomass,list(lter.f$year,lter.f$site,lter.f$depth,lter.f$transect),sum,na.rm=T)
names(lter.b.sum)<-c("year","site","depth","transect","biomass")

### Mean SE coral per site and habitat
fish.b.mean<-do.call(data.frame,aggregate(lter.b.sum$biomass,list(lter.b.sum$year,lter.b.sum$site,lter.b.sum$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.b.mean)[1:5]<-c("year","site","depth","biom.mean","biom.se")


### Entire island (all sites)
# Mean SE fish abundance
fish.b.tot<-do.call(data.frame,aggregate(fish.b.mean$biom.mean,list(fish.b.mean$year,fish.b.mean$depth),function(x) c(mean=mean(x),sd=std.error(x))))
names(fish.b.tot)[1:4]<-c("year","depth","bm.mean","bm.se")


### Graphs

# Island
p.tot<-ggplot(fish.b.tot)+
	geom_line(aes(x=year,y=bm.mean,group=depth),colour="deepskyblue")+
	geom_errorbar(aes(x= year,y=bm.mean,ymin=bm.mean-bm.se,ymax=bm.mean+bm.se),colour="deepskyblue",width=0)+
	xlab("") + ylab("Biomass (XXX g/m2)")+
	facet_wrap(~depth,scales="free")+
	scale_y_continuous(limits=c(0,40000))+
scale_x_continuous(limits=c(2006,2024),breaks=c(2006,2012,2018,2024),labels=c(2006,2012,2018,2024))+
	ggtitle("Moorea LTER")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position=c(0.08,0.85),legend.background=element_rect(fill=NA))


