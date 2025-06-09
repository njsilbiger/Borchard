### ATPP data Moorea ###

setwd("/Volumes/MORITZ/Borchard/Data/CRIOBE")


library(vegan)
library(reshape)
library(reshape2)
library(plotrix)
library(ggplot2)
library(gridExtra)

### Barrier reef ###

# Load table
atpp<-read.table("ATPPbarrierSubstrate.csv",header=T,sep=";",dec=",",stringsAsFactor=T)

head(atpp)
str(atpp)

# Remove unnecessary columns
atpp<-atpp[,-c(2,3,8)]

names(atpp)<-tolower(names(atpp))

# C: live coral
# P: CCA
# R: rubble 0.2-15 cm.
# T: turf
# A: macroalgae
# D: rock
# S: sand
# O: other

# One row with no substrate: NA
atpp[34884,5]<-NA

droplevels(atpp$substrate)

# Count number of points per substrate
sub.count<-aggregate(atpp$substrate,list(atpp$year,atpp$station,atpp$substrate),length)
names(sub.count)<-c("year","station","substrate","cover")

# Mean se across stations
sub.moy.se<-do.call(data.frame,aggregate(sub.count$cover,list(sub.count$year,sub.count$substrate),function(x) c(mean=mean(x),sd=std.error(x))))
names(sub.moy.se)[1:4]<-c("year","substrate","mean","se")

### Graphs

# Island
p.tot<-ggplot(sub.moy.se[sub.moy.se$substrate %in% c("C","A"),],aes(x=year,y=mean,colour=substrate))+
	geom_line()+
	geom_errorbar(aes(x=year,y=mean,ymin=mean-se,ymax=mean+se),width=0)+
	xlab("") + ylab("Cover (%)")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(1990,2024),breaks=c(1990,2000,2010,2020),labels=c(1990,2000,2010,2020))+
	scale_colour_manual(values=c("aquamarine3","salmon1"))+
	ggtitle("Moorea Barrier ATPP")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="none",legend.background=element_rect(fill=NA))

# Site
p.tot<-ggplot(sub.count[sub.count$substrate %in% c("C","A"),],aes(x=year,y=cover,colour=substrate))+
	geom_line()+
	xlab("") + ylab("Cover (%)")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(1990,2024),breaks=c(1990,2000,2010,2020),labels=c(1990,2000,2010,2020))+
	scale_colour_manual(values=c("aquamarine3","salmon1"))+
	facet_wrap(~station,scales="free")+
	ggtitle("Moorea Barrier ATPP")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="none",legend.background=element_rect(fill=NA))


### Fore reef ###

# Load table
atppo<-read.table("ATPPouterSubstrate.csv",header=T,sep=";",dec=",",stringsAsFactor=T)

head(atppo)
str(atppo)

# Only 1-2 transect in 2009: remove?...

# Replace NA by zeros
atppo[is.na(atppo)]<-0

# Sum coral and sum algae (including turf)
cor.alg<-data.frame(atppo[,c(2,4)],coral=rowSums(atppo[,5:26]),algae=rowSums(atppo[,28:33]))
names(cor.alg)[1]<-"year"

# Mean se across stations
cor.alg.moy.se<-do.call(data.frame,aggregate(list(cor.alg$coral,cor.alg$algae),list(cor.alg$year),function(x) c(mean=mean(x),sd=std.error(x))))
names(cor.alg.moy.se)[1:5]<-c("year","cor.mean","cor.se","alg.mean","alg.se")


### Graphs

# Island
p.tot<-ggplot(cor.alg.moy.se,aes(x=year,y=cor.mean,colour="salmon1"))+
	geom_line()+
	geom_errorbar(aes(x=year,y=cor.mean,ymin=cor.mean-cor.se,ymax=cor.mean+cor.se),width=0,colour="salmon1")+
	geom_line(aes(x=year,y=alg.mean),colour="aquamarine3")+
	geom_errorbar(aes(x=year,y=alg.mean,ymin=alg.mean-alg.se,ymax=alg.mean+alg.se),width=0,colour="aquamarine3")+
	xlab("") + ylab("Cover (%)")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(1990,2024),breaks=c(1990,2000,2010,2020),labels=c(1990,2000,2010,2020))+
	ggtitle("Moorea Fore Reef ATPP")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="none",legend.background=element_rect(fill=NA))

# Site
p.tot<-ggplot(cor.alg,aes(x=year,y=coral,colour="salmon1"))+
	geom_line()+
	geom_line(aes(x=year,y=algae),colour="aquamarine3")+
	xlab("") + ylab("Cover (%)")+
	scale_y_continuous(limits=c(0,100))+
	scale_x_continuous(limits=c(1990,2024),breaks=c(1990,2000,2010,2020),labels=c(1990,2000,2010,2020))+
	facet_wrap(~UE,scales="free")+
	ggtitle("Moorea Fore Reef ATPP")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="none",legend.background=element_rect(fill=NA))




