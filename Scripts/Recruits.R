### Recruit data Moorea ###

setwd("/Volumes/MORITZ/Borchard/Data/CRIOBE")

library(vegan)
library(reshape)
library(reshape2)
library(plotrix)
library(ggplot2)
library(gridExtra)

###### CRIOBE ######

# Load table
rec<-read.table("Recruitment.csv",header=T,sep=";",dec=",",stringsAsFactor=T)
head(rec)
str(rec)

names(rec)<-tolower(names(rec))

levels(rec$site)
levels(rec$profondeur)

# Correct famille
levels(rec$famille)[3]<-"Acroporidae"

# New date levels
levels(rec$date.début)
levels(rec$date.début)<-c("2000-12-15","2001-12-15","2002-12-15","2003-12-15","2004-12-15","2005-12-15","2006-12-15","2007-12-15","2008-12-15","2009-12-15","2010-12-15","2011-12-15","2012-12-15","2013-12-15","2014-12-15","2015-12-15","2016-12-15","2017-12-15","2020-06-15","2024-05-15","2024-11-15","2001-09-15","2002-09-15","2003-09-15","2004-09-15","2005-09-15","2006-09-15","2007-09-15","2008-09-15","2009-09-15","2010-09-15","2011-09-15","2012-09-15","2013-09-15","2014-09-15","2015-09-15","2016-09-15","2017-09-15","2018-09-15","2019-09-15","2021-09-15")

levels(rec$date.début)<-sort(as.Date(levels(rec$date.début)))

# Keep only main families
rec<-rec[rec$famille %in% c("Acroporidae","Pocilloporidae","Poritidae"),]

### Sum by plate (dessus dessous)
rec.sum<-aggregate(rec$nombre,list(rec$date.début,rec$site,rec$profondeur,rec$plaque,rec$famille),sum,na.rm=T)
names(rec.sum)[1:6]<-c("year","site","depth","plate","family","number")

### Mean SE coral per site and depth
rec.moy.se<-do.call(data.frame,aggregate(rec.sum$number,list(rec.sum$year,rec.sum$site,rec.sum$depth,rec.sum$family),function(x) c(mean=mean(x),sd=std.error(x))))
names(rec.moy.se)[1:6]<-c("year","site","depth","family","rec.mean","rec.se")

### Entire island (all sites)
rec.tot<-do.call(data.frame,aggregate(rec.moy.se$rec.mean,list(rec.moy.se$year,rec.moy.se$depth,rec.moy.se$family),function(x) c(mean=mean(x),sd=std.error(x))))
names(rec.tot)[1:5]<-c("year","depth","family","rec.mean","rec.se")


### Graphs

# Island
p.tot<-ggplot(rec.tot)+
	geom_line(aes(x=year,y=rec.mean,group=family,colour=family))+
	geom_errorbar(aes(x=year,y=rec.mean,ymin=rec.mean-rec.se,ymax=rec.mean+rec.se),colour="purple4",width=0)+
	xlab("") + ylab("Number of recruits")+
	facet_wrap(~depth,scales="free",nrow=3)+
	scale_colour_manual(values=c("orchid3","purple4","royalblue3"))+
	scale_y_continuous(limits=c(0,11))+
	#scale_x_continuous(limits=c(2000,2024),breaks=c(2000,2004,2008,2012,2016,2020,2024),labels=c(2000,2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea Recruits CRIOBE")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="top",legend.background=element_rect(fill=NA))


# Site
p.tot<-ggplot(rec.moy.se[rec.moy.se$site=="Haapiti",])+
	geom_line(aes(x=year,y=rec.mean,group=family,colour=family))+
	geom_errorbar(aes(x=year,y=rec.mean,ymin=rec.mean-rec.se,ymax=rec.mean+rec.se),colour="purple4",width=0)+
	xlab("") + ylab("Number of recruits")+
	facet_wrap(~depth,scales="free",nrow=3)+
	scale_colour_manual(values=c("orchid3","purple4","royalblue3"))+
	scale_y_continuous(limits=c(0,11))+
	#scale_x_continuous(limits=c(2000,2024),breaks=c(2000,2004,2008,2012,2016,2020,2024),labels=c(2000,2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea Recruits Haapiti CRIOBE")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="top",legend.background=element_rect(fill=NA))

p.tot<-ggplot(rec.moy.se[rec.moy.se$site=="Tiahura",])+
	geom_line(aes(x=year,y=rec.mean,group=family,colour=family))+
	geom_errorbar(aes(x=year,y=rec.mean,ymin=rec.mean-rec.se,ymax=rec.mean+rec.se),colour="purple4",width=0)+
	xlab("") + ylab("Number of recruits")+
	facet_wrap(~depth,scales="free",nrow=3)+
	scale_colour_manual(values=c("orchid3","purple4","royalblue3"))+
	scale_y_continuous(limits=c(0,11))+
	#scale_x_continuous(limits=c(2000,2024),breaks=c(2000,2004,2008,2012,2016,2020,2024),labels=c(2000,2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea Recruits Tiahura CRIOBE")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="top",legend.background=element_rect(fill=NA))

p.tot<-ggplot(rec.moy.se[rec.moy.se$site=="Vaipahu",])+
	geom_line(aes(x=year,y=rec.mean,group=family,colour=family))+
	geom_errorbar(aes(x=year,y=rec.mean,ymin=rec.mean-rec.se,ymax=rec.mean+rec.se),colour="purple4",width=0)+
	xlab("") + ylab("Number of recruits")+
	facet_wrap(~depth,scales="free",nrow=3)+
	scale_colour_manual(values=c("orchid3","purple4","royalblue3"))+
	scale_y_continuous(limits=c(0,11))+
	#scale_x_continuous(limits=c(2000,2024),breaks=c(2000,2004,2008,2012,2016,2020,2024),labels=c(2000,2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea Recruits Vaipahu CRIOBE")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="top",legend.background=element_rect(fill=NA))


###### LTER ######

setwd("/Volumes/MORITZ/Borchard/Data/LTER")

# Load table
rec<-read.table("coral_recruit_tile_spat_counts_2006-2016_20180626.csv",header=T,sep=",",dec=".",stringsAsFactor=T)
head(rec)
str(rec)

levels(rec$season) # 2 seasons
levels(rec$shore) # 3 shores-sites
levels(rec$habitat) # 2 habitats-depth

# Keep only main families
rec<-rec[rec$family %in% c("Acroporidae","Pocilloporidae","Poritidae"),]

# Sum across seasons (= sum of recruits for each year)
# Sum across locations in each shore/habitat
# Sum across tiles (too low to do the mean...)
rec.sum<-aggregate(rec$count,list(rec$nominal_year,rec$shore,rec$habitat,rec$family),sum, na.rm=T)
names(rec.sum)<-c("year","shore","habitat","family","number")

### Mean SE coral per site and depth
rec.moy.se<-do.call(data.frame,aggregate(rec.sum$number,list(rec.sum$year,rec.sum$shore,rec.sum$habitat,rec.sum$family),function(x) c(mean=mean(x),sd=std.error(x))))
names(rec.moy.se)[1:6]<-c("year","shore","habitat","family","rec.mean","rec.se")

### Entire island (all sites)
rec.tot<-do.call(data.frame,aggregate(rec.moy.se$rec.mean,list(rec.moy.se$year,rec.moy.se$habitat,rec.moy.se$family),function(x) c(mean=mean(x),sd=std.error(x))))
names(rec.tot)[1:5]<-c("year","habitat","family","rec.mean","rec.se")


### Graphs

# Island
p.tot<-ggplot(rec.tot)+
	geom_line(aes(x=year,y=rec.mean,group=family,colour=family))+
	geom_errorbar(aes(x=year,y=rec.mean,ymin=rec.mean-rec.se,ymax=rec.mean+rec.se),colour="purple4",width=0)+
	xlab("") + ylab("Number of recruits")+
	facet_wrap(~habitat,scales="free",nrow=3)+
	scale_colour_manual(values=c("orchid3","purple4","royalblue3"))+
	scale_y_continuous(limits=c(0,500))+
	#scale_x_continuous(limits=c(2000,2024),breaks=c(2000,2004,2008,2012,2016,2020,2024),labels=c(2000,2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea Recruits LTER")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="top",legend.background=element_rect(fill=NA))

# Nothing in Lagoon east and west?
p.tot<-ggplot(rec.moy.se)+
	geom_line(aes(x=year,y=rec.mean,group=family,colour=family))+
	geom_errorbar(aes(x=year,y=rec.mean,ymin=rec.mean-rec.se,ymax=rec.mean+rec.se),colour="purple4",width=0)+
	xlab("") + ylab("Number of recruits")+
	facet_wrap(~habitat+shore,scales="free",nrow=3)+
	scale_colour_manual(values=c("orchid3","purple4","royalblue3"))+
	scale_y_continuous(limits=c(0,500))+
	#scale_x_continuous(limits=c(2000,2024),breaks=c(2000,2004,2008,2012,2016,2020,2024),labels=c(2000,2004,2008,2012,2016,2020,2024))+
	ggtitle("Moorea Recruits North LTER")+
	theme_classic(base_size=10)+
	theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),strip.background=element_blank(),strip.text.x=element_text(size=9),axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0,face="bold",size=10),legend.title=element_blank(),legend.position="top",legend.background=element_rect(fill=NA))


