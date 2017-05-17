#############################################
#############################################
# Make figures and maps of US with swine CVI data
#############################################
#############################################
library(maps)
library(igraph)
library(plyr)
library(RColorBrewer)
library(plotrix)
library(SDMTools)
library(shape)
library(sp)

# NOTE TO SELF- WHEN REMAKE FINAL FIGURES, REMEMBER TO REMOVE WITHIN STATE SHIPMENTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

###############################################
#############################################
# Figure 1: Histogram of shipment size, overall
#############################################
###############################################
# read in data
setwd("~/Documents/post-doc/Swine")
# Read in data
data.cvi <- read.csv("Swine_cvi_final.csv")
data.cvi = data.cvi[!is.na(data.cvi$NUM_SWINE),]  #-1
data.cvi = data.cvi[data.cvi$NUM_SWINE > 0,] # -13
data.cvi = data.cvi[!is.na(data.cvi$SAMPLE_YEAR2),]  #-38 
data.cvi = data.cvi[data.cvi$NUM_SWINE > 0,]
summary(data.cvi)
colnames(data.cvi)

# make a new column of all ones that represents the number of shipments
data.cvi$MOVE <- 1

# subset datasheet
data.cvi <- data.cvi[, c("STATE", "SAMPLE_YEAR2", 
	"PURPOSE", "NUM_SWINE", "NUM_BOAR", 
    "NUM_BARROW", "NUM_GILT", "NUM_SOW", 
    "NUM_AGE_0.2_MONTHS", "NUM_AGE_2.6_MONTHS",
    "NUM_AGE_6._MONTHS", "NUM_MALE", "NUM_FEMALE",
    "D_STATE", "D_FIPS_X", "D_FIPS_Y",  
    "O_FIPS_X", "O_FIPS_Y", "O_STATE", 
    "D_FIPS", "O_FIPS", "O_ST_FIPS", "D_ST_FIPS", "MOVE" )]

# remove NAs    
data.cvi <- data.cvi[!is.na(data.cvi$O_FIPS),]
data.cvi <- data.cvi[!is.na(data.cvi$D_FIPS),]
 
# Subset cvi data by year.
data10 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2010",]
data11 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2011",]
data.cvi$MOVE <- 1
data10$MOVE <- 1
data11$MOVE <- 1

# make data.cvi without Nebraska
data2011red <- data11[data11$O_STATE != "NE",]
data <- rbind(data10, data11)

# Subset the data, naming it something you like...
alldata <- data$NUM_SWINE

# this function makes the histogram, and saves it to your working directory
my.histogram.maker<-function(data, filename){
  	swine.hist =  hist(data, plot=FALSE) 
	tiff(paste(filename, ".tiff", sep=""), 
		width=9, height=7, units="in", res=600)
	par(mar=c(6,6,4,2))
  	max.x = max(swine.hist$breaks)
  	max.y = max(swine.hist$counts)
  	barplot(swine.hist$counts, width = 1, space = 0, main = NULL, 
  		xlab="", ylim=c(0,max.y+10), ylab="", cex.lab = 1.5, 
  		cex.axis = 1.4, bty = "n", col = "darkseagreen", las = 1)
  	mtext("Number of shipments", side = 2, line = 4.4, cex = 1.6) 
	mtext("Number of swine", side = 1, line = 3, cex = 1.6) 
  	axis(side = 1, at = seq(1, (length(swine.hist$breaks)-1)), 
  		labels = swine.hist$breaks[-1], cex = 1.5)
  
  	# this part of the function plots the inset figure: 
  	swine.hist100 = hist(data[data < 100], 
  		breaks = seq(0, 100, 10), plot=FALSE)
	par(fig = c(0.45, 0.95, 0.45, 0.95), new = T, 
		mar = c(3, 2, 1.5, 1), lwd = 1, mgp = c(1, 0.25, 0))
  	max.x100 = max(swine.hist100$breaks)
  	max.y100 = max(swine.hist100$counts)
  	barplot(swine.hist100$counts, width = 1, space = 0, main = NULL, 
  		xlab = "", ylim = c(0, max.y100 + 10), 
  		cex.lab = 1.3, cex.axis = 1.2, bty = "n", 
  		col = "darkslateblue", las = 1, tcl = -0.2)
  	axis(side = 1, at = seq(1, (length(swine.hist100$breaks)-1)),
  		labels = swine.hist100$breaks[-1], cex.lab = 1.2, tcl = -0.2)
  	dev.off()
}

my.histogram.maker(alldata, "Figure1")


###############################################
###############################################
# Figure 2: Purpose/ Production type, made in prism
###############################################
###############################################
table(data.cvi$PURPOSE)
tapply(data.cvi$NUM_SWINE, data.cvi$PURPOSE, sum)

###############################################
###############################################
# Figure 3: Age and sex patterns, made in prism
###############################################
###############################################
# calculate sum of number of swine by year
tapply(data$NUM_MALE, data$SAMPLE_YEAR2, sum)
# 2010  2011 
#73096 14684 
tapply(data$NUM_FEMALE, data$SAMPLE_YEAR2, sum)
# 2010  2011 
#30066 31925 

# calculate sum of number of swine by year
tapply(data$NUM_AGE_0.2_MONTHS, data$SAMPLE_YEAR2, sum)
# 2010  2011 
#1182595 1340820  
tapply(data$NUM_AGE_2.6_MONTHS, data$SAMPLE_YEAR2, sum)
# 2010  2011 
#312329 313963 
tapply(data$NUM_AGE_6._MONTHS, data$SAMPLE_YEAR2, sum)
# 2010  2011 
#14050 21231 

# calculates sum of number of gilt, boar, barrow ect. 
tapply(data$NUM_BOAR, data$SAMPLE_YEAR2, sum)
#2010 2011 
# 301 2110 
tapply(data$NUM_BARROW, data$SAMPLE_YEAR2, sum)
#2010  2011 
# 8358 15831 
tapply(data$NUM_GILT, data$SAMPLE_YEAR2, sum)
# 2010  2011 
#57820 40374 
tapply(data$NUM_SOW, data$SAMPLE_YEAR2, sum)
#2010 2011 
# 498  237 

# see summarystats fig 1,2,&3 for wrongness evaluation
data$totalsex <- data$NUM_MALE + data$NUM_FEMALE +
	data$NUM_FEMALE2 + data$NUM_MALE2
data$wrongness <- data$NUM_SWINE - data$totalsex
# still many missing
data$totalfem <- data$NUM_FEMALE + data$NUM_FEMALE2
tapply(data$totalfem, data$SAMPLE_YEAR2, sum)
data$totalmale <- data$NUM_MALE + data$NUM_MALE2
tapply(data$totalmale, data$SAMPLE_YEAR2, sum)
#female
# 2010  2011 
#88384 72536 
#male
# 2010  2011 
#81755 32625 

###############################################
###############################################
# Figure S1: Plot of GSCC, GWCC at the county scale
###############################################
###############################################
# Read in GWCC, GSCC identity. 
node.stats <- read.csv("~/Documents/post-doc/Swine/node_stats_2010.csv")
node.stats2011 <- read.csv("~/Documents/post-doc/Swine/node_stats_2011all.csv")
node.stats2011none <- read.csv("~/Documents/post-doc/Swine/node_stats_2011noNE.csv")

data(county.fips)
node.stats$COUNTY_NAME_R <- county.fips$polyname[
	match(node.stats$NodeID, county.fips$fips)]
node.stats2011$COUNTY_NAME_R <- county.fips$polyname[
	match(node.stats2011$NodeID, county.fips$fips)]
node.stats2011none$COUNTY_NAME_R <- county.fips$polyname[
	match(node.stats2011none$NodeID, county.fips$fips)]

ctname <- map('county', resolution=0, plot=FALSE)$names
ctname <- as.matrix(ctname)
name <- data.frame(ctname = ctname, GSCC2010 = NA, 
	GWCC2010 = NA, GSCC2011 = NA, GWCC2011 = NA, 
	GSCC2011none = NA, GWCC2011none = NA)
# not necessary #######
name$GSCC2010 <-node.stats$StrongClusters[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$GWCC2010<-node.stats$WeakClusters[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$GSCC2010[is.na(name$GSCC2010)]<-0
name$GWCC2010[is.na(name$GWCC2010)]<-0
name$GSCC2011 <-node.stats2011$StrongClusters[match(
	name$ctname, node.stats2011$COUNTY_NAME_R)]
name$GWCC2011<-node.stats2011$WeakClusters[match(
	name$ctname, node.stats2011$COUNTY_NAME_R)]
name$GSCC2011[is.na(name$GSCC2011)]<-0
name$GWCC2011[is.na(name$GWCC2011)]<-0
name$GSCC2011none <-node.stats2011none$StrongClusters[match(
	name$ctname, node.stats2011none$COUNTY_NAME_R)]
name$GWCC2011none<-node.stats2011none$WeakClusters[match(
	name$ctname, node.stats2011none$COUNTY_NAME_R)]
name$GSCC2011none[is.na(name$GSCC2011none)]<-0
name$GWCC2011none[is.na(name$GWCC2011none)]<-0
############
# note: 3 nodes did not transfer from node-stats to name. 

# set up color, so not in either GSCC or GWCC is gray; 
# no data=white; GSCC is light blue; GWCC is dark blue
cols <- colorRampPalette(brewer.pal(9, "PuBu"))(7)   # colors for level plots
colmatch<-data.frame(num=seq(0,7,1), col=c("white", cols))
plot(1:8, 1:8, col= cols, pch=19, cex=2)   
# want cols[7] for GSCC; cols[3] or 4 for weakly connected; cols[2] or light gray for nas. 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# DEFINE COLORS FOR 2010
clusters=matrix(0,nrow=length(ctname),ncol=2)
ctname<-as.character(ctname)
  for(i in 1:length(node.stats$NodeID)){
    temp=which(county.fips$fips==node.stats$NodeID[i])
    temp2=which(ctname== county.fips[temp,2])
    clusters[temp2,1]=rep(node.stats[i,"StrongClusters"],length(temp2))
    clusters[temp2,2]=rep(node.stats[i,"WeakClusters"],length(temp2))
  }  
  	clusters[clusters==0]=NA  # if no info cluster assigned, na= no data.   
  	clusters.new=matrix(0,nrow=length(ctname),ncol=1)
	clusters.new[clusters[,1]==as.numeric(Mode(na.exclude(clusters[,1])))]=cols[7]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,
		2]==as.numeric(Mode(na.exclude(clusters[,2]))))]=cols[4]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,2]!=as.numeric(Mode(na.exclude(clusters[,2]))) & !is.na(clusters[,1]))]="lightskyblue1"
	clusters.new[is.na(clusters[,1])]="gray93"

name$color2010<-clusters.new

# DEFINE COLORS FOR 2011, node.stats2011
clusters=matrix(0,nrow=length(ctname),ncol=2)
ctname<-as.character(ctname)
  for(i in 1:length(node.stats2011$NodeID)){
    temp=which(county.fips$fips== node.stats2011$NodeID[i])
    temp2=which(ctname== county.fips[temp,2])
    clusters[temp2,1]=rep(node.stats2011[i,"StrongClusters"],length(temp2))
    clusters[temp2,2]=rep(node.stats2011[i,"WeakClusters"],length(temp2))
  }    
  	clusters[clusters==0]=NA  # if no info cluster assigned, na= no data.   
  	clusters.new=matrix(0,nrow=length(ctname),ncol=1)
	clusters.new[clusters[,1]==as.numeric(Mode(na.exclude(clusters[,1])))]=cols[7]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,
		2]==as.numeric(Mode(na.exclude(clusters[,2]))))]=cols[4]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,2]!=as.numeric(Mode(na.exclude(clusters[,2]))) & !is.na(clusters[,1]))]="lightskyblue1"
	clusters.new[is.na(clusters[,1])]="gray93"

name$color2011<-clusters.new

# DEFINE COLORS FOR 2011, node.stats2011
clusters=matrix(0,nrow=length(ctname),ncol=2)
ctname<-as.character(ctname)
  for(i in 1:length(node.stats2011none$NodeID)){
    temp=which(county.fips$fips== node.stats2011none$NodeID[i])
    temp2=which(ctname== county.fips[temp,2])
    clusters[temp2,1]=rep(node.stats2011none[i,"StrongClusters"],length(temp2))
    clusters[temp2,2]=rep(node.stats2011none[i,"WeakClusters"],length(temp2))
  }  
  	clusters[clusters==0]=NA  # if no info cluster assigned, na= no data.   
  	clusters.new=matrix(0,nrow=length(ctname),ncol=1)
	clusters.new[clusters[,1]==as.numeric(Mode(na.exclude(clusters[,1])))]=cols[7]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,
		2]==as.numeric(Mode(na.exclude(clusters[,2]))))]=cols[4]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,2]!=as.numeric(Mode(na.exclude(clusters[,2]))) & !is.na(clusters[,1]))]="lightskyblue1"
	clusters.new[is.na(clusters[,1])]="gray93"

name$color2011none<-clusters.new

# 2010 map
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/GSCC&GWCC_2010.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('county', resolution=0, lwd=0.2, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$color2010, boundary="light gray", lwd=0.2, add=TRUE)
map('state', resolution=0, add=TRUE, fill=FALSE, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="black", add=TRUE, lwd=1.5)
dev.off()
#2011all
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/GSCC&GWCC_2011.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('county', resolution=0, lwd=0.2, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$color2011, boundary="light gray", lwd=0.2, add=TRUE)
map('state', resolution=0, add=TRUE, fill=FALSE, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="black", add=TRUE, lwd=1.5)
dev.off()
#2011none
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/GSCC&GWCC_2011none.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('county', resolution=0, lwd=0.2, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$color2011none, boundary="light gray", lwd=0.2, add=TRUE)
map('state', resolution=0, add=TRUE, fill=FALSE, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="black", add=TRUE, lwd=1.5)
dev.off()

# summary of GWCC and GSCC size
length(node.stats$StrongClusters[node.stats$StrongClusters==395])/length(node.stats$StrongClusters)# 80/total
length(node.stats2011$StrongClusters[node.stats2011$StrongClusters==473])/length(node.stats2011$StrongClusters) # 107/total= 13.19%
length(node.stats$WeakClusters[node.stats$WeakClusters==2])/length(node.stats$WeakClusters)
length(node.stats2011$WeakClusters[node.stats2011$WeakClusters==2])/length(node.stats2011$WeakClusters)
# summary of GWCC and GSCC size in states with data
datastates<-c(19, 6, 27, 31, 36, 37, 48, 55)
datastatesnoNE<-c(19, 6, 27, 36, 37, 48, 55)
node.stats2010sub<-node.stats[node.stats$StateID %in% datastatesnoNE,]
node.stats2011sub<-node.stats2011[node.stats2011$StateID %in% datastates,]
node.stats2011nonesub<-node.stats2011none[node.stats2011none$StateID %in% datastates,]

length(node.stats2010sub$StrongClusters[node.stats2010sub$StrongClusters==395])/length(node.stats2010sub$StrongClusters)  # 80/total=24.09
length(node.stats2011sub$StrongClusters[node.stats2011sub$StrongClusters== 473])/length(node.stats2011sub$StrongClusters)  # 26.81%
length(node.stats2010sub$WeakClusters[node.stats2010sub$WeakClusters==2])/length(node.stats2010sub$WeakClusters)  # 0.9398
length(node.stats2011sub$WeakClusters[node.stats2011sub$WeakClusters==2])/length(node.stats2011sub$WeakClusters)  #0.9423

# ASSORTIVITY IN SUBSET: 
test<-read.csv("node_stats_sub2011all.csv")
test<-test[!is.na(test$AveNearNeighDeg),]
cor(test$AveNearNeighDeg, test$Unweighted_TotalDegree) #0.12

test<-read.csv("node_stats_sub2011noNE.csv")
test<-test[!is.na(test$AveNearNeighDeg),]
cor(test$AveNearNeighDeg, test$Unweighted_TotalDegree)  # 0.13

test<-read.csv("node_stats_sub2010.csv")
test<-test[!is.na(test$AveNearNeighDeg),]
cor(test$AveNearNeighDeg, test$Unweighted_TotalDegree)

# STATE ASSORTIVITY IN THE SUBSET
st_node.stats2010<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2010.csv")
st_node.stats2011<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2011all.csv")
st_node.stats2011none<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2011noNE.csv")
st_node.stats2010<-st_node.stats2010[st_node.stats2010$NodeID %in% datastatesnoNE,]
st_node.stats2011<-st_node.stats2011[st_node.stats2011$NodeID %in% datastates,]
st_node.stats2011none<-st_node.stats2011none[st_node.stats2011none$NodeID %in% datastatesnoNE,]


###############################################
###############################################
# Figure 4: Flows map
###############################################
###############################################
##########################
# 2010
##########################
# add flows through state centroids
data2010<-data10
##make a vector of all pairs
edge<-paste(as.character(data2010$O_ST_FIPS),as.character(data2010$D_ST_FIPS),sep='.u.')
##make a vector of unique pairs
edge1<-unique(paste(as.character(data2010$O_ST_FIPS),as.character(data2010$D_ST_FIPS),sep='.u.'))

##censor the edges list if both vertices are the same
for(i in 1:length(edge1)){
  if(sum(paste(strsplit(edge1[i],'.u.')[[1]][2],strsplit(edge1[i],'.u.')[[1]][1],sep='.u.')==edge1)>0){
		edge2<-edge1[-which(paste(strsplit(edge1[i],'.u.')[[1]][2],strsplit(edge1[i],'.u.')[[1]][1],sep='.u.')==edge1)]
		}
	}

# HAVE NOT YET FIGURED OUT HOW TO REMOVE FORWARD AND REVERSE EDGES- IF 
##censor the edges list if both vertices are the same
#for(i in 1:length(edge2)){
#	for (j in 1:length(edge1)){
#  if(sum(paste(strsplit(edge2[i],'.u.')[[1]][2],strsplit(edge2[i],'.u.')[[1]][1],sep='.u.')== edge1[j], 
#  	paste(strsplit(edge2[i],'.u.')[[1]][1],strsplit(edge2[i],'.u.')[[1]][2],sep='.u.')== edge1[j])>1){
#		edge3<-edge2[-which(paste(strsplit(edge2[i],'.u.')[[1]][2],strsplit(edge2[i],'.u.')[[1]][1],sep='.u.')== edge2)]
#		}
#	}
#	}
	
##censor the edges list if both vertices are the same but in reverse order

##use the list of unique vertex pairs to tabulate their frequency
edge2010<-data.frame(edge=as.character(edge2), node1=NA, node2=NA, unique=NA, N=0, swine=0)	
	for (i in 1:length(edge2010[,1])){
	edge2010$node1[i]=as.character(strsplit(as.character(edge2010$edge[i]), '.u.')[[1]][1])
	edge2010$node2[i]=strsplit(as.character(edge2010$edge[i]), '.u.')[[1]][2]
}

temp<-NA
for (i in 1:length(edge2010[,1])){
	temp[i]<-sum(which(paste(edge2010$node2[i], edge2010$node1[i], sep='.u.')== as.character(edge2010$edge)))
	ifelse(temp[i]==0, edge2010$unique[i]<-TRUE, edge2010$unique[i]<-FALSE)
}
data2010$NUM_SWINE<-as.numeric(data2010$NUM_SWINE)

# SUM the total number of shipmetns between states; sums, both directions, change this to make for just out or in degree. 
# NOTE: still have OSTATE.u.DSTATE and DSTATE.u.OSTATE when they are not unique, but they have the same values of N.
for (i in 1: length(data2010[,1])){
	for (j in 1:length(edge2010[,1])){
		if (
		paste(as.character(data2010$O_ST_FIPS[i]), 
		as.character(data2010$D_ST_FIPS[i]),sep='.u.')==
		as.character(edge2010$edge[j]) || paste(as.character(data2010$D_ST_FIPS[i]), 
		as.character(data2010$O_ST_FIPS[i]),sep='.u.')== as.character(edge2010$edge[j]))
		edge2010$N[j]<- edge2010$N[j]+1
		edge2010$swine[j]<- edge2010$swine[j] + data2010$NUM_SWINE[i]		
	}
}	


data(state.fips)
# add latitude and longitude and plot
matchdata<-read.csv("county_centroid_coordinates.csv")
matchdata<-matchdata[!is.na(matchdata$FIPS),]
matchdata$state2<-tolower(matchdata$state)
#statecoords<-read.csv("state_latlon.csv") # funky coordinates, old
statecoords<-read.table("~/Documents/post-doc/2010 Cattle Movement Practice/state_centroid_coordinates.txt", header=TRUE, sep=",")  # same projection
# instead use values in the maps package

library(ggplot2)
getLabelPoint<-
	function(state){Polygon(state[c('long', 'lat')])@labpt}
df<-map_data('state')
centroids<-by(df, df$region, getLabelPoint)             # returns list of labelpoints
centroids<-do.call("rbind.data.frame", centroids)  # convert to a Data frame
names(centroids) <-c('long', 'lat')	
centroids$names<-rownames(centroids)

# check: states washington, virginia, north carolina, new york, michigan are funky. 
#map('state')
#text(centroids$long, centroids$lat, rownames(centroids), offset=0, cex=0.4)
centroids$lat[centroids$names=="washington"]<-statecoords$latitude[statecoords$state=="Washington"]
#centroids$long[centroids$names=="washington"]<-statecoords$longitude[statecoords$state=="Washington"]	
centroids$lat[centroids$names=="virginia"]<-statecoords$latitude[statecoords$state=="Virginia"]
centroids$long[centroids$names=="virginia"]<-statecoords$longitude[statecoords$state=="Virginia"]	
centroids$lat[centroids$names=="north carolina"]<-statecoords$latitude[statecoords$state=="North Carolina"]
centroids$long[centroids$names=="north carolina"]<-statecoords$longitude[statecoords$state=="North Carolina"]	
centroids$lat[centroids$names=="new york"]<-statecoords$latitude[statecoords$state=="New York"]
centroids$long[centroids$names=="new york"]<-statecoords$longitude[statecoords$state=="New York"]	
centroids$lat[centroids$names=="michigan"]<-statecoords$latitude[statecoords$state=="Michigan"]
centroids$long[centroids$names=="michigan"]<-statecoords$longitude[statecoords$state=="Michigan"]	
centroids$lat[centroids$names=="new york"]<-42.8
centroids$lat[centroids$names=="massachusetts"]<-42.45
map('state')
text(centroids$long, centroids$lat, rownames(centroids), offset=0, cex=0.4)

matchdata$Rlatitude<-centroids$lat[match(matchdata$state2, rownames(centroids))]
matchdata$Rlongitude<-centroids$long[match(matchdata$state2, rownames(centroids))]

edge2010$Olatitude<- matchdata$Rlatitude[match(edge2010$node1, matchdata$ST_FIPS)]
edge2010$Dlatitude<- matchdata$Rlatitude[match(edge2010$node2, matchdata$ST_FIPS)]
edge2010$Olongitude<- matchdata$Rlongitude[match(edge2010$node1, matchdata$ST_FIPS)]
edge2010$Dlongitude<- matchdata$Rlongitude[match(edge2010$node2, matchdata$ST_FIPS)]
edge2010<-edge2010[-113,]   # remove row 113, with a shipment from Cali to Hawaii...


# get colors based on strong and weak components for map
node.stats<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2010.csv")
node.stats2011<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2011all.csv")
node.stats2011none<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2011noNE.csv")

node.stats$STATE_NAME_R<-state.fips$polyname[match(node.stats$NodeID, state.fips$fips)]
node.stats2011$STATE_NAME_R<-state.fips$polyname[match(node.stats2011$NodeID, state.fips$fips)]
node.stats2011none$STATE_NAME_R<-state.fips$polyname[match(node.stats2011none$NodeID, state.fips$fips)]

ctname<-map('state', resolution=0, plot=FALSE)$names
ctname<-as.matrix(ctname)
name<-data.frame(ctname=ctname, GSCC2010=NA, GWCC2010=NA, GSCC2011=NA, GWCC2011=NA, GSCC2011none=NA, GWCC2011none=NA)

cols <- colorRampPalette(brewer.pal(9, "PuBu"))(7)   # colors for level plots

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# DEFINE COLORS FOR 2010
clusters=matrix(0,nrow=length(ctname),ncol=2)
ctname<-as.character(ctname)
  for(i in 1:length(node.stats$NodeID)){
    temp=which(state.fips$fips==node.stats$NodeID[i])
    temp2=which(ctname== as.character(state.fips[temp,6]))
    clusters[temp2,1]=rep(node.stats[i,"StrongClusters"],length(temp2))
    clusters[temp2,2]=rep(node.stats[i,"WeakClusters"],length(temp2))
  }  
  	clusters[clusters==0]=NA  # if no info cluster assigned, na= no data.   
  	clusters.new=matrix(0,nrow=length(ctname),ncol=1)
	clusters.new[clusters[,1]==as.numeric(Mode(na.exclude(clusters[,1])))]=cols[7]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,
		2]==as.numeric(Mode(na.exclude(clusters[,2]))))]=cols[4]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,2]!=as.numeric(Mode(na.exclude(clusters[,2]))) & !is.na(clusters[,1]))]="lightskyblue1"
	clusters.new[is.na(clusters[,1])]="gray93"

name$color2010<-clusters.new
edge2010<-edge2010[order(edge2010$N),]
#NY, NC, VA, CT should all be light blue.  
name$color2010[34:37]<-cols[7]  #NY 
name$color2010[38:40]<-cols[7] # NC
name$color2010[53:55]<-cols[4]		#VA
name$color2010[6]<-cols[4]		#CT



# 2010 map, with lines and state-level GWCC
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/GSCC&GWCC_st_lines_2010.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('state', resolution=0, fill=TRUE, col=name$color2010, boundary="dark gray", lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="black", add=TRUE, lwd=1.5)
N<-(log(edge2010$N+1)-0.5)*1.6
for (i in 1:150){	
lines(x=c(edge2010$Olongitude[i], edge2010$Dlongitude[i]),
      y=c(edge2010$Olatitude[i], edge2010$Dlatitude[i]), lwd=N[i], col=rgb(100, 100, 100, alpha=round(log((edge2010$N[i]+1), base=1.8)*5+192), max=255))
}
#alpha=log((edge2010$N[i]+1), base=1.8)*5+192
dev.off()


############################
# BETTER: FOR KATIE
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/flowmapall_st_lines_2010.tiff",
 width = 140, height = 90, units = "mm", res=600, compression="lzw")
 par(mai=c(1,1,1,1))
 map('state', resolution=0, fill=TRUE, col="white", boundary="dark gray", lwd=0.5)
 map('state', region=c("Iowa", "Texas", "California", 
     "Minnesota", "New York", "North Carolina", "Wisconsin"),
     resolution=0, col="black", add=TRUE, lwd=1.5)
 N<-(log(edge2010$N+1, base=5)-0.6)*1.8
 for (i in 1:length(edge2010[,1])){	
 lines(x=c(edge2010$Olongitude[i], edge2010$Dlongitude[i]),
       y=c(edge2010$Olatitude[i], edge2010$Dlatitude[i]), lwd=N[i], col="gray44")
 }
 dev.off()



#################
# 2011
#################
# add flows through state centroids
data2011<-data11
##make a vector of all pairs
edge<-paste(as.character(data2011$O_ST_FIPS),as.character(data2011$D_ST_FIPS),sep='.u.')
##make a vector of unique pairs
edge1<-unique(paste(as.character(data2011$O_ST_FIPS),as.character(data2011$D_ST_FIPS),sep='.u.'))

##censor the edges list if both vertices are the same
for(i in 1:length(edge1)){
  if(sum(paste(strsplit(edge1[i],'.u.')[[1]][2],strsplit(edge1[i],'.u.')[[1]][1],sep='.u.')==edge1)>0){
		edge2<-edge1[-which(paste(strsplit(edge1[i],'.u.')[[1]][2],strsplit(edge1[i],'.u.')[[1]][1],sep='.u.')==edge1)]
		}
	}

##use the list of unique vertex pairs to tabulate their frequency
edge2011<-data.frame(edge=as.character(edge2), node1=NA, node2=NA, unique=NA, N=0, swine=0)	
	for (i in 1:length(edge2011[,1])){
	edge2011$node1[i]=as.character(strsplit(as.character(edge2011$edge[i]), '.u.')[[1]][1])
	edge2011$node2[i]=strsplit(as.character(edge2011$edge[i]), '.u.')[[1]][2]
}

temp<-NA
for (i in 1:length(edge2011[,1])){
	temp[i]<-sum(which(paste(edge2011$node2[i], edge2011$node1[i], sep='.u.')== as.character(edge2011$edge)))
	ifelse(temp[i]==0, edge2011$unique[i]<-TRUE, edge2011$unique[i]<-FALSE)
}

for (i in 1: length(data2011[,1])){
	for (j in 1:length(edge2011[,1])){
		if (
		paste(as.character(data2011$O_ST_FIPS[i]),as.character(data2011$D_ST_FIPS[i]),sep='.u.')==
		as.character(edge2011$edge[j]) || paste(as.character(data2011$D_ST_FIPS[i]), 
		as.character(data2011$O_ST_FIPS[i]),sep='.u.')== as.character(edge2011$edge[j]))
		edge2011$N[j]<- edge2011$N[j]+1
		edge2011$swine[j]<- edge2011$swine[j] + data2011$NUM_SWINE[i]		
	}
}	

# add in centroid data for flows from centroids and matchdata compiled for 2010 above
edge2011$Olatitude<-NA; edge2011$Dlatitude<-NA
edge2011$Olatitude<- matchdata$Rlatitude[match(edge2011$node1, matchdata$ST_FIPS)]
edge2011$Dlatitude<- matchdata$Rlatitude[match(edge2011$node2, matchdata$ST_FIPS)]
edge2011$Olongitude<- matchdata$Rlongitude[match(edge2011$node1, matchdata$ST_FIPS)]
edge2011$Dlongitude<- matchdata$Rlongitude[match(edge2011$node2, matchdata$ST_FIPS)]

# check for strange origin locations?
#edge2011<-edge2011[-21,]   # remove row 113, with a shipment from Cali to Hawaii (FIPS=15)...
edge2011<-edge2011[edge2011$node2!=15,]
edge2011<-edge2011[edge2011$node2!=2,]

# get colors based on strong and weak components for map, loaded above in 2010 figure
#node.stats<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2010.csv")
#node.stats2011<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2011all.csv")
#node.stats2011none<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2011noNE.csv")

#node.stats$STATE_NAME_R<-state.fips$polyname[match(node.stats$NodeID, state.fips$fips)]
#node.stats2011$STATE_NAME_R<-state.fips$polyname[match(node.stats2011$NodeID, state.fips$fips)]
#node.stats2011none$STATE_NAME_R<-state.fips$polyname[match(node.stats2011none$NodeID, state.fips$fips)]

###########################
###########################
# skip this part
###########################
###########################

ctname<-map('state', resolution=0, plot=FALSE)$names
ctname<-as.matrix(ctname)
name<-data.frame(ctname=ctname, GSCC2010=NA, GWCC2010=NA, GSCC2011=NA, GWCC2011=NA, GSCC2011none=NA, GWCC2011none=NA)

cols <- colorRampPalette(brewer.pal(9, "PuBu"))(7)   # colors for level plots

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# DEFINE COLORS FOR 2011
clusters=matrix(0,nrow=length(ctname),ncol=2)
ctname<-as.character(ctname)
  for(i in 1:length(node.stats2011$NodeID)){
    temp=which(state.fips$fips==node.stats2011$NodeID[i])
    temp2=which(ctname== as.character(state.fips[temp,6]))
    clusters[temp2,1]=rep(node.stats2011[i,"StrongClusters"],length(temp2))
    clusters[temp2,2]=rep(node.stats2011[i,"WeakClusters"],length(temp2))
  }  
  	clusters[clusters==0]=NA  # if no info cluster assigned, na= no data.   
  	clusters.new=matrix(0,nrow=length(ctname),ncol=1)
	clusters.new[clusters[,1]==as.numeric(Mode(na.exclude(clusters[,1])))]=cols[7]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,
		2]==as.numeric(Mode(na.exclude(clusters[,2]))))]=cols[4]
	clusters.new[(clusters[,1]!=as.numeric(Mode(na.exclude(clusters[,1]))) & clusters[,2]!=as.numeric(Mode(na.exclude(clusters[,2]))) & !is.na(clusters[,1]))]="lightskyblue1"
	clusters.new[is.na(clusters[,1])]="gray93"

name$color2011<-clusters.new
edge2011<-edge2011[order(edge2011$N),]
#NY, NC, VA, CT should all be light blue.  
name$color2011[34:37]<-cols[7]  #NY 
name$color2011[38:40]<-cols[7] # NC
name$color2011[53:55]<-cols[4]		#VA
name$color2011[6]<-cols[4]		#CT


# 2011 map, with lines and state-level GWCC
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/GSCC&GWCC_st_lines_2011.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('state', resolution=0, fill=TRUE, col=name$color2011, boundary="dark gray", lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="black", add=TRUE, lwd=1.5)
N<-(log(edge2011$N+1)-0.5)*1.6
for (i in 1:150){	
lines(x=c(edge2011$Olongitude[i], edge2011$Dlongitude[i]),
      y=c(edge2011$Olatitude[i], edge2011$Dlatitude[i]), lwd=N[i], 
      col=rgb(100, 100, 100, max=255, alpha=round(log((edge2011$N[i]+1), base=1.8)*4+220)))
}
#alpha=log((edge2010$N[i]+1), base=1.8)*5+192
#alpha=round(log((edge2011$N[i]+1), base=1.8)*4+192)
dev.off()

##############################
# Map for Katie
##############################
############################
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/flowmapall_st_lines_2011.tiff",
 width = 140, height = 90, units = "mm", res=600, compression="lzw")
 par(mai=c(1,1,1,1))
 map('state', resolution=0, fill=TRUE, col="white", boundary="dark gray", lwd=0.5)
 map('state', region=c("Iowa", "Texas", "California", 
     "Minnesota", "New York", "North Carolina", "Wisconsin", "Nebraska"),
     resolution=0, col="black", add=TRUE, lwd=1.5)
 N<-(log(edge2011$N+1, base=5)-0.4)*1.8
 for (i in 1:length(edge2011[,1])){	
 lines(x=c(edge2011$Olongitude[i], edge2011$Dlongitude[i]),
       y=c(edge2011$Olatitude[i], edge2011$Dlatitude[i]), lwd=N[i], col="gray44")
 }
 dev.off()



temp<-data2011[data2011$O_STATE_NAME_FIPS=="California",]
table(temp$D_STATE_NAME_FIPS)



##################################
##################################
# plot WEIGHTED outdegree by county- data= cvi data 
##################################
##################################
data2010<-data[data$SAMPLE_YEAR=="2010",]
data2011<-data[data$SAMPLE_YEAR=="2011",]

# make networks
counties=unique(cbind(c(data2010$O_ST_FIPS, data2010$D_ST_FIPS), c(data2010$O_FIPS, data2010$D_FIPS)))  
counties=counties[order(counties[,2]),]
 node.stats=data.frame(matrix(NA,nrow=length(counties[,1]),
                             ncol=12,
                             dimnames=list(NULL, c("StateID","NodeID",
                             "Unweighted_InDeg","InDegree_Ship","InDegree_Swine", 							 							"Unweighted_OutDeg","OutDegree_Ship","OutDegree_Swine",
 							"TotalDegree_Ship", "TotalDegree_Swine", "Betweenness",
 							"Transitivity"))))	
node.stats$NodeID=counties[,2]
node.stats$StateID=counties[,1]
# It is good practice to watch what you do as you go.  Type summary to see what was done. 
summary(node.stats)  

temp_graph1=graph.edgelist(el=as.matrix(cbind(as.character(data2010$O_FIPS), as.character(data2010$D_FIPS))), directed=TRUE)
temp_graph2=graph.edgelist(el=as.matrix(unique(cbind(as.character(data2010$O_FIPS), as.character(data2010$D_FIPS)))), directed=TRUE)         #  not weighted
temp_graph<-set.edge.attribute(temp_graph1,"weight",value=data2010$NUM_SWINE)
                    										
temp_graph_st=graph.edgelist(el=as.matrix(cbind(as.character(data2010$O_ST_FIPS), as.character(data2010$D_ST_FIPS))), directed=TRUE)
# Calculate node statistics  
	node.stats$Unweighted_InDeg=degree(temp_graph2,mode=c("in"))[order(as.numeric(V(temp_graph2)$name))]
	node.stats$Unweighted_OutDeg=degree(temp_graph2,mode=c("out"))																							[order(as.numeric(V(temp_graph2)$name))]
    node.stats$InDegree_Ship=degree(temp_graph,mode=c("in"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Ship=degree(temp_graph,mode=c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Ship=node.stats$InDegree_Ship+node.stats$OutDegree_Ship
    node.stats$InDegree_Swine=graph.strength(temp_graph,mode=c("in"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Swine=graph.strength(temp_graph,mode=c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Swine=node.stats$InDegree_Swine+node.stats$OutDegree_Swine
    node.stats$Betweenness=betweenness(temp_graph2)[order(as.numeric(V(temp_graph2)$name))] 
	node.stats$Transitivity=transitivity(temp_graph,type=c("local"))[order(as.numeric(V(temp_graph)$name))]

#plot

ctname<-map('county', resolution=0, plot=FALSE)$names
ctname<-as.matrix(ctname)
data(county.fips)
node.stats$COUNTY_NAME_R<-county.fips$polyname[match(node.stats$NodeID, county.fips$fips)]
name<-data.frame(ctname=ctname, OutDegreeShip=NA, OutDegreeSwine=NA)
name$OutDegreeShip<-node.stats$OutDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeSwine<-node.stats$OutDegree_Swine[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeShip[is.na(name$OutDegreeShip)]<-0
name$OutDegreeSwine[is.na(name$OutDegreeSwine)]<-0
# try two color schemes... since shipments range from 0 to 172.  

# 1) range of one color
cols <- colorRampPalette(brewer.pal(9, "YlGnBu"))(172)   # colors for level plots
colmatch<-data.frame(num=seq(1,172,1), col=cols)
name$col<-as.character(colmatch$col[match(name$OutDegreeShip, colmatch$num)])
name$col[is.na(name$col)]<-"#FFFFFF"

par(mai=c(1,1,1,5))
map('county', resolution=0, lwd=0.5, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$col, boundary="light gray", lwd=0.5, add=TRUE)
map('state', add=TRUE)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="dark blue", add=TRUE, lwd=2)
leg.txt<-rep("", 172); leg.txt[1]<-1; leg.txt[172]<-172
color.legend(11, 6, 11.8, 9, leg.txt, rect.col=cols)



# on log scale
# 1) range of one color
cols <- colorRampPalette(brewer.pal(9, "YlGnBu"))(6)   # colors for level plots
colmatch<-data.frame(num=seq(0,6,1), col=c("white", cols))
name$col<-as.character(colmatch$col[match(name$OutDegreeShip, colmatch$num)])
name$col[is.na(name$col)]<-"#FFFFFF"
name$ODbin<-NA
for (i in 1:length(name$OutDegreeShip)){
	if (name$OutDegreeShip[i]==0) {name$ODbin[i]<-0}
	else if (log(name$OutDegreeShip[i])>=0 & log(name$OutDegreeShip[i])<=1) {name$ODbin[i]<-1}
	else if (log(name$OutDegreeShip[i])>1 & log(name$OutDegreeShip[i])<=2) {name$ODbin[i]<-2}
	else if (log(name$OutDegreeShip[i])>2 & log(name$OutDegreeShip[i])<=3) {name$ODbin[i]<-3}
	else if (log(name$OutDegreeShip[i])>3 & log(name$OutDegreeShip[i])<=4) {name$ODbin[i]<-4}
	else if (log(name$OutDegreeShip[i])>4 & log(name$OutDegreeShip[i])<=5) {name$ODbin[i]<-5}
	else if (log(name$OutDegreeShip[i])>5 & log(name$OutDegreeShip[i])<=6) {name$ODbin[i]<-6}
}
name$col<-as.character(colmatch$col[match(name$ODbin, colmatch$num)])

tiff('paperdrafts_swine/swine_od_countylevel_2010.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$col, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)
#leg.txt<-c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6")
#leg.col<-cols[1:6]
#color.legend(1, 6, 12, 9, leg.txt, rect.col=leg.col)  # didn't work
#pnts = cbind(x =c(-71,-73.5,-73.5, -71), y =c(28, 38, 38, 28))
#legend.gradient(pnts, cols = leg.col, limits=c("1", "175"), title="", cex=0.5)
dev.off()

################################
# IN-DEGREE FIGURE 2010
name$InDegreeShip<- node.stats$InDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$InDegreeSwine<-node.stats$InDegree_Swine[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$InDegreeShip[is.na(name$InDegreeShip)]<-0
name$InDegreeSwine[is.na(name$InDegreeSwine)]<-0

# Go with a redish color pallette: 
cols <- colorRampPalette(brewer.pal(9, "YlOrRd"))(6)   # colors for level plots
colmatch<-data.frame(num=seq(0,5,1), col=c("white", cols[2:6]))
name$colID<-as.character(colmatch$col[match(name$InDegreeShip, colmatch$num)])
name$colID[is.na(name$col)]<-"#FFFFFF"
name$IDbin<-NA
for (i in 1:length(name$InDegreeShip)){
	if (name$InDegreeShip[i]==0) {name$IDbin[i]<-0}
	else if (log(name$InDegreeShip[i])>=0 & log(name$InDegreeShip[i])<=1) {name$IDbin[i]<-1}
	else if (log(name$InDegreeShip[i])>1 & log(name$InDegreeShip[i])<=2) {name$IDbin[i]<-2}
	else if (log(name$InDegreeShip[i])>2 & log(name$InDegreeShip[i])<=3) {name$IDbin[i]<-3}
	else if (log(name$InDegreeShip[i])>3 & log(name$InDegreeShip[i])<=4) {name$IDbin[i]<-4}
	else if (log(name$InDegreeShip[i])>4 & log(name$InDegreeShip[i])<=5) {name$IDbin[i]<-5}
}
name$colID<-as.character(colmatch$col[match(name$IDbin, colmatch$num)])

tiff('paperdrafts_swine/Figure5a_swine_id_countylevel_2010_nolegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$colID, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)
#leg.txt<-c("0-1", "1-2", "2-3", "3-4", "4-5")
#leg.col<-cols[2:6]
#color.legend(1, 6, 12, 9, leg.txt, rect.col=leg.col)  # didn't work
#pnts = cbind(x =c(-71,-73.5,-73.5, -71), y =c(28, 38, 38, 28))
#legend.gradient(pnts, cols = leg.col, limits=c("1", "102"), title="", cex=0.5)
dev.off()


################################
# Betweenness FIGURE 2010
################################
name$Betweenness<- node.stats$Betweenness[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$Betweenness[is.na(name$Betweenness)]<-0

# Betwenness range is HUGE, max= 13922
cols <- colorRampPalette(brewer.pal(9, "BuGn"))(9)   # colors for level plots
colmatch<-data.frame(num=seq(0,7,1), col=c("white", cols[3:9]))  # 8 long, 7 colors
#name$colB<-as.character(colmatch$col[match(name$Betweenness, colmatch$num)])
#name$colB[is.na(name$col)]<-"#FFFFFF"
name$colB<-NA
name$Bbin<-NA
for (i in 1:length(name$Betweenness)){
	if (name$Betweenness[i]==0) {name$Bbin[i]<-0}
	else if (log(name$Betweenness[i])>=0 & log(name$Betweenness[i])<=1.5) {name$Bbin[i]<-1}
	else if (log(name$Betweenness[i])>1.5 & log(name$Betweenness[i])<=3) {name$Bbin[i]<-2}
	else if (log(name$Betweenness[i])>3 & log(name$Betweenness[i])<=4.5) {name$Bbin[i]<-3}
	else if (log(name$Betweenness[i])>4.5 & log(name$Betweenness[i])<=6) {name$Bbin[i]<-4}
	else if (log(name$Betweenness[i])>6 & log(name$Betweenness[i])<=7.5) {name$Bbin[i]<-5}
	else if (log(name$Betweenness[i])>7.5 & log(name$Betweenness[i])<=9) {name$Bbin[i]<-6}
	else if (log(name$Betweenness[i])>9) {name$Bbin[i]<-7}
}

name$colB<-as.character(colmatch$col[match(name$Bbin, colmatch$num)])

tiff('paperdrafts_swine/Figure5e_swine_btwn_countylevel_2010_nolegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$colB, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)
#leg.txt<-c("0-1", "1-2", "2-3", "3-4", "4-5")
#leg.col<-cols[3:9]
#color.legend(1, 6, 12, 9, leg.txt, rect.col=leg.col)  # didn't work
#pnts = cbind(x =c(-71,-73.5,-73.5, -71), y =c(28, 38, 38, 28))
#legend.gradient(pnts, cols = leg.col, limits=c("1", "13922"), title="", cex=0.5)
dev.off()




#####################################
#####################################
#####################################
# make networks on the log scale for 2011
#####################################
#####################################

# make networks
counties=unique(cbind(c(data2011$O_ST_FIPS, data2011$D_ST_FIPS), c(data2011$O_FIPS, data2011$D_FIPS)))  
counties=counties[order(counties[,2]),]
 node.stats=data.frame(matrix(NA,nrow=length(counties[,1]),
                             ncol=12,
                             dimnames=list(NULL, c("StateID","NodeID",
                             "Unweighted_InDeg","InDegree_Ship","InDegree_Swine", 							 							"Unweighted_OutDeg","OutDegree_Ship","OutDegree_Swine",
 							"TotalDegree_Ship", "TotalDegree_Swine", "Betweenness",
 							"Transitivity"))))	
node.stats$NodeID=counties[,2]
node.stats$StateID=counties[,1]
# It is good practice to watch what you do as you go.  Type summary to see what was done. 
summary(node.stats)  

temp_graph1=graph.edgelist(el=as.matrix(cbind(as.character(data2011$O_FIPS), as.character(data2011$D_FIPS))), directed=TRUE)
temp_graph2=graph.edgelist(el=as.matrix(unique(cbind(as.character(data2011$O_FIPS), as.character(data2011$D_FIPS)))), directed=TRUE)         #  not weighted
temp_graph<-set.edge.attribute(temp_graph1,"weight",value=data2011$NUM_SWINE)
                    										
temp_graph_st=graph.edgelist(el=as.matrix(cbind(as.character(data2011$O_ST_FIPS), as.character(data2011$D_ST_FIPS))), directed=TRUE)
# Calculate node statistics  
	node.stats$Unweighted_InDeg=degree(temp_graph2,mode=c("in"))[order(as.numeric(V(temp_graph2)$name))]
	node.stats$Unweighted_OutDeg=degree(temp_graph2,mode=c("out"))																							[order(as.numeric(V(temp_graph2)$name))]
    node.stats$InDegree_Ship=degree(temp_graph,mode=c("in"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Ship=degree(temp_graph,mode=c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Ship=node.stats$InDegree_Ship+node.stats$OutDegree_Ship
    node.stats$InDegree_Swine=graph.strength(temp_graph,mode=c("in"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Swine=graph.strength(temp_graph,mode=c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Swine=node.stats$InDegree_Swine+node.stats$OutDegree_Swine
    node.stats$Betweenness=betweenness(temp_graph2)[order(as.numeric(V(temp_graph2)$name))] 
	node.stats$Transitivity=transitivity(temp_graph,type=c("local"))[order(as.numeric(V(temp_graph)$name))]

#plot

ctname<-map('county', resolution=0, plot=FALSE)$names
ctname<-as.matrix(ctname)
data(county.fips)
node.stats$COUNTY_NAME_R<-county.fips$polyname[match(node.stats$NodeID, county.fips$fips)]
name<-data.frame(ctname=ctname, OutDegreeShip=NA, OutDegreeSwine=NA)
name$OutDegreeShip<-node.stats$OutDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeSwine<-node.stats$OutDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeShip[is.na(name$OutDegreeShip)]<-0
name$OutDegreeSwine[is.na(name$OutDegreeSwine)]<-0

# 167 is max this year. 

# on log scale
# 1) range of one color
cols <- colorRampPalette(brewer.pal(9, "YlGnBu"))(6)   # colors for level plots
colmatch<-data.frame(num=seq(0,6,1), col=c("white", cols))
name$col<-as.character(colmatch$col[match(name$OutDegreeShip, colmatch$num)])
name$col[is.na(name$col)]<-"#FFFFFF"
name$ODbin<-NA
for (i in 1:length(name$OutDegreeShip)){
	if (name$OutDegreeShip[i]==0) {name$ODbin[i]<-0}
	else if (log(name$OutDegreeShip[i])>=0 & log(name$OutDegreeShip[i])<=1) {name$ODbin[i]<-1}
	else if (log(name$OutDegreeShip[i])>1 & log(name$OutDegreeShip[i])<=2) {name$ODbin[i]<-2}
	else if (log(name$OutDegreeShip[i])>2 & log(name$OutDegreeShip[i])<=3) {name$ODbin[i]<-3}
	else if (log(name$OutDegreeShip[i])>3 & log(name$OutDegreeShip[i])<=4) {name$ODbin[i]<-4}
	else if (log(name$OutDegreeShip[i])>4 & log(name$OutDegreeShip[i])<=5) {name$ODbin[i]<-5}
	else if (log(name$OutDegreeShip[i])>5 & log(name$OutDegreeShip[i])<=6) {name$ODbin[i]<-6}
}
name$col<-as.character(colmatch$col[match(name$ODbin, colmatch$num)])

tiff('paperdrafts_swine/Figure5d_swine_od_countylevel_2011_updatelegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$col, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "Nebraska", "North Carolina", "Wisconsin"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)

# state level map legends use:
colorlegend(col=cols, zval=c(0, 2.8, 7.4, 20.1, 54.6, 148.4, 403.4), zlim=c(1, 403), log=TRUE, posx=c(0.8, 0.83), posy=c(0.22, 0.6), digit=0, cex=0.2)
# initial county-level map legends (changed for consistency)
#leg.txt<-c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6")
#leg.col<-cols[1:6]
#pnts = cbind(x =c(-71,-73.5,-73.5, -71), y =c(28, 38, 38, 28))
#legend.gradient(pnts, cols = leg.col, limits=c("1", "175"), title="", cex=0.5)
dev.off()

################################
# IN-DEGREE FIGURE 2011
################################
name$InDegreeShip<- node.stats$InDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$InDegreeSwine<-node.stats$InDegree_Swine[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$InDegreeShip[is.na(name$InDegreeShip)]<-0
name$InDegreeSwine[is.na(name$InDegreeSwine)]<-0

# Go with a redish color pallette:  can use same range because max=133, exp(5)=148
cols <- colorRampPalette(brewer.pal(9, "YlOrRd"))(6)   # colors for level plots
colmatch<-data.frame(num=seq(0,5,1), col=c("white", cols[2:6]))
name$colID<-as.character(colmatch$col[match(name$InDegreeShip, colmatch$num)])
name$colID[is.na(name$col)]<-"#FFFFFF"
name$IDbin<-NA
for (i in 1:length(name$InDegreeShip)){
	if (name$InDegreeShip[i]==0) {name$IDbin[i]<-0}
	else if (log(name$InDegreeShip[i])>=0 & log(name$InDegreeShip[i])<=1) {name$IDbin[i]<-1}
	else if (log(name$InDegreeShip[i])>1 & log(name$InDegreeShip[i])<=2) {name$IDbin[i]<-2}
	else if (log(name$InDegreeShip[i])>2 & log(name$InDegreeShip[i])<=3) {name$IDbin[i]<-3}
	else if (log(name$InDegreeShip[i])>3 & log(name$InDegreeShip[i])<=4) {name$IDbin[i]<-4}
	else if (log(name$InDegreeShip[i])>4 & log(name$InDegreeShip[i])<=5) {name$IDbin[i]<-5}
}
name$colID<-as.character(colmatch$col[match(name$IDbin, colmatch$num)])

tiff('paperdrafts_swine/Figure5b_swine_id_countylevel_2011_updatelegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$colID, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)
# state level map legends use:
colorlegend(col=cols[2:6], zval=c(0, 2.8, 7.4, 20.1, 54.6, 148.4), zlim=c(1, 148), log=TRUE, posx=c(0.8, 0.83), posy=c(0.22, 0.6), digit=0, cex=0.2)

#leg.txt<-c("0-1", "1-2", "2-3", "3-4", "4-5")
#leg.col<-cols[2:6]
#pnts = cbind(x =c(-71,-73.5,-73.5, -71), y =c(28, 38, 38, 28))
#legend.gradient(pnts, cols = leg.col, limits=c("1", "133"), title="", cex=0.5)
dev.off()

################################
# Betweenness FIGURE 2011
################################
name$Betweenness<- node.stats$Betweenness[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$Betweenness[is.na(name$Betweenness)]<-0

# Go with a redish color pallette:  can use same range because max=133, exp(5)=148
cols <- colorRampPalette(brewer.pal(9, "BuGn"))(9)   # colors for level plots
colmatch<-data.frame(num=seq(0,7,1), col=c("white", cols[3:9]))  # 8 long, 7 colors
#name$colB<-as.character(colmatch$col[match(name$Betweenness, colmatch$num)])
#name$colB[is.na(name$col)]<-"#FFFFFF"
name$colB<-NA
name$Bbin<-NA
for (i in 1:length(name$Betweenness)){
	if (name$Betweenness[i]==0) {name$Bbin[i]<-0}
	else if (log(name$Betweenness[i])>=0 & log(name$Betweenness[i])<=1.5) {name$Bbin[i]<-1}
	else if (log(name$Betweenness[i])>1.5 & log(name$Betweenness[i])<=3) {name$Bbin[i]<-2}
	else if (log(name$Betweenness[i])>3 & log(name$Betweenness[i])<=4.5) {name$Bbin[i]<-3}
	else if (log(name$Betweenness[i])>4.5 & log(name$Betweenness[i])<=6) {name$Bbin[i]<-4}
	else if (log(name$Betweenness[i])>6 & log(name$Betweenness[i])<=7.5) {name$Bbin[i]<-5}
	else if (log(name$Betweenness[i])>7.5 & log(name$Betweenness[i])<=9) {name$Bbin[i]<-6}
	else if (log(name$Betweenness[i])>9) {name$Bbin[i]<-7}
}
# There are two rows with non-zero betweenness values (0.91667 and 0.667) that don't fit into my color scheme...
name$Bbin[756]<-1
name$Bbin[1323]<-1
name$colB<-as.character(colmatch$col[match(name$Bbin, colmatch$num)])

tiff('paperdrafts_swine/Figure5f_swine_btwn_countylevel_2011_updatelegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$colB, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)
# state level map legends use:
colorlegend(col=cols[3:9], zval=c(0, 4.48, 20.09, 90.02, 403.43, 1808, 8103), zlim=c(1, 8103), log=TRUE, posx=c(0.8, 0.83), posy=c(0.22, 0.6), digit=0, cex=0.2)

# county legend
#leg.txt<-c("0-1", "1-2", "2-3", "3-4", "4-5")
#leg.col<-cols[3:9]
#pnts = cbind(x =c(-71,-73.5,-73.5, -71), y =c(28, 38, 38, 28))
#legend.gradient(pnts, cols = leg.col, limits=c("1", "13123"), title="", cex=0.5)
dev.off()


###############################################
#############################################
# Figure 6: MAPS OF IN-DEGREE AND OUT DEGREE AT STATE LEVEL
#############################################
###############################################
net.stats_st_2011<-read.csv("~/Documents/post-doc/Swine/net_stats_st_2011all.csv")
node.stats_st_2011<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2011all.csv")
net.stats_st_2010<-read.csv("~/Documents/post-doc/Swine/net_stats_st_2010.csv")
node.stats_st_2010<-read.csv("~/Documents/post-doc/Swine/node_stats_st_2010.csv")
data(state.fips)

# Map of out degree= blue; In degree by state= red; betweeness= green. 

# Out degree= 
ctname<-map('state', resolution=0, plot=FALSE)$names
ctname<-as.matrix(ctname)
data(state.fips)
node.stats_st_2010$COUNTY_NAME_R<-state.fips$polyname[match(node.stats_st_2010$NodeID, state.fips$fips)]
node.stats_st_2011$COUNTY_NAME_R<-state.fips$polyname[match(node.stats_st_2011$NodeID, state.fips$fips)]

name<-data.frame(ctname=ctname, OutDegreeShip2010=NA, OutDegreeSwine2010=NA, OutDegreeShip2011=NA, OutDegreeSwine2011=NA, InDegreeShip2010=NA, InDegreeSwine2010=NA, InDegreeShip2011=NA, InDegreeSwine2011=NA, Betweenness2010=NA, Betweenness2011=NA)
# fill outdegree columns
name$OutDegreeShip2010<-node.stats_st_2010$OutDegree_Ship[match(name$ctname, node.stats_st_2010$COUNTY_NAME_R)]
name$OutDegreeSwine2010<-node.stats_st_2010$OutDegree_Ship[match(name$ctname, node.stats_st_2010$COUNTY_NAME_R)]
name$OutDegreeShip2010[is.na(name$OutDegreeShip2010)]<-0
name$OutDegreeSwine2010[is.na(name$OutDegreeSwine2010)]<-0
name$OutDegreeShip2011<-node.stats_st_2011$OutDegree_Ship[match(name$ctname, node.stats_st_2011$COUNTY_NAME_R)]
name$OutDegreeSwine2011<-node.stats_st_2011$OutDegree_Ship[match(name$ctname, node.stats_st_2011$COUNTY_NAME_R)]
name$OutDegreeShip2011[is.na(name$OutDegreeShip2011)]<-0
name$OutDegreeSwine2011[is.na(name$OutDegreeSwine2011)]<-0
# fill indegree columns
name$InDegreeShip2010<-node.stats_st_2010$InDegree_Ship[match(name$ctname, node.stats_st_2010$COUNTY_NAME_R)]
name$InDegreeSwine2010<-node.stats_st_2010$InDegree_Ship[match(name$ctname, node.stats_st_2010$COUNTY_NAME_R)]
name$InDegreeShip2010[is.na(name$InDegreeShip2010)]<-0
name$InDegreeSwine2010[is.na(name$InDegreeSwine2010)]<-0
name$InDegreeShip2011<-node.stats_st_2011$InDegree_Ship[match(name$ctname, node.stats_st_2011$COUNTY_NAME_R)]
name$InDegreeSwine2011<-node.stats_st_2011$InDegree_Ship[match(name$ctname, node.stats_st_2011$COUNTY_NAME_R)]
name$InDegreeShip2011[is.na(name$InDegreeShip2011)]<-0
name$InDegreeSwine2011[is.na(name$InDegreeSwine2011)]<-0
# Betweenness
name$Betweenness2010<-node.stats_st_2010$Betweenness[match(name$ctname, node.stats_st_2010$COUNTY_NAME_R)]
name$Betweenness2011<-node.stats_st_2011$Betweenness[match(name$ctname, node.stats_st_2011$COUNTY_NAME_R)]

# try two color schemes... since shipments range from 0 to 172.  

############################
# Make Out Degree Color Schemes & Plot 2010 & 2011
# 1) range of one color
cols <- colorRampPalette(brewer.pal(9, "PuBu"))(7)   # colors for level plots
colmatch<-data.frame(num=seq(0,7,1), col=c("white", cols))
name$OutDegreeShip2010[43]<-0
name$OutDegreeShip2010[29]<-0

#log scale color
#name$ODbin2010<-NA; name$ODbin2011<-NA; name$col2010<-NA; name$col2011<-NA
#for (i in 1:length(name$OutDegreeShip2010)){
#	if (name$OutDegreeShip2010[i]==0) {name$ODbin2010[i]<-0}
#	else if (log(name$OutDegreeShip2010[i])>=0 & log(name$OutDegreeShip2010[i])<=1) {name$ODbin2010[i]<-1}
#	else if (log(name$OutDegreeShip2010[i])>1 & log(name$OutDegreeShip2010[i])<=2) {name$ODbin2010[i]<-2}
#	else if (log(name$OutDegreeShip2010[i])>2 & log(name$OutDegreeShip2010[i])<=3) {name$ODbin2010[i]<-3}
#	else if (log(name$OutDegreeShip2010[i])>3 & log(name$OutDegreeShip2010[i])<=4) {name$ODbin2010[i]<-4}
#	else if (log(name$OutDegreeShip2010[i])>4 & log(name$OutDegreeShip2010[i])<=5) {name$ODbin2010[i]<-5}
#	else if (log(name$OutDegreeShip2010[i])>5 & log(name$OutDegreeShip2010[i])<=6) {name$ODbin2010[i]<-6}
#	else if (log(name$OutDegreeShip2010[i])>6 & log(name$OutDegreeShip2010[i])<=7) {name$ODbin2010[i]<-7}
#}

name$ODbin2010<-NA; name$ODbin2011<-NA; name$col2010<-NA; name$col2011<-NA
for (i in 1:length(name$OutDegreeShip2010)){
	if (name$OutDegreeShip2010[i]==0) {name$ODbin2010[i]<-0}
	else if ((name$OutDegreeShip2010[i])>=4 &(name$OutDegreeShip2010[i])<=200) {name$ODbin2010[i]<-2}
	else if ((name$OutDegreeShip2010[i])>200 &(name$OutDegreeShip2010[i])<=400) {name$ODbin2010[i]<-3}
	else if ((name$OutDegreeShip2010[i])>400 &(name$OutDegreeShip2010[i])<=600) {name$ODbin2010[i]<-4}
	else if ((name$OutDegreeShip2010[i])>600 &(name$OutDegreeShip2010[i])<=800) {name$ODbin2010[i]<-5}
	else if ((name$OutDegreeShip2010[i])>800 &(name$OutDegreeShip2010[i])<=1000) {name$ODbin2010[i]<-6}
	else if ((name$OutDegreeShip2010[i])>1000 &(name$OutDegreeShip2010[i])<=1200) {name$ODbin2010[i]<-7}
}

name$col2010<-as.character(colmatch$col[match(name$ODbin2010, colmatch$num)])
name$col2010[is.na(name$col2010)]<-"#FFFFFF"

name$col2010[39]<-name$col2010[38]  # fill in main NC with rest of NC
name$col2010[40]<-name$col2010[38]  # fill in main NC with rest of NC

name$col2010[35]<-name$col2010[34]  # fill in main NY with rest of NY
name$col2010[36]<-name$col2010[34]
name$col2010[37]<-name$col2010[34]

name$col2010[24]<-name$col2010[23]  # fill in main MI with rest of MI
name$col2010[21]<-name$col2010[20]  # mass
name$col2010[22]<-name$col2010[20] 

name$col2010[54]<-name$col2010[53]  # fill in main VA
name$col2010[55]<-name$col2010[53]  


name$col2010[57]<-name$col2010[56]  # fill in main Washington
name$col2010[58]<-name$col2010[56] 
name$col2010[59]<-name$col2010[56] 
name$col2010[60]<-name$col2010[56] 


#for (i in 1:length(name$OutDegreeShip2011)){
#	if (name$OutDegreeShip2011[i]==0) {name$ODbin2011[i]<-0}
#	else if (log(name$OutDegreeShip2011[i])>=0 & log(name$OutDegreeShip2011[i])<=1) {name$ODbin2011[i]<-1}
#	else if (log(name$OutDegreeShip2011[i])>1 & log(name$OutDegreeShip2011[i])<=2) {name$ODbin2011[i]<-2}
#	else if (log(name$OutDegreeShip2011[i])>2 & log(name$OutDegreeShip2011[i])<=3) {name$ODbin2011[i]<-3}
#	else if (log(name$OutDegreeShip2011[i])>3 & log(name$OutDegreeShip2011[i])<=4) {name$ODbin2011[i]<-4}
#	else if (log(name$OutDegreeShip2011[i])>4 & log(name$OutDegreeShip2011[i])<=5) {name$ODbin2011[i]<-5}
#	else if (log(name$OutDegreeShip2011[i])>5 & log(name$OutDegreeShip2011[i])<=6) {name$ODbin2011[i]<-6}
#	else if (log(name$OutDegreeShip2011[i])>6 & log(name$OutDegreeShip2011[i])<=7) {name$ODbin2011[i]<-7}
#}

for (i in 1:length(name$OutDegreeShip2011)){
	if (name$OutDegreeShip2010[i]==0) {name$ODbin2010[i]<-0}
	else if ((name$OutDegreeShip2011[i])>=4 &(name$OutDegreeShip2011[i])<=200) {name$ODbin2011[i]<-2}
	else if ((name$OutDegreeShip2011[i])>200 &(name$OutDegreeShip2011[i])<=400) {name$ODbin2011[i]<-3}
	else if ((name$OutDegreeShip2011[i])>400 &(name$OutDegreeShip2011[i])<=600) {name$ODbin2011[i]<-4}
	else if ((name$OutDegreeShip2011[i])>600 &(name$OutDegreeShip2011[i])<=800) {name$ODbin2011[i]<-5}
	else if ((name$OutDegreeShip2011[i])>800 &(name$OutDegreeShip2011[i])<=1000) {name$ODbin2011[i]<-6}
	else if ((name$OutDegreeShip2011[i])>1000 &(name$OutDegreeShip2011[i])<=1200) {name$ODbin2011[i]<-7}
}
name$ODbin2011[name$ctname=="nebraska"]<-6

name$col2011<-as.character(colmatch$col[match(name$ODbin2011, colmatch$num)])
name$col2011[is.na(name$col2011)]<-"#FFFFFF"

name$col2011[39]<-name$col2011[38]  # fill in main NC with rest of NC
name$col2011[40]<-name$col2011[38]  # fill in main NC with rest of NC

name$col2011[35]<-name$col2011[34]  # fill in main NY with rest of NY
name$col2011[36]<-name$col2011[34]
name$col2011[37]<-name$col2011[34]

# 2010
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/OD2010.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('state', resolution=0, lwd=0.5, col="dark gray")
map('state', resolution=0, fill=TRUE, col=name$col2010, boundary="light gray", lwd=0.5, add=TRUE)
map('state', resolution=0, add=TRUE)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="dark blue", add=TRUE, lwd=2)
#color.legend(1, 6, 12, 9, leg.txt, rect.col=leg.col)  # didn't work
dev.off()

# 2011
col2=cols[c(1,3:7)]
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/OD2011.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('state', resolution=0, lwd=0.5, col="dark gray")
map('state', resolution=0, fill=TRUE, col=name$col2011, boundary="light gray", lwd=0.5, add=TRUE)
map('state', resolution=0, add=TRUE)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "Nebraska", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="dark blue", add=TRUE, lwd=2)
colorlegend(col=col2, zval=c(0, 200, 400, 600, 800, 1000, 1200), zlim=c(1, 1200), log=FALSE, posx=c(0.8, 0.83), posy=c(0.22, 0.6), digit=0, cex=0.2)
dev.off()

############################
# Make In Degree Color Schemes & Plot 2010 & 2011

# 1) range of one color
cols <- colorRampPalette(brewer.pal(9, "YlOrRd"))(7)   # colors for level plots
colmatch<-data.frame(num=seq(0,7,1), col=c("white", cols))

name$IDbin2010<-NA; name$IDbin2011<-NA; name$idcol2010<-NA; name$idcol2011<-NA
for (i in 1:length(name$OutDegreeShip2010)){
	if (name$InDegreeShip2010[i]==0) {name$IDbin2010[i]<-0}
	else if (log(name$InDegreeShip2010[i])>=0 & log(name$InDegreeShip2010[i])<=1) {name$IDbin2010[i]<-1}
	else if (log(name$InDegreeShip2010[i])>1 & log(name$InDegreeShip2010[i])<=2) {name$IDbin2010[i]<-2}
	else if (log(name$InDegreeShip2010[i])>2 & log(name$InDegreeShip2010[i])<=3) {name$IDbin2010[i]<-3}
	else if (log(name$InDegreeShip2010[i])>3 & log(name$InDegreeShip2010[i])<=4) {name$IDbin2010[i]<-4}
	else if (log(name$InDegreeShip2010[i])>4 & log(name$InDegreeShip2010[i])<=5) {name$IDbin2010[i]<-5}
	else if (log(name$InDegreeShip2010[i])>5 & log(name$InDegreeShip2010[i])<=6) {name$IDbin2010[i]<-6}
	else if (log(name$InDegreeShip2010[i])>6 & log(name$InDegreeShip2010[i])<=7.5) {name$IDbin2010[i]<-7}
}
name$idcol2010<-as.character(colmatch$col[match(name$IDbin2010, colmatch$num)])
name$idcol2010[is.na(name$idcol2010)]<-"#FFFFFF"

name$idcol2010[39]<-name$idcol2010[38]  # fill in main NC with rest of NC
name$idcol2010[40]<-name$idcol2010[38]  # fill in main NC with rest of NC

name$idcol2010[35]<-name$idcol2010[34]  # fill in main NY with rest of NY
name$idcol2010[36]<-name$idcol2010[34]
name$idcol2010[37]<-name$idcol2010[34]

name$idcol2010[24]<-name$idcol2010[23]  # fill in main MI with rest of MI
name$idcol2010[21]<-name$idcol2010[20]  # mass
name$idcol2010[22]<-name$idcol2010[20] 

name$idcol2010[54]<-name$idcol2010[53]  # fill in main VA
name$idcol2010[55]<-name$idcol2010[53]  


name$idcol2010[57]<-name$idcol2010[56]  # fill in main Washington
name$idcol2010[58]<-name$idcol2010[56] 
name$idcol2010[59]<-name$idcol2010[56] 
name$idcol2010[60]<-name$idcol2010[56] 

for (i in 1:length(name$InDegreeShip2011)){
	if (name$InDegreeShip2011[i]==0) {name$IDbin2011[i]<-0}
	else if (log(name$InDegreeShip2011[i])>=0 & log(name$InDegreeShip2011[i])<=1) {name$IDbin2011[i]<-1}
	else if (log(name$InDegreeShip2011[i])>1 & log(name$InDegreeShip2011[i])<=2) {name$IDbin2011[i]<-2}
	else if (log(name$InDegreeShip2011[i])>2 & log(name$InDegreeShip2011[i])<=3) {name$IDbin2011[i]<-3}
	else if (log(name$InDegreeShip2011[i])>3 & log(name$InDegreeShip2011[i])<=4) {name$IDbin2011[i]<-4}
	else if (log(name$InDegreeShip2011[i])>4 & log(name$InDegreeShip2011[i])<=5) {name$IDbin2011[i]<-5}
	else if (log(name$InDegreeShip2011[i])>5 & log(name$InDegreeShip2011[i])<=6) {name$IDbin2011[i]<-6}
	else if (log(name$InDegreeShip2011[i])>6 & log(name$InDegreeShip2011[i])<=7.5) {name$IDbin2011[i]<-7}

}
name$idcol2011<-as.character(colmatch$col[match(name$IDbin2011, colmatch$num)])
name$idcol2011[is.na(name$idcol2011)]<-"#FFFFFF"

name$idcol2011[39]<-name$idcol2011[38]  # fill in main NC with rest of NC
name$idcol2011[40]<-name$idcol2011[38]  # fill in main NC with rest of NC

name$idcol2011[35]<-name$idcol2011[34]  # fill in main NY with rest of NY
name$idcol2011[36]<-name$idcol2011[34]
name$idcol2011[37]<-name$idcol2011[34]

name$idcol2011[24]<-name$idcol2011[23]  # fill in main MI with rest of MI
name$idcol2011[21]<-name$idcol2011[20]  # mass
name$idcol2011[22]<-name$idcol2011[20] 

name$idcol2011[54]<-name$idcol2011[53]  # fill in main VA
name$idcol2011[55]<-name$idcol2011[53]  


name$idcol2011[57]<-name$idcol2011[56]  # fill in main Washington
name$idcol2011[58]<-name$idcol2011[56] 
name$idcol2011[59]<-name$idcol2011[56] 
name$idcol2011[60]<-name$idcol2011[56] 

# 2010
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/ID2010.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('state', resolution=0, lwd=0.5, col="dark gray")
map('state', resolution=0, fill=TRUE, col=name$idcol2010, boundary="light gray", lwd=0.5, add=TRUE)
map('state', resolution=0, add=TRUE)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin"),
    resolution=0, col="dark blue", add=TRUE, lwd=2)
#colorlegend(col=cols, zval=c(0, 2.8, 7.4, 20.1, 54.6, 148.4, 403.4, 1111), zlim=c(1, 1111), log=TRUE, posx=c(0.86, 0.89), posy=c(0.22, 0.6), digit=0, cex=0.8)
dev.off()

# 2011
tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/ID2011.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('state', resolution=0, lwd=0.5, col="dark gray")
map('state', resolution=0, fill=TRUE, col=name$idcol2011, boundary="light gray", lwd=0.5, add=TRUE)
map('state', resolution=0, add=TRUE)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "Nebraska", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="dark blue", add=TRUE, lwd=2)
colorlegend(col=cols, zval=c(0, 2.8, 7.4, 20.1, 54.6, 148.4, 403.4, 1083), zlim=c(1, 1111), log=TRUE, posx=c(0.8, 0.83), posy=c(0.22, 0.6), digit=0, cex=0.2)

dev.off()


tiff(filename="~/Documents/post-doc/Swine/paperdrafts_swine/mapforholly.tiff",
width = 140, height = 90, units = "mm", res=600, compression="lzw")
par(mai=c(1,1,1,1))
map('state', resolution=0, lwd=1)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "Nebraska", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="dark blue", add=TRUE, lwd=2)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "Nebraska", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="light blue", add=TRUE, fill=TRUE)
dev.off()
