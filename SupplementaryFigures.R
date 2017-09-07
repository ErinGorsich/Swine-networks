# Supplementary Figures


setwd("~/Documents/post-doc/Swine")

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
# Industry information
###############################################
###############################################
income <- read.csv("~/Documents/post-doc/Swine/paperdrafts_swine/toRyan_SwineMS/postRyan/Hogs_gross_income.csv")
dataframe <- income
dataframe<-dataframe[!is.na(dataframe$State),]
dataframe$Value<-as.character(dataframe$Value)
dataframe$Value2<-as.numeric(gsub(",", "", dataframe$Value))
dataframe<-dataframe[!is.na(dataframe$Value2),]  # repeat, some NAs not removed earlier
df <- dataframe[dataframe$Year == "2016",]
sum(df$Value2)
sum(df$Value2[df$State %in% c("CALIFORNIA", "IOWA", "MINNESOTA", "NEBRASKA", "NORTH CAROLINA", "TEXAS", "NEW YORK", "WISCONSIN")])
# 63%



###############################################
###############################################
# Betweenness summary information for discussion
###############################################
###############################################
test <- read.csv("~/Documents/post-doc/Swine/node_stats_2011all.csv")
df <- test[test$Betweenness >= quantile(test$Betweennes, c(0.9))[[1]],]
df <- test[test$Betweenness >= quantile(test$Betweennes, c(0.98))[[1]],]

df <- test[test$Betweenness >= quantile(test$Betweennes[test$Betweenness >0], c(0.9))[[1]],]
df <- test[test$Betweenness >= quantile(test$Betweennes[test$Betweenness >0], c(0.8))[[1]],]

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