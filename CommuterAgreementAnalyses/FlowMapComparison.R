###############################################
###############################################
# Figure 4: Flow map comparison ICVI vs. CA
###############################################
###############################################
library(maps)
library(sp)
library(RColorBrewer)
library(ggplot2)

setwd("~/Documents/post-doc/Swine")

###############################################
# Read in and groom ICVI data
###############################################
data.cvi <- read.csv("Swine_cvi_final.csv")
data.cvi <- data.cvi[!is.na(data.cvi$NUM_SWINE), ]  #-1
data.cvi <- data.cvi[data.cvi$NUM_SWINE > 0, ] # -13
data.cvi <- data.cvi[!is.na(data.cvi$SAMPLE_YEAR2), ]  #-38 
data.cvi <- data.cvi[!is.na(data.cvi $O_FIPS), ]
data.cvi <- data.cvi[!is.na(data.cvi $D_FIPS), ]
data.cvi <- data.cvi[data.cvi$NUM_SWINE > 0, ]
summary(data.cvi)
colnames(data.cvi)

# make a new column of all ones that represents the no. shipments
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
 
 # Proxy for non-slaughter, out-of state shipments!
data.cvi = data.cvi[data.cvi$PURPOSE != "Slaughter",]
data.cvi <-data.cvi[data.cvi$O_FIPS!=data.cvi$D_FIPS,] # REMOVE INTRASTATE 
length(data.cvi[data.cvi$O_ST_FIPS==data.cvi$D_ST_FIPS,])
data.cvi<-data.cvi[data.cvi$O_ST_FIPS!=data.cvi$D_ST_FIPS,]
data <- data.cvi
 
 
# Subset cvi data by year.
data2011 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2011",]

###############################################
# Read in and groom CA data
###############################################
ca <- read.csv("CommuterAgreementAnalyses/Swine_CA_Database_31May2017.csv")
ca <- ca[ca$Year == "2014",]
# no longer an issue (was with previous db Swine_Commuter_Database_v2)
#errors <- c("Lowden", "IA50846", "IA52586")
#ca$D.state[ca$D.state %in% errors] <- "IA"
table(ca$O.state, ca$D.state)
ca <- ca[!is.na(ca$D.state),]  # 321 removed
ca <- ca[!is.na(ca$O.state),]  # 5 removed
ca <- ca[!is.na(ca$NUM.SHIPPED),]  # 6 removed
ca$O.state <- as.character(ca$O.state)
ca$D.state <- as.character(ca$D.state)
# one county was associated with two different states. 
#Checking the data, this looks like a typo as Minnesota has state fips 27
ca$D.state[ca$D.FIPS == 27045 & !is.na(ca$D.FIPS)] <- "MN"

statefips <- read.csv("~/Documents/post-doc/2009-2011 comparison final/county_centroid_coordinates.csv")
statefips$stab <- as.character(statefips$stab)
ca$O.state.FIPS <- statefips$ST_FIPS[match(ca$O.state, statefips$stab)]
ca$D.state.FIPS <-  statefips$ST_FIPS[match(ca$D.state, statefips$stab)]
ca <- ca[!(ca$O.state.FIPS == ca$D.state.FIPS),] #removed 7, now 10030 records
ca <- ca[!(ca$D.state.FIPS == 19 & ca$D.FIPS == "46083"),]
ca <- ca[!is.na(ca$O.state.FIPS), ]

# remove shipments between states that should not be... 
#table(ca$O.state, ca$D.state)
#ca <- ca[!(ca$O.state == "IN"& ca$D.state == "IL"),]  # 11 from IN to IL
#ca <- ca[!(ca$O.state == "TX" & ca$D.state == "IL"),] # 2
#ca <- ca[!(ca$O.state == "MN" & ca$D.state == "IN"),] # 4
#ca <- ca[!(ca$D.state %in% c("MN", "WY", "14" )),]# 22 to WY fm SD, 14 SD fm IN & OK
# Check on MN (119 going there from IN, 10 from MO, 47 from SD, 32 from WY)
#ca <- ca[!(ca$O.state == "CO" & ca$D.state == "OK"),] # 2
#ca <- ca[!(ca$O.state == "OK" & ca$D.state == "SD"),] # 7
#ca <- ca[!(ca$O.state == "TX" & ca$D.state == "IL"),] # not supposed to be here?
#ca <- ca[!(ca$O.state == "TX" & ca$D.state == "MO"),] # 44

###############################################
# Organize county centroid information
###############################################
matchdata <- statefips
matchdata <- matchdata[!is.na(matchdata$FIPS), ]
matchdata$state2<-tolower(matchdata$state)

getLabelPoint = function(state){
	Polygon(state[c('long', 'lat')])@labpt
}
df <- map_data('state')
centroids <- by(df, df$region, getLabelPoint)   # returns list of labelpoints
centroids <- do.call("rbind.data.frame", centroids)  # convert to a Data frame
names(centroids) <- c('long', 'lat')	
centroids$names <- rownames(centroids)

# check centroids are in the correct spot: 
map('state')
text(centroids$long, centroids$lat, rownames(centroids), offset=0, cex=0.4)

# States washington, virginia, north carolina, new york, michigan are funky. 
statecoords<-read.table("~/Documents/post-doc/2010 Cattle Movement Practice/state_centroid_coordinates.txt", header=TRUE, sep=",") 
centroids$lat[centroids$names=="washington"] <- statecoords$latitude[
    statecoords$state=="Washington"]
centroids$lat[centroids$names=="virginia"] <- statecoords$latitude[
    statecoords$state=="Virginia"]
centroids$long[centroids$names=="virginia"] <- statecoords$longitude[
    statecoords$state=="Virginia"]	
centroids$lat[centroids$names=="north carolina"] <- statecoords$latitude[
    statecoords$state=="North Carolina"]
centroids$long[centroids$names=="north carolina"] <- statecoords$longitude[
    statecoords$state=="North Carolina"]	
centroids$lat[centroids$names=="new york"] <- statecoords$latitude[
    statecoords$state=="New York"]
centroids$long[centroids$names=="new york"] <- statecoords$longitude[
    statecoords$state=="New York"]	
centroids$lat[centroids$names=="michigan"] <- statecoords$latitude[
    statecoords$state=="Michigan"]
centroids$long[centroids$names=="michigan"] <- statecoords$longitude[
    statecoords$state=="Michigan"]	
centroids$lat[centroids$names=="new york"] <- 42.8
centroids$lat[centroids$names=="massachusetts"] <- 42.45
map('state')
text(centroids$long, centroids$lat, rownames(centroids), offset=0, cex=0.4)

matchdata$Rlatitude<-centroids$lat[match(matchdata$state2, rownames(centroids))]
matchdata$Rlongitude<-centroids$long[match(matchdata$state2, rownames(centroids))]

###############################################
# Make ICVI edgelist
###############################################
##make a vector of all pairs
edge <- unique(cbind(data2011$O_ST_FIPS, data2011$D_ST_FIPS) )
edgerev <- unique(cbind(data2011$D_ST_FIPS, data2011$O_ST_FIPS) )

# gets any rows that are duplicate
remove <- unique( c(which( outer(edge[,1], edge[,2], "==") & 
       outer(edge[,2], edge[,1], "=="), 
       arr.ind=TRUE) ))
       
edge <- data.frame(edge)
colnames(edge) <- c("node1", "node2")
edge$seq <- seq(1, length(edge$node1))
temp <- edge[remove, ]
temp <- temp[order(temp$node1), ]
# but only want to remove the fist duplicate of each
# [1]   8  20  32   4  44  13  12 105   6  91   7 150 128  11 114 168 108  29 159 147  16
# [22]  84  46 164  43  97 175  41  95 121  48 149
rm <- remove[remove %in% c(4, 11, 12,  114, # duplicates with 19
	43, 121, 95,  # duplicates with 6 
	7, 20,  84, 105, # duplicates with 27
	150,  # with 31
	175, 91, 147)]
edge2011 <- edge[!(edge$seq %in% rm), ]

edge2011$N <- NA; edge2011$swine <- NA

for (i in 1:length(edge2011[,1])) {
	temp <- data2011$NUM_SWINE[data2011$O_ST_FIPS == edge2011$node1[i] &
		data2011$D_ST_FIPS == edge2011$node2[i]]
	temp2 <- data2011$NUM_SWINE[data2011$O_ST_FIPS == edge2011$node2[i] &
		data2011$D_ST_FIPS == edge2011$node1[i]] 
	edge2011$N[i] <- length(temp) + length(temp2)
	edge2011$swine[i] <- sum(temp) + sum(temp2)
	rm(temp, temp2)
}

# add in centroid data for flows from centroids and matchdata compiled for 2010 above
edge2011$Olatitude<-NA; edge2011$Dlatitude<-NA
edge2011$Olatitude<- matchdata$Rlatitude[match(edge2011$node1, matchdata$ST_FIPS)]
edge2011$Dlatitude<- matchdata$Rlatitude[match(edge2011$node2, matchdata$ST_FIPS)]
edge2011$Olongitude<- matchdata$Rlongitude[match(edge2011$node1, matchdata$ST_FIPS)]
edge2011$Dlongitude<- matchdata$Rlongitude[match(edge2011$node2, matchdata$ST_FIPS)]

# check for strange origin locations? (NAs in total as well)
# remove row 113, with a shipment from Cali to Hawaii (FIPS=15)...
edge2011<-edge2011[edge2011$node2!=15,]
edge2011<-edge2011[edge2011$node2!=2,]



###############################################
# Make CA edgelist
###############################################
##make a vector of all pairs
edge <- unique(cbind(ca$O.state.FIPS, ca$D.state.FIPS) )
edgerev <- unique(cbind(ca$D.state.FIPS, ca$O.state.FIPS) )
# no repeats
#remove <- unique( c(which( outer(edge[,1], edge[,2], "==") & 
#      outer(edge[,2], edge[,1], "=="), 
#       arr.ind=TRUE) ))
#edge2 <- edge[ ! 1:NROW(edge) %in% remove, ]

edge2014 <- data.frame(edge)
colnames(edge2014) <- c("node1", "node2")
edge2014$N <- NA; edge2014$swine <- NA

for (i in 1:length(edge2014[,1])) {
	temp <- ca$NUM.SHIPPED[ca$O.state.FIPS == edge2014$node1[i] &
		ca$D.state.FIPS == edge2014$node2[i]]
	temp2 <- ca$NUM.SHIPPED[ca$O.state.FIPS == edge2014$node2[i] &
		ca$D.state.FIPS == edge2014$node1[i]] 
	edge2014$N[i] <- length(temp) + length(temp2)
	edge2014$swine[i] <- sum(temp) + sum(temp2)
	rm(temp, temp2)
}

# add in centroid data for flows from centroids and matchdata compiled for 2010 above
edge2014$Olatitude<-NA; edge2014$Dlatitude<-NA
edge2014$Olatitude<- matchdata$Rlatitude[match(edge2014$node1, matchdata$ST_FIPS)]
edge2014$Dlatitude<- matchdata$Rlatitude[match(edge2014$node2, matchdata$ST_FIPS)]
edge2014$Olongitude<- matchdata$Rlongitude[match(edge2014$node1, matchdata$ST_FIPS)]
edge2014$Dlongitude<- matchdata$Rlongitude[match(edge2014$node2, matchdata$ST_FIPS)]


###############################################
# Make maps
###############################################
edge2014$plotN <- edge2014$N*0.3

par(mfrow = c(2,2))
hist(edge2011$N)
hist(edge2014$plotN)
hist(sqrt(edge2011$N))
hist(sqrt(edge2014$plotN))

maxn <- max(edge2011$N, edge2014$plotN)
minn <- min(0, edge2014$plotN)
length.out <- 50

blues <- brewer.pal(8, "Blues")[3:8]
bluefunction <- colorRampPalette(blues)
test <- data.frame(colval = seq(1, maxn + 1, by = ((maxn - 1)/ (length.out -1))), col = NA)
test$col <- bluefunction(length(test$colval))

edge2011$col <- NA
edge2014$col <- NA

for (i in 1:length(edge2011[ ,1])){
	for (j in 1:length(test[ ,1])) {
		if (edge2011$N[i] > test$colval[j] & edge2011$N[i] <= test$colval[j+1]) {
			edge2011$col[i] <- test$col[j]
		}	
	}
}
edge2011 <- edge2011[order(edge2011$N), ]

for (i in 1:length(edge2014[ ,1])){
	for (j in 1:length(test[ ,1])) {
		if (edge2014$plotN[i] > test$colval[j] & edge2014$plotN[i] <= test$colval[j+1]) {
			edge2014$col[i] <- test$col[j]
		}	
	}
}
edge2014 <- edge2014[order(edge2014$plotN),]



tiff(filename="CommuterAgreementAnalyses/flowmapall_ICVI_st_2011_v2.tiff",
 width = 140, height = 90, units = "mm", res=600, compression="lzw")
 par(mai=c(1,1,1,1))
 map('state', resolution=0, fill=TRUE, col="white", boundary="dark gray", lwd=0.5)
 map('state', region=c("Iowa", "Texas", "California", 
     "Minnesota", "New York", "North Carolina", "Wisconsin", "Nebraska"),
     resolution=0, col="black", add=TRUE, lwd=1.5)
N <- sqrt(edge2011$N) - 10
#N<-(log(edge2011$N+1, base=4)-0.4)*1.8
 for (i in 1:length(edge2011[,1])){	
 	lines(x=c(edge2011$Olongitude[i], edge2011$Dlongitude[i]),
    		y=c(edge2011$Olatitude[i], edge2011$Dlatitude[i]), lwd=N[i], col = edge2011$col[i])
 }
 dev.off()

tiff(filename="CommuterAgreementAnalyses/flowmapall_ca_st_2014_v2.tiff",
 width = 140, height = 90, units = "mm", res=600, compression="lzw")
 par(mai=c(1,1,1,1))
 map('state', resolution=0, fill=TRUE, col="white", boundary="dark gray", lwd=0.5)
 map('state', region=c("Iowa", "Michigan", "California", 
 	"New York", "North Carolina", "Wisconsin"),  # Nebraska
     resolution=0, col="black", add=TRUE, lwd=1.5)
Nval <- sqrt(edge2014$plotN) - 10
# N<-(log(edge2014$N+1, base=4)-0.4)*1.8
 for (i in 1:length(edge2014[,1])){	
 	lines(x = c(edge2014$Olongitude[i], edge2014$Dlongitude[i]),
       y = c(edge2014$Olatitude[i], edge2014$Dlatitude[i]), lwd = Nval[i], col=edge2014$col[i])
 }
 dev.off()
 # I think Nebraska looks like it is outlined here, but probably should not be.  
 # They told us they have 4 outgoing and 2 incoming agreements, but didn’t give us the data.  
 # We see some of it from the Iowa data, but I think we don’t have all of it.

library(shape)
tiff(filename = "CommuterAgreementAnalyses/flowmap_legend_v2.tiff")
plot(1, type = "n", axes = F, xlab = "", ylab = "")
colorlegend(col=test$col, zval = test$colval[c(1, 10, 20, 30, 40, 50)], zlim=c(1, max(test$colval)), log=FALSE, posx=c(0.7, 0.83), posy=c(0.1, 0.9), digit=0, cex=1)
 dev.off()
 
 
 
 
 