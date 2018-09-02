# NEED TO REMOVE WITHIN STATE, WIHTIN COUNTY, SLAUGHTER

####################################################################
####################################################################
####################################################################
# Spatio-temporal patterns and characteristics of swine shipments in the U.S. 
# based on Interstate Certificates of Veterinary Inspection
# Code to make figures and maps of swine CVI data
# Erin E. Gorsich
####################################################################
####################################################################
####################################################################
library(maps)
library(igraph)
library(plyr)
library(RColorBrewer)
library(plotrix)
library(SDMTools)
library(shape)
library(sp)

####################################################################
####################################################################
# Figure 1: Histogram of shipment size, overall
####################################################################
####################################################################
# read in data
setwd("~/Documents/post-doc/Swine")
# Read in data
data.cvi <- read.csv("Swine_cvi_final.csv")
data.cvi <- data.cvi[!is.na(data.cvi$NUM_SWINE), ]  #-1
data.cvi <- data.cvi[data.cvi$NUM_SWINE > 0, ] # -13
data.cvi <- data.cvi[!is.na(data.cvi$SAMPLE_YEAR2), ]  #-38 
data.cvi <- data.cvi[!is.na(data.cvi $O_FIPS), ]
data.cvi <- data.cvi[!is.na(data.cvi $D_FIPS), ]
data.cvi <- data.cvi[data.cvi$NUM_SWINE > 0, ]

# proxy for non-slaughter out-of-state shipments
data.cvi <-data.cvi[data.cvi$O_FIPS!= data.cvi$D_FIPS,]  
data.cvi <-data.cvi[data.cvi$O_ST_FIPS!= data.cvi$D_ST_FIPS,]
data.cvi <- data.cvi[data.cvi$PURPOSE != "Slaughter",]

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

 
# Subset cvi data by year.
data10 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2010",]
data11 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2011",]
data.cvi$MOVE <- 1
data10$MOVE <- 1
data11$MOVE <- 1
alldata <- data.cvi$NUM_SWINE

# make data = data.cvi without Nebraska
data11red <- data11[data11$O_STATE != "NE",]
data <- rbind(data10, data11red)  # new change


# this function makes a histogram, and saves it to your working directory
# used in exploratory analyses to look at different subsets of the data.
my.histogram.maker<-function(data, filename){
  	swine.hist =  hist(data, plot=FALSE) 
	tiff(paste(filename, ".tiff", sep=""), 
		width=9, height=7, units="in", res=600)
	par(mar=c(6,6,4,2))
  	max.x = max(swine.hist$breaks)
  	max.y = max(swine.hist$counts)
  	barplot(swine.hist$counts, width = 1, space = 0, main = NULL, 
  		xlab="", ylim=c(0,max.y+10), ylab="", cex.lab = 1.5, 
  		cex.axis = 1.4, bty = "n", col = "darkgray", las = 1)
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
  		col = "darkgray", las = 1, tcl = -0.2)
  	axis(side = 1, at = seq(1, (length(swine.hist100$breaks)-1)),
  		labels = swine.hist100$breaks[-1], cex.lab = 1.2, tcl = -0.2)
  	dev.off()
}

my.histogram.maker(alldata, "Figure1")

####################################################################
####################################################################
# Figure 2: Purpose/ Production type, made in prism
####################################################################
####################################################################
# figure 2 (see demographic_purpose_analyses for analyses)
table(data10$PURPOSE)
table(data11red $PURPOSE)

####################################################################
####################################################################
# Figure 3: Age and sex patterns, made in prism
####################################################################
####################################################################
# figure 3 (see demographic_purpose_analyses for analyses)


####################################################################
####################################################################
# FIGURE 4.1 Weighted outdegree at county-level, 2010
####################################################################
####################################################################
# all these figures use full dataset 
data2010 <- data10
data2011 <- data11

# make networks
counties <- unique(cbind(c(data2010$O_ST_FIPS, data2010$D_ST_FIPS), c(data2010$O_FIPS, data2010$D_FIPS)))  
counties <- counties[order(counties[,2]),]
node.stats <- data.frame(matrix(NA,nrow=length(counties[,1]),
    ncol=12,
    dimnames=list(NULL, c("StateID","NodeID",
        "Unweighted_InDeg","InDegree_Ship","InDegree_Swine", 							 							
        "Unweighted_OutDeg","OutDegree_Ship","OutDegree_Swine", 
        "TotalDegree_Ship", "TotalDegree_Swine", "Betweenness",
        "Transitivity"))))	
node.stats$NodeID <- counties[,2]
node.stats$StateID <- counties[,1]
summary(node.stats)  

temp_graph1 <- graph.edgelist(el = as.matrix(cbind(as.character(data2010$O_FIPS), as.character(data2010$D_FIPS))), 
													directed=TRUE)
temp_graph2 <- graph.edgelist(el = as.matrix(unique(cbind(as.character(data2010$O_FIPS), as.character(data2010$D_FIPS)))),
													 directed=TRUE) 
temp_graph <- set.edge.attribute(temp_graph1,"weight",value = data2010$NUM_SWINE)					
temp_graph_st <- graph.edgelist(el=as.matrix(cbind(as.character(data2010$O_ST_FIPS), as.character(data2010$D_ST_FIPS))), directed=TRUE)

# Calculate node statistics  
node.stats$Unweighted_InDeg <- degree(temp_graph2, mode = c("in"))[order(as.numeric(V(temp_graph2)$name))]
node.stats$Unweighted_OutDeg <- degree(temp_graph2, mode = c("out"))[order(as.numeric(V(temp_graph2)$name))]
node.stats$InDegree_Ship <- degree(temp_graph, mode = c("in"))[order(as.numeric(V(temp_graph)$name))]
node.stats$OutDegree_Ship <- degree(temp_graph, mode = c("out"))[order(as.numeric(V(temp_graph)$name))]
node.stats$TotalDegree_Ship <- node.stats$InDegree_Ship + node.stats$OutDegree_Ship
node.stats$InDegree_Swine <- graph.strength(temp_graph, mode = c("in"))[order(as.numeric(V(temp_graph)$name))]
node.stats$OutDegree_Swine <- graph.strength(temp_graph, mode = c("out"))[order(as.numeric(V(temp_graph)$name))]
node.stats$TotalDegree_Swine <- node.stats$InDegree_Swine + node.stats$OutDegree_Swine
node.stats$Betweenness <- betweenness(temp_graph2)[order(as.numeric(V(temp_graph2)$name))] 
node.stats$Transitivity <- transitivity(temp_graph,type=c("local"))[order(as.numeric(V(temp_graph)$name))]

#plot
ctname <- map('county', resolution=0, plot=FALSE)$names
ctname <- as.matrix(ctname)
data(county.fips)
node.stats$COUNTY_NAME_R <- county.fips$polyname[match(node.stats$NodeID, county.fips$fips)]
name <- data.frame(ctname = ctname, OutDegreeShip = NA, OutDegreeSwine = NA)
name$OutDegreeShip <- node.stats$OutDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeSwine <- node.stats$OutDegree_Swine[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeShip[is.na(name$OutDegreeShip)] <- 0
name$OutDegreeSwine[is.na(name$OutDegreeSwine)] <- 0

# 1) range of one color
#cols <- colorRampPalette(brewer.pal(9, "YlGnBu"))(172)   # colors for level plots
#colmatch<-data.frame(num=seq(1,172,1), col=cols)
#name$col<-as.character(colmatch$col[match(name$OutDegreeShip, colmatch$num)])
#name$col[is.na(name$col)]<-"#FFFFFF"

#par(mai=c(1,1,1,5))
#map('county', resolution=0, lwd=0.5, col="dark gray")
#map('county', resolution=0, fill=TRUE, col=name$col, boundary="light gray", lwd=0.5, add=TRUE)
#map('state', add=TRUE)
#map('state', region=c("Iowa", "Texas", "California", 
#   "Minnesota", "New York", "North Carolina", "Wisconsin"),
#    resolution=0, col="dark blue", add=TRUE, lwd=2)
#leg.txt<-rep("", 172); leg.txt[1]<-1; leg.txt[172]<-172
#color.legend(11, 6, 11.8, 9, leg.txt, rect.col=cols)

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

tiff('paperdrafts_swine/swine_od_countylevel_2010_test.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
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

####################################################################
####################################################################
# FIGURE 4.2 Weighted indegree at county-level, 2010
####################################################################
####################################################################
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

tiff('paperdrafts_swine/swine_id_countylevel_2010_nolegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
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

####################################################################
####################################################################
# FIGURE 4.3 betweenness at county-level, 2010
####################################################################
####################################################################
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

tiff('paperdrafts_swine/swine_btwn_countylevel_2010_nolegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
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

####################################################################
####################################################################
# FIGURE 4.4 Weighted outdegree at county-level, 2011
####################################################################
####################################################################

# make networks
counties <- unique(cbind(c(data2011$O_ST_FIPS, data2011$D_ST_FIPS), c(data2011$O_FIPS, data2011$D_FIPS)))  
counties <- counties[order(counties[,2]),]
 node.stats <- data.frame(matrix(NA,nrow = length(counties[,1]),
                             ncol = 12,
                             dimnames = list(NULL, c("StateID","NodeID",
                             "Unweighted_InDeg","InDegree_Ship","InDegree_Swine", 							 							"Unweighted_OutDeg","OutDegree_Ship","OutDegree_Swine",
 							"TotalDegree_Ship", "TotalDegree_Swine", "Betweenness",
 							"Transitivity"))))	
node.stats$NodeID <- counties[,2]
node.stats$StateID <- counties[,1]
# It is good practice to watch what you do as you go.  Type summary to see what was done. 
summary(node.stats)  

temp_graph1 <- graph.edgelist(el = as.matrix(cbind(as.character(data2011$O_FIPS), as.character(data2011$D_FIPS))), directed = TRUE)
temp_graph2 <- graph.edgelist(el = as.matrix(unique(cbind(as.character(data2011$O_FIPS), as.character(data2011$D_FIPS)))), directed = TRUE)
temp_graph <- set.edge.attribute(temp_graph1,"weight", value = data2011$NUM_SWINE)                   										
temp_graph_st <- graph.edgelist(el = as.matrix(cbind(as.character(data2011$O_ST_FIPS), as.character(data2011$D_ST_FIPS))), directed = TRUE)

# Calculate node statistics  
node.stats$Unweighted_InDeg <- degree(temp_graph2, mode = c("in"))[order(as.numeric(V(temp_graph2)$name))]
node.stats$Unweighted_OutDeg <- degree(temp_graph2, mode = c("out"))[order(as.numeric(V(temp_graph2)$name))]
node.stats$InDegree_Ship <- degree(temp_graph, mode = c("in"))[order(as.numeric(V(temp_graph)$name))]
node.stats$OutDegree_Ship <-degree(temp_graph, mode = c("out"))[order(as.numeric(V(temp_graph)$name))]
node.stats$TotalDegree_Ship <- node.stats$InDegree_Ship + node.stats$OutDegree_Ship
node.stats$InDegree_Swine <- graph.strength(temp_graph, mode = c("in"))[order(as.numeric(V(temp_graph)$name))]
node.stats$OutDegree_Swine <- graph.strength(temp_graph,mode = c("out"))[order(as.numeric(V(temp_graph)$name))]
node.stats$TotalDegree_Swine <- node.stats$InDegree_Swine+node.stats$OutDegree_Swine
node.stats$Betweenness <- betweenness(temp_graph2)[order(as.numeric(V(temp_graph2)$name))] 
node.stats$Transitivity <- transitivity(temp_graph, type = c("local"))[order(as.numeric(V(temp_graph)$name))]

#plot
ctname <- map('county', resolution=0, plot=FALSE)$names
ctname <- as.matrix(ctname)
data(county.fips)
node.stats$COUNTY_NAME_R <- county.fips$polyname[match(node.stats$NodeID, county.fips$fips)]
name <- data.frame(ctname = ctname, OutDegreeShip=NA, OutDegreeSwine=NA)
name$OutDegreeShip <- node.stats$OutDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeSwine <- node.stats$OutDegree_Ship[match(name$ctname, node.stats$COUNTY_NAME_R)]
name$OutDegreeShip[is.na(name$OutDegreeShip)] <- 0
name$OutDegreeSwine[is.na(name$OutDegreeSwine)] <- 0

# on log scale
# 1) range of one color
cols <- colorRampPalette(brewer.pal(9, "YlGnBu"))(6)   # colors for level plots
colmatch <- data.frame(num=seq(0,6,1), col=c("white", cols))
name$col <- as.character(colmatch$col[match(name$OutDegreeShip, colmatch$num)])
name$col[is.na(name$col)] <- "#FFFFFF"
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

tiff('paperdrafts_swine/swine_od_countylevel_2011_updatelegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
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

####################################################################
####################################################################
# FIGURE 4.5 Weighted indegree at county-level, 2011
####################################################################
####################################################################
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

tiff('paperdrafts_swine/swine_id_countylevel_2011_updatelegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$colID, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)
# state level map legends use:
colorlegend(col=cols[2:6], zval=c(0, 2.8, 7.4, 20.1, 54.6, 148.4), zlim=c(1, 148), log=TRUE, posx=c(0.8, 0.83), posy=c(0.22, 0.6), digit=0, cex=0.2)
dev.off()

####################################################################
####################################################################
# FIGURE 4.6 Betweenness, 2011
####################################################################
####################################################################
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

tiff('paperdrafts_swine/swine_btwn_countylevel_2011_updatelegend.tiff',res=600,height=90,width=140,units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$colB, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
map('state', region=c("Iowa", "Texas", "California", 
    "Minnesota", "New York", "North Carolina", "Wisconsin", "Nebraska"),
    resolution=0, col="dark blue", add=TRUE, lwd=1)
# state level map legends use:
colorlegend(col = cols[3:9], zval = c(1, 2, 3, 4, 5, 6, 7),
	zlim = c(0, 7), log =FALSE, posx = c(0.8, 0.83), posy = c(0.22, 0.6), digit = 0, cex = 0.2)
#colorlegend(col = cols[3:9], zval = c(1, 4.48, 20.09, 90.02, 403.43, 1808, 8103, 98715), 
#	zlim = c(1, 98715), log = TRUE, posx = c(0.8, 0.83), posy = c(0.22, 0.6), digit = 0, cex = 0.2)
#colorlegend(col = cols[3:9], zval = c(0, 4.48, 20.09, 90.02, 403.43, 1808, 8103), 
#	zlim = c(1, 8103), log = TRUE, posx = c(0.8, 0.83), posy = c(0.22, 0.6), digit = 0, cex = 0.2)
dev.off()



###############################################
#############################################
# Figure 5 MAPS OF IN-DEGREE AND OUT DEGREE AT STATE LEVEL
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
