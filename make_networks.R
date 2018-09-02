#################################################
#################################################
# Goal of this code is to Generate network statistics
# for swine movement networks
# We generate networks based on 3 datasets 
# (2010, 2011 and a 2011 without Nebraska)
# Save results to our working directory as net_stats_2010.csv ect. 
#################################################
#################################################
# Outline
# Step 1:  Set your working directory and install
# Step 2:  Read in the data and remove/change any wierd things
# Step 3: Make networks
#################################################
#################################################


#################################################
# Step 1:  Set your working directory and install
# packages needed to create a network
#################################################
# set working directory.
setwd("~/Documents/post-doc/Swine")

library(igraph)  # igraph is the package that makes networks
library(plyr)      #  plyr helps manipulate dataframes


#################################################
# Step 2:  Read in the data and remove/change any wierd things
#################################################
dat = read.csv("Swine_cvi_final.csv")
dat = dat[!is.na(dat$NUM_SWINE),]  #-1
dat = dat[dat$NUM_SWINE>0,] # -13
dat = dat[!is.na(dat$SAMPLE_YEAR2),]  #-38 #Clay added new col for year
dat = dat[!is.na(dat$O_FIPS),]
dat = dat[!is.na(dat$D_FIPS),]
dat = dat[dat$NUM_SWINE>0,]

summary(dat)

# make a new column of 1s that represents the number of shipments
dat$MOVE <-1

dat<-dat[, c("STATE", "SAMPLE_YEAR2", "PURPOSE", 
	"NUM_SWINE", "NUM_BOAR", "NUM_BARROW", "NUM_GILT", 
	"NUM_SOW", "NUM_AGE_0.2_MONTHS", 
	"NUM_AGE_2.6_MONTHS", "NUM_AGE_6._MONTHS", 
	"NUM_MALE", "NUM_FEMALE", "D_STATE", "D_FIPS_X",
	"D_FIPS_Y", "O_STATE", "D_FIPS", "O_FIPS", 
	"O_ST_FIPS", "D_ST_FIPS")]
summary(dat)

# Proxy for non-slaughter, out-of state shipments!
data = dat[dat$PURPOSE != "Slaughter",]
data<-data[!is.na(data$D_FIPS),]
data<-data[data$O_FIPS!=data$D_FIPS,]  # REMOVE INTRASTATE SHIPMENTS!
length(data[data$O_ST_FIPS==data$D_ST_FIPS,])
data<-data[data$O_ST_FIPS!=data$D_ST_FIPS,]

# make networks for
#1) 2010
data2010=data[data$SAMPLE_YEAR2==2010,]
data2010 <- data2010[data2010$O_STATE != "NE",]
#2) 2011 all stats
data2011=data[data$SAMPLE_YEAR2==2011,]
#3) 2011, no Nebraska
datared2011= data2011[data2011$STATE!="NE",]


#################################################
# Step 3:  Make Networks, function to apply to each subset of the data 
#################################################
 makenetworks<-function(datared, filename){
	#############################################
	# Input: datared= dataset used to make network.  
	# Must have columns from the main swine spreadsheet. 
	# filename= a character string used identify the
	#  network made - appended to output filename.
	# Output: 2 spreadsheets, net_stats with the 
	# network properties and node_stats with the node properties	
	###############################################
 	counties = unique(cbind(c(datared$O_ST_FIPS, 
 		datared$D_ST_FIPS), c(datared$O_FIPS, datared$D_FIPS))) 
 	counties = counties[order(counties[,2]),]

	# this makes some empty dataframes that we will 
	# later fill with information;
 	node.stats = data.frame(matrix(NA, nrow = length(counties[,1]),
		ncol = 18,
		dimnames = list(NULL, c("StateID","NodeID",
		"Unweighted_InDeg","InDegree_Ship","InDegree_Swine", 		"Unweighted_OutDeg","OutDegree_Ship","OutDegree_Swine",
		"Unweighted_TotalDegree", "TotalDegree_Ship", 
		"TotalDegree_Swine", "Betweenness", "Transitivity", 
		"AveNearNeighDeg", "AveNearNeighDeg_Ship", 
		"AveNearNeighDeg_Swine", "StrongClusters","WeakClusters"))))
	net.stats = data.frame(matrix(0, nrow = 1, ncol = 13,
		dimnames = list(NULL, c("NumNodes", "NumEdges", 
		"NumEdges_unwt", "Diameter", "GSCCsize","GSCCdiameter", 		
		"GWCCsize","GWCCdiameter", "Reciprocity", "Assortativity",
		"Assortativity_Ship", "Assortativity_Swine", 
		"GlobalTransitivity"))))

	# Calculate the pieces to fill node.stats
	node.stats$NodeID = counties[,2]
	node.stats$StateID = counties[,1]

	# Make network objects
	# weighted by number of shipments
	temp_graph1 = graph.edgelist(el = 
		as.matrix(cbind(as.character(datared$O_FIPS), 
		as.character(datared$D_FIPS))), directed = TRUE)  
	# not weighted
	#temp_graph2 = graph.edgelist(el = 
	#	as.matrix(unique(cbind(as.character(datared$O_FIPS), 
	#	as.character(datared$D_FIPS)))), directed = TRUE)        
	temp_graph2 = graph.edgelist(el = 
		as.matrix(unique(cbind(as.character(datared$O_FIPS), 
		as.character(datared$D_FIPS)))), directed = FALSE)
	temp_graph2 <- simplify(temp_graph2, remove.multiple = TRUE, remove.loops = TRUE)        
	# weighted by the number of swine	 
	temp_graph <- set.edge.attribute(temp_graph1, "weight", 
		value = datared$NUM_SWINE)

	# Calculate node statistics  
	node.stats$Unweighted_InDeg = degree(
		temp_graph2,mode=c("in"))[order(as.numeric(V(temp_graph2)$name))]
	node.stats$Unweighted_OutDeg = degree(
		temp_graph2,mode=c("out"))[order(as.numeric(V(temp_graph2)$name))]
	node.stats$Unweighted_TotalDegree = node.stats$Unweighted_InDeg + 
		node.stats$Unweighted_OutDeg
    node.stats$InDegree_Ship = degree(
    		temp_graph,mode=c("in"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Ship = degree(
    	temp_graph,mode=c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Ship = node.stats$InDegree_Ship + 
    		node.stats$OutDegree_Ship
    node.stats$InDegree_Swine = graph.strength(temp_graph, 
    		mode = c("in"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Swine = graph.strength(
    		temp_graph,mode=c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Swine = node.stats$InDegree_Swine + 
    		node.stats$OutDegree_Swine
    node.stats$Betweenness = betweenness(
    		temp_graph2)[order(as.numeric(V(temp_graph2)$name))] 
	node.stats$Transitivity = transitivity(
		temp_graph,type = c("local"))[order(as.numeric(V(temp_graph)$name))]
 	node.stats$AveNearNeighDeg = graph.knn(
 		simplify(temp_graph2))$knn[order(
 		as.numeric(V(simplify(temp_graph2))$name))]
 	#node.stats$AveNearNeighDeg_Ship= graph.knn(temp_graph1$knn
 	#	[order(as.numeric(V(temp_graph1)$name))]
 	#node.stats$AveNearNeighDeg_Swine= graph.knn(
 	# temp_graph,weights=E(temp_graph)$weight)$knn
 	#	[order(as.numeric(V(temp_graph)$name))]
	temp.strong = clusters(temp_graph2, mode = c("strong"))
	temp.weak = clusters(temp_graph2, mode = c("weak"))    
	node.stats$StrongClusters = temp.strong$membership[
		order(as.numeric(V(temp_graph)$name))] + 1
	node.stats$WeakClusters = temp.weak$membership[
		order(as.numeric(V(temp_graph)$name))] + 1

	# Calculate network statistics
    net.stats$NumNodes = length(node.stats$NodeID)
    net.stats$NumEdges = ecount(temp_graph)  #wt
 	net.stats$NumEdges_unwt = ecount(temp_graph2)
 	net.stats$Diameter = diameter(temp_graph1)
	net.stats$Reciprocity = reciprocity(temp_graph)
	#net.stats$Assortativity_Ship = cor(
	#	node.stats$TotalDegree_Ship,node.stats$AveNearNeighDeg_Ship)
	#net.stats$Assortativity_Swine = cor(
	#	node.stats$TotalDegree_Swine,node.stats$AveNearNeighDeg_Swine)
	net.stats$Assortivity = cor(
		node.stats$Unweighted_TotalDegree, node.stats$AveNearNeighDeg)
	net.stats$GlobalTransitivity = transitivity(temp_graph,type = c("global"))
	 temp.nodes.strong = which(temp.strong$membership == which.max(
	 	temp.strong$csize))
     temp.nodes.weak = which(temp.weak$membership == which.max(
     	temp.weak$csize)) 
    net.stats$GSCCsize = length(temp.nodes.strong)
    net.stats$GSCCdiameter = diameter(
    		induced.subgraph(temp_graph2,v=temp.nodes.strong))
    net.stats$GWCCsize = length(temp.nodes.weak)
    net.stats$GWCCdiameter = diameter(
    		induced.subgraph(temp_graph2, v = temp.nodes.weak))  # check

	# save the results. 
	###################
	write.csv(node.stats, file=paste("node_stats_", filename, ".csv", sep=""))
	write.csv(net.stats, file=paste("net_stats_", filename, ".csv", sep=""))
}

#################################################
# Step 4: Apply function
################################################# 
makenetworks(data2010, filename="2010")
makenetworks(data2011, filename="2011all")
makenetworks(datared2011, filename="2011noNE")

# network stats only including subset of states 
datastates <- c(19, 6, 27, 31, 36, 37, 48, 55)  # IA=19,TX=48,CA=6,MN=27,NE=31,NY=36,NC=37,WI=55
datastatesnoNE <- c(19, 6, 27, 36, 37, 48, 55)
datasub2010 = data2010[data2010$D_ST_FIPS %in% datastatesnoNE,]
datasub2010 = datasub2010[datasub2010$O_ST_FIPS %in% datastatesnoNE,]
datasub2011 = data2011[data2011$D_ST_FIPS %in% datastates,]
datasubred2011 = datared2011[datared2011$D_ST_FIPS %in% datastatesnoNE,]
datasubred2011 = datasubred2011[datasubred2011$O_ST_FIPS %in% datastatesnoNE,]
makenetworks(datasub2010, filename = "sub2010")
makenetworks(datasub2011, filename ="sub2011all")
makenetworks(datasubred2011, filename ="sub2011noNE")

dens = function(e, n) {
	2*e / (n * (n-1))
}
# subset data
dens(695, 261)
dens(600, 248)
dens(901, 321)
#full data
dens(1364, 676)
dens(1345, 725)
dens(1709, 809)

#################################################
# Step 5:  State networks
#################################################
make_state_networks = function(datared, filename){
	
	counties = sort(unique(c(datared$O_ST_FIPS, datared$D_ST_FIPS)))

	# makes empty dataframes to fill later
 	node.stats = data.frame(matrix(NA, nrow = length(counties),
		ncol=18,
		dimnames = list(NULL, c("StateID","NodeID",
		"Unweighted_InDeg","InDegree_Ship","InDegree_Swine", 		"Unweighted_OutDeg","OutDegree_Ship","OutDegree_Swine",
		"Unweighted_TotalDegree", "TotalDegree_Ship", 
		"TotalDegree_Swine", "Betweenness", "Transitivity", 
		"AveNearNeighDeg", "AveNearNeighDeg_Ship", 
		"AveNearNeighDeg_Swine", "StrongClusters", "WeakClusters"))))
	net.stats = data.frame(matrix(0, nrow = 1, ncol = 13,
		dimnames = list(NULL, c("NumNodes", "NumEdges", 
		"NumEdges_unwt", "Diameter", "GSCCsize","GSCCdiameter", 		
		"GWCCsize","GWCCdiameter", "Reciprocity", "Assortativity",
		"Assortativity_Ship", "Assortativity_Swine", "GlobalTransitivity"))))

	# Make network objects
	# Weighted by number of shipments
    temp_graph1 = graph.edgelist(el = 
    		as.matrix(cbind(as.character(datared$O_ST_FIPS), 
      	as.character(datared$D_ST_FIPS))), directed = TRUE)
	# Not weighted
	# temp_graph2 = graph.edgelist(el = as.matrix(unique(cbind( 
    #  	as.character(datared$O_ST_FIPS), 
    #  	as.character(datared$D_ST_FIPS)))), directed = TRUE) 
	temp_graph2 = graph.edgelist(el = as.matrix(unique(cbind( 
      	as.character(datared$O_ST_FIPS), 
      	as.character(datared$D_ST_FIPS)))), directed = FALSE) 
	temp_graph2 <- simplify(temp_graph2, remove.multiple = TRUE, remove.loops = TRUE)        
    # Weighted by the number of swine
    temp_graph <- set.edge.attribute(temp_graph1, "weight", 
      		value = datared$NUM_SWINE) 

	# fill in node stats
	node.stats$NodeID = counties 
	node.stats$Unweighted_InDeg = degree(temp_graph2, 
		mode = c("in"))[order(as.numeric(V(temp_graph2)$name))]
	node.stats$Unweighted_OutDeg = degree(temp_graph2, mode = 
		c("out"))[order(as.numeric(V(temp_graph2)$name))]
	node.stats$Unweighted_TotalDegree = node.stats$Unweighted_InDeg + 
		node.stats$Unweighted_OutDeg
    node.stats$InDegree_Ship = degree(
    		temp_graph,mode = c("in"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Ship = degree(
    		temp_graph,mode = c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Ship = node.stats$InDegree_Ship + 
    		node.stats$OutDegree_Ship
    node.stats$InDegree_Swine = graph.strength(
    		temp_graph,mode = c("in"))[order	(as.numeric(V(temp_graph)$name))]
    node.stats$OutDegree_Swine = graph.strength(
    		temp_graph, mode = c("out"))[order(as.numeric(V(temp_graph)$name))]
    node.stats$TotalDegree_Swine = node.stats$InDegree_Swine + 
    		node.stats$OutDegree_Swine
    node.stats$Betweenness = betweenness(
    		temp_graph2)[order(as.numeric(V(temp_graph2)$name))] 
	node.stats$Transitivity = transitivity(
		temp_graph, type = c("local"))[order(as.numeric(V(temp_graph)$name))]
	node.stats$AveNearNeighDeg = graph.knn(
		temp_graph2)$knn[order(as.numeric(V(temp_graph2)$name))]
	temp.strong = clusters(temp_graph2, mode = c("strong"))
	temp.weak = clusters(temp_graph2, mode=c("weak"))  
	node.stats$StrongClusters = temp.strong$membership[
		order(as.numeric(V(temp_graph)$name))] + 1
	node.stats$WeakClusters = temp.weak$membership[
		order(as.numeric(V(temp_graph)$name))] + 1

	# fill in net stats
	net.stats$NumNodes = length(node.stats$NodeID)
    net.stats$NumEdges = ecount(temp_graph)
 	net.stats$NumEdges_unwt = ecount(temp_graph2)
 	net.stats$Diameter = diameter(temp_graph1)
	net.stats$Reciprocity = reciprocity(temp_graph)
	#net.stats$Assortativity_Ship= cor(
	#	node.stats$TotalDegree_Ship, node.stats$AveNearNeighDeg_Ship)
	#net.stats$Assortativity_Swine = cor(
	#	node.stats$TotalDegree_Swine, node.stats$AveNearNeighDeg_Swine)
	net.stats$Assortivity = cor(
		node.stats$Unweighted_TotalDegree, node.stats$AveNearNeighDeg)
	net.stats$GlobalTransitivity= transitivity(temp_graph,type=c("global"))
	temp.nodes.strong = which(
	 	temp.strong$membership==which.max(temp.strong$csize))
    temp.nodes.weak = which(temp.weak$membership==which.max(temp.weak$csize)) 
    net.stats$GSCCsize = length(temp.nodes.strong)
    net.stats$GSCCdiameter = diameter(
    		induced.subgraph(temp_graph2, v = temp.nodes.strong))
    net.stats$GWCCsize = length(temp.nodes.weak)
    net.stats$GWCCdiameter = diameter(
    		induced.subgraph(temp_graph2, v = temp.nodes.weak))

	# save the results. 
	write.csv(node.stats, file = paste("node_stats_st_", 
		filename, ".csv", sep=""))
	write.csv(net.stats, file = paste("net_stats_st_", 
		filename, ".csv", sep=""))
}

make_state_networks(data2010, filename="2010")
make_state_networks(data2011, filename="2011all")
make_state_networks(datared2011, filename="2011noNE")

# need to hash out ave nearest neighbor degree and net.assortivity.
make_state_networks(datasub2010, filename="sub2010")
make_state_networks(datasub2011, filename="sub2011all")
make_state_networks(datasubred2011, filename="sub2011noNE")

dens(175, 48)
dens(154, 48)
dens(136, 45)

dens(22, 8)
dens(17, 7)
dens(17, 7)