#########################################
#########################################
# Data read in
#########################################
#########################################
# Functions to make state (make_state_networks) and county level networks (make_networks)
source('~/Documents/post-doc/Swine/CommuterAgreementAnalyses/make_networks.R', chdir = TRUE)
library(igraph)
library(boot)

# Read in 2014 commuter agreement data
#########################################
ca <- read.csv("~/Documents/post-doc/Swine/CommuterAgreementAnalyses/SWINE Commuter Data - FTP site originals/Swine_Commuter_Data_31May2017_02.csv")
ca <- ca[ca$Year == "2014",]
# no longer an issue in this dataset
#errors <- c("Lowden", "IA50846", "IA52586")
#ca$D.state[ca$D.state %in% errors] <- "IA"
length(ca$O.state)  #10364 shipments total
table(ca$O.state, ca$D.state)
ca <- ca[!is.na(ca$D.state),]  # 321 removed
ca <- ca[!is.na(ca$O.state),]  # 5 removed
length(ca$O.state)  # 10043 shipments after cleaning
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
ca <- ca[!(ca$O.state.FIPS == ca$D.state.FIPS),]   #removed 7, now at 10030 records
ca <- ca[!(ca$D.state.FIPS == 19 & ca$D.FIPS == "46083"),]
ca <- ca[!is.na(ca$O.state.FIPS), ]

summary(ca)


# Read in ICVI data 
#########################################
dat <- read.csv("~/Documents/post-doc/Swine/Swine_cvi_final.csv")
dat <- dat[!is.na(dat$NUM_SWINE),]  #-1
dat <- dat[dat$NUM_SWINE>0,] # -13
dat <- dat[!is.na(dat$SAMPLE_YEAR2),]  #-38 ####Clay added new collum for year#####
dat <- dat[!is.na(dat$O_FIPS),]
dat <- dat[!is.na(dat$D_FIPS),]
summary(dat)

dat<-dat[, c("STATE", "SAMPLE_YEAR2", "PURPOSE", "NUM_SWINE", "NUM_BOAR", 
             "NUM_BARROW", "NUM_GILT", "NUM_SOW", "NUM_AGE_0.2_MONTHS", 
             "NUM_AGE_2.6_MONTHS", "NUM_AGE_6._MONTHS", "NUM_MALE", "NUM_FEMALE",
             "D_STATE", "D_FIPS_X", "D_FIPS_Y", "O_STATE", "D_FIPS", "O_FIPS", "O_ST_FIPS", "D_ST_FIPS" )]
dat$MOVE <- rep(1, length(dat[,1]))

data<-dat[dat$O_FIPS!=dat$D_FIPS,]  # REMOVE INTRACONTY SHIPMENTS!
length(data[data$O_ST_FIPS==data$D_ST_FIPS,])
cvi<-data[data$O_ST_FIPS!=data$D_ST_FIPS,] # REMOVE WITHIN STATE
cvi$MOVE<-1

cvi2010 <- cvi[cvi$SAMPLE_YEAR2=="2010",]
cvi2011 <- cvi[cvi$SAMPLE_YEAR2=="2011",]
cvi2011_noNE <- cvi[cvi$SAMPLE_YEAR2=="2011" & cvi$O_STATE != "NE",]

#########################################
#########################################
# Compare the size of shipments 
#########################################
#########################################
# sumary shipment size
summary(as.numeric(as.character(ca$NUM.SHIPPED)))
summary(as.numeric(as.character(cvi2010$NUM_SWINE)))
summary(as.numeric(as.character(cvi2011$NUM_SWINE)))
summary(as.numeric(as.character(cvi2011_noNE$NUM_SWINE)))

canum <- as.numeric(as.character(ca$NUM.SHIPPED))
canum <- na.exclude(canum)

# get bootstrap CI
nboot <- 1000
alpha <- 0.05
n <- 1000 # out of the 10014 shipments
bootfunction <- function(x, i) { median(x[i])}
boot.median <- boot(canum, bootfunction, R = nboot)
#boot.ci(boot.median, conf = 1-alpha)

med <- NA
for (i in 1:nboot) {
	data <-sample(canum, size =1000, replace = TRUE)
	med[i] <- median(data)
}
hist(med)
quantile(med, c(0.025, 0.975))


par(mfrow = c(1,3))
hist(canum, xlab = "Number of swine per shipment", main = "2014 Commuter Agreements")
hist(cvi2010$NUM_SWINE, xlab = "Number of swine per shipment", main = "2010 ICVI")
hist(cvi2011_noNE$NUM_SWINE, xlab = "Number of swine per shipment", main = "2011 ICVI (no NE)")

# even though similar numbers of large shipments occur in both the ICVI and CA datasets, 
# the CA dataset has more "mid-sized" shipment of 1000-3000 head

# need to break this down by state - to make sure differences are from differnces in sampling

# Percent of counties in IA, CA, WI, MI, NY, NC
length(unique(c(ca$O.FIPS[ca$O.state %in% c("IA", "CA", "WI", "MI", "NY", "NC")], 
	ca$D.FIPS[ca$D.state %in% c("IA", "CA", "WI", "MI", "NY", "NC")] ) ))
ntot.counties <- 99 + 58 + 72 + 83 + 62 + 100
100/ntot.counties

# Percent of counties in ICVIs: CA, IA, NE, TX, MN, WI, NC, NY

st <- unique(cvi2011$O_STATE)
length(unique(c(c(cvi2011$O_FIPS), c(cvi2011$D_FIPS[cvi2011$D_STATE %in% st]) )))
ntot.counties <- 99 + 58 +  72 + 62 + 100 + 93 + 254 + 87 #NE + TX + MN +

410/ntot.counties

#########################################
#########################################
# Age and Sex in the Commuter Agreement data
#########################################
#########################################
# Age
length(ca$AGE[is.na(ca$AGE)])/ length(ca$AGE)

# Over 50% of the shipments did not report information on sex of the shipment
length(ca$SEX[is.na(ca$SEX)])/length(ca$SEX)

temp <- data.frame(table(ca$SEX))
sum(temp$Freq[temp$Var1 %in% c("CF", "Gilts", "Sow")]) #490
sum(temp$Freq[temp$Var1 %in% c("Split", "mixed", "Mixed", "MIX", "Mix")]) #1193
sum(temp$Freq)

# cf = castrated female, gilts = female, sow = female


# WHAT DOES M mean in the SEX column?  (Kate/Ryan)
# If M = Male, then 36% of the shipmetn where sex is reported are mixed-sex shipments
table(ca$SEX)
(1591 + 452 + 206 + 77)/ length(ca$SEX[is.na(ca$SEX)])



test <- data.frame(table(ca$O.state, ca$D.state))



#########################################
#########################################
# Make Commuter Agreement Networks at the state level
#########################################
#########################################
states= sort(unique(c(ca$O.state.FIPS, ca$D.state.FIPS)))
# this makes some empty dataframes that we will later fill with information;

###################
node.stats=data.frame(matrix(NA,nrow=length(states),
		ncol=15,
		dimnames=list(NULL, c("NodeID",
		"Unweighted_InDeg","InDegree_Ship","InDegree_Swine", 				
		"Unweighted_OutDeg","OutDegree_Ship","OutDegree_Swine",
		"Unweighted_TotalDegree", "TotalDegree_Ship", "TotalDegree_Swine", "Betweenness",
		"Transitivity", "AveNearNeighDeg", "StrongClusters","WeakClusters"))))
net.stats=data.frame(matrix(0,nrow=1, ncol=13, dimnames=list(NULL,c("NumNodes", "NumEdges", 
		"NumEdges_unwt", "Diameter", "GSCCsize","GSCCdiameter", 		
		"GWCCsize","GWCCdiameter", "Reciprocity", "Assortativity",
		"Assortativity_Ship", "Assortativity_Swine", "GlobalTransitivity"))))

# Make network objects
###################
 temp_graph1=graph.edgelist(el=as.matrix(cbind(as.character(ca$O.state.FIPS), 
      		as.character(ca$D.state.FIPS))), directed=TRUE)         	# weighted by number of shipments
temp_graph2=graph.edgelist(el=as.matrix(unique(cbind( 
      		as.character(ca$O.state.FIPS),as.character(ca$D.state.FIPS)))), directed=TRUE)  #  not wt
temp_graph<-set.edge.attribute(temp_graph1,"weight", 
      		value=ca$NUM.SHIPPED) # weighted by number of swine

# fill in node stats
###################
node.stats$NodeID=states 
node.stats$Unweighted_InDeg = degree(temp_graph2, mode=c("in"))[order(as.numeric(V
	(temp_graph2)$name))]
node.stats$Unweighted_OutDeg = degree(temp_graph2,mode=c("out"))[order(
	as.numeric(V(temp_graph2)$name))]
node.stats$Unweighted_TotalDegree = node.stats$Unweighted_InDeg + node.stats$Unweighted_OutDeg
node.stats$InDegree_Ship = degree(temp_graph,mode=c("in"))[order
    	(as.numeric(V(temp_graph)$name))]
node.stats$OutDegree_Ship=degree(temp_graph,mode=c("out"))[order
    	(as.numeric(V(temp_graph)$name))]
node.stats$TotalDegree_Ship=node.stats$InDegree_Ship+node.stats$OutDegree_Ship
node.stats$InDegree_Swine=graph.strength(temp_graph,mode=c("in"))[order
    	(as.numeric(V(temp_graph)$name))]
node.stats$OutDegree_Swine=graph.strength(temp_graph,mode=c("out"))[order
    	(as.numeric(V(temp_graph)$name))]
node.stats$TotalDegree_Swine=node.stats$InDegree_Swine+node.stats$OutDegree_Swine
node.stats$Betweenness=betweenness(temp_graph2)[order
    	(as.numeric(V(temp_graph2)$name))] 
node.stats$Transitivity=transitivity(temp_graph,type=c("local"))[order
	(as.numeric(V(temp_graph)$name))]
node.stats$AveNearNeighDeg= graph.knn(temp_graph2)$knn[order
 		(as.numeric(V(temp_graph2)$name))]
temp.strong=clusters(temp_graph2,mode=c("strong"))
temp.weak=clusters(temp_graph2,mode=c("weak"))    # shouldn't matter which one used
node.stats$StrongClusters=temp.strong$membership[order(as.numeric(V(temp_graph)$name))]+1
node.stats$WeakClusters=temp.weak$membership[order(as.numeric(V(temp_graph)$name))]+1

# fill in net stats
###################
net.stats$NumNodes=length(node.stats$NodeID)
net.stats$NumEdges=  ecount(temp_graph)  #wt
net.stats$NumEdges_unwt= ecount(temp_graph2)
net.stats$Diameter=diameter(temp_graph1)
net.stats$Reciprocity=reciprocity(temp_graph)
	#net.stats$Assortativity_Ship= cor(node.stats$TotalDegree_Ship,node.stats$AveNearNeighDeg_Ship)
	#net.stats$Assortativity_Swine=cor(node.stats$TotalDegree_Swine,node.stats$AveNearNeighDeg_Swine)
net.stats$Assortivity= cor(node.stats$Unweighted_TotalDegree, node.stats$AveNearNeighDeg)
net.stats$GlobalTransitivity= transitivity(temp_graph,type=c("global"))

temp.nodes.strong=which(temp.strong$membership==which.max(temp.strong$csize))
temp.nodes.weak=which(temp.weak$membership==which.max(temp.weak$csize)) 

net.stats$GSCCsize=length(temp.nodes.strong)
net.stats$GSCCdiameter=diameter(induced.subgraph(temp_graph2,v=temp.nodes.strong))
net.stats$GWCCsize=length(temp.nodes.weak)
net.stats$GWCCdiameter=diameter(induced.subgraph(temp_graph2,v=temp.nodes.weak))  # check

# use write.csv command to save the results. 
###################
setwd("~/Documents/post-doc/Swine/CommuterAgreementAnalyses")
write.csv(node.stats, file = "swine_CA_state_node_stats.csv")
write.csv(net.stats, file = "swine_CA_state_network_stats.csv")


#########################################
#########################################
# Make Networks at the state level based on the 2014
# commuter agreement data and 2011
#########################################
#########################################
# Put commuter data in same order as ICVI
head(cvi2011)
cvired <- cvi2011[, which(colnames(cvi2011) %in% c("O_STATE", 
	"D_STATE", "O_ST_FIPS", "D_ST_FIPS", "NUM_SWINE"))]
cared <- ca[, which(colnames(ca) %in% c("O.state.FIPS", 
	"D.state.FIPS", "O.state",  "D.state", "NUM.SHIPPED"))]
cared <- cared[,c(1,3,2,4,5)]
colnames(cared) <- colnames(cvired)
data <- data.frame(rbind(cvired, cared))

setwd("~/Documents/post-doc/Swine/CommuterAgreementAnalyses")
make_state_networks(data, "swine_allCAandICVI")

make_state_networks(cvired, "swine_ICVI")

#########################################
#########################################
# Make Commuter Agreement Networks at the county level
#########################################
#########################################
cared <- ca[, which(colnames(ca) %in% c("O.state.FIPS", 
	"D.state.FIPS", "O.state",  "D.state", "NUM.SHIPPED", "O.FIPS", "D.FIPS"))]
# remove counties with no FIPS codes...  25% of the data!!!!!!!!!!!!!!!!
cared <- cared[!is.na(cared$O.FIPS),]  
cared <- cared[!is.na(cared$D.FIPS),]  # 8936
cared <- cared[,c(4,5,1,2, 3,6,7)]
colnames(cared) <- c("O_FIPS", "D_FIPS", "NUM_SWINE", "O_STATE", "D_STATE", "O_ST_FIPS", "D_ST_FIPS")
# one county is in two different states!  County 27045 in state 27 and 19...
# remove 27045's that are confused and going to Iowa? or turn them into going to MN

setwd("~/Documents/post-doc/Swine/CommuterAgreementAnalyses")
makenetworks(cared, "swine_CA")


#########################################
#########################################
# Make Commuter Agreement and ICVI networks at the county level
#########################################
#########################################
cvired <- cvi2011[, which(colnames(cvi2011) %in% c("O_STATE", 
	"D_STATE", "O_ST_FIPS", "D_ST_FIPS", "NUM_SWINE", "O_FIPS", "D_FIPS"))]
cvired <- cvired[,c(5,4,1,3,2,6,7)]
data <- data.frame(rbind(cvired, cared))

setwd("~/Documents/post-doc/Swine/CommuterAgreementAnalyses")
makenetworks(data, "swine_allCAandICVI")

setwd("~/Documents/post-doc/Swine/CommuterAgreementAnalyses")
makenetworks(cvired, "swine_ICVI")

density = function(nodes, edges){edges/(nodes*(nodes-1)/2)}
density(811, 1764)
density(851, 2369)
density(18, 29)
density(48, 192)
density(48, 205)






