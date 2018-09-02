########################################
########################################
# Outline
########################################
########################################
# 1) Load Data
# 2) Calculate correlation between terms, choose covariates

########################################
########################################
setwd("~/Documents/post-doc/Swine")

########################################
########################################
# 1) Load Data
########################################
########################################

# Commuter Agreement Number of Shipments - Network statistics
########################################
ca <- read.csv("~/Documents/post-doc/Swine/CommuterAgreementAnalyses/node_stats_swine_CA.csv")

# Raw commuter agreement data
########################################
caraw <-read.csv("~/Documents/post-doc/Swine/CommuterAgreementAnalyses/SWINE Commuter Data - FTP site originals/Swine_Commuter_Data_31May2017_02.csv")
caraw <- caraw[caraw$Year == "2014",]
caraw <- caraw[!is.na(caraw$D.state),]  # 321 removed
caraw <- caraw[!is.na(caraw$O.state),]  # 5 removed
caraw <- caraw[!is.na(caraw$NUM.SHIPPED),]  # 6 removed
caraw$O.state <- as.character(caraw$O.state)
caraw$D.state <- as.character(caraw$D.state)
caraw$D.state[caraw$D.FIPS == 27045 & !is.na(caraw$D.FIPS)] <- "MN"

statefips <- read.csv("~/Documents/post-doc/2009-2011 comparison final/county_centroid_coordinates.csv")
statefips$stab <- as.character(statefips$stab)
caraw$O.state.FIPS <- statefips$ST_FIPS[match(caraw$O.state, statefips$stab)]
caraw$D.state.FIPS <-  statefips$ST_FIPS[match(caraw$D.state, statefips$stab)]
caraw <- caraw[!(caraw$O.state.FIPS == caraw$D.state.FIPS),]   #removed 7, now at 10030 records

# ICVI number of shipments
########################################
cvi <- read.csv("~/Documents/post-doc/Swine/node_stats_2011all.csv")

# Raw ICVI data
########################################
data.cvi <- read.csv("Swine_cvi_final.csv")
data.cvi <- data.cvi[!is.na(data.cvi$NUM_SWINE), ]  #-1
data.cvi <- data.cvi[data.cvi$NUM_SWINE > 0, ] # -13
data.cvi <- data.cvi[!is.na(data.cvi$SAMPLE_YEAR2), ]  #-38 
data.cvi <- data.cvi[!is.na(data.cvi $O_FIPS), ]
data.cvi <- data.cvi[!is.na(data.cvi $D_FIPS), ]
data.cvi <- data.cvi[data.cvi$NUM_SWINE > 0, ]
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
 
cvi2011 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2011",]

# NASS Co-variates 
########################################
##################
## functions used
newcol<-NA
standardize<-function(column){
	for (i in 1:length(column)){
	newcol[i]<-(column[i]-mean(column))/sd(column)
}
	return(newcol)
}

newdataframe<-NA
get_useful_parts<-function(dataframe){
	##########################
	### This function takes a dataframe.  It
	#  1) extracts the useful columns (value, State/County ID/ Value),
	# 	2) makes a new column, valsd, containing standardized version of Value, 3) creates a new column,
	# FIPS= complete county ID that should match our datasets, and 4) returs a clean dataset.  
	### Ins/Outs
	# Input= dataframe directly downloaded from NASS
	# Name as output an INFORMATIVE name!!!!  I do not keep track of what things are what!
	##########################

	# Clean dataframe by removing NAs
	dataframe<-dataframe[dataframe$Geo.Level=="COUNTY",]
	dataframe<-dataframe[!is.na(dataframe$County.ANSI),]  
	# Get rid of commas in the Value column, standardize the Value, put in new column, "valsd"
	dataframe$Value<-as.character(dataframe$Value)
	dataframe$Value2<-as.numeric(gsub(",", "", dataframe$Value))
	dataframe<-dataframe[!is.na(dataframe$Value2),]  # repeat, some NAs not removed earlier
	#dataframe$valsd<-standardize(dataframe$Value2)
	dataframe$FIPS<-NA
	# create FIPS column, that merges the StateID and column ID
	for (i in 1:length(dataframe[,1])){
		if (dataframe$County.ANSI[i]<10) {
		dataframe$FIPS[i]<-paste(dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep="00")
	}
	else if (dataframe$County.ANSI[i]<100 & dataframe$County.ANSI[i]>=10){
		dataframe$FIPS[i]<-paste(dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep="0")
	}
	else {dataframe$FIPS[i]<-paste(dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep="")
		}	
	}
	# Subset and rename things so it is easier to work with
	newdataframe<- dataframe[,colnames(dataframe) %in% c("Year", "State.ANSI", "County.ANSI", "Value2", "FIPS")]
	new= data.frame(FIPS= newdataframe$FIPS, year= newdataframe$Year, stateFIPS=newdataframe$State.ANSI, countyFIPS=newdataframe$County.ANSI, value=newdataframe$Value2)
	return(new)
}


source('~/GitHub/Swine-networks/clean_NASS_data.R', chdir = TRUE)
n <- read.csv("~/Documents/post-doc/Swine/NASS/NASS_2012_Hogs_county_level_data_REFORMATTED.csv")
colnames(n) <- c("fips", "ows", "smd", "smh", "owp", "pmh", "inv", "owi", "bri", "boi", "mai", "moi")

#inv_p<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Inventory_of_production.csv")
#inv_p<-inv_p[inv_p$Year=="2012",]
#inv_p2<-get_useful_parts(inv_p)
#n$inp <- inv_p2$value[match( n$fips, inv_p2$FIPS)]

n2 <- read.csv("~/Documents/post-doc/Swine/NASS/CA_Node Stats_County level.csv")
colnames(n2)[22:32] <- c("ows", "smd", "smh", "owp", "pmh", "inv", "owi", "bri", "boi", "mai", "moi")
write.csv(n2, "NASS/CA_Node Stats_County level.csv")  # sent to MATT to do single correlations

fips <- read.csv("~/Documents/post-doc/2009-2011 comparison final/paperdrafts2- MxSim/MxCattle_overview/county-fips.csv")

# Summary information for descriptive statistics
# total number of operations with operations with production (n is overall, n2 is commuter dataset)
cor.test(n$owi, n$owp) # 0.69
cor.test(n2$owi, n2$owp) # 0.95
# total number of operations with operations with breeding inventory
cor.test(n$owi, n$boi)  # 0.832
cor.test(n2$owi, n2$boi)  # 0.754

# total inventory with production inventory
cor.test(n$inv, n$pmh) #  0.895
cor.test(n2$inv, n2$pmh) # 0.889
# total inventory with breeding inventory
cor.test(n$inv, n$bri) # 0.839
cor.test(n2$inv, n2$bri) # 0.815

# inventory and operations iwth inventory
cor.test(n$inv, n$owi) # 0.6
cor.test(n2$inv, n2$owi) # 0.83

# operations iwht inventory and operations with sales
cor.test(n$owi, n$ows)
cor.test(n2$owi, n2$ows)

# Table 3:
#operations
cor.test(n2$Unweighted_InDeg, n2$owi) 
cor.test(n2$InDegree_Ship, n2$owi)
cor.test(n2$Unweighted_OutDeg, n2$owi )
cor.test(n2$OutDegree_Ship, n2$owi)
cor.test(n2$Betweenness, n2$owi)
#inventory
cor.test(n2$Unweighted_InDeg, n2$inv) 
cor.test(n2$InDegree_Ship, n2$inv)
cor.test(n2$Unweighted_OutDeg, n2$inv )
cor.test(n2$OutDegree_Ship, n2$inv)
cor.test(n2$Betweenness, n2$inv)

get_corr<-function(netval, nassval){
	temp<-cor.test(netval, nassval)
	results<-c(temp$estimate[[1]], temp$conf.int[1], temp$conf.int[2], temp$p.value)
	return(results) 	
}


wilcox.test(cvi2011$NUM_SWINE, caraw$NUM.SHIPPED)


plot(n2$owp ~ n2$OutDegree_Ship)
cor.test(n2$owp, n2$OutDegree_Ship)

plot(n2$pmh ~ n2$OutDegree_Ship)
cor.test(n2$pmh, n2$OutDegree_Ship)

plot(n2$bri ~ n2$OutDegree_Ship)
cor.test(n2$bri, n2$OutDegree_Ship)

plot(n2$bri[n2$OutDegree_Ship < 600] ~ n2$OutDegree_Ship[n2$OutDegree_Ship < 600])
cor.test(n2$bri[n2$OutDegree_Ship < 600], n2$OutDegree_Ship[n2$OutDegree_Ship < 600])



########################################
########################################
# 2) Calculate correlation between terms, choose covariates
########################################
########################################
pairs(n[,2:13])
covariates <- colnames(n)[-1]
df <- data.frame(cov1 = rep(covariates, length.out = length(covariates), 
	cov2 = rep(covariates, each = length(covariates)))
cor(n$ows, n$bri, use = "pairwise.complete.obs")

########################################
########################################
# 3) Analyses
########################################
########################################