#############################################
#############################################
# This script merges data taken from NASS with county information. 
#############################################
#############################################

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
	dataframe$valsd<-standardize(dataframe$Value2)
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
	newdataframe<- dataframe[,colnames(dataframe) %in% c("Year", "State.ANSI", "County.ANSI", "Value2", "valsd", "FIPS")]
	new= data.frame(FIPS= newdataframe$FIPS, year= newdataframe$Year, stateFIPS=newdataframe$State.ANSI, countyFIPS=newdataframe$County.ANSI, value=newdataframe$Value2, valuesd=newdataframe$valsd)
	return(new)
}


################
# NASS data used in project- make sure only one data item; one domain in dataset; one year. 
# function specifies geo level and removes NAs. 
## INVENTORY
inv<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Inventory.csv")
inv_p<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Inventory_of_production.csv")
# state level only
#inv_f<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Inventory_of_feeder_hogs_JUSTUSETOTAL.csv")
inv_b<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Inventory_breeding.csv")

## OPERATIONS
op<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Operations_Inventory.csv")
op_p<-read.csv("~/Documents/post-doc/Swine/NASS/2012_NASS_HOGS_Operations_with_production.csv")
op_b<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Operations_wtih_breeding_inventory.csv")
# state level only. 
#op_f<-read.csv("~/Documents/post-doc/Swine/NASS/*2012_NASS_HOGS_Operations_with_farrowtofeeder_inventory.csv")

inv2<-get_useful_parts(inv)
inv_p<-inv_p[inv_p$Year=="2012",]
inv_p2<-get_useful_parts(inv_p)
inv_b2<-get_useful_parts(inv_b)

op2<-get_useful_parts(op)  # only file with multiple rows per county, have to sum them. 
op3=data.frame(FIPS=unique(op2$FIPS))
for (i in 1:length(op3$FIPS)){
val=op3$FIPS[i]
tempdf=op2[op2$FIPS==val,]
op3$newvalue[i]=sum(tempdf$value)
}

op_p2<-get_useful_parts(op_p)
op_b<-op_b[op_b$Year=="2012" & op_b$Domain=="TOTAL",]
op_b2<-get_useful_parts(op_b)

###################
# correlations with node stats (in-degree, out-degree, weighted in-degree, weighted out-degree, betweenness)
# only in states with data. 
ns2010<-read.csv("~/Documents/post-doc/Swine/node_stats_sub2010.csv")
ns2010$inv<-inv2$value[match(ns2010$NodeID, inv2$FIPS)]
ns2010$inv_p<-inv_p2$value[match(ns2010$NodeID, inv_p2$FIPS)]
ns2010$inv_b<-inv_b2$value[match(ns2010$NodeID, inv_b2$FIPS)]
ns2010$op<-op3$newvalue[match(ns2010$NodeID, op3$FIPS)]
ns2010$op_p<- op_p2$value[match(ns2010$NodeID, op_p2$FIPS)]
ns2010$op_b<-op_b2$value[match(ns2010$NodeID, op_b2$FIPS)]

ns2011<-read.csv("~/Documents/post-doc/Swine/node_stats_sub2011all.csv")
ns2011$inv<-inv2$value[match(ns2011$NodeID, inv2$FIPS)]
ns2011$inv_p<-inv_p2$value[match(ns2011$NodeID, inv_p2$FIPS)]
ns2011$inv_b<-inv_b2$value[match(ns2011$NodeID, inv_b2$FIPS)]

ns2011$op<-op3$newvalue[match(ns2011$NodeID, op3$FIPS)]
ns2011$op_p<- op_p2$value[match(ns2011$NodeID, op_p2$FIPS)]
ns2011$op_b<-op_b2$value[match(ns2011$NodeID, op_b2$FIPS)]

# test for correlations 
net10<-ns2010[,c(4,5,7,8,13)]; net11<-ns2011[,c(4,5,7,8,13)]
nass10<-ns2010[,c(20:25)]; nass11<-ns2011[,c(20:25)]
# want cor.test over each column
get_corr<-function(netval, nassval){
	temp<-cor.test(netval, nassval)  # method="pearson" as default
	results<-c(temp$estimate[[1]], temp$conf.int[1], temp$conf.int[2], temp$p.value)
	return(results) 	
}

lnet<-5 
lnass<- 6
arr10<-array(,dim=c(lnass, 4, lnet), dimnames=list(colnames(nass10), c("est", "LCI", "UCI", "p")))
arr11<-arr10
# one matrix per network property- 4 columns per array with answers to get_corr
# each row is a comparison to lnas column namae

for (i in 1:lnet){
	for (j in 1:lnass){
		arr10[j,,i]<-get_corr(netval=net10[,i], nassval=nass10[,j])
		arr11[j,,i]<-get_corr(netval=net11[,i], nassval=nass11[,j])
	}
}