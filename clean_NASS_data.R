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
	### This function takes a dataframe.  It 1) extracts the useful columns (value, State/County ID/ Value),
	# 	2) makes a new column, valsd, containing standardized version of Value, 3) creates a new column,
	# FIPS= complete county ID that should match our datasets, and 4) returs a clean dataset.  
	### Ins/Outs
	# Input= dataframe directly downloaded from NASS
	# Name as output an INFORMATIVE name!!!!  I do not keep track of what things are what!
	##########################

	# Clean dataframe by removing NAs
	dataframe<-dataframe[!is.na(dataframe$County.ANSI),]  
	dataframe<-dataframe[!is.na(dataframe$Value),]
	# Get rid of commas in the Value column, standardize the Value, put in new column, "valsd"
	dataframe$Value2<-as.numeric(gsub(",", "", dataframe$Value))
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
# Here is how to use the function
corn<-read.csv("~/Documents/post-doc/2009-2011 comparison final/overlay/econ&aggdata/AcresPlanted_foranalysis.csv")

corn_acres_planted_2008<-get_useful_parts(corn)
write.csv(corn_acres_planted_2008, "~/Documents/post-doc/2009-2011 comparison final/overlay/econ&aggdata/groomed_acresplanted.csv")

################
# NASS data used in project




