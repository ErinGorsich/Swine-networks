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

op2<-get_useful_parts(op)
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
ns2010$op<-op2$value[match(ns2010$NodeID, op2$FIPS)]
ns2010$op_p<- op_p2$value[match(ns2010$NodeID, op_p2$FIPS)]
ns2010$op_b<-op_b2$value[match(ns2010$NodeID, op_b2$FIPS)]

ns2011<-read.csv("~/Documents/post-doc/Swine/node_stats_sub2011all.csv")
ns2011$inv<-inv2$value[match(ns2011$NodeID, inv2$FIPS)]
ns2011$inv_p<-inv_p2$value[match(ns2011$NodeID, inv_p2$FIPS)]
ns2011$inv_b<-inv_b2$value[match(ns2011$NodeID, inv_b2$FIPS)]

ns2011$op<-op2$value[match(ns2011$NodeID, op2$FIPS)]
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

#2010
, , 1

             est         LCI         UCI            p
inv    0.4542005  0.35199641  0.54570132 1.221245e-14
inv_p  0.1820895  0.01419162  0.34000080 3.386456e-02
inv_b  0.1996849  0.06954639  0.32314054 2.865741e-03
op    -0.1806458 -0.29536729 -0.06078891 3.344042e-03
op_p   0.5365025  0.41513816  0.63910518 3.712586e-13
op_b   0.1952941  0.07519535  0.30980804 1.587338e-03

, , 2

             est         LCI         UCI            p
inv    0.4684580  0.36776306  0.55829469 1.332268e-15
inv_p  0.2036319  0.03655191  0.35963713 1.741696e-02
inv_b  0.2065143  0.07663151  0.32950518 2.028828e-03
op    -0.1991653 -0.31280309 -0.07990632 1.191582e-03
op_p   0.4918514  0.36363762  0.60177654 5.278289e-11
op_b   0.1846450  0.06420128  0.29978668 2.855335e-03

, , 3

             est         LCI         UCI            p
inv    0.3417598  0.22965427  0.44491547 1.555235e-08
inv_p  0.1329114 -0.03623210  0.29465194 1.229411e-01
inv_b  0.3460901  0.22436661  0.45718017 1.292557e-07
op    -0.1718889 -0.28709609 -0.05177979 5.274805e-03
op_p   0.2306969  0.07734253  0.37338738 3.542873e-03
op_b   0.1958862  0.07580752  0.31036453 1.534975e-03

, , 4

             est         LCI         UCI            p
inv    0.3319772  0.21917525  0.43602163 4.170449e-08
inv_p  0.1368953 -0.03217907  0.29835298 1.120117e-01
inv_b  0.3854695  0.26708509  0.49239230 3.054868e-09
op    -0.1792526 -0.29405254 -0.05935431 3.600437e-03
op_p   0.2195895  0.06570192  0.36327434 5.567885e-03
op_b   0.1556733  0.03443804  0.27239334 1.212389e-02

, , 5

             est         LCI        UCI            p
inv    0.3264220  0.21323607  0.4309620 7.192448e-08
inv_p  0.1231313 -0.04615826  0.2855457 1.532538e-01
inv_b  0.2636566  0.13643251  0.3823248 7.255646e-05
op    -0.1798745 -0.29463944 -0.0599946 3.483873e-03
op_p   0.3328463  0.18639294  0.4648291 1.927946e-05
op_b   0.1669900  0.04603841  0.2831161 7.072976e-03


#2011
             est        LCI         UCI            p
inv    0.6555623  0.5884256  0.71372126 0.000000e+00
inv_p  0.6465829  0.5447958  0.72959361 0.000000e+00
inv_b  0.4416548  0.3371013  0.53544720 1.341149e-13
op    -0.1415881 -0.2465821 -0.03330898 1.060145e-02
op_p   0.7303061  0.6570274  0.78991330 0.000000e+00
op_b   0.2588993  0.1532792  0.35867092 2.887439e-06

, , 2

             est        LCI         UCI            p
inv    0.6602040  0.5937500  0.71771284 0.000000e+00
inv_p  0.6304122  0.5253105  0.71656733 0.000000e+00
inv_b  0.4496926  0.3459614  0.54256318 4.263256e-14
op    -0.1597557 -0.2639602 -0.05186485 3.883080e-03
op_p   0.6700217  0.5842984  0.74095813 0.000000e+00
op_b   0.2383778  0.1318499  0.33946249 1.737221e-05

, , 3

             est         LCI         UCI            p
inv    0.3268630  0.22562504  0.42111684 1.873967e-09
inv_p  0.2098093  0.05446426  0.35523804 8.569476e-03
inv_b  0.4506048  0.34696814  0.54336997 3.730349e-14
op    -0.1626485 -0.26672109 -0.05482636 3.276897e-03
op_p   0.1892581  0.04970856  0.32155960 8.218625e-03
op_b   0.1412964  0.03180590  0.24743582 1.165505e-02

, , 4

              est          LCI         UCI            p
inv    0.28400432  0.180302604  0.38146180 2.176666e-07
inv_p  0.19060421  0.034496697  0.33763231 1.715390e-02
inv_b  0.44281564  0.338379795  0.53647574 1.136868e-13
op    -0.15318347 -0.257681399 -0.04514356 5.652890e-03
op_p   0.14292653  0.002093848  0.27819988 4.680176e-02
op_b   0.08849049 -0.021705487  0.19656216 1.152822e-01

, , 5

             est        LCI        UCI            p
inv    0.5234606  0.4392957  0.5985160 0.000000e+00
inv_p  0.4669943  0.3343926  0.5814650 7.980538e-10
inv_b  0.4237886  0.3174735  0.5195821 1.548095e-12
op    -0.1265255 -0.2321226 -0.0179808 2.252723e-02
op_p   0.4774152  0.3608067  0.5793269 1.944667e-12
op_b   0.2510308  0.1450508  0.3513158 5.852168e-06