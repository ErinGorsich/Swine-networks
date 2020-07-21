########################################
########################################
# Outline
########################################
########################################
# 1) Load Shipment Data
# 2) Load NASS Data
# 3) Calculate correlations

########################################
########################################
setwd("~/Google Drive/Warwick/students/Callum/Swine")

########################################
########################################
# 1) Load Shipment Data
########################################
########################################

# Raw commuter agreement data
########################################
caraw <- read.csv("CommuterAgreementAnalyses/SWINE Commuter Data - FTP site originals/Swine_Commuter_Data_31May2017_02.csv")
caraw <- caraw[caraw$Year == "2014",]
caraw <- caraw[!is.na(caraw$D.state),]  # 321 removed
caraw <- caraw[!is.na(caraw$O.state),]  # 5 removed
caraw <- caraw[!is.na(caraw$NUM.SHIPPED),]  # 6 removed
caraw$O.state <- as.character(caraw$O.state)
caraw$D.state <- as.character(caraw$D.state)
caraw$D.state[caraw$D.FIPS == 27045 & !is.na(caraw$D.FIPS)] <- "MN"

statefips <- read.csv("county_centroid_coordinates.csv")
statefips$stab <- as.character(statefips$stab)
caraw$O.state.FIPS <- statefips$ST_FIPS[match(caraw$O.state, statefips$stab)]
caraw$D.state.FIPS <-  statefips$ST_FIPS[match(caraw$D.state, statefips$stab)]
caraw <- caraw[!(caraw$O.state.FIPS == caraw$D.state.FIPS),]   # removed 7
caraw <- caraw[!is.na(caraw$O.FIPS), ]
caraw <- caraw[!is.na(caraw$D.FIPS), ]

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

########################################
########################################
# 3) NASS Co-variates 
########################################
########################################

## functions used
newcol <- NA
standardize = function(column){
	for (i in 1:length(column)){
	    newcol[i] <- (column[i] - mean(column)) / sd(column)
}
	return(newcol)
}

newdataframe<-NA
get_useful_parts = function(dataframe){
	##########################
	### This function takes a dataframe.  It
	# 1) extracts the useful columns (value, State/County ID/ Value),
	# 2) makes a new col=valsd, containing standardized version of Value, 
    # 3) creates a new column = FIPS= complete county ID  
	### Ins/Outs
	# Input= dataframe directly downloaded from NASS; Output = clean df
	##########################

    # domain = TOTAL - in case there are multiple outputs in the file
    dataframe <- dataframe[dataframe$Domain == "TOTAL",]
    
	# Clean dataframe by removing NAs
	dataframe <- dataframe[dataframe$Geo.Level=="COUNTY",]
	dataframe <- dataframe[!is.na(dataframe$County.ANSI),] 
	
	# Get rid of commas in the Value col, standardize, put in new col "valsd"
	dataframe$Value <- as.character(dataframe$Value)
	dataframe$Value[dataframe$Value == " (D)"] <- -200  # assign censored -200!
	dataframe$Value2 <- as.numeric(gsub(",", "", dataframe$Value))
	dataframe$FIPS <- NA
	
	# create FIPS column, that merges the StateID and column ID
	for (i in 1:length(dataframe[,1])){
		if (dataframe$County.ANSI[i] < 10) {
		dataframe$FIPS[i] <- paste(
		    dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep = "00")
	}
	else if (dataframe$County.ANSI[i] < 100 & dataframe$County.ANSI[i] >= 10){
		dataframe$FIPS[i] <- paste(
		    dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep = "0")
	}
	else {dataframe$FIPS[i] <- paste(
	    dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep = "")
		}	
	}
	
	# Subset and rename things so it is easier to work with
	newdataframe <- dataframe[ , colnames(dataframe) %in% c(
	    "Year", "State.ANSI", "County.ANSI", "Value2", "FIPS")]
	new <- data.frame(FIPS = newdataframe$FIPS, year = newdataframe$Year, 
	                  stateFIPS = newdataframe$State.ANSI, 
	                  countyFIPS = newdataframe$County.ANSI, 
	                  value = newdataframe$Value2)
	return(new)
}

# File read-in
########################################
# Operations 
owi <- get_useful_parts(
    read.csv("NASS/2012_NASS_HOGS_Operations_with_inventory.csv"))
owbreeding <- get_useful_parts(
    read.csv("NASS/*2012_NASS_HOGS_Operations_with_breeding_inventory.csv"))
owproduction <- get_useful_parts(
    read.csv("NASS/2012_NASS_HOGS_Operations_with_production.csv"))
owsales <- get_useful_parts(
    read.csv("NASS/2012_NASS_HOGS_Operations_with_sales.csv"))
owmarket <- get_useful_parts(
    read.csv("NASS/2012_NASS_HOGS_Operations_with_market_inventory.csv"))

# Inventory (warnings ok)
inv <- get_useful_parts(read.csv("NASS/*2012_NASS_HOGS_Inventory.csv"))
invbreeding <- get_useful_parts(
    read.csv("NASS/*2012_NASS_HOGS_Inventory_breeding.csv"))
invproduction <- get_useful_parts(
    read.csv("NASS/*2012_NASS_HOGS_Inventory_of_production.csv"))
invmarket <- get_useful_parts(
    read.csv("NASS/2012_NASS_HOGS_Inventory_market.csv"))

files <- list(owi = owi, owbreeding = owbreeding, owproduction = owproduction, 
              owsales = owsales, owmarket = owmarket, inv = inv, 
              invbreeding = invbreeding, invproduction = invproduction, 
              invmarket = invmarket)
newdf <- data.frame(
    FIPS = unique(c(files[[1]][,1], files[[2]][,1], files[[3]][,1], 
                    files[[4]][,1], files[[5]][,1], files[[6]][,1],
                    files[[7]][,1], files[[8]][,1], files[[9]][,1]))
)
i <- 2
for (file in files){
    newdf[, i] <- file$value[match(newdf$FIPS, file$FIPS, nomatch = NA)]
    i <- i + 1
}
colnames(newdf) <- c("FIPS", names(files))

# Note OWI, sales, market, (to a lesser extent production) were all highly correlated. 
# % breeding or total is useful because stratified with total
pairs(newdf[ ,2:6], pch = 19, cex = 0.7)

# inventory, market inventory (to a lesser extent production) are strongly 
pairs(newdf[ ,c(2:3, 7:10)], pch = 19, cex = 0.7)

usedf <- newdf[ , c('FIPS', "owi", "owbreeding", "owproduction", 
                    "inv", "invbreeding", "invproduction")]
pairs(usedf[2:7])

########################################
########################################
# 3) Analysis
########################################
########################################
# Correlations
caraw$move <- 1
caraw$O.FIPS <- as.character(caraw$O.FIPS) 
caraw$D.FIPS <- as.character(caraw$D.FIPS)
cvi2011$O_FIPS <- as.character(cvi2011$O_FIPS) 
cvi2011$D_FIPS <- as.character(cvi2011$D_FIPS)
usedf$ICVIin <- NA; usedf$ICVIout <- NA

for (i in 1:length(usedf[,1])){
    fips <- usedf$FIPS[i]
    usedf$ICVIin[i] <- sum(cvi2011$MOVE[cvi2011$D_FIPS == fips])
    usedf$ICVIout[i] <- sum(cvi2011$MOVE[cvi2011$O_FIPS == fips])
    usedf$headICVIin[i] <- sum(cvi2011$NUM_SWINE[cvi2011$D_FIPS == fips])
    usedf$headICVIout[i] <- sum(cvi2011$NUM_SWINE[cvi2011$O_FIPS == fips])

    usedf$CAin[i] <- sum(caraw$move[caraw$D.FIPS == fips])
    usedf$CAout[i] <- sum(caraw$move[caraw$O.FIPS == fips])
    usedf$headCAin[i] <- sum(caraw$NUM.SHIPPED[caraw$D.FIPS == fips])
    usedf$headCAout[i] <- sum(caraw$NUM.SHIPPED[caraw$O.FIPS == fips]) 
}

usedf$totICVI <- usedf$ICVIin + usedf$ICVIout
usedf$headtotICVI <- usedf$headICVIin + usedf$headICVIout 
usedf$totCA <- usedf$CAin + usedf$CAout
usedf$headtotCA <- usedf$headCAin + usedf$headCAout
usedf$tot <- usedf$totICVI + usedf$totCA
usedf$tothead <- usedf$headtotICVI + usedf$headtotCA

usedf$totin <- usedf$ICVIin + usedf$CAin
usedf$headtotin <- usedf$headICVIin + usedf$headCAin
usedf$totout <- usedf$ICVIout + usedf$CAout
usedf$headtotout <- usedf$headICVIout + usedf$headCAout

# Total shipments - totICVI and operations with production (no other operations)
# all inv correlate with total ICVI shipments (and a lesser extent head in ICVI)
# !!! counties with low inventory did not receive a CA - 
# so the CA indicates (so only if > 300 pigs/county) 
# REALLY THEY ARE CAPTURING DIFFERENT COUNTIES
##########################################
pairs(usedf[, c(2:4, 16:21)], log = "xy", pch = 19, cex = 0.6) # operations
pairs(usedf[, c(5:7, 16:21)], log = "xy", pch = 19, cex = 0.6) # inventory
    
# In shipments - 
# ICVIin & CAin with owprod, owi, less breeding; combined not an improvement likely because different counties
# all inv correlate in ICVI & CA shipments BUT counties with low inv not in CA data!!!!!
# combined is an improvement - because capturing different counties
##########################################
pairs(usedf[, c(2:4, 8, 10, 12, 14, 22:23)], log = "xy", pch = 19, cex = 0.6) # operations
pairs(usedf[, c(5:7, 8, 10, 12, 14, 22:23)], log = "xy", pch = 19, cex = 0.6) # inventory

# Out shipments - ICVI out and breeding inventory
##########################################
pairs(usedf[, c(2:4, 9, 11, 13, 15, 24:25)], log = "xy", pch = 19, cex = 0.6) # operations
pairs(usedf[, c(5:7, 9, 11, 13, 15, 24:25)], log = "xy", pch = 19, cex = 0.6) # inventory

# make colored by ICVI counties; CA counties; both to show separate subsets
# add map
par(mfrow = c(1, 2))
plot(log(usedf$invproduction[usedf$ICVIin > 0 & usedf$CAin > 0]+1) ~ log(usedf$totin[
    usedf$ICVIin > 0 & usedf$CAin > 0]+1), pch = 19, cex = 0.6, col = "red", 
    ylab = "Production inventory in head (log)", xlab = "In-shipments (log)", ylim = c(7, 15), 
    xlim = c(1, 7))
points(log(usedf$invproduction[usedf$ICVIin > 0]+1) ~ log(usedf$ICVIin[usedf$ICVIin > 0]+1),
       pch = 19, cex = 0.6, col = "green")
points(log(usedf$invproduction[usedf$CAin > 0]+1) ~ log(usedf$CAin[usedf$CAin > 0]+1), 
       pch = 19, cex = 0.6, col = "blue")

plot(log(usedf$owi[usedf$totICVI > 0 & usedf$totCA > 0]+1) ~ log(usedf$tot[
    usedf$totICVI > 0 & usedf$totCA > 0]+1), pch = 19, cex = 0.6, col = "red", 
    ylab = "Operations with inventory (log)", xlab = "Total shipments (log)", 
    ylim = c(1, 7), xlim = c(1, 7))
points(log(usedf$owi[usedf$totICVI > 0]+1) ~ log(usedf$totICVI[usedf$totICVI > 0]+1),
       pch = 19, cex = 0.6, col = "green")
points(log(usedf$owi[usedf$totCA > 0] + 1) ~ log(usedf$totCA[usedf$totCA > 0]+1), 
       pch = 19, cex = 0.6, col = "blue")

# Out
# plot(log(usedf$invbreeding[usedf$ICVIout > 0 & usedf$CAout > 0]+1) ~ log(usedf$totout[
#     usedf$ICVIout > 0 & usedf$CAout > 0]+1), pch = 19, cex = 0.6, col = "red", 
#     ylab = "Breeding inventory (head, log)", xlab = "Out-shipments (log)", ylim = c(1, 12), 
#     xlim = c(1, 7))
# points(log(usedf$invbreeding[usedf$ICVIout > 0]+1) ~ log(usedf$ICVIout[usedf$ICVIout > 0]+1),
#        pch = 19, cex = 0.6, col = "green")
# points(log(usedf$invbreeding[usedf$CAout > 0] + 1) ~ log(usedf$CAout[usedf$CAout > 0]+1), 
#        pch = 19, cex = 0.6, col = "blue")


# End updates


# Quick PCA of NASS covariates - need linear patterns
# library(vegan)
# library(ade4)
# usedf[usedf < -100] <- NA
# data("mite")
# rda(X = usedf[, 2:7])
# usedf$inv <- log(usedf$inv); usedf$invbreeding <- log(usedf$invbreeding)
# usedf$invproduction <- log(usedf$invproduction)



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
# pairs(n[,2:13])
# covariates <- colnames(n)[-1]
# df <- data.frame(cov1 = rep(covariates, length.out = length(covariates), 
# 	cov2 = rep(covariates, each = length(covariates)))
# cor(n$ows, n$bri, use = "pairwise.complete.obs")

########################################
########################################
# 3) Analyses
########################################
########################################