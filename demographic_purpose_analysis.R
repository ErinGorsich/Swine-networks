####################################################################
####################################################################
####################################################################
# Spatio-temporal patterns and characteristics of swine shipments in the U.S. 
# based on Interstate Certificates of Veterinary Inspection
# Code to run many wilcoxon tests, compare purpose/age/sex by state and year
# Erin E. Gorsich
####################################################################
####################################################################
####################################################################
# Outline
# Step 1: Set your working directory and install
# Step 2: Read in the data and remove/change any wierd things
# Step 3: Shipment size analyses
# Step 4: Purpose analyses
# Step 5: Age
#################################################
#################################################

#################################################
# Step 1:  Set your working directory and install
# packages needed to create a network
#################################################
# set working directory.
setwd("~/Documents/post-doc/Swine")
library(MASS)

################################################
# Step 2:  Read in the data and remove/change any wierd things
#################################################

dat = read.csv("Swine_cvi_final.csv")
dat = dat[!is.na(dat$NUM_SWINE),]  #-1
dat = dat[dat$NUM_SWINE>0,] # -13
dat = dat[!is.na(dat$SAMPLE_YEAR2),]  #-38 #Clay added new col for year
dat = dat[!is.na(dat$O_FIPS),]
dat = dat[!is.na(dat$D_FIPS),]
dat = dat[dat$NUM_SWINE>0,]

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
data<-data[data$O_FIPS!=data$D_FIPS,]  # REMOVE INTRASTATE SHIPMENTS!
length(data[data$O_ST_FIPS==data$D_ST_FIPS,])
data<-data[data$O_ST_FIPS!=data$D_ST_FIPS,]

# all combined data
data.cvi <- data
# 2010 data only 
data2010=data[data$SAMPLE_YEAR2==2010,]
data2010 = data2010[data2010$O_STATE != "NE",]

#2) 2011 data only 
data2011=data[data$SAMPLE_YEAR2==2011,]
#3) 2011 data only
datared2011= data2011[data2011$STATE!="NE",]
data <- rbind(data2010, datared2011)

# number of shipments in the dataset
length(data.cvi[,1]) 
# number of swine in the dataset
sum(data.cvi$NUM_SWINE)
# number of premises

#################################################
################################################
# Step 3:  Shipment size
#################################################
#################################################
summary(data.cvi$NUM_SWINE)
summary(data2010$NUM_SWINE)
summary(data2011$NUM_SWINE)
summary(datared2011$NUM_SWINE)
wilcox.test(data2010$NUM_SWINE, data2011$NUM_SWINE)
wilcox.test(data2010$NUM_SWINE, datared2011$NUM_SWINE)


#################################################
################################################
# Step 4:  Purpose
#################################################
#################################################

# Figure 2 - shipment volume and size by purpose
#################################################
table(data2010$PURPOSE)
table(datared2011$PURPOSE)
# feedign/grazing together
# transit, no data, not legible, NS, other all in not specified

tapply(data2010$NUM_SWINE, data2010$PURPOSE, median)
tapply(datared2011$NUM_SWINE, datared2011$PURPOSE, median)
tapply(data$NUM_SWINE, data$PURPOSE, median)
nas <- c("NOT SPECIFIED", "Other", "Not Legible", "No Data", "Transit")
feed <- c("Feeding", "Feeding/Grazing", "Production")

median(data$NUM_SWINE[data$PURPOSE %in% feed])
median(data2010$NUM_SWINE[data2010$PURPOSE %in% feed])
median(data2011$NUM_SWINE[data2011$PURPOSE %in% feed])
median(data$NUM_SWINE[data$PURPOSE %in% nas])

# Table 1 - summary stats
#################################################
table(data2010$O_STATE); table(data2011$O_STATE)
tapply(data2010$NUM_SWINE, data2010$O_STATE, sum)
sum(data2010$NUM_SWINE)
tapply(data2011$NUM_SWINE, data2011$O_STATE, sum)
sum(data2011$NUM_SWINE)
# median head/shipment
tapply(data2010$NUM_SWINE, data2010$O_STATE, median)
median(data2010$NUM_SWINE)
tapply(data2011$NUM_SWINE, data2011$O_STATE, median)
median(data2011$NUM_SWINE)
# max head/shipment
tapply(data2010$NUM_SWINE, data2010$O_STATE, max)
max(data2010$NUM_SWINE)
tapply(data2011$NUM_SWINE, data2011$O_STATE, max)
max(data2011$NUM_SWINE)
# percent by purpose, overall
table(data2010$PURPOSE)/31.10
table(data2011$PURPOSE)/35.22
# by state
ca <- data2010[data2010$O_STATE== "CA",]
table(ca$PURPOSE)/length(ca$PURPOSE)*100
ia <- data2010[data2010$O_STATE== "IA",]
table(ia$PURPOSE)/length(ia$PURPOSE)*100
mn <- data2010[data2010$O_STATE== "MN",]
table(mn$PURPOSE)/length(mn$PURPOSE)*100
nc <- data2010[data2010$O_STATE== "NC",]
table(nc$PURPOSE)/length(nc$PURPOSE)*100
ny <- data2010[data2010$O_STATE== "NY",]
table(ny$PURPOSE)/length(ny$PURPOSE)*100
tx <- data2010[data2010$O_STATE== "TX",]
table(tx$PURPOSE)/length(tx$PURPOSE)*100
wi <- data2010[data2010$O_STATE== "WI",]
table(wi$PURPOSE)/length(wi$PURPOSE)*100
# by state 2011
ca <- data2011[data2011$O_STATE== "CA",]
table(ca$PURPOSE)/length(ca$PURPOSE)*100
ia <- data2011[data2011$O_STATE== "IA",]
table(ia$PURPOSE)/length(ia$PURPOSE)*100
mn <- data2011[data2011$O_STATE== "MN",]
table(mn$PURPOSE)/length(mn$PURPOSE)*100
nc <- data2011[data2011$O_STATE== "NC",]
table(nc$PURPOSE)/length(nc$PURPOSE)*100
ny <- data2011[data2011$O_STATE== "NY",]
table(ny$PURPOSE)/length(ny$PURPOSE)*100
tx <- data2011[data2011$O_STATE== "TX",]
table(tx$PURPOSE)/length(tx$PURPOSE)*100
wi <- data2011[data2011$O_STATE== "WI",]
table(wi$PURPOSE)/length(wi$PURPOSE)*100


# Table B1: hypothesis tests by purpose
#################################################
# Breeding
wilcox.test(data2010$NUM_SWINE[data2010$PURPOSE %in% feed], 
	data2010$NUM_SWINE[data2010$PURPOSE == "Breeding"])
wilcox.test(data2011$NUM_SWINE[data2011$PURPOSE %in% feed], 
	data2011$NUM_SWINE[data2011$PURPOSE == "Breeding"])

wilcox.test(data2010$NUM_SWINE[data2010$PURPOSE =="Sale"], 
	data2010$NUM_SWINE[data2010$PURPOSE == "Breeding"])
wilcox.test(data2011$NUM_SWINE[data2011$PURPOSE == "Sale"], 
	data2011$NUM_SWINE[data2011$PURPOSE == "Breeding"])

wilcox.test(data2010$NUM_SWINE[data2010$PURPOSE =="Show/Exhibition"], 
	data2010$NUM_SWINE[data2010$PURPOSE == "Breeding"])
wilcox.test(data2011$NUM_SWINE[data2011$PURPOSE == "Show/Exhibition"], 
	data2011$NUM_SWINE[data2011$PURPOSE == "Breeding"])

# Feeding
wilcox.test(data2010$NUM_SWINE[data2010$PURPOSE %in% feed], 
	data2010$NUM_SWINE[data2010$PURPOSE == "Sale"])
wilcox.test(data2011$NUM_SWINE[data2011$PURPOSE %in% feed], 
	data2011$NUM_SWINE[data2011$PURPOSE == "Sale"])

wilcox.test(data2010$NUM_SWINE[data2010$PURPOSE %in% feed], 
	data2010$NUM_SWINE[data2010$PURPOSE == "Show/Exhibition"])
wilcox.test(data2011$NUM_SWINE[data2011$PURPOSE %in% feed], 
	data2011$NUM_SWINE[data2011$PURPOSE == "Show/Exhibition"])

# Sale vs Show
wilcox.test(data2010$NUM_SWINE[data2010$PURPOSE =="Show/Exhibition"], 
	data2010$NUM_SWINE[data2010$PURPOSE == "Sale"])
wilcox.test(data2011$NUM_SWINE[data2011$PURPOSE == "Show/Exhibition"], 
	data2011$NUM_SWINE[data2011$PURPOSE == "Sale"])
	
# Figure B1: Purpose by state
#################################################
table(data2010$O_STATE, data2010$PURPOSE)
table(data2011$O_STATE, data2011$PURPOSE)
# chisq.test()

d10 <- data2010[data2010$PURPOSE %in% c(feed, "Breeding", "Sale", "Show/Exhibition"),]
d10$PURPOSE[d10$PURPOSE == "Feeding/Grazing"] <- "Feeding"
d10$PURPOSE[d10$PURPOSE == "Production"] <- "Feeding"
d10$PURPOSE <- droplevels(d10$PURPOSE)
d11 <- data2011[data2011$PURPOSE %in% c(feed, "Breeding", "Sale", "Show/Exhibition"),]
d11$PURPOSE[d11$PURPOSE == "Feeding/Grazing"] <- "Feeding"
d11$PURPOSE[d11$PURPOSE == "Production"] <- "Feeding"
d11$PURPOSE <- droplevels(d11$PURPOSE)

dr11 <- datared2011[datared2011$PURPOSE %in% c(feed, "Breeding", "Sale", "Show/Exhibition"),]
dr11$PURPOSE[dr11$PURPOSE == "Feeding/Grazing"] <- "Feeding"
dr11$PURPOSE[dr11$PURPOSE == "Production"] <- "Feeding"
dr11$PURPOSE <- droplevels(dr11$PURPOSE)

get_chisq = function(st1, st2, df) {
	newdf <- df[df$O_STATE %in% c(st1, st2),]
	newdf$O_STATE <- droplevels(newdf$O_STATE)
	test <- chisq.test(table(newdf$PURPOSE, newdf$O_STATE), 
		simulate.p.value = TRUE)
	rm(newdf)
	return(test)
}
get_chisq_yr = function(st, df1, df2) {
	newdf1 <- df1[df1$O_STATE == st,]
	newdf2 <- df2[df2$O_STATE == st,]
	newdf1$O_STATE <- droplevels(newdf1$O_STATE)
	newdf2$O_STATE <- droplevels(newdf2$O_STATE)
	temp <- data.frame(p = c(newdf1$PURPOSE, newdf2$PURPOSE), 
		y = c(rep("2010", length(newdf1$O_STATE)), 
			rep("2011", length(newdf2$O_STATE))) )
	test <- chisq.test(table(temp$p, temp$y), 
		simulate.p.value = TRUE)
	rm(newdf1, newdf2, temp)
	return(test)
}

# 2010 
newdf <- d10[d10$O_STATE %in% c("CA", "IA"),]
newdf$O_STATE <- droplevels(newdf$O_STATE)
chisq.test(table(newdf$PURPOSE, newdf$O_STATE), simulate.p.value = TRUE)

newdf <- d10[d10$O_STATE %in% c("CA", "MN"),]
newdf$O_STATE <- droplevels(newdf$O_STATE)
chisq.test(table(newdf$PURPOSE, newdf$O_STATE), simulate.p.value = TRUE)

newdf <- d10[d10$O_STATE %in% c("CA", "NC"),]
newdf$O_STATE <- droplevels(newdf$O_STATE)
chisq.test(table(newdf$PURPOSE, newdf$O_STATE), simulate.p.value = TRUE)

# no breeding/feeding in NY
newdf <- d10[d10$O_STATE %in% c("CA", "NY"),]
newdf$O_STATE <- droplevels(newdf$O_STATE)
chisq.test(table(newdf$PURPOSE, newdf$O_STATE), simulate.p.value = TRUE)

newdf <- d10[d10$O_STATE %in% c("CA", "TX"),]
newdf$O_STATE <- droplevels(newdf$O_STATE)
chisq.test(table(newdf$PURPOSE, newdf$O_STATE), simulate.p.value = TRUE)

newdf <- d10[d10$O_STATE %in% c("CA", "WI"),]
newdf$O_STATE <- droplevels(newdf$O_STATE)
chisq.test(table(newdf$PURPOSE, newdf$O_STATE), simulate.p.value = TRUE)


get_chisq("CA", "IA", d10)
get_chisq("CA", "MN", d10)
get_chisq("CA", "NC", d10)
get_chisq("CA", "NY", d10)
get_chisq("CA", "TX", d10)
get_chisq("CA", "WI", d10)

get_chisq("IA", "MN", d10)
get_chisq("IA", "NC", d10)
get_chisq("IA", "NY", d10)
get_chisq("IA", "TX", d10)
get_chisq("IA", "WI", d10)

get_chisq("MN", "NC", d10)
get_chisq("MN", "NY", d10)
get_chisq("MN", "TX", d10)
get_chisq("MN", "WI", d10)

get_chisq("NC", "NY", d10)
get_chisq("NC", "TX", d10)
get_chisq("NC", "WI", d10)

get_chisq("NY", "TX", d10)
get_chisq("NY", "WI", d10)
get_chisq("TX", "WI", d10)

newdf <- d10[d10$O_STATE %in% c("NY", "TX"),]
newdf$O_STATE <- droplevels(newdf$O_STATE)
chisq.test(table(newdf$PURPOSE, newdf$O_STATE), simulate.p.value = TRUE)


#2011
get_chisq("CA", "IA", d11)
get_chisq("CA", "MN", d11)
get_chisq("CA", "NC", d11)
get_chisq("CA", "NY", d11)
get_chisq("CA", "TX", d11)
get_chisq("CA", "WI", d11)

get_chisq("NE", "IA", d11)
get_chisq("NE", "MN", d11)
get_chisq("NE", "NY", d11)
get_chisq("NE", "TX", d11)
get_chisq("NE", "WI", d11)

get_chisq("IA", "MN", d11)
get_chisq("IA", "NC", d11)
get_chisq("IA", "NY", d11)
get_chisq("IA", "TX", d11)
get_chisq("IA", "WI", d11)

get_chisq("MN", "NC", d11)
get_chisq("MN", "NY", d11)
get_chisq("MN", "TX", d11)
get_chisq("MN", "WI", d11)

get_chisq("NC", "NY", d11)
get_chisq("NC", "TX", d11)
get_chisq("NC", "WI", d11)

get_chisq("NY", "TX", d11)
get_chisq("NY", "WI", d11)
get_chisq("TX", "WI", d11)

get_chisq_yr("CA", d10, dr11)
get_chisq_yr("IA", d10, dr11)
get_chisq_yr("MN", d10, dr11)
get_chisq_yr("NC", d10, dr11)
get_chisq_yr("NY", d10, dr11)
get_chisq_yr("TX", d10, dr11)
get_chisq_yr("WI", d10, dr11)

#################################################
################################################
# Step 5:  Age
#################################################
#################################################

# Figure 3a: Age 
####################################################################
table(data$NUM_AGE_0.2_MONTHS, data$SAMPLE_YEAR2)

# No Nebraska , main text
tapply(data$NUM_AGE_0.2_MONTHS, data$SAMPLE_YEAR2, sum)
tapply(data$NUM_AGE_2.6_MONTHS, data$SAMPLE_YEAR2, sum)
tapply(data$NUM_AGE_6._MONTHS, data$SAMPLE_YEAR2, sum)

# No Nebraska , main text
tapply(data.cvi$NUM_AGE_0.2_MONTHS, data.cvi$SAMPLE_YEAR2, sum)
tapply(data.cvi$NUM_AGE_2.6_MONTHS, data.cvi$SAMPLE_YEAR2, sum)
tapply(data.cvi$NUM_AGE_6._MONTHS, data.cvi$SAMPLE_YEAR2, sum)

data$wrongness_age<-data$NUM_SWINE-(data$NUM_AGE_0.2_MONTHS+data$NUM_AGE_2.6_MONTHS+data$NUM_AGE_6._MONTHS)
par(mfrow = c(1, 2))
hist(data$wrongness_age)
hist(data$wrongness_age[data$wrongness_age>50])
 #total number of shipments- length(data[,1])
 length(data[,1]) - length(data$wrongness_age[data$wrongness_age==0])  # 400 still off... = 6.9% wrong

# how much missing data
d<- length(data$NUM_AGE_0.2_MONTHS[data$NUM_AGE_0.2_MONTHS > 0 & data$NUM_AGE_2.6_MONTHS > 0 & data$NUM_AGE_6._MONTHS > 0])
d/ length(data$totalfem)


# Figure 3b: Sex  calculate sum of number of swine by year
####################################################################
# how much missing data in Num_Male/ Num_Female columns
data$wrongness_sex <- data$NUM_SWINE - 
	(data$NUM_MALE + data$NUM_FEMALE)
data2010$wrongness_sex <- data2010$NUM_SWINE - 
	(data2010$NUM_MALE+ data2010$NUM_FEMALE)
data2011$wrongness_sex <- data2011$NUM_SWINE - 
	(data2011$NUM_MALE + data2011$NUM_FEMALE)
par(mfrow = c(1, 3))
hist(data$wrongness_sex)
hist(data2010$wrongness_sex)
hist(data2011$wrongness_sex)

# no nebraska (only NUM_MALE/NUM_FEMALE column)
tapply(data$NUM_MALE, data$SAMPLE_YEAR2, sum)
tapply(data$NUM_FEMALE, data$SAMPLE_YEAR2, sum)

# with Nebraska
tapply(data.cvi$NUM_MALE, data.cvi$SAMPLE_YEAR2, sum)
tapply(data.cvi$NUM_FEMALE, data.cvi$SAMPLE_YEAR2, sum)

# how much missing data in Num_boar/barrow ect. 
# there are 1167 rows with information on either NUM_MALE2 or NUM_FEMALE2; when it is present, it matches NUM_SWINE
data$NUM_MALE2<-data$NUM_BOAR+data$NUM_BARROW
data$NUM_FEMALE2<-data$NUM_GILT+data$NUM_SOW
data$wrongness_sex2<-data$NUM_SWINE-(data$NUM_MALE2+data$NUM_FEMALE2)
par(mfrow = c(1,2))
hist(data$wrongness_sex)
hist(data$wrongness_sex2)

# calculates sum of number of gilt, boar, barrow ect. 
tapply(data$NUM_BOAR, data$SAMPLE_YEAR2, sum)
tapply(data$NUM_BARROW, data$SAMPLE_YEAR2, sum)
tapply(data$NUM_GILT, data$SAMPLE_YEAR2, sum)
tapply(data$NUM_SOW, data$SAMPLE_YEAR2, sum)

# Decide if we can sum the two characterizations (e.g. people either fill out the male/female column or the boar/barrow ect column):
data$totalsex<-data$NUM_MALE+data$NUM_FEMALE+ data$NUM_FEMALE2+data$NUM_MALE2
data$wrongness<-data$NUM_SWINE-data$totalsex
summary(data$wrongness)   # no non-negative numbers, so no duplicatess!

data$totalfem<-data$NUM_FEMALE+data$NUM_FEMALE2
tapply(data$totalfem, data$SAMPLE_YEAR2, sum)
data$totalmale<-data$NUM_MALE+data$NUM_MALE2
tapply(data$totalmale, data$SAMPLE_YEAR2, sum)

# Percent of shipmetns reporting sex information
d<- length(data$totalfem[data$totalmale > 0 & data$totalfem > 0])
d/ length(data$totalfem)

# Percent by year
d <- length(data$totalfem[data$totalmale > 0 & 
	data$totalfem > 0 & data$SAMPLE_YEAR2 == "2010"]) #7.3
d/ length(data$totalfem[data$SAMPLE_YEAR2 == "2010"])  # 6.3%
d <- length(data$totalfem[data$totalmale > 0 & 
	data$totalfem > 0 & data$SAMPLE_YEAR2 == "2011"])
d/ length(data$totalfem[data$SAMPLE_YEAR2 == "2011"])  # 8.5%

# For supplement
data.cvi$NUM_MALE2<-data.cvi$NUM_BOAR+ data.cvi$NUM_BARROW
data.cvi$NUM_FEMALE2<-data.cvi$NUM_GILT+ data.cvi$NUM_SOW
data.cvi$totalfem<-data.cvi$NUM_FEMALE+data.cvi$NUM_FEMALE2
tapply(data.cvi$totalfem, data.cvi$SAMPLE_YEAR2, sum)
data.cvi$totalmale<-data.cvi$NUM_MALE+data.cvi$NUM_MALE2
tapply(data.cvi$totalmale, data.cvi$SAMPLE_YEAR2, sum)



