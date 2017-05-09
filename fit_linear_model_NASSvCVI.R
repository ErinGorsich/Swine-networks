#####################6/26##################################
########################################################
########################################################
###############################################
###############################################
# TO HOLLY:  THIS SCRIPT CALCULATES THE NUMBER OF FARMS PER COUNTY 
# IN THE CVI DATA AND THE NUMBER OF PREMISES PER COUNTY IN THE NASS
# DATA AND RUNS A LINEAR REGRESSION TO COMPARE THE TWO!
###############################################
###############################################

###############################################
###############################################
# Install packages and do preliminary grooming
###############################################
###############################################
# Read in packages
library(lme4)

# Read in data
data.cvi<-read.csv("Swine_cvi_final.csv")
data.cvi <-data.cvi[!is.na(data.cvi$O_FIPS),]
data.cvi <-data.cvi[!is.na(data.cvi$D_FIPS),]

#@@@@@@@ MORE NEEDED HERE @@@@@@@@@@ 
# add other data grooming steps
# add data grooming steps here!
data.cvi$MOVE<-1

# Subset cvi data by year.
dat=data.cvi
dat= dat[!is.na(dat$NUM_SWINE),]  #-1
dat=dat[dat$NUM_SWINE>0,] # -13
dat=dat[!is.na(dat$SAMPLE_YEAR2),]  #-38 ####Clay added new collum for year#####
dat=dat[dat$NUM_SWINE>0,]
summary(dat)

data10<-data.cvi[data.cvi$SAMPLE_YEAR=="2010",]
data11<-data.cvi[data.cvi$SAMPLE_YEAR=="2011",]



###############################################
###############################################
# calculate number of farms per county; origin and destination farms
###############################################
###############################################
calculate_number_farms=function(data){
  # function reads in the CVI data and returns a data.frame with columns=NodeID, StateID, NumFarms, NumOFarms, NumDFarms
  node.stats=data.frame(matrix(0,nrow=length(unique(c(data[,"O_FIPS"],
                                                      data[,"D_FIPS"]))),
                               ncol=5,
                               dimnames=list(NULL,
                                             c("NodeID", "StateID", "NumFarms","NumOFarms","NumDFarms"))))
  
  
  node.stats$NodeID=sort(unique(c(data[,"O_FIPS"],  data[,"D_FIPS"])))
  
  fipsmatch=data.frame(ctfips=c(data$O_FIPS, data$D_FIPS), stfips=c(data$O_ST_FIPS, data$D_ST_FIPS))
  node.stats$StateID=fipsmatch$stfips[match(node.stats$NodeID, fipsmatch$ctfips)]
  
  #Calculate the number of farms per node
  temp.loc.data=unique(cbind((c(as.numeric(data$O_FIPS_X),as.numeric(data$D_FIPS_X))),
                             (c(as.numeric(data$O_FIPS_Y),as.numeric(data$D_FIPS_Y))),
                             (c(as.numeric(data[,"O_FIPS"]),
                                as.numeric(data[,"D_FIPS"]))),
                             c(data$MOVE,data$MOVE)))
  
  
  temp=aggregate(as.numeric(temp.loc.data[,4]),list(temp.loc.data[,3]),FUN=sum)
  node.stats$NumFarms=temp$x
  
  temp.o.loc.data=unique(cbind(as.factor(data$O_FIPS_X),as.factor(data$O_FIPS_Y),
                               data[,"O_FIPS"],
                               data$MOVE))
  o.temp=aggregate(as.numeric(temp.o.loc.data[,4]),list(temp.o.loc.data[,3]),FUN=sum)
  for(k in 1:length(o.temp$Group.1)){
    node.stats$NumOFarms[as.numeric(node.stats$NodeID)==as.numeric(o.temp$Group.1[k])]=o.temp$x[k]
  }
  
  #Calculate the number of farms per destination node
  temp.d.loc.data=unique(cbind(as.factor(data$D_FIPS_X),as.factor(data$D_FIPS_Y),
                               data[,"D_FIPS"],
                               data$MOVE))
  d.temp=aggregate(temp.d.loc.data[,4],list(temp.d.loc.data[,3]),FUN=sum)
  for(k in 1:length(d.temp$Group.1)){
    node.stats$NumDFarms[as.numeric(node.stats$NodeID)==as.numeric(d.temp$Group.1[k])]=d.temp$x[k]
  }
  return(node.stats)
}


# apply function to calcuate get number of farms per county  
num_cvi_farms2010<- calculate_number_farms(data10)
num_cvi_farms2011<- calculate_number_farms(data11)

# subset node.stats to only include states with data
#num_cvi_farms2010
stateswant10<-c(6, 19, 27, 36, 37, 48, 55)
stateswant11<-c(6, 19, 27, 36, 37, 48, 55, 31)

cvidata10 <-num_cvi_farms2010[which(num_cvi_farms2010$StateID %in% stateswant10),]
cvidata11 <-num_cvi_farms2011[which(num_cvi_farms2011$StateID %in% stateswant11),]

# plot data to check model assumptions
hist(cvidata10$NumFarms)  # will need to think about Poisson/ Quasipoisson because log transformations don't make the distribution normal
length(cvidata10$NumFarms==0)
hist(log(cvidata10$NumFarms))

###############################################
###############################################
# add in NASS data and groom
###############################################
###############################################        
NASS<-read.csv("NASS2012.csv")          # cleaned Operations with Inventory
dataframe=NASS 
dataframe<-dataframe[!is.na(dataframe$County.ANSI),]  
dataframe<-dataframe[!is.na(dataframe$Value),]
# Get rid of commas in the Value column, standardize the Value, put in new column, "valsd"
dataframe$Value2<-as.numeric(gsub(",", "", dataframe$Value))
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
temp<- dataframe[,colnames(dataframe) %in% c("Year", "State.ANSI", "County.ANSI", "Value2", "FIPS")]
newdataframe=data.frame(FIPS=unique(temp$FIPS))
newdataframe$newvalue<-NA

for (i in 1:length(newdataframe$FIPS)){
val=newdataframe$FIPS[i]
tempdf=temp[temp$FIPS==val,]
newdataframe$newvalue[i]=sum(tempdf$Value2)
}


finaldata2010<-newdataframe
finaldata2010$NumCVIFarms<-cvidata10$NumFarms[match(finaldata2010$FIPS, cvidata10$NodeID)]
finaldata2010<- finaldata2010[!is.na(finaldata2010$NumCVIFarms),]
finaldata2011<-newdataframe
finaldata2011$NumCVIFarms<-cvidata11$NumFarms[match(finaldata2011$FIPS, cvidata11$NodeID)]
finaldata2011<- finaldata2011[!is.na(finaldata2011$NumCVIFarms),]


###############################################
###############################################
# Get which counties are boarder counties
###############################################
###############################################        
border<-read.csv("bordercounties.csv") 
finaldata2010$border_ind<-border$border_ind[match(finaldata2010$FIPS, border$fips)]
finaldata2011$border_ind<-border$border_ind[match(finaldata2011$FIPS, border$fips)]

finaldata2010$state<-border$state[match(finaldata2010$FIPS, border$fips)]
finaldata2011$state<-border$state[match(finaldata2011$FIPS, border$fips)]
###############################################
###############################################
# run model
###############################################
###############################################               
# additive model first
test.mod<-glm(NumCVIFarms~ state+newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log")); extractAIC(test.mod)     

# conduct backwards selection
library("MuMIn")
#library("bbmle")
full.mod<-glm(NumCVIFarms~ state*newvalue+state*border_ind+ newvalue*border_ind, data=finaldata2010, family=quasipoisson(link="log"))     
full.mod.pois<-glm(NumCVIFarms~ state*newvalue+state*border_ind+ newvalue*border_ind, data=finaldata2010, family=poisson(link="log"))     

# extract the log likelihood form the regular model; dispersion parameter from the quasi version
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}   #dfun(full.mod)= 2.92
logLik(full.mod.pois)

drop.mod1<-glm(NumCVIFarms~ state*border_ind+ newvalue*border_ind, data=finaldata2010, family=quasipoisson(link="log"))     
drop.p1<-update(drop.mod1, family=poisson)
drop.mod2<-glm(NumCVIFarms~ state*newvalue+ newvalue*border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
drop.p2<-update(drop.mod2, family=poisson)
drop.mod3<-glm(NumCVIFarms~ state*newvalue+state*border_ind, data=finaldata2010, family=quasipoisson(link="log"))     
drop.p3<-update(drop.mod3, family=poisson)

# we want to choose the model with the lowest AIC
#deviance(full.mod)/df.residual(full.mod)
chat=deviance(full.mod.pois)/df.residual(full.mod.pois) # same

QAIC(full.mod.pois, chat=chat)
QAIC(drop.p1, chat=chat)
QAIC(drop.p2, chat=chat) # selected, qAIC=727.43
QAIC(drop.p3, chat=chat)

#old, ignore
#qAIC(full.mod.pois, dispersion=dfun(full.mod.pois))  # get qAIC from poisson model
#qAIC(drop.p1, dispersion=dfun(drop.p1))  # get qAIC from poisson model
#qAIC(drop.p1, dispersion=dfun(full.mod.pois))
#ICtab(full.mod.pois, drop.p1, drop.p2, drop.p3, dispersion=dfun(full.mod.pois), type="qAIC")

# repeat dropping procedure:
drop.mod2<-glm(NumCVIFarms~ state*newvalue+ newvalue*border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
drop.p2<-update(drop.mod2, family=poisson)
drop.mod4<-glm(NumCVIFarms~ state+newvalue+border_ind+ newvalue*border_ind, data=finaldata2010, family=quasipoisson(link="log")) 
drop.p4<-update(drop.mod4, family=poisson)
drop.mod5<-glm(NumCVIFarms~ state*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
drop.p5<-update(drop.mod5, family=poisson)

QAIC(drop.p2, chat=chat)
QAIC(drop.p4, chat=chat)
QAIC(drop.p5, chat=chat)  # selected qAIC=726.32

# repeat dropping of last interaction term
drop.mod5<-glm(NumCVIFarms~ state*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
drop.p5<-update(drop.mod5, family=poisson)
drop.mod6<-glm(NumCVIFarms~ state+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
drop.p6<-update(drop.mod6, family=poisson)
QAIC(drop.p6, chat=chat) # not selected, keep mod5 and drop main effects


drop.mod7<-glm(NumCVIFarms~ state*newvalue, data=finaldata2010, family=quasipoisson(link="log"))  
drop.p7<-update(drop.mod7, family=poisson)
QAIC(drop.p7, chat=chat) # not selected, keep mod5 and drop main effects

summary(drop.mod5)
finaldata2010$state2<-relevel(finaldata2010$state, "iowa")
finaldata2010$state3<-relevel(finaldata2010$state, "texas")
finaldata2010$state4<-relevel(finaldata2010$state, "wisconsin")
finaldata2010$state5<-relevel(finaldata2010$state, "new york")
finaldata2010$state6<-relevel(finaldata2010$state, "minnesota")
finaldata2010$state7<-relevel(finaldata2010$state, "north carolina")

test.mod<-glm(NumCVIFarms~ state2*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
summary(test.mod)
test.mod<-glm(NumCVIFarms~ state3*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
summary(test.mod)
test.mod<-glm(NumCVIFarms~ state4*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
summary(test.mod)
test.mod<-glm(NumCVIFarms~ state6*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
summary(test.mod)
test.mod<-glm(NumCVIFarms~ state7*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
summary(test.mod)
#NY
test.mod<-glm(NumCVIFarms~ state5*newvalue+ newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log"))  
summary(test.mod)


