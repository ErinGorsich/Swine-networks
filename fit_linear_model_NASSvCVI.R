###############################################
###############################################
###############################################
# NASS - CVI Number of farms per county script
# Erin Gorsich
###############################################
###############################################
###############################################
# Outline
# 1) Read in and groom ICVI data 
# 2) Calculate number of farms per county in the ICVI data
# 3) Add in NASS data and groom, add stateID, borderID, and yearID
# 4) Regression on data from all years together as in updated paper
# 5) Regression on each year separately (version 1, not used)
###############################################
###############################################
###############################################


###############################################
###############################################
# 1) Install packages and do preliminary grooming
###############################################
###############################################
# Read in packages
library(lme4)
library(MuMIn)
library(car)
library(RColorBrewer)
setwd("~/Documents/post-doc/Swine")

# Read in data
data.cvi<-read.csv("Swine_cvi_final.csv")
data.cvi <-data.cvi[!is.na(data.cvi$O_FIPS),]
data.cvi <-data.cvi[!is.na(data.cvi$D_FIPS),]
data.cvi$MOVE<-1

# Subset cvi data by year and groom
dat=data.cvi
dat= dat[!is.na(dat$NUM_SWINE),]  #-1
dat=dat[dat$NUM_SWINE>0,] # -13
dat=dat[!is.na(dat$SAMPLE_YEAR2),]  #-38 ####Clay added new collum for year#####
dat = dat[!is.na(dat$O_FIPS),]   # ADDED AFTER WRITE UP !!!!!!!!!! DOUBLE CHECK ESTIMATES IN MS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dat = dat[!is.na(dat$D_FIPS),]   # ADDED AFTER WRITE UP !!!!!!!!!! DOUBLE CHECK ESTIMATES IN MS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dat=dat[dat$NUM_SWINE>0,]
summary(dat)

data10<-dat[dat$SAMPLE_YEAR=="2010",]
data11<-dat[dat$SAMPLE_YEAR=="2011",]


###############################################
###############################################
# 2) calculate number of farms per county; origin and destination farms
###############################################
###############################################

calculate_number_farms <- function(data) {
  ################################################
  # function reads in the CVI data and returns a data.frame with
  # columns=NodeID, StateID, NumFarms, NumOFarms, NumDFarms
  ################################################  
  node.stats <- data.frame(matrix(0, 
  							nrow = length(unique(c(data[ ,"O_FIPS"], data[ ,"D_FIPS"]))),
    						ncol = 5, 
    						dimnames = list(NULL,  
    												  c("NodeID", "StateID", "NumFarms", 
    													"NumOFarms","NumDFarms"))))
 
  node.stats$NodeID <- sort(unique(c(data[ ,"O_FIPS"], data[ ,"D_FIPS"])))
  fipsmatch <- data.frame(ctfips = c(data$O_FIPS, data$D_FIPS), 
  										  stfips = c(data$O_ST_FIPS, data$D_ST_FIPS))
  node.stats$StateID <- fipsmatch$stfips[match(node.stats$NodeID, fipsmatch$ctfips)]
  
  #Calculate the number of farms per node
  temp.loc.data <- unique(cbind((c(as.numeric(data$O_FIPS_X), as.numeric(data$D_FIPS_X))),
                             			(c(as.numeric(data$O_FIPS_Y), as.numeric(data$D_FIPS_Y))),
                             			(c(as.numeric(data[,"O_FIPS"]), as.numeric(data[,"D_FIPS"]))),
                             			c(data$MOVE,data$MOVE)))
  
  temp <- aggregate(as.numeric(temp.loc.data[ ,4]), list(temp.loc.data[ ,3]),FUN = sum)
  node.stats$NumFarms <- temp$x
  
  temp.o.loc.data <- unique(cbind(as.factor(data$O_FIPS_X), as.factor(data$O_FIPS_Y),
                               				data[,"O_FIPS"], data$MOVE))
  o.temp <- aggregate(as.numeric(temp.o.loc.data[ ,4]), list(temp.o.loc.data[ ,3]),FUN = sum)
  for(k in 1:length(o.temp$Group.1)) {
    node.stats$NumOFarms[as.numeric(node.stats$NodeID) == as.numeric(o.temp$Group.1[k])] = o.temp$x[k]
  }
  
  #Calculate the number of farms per destination node
  temp.d.loc.data = unique(cbind(as.factor(data$D_FIPS_X), as.factor(data$D_FIPS_Y),
                               				data[ ,"D_FIPS"], data$MOVE))
  d.temp = aggregate(temp.d.loc.data[ ,4],list(temp.d.loc.data[ ,3]),FUN = sum)
  for(k in 1:length(d.temp$Group.1)) {
    node.stats$NumDFarms[as.numeric(node.stats$NodeID) == as.numeric(d.temp$Group.1[k])] = d.temp$x[k]
  }
  return(node.stats)
}

# apply function to calcuate get number of farms per county  
num_cvi_farms <- calculate_number_farms(dat)
num_cvi_farms2010 <- calculate_number_farms(data10)
num_cvi_farms2011 <- calculate_number_farms(data11)

# subset node.stats to only include states with data (only using states with data in both years...)
stateswant10<-c(6, 19, 27, 36, 37, 48, 55)
stateswant11<-c(6, 19, 27, 36, 37, 48, 55, 31)

cvidata10 <-num_cvi_farms2010[which(num_cvi_farms2010$StateID %in% stateswant10),]
cvidata11 <-num_cvi_farms2011[which(num_cvi_farms2011$StateID %in% stateswant11),]

# plot data to check model assumptions
hist(cvidata10$NumFarms)  # will need to think about Poisson/ Quasipoisson because log transformations don't make the distribution normal
length(cvidata10$NumFarms == 0)
hist(log(cvidata10$NumFarms))

###############################################
###############################################
# 3) Add in NASS data and groom, add stateID, borderID, and yearID
###############################################
###############################################        
NASS <- read.csv("NASS/NASS2012.csv")          # cleaned Operations with Inventory
dataframe <- NASS 
dataframe <- dataframe[!is.na(dataframe$County.ANSI),]  
dataframe <- dataframe[!is.na(dataframe$Value),]

# Get rid of commas in the Value column, standardize the Value, put in new column, "valsd"
dataframe$Value2 <- as.numeric(gsub(",", "", dataframe$Value))
#dataframe$valsd<-standardize(dataframe$Value2)
dataframe$FIPS <- NA

# create FIPS column, that merges the StateID and column ID
for (i in 1:length(dataframe[,1])){
  if (dataframe$County.ANSI[i] < 10) {
    dataframe$FIPS[i] <- paste(dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep = "00")
  }
  else if (dataframe$County.ANSI[i] < 100 & dataframe$County.ANSI[i] >= 10){
    dataframe$FIPS[i] <- paste(dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep = "0")
  }
  else {dataframe$FIPS[i] <- paste(dataframe$State.ANSI[i], dataframe$County.ANSI[i], sep = "")
  }  
}

# Subset and rename things so it is easier to work with
temp <- dataframe[,colnames(dataframe) %in% c("Year", "State.ANSI", "County.ANSI", "Value2", "FIPS")]
newdataframe <- data.frame(FIPS = unique(temp$FIPS))
newdataframe$newvalue <- NA

for (i in 1:length(newdataframe$FIPS)){
  val <- newdataframe$FIPS[i]
  tempdf <- temp[temp$FIPS == val,]
  newdataframe$newvalue[i] = sum(tempdf$Value2)
}

finaldata2010<-newdataframe
finaldata2010$NumCVIFarms <- cvidata10$NumFarms[match(finaldata2010$FIPS, cvidata10$NodeID)]
finaldata2010<- finaldata2010[!is.na(finaldata2010$NumCVIFarms),]
finaldata2011<-newdataframe
finaldata2011$NumCVIFarms<-cvidata11$NumFarms[match(finaldata2011$FIPS, cvidata11$NodeID)]
finaldata2011<- finaldata2011[!is.na(finaldata2011$NumCVIFarms),]

# Get which counties are boarder counties
###############################################
border<-read.csv("bordercounties.csv") 
finaldata2010$border_ind <- border$border_ind[match(finaldata2010$FIPS, border$fips)]
finaldata2011$border_ind <- border$border_ind[match(finaldata2011$FIPS, border$fips)]

finaldata2010$state <- border$state[match(finaldata2010$FIPS, border$fips)]
finaldata2011$state <- border$state[match(finaldata2011$FIPS, border$fips)]

# Dataset for both years
finaldata <- data.frame(rbind(finaldata2010, finaldata2011))
finaldata$year <- c(rep("2010", length(finaldata2010[,1])), rep("2011", length(finaldata2011[,1])))
finaldata <- finaldata[finaldata$state != "nebraska",]

###############################################
###############################################
# run model (with year as a co-variate): in paper
###############################################
###############################################     
plot(finaldata$NumCVIFarms~finaldata$newvalue)
points(finaldata$NumCVIFarms[finaldata$border_ind == 1]~ finaldata$newvalue[finaldata$border_ind == 1], pch = 19)
   
finaldata$state <- relevel(finaldata$state, "north carolina")

# additive model first
test.mod <- glm(NumCVIFarms ~ state + newvalue + border_ind + year, 
						 data = finaldata, family = quasipoisson(link = "log")); extractAIC(test.mod)

# conduct backwards selection
full.mod <- glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind + state*year +
						  newvalue*border_ind + newvalue*year + border_ind*year, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link = "log"))     
full.mod.pois <- glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  		  state*newvalue + state*border_ind + state*year +
						  		  newvalue*border_ind + newvalue*year + border_ind*year, 
						  		  data = finaldata[which(fitted(best) < 60), ], family = poisson(link = "log"))     
   
# model selection
###############################################
# extract the log likelihood form the regular model; dispersion parameter from the quasi version
dfun <- function(object) {
  with(object, sum((weights * residuals^2)[weights > 0]) / df.residual)
}
logLik(full.mod.pois)

# Drop step 1!
drop.mod1<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind + state*year +
						  newvalue*border_ind + newvalue*year, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))     
drop.p1<-update(drop.mod1, family=poisson)
drop.mod2<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind + state*year +
						  newvalue*border_ind + border_ind*year, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))  
drop.p2 <- update(drop.mod2, family=poisson)
drop.mod3 <- glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind + state*year +
						  newvalue*year + border_ind*year, 
						  data = finaldata[which(fitted(best) < 60), ], family=quasipoisson(link="log"))     
drop.p3 <- update(drop.mod3, family=poisson)
drop.mod4 <- glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind +
						  newvalue*border_ind + newvalue*year + border_ind*year, 
						  data = finaldata[which(fitted(best) < 60), ], family=quasipoisson(link="log"))
drop.p4 <- update(drop.mod4, family=poisson)
drop.mod5 <- glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*year +
						  newvalue*border_ind + newvalue*year + border_ind*year,
						  data = finaldata[which(fitted(best) < 60), ], family=quasipoisson(link="log"))
drop.p5 <- update(drop.mod5, family=poisson)
drop.mod6 <- glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*border_ind + state*year +
						  newvalue*border_ind + newvalue*year + border_ind*year,
						  data = finaldata[which(fitted(best) < 60), ], family=quasipoisson(link="log"))
drop.p6 <- update(drop.mod6, family=poisson)

# we want to choose the model with the lowest AIC
chat <- deviance(full.mod.pois)/df.residual(full.mod.pois) 

QAIC(full.mod.pois, chat=chat) # 1550.633
QAIC(drop.p1, chat=chat) 
QAIC(drop.p2, chat=chat)
QAIC(drop.p3, chat=chat)
QAIC(drop.p4, chat=chat) # lowest, remove boarder*year 1542.843
QAIC(drop.p5, chat=chat)
QAIC(drop.p6, chat=chat)

# Drop step 2
drop.mod1<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind +
						  newvalue*border_ind + newvalue*year ,  # + border_ind*year
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))     
drop.p1 <- update(drop.mod1, family=poisson)
drop.mod2<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind +
						  newvalue*border_ind + border_ind*year,  #+ newvalue*year 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))    
drop.p2 <- update(drop.mod2, family=poisson)						  
drop.mod3<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind +
						  newvalue*year + border_ind*year,  # newvalue*border_ind +
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))    
drop.p3 <- update(drop.mod3, family=poisson)
drop.mod4<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + #state*border_ind +
						  newvalue*border_ind + newvalue*year + border_ind*year, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))    
drop.p4 <- update(drop.mod4, family=poisson)
drop.mod5<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*border_ind + # cut state*newvalue +
						  newvalue*border_ind + newvalue*year + border_ind*year, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))    			
drop.p5 <- update(drop.mod5, family=poisson)
						  
QAIC(drop.p1, chat=chat) # lowest 1540.843
QAIC(drop.p2, chat=chat)
QAIC(drop.p3, chat=chat)
QAIC(drop.p4, chat=chat)
QAIC(drop.p5, chat=chat)

# Drop step 3
drop.mod1<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind +
						  newvalue*border_ind,  # cut: + newvalue*year  
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))     
drop.p1 <- update(drop.mod1, family=poisson)
drop.mod2<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind +
						  newvalue*year ,  #newvalue*border_ind + 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))     
drop.p2 <- update(drop.mod2, family=poisson)
drop.mod3<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + # cut: state*border_ind +
						  newvalue*border_ind + newvalue*year , 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))     
drop.p3 <- update(drop.mod3, family=poisson)
drop.mod4<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*border_ind + # state*newvalue + 
						  newvalue*border_ind + newvalue*year , 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))     
drop.p4 <- update(drop.mod4, family=poisson)

QAIC(drop.p1, chat=chat)   
QAIC(drop.p2, chat=chat) # 1538.846
QAIC(drop.p3, chat=chat)
QAIC(drop.p4, chat=chat)



# Drop step 4
drop.mod1<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue + state*border_ind, # +  newvalue*year ,  
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))   
drop.p1 <- update(drop.mod1, family=poisson)
drop.mod2<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue +  newvalue*year ,  #state*border_ind +
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log")) 
drop.p2 <- update(drop.mod2, family=poisson)
drop.mod3<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*border_ind +  newvalue*year ,   #state*newvalue + 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log")) 
drop.p3 <- update(drop.mod3, family=poisson)

QAIC(drop.p1, chat=chat)  
QAIC(drop.p2, chat=chat) # 1538
QAIC(drop.p3, chat=chat)

# Drop step 5
drop.mod1<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						  state*newvalue, #+  newvalue*year  
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log")) 
drop.p1 <- update(drop.mod1, family=poisson)
drop.mod2<-glm(NumCVIFarms ~  state + newvalue + border_ind + year + 
						    newvalue*year , #state*newvalue +
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log")) 
drop.p2 <- update(drop.mod2, family=poisson)
QAIC(drop.p1, chat=chat)  #1537.815
QAIC(drop.p2, chat=chat) 

# Drop step 6
drop.mod1<-glm(NumCVIFarms ~  state + newvalue + border_ind + state*newvalue, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log")) 
drop.p1 <- update(drop.mod1, family=poisson)
QAIC(drop.p1, chat=chat)  #1536.642

best <- glm(NumCVIFarms ~  state + newvalue + border_ind + state*newvalue, 
						  data = finaldata, family = quasipoisson(link="log"))  
# In paper
bestred <- glm(NumCVIFarms ~  state + newvalue + border_ind + state*newvalue, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))     


par(mfrow = c(1,2))
plot(resid(best)~fitted(best))
plot(resid(bestred)~fitted(bestred))
# one county in Iowa is causing this problem, with the highest values of both 
finaldata[which(fitted(best) > 60), ]
		
plot(resid(drop.mod1)~fitted(drop.mod1))
leveragePlots(best)

hist(resid(best))
hist(resid(drop.mod4))
				  			  
				  			  
				  			  
				  			  
	
###############################################
###############################################
# Figure B2
###############################################
###############################################    	
par(mfrow = c(1,2))
plot.mod <- glm(NumCVIFarms ~  log(newvalue) + border_ind, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log")) 

plot(jitter(log(finaldata$NumCVIFarms[finaldata$border_ind == 0])) ~ 
       log(finaldata$newvalue[finaldata$border_ind == 0]), 
       xlab = "Log number of farms per county, NASS", 
       ylab = "Log number of farms per county, ICVI", 
       ylim = c(0, 4), las = 1, cex.lab = 1.2, bty = "l")
points(log(finaldata$NumCVIFarms[finaldata$border_ind == 1]) ~ 
           log(finaldata$newvalue[finaldata$border_ind == 1]), pch = 19)
legend("topleft", pch = c(19, 21), lty = c(1, 2), legend = c("border county", "non-border county"), 
           bty = "n")
xval <- seq(1, 300, 1)	
center	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval, border_ind = rep(0, length(xval))),
  						levels = levels(border_ind))
border	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval, border_ind = rep(1, length(xval))),
  						levels = levels(border_ind))
lines(log(xval), log(center$fit), lty = 2)	
lines(log(xval), log(border$fit), lty = 1)	

# not log transformed
plot.mod <- glm(NumCVIFarms ~ newvalue + border_ind, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log")) 

plot(jitter(log(finaldata$NumCVIFarms[finaldata$border_ind == 0 & finaldata$NumCVIFarms < 60])) ~ 
      finaldata$newvalue[finaldata$border_ind == 0 & finaldata$NumCVIFarms < 60], 
       xlab = "Number of farms per county, NASS", 
       ylab = "Log number of farms per county, ICVI", 
       ylim = c(0, 4), las = 1, cex.lab = 1.2, bty = "l")
points(log(finaldata$NumCVIFarms[finaldata$border_ind == 1 & finaldata$NumCVIFarms < 60]) ~ 
          finaldata$newvalue[finaldata$border_ind == 1 & finaldata$NumCVIFarms < 60], pch = 19)
legend("topleft", pch = c(19, 21), lty = c(1, 2), legend = c("border county", "non-border county"), 
           bty = "n")
xval <- seq(1, 300, 1)	
center	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval, border_ind = rep(0, length(xval))),
  						levels = levels(border_ind))
border	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval, border_ind = rep(1, length(xval))),
  						levels = levels(border_ind))
lines(xval, log(center$fit), lty = 2)	
lines(xval, log(border$fit), lty = 1)	
					  			  
# Broken down by state		
###############################################
###############################################
colors <- brewer.pal(7, "Set1")

# bestred model			  			  
plot.mod <- glm(NumCVIFarms ~  state + newvalue + border_ind + state*newvalue, 
						  data = finaldata[which(fitted(best) < 60), ], family = quasipoisson(link="log"))   
df <- finaldata[finaldata$NumCVIFarms < 60, ]
plot(NULL,
       xlab = "Number of farms per county, NASS", 
       ylab = "Log number of farms per county, ICVI", 
       ylim = c(0, 4), xlim = c(0, 300), las = 1, cex.lab = 1.2, bty = "l")
       #jitter(log(df$NumCVIFarms[df$border_ind == 0])) ~  df$newvalue[df$border_ind == 0], 

points(log(df$NumCVIFarms[df$border_ind == 1 & df$state == "california"]) ~ 
          df$newvalue[df$border_ind == 1 & df$state == "california"], pch = 19, col = colors[1], cex = 0.6)
points(log(df$NumCVIFarms[df$border_ind == 1 & df$state == "iowa"]) ~ 
          df$newvalue[df$border_ind == 1 & df$state == "iowa"], pch = 19, col = colors[2], cex = 0.6)          
points(log(df$NumCVIFarms[df$border_ind == 1 & df$state == "minnesota"]) ~ 
          df$newvalue[df$border_ind == 1 & df$state == "minnesota"], pch = 19, col = colors[3], cex = 0.6)     

points(jitter(log(df$NumCVIFarms[df$border_ind == 0 & df$state == "california"])) ~ 
          df$newvalue[df$border_ind == 0 & df$state == "california"], col = colors[1], cex = 0.6)
points(jitter(log(df$NumCVIFarms[df$border_ind == 0 & df$state == "iowa"])) ~ 
          df$newvalue[df$border_ind == 0 & df$state == "iowa"], col = colors[2], cex = 0.6)          
points(jitter(log(df$NumCVIFarms[df$border_ind == 0 & df$state == "minnesota"])) ~ 
          df$newvalue[df$border_ind == 0 & df$state == "minnesota"],  col = colors[3], cex = 0.6)     
              
legend("topleft", pch = c(19, 21), lty = c(1, 2), legend = c("border county", "non-border county"), 
           bty = "n")
xval <- seq(1, 300, 1)	
n <- length(xval)
center_ca	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval, state = rep("california", n), border_ind = rep(0, n)),
  						levels = levels(border_ind))
border_ca	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval,  state = rep("california", n), border_ind = rep(1, length(xval))),
  						levels = levels(border_ind))
center_ia	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval, state = rep("iowa", n), border_ind = rep(0, n)),
  						levels = levels(border_ind))
border_ia	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval,  state = rep("iowa", n), border_ind = rep(1, length(xval))),
  						levels = levels(border_ind))  						
center_mn	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval, state = rep("minnesota", n), border_ind = rep(0, n)),
  						levels = levels(border_ind))
border_mn	<- predict(plot.mod, type = "response", se.fit = TRUE, 
  						data.frame(newvalue = xval,  state = rep("minnesota", n), border_ind = rep(1, length(xval))),
  						levels = levels(border_ind))
lines(xval, log(center_ca$fit), lty = 2, col = colors[1])	
lines(xval, log(border_ca$fit), lty = 1, col = colors[1])						  			  
lines(xval, log(center_ia$fit), lty = 2, col = colors[2])	
lines(xval, log(border_ia$fit), lty = 1, col = colors[2])								  			  
lines(xval, log(center_mn$fit), lty = 2, col = colors[3])	
lines(xval, log(border_mn$fit), lty = 1, col = colors[3])								  			  
					  			  


make_plot <- function (df, state, colors, main) {
	plot(NULL,
    		main = main,
    		xlab = "Number of farms per county, NASS", 
     	ylab = "Log number of farms per county, ICVI", 
     	ylim = c(0, 4), xlim = c(0, 300), las = 1, cex.lab = 1.2, bty = "l", cex.axis = 1.2)

	points(log(df$NumCVIFarms[df$border_ind == 1 & df$state == state]) ~ 
     		df$newvalue[df$border_ind == 1 & df$state == state], pch = 19, col = colors)
	points(jitter(log(df$NumCVIFarms[df$border_ind == 0 & df$state == state])) ~ 
         	df$newvalue[df$border_ind == 0 & df$state == state], col = colors)
	if (state == "california") {
		legend("topleft", pch = c(19, 21), lty = c(1, 2), legend = c("border county", "non-border county"), 
					bty = "n", col = c(colors, colors))
	}
	xval <- seq(1, max(df$newvalue[df$state == state]) + 5, 1)	
	n <- length(xval)
	center <- predict(plot.mod, type = "response", se.fit = TRUE, 
  							data.frame(newvalue = xval, state = rep(state, n), border_ind = rep(0, n)),
  							levels = levels(border_ind))
	border <- predict(plot.mod, type = "response", se.fit = TRUE, 
  							data.frame(newvalue = xval,  state = rep(state, n), border_ind = rep(1, length(xval))),
  							levels = levels(border_ind))
	lines(xval, log(center$fit), lty = 2, col = colors)	
	lines(xval, log(border$fit), lty = 1, col = colors)
}
statelist <- as.character(unique(finaldata$state))
mainlist <- c("California", "Iowa", "Minnesota", "New York", "North Carolina", "Texas", "Wisconsin")
df <- finaldata[finaldata$NumCVIFarms < 60, ]

par(mfrow = c(2, 4))
	i <- 1
for (state in statelist) {
	print(state)
	make_plot(df, state,  colors[i], mainlist[i])
	i <- i + 1
}
				  			  
###############################################
###############################################
# run model (by year separately, not used!)
###############################################
###############################################               
# additive model first
test.mod<-glm(NumCVIFarms~ state+newvalue+border_ind, data=finaldata2010, family=quasipoisson(link="log")); extractAIC(test.mod)

# conduct backwards selection
full.mod<-glm(NumCVIFarms~ state*newvalue+state*border_ind+ newvalue*border_ind, data=finaldata2010, family=quasipoisson(link="log"))     
full.mod.pois<-glm(NumCVIFarms~ state*newvalue+state*border_ind+ newvalue*border_ind, data=finaldata2010, family=poisson(link="log"))     

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


