  ###############################################
  ###############################################
 # THIS SCRIPT CALCULATES THE NUMBER OF FARMS PER COUNTY 
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
  setwd("~/Documents/post-doc/Swine")
  # Read in data
  data.cvi<-read.csv("Swine_cvi_final.csv")
  data.cvi= data.cvi[!is.na(data.cvi$NUM_SWINE),]  #-1
  data.cvi=data.cvi[data.cvi$NUM_SWINE>0,] # -13
  data.cvi=data.cvi[!is.na(data.cvi$SAMPLE_YEAR2),]  #-38 ####Clay added new collum for year#####
  data.cvi=data.cvi[data.cvi$NUM_SWINE>0,]
  summary(data.cvi)
  colnames(data.cvi)
  # make a new column of all ones that represents the number of shipments
  data.cvi$MOVE <-1
  data.cvi<-data.cvi[, c("STATE", "SAMPLE_YEAR2", "PURPOSE", "NUM_SWINE", "NUM_BOAR", 
               "NUM_BARROW", "NUM_GILT", "NUM_SOW", "NUM_AGE_0.2_MONTHS", 
               "NUM_AGE_2.6_MONTHS", "NUM_AGE_6._MONTHS", "NUM_MALE", "NUM_FEMALE",
               "D_STATE", "D_FIPS_X", "D_FIPS_Y",  "O_FIPS_X", "O_FIPS_Y", "O_STATE", 
               "D_FIPS", "O_FIPS", "O_ST_FIPS", "D_ST_FIPS", "move" )]
    
  data.cvi <-data.cvi[!is.na(data.cvi$O_FIPS),]
  data.cvi <-data.cvi[!is.na(data.cvi$D_FIPS),]
 
 # Subset cvi data by year.
 data10<-data.cvi[data.cvi$SAMPLE_YEAR2=="2010",]
 data11<-data.cvi[data.cvi$SAMPLE_YEAR2=="2011",]
  data.cvi$MOVE<-1
  data10$MOVE<-1
  data11$MOVE<-1
 
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
      num_cvi_farms<- calculate_number_farms(data.cvi)
        # subset node.stats to only include states with data
      #num_cvi_farms2010
      stateswant10<-c(6, 19, 27, 36, 37, 40, 48, 55)
      stateswant11<-c(6, 19, 27, 36, 37, 40, 48, 55, 31)

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

  NASS<- read.csv("NASS2012.csv")        
  NASS<-NASS[!is.na(NASS$County.ANSI),]
  
  
  NASS$FIPS<-NA
  # create FIPS column, that merges the StateID and column ID
  for (i in 1:length(NASS[,1])){
    if (NASS$County.ANSI[i]<10) {
      NASS$FIPS[i]<-paste(NASS$State.ANSI[i], NASS$County.ANSI[i], sep="00")
    }
    else if (NASS$County.ANSI[i]<100 & NASS$County.ANSI[i]>=10){
      NASS$FIPS[i]<-paste(NASS$State.ANSI[i], NASS$County.ANSI[i], sep="0")
    }
    else {NASS$FIPS[i]<-paste(NASS$State.ANSI[i], NASS$County.ANSI[i], sep="")
    }  
  }
  
  
  
   finaldata<-NASS[c(NASS$State.ANSI=="6"| NASS$State.ANSI=="19"| NASS$State.ANSI=="27"|
                     NASS$State.ANSI=="36"| NASS$State.ANSI=="37"| NASS$State.ANSI=="40"|
                     NASS$State.ANSI=="40", NASS$State.ANSI=="48"| NASS$State.ANSI=="55"), ] 
   
  finaldata<-finaldata[,c(1,3, 7,9)]
    finaldata$NumFarms<-cvidata10$NumFarms[match(finaldata$FIPS, cvidata10$NodeID)]
  finaldata<-finaldata[!is.na(finaldata$NumFarms),]
  colnames(finaldata)<-c("Year", "State.ID", "NASS", "Node.ID", "NumFarms")
  
  ###############################################
  ###############################################
  # Get which counties are boarder counties
  ###############################################
  ###############################################        
   border<-read.table("~/Documents/post-doc/Swine/cntydist.asc.txt", header=FALSE)  
   border<-border[,c(1:8)]
   colnames(border)<- c("bordindx", "ST-ST", "st1", "st2", "st", "county", "mindist", "milemark")
#	1.	BORDINDX Index of border
#	2.	ST1ST2 Name of the border
#	3.	ST1 State 1 of the border pair
#	4.	ST2 State 2 of the border pair
#	5.	ST
#	6.	COUNTY
#	7.	MINDIST Minimum distance of county from border.
#	8.	MILEMARK Point along border where minimum is obtained.
 
 border$state2<- as.numeric(gsub(",", "", border$st))
 border$county2<-gsub(",", "", border$county)
 border$FIPS<- paste(border$state2, border$county2, sep="")
 
        
 ###############################################
  ###############################################
  # run model
  ###############################################
  ###############################################                     
  finaldata$State.ID<-as.factor(finaldata$State.ID )     
        
  test.mod<-glmer(NumFarms~ State.ID+ NASS +(1|Node.ID), family=poisson(link="log"), data=finaldata)  
  test.mod<-glmer(NumFarms~ State.ID*NASS +(1|Node.ID), family=poisson(link="log"), data=finaldata)     
  
  summary(test.mod)
#  test.mod<-glmer(NumFarms~ StateID+ border+ NASS +(1|NodeID), family=Poisson(link="log"))     
  
  cor(finaldata$NASS, finaldata$NumFarms)      
        
  # test model assumptions---   try quasipoisson
  # try with next year of data
  # add border effect
  # do formal model selection
        
        