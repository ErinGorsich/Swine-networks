setwd("/Users/u1774615/Dropbox/Swine")

# Read in data
data.cvi<-read.csv("Swine_cvi_final.csv")
data.cvi <- data.cvi[!is.na(data.cvi$NUM_SWINE),] 
data.cvi <- data.cvi[data.cvi$NUM_SWINE>0,] 
data.cvi <- data.cvi[!is.na(data.cvi$SAMPLE_YEAR2),] 
data.cvi <- data.cvi[data.cvi$NUM_SWINE>0,]
data.cvi <-data.cvi[!is.na(data.cvi$O_FIPS),]
data.cvi <-data.cvi[!is.na(data.cvi$D_FIPS),]
data.cvi$MOVE <- 1
colnames(data.cvi)

# Subset cvi data by year.
data10 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2010",]
data11 <- data.cvi[data.cvi$SAMPLE_YEAR2=="2011",]

# remove 31, 40
data10 <- data10[data10$O_ST_FIPS != 31, ]
data10 <- data10[data10$O_ST_FIPS != 40, ]

cols <- c('O_ST_FIPS', "D_ST_FIPS", 'O_STATE', 'D_STATE')
data10[cols] <- sapply(data10[cols], as.character)
data11[cols] <- sapply(data11[cols], as.character)

el <- unique(cbind(data10[,"O_ST_FIPS"], data10[,"D_ST_FIPS"]))
edgelist.st.2010 <- data.frame(
  origin.state = rep("", length(el[,1])),
  destination.state = rep("", length(el[,1])), 
  origin.fips = el[, 1], 
  destination.fips = el[ , 2],
  number.shipments = rep(0, length(el[, 1])), 
  number.swine = rep(0, length(el[, 1])))
edgelist.st.2010$origin.state <- data10$O_STATE[match(edgelist.st.2010$origin.fips, data10$O_ST_FIPS)]
edgelist.st.2010$destination.state <-  data10$D_STATE[match(edgelist.st.2010$destination.fips, data10$D_ST_FIPS)]
table(edgelist.st.2010$origin.fips); table(edgelist.st.2010$origin.state)


el <- unique(cbind(data11[,"O_ST_FIPS"], data11[,"D_ST_FIPS"]))
edgelist.st.2011 <- data.frame(
  origin.state = rep("", length(el[ ,1])),
  destination.state = rep("", length(el[ ,1])), 
  origin.fips = el[, 1], 
  destination.fips = el[ , 2],
  number.shipments = rep(0, length(el[, 1])), 
  number.swine = rep(0, length(el[, 1])))
edgelist.st.2011$origin.state <- data11$O_STATE[match(edgelist.st.2011$origin.fips, data11$O_ST_FIPS)]
edgelist.st.2011$destination.state <-  data11$D_STATE[match(edgelist.st.2011$destination.fips, data11$D_ST_FIPS)]

for (i in 1:length(edgelist.st.2010[ ,1])) {
  temp <- data10[c(data10$O_ST_FIPS == edgelist.st.2010$origin.fips[i] & data10$D_ST_FIPS == edgelist.st.2010$destination.fips[i]), ]
  edgelist.st.2010$number.shipments[i] <- sum(temp$MOVE)
  edgelist.st.2010$number.swine[i] <- sum(temp$NUM_SWINE)
  rm (temp)  
}

for (i in 1:length(edgelist.st.2011[ ,1])) {
  temp <- data11[c(data11$O_ST_FIPS == edgelist.st.2011$origin.fips[i] & data11$D_ST_FIPS == edgelist.st.2011$destination.fips[i]), ]
  edgelist.st.2011$number.shipments[i] <- sum(temp$MOVE)
  edgelist.st.2011$number.swine[i] <- sum(temp$NUM_SWINE)
  rm (temp)
}

write.csv(edgelist.st.2010, "Gorsich_Miller_et_al_state_edgelist_2010.csv")
write.csv(edgelist.st.2011, "Gorsich_Miller_et_al_state_edgelist_2011.csv")


