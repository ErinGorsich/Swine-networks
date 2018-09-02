########################################
########################################
# Figure Code
########################################
########################################

setwd("~/Documents/post-doc/Swine")
source('~/GitHub/bTB-bruc-co-infection-ms/pde/multiplot.R', chdir = TRUE)
library(ggplot2)

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

########################################
########################################
# 2) Figure 1 Histograms
########################################
########################################
summary(as.numeric(as.character(caraw$NUM.SHIPPED)))
summary(as.numeric(as.character(cvi2011$NUM_SWINE)))

# V1 with baxplot underneath
##################################################
swine.hist =  hist(canum, plot=FALSE) 
#	tiff(paste(filename, ".tiff", sep=""), 
#		width=9, height=7, units="in", res=600)
par(mar=c(2,6,4,2), fig = c(0, 1, 0.25, 1))
canum <- as.numeric(as.character(caraw$NUM.SHIPPED))
canum <- na.exclude(canum)
max.x = max(swine.hist$breaks)
max.y = max(swine.hist$counts)
barplot(swine.hist$counts, width = 1, space = 0, main = NULL,
			xlab="", ylim=c(0,max.y+10), ylab="", cex.lab = 1.5, 
  			cex.axis = 1.4, bty = "n", col = "darkseagreen", las = 1)
  			mtext("Number of shipments", side = 2, line = 4.4, cex = 1.6) 
			mtext("Number of swine", side = 1, line = 3, cex = 1.6) 
  			axis(side = 1, at = seq(1, (length(swine.hist$breaks)-1)), 
  			labels = swine.hist$breaks[-1], cex = 1.5)
legend("topright", fill = c("darkseagreen", "darkslateblue"), 
	legend = c("commuter agreement", "ICVI"), bty = "n")
par(fig = c(0, 1, 0, 0.2), mar = c(0, 6, 0, 2), new = TRUE)
boxplot(list(cvi2011$NUM_SWINE, canum), cex = 0.4, pch = 19, 
	col = c("darkslateblue", "darkseagreen"),
	horizontal = TRUE, ylim = c(0, 6500), axes = FALSE)


canum <- cvi2011$NUM_SWINE
swine.hist =  hist(canum, plot=FALSE) 
barplot(swine.hist$counts, width = 1, space = 0,  
			xlab="", ylim=c(0,max.y+10), ylab="", cex.lab = 1.5, 
  			cex.axis = 1.4, bty = "n", col = "darkseagreen", las = 1, 
  			main = "ICVI")
  			mtext("Number of shipments", side = 2, line = 4.4, cex = 1.6) 
			mtext("Number of swine", side = 1, line = 3, cex = 1.6) 
  			axis(side = 1, at = seq(1, (length(swine.hist$breaks)-1)), 
  			labels = swine.hist$breaks[-1], cex = 1.5)


# Trial 2: 
##################################################
dat <- data.frame(num = c(canum, cvi2011$NUM_SWINE), 
	dataset = c( rep("commuter agreement", length(canum)), rep("ICVI", length(cvi2011$NUM_SWINE)) ))
p <- ggplot(dat, aes(x = num)) + 
	geom_histogram(data = subset(dat, dataset == "commuter agreement"), 
		fill = "red", colour = "dimgray", alpha = 0.7) + 
	geom_histogram(data = subset(dat, dataset == "ICVI"), 
		fill = "blue", colour = "dimgray", alpha = 0.7) + 
	xlab("Shipment Size (number of head)") + 
	ylab("Frequency") + 
	theme_bw() +
	theme(panel.border = element_blank(), 
		axis.title.x = element_text(size=16, vjust=-0.15),
        axis.title.y = element_text(size=16, vjust= 0.8),
        axis.text.x = element_text(size=14, vjust=-0.05),
        axis.text.y = element_text(size=14)) +  
        #panel.grid.major = element_line(colour = "gray")) +
        theme(axis.line.x = element_line(colour= "black"),
  			axis.line.y = element_line(colour= "black"),   
			#legend information
        		legend.position=c(0.85, 0.2),  
        		legend.background= element_rect(fill="white", colour="white"),
        		legend.key= element_blank(),
        		legend.title= element_blank(),
        		legend.text = element_text(size=14))

p2 <- ggplot(dat, aes(y = num, x = dataset, fill = dataset)) +
	geom_boxplot(fill = c("red", "blue"), colour = "black") + 
	xlab("Dataset") + 
	ylab("Shipment Size") + 
	theme_bw() +
	theme(panel.border = element_blank(), 
		axis.title.x = element_text(size=16, vjust=-0.15),
        axis.title.y = element_text(size=16, vjust= 0.8),
        axis.text.x = element_text(size=14, vjust=-0.05),
        axis.text.y = element_text(size=14)) +  
        #panel.grid.major = element_line(colour = "gray")) +
        theme(axis.line.x = element_line(colour= "black"),
  			axis.line.y = element_line(colour= "black"),   
			#legend information
        		legend.position=c(0.85, 0.2),  
        		legend.background= element_rect(fill="white", colour="white"),
        		legend.key= element_blank(),
        		legend.title= element_blank(),
        		legend.text = element_text(size=14))
        		
multiplot(p, p2, cols = 2)