########################################
########################################
# Swine Presentation
########################################
########################################
library(maps)
library(RColorBrewer)
setwd("~/Google Drive/Warwick/students/Callum/Swine")


get_useful_parts = function(dataframe, county = TRUE){
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
 
    # Get rid of commas in the Value col, standardize, put in new col "valsd"
    dataframe$Value <- as.character(dataframe$Value)
    dataframe$Value[dataframe$Value == " (D)"] <- -200  # assign censored -200!
    dataframe$Value2 <- as.numeric(gsub(",", "", dataframe$Value))
    dataframe$FIPS <- NA
    dataframe$Year <- dataframe$Year
       
    if (county) {
        # Clean dataframe by removing NAs
        dataframe <- dataframe[dataframe$Geo.Level=="COUNTY",]
        dataframe <- dataframe[!is.na(dataframe$County.ANSI),]
    
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

# Map of NASS data 2012
########################################
ctname <- map('county', resolution=0, plot=FALSE)$names
ctname <- as.matrix(ctname)
data(county.fips)
name <- data.frame(ctname = ctname, head2007 = NA, head2012 = NA, 
                   head2017 = NA, op2007 = NA, op2012 = NA, op2017 = NA)
op2012 <- get_useful_parts(
    read.csv("NASS/2012_NASS_HOGS_Operations_with_inventory.csv"))
op2012$ctname <- county.fips$polyname[match(op2012$FIPS, county.fips$fips)]
name$head2012 <- head2012$value[match(name$ctname, head2012$ctname)]

head2012 <- get_useful_parts(read.csv("NASS/*2012_NASS_HOGS_Inventory.csv"))
head2012$ctname <- county.fips$polyname[match(head2012$FIPS, county.fips$fips)]
name$op2012 <- op2012$value[match(name$ctname, op2012$ctname)]

# map
cols <- colorRampPalette(brewer.pal(9, "YlOrRd"))(143)   # colors for level plots
colmatch <- data.frame(num=seq(1,143,1), col=cols)
name$col<-as.character(colmatch$col[match(name$op2012, colmatch$num)])
name$col[is.na(name$col)]<-"#FFFFFF"

tiff('NASS/op2012.tiff',res=400, 
     height=90, width=140, units="mm",compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$col, boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
dev.off()


# 2007 - 2017
head2007 <- get_useful_parts(
    read.csv("NASS/2007_NASS_HOGS_Inventory.csv"))
head2017 <- get_useful_parts(
    read.csv("NASS/2017_NASS_HOGS_Inventory.csv"))
head2007$ctname <- county.fips$polyname[match(head2007$FIPS, county.fips$fips)]
head2017$ctname <- county.fips$polyname[match(head2017$FIPS, county.fips$fips)]
name$head2007 <- head2007$value[match(name$ctname, head2007$ctname)]
name$head2017 <- head2017$value[match(name$ctname, head2017$ctname)]
name$head2007nona <- name$head2007; name$head2017nona <- name$head2017
name$head2007nona[is.na(name$head2007)] <- 0
name$head2017nona[is.na(name$head2017)] <- 0
name$diff <- name$head2017nona - name$head2007nona
name$difflog <- round(log(abs(name$diff)+1)*((name$diff+0.1)/abs(name$diff+0.1)), 1)

# map
cols <- colorRampPalette(brewer.pal(9, "RdYlBu"))(10)   # colors for level plots
for (i in 1:length(name$diff)){
    if ((name$diff[i])>=-338769.0 & (name$diff[i])<=-5514.6) {name$dcol[i]<-cols[1]}
    else if ((name$diff[i])>-5514.6 & (name$diff[i])<=-652.2) {name$dcol[i]<-cols[2]}
    else if ((name$diff[i])>-652.2 & (name$diff[i])<= -200.0) {name$dcol[i]<-cols[3]}
    else if ((name$diff[i])> -200.0 & (name$diff[i])<=-46.4) {name$dcol[i]<-cols[4]}
    else if ((name$diff[i])>-46.4 & (name$diff[i])<=0) {name$dcol[i]<-cols[5]}
    else if ((name$diff[i])>0 & (name$diff[i])<=19.0) {name$dcol[i]<-cols[6]}
    else if ((name$diff[i])>19.0 & (name$diff[i])<=119.8) {name$dcol[i]<-cols[7]}
    else if ((name$diff[i])>119.8 & (name$diff[i])<=265.0) {name$dcol[i]<-cols[8]}
    else if ((name$diff[i])>265.0 & (name$diff[i])<=1933.6) {name$dcol[i]<-cols[9]}
    else if ((name$diff[i])>1933.6 & (name$diff[i])<=738418) {name$dcol[i]<-cols[10]}
}
name$dcol[name$head2007 == -200] <- "lightgray"
name$dcol[name$head2017 == -200] <- "lightgray"

cols <- colorRampPalette(brewer.pal(9, "RdYlBu"))(11)   # colors for level plots
for (i in 1:length(name$diff)){
    if ((name$diff[i])>=-338769.0 & (name$diff[i])<=-600) {name$dcol2[i]<-cols[1]}
    else if ((name$diff[i])>-600 & (name$diff[i])<=-450) {name$dcol2[i]<-cols[2]}
    else if ((name$diff[i])>-450 & (name$diff[i])<= -300.0) {name$dcol2[i]<-cols[3]}
    else if ((name$diff[i])> -300.0 & (name$diff[i])<=-150) {name$dcol2[i]<-cols[4]}
    else if ((name$diff[i])>-150 & (name$diff[i])<=0) {name$dcol2[i]<-cols[5]}
    else if ((name$diff[i])>0 & (name$diff[i])<=150) {name$dcol2[i]<-cols[6]}
    else if ((name$diff[i])>150 & (name$diff[i])<=300) {name$dcol2[i]<-cols[7]}
    else if ((name$diff[i])>300 & (name$diff[i])<=450) {name$dcol2[i]<-cols[8]}
    else if ((name$diff[i])>450 & (name$diff[i])<=600) {name$dcol2[i]<-cols[9]}
    else if ((name$diff[i])>600 & (name$diff[i])<=750) {name$dcol2[i]<-cols[10]}
    else if ((name$diff[i])>750 & (name$diff[i])<=738418) {name$dcol2[i]<-cols[11]}
}
name$dcol2[name$head2007 == -200] <- "lightgray"
name$dcol2[name$head2017 == -200] <- "lightgray"

# Try one - on log scale
# cols <- colorRampPalette(brewer.pal(9, "RdYlBu"))(271)
# colmatch <- data.frame(num=seq(13.5, -13.5, -0.1), col=cols)
# colmatch$num <- round(colmatch$num, 1)
# name$dcol<-as.character(colmatch$col[match(name$difflog, colmatch$num)])
# name$dcol[name$head2007 == -200] <- "lightgray"
# name$dcol[name$head2017 == -200] <- "lightgray"
# NEED TO FIX NAs!!!!!!!!!!!!!!

# map
tiff('NASS/headdiff.tiff',res=400, 
     height=90, width=140, units="mm", compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$dcol, 
    boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
dev.off()

# map 2
tiff('NASS/headdiff2.tiff',res=400, 
     height=90, width=140, units="mm", compression="lzw")
par(mai=c(1,1,1,3))
map('county', resolution=0, lwd=0.3, col="dark gray")
map('county', resolution=0, fill=TRUE, col=name$dcol2, 
    boundary="light gray", lwd=0.3, add=TRUE)
map('state', add=TRUE, resolution=0, lwd=0.5)
dev.off()

colorlegend(col=cols[2:6], zval=c(0, 2.8, 7.4, 20.1, 54.6, 148.4), zlim=c(1, 148), log=TRUE, posx=c(0.8, 0.83), posy=c(0.22, 0.6), digit=0, cex=0.2)
dev.off()


# FIPS TO WATCH
# Var1 Freq
# 12091    2
# 22099    2
# 30067    2
# 37053    3
# 48167    2
# 51001    2
# 53053    2
# 53055    3

# Trend over time
########################################
df <- get_useful_parts(
    read.csv("NASS/1997-2019 - head of hog.csv"), FALSE)
df$head <- df$value/1000000
library(ggplot2)
ggplot(data = df, aes(x = year, y = head)) + geom_point() + geom_line() + 
    theme_classic() + xlab("") + ylab("Head of hog (in millions)") + 
    xlim(1995, 2021) + 
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 16), 
          panel.grid.major = element_line())

ggplot(data = df, aes(x = year, y = value)) + geom_bar()
