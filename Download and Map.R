install.packages("maps")
install.packages("mapdata")
install.packages("ISOcodes")

library(maps)       # Provides functions that let us plot the maps
library(mapdata)    # Contains the hi-resolution points that mark out the countries.
library(ISOcodes)
library(RCurl)
library(stringr)
library(mapproj)
##USA Data
x <- getURL("https://raw.githubusercontent.com/cdcepi/zika/master/United_States/data/US_Zika-2016-03-16.csv")
#https://raw.githubusercontent.com/cmrivers/zika/master/paho_epicurves.csv

USA <- read.csv(text = x)



dev.off()
map('worldHires', xlim=c(-200,-20),ylim=c(-30,50))
colors = c("red", "orange", "yellow", "green", "blue", "purple")
USAimported <- subset(USA, data_field_code =="US0001")
USAimported$State  <- str_sub(as.character(USAimported$location), start=15)
USAimported$State <- gsub("_"," ",USAimported$State)
#USAimported$State <- strsplit(USAimported$location, "[-]")
USAimported$colorBuckets <- as.numeric(cut(USAimported$value, c(0, 10, 100, 1000, 10000, 100000)))
leg.txt <- c("1", "10", "100", "1,000", "10,000", "100,000+")

stateMapped <- match.map("state", USAimported$State)
colorsmatched <- USAimported$colorBuckets [stateMapped]
# draw map
dev.off()
map("state", col = "grey", fill = TRUE, lty = 1, lwd = 0.2,projection="polyconic")


map("state", col = colors[colorsmatched], fill = TRUE, add=TRUE, resolution = 0,
    lty = 0, projection = "polyconic")

#map.text(wm,regions=".",label=USAimported$State,fill=TRUE)
#map("worldHires","Guam", col="red", fill=TRUE, add=TRUE) 
title("Imported Zika in the USA, 2016")

legend("bottomleft",leg.txt, fill = colors)


###### homegrown
dev.off()
map('worldHires', xlim=c(-200,-20),ylim=c(-30,50))
colors = c("red", "orange", "yellow", "green", "blue", "purple")
USAhomegrown <- subset(USA, data_field_code =="US0002")
USAhomegrown$State  <- str_sub(as.character(USAhomegrown$location), start=15)
USAhomegrown$State <- gsub("_"," ",USAhomegrown$State)
#USAhomegrown$State <- strsplit(USAhomegrown$location, "[-]")
USAhomegrown$colorBuckets <- as.numeric(cut(USAhomegrown$value, c(0, 10, 100, 1000, 10000, 100000)))
leg.txt <- c("1", "10", "100", "1,000", "10,000", "100,000+")

stateMapped <- match.map("state", USAhomegrown$State)
colorsmatched <- USAhomegrown$colorBuckets [stateMapped]
# draw map
dev.off()
map("state", col = "grey", fill = TRUE, lty = 1, lwd = 0.2,projection="polyconic")


map("state", col = colors[colorsmatched], fill = TRUE, add=TRUE, resolution = 0,
    lty = 0, projection = "polyconic")


#map("worldHires","Guam", col="red", fill=TRUE, add=TRUE) 
title("homegrown Zika in the USA, 2016")

legend("right", leg.txt, fill = colors)

####Western
dev.off()

Zika <- getURL("https://raw.githubusercontent.com/cmrivers/zika/master/paho_epicurves.csv")
Zika <- read.csv(text = Zika)


head(Zika)
aggdata <-aggregate(total_cases ~ country, data=Zika, FUN=sum)
#aggdata
#cbind 
colors = c("red", "orange", "yellow", "green", "blue", "purple")
USAimported <- subset(USA, data_field_code =="US0001")
total_cases<-sum(USAimported$value)
country <- "USA"
aggdata <-rbind(aggdata,data.frame(country,total_cases))
#rbind(aggdata,US)
#wm<-map('worldHires', aggdata$country, xlim=c(-200,-20),ylim=c(-30,50))


aggdata$colorBuckets <- as.numeric(cut(aggdata$total_cases, c(0, 10, 100, 1000, 10000, 100000)))
leg.txt <- c("1", "10", "100", "1,000", "10,000", "100,000+")
#stateMapped <- match.map("state", USAimported$State)
#colorsmatched <- USAimported$colorBuckets [stateMapped]
countryMapped <- sov.expand(aggdata$country, regex=TRUE)#match.map(aggdata$country)
colorsmatched <- aggdata$colorBuckets [countryMapped]
# draw map
dev.off()
wm<-map(wm, col = "grey", fill = TRUE, lty = 1, lwd = 0.2,projection="polyconic")
map(wm, col = aggdata$colorBuckets, fill = TRUE, add=TRUE, resolution = 0,
    lty = 0, projection = "polyconic")

