#'Bird Sightings Data Cleaning and Plotting
#'cleans data from three different bird sighting files, combines them, plots them on map of DC
#'@param audubon_data.csv bird sighting data set 1
#'@param gw_data_mac.csv bird sighting data set 2
#'@param nat_geo_data.csv bird sighting data set 3
#'@keywords cleaning, plot, bird
#'@export
#'@examples

#####removed pathways to access data files#####

######audubon data cleaning######
audubon.data <- read.csv("audubon_data.csv", header=TRUE, sep=",", na.strings="NA")
audubon.data[audubon.data=="N/A"] <- NA
audubon.data[audubon.data==""] <- NA
audubon.data$Date <- as.Date(audubon.data$Date, format="%m/%d/%y")
audubon.data$Latitude <- substr(audubon.data$Latitude, 2, nchar(audubon.data$Latitude))
audubon.data$Longitude <- substr(audubon.data$Longitude, 2, nchar(audubon.data$Longitude))
audubon.data$Longitude<-as.numeric(audubon.data$Longitude)
audubon.data$Latitude<-as.numeric(audubon.data$Latitude)
audubon.data$Longitude <- (audubon.data$Longitude*(-1))
audubon.data$Survey_Type[grep("non", audubon.data$Survey_Type, ignore.case=TRUE)] <- "NA"
audubon.data$Survey_Type[grep("trans", audubon.data$Survey_Type, ignore.case=TRUE)] <- "transect"

#####audubon data filtering#####
audubon.data.final <- subset(audubon.data, Survey_Type=="transect")
audubon.data.final <- subset(audubon.data.final, Date>= "2010-01-01")
audubon.data.final <- audubon.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]

#####gw data cleaning#####
#####removed nontransect entries#####
gw.data <- read.csv("gw_data_mac.csv", header=TRUE, sep=",", na.strings="NA")
gw.data$Date <- as.Date(gw.data$Date, format="%d-%b-%y")
gw.data$Survey_Type[grep("non", gw.data$Survey_Type, ignore.case=TRUE)] <- "NA"
gw.data$Survey_Type[grep("trans", gw.data$Survey_Type, ignore.case=TRUE)] <- "transect"
list1 <- strsplit(gw.data$Longitude, "°")
longitude <- data.frame(degrees=sapply(list1, "[[", 1),
                        minutes=sapply(list1, "[[", 2))
longitude$minutes <- substr(longitude$minutes, 1, nchar(longitude$minutes)-2)
longitude$minutes<-as.numeric(longitude$minutes)
longitude$minutes <- (longitude$minutes/60)
longitude$degrees<-as.numeric(longitude$degrees)
gw.data$Longitude <- ((longitude$degrees + longitude$minutes)*(-1))
list2 <- strsplit(gw.data$Latitude, "°")
latitude <- data.frame(degrees=sapply(list2, "[[", 1),
                       minutes=sapply(list2, "[[", 2))
latitude$minutes <- substr(latitude$minutes, 1, nchar(latitude$minutes)-2)
latitude$minutes<-as.numeric(latitude$minutes)
latitude$minutes <- (latitude$minutes/60)
latitude$degrees<-as.numeric(latitude$degrees)
gw.data$Latitude <- (latitude$degrees + latitude$minutes)

#####gw data filtering#####
gw.data.final <- subset(gw.data, Date>= "2010-01-01")
gw.data.final <- subset(gw.data.final, Survey_Type=="transect")
gw.data.final <- gw.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]

#####nat geo data cleaning#####
#####removed nontransect entries#####
natgeo.data <- read.csv("nat_geo_data.csv", header=TRUE, sep=",", na.strings="NA")
natgeo.data$Survey_Type[grep("non", natgeo.data$Survey_Type, ignore.case=TRUE)] <- "NA"
natgeo.data$Survey_Type[grep("trans", natgeo.data$Survey_Type, ignore.case=TRUE)] <- "transect"
#####removed data points not geographically in DC#####
natgeo.data.noantarctica <- subset(natgeo.data, Longitude<=0)
natgeo.data.noantarctica$Date <- as.Date(natgeo.data.noantarctica$Date, format="%Y-%m-%d")

#####nat geo data filtering#####
natgeo.data.final <- subset(natgeo.data.noantarctica, Date>= "2010-01-01")
natgeo.data.final <- subset(natgeo.data.final, Survey_Type=="transect")
natgeo.data.final <- natgeo.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]

#####combining data#####
clean_data <-rbind(audubon.data.final, gw.data.final, natgeo.data.final)
row.names(clean_data) <- 1:nrow(clean_data)

#####writing clean data file#####
write.csv(clean_data, file = "my_clean_data.csv", row.names = FALSE)

#####plotting data#####
library(sp)
library(rgdal)
clean_data <- read.csv("my_clean_data.csv")
plotting_data <- SpatialPoints(clean_data[, c("Longitude", "Latitude")])
dc <- readOGR("Neighborhood_Clusters-shp", "Neighborhood_Clusters")
par(mar = c(1, 1, 1, 1))
plot(
  dc,
  col = "darkgrey",
  border = "white",
  main = "District of Columbia Bird Sightings"
)
plot(dc[46, ],
     add = TRUE,
     col = "#718BAE80",
     border = "white")
plot(plotting_data,
     add = TRUE,
     pch = 16,
     cex = 0.25)