audubon.data <- read.csv("/users/christinabi/desktop/audubon_data.csv", header=TRUE, sep=",", na.strings="NA")
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
audubon.data.final <- subset(audubon.data, Survey_Type=="transect")
audubon.data.final <- subset(audubon.data.final, Date>= "2010-01-01")
audubon.data.final <- audubon.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]

gw.data <- read.csv("/users/christinabi/desktop/gw_data_mac.csv", header=TRUE, sep=",", na.strings="NA")
gw.data$Date <- as.Date(gw.data$Date, format="%d-%b-%y")
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
gw.data.final <- subset(gw.data, Date>= "2010-01-01")
gw.data.final <- subset(gw.data.final, Survey_Type=="transect")
gw.data.final <- gw.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]

natgeo.data <- read.csv("/users/christinabi/desktop/nat_geo_data.csv", header=TRUE, sep=",", na.strings="NA")
natgeo.data$Survey_Type[grep("trans", natgeo.data$Survey_Type, ignore.case=TRUE)] <- "transect"
natgeo.data$Date <- as.Date(natgeo.data$Date, format="%Y-%m-%d")
natgeo.data.final <- subset(natgeo.data, Date>= "2010-01-01")
natgeo.data.final <- subset(natgeo.data.final, Survey_Type=="transect")
natgeo.data.final <- natgeo.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]

clean_data <-rbind(audubon.data.final, gw.data.final, natgeo.data.final)
row.names(clean_data) <- 1:nrow(clean_data)

write.csv(clean_data, file = "my_clean_data.csv", row.names = FALSE)