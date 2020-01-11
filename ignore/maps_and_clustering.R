
# required libraries ------------------------------------------------------

library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library("ggmap")
library(broom)

#API key: AIzaSyA3nim7i8YYK3_Bkz9oqFW1Vcg0rXDG7lY
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)


# data --------------------------------------------------------------------


threeoneone_2 = read_csv("https://datagate.dc.gov/search/open/311requests?daterange=8years&details=true&format=csv")

dim(threeoneone_2)
head(threeoneone_2)
attach(threeoneone_2)

threeoneone_2long <- threeoneone_2$LONGITUDE
threeoneone_2lat <- threeoneone_2$LATITUDE

crashdata <- read.csv("https://opendata.arcgis.com/datasets/70392a096a8e431381f1f692aaa06afd_24.csv")

dim(crashdata)
head(crashdata)
attach(crashdata)

crashdatalong <- crashdata$LONGITUDE
crashdatalat <- crashdata$LATITUDE


# custom colors -----------------------------------------------------------


col1 = "#011f4b"

col2 = "#6497b1"

col3 = "#b3cde0"

col4 = "#CC0000"


# data alterations --------------------------------------------------------


threeoneone_2$ymd <- ymd_hms(ADDDATE)

threeoneone_2$ymd 

threeoneone_2[complete.cases(threeoneone_2), ]

crashdata_clean_date <- gsub("T.*", "", crashdata$FROMDATE)

crashdata$crashdate <- ymd(crashdata_clean_date)

crashdata[complete.cases(crashdata), ]



# create DC map with Google API -------------------------------------------


ggmap::register_google(key = "AIzaSyA3nim7i8YYK3_Bkz9oqFW1Vcg0rXDG7lY")


dcmap <- ggmap(get_map(location = c(lon = -77.02106, lat = 38.905), zoom = 12, scale = 1, maptype = 'terrain', color = 'color'))

dcmap

# 311 requests in DC ------------------------------------------------------

dcmap_311 <- dcmap + geom_point(data = threeoneone_2, aes(x = threeoneone_2long, y = threeoneone_2lat), color = col1, alpha = 0.05, size = 0.05) + theme(legend.position = "none") 

dcmap_311

#color = WARD ??? + theme(legend.position = "bottom") ???


# crashes in DC -----------------------------------------------------------

dcmap_crashes <- dcmap + geom_point(data = crashdata, aes(x = crashdatalong, y = crashdatalat), color = col4, alpha = 0.05, size = 0.05) + theme(legend.position = "none")

dcmap_crashes

# 311 requests + crashes --------------------------------------------------

# all of DC

dcmap_311_crashes <- dcmap + geom_point(data = threeoneone_2, aes(x = threeoneone_2long, y = threeoneone_2lat), color = col1, alpha = 0.05, size = 0.05) + geom_point(data = crashdata, aes(x = crashdatalong, y = crashdatalat), color = col4, alpha = 0.05, size = 0.05) + theme(legend.position = "none") 

dcmap_311_crashes

# zoom in on downtown

dcmap_dt <- ggmap(get_map(location = c(lon = -77.02906, lat = 38.905), zoom = 14, scale = 2, maptype = 'terrain', color = 'color'))

dcmap_dt

dcmap_dt_311_crashes <- dcmap_dt + geom_point(data = threeoneone_2, aes(x = threeoneone_2long, y = threeoneone_2lat), color = col1, alpha = 0.05, size = 1) + geom_point(data = crashdata, aes(x = crashdatalong, y = crashdatalat), color = col4, alpha = 0.05, size = 1) + theme(legend.position = "none")

dcmap_dt_311_crashes

# zoom in on nw -----------------------------------------------------------

dcmap_nw <- ggmap(get_map(location = c(lon = -77.07, lat = 38.94), zoom = 14, scale = 2, maptype = 'terrain', color = 'color'))

dcmap_nw

dcmap_nw_311_crashes <- dcmap_nw + geom_point(data = threeoneone_2, aes(x = threeoneone_2long, y = threeoneone_2lat), color = col1, alpha = 0.05, size = 1) + geom_point(data = crashdata, aes(x = crashdatalong, y = crashdatalat), color = col4, alpha = 0.05, size = 1) + theme(legend.position = "none")

dcmap_nw_311_crashes

# zoom in on ne -----------------------------------------------------------

dcmap_ne <- ggmap(get_map(location = c(lon = -76.995, lat = 38.925), zoom = 14, scale = 2, maptype = 'terrain', color = 'color'))

dcmap_ne

dcmap_ne_311_crashes <- dcmap_ne + geom_point(data = threeoneone_2, aes(x = threeoneone_2long, y = threeoneone_2lat), color = col1, alpha = 0.05, size = 1) + geom_point(data = crashdata, aes(x = crashdatalong, y = crashdatalat), color = col4, alpha = 0.05, size = 1) + theme(legend.position = "none")

dcmap_ne_311_crashes

# zoom in on sw/se -----------------------------------------------------------

dcmap_swse <- ggmap(get_map(location = c(lon = -76.993, lat = 38.867), zoom = 14, scale = 2, maptype = 'terrain', color = 'color'))

dcmap_swse

dcmap_swse_311_crashes <- dcmap_swse + geom_point(data = threeoneone_2, aes(x = threeoneone_2long, y = threeoneone_2lat), color = col1, alpha = 0.05, size = 1) + geom_point(data = crashdata, aes(x = crashdatalong, y = crashdatalat), color = col4, alpha = 0.05, size = 1) + theme(legend.position = "none")

dcmap_swse_311_crashes


# crash data clustering  -------------------------------------------------------


library(broom)

# library(sp)
# library(rgdal)
# library(geosphere)
#vcrashxy <- SpatialPointsDataFrame(matrix(c(crashx,crashy), ncol = 2), data.frame(ID = seq(1:length(crashx))), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

#crashm <- distm(crashxy, fun = distGeo)

#hc <- hclust(as.dist(crashmd), method = "complete")

# #dbscan
# 
# library(dbscan)
# 
# dbscan(crashxy, eps = .03, minPts = 5)

crashdata <- crashdata %>% filter( X > -77.11, X < -76.89)
crashdata <- crashdata %>% filter( Y < 39, Y > 38.7)

crashx <- crashdata$X
crashy <- crashdata$Y 

crashxy <- matrix(c(crashx,crashy), ncol = 2)


#cluster

crash_clusterinfo_8 <- kmeans(crashxy, 8)

crash_clusterinfo_15 <- kmeans(crashxy, 15)

#quickly plot cluster

plot(crashxy, col = crash_clusterinfo_8$cluster)

#add cluster data to coordinate

crashxy_8 <- augment(crash_clusterinfo_8, crashxy)
crashxy_15 <- augment(crash_clusterinfo_15, crashxy)


dcmap_crash_kcluster_8 <- dcmap + geom_point(data = crashxy_8, aes(x = crashxy_8$X1, y = crashxy_8$X2, 
                                                                   color = crashxy_8$.cluster, alpha = 0.01, size = 0.001)) + theme(legend.position = "none")

dcmap_crash_kcluster_8

dcmap_crash_kcluster_15 <- dcmap + geom_point(data = crashxy_15, aes(x = crashxy_15$X1, y = crashxy_15$X2, 
                                                                     color = crashxy_15$.cluster, alpha = 0.01, size = 0.001)) + theme(legend.position = "none")

dcmap_crash_kcluster_15


# threeoneone cluster -----------------------------------------------------

threeoneonex <- threeoneone_2$LATITUDE
threeoneoney <- threeoneone_2$LONGITUDE

threeoneonexy <- matrix(c(threeoneonex,threeoneoney), ncol = 2)

#cluster

threeoneone_cluster_8 <- kmeans(threeoneonexy, 8)
threeoneone_cluster_15 <- kmeans(threeoneonexy, 15)

#quick plot

plot(threeoneonexy, col = threeoneone_cluster_8$cluster)

#add cluster data to coordinates

threeoneonexy_8 <- augment(threeoneone_cluster_8, threeoneonexy)

threeoneonexy_8

threeoneonexy_15 <- augment(threeoneone_cluster_15, threeoneonexy)

dcmap_threeoneone_kcluster_8 <- dcmap + geom_point(data = threeoneonexy_8, aes(x = threeoneonexy_8$X1, y = threeoneonexy_8$X2, color = threeoneonexy_8$.cluster, alpha = 0.01, size = 0.001)) + theme(legend.position = "none")

dcmap_threeoneone_kcluster_8

dcmap_threeoneone_kcluster_15 <- dcmap + geom_point(data = threeoneonexy_15, aes(x = threeoneonexy_15$X1, y = threeoneonexy_15$X2, color = threeoneonexy_15$.cluster, alpha = 0.01, size = 0.001)) + theme(legend.position = "none")

dcmap_threeoneone_kcluster_15

