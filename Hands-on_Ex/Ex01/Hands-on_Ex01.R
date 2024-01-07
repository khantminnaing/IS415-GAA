pacman::p_load(sf, tidyverse)
setwd("~/Desktop/IS415/GeocomputingR/Hands-on_Ex01")

#Importing polygon feature data in shapefile format
mpsz = st_read(dsn = "data/geospatial", 
               layer = "MP14_SUBZONE_WEB_PL")
cyclingpath = st_read(dsn = "data/geospatial", 
                      layer = "CyclingPathGazette")
preschool = st_read("data/geospatial/PreSchoolsLocation.kml")

#Checking the Content of A Simple Feature Data Frame
##retrieve the geometry list-column
st_geometry(mpsz)
##retrieve associated attribute information in the data frame
glimpse(mpsz)
##reveal complete information
head(mpsz, n=5)  

#Plotting the Geospatial Data
plot(mpsz)
plot(st_geometry(mpsz))
plot(mpsz["PLN_AREA_N"])

#Working with Projection
st_crs(mpsz)
mpsz3414 <- st_set_crs(mpsz, 3414)
st_crs(mpsz3414)

##Transforming the projection of preschool from wgs84 to svy21
preschool3414 <- st_transform(preschool, 
                              crs = 3414)

# Importing and Converting An Aspatial Data
listings <- read_csv("data/aspatial/listings.csv")
list(listings) 
##Creating a simple feature data frame from an aspatial data frame
listings_sf <- st_as_sf(listings, 
                        coords = c("longitude", "latitude"),
                        crs=4326) %>%
  st_transform(crs = 3414)
glimpse(listings_sf)

#Geoprocessing with sf package

##Buffering
buffer_cycling <- st_buffer(cyclingpath, 
                            dist=5, nQuadSegs = 30)
buffer_cycling$AREA <- st_area(buffer_cycling)
sum(buffer_cycling$AREA)

#Point-in-polygon count
##Point Count
mpsz3414$`PreSch Count`<- lengths(st_intersects(mpsz3414, preschool3414))
summary(mpsz3414$`PreSch Count`)
top_n(mpsz3414, 1, `PreSch Count`)
##Area
mpsz3414$Area <- mpsz3414 %>%
  st_area()
##Density
mpsz3414 <- mpsz3414 %>%
  mutate(`PreSch Density` = `PreSch Count`/Area * 1000000)

#Exploratory Data Analysis
hist(mpsz3414$`PreSch Density`)

#Histogram
ggplot(data=mpsz3414, 
       aes(x= as.numeric(`PreSch Density`)))+
  geom_histogram(bins=20, 
                 color="black", 
                 fill="grey") +
  labs(title = "Are pre-school even distributed in Singapore?",
       subtitle= "There are many planning sub-zones with a single pre-school, on the other hand, \nthere are two planning sub-zones with at least 20 pre-schools",
       x = "Pre-school density (per km sq)",
       y = "Frequency")

#Scatterplot
ggplot(data=mpsz3414, 
       aes(y = `PreSch Count`, 
           x= as.numeric(`PreSch Density`)))+
  geom_point(color="black", 
             fill="light blue") +
  xlim(0, 40) +
  ylim(0, 40) +
  labs(title = "",
       x = "Pre-school density (per km sq)",
       y = "Pre-school count")

