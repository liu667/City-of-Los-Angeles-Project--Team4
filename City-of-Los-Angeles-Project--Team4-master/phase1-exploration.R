library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggmap)
library(zipcode)
library(lubridate)
library(readxl)

data_311 = read.csv("la_city_independent_analysis/data/311_calls_w_CTs20171102134828.csv")
data_crime = read.csv("la_city_independent_analysis/data/crime_w_CTs20171102134814.csv")
data_shelter = read.csv("la_city_independent_analysis/data/shelters_w_CTs20171102134808.csv")
data_census17_tract = read_excel("la_city_independent_analysis/data/homeless-count-2017-results-by-census-tract.xlsx",
                           sheet = "Count_by_Tract")
data_census17_community = read_excel("la_city_independent_analysis/data/homeless-count-2017-results-by-census-tract.xlsx",
                                     sheet = "Count_by_Community")

## 1. shelter vs. crime map
lamap = get_map("Los Angeles")
colnames(data_shelter)[c(1,2)] <- c("lon","lat")


ggmap(lamap)+
  geom_point(data = data_shelter,
             aes(x = lon, y = lat,color = ZIP))

## 2. ziplevel homeless number
data(zipcode)

LA_zipcode = zipcode %>%
  filter(city == "Los Angeles") 

homeless_zip = data_311 %>%
  filter(ZIPCODE != "NA" & ZIPCODE != 0) %>%
  group_by(ZIPCODE) %>%
  summarise(number = n())
  
homeless_zip$ZIPCODE = as.character(homeless_zip$ZIPCODE)

homeless_zipmap = left_join(homeless_zip, zipcode,
                            by = c("ZIPCODE" = "zip"))    
#plot it in map
ggmap(lamap)+
 geom_point(data = homeless_zipmap,
       aes(x = longitude, y =latitude, color = number))+
  scale_color_gradient(low = "orange", high = "black")

## 3. try to find relationship between shelter and homeless
data_shelter_tract = data_shelter %>%
  group_by(CT10) %>%
  summarise(number = n())

data_census17_shelter = data_census17_tract %>%
  right_join(data_shelter_tract,
            by = c("tract" = "CT10")) %>%
  arrange(desc(totSheltPeople))

tract_shelter_top15 = head(data_census17_shelter, 15)[c(1,72,73,74)]
shelter_top15_map = left_join(tract_shelter_top15, data_shelter,
                              by = c("tract" = "CT10"))
ggmap(lamap)+
  geom_point(data = shelter_top15_map,
               aes(x = lon, y = lat,
                   color = number,
                   size = totSheltPeople))+
  scale_color_gradient(low = "red", high = "black")
  

## 4. Get the unsheltered people location

#get the location of unshelter people from 311 and tract sheets

unshelter = data_census17_tract %>%
  left_join(data_311,
            by= c("tract" = "CT10"))

ggmap(lamap) +
  geom_polygon(data = unshelter,
               aes(x = LONGITUDE, y = LATITUDE,
                   group = ZIPCODE,
                   fill = totUnsheltPeople))+
  scale_fill_gradient(low = "orange", high = "black")

#top10/15 unshelter point on map
unshelter_top15 = data_census17_tract %>%
  arrange(-totUnsheltPeople) %>%
  slice(1:15) %>%
  left_join(data_311,
            by = c("tract" = "CT10"))

ggmap(lamap)+
  geom_point(data = unshelter_top15,
          aes(x = LONGITUDE, y = LATITUDE,
              group = tract,
              color = totUnsheltPeople))+
  scale_color_gradient(low = "red", high = "black")


##to zipcode level
##have no idea how to do that

##community level
ggmap(lamap) +
  geom_polygon(data = unshelter,
               aes(x = LONGITUDE, y = LATITUDE,
                   group = Community_Name,
                   fill = totUnsheltPeople))+
  scale_fill_gradient(low = "orange", high = "black")

community_unshelter_top10 = data_census17_tract %>%
  group_by(Community_Name) %>%
  summarise(unshelter_commu = sum(totUnsheltPeople)) %>%
  arrange(desc(unshelter_commu)) 
community_unshelter_top10 = head(community_unshelter_top10,10)

ggplot(community_unshelter_top10, aes(x = reorder(Community_Name,-unshelter_commu), 
                                      y = unshelter_commu))+
  geom_bar(stat = "identity")+
  labs(x = "Community Name", y = "Unshelter Total People Number", 
       title = "Which community has the highest unshelter homeless people?")


## 5. Crime

#Time occurred
data_crime$TIME.OCCURRED = 
  format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_crime$TIME.OCCURRED), 
                                  format="%H%M"), 12, 16), '%H:%M'), '%I:%M %p')

data_crime$TIME.OCCURRED = strptime(data_crime$TIME.OCCURRED, "%I:%M %p")

data_crime$TIME.OCCURRED = 
  str_sub(string = data_crime$TIME.OCCURRED, start = 12, end = 13)

data_crime %>%
  group_by(TIME.OCCURRED) %>%
  summarise(number_occured = n()) %>%
  ggplot(aes(x = TIME.OCCURRED, y = number_occured))+
  geom_bar(stat = "identity", fill = "light blue")+
  labs(y = "The Number of Crime", x = "Hour", title = "The Number of Crime Reported by Hour")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 18))+
  theme(axis.title = element_text(size = 14))+
  coord_fixed(ratio = 1/10)


##map 311 area
ggmap(lamap)+
  geom_polygon(data = data_311,aes(x = LONGITUDE,y = LATITUDE,
                   group = APC,
                   fill = APC))
  
  
ggmap(lamap)+
  geom_polygon(data = data_crime, aes(x = LONGITUDE, y = LATITUDE,
                                      group = AREA.NAME,
                                      fill = AREA.NAME))

###311 call

data_311 %>%
  group_by(CT10) %>%
  summarise(number = n())


####working on map
library(leaflet)
library(rgdal)
library(sp)
library(raster)
library(maptools)
library(geojsonio)
library(tmap)



###Tract level-total homeless
tract_map = readOGR("la_city_independent_analysis/data/raw_data/CENSUS_TRACTS_2010.zip_unzipped/CENSUS_TRACTS_2010.shp")
tract_map2 = spTransform(tract_map, CRS("+proj=longlat +datum=WGS84"))

###leaflet(tract_map2) %>%
  addTiles()%>%
  addPolygons()

##tract_map = data.frame(OBJECTID = tract_map2$OBJECTID, GEOID10 = tract_map2$GEOID10,
##                      CT10 = tract_map2$CT10)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
tract_map = fortify(tract_map2,region = "CT10")

tract_map$id = as.numeric(tract_map$id)

ladata = data_census17_tract %>%
  filter(City == "Los Angeles") %>%
  arrange(-totPeople) %>%
  slice(2:1004)

combine = tract_map %>%
  left_join(ladata,
            by = c("id"="tract")) %>%
  filter(City == "Los Angeles")
  


###tract_map1 = distinct(combine,long, lat, id, City, totUnsheltPeople, totSheltPeople, totPeople)
tract_map1 = distinct(combine, id,  totPeople)
tractid <- tract_map1$id

polyFunc<-function(groupname, dat){
  poly<-filter(dat, id==groupname) %>%
    dplyr::select(long, lat)
  return(Polygons(list(Polygon(poly)), groupname))
}

polygons<-lapply(tractid, function(x) polyFunc(x, dat=combine))

sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon,
                                     data=data.frame(row.names=tractid, tract_map1))
pal <- colorNumeric(
  palette = "Blues",
  domain = df.polygon$totPeople
)

leaflet()%>%
  addTiles() %>%
  addPolygons(data = df.polygon,
              fillColor = ~pal(totPeople),
              color = "#5297A8", 
              fillOpacity = 1, 
              weight = 1, 
              smoothFactor = 1)%>%
  addLegend(pal = pal, 
            values = df.polygon$totPeople, 
            position = "bottomright", 
            title = "Number of Homeless People")

##community level
community_map = readOGR("CnclDist_July2012/CnclDist_July2012.shp")
community_map1 = spTransform(community_map, CRS("+proj=longlat +datum=WGS84"))
community_map = fortify(community_map1, region = "DISTRICT")
communitymap = as.data.frame(community_map1)

total_commu = data_census17_tract %>%
  group_by(CD) %>%
  summarise(Number = sum(totPeople))
total_commu$CD = as.character(total_commu$CD)
commu_combine = left_join(community_map, total_commu,
                          by = c("id" = "CD"))

community1 = distinct(commu_combine, id, Number)
commuid = community1$id

commupolygons<-lapply(commuid, function(x) polyFunc(x, dat=commu_combine))

commusp.polygon<-SpatialPolygons(commupolygons)
commudf.polygon<-SpatialPolygonsDataFrame(commusp.polygon,
                                     data=data.frame(row.names=commuid, community1))
pal <- colorNumeric(
  palette = "Blues",
  domain = commudf.polygon$Number
)

leaflet()%>%
  addTiles() %>%
  addPolygons(data = commudf.polygon,
              fillColor = ~pal(Number),
              color = "#5297A8", 
              fillOpacity = 0.8, 
              weight = 1, 
              smoothFactor = 1)%>%
  addLegend(pal = pal, 
            values = commudf.polygon$Number, 
            position = "bottomright", 
            title = "Number of Homeless People by Distrit")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = community_map1)
  

###City level
city_map = geojson_read("City Boundaries for Los Angeles County.geojson",what = "sp")
city_map2 = spTransform(city_map, CRS("+proj=longlat +datum=WGS84"))

###another method different from below
citymap2 <-as.data.frame(city_map2)
citymap2 = distinct(citymap2, CITY_ID, CITY_NAME)
citymap2$CITY_NAME = as.character(citymap2$CITY_NAME)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
city_map3 = fortify(city_map2,region = "CITY_ID")

citycensus = data_census17_tract %>%
  group_by(City) %>%
  summarise(Number = sum(totPeople))
census_17 = left_join(citycensus, citymap2,
                      by = c("City" = "CITY_NAME"))

census_17$CITY_ID = as.character(census_17$CITY_ID)
combine = city_map3 %>%
  left_join(census_17,by = c("id" = "CITY_ID"))


###cd = spTransform(map, CRS("+proj=longlat +datum=WGS84"))

citydata = distinct(combine, id, Number)
cityid <- citydata$id

citypolygon<-lapply(cityid, function(x) polyFunc(x, dat=combine))

sp.polygon_city<-SpatialPolygons(citypolygon)
df.polygon_city<-SpatialPolygonsDataFrame(sp.polygon_city,
                                     data=data.frame(row.names=cityid, citydata))
pal <- colorNumeric(
  palette = "Blues",
  domain = df.polygon_city$Number
)

leaflet()%>%
  addTiles() %>%
  addPolygons(data = df.polygon_city,
              fillColor = ~pal(Number),
              color = "#55C3DC", 
              fillOpacity = 1, 
              weight = 1, 
              smoothFactor = 1)%>%
  addLegend(pal = pal, 
            values = df.polygon_city$Number, 
            position = "bottomright", 
            title = "Number of Homeless People")

###bar chart for shelter
data_shelter$CITY = str_replace_all(string = data_shelter$CITY, pattern = "Los Angeles ",
                replacement = "Los Angeles")

data_shelter %>%
  group_by(CITY) %>%
  summarise(number = n()) %>%
  arrange(-number) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(CITY,number), y = number))+
  geom_bar(stat = "identity",fill = "light blue")+
  labs(x = "Number", y = "City", title = "Shelter Numbers in Different Cities")+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 18))+
  theme(axis.title = element_text(size = 14))

unique(data_census17_tract[,3])

###11.14 updating

##homeless shelter shp file
shelter_map = readOGR("Homeless_Shelters_and_Services/Homeless_Shelters_and_Services.shp")

la_shelter = data_shelter %>%
  filter(CITY == "Los Angeles")
xy <- la_shelter[,c(1,2)]
sp_lashelter = SpatialPointsDataFrame(coords = xy, data = la_shelter,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 
  
###pal <- colorFactor(palette = "Blues", domain = la_shelter$ZIP)


leaflet(sp_lashelter) %>%
  addTiles() %>%
  addCircleMarkers(stroke = FALSE,  
                   fillOpacity = 0.8,
                   radius = 8,
                   fillColor = "#CD745B")
  
  
##data_311 2017
data_311 %>%
  group_by(REQUESTSOURCE) %>%
  summarise(Number = n()) %>%
  ggplot(aes(x = reorder(REQUESTSOURCE,Number),y = Number))+
  geom_bar(stat = "identity", fill = "light blue")+
  coord_flip()+
  labs(x = "Requestsoure",y = "Number", title = "311 Call Request Source Number")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 18))+
  theme(axis.title = element_text(size = 14))


data_311$CREATEDDATE = str_sub(data_311$CREATEDDATE, start = 1, end = 2)
data_311 %>%
  filter(CREATEDDATE != "11") %>%
  group_by(CREATEDDATE) %>%
  summarise(number = n()) %>%
  ggplot(aes(x = CREATEDDATE, y = number))+
  geom_bar(stat = "identity", fill = "lightblue")+
  labs(x = "Month",y = "Number", title = "Number of 311 calls by Month")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 18))+
  theme(axis.title = element_text(size = 14))

###mapping crime

crime_tract = data_crime %>%
  group_by(CT10) %>%
  summarise(Number = n())

total_crime = tract_map %>%
  left_join(crime_tract,
            by = c("id"="CT10"))

crime_map = distinct(total_crime, id,  Number)
crime_map = crime_map %>%
  filter(Number != "NA")
crimemapid <- crime_map$id

crimepolygons<-lapply(crimemapid, function(x) polyFunc(x, dat=total_crime))

sp.polygon_crime<-SpatialPolygons(crimepolygons)
df.polygon_crime<-SpatialPolygonsDataFrame(sp.polygon_crime,
                                     data=data.frame(row.names=crimemapid, crime_map))
pal <- colorNumeric(
  palette = "Blues",
  domain = df.polygon_crime$Number
)

leaflet()%>%
  addTiles() %>%
  addPolygons(data = df.polygon_crime,
              fillColor = ~pal(Number),
              color = "#5297A8", 
              fillOpacity = 0.5, 
              weight = 1, 
              smoothFactor = 1)%>%
  addLegend(pal = pal, 
            values = df.polygon_crime$Number, 
            position = "bottomright", 
            title = "Number of Crime")




###community level 311
###community_map = readOGR("CnclDist_July2012/CnclDist_July2012.shp")
### density calculation: people/ square mile
community_map = readOGR("CnclDist_July2012/CnclDist_July2012.shp")
community_map1 = spTransform(community_map, CRS("+proj=longlat +datum=WGS84"))

crs(community_map1)
community_map1$SHAPE_Area = area(community_map1)
community_map = fortify(community_map1, region = "DISTRICT")
communitymap = as.data.frame(community_map1)

communitymap$DISTRICT = as.numeric(communitymap$DISTRICT)
communitymap$SQ_MI = as.numeric(communitymap$SQ_MI)
cd_crime = data_crime %>%
  left_join(data_census17_tract,
            by = c("CT10" = "tract")) %>%
  group_by(CD) %>%
  summarise(Number = n()) %>%
  left_join(communitymap,
            by = c("CD" = "DISTRICT")) %>%
  mutate(density = Number/SQ_MI)

cd_crime$CD = as.character(cd_crime$CD)
crime_combine = left_join(community_map, cd_crime,
                          by = c("id" = "CD"))

crime1 = distinct(crime_combine, id, density)
crimeid = crime1$id

crimepolygons<-lapply(crimeid, function(x) polyFunc(x, dat=crime_combine))

crimesp.polygon<-SpatialPolygons(crimepolygons)
crimedf.polygon<-SpatialPolygonsDataFrame(crimesp.polygon,
                                          data=data.frame(row.names=crimeid, crime1))

pal <- colorNumeric(
  palette = "Blues",
  domain = crimedf.polygon$density
)

leaflet()%>%
  addTiles() %>%
  addPolygons(data = crimedf.polygon,
              fillColor = ~pal(density),
              color = "#5297A8", 
              fillOpacity = 0.8, 
              weight = 1, 
              smoothFactor = 1)%>%
  addLegend(pal = pal, 
            values = crimedf.polygon$density, 
            position = "bottomright", 
            title = "Density of Crime by Distrit")







