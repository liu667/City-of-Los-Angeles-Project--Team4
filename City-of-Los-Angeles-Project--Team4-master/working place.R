library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggmap)
library(zipcode)
library(lubridate)
library(readxl)
library(leaflet)
library(rgdal)
library(sp)
library(raster)
library(maptools)
library(geojsonio)
library(tmap)
setwd("/Users/xiwenchen/Dropbox/DSO 545 statistical computing and data visualization/Final Project/Team4-cityoflosangeles/City-of-Los-Angeles-Project--Team4-master")
data_311 = read.csv("la_city_independent_analysis/data/311_calls_w_CTs20171102134828.csv")
data_crime = read.csv("la_city_independent_analysis/data/crime_w_CTs20171102134814.csv")
data_shelter = read.csv("la_city_independent_analysis/data/shelters_w_CTs20171102134808.csv")
data_census17_tract = read_excel("la_city_independent_analysis/data/homeless-count-2017-results-by-census-tract.xlsx",
                                 sheet = "Count_by_Tract")
data_census17_community = read_excel("la_city_independent_analysis/data/homeless-count-2017-results-by-census-tract.xlsx",
                                     sheet = "Count_by_Community")

community_map = readOGR("CnclDist_July2012/CnclDist_July2012.shp")
community_map1 = spTransform(community_map, CRS("+proj=longlat +datum=WGS84"))
community_map = fortify(community_map1, region = "DISTRICT")
communitymap = as.data.frame(community_map1)
tract_map = readOGR("la_city_independent_analysis/data/raw_data/CENSUS_TRACTS_2010.zip_unzipped/CENSUS_TRACTS_2010.shp")
tract_map2 = spTransform(tract_map, CRS("+proj=longlat +datum=WGS84"))
tract_map = fortify(tract_map2,region = "CT10")
tract_map$id = as.numeric(tract_map$id)

total_commu$CD = as.character(total_commu$CD)
communitymap$DISTRICT = as.character(communitymap$DISTRICT)
data_census17_tract$CD = as.character(data_census17_tract$CD)
communitymap$SQ_MI = as.numeric(communitymap$SQ_MI)
total_commu$CD = as.character(total_commu$CD)

polyFunc<-function(groupname, dat){
  poly<-filter(dat, id==groupname) %>%
    dplyr::select(long, lat)
  return(Polygons(list(Polygon(poly)), groupname))
}

shelter_map = readOGR("Homeless_Shelters_and_Services/Homeless_Shelters_and_Services.shp")





data_311$CREATEDDATE = mdy_hms(data_311$CREATEDDATE)
data_311$Week = week(data_311$CREATEDDATE)


  
  data_week311 = data_311 %>%
    filter(Week == "1") 
  
  data_week311%>%
    leaflet()%>%  
    addTiles() %>%
    addPolygons(data = community_map1,
                color = "#c8515f", 
                fillOpacity = 0.5, 
                weight = 1, 
                smoothFactor = 1) %>%
    addCircleMarkers(lng=data_week311[,23], lat=data_week311[,22],
                     radius = 6, stroke = FALSE, fillOpacity = 0.5)


  homeless_count_2016 = read_excel("la_city_independent_analysis/data/HC2016_Total_Counts_by_Census_Tract_LA_CoC_07132016.xlsx",
                                                         sheet = "Data")
  homeless_count_2015 = read.csv("2015_homeless_count.csv")

  unsheltered2015 = homeless_count_2015%>%
    dplyr::select(City,X2015_Total,Sheltered,SQMI)%>%
    mutate(unsheltered = X2015_Total - Sheltered,
           perc_unsheltered = ifelse(X2015_Total<=0,0,unsheltered/X2015_Total))%>%
    group_by(City)%>%
    summarise("unsheltered_perc_15" = mean(perc_unsheltered),
              "unsheltered_num_15" = mean(unsheltered))%>%
    arrange(desc(unsheltered_perc_15))
  
  ## YEAR 2016 showing %-unsheltered in each city
  unsheltered2016 = homeless_count_2016%>%
    dplyr::select(City,totUnsheltPeople,totPeople)%>%
    mutate(perc_unsheltered = ifelse(totPeople<=0,0,totUnsheltPeople/totPeople))%>%
    group_by(City)%>%
    summarise("unsheltered_perc_16" = mean(perc_unsheltered),
              "unsheltered_num_16" = mean(totUnsheltPeople))%>%
    arrange(desc(unsheltered_perc_16))
  
  ## YEAR 2017 %-unsheltered in each city
  unsheltered2017 = data_census17_tract%>%
    dplyr::select(City,totUnsheltPeople,totPeople)%>%
    mutate(perc_unsheltered = ifelse(totPeople<=0,0,totUnsheltPeople/totPeople))%>%
    group_by(City)%>%
    summarise("unsheltered_perc_17" = mean(perc_unsheltered),
              "unsheltered_num_17" = mean(totUnsheltPeople))%>%
    arrange(desc(unsheltered_perc_17))
  
  ## 3 YEAR Percent unsheltered people in each city
  ##        Number of unsheltered people in each city
  ##        Number of shelters in each city
  "unsheltered15-16" = left_join(unsheltered2016,unsheltered2015,by = "City")
  "unsheltered15-17" = left_join(`unsheltered15-16`,unsheltered2017,by ="City")%>%
    dplyr::select(City = City,"2015_perc" = unsheltered_perc_15,
           "2015_num" = unsheltered_num_15,
           "2016_perc" = unsheltered_perc_16,
           "2016_num" = unsheltered_num_16,
           "2017_perc" = unsheltered_perc_17,
           "2017_num" = unsheltered_num_17)

  shelter_count = data_shelter %>%
    group_by(CITY)%>%
    summarise(num_shelter = n())
  `unsheltered15-17` = full_join(`unsheltered15-17`,shelter_count, by = c("City"="CITY"))
  
  ## 3 Year Unsheltered Severity = percent unsheltered x number of unsheltered people
  unsheltered_severity = `unsheltered15-17`%>%
    dplyr::mutate("15_severity" = `2015_perc`*`2015_num`,
           "16_severity" = `2016_perc`*`2016_num`,
           "17_severity" = `2017_perc`*`2017_num`,
           changeInSeverity = ifelse(is.na(`15_severity`),
                                     `17_severity`-`16_severity`,
                                     `17_severity`-`15_severity`))%>%
    dplyr::select(City,`15_severity`,`16_severity`,`17_severity`,changeInSeverity)%>%
    dplyr::arrange(changeInSeverity)
  
 severityIncTop10 = unsheltered_severity%>%
   dplyr::arrange(changeInSeverity)%>%
    top_n(10)
 

  







  