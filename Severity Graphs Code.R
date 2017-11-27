library(ggplot2)
library(dplyr)
library(ggmap)
library(stringr)
library(leaflet)
library(rgdal)
library(sp)
library(raster)
library(maptools)
library(tmap)
library(tidyr)


"311 Calls" = read.csv("311 Calls.csv")
homeless_count_2016 = read.csv("2016_homeless_count.csv")
homeless_count_2015 = read.csv("2015_homeless_count.csv")
homeless_count_2017_tract = read.csv("2017_homeless_count_tract.csv")
homeless_count_2017_community = read.csv("2017_homeless_count_community.csv")

##YEAR 2015: showing the average DENSITY & %-UNSHELTERED of each city in desc order
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
unsheltered2017 = homeless_count_2017_tract%>%
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
  select(City = City,"2015_perc" = unsheltered_perc_15,
         "2015_num" = unsheltered_num_15,
         "2016_perc" = unsheltered_perc_16,
         "2016_num" = unsheltered_num_16,
         "2017_perc" = unsheltered_perc_17,
         "2017_num" = unsheltered_num_17)
Shelter = read.csv("Shelter.csv")
shelter_count = Shelter %>%
  group_by(CITY)%>%
  summarise(num_shelter = n())
`unsheltered15-17` = full_join(`unsheltered15-17`,shelter_count, by = c("City"="CITY"))

## 3 Year Unsheltered Severity = percent unsheltered x number of unsheltered people
unsheltered_severity = `unsheltered15-17`%>%
  mutate("15_severity" = `2015_perc`*`2015_num`,
         "16_severity" = `2016_perc`*`2016_num`,
         "17_severity" = `2017_perc`*`2017_num`,
         changeInSeverity = ifelse(is.na(`15_severity`),
                                   `17_severity`-`16_severity`,
                                   `17_severity`-`15_severity`))%>%
  select(City,`15_severity`,`16_severity`,`17_severity`,changeInSeverity)%>%
  arrange(changeInSeverity)

##### GRAPH ### Unsheltered severity increase top 10 cities
severityIncTop10 = unsheltered_severity%>%
  arrange(changeInSeverity)%>%
  top_n(10)
ggplot(severityIncTop10,aes(x=reorder(City,changeInSeverity),
                            y=changeInSeverity))+
  geom_col(stat = "identity",fill = "lightblue")+coord_flip()+
  geom_text(aes(label=round(changeInSeverity,digits = 0)),position = position_nudge(x=0,y=5))+
  ylab("Increase in Unsheltered Severity")+guides(fill = FALSE)+
  xlab("City")+
  ggtitle("Top 10 Cities with Greateset INCREASE in Unshltered Severity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 15))+
  theme(axis.title = element_text(size = 14))

##### GRAPH ### Unsheltered severity decrease top 10 cities
severityDecTop10 = unsheltered_severity%>%
  arrange(changeInSeverity)%>%
  slice(1:10)%>%
  mutate(decrease = changeInSeverity*-1)
ggplot(severityDecTop10, aes(x=reorder(City,decrease),y=decrease))+
  geom_col(stats = "identity",fill = "lightblue")+coord_flip()+
  geom_text(aes(label=round(changeInSeverity,digits = 0)),position = position_nudge(x=0,y=1.5))+
  ylab("Decrease in Unsheltered Severity")+guides(fill=FALSE)+
  xlab("City")+
  ggtitle("Top 10 Cities with Greatest DECREASE in Unsheltered Severity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 14))+
  theme(axis.title = element_text(size = 14))


## getting lon and lat for Top DEC Cities
severityDecTop10$City = paste(severityDecTop10$City,",CA")
severityDecTop10$City = str_replace(severityDecTop10$City,"Unincorporated","")
SeverityDecCoords = geocode(severityDecTop10$City)
SeverityDecCoords = SeverityDecCoords%>%
  mutate(City = severityDecTop10$City)%>%
  mutate(Rank = c(1:10))
severityDecTop10 = full_join(severityDecTop10,SeverityDecCoords,by = "City")

## getting lon and lat for Top INC Cities
severityIncTop10$City = paste(severityIncTop10$City,",CA")
severityIncTop10$City = str_replace(severityIncTop10$City,"Unincorporated","")
SeverityIncCoords = geocode(severityIncTop10$City)
severityIncCoords = SeverityIncCoords%>%
  mutate(City = severityIncTop10$City)%>%
  mutate(Rank = c(10:1))
severityIncTop10 = full_join(severityIncTop10,severityIncCoords,by = "City")

#### GRAPH ## Spatial Map of Top DEC Cities
LAMap = get_map("Malibu",zoom = 8,maptype = "terrain")
ggmap(LAMap)+
  geom_point(data = severityDecTop10,
             aes(x=lon,y=lat,size = severityDecTop10$decrease*40,
                 color = "green"))+
  geom_text(data = severityDecTop10,aes(x=lon,y=lat+0.05,label=Rank,size = 100))+
  scale_color_manual(values = "darkgreen")+
  theme(legend.position = "none")+
  ggtitle("Location of Top Cities with Greatest Decrease in Unsheltered Severity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 12))+
  theme(axis.title = element_text(size = 14))

#### GRAPH ## Spatial Map of Top INC Cities
LAMap_zoom = get_map("Santa Monica",zoom = 9,maptype = "terrain")
ggmap(LAMap_zoom)+
  geom_point(data = severityIncTop10,
             aes(x=lon,y=lat,size = severityIncTop10$changeInSeverity*30,color = "red"))+
  geom_text(data = severityIncTop10,aes(x=lon,y=lat,label=City,size = 50))+
  scale_color_manual(values = "darkred")+
  theme(legend.position = "none")+
  ggtitle("Location of Top Cities with Greatest Increase in Unsheltered Severity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background= element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(face = "bold",size = 12))+
  theme(axis.title = element_text(size = 14))
