# Leaflet Introduction
library(leaflet)
library(stringr)
library(dplyr)
library(tidyverse)
library(htmltools)
#devtools::install_github('rstudio/leaflet')
devtools::install_github('bhaskarvk/leaflet.extras') # Cran too old install from github

install.packages("leaflet.extras")
library(leaflet.extras)
library(ggplot2)

setwd("C:/Users/blake/OneDrive/Stryker Project/Learning Leaflet")
#############################################################################################################################
#############################################################################################################################
# Base map by scrolling your grabbing new tiles
leaflet() %>%
  addTiles()

##############################################################################################################################
#############################################################################################################################
#############################################################################################################################
# Adding Provider Tiles (leaflet has over 100)

# To see names of provider tiles use this 
names(providers)[1:10] # gives you top 10 tiles

# Get all tiles provided by open street map
names(providers)[str_detect(names(providers), "OpenStreetMap")]
names(providers)[str_detect(names(providers), "CartoDB")]

#Change the base map to provider tile that is desired
leaflet() %>%
  #addTiles
  addProviderTiles("CartoDB.DarkMatter") # My favorite one!

##############################################################################################################################
#############################################################################################################################
#############################################################################################################################
# Setting default view of map zooming in on particular place

library(ggmap) # used to pass a coordinate in and it wille give you coordinates
ggmap::register_google(key = "AIzaSyAjmFA3U_enOIK6WzTTN7_0KlrryTlUEOE")
#get geocode of parents house
geocode("397 Lake Forest Boulevard, Kalamazoo, MI 49006") # 42.294668, -85.621254


# Setting default view in leaflet
# Zooming in on appartment
leaflet() %>%
  addProviderTiles('CartoDB') %>%
  setView(lng = -85.62125,lat = 42.29466, zoom = 13)

# Zoom in further
leaflet() %>%
  addProviderTiles('CartoDB') %>%
  setView(lng = -85.62125,lat = 42.29466, zoom = 20)

# Setting a bound or square box around appartment
leaflet(options = leafletOptions(minZoom = 12, dragging = TRUE)) %>%
  addProviderTiles('CartoDB') %>%
  setView(lng = -85.62125,lat = 42.29466, zoom = 14)%>% 
  # Set max bounds of map
  setMaxBounds(lng1 = -85.62125 + .05,
               lat1 = 42.29466 + .05,
               lng2 = -85.62125 - .05,
               lat2 = 42.29466- .05)
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
# Plotting Markers

# adding marker on apartments
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = -85.62125,lat = 42.29466)

# Passing multiple Markers you can use dataframe or tible
dc_hq <- tibble(hq = c("Datacamp - NYC", "DataCamp - Belgium"),
                lon = c(-73.98575, 4.718863),
                lat = c(40.74856, 50.881363))

# now pass data frame through leaflet
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat)

# You can also do it this way by piping the data it will find values as well
dc_hq %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

# Add pop-up when click on icon with markers
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat, popup = dc_hq$hq)


# Add pop-up without the markers
leaflet() %>%
  addTiles() %>%
  addPopups(lng = dc_hq$lon, lat = dc_hq$lat, popup = dc_hq$hq)


# Storing leaflet Map as an Object
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = dc_hq$lon[1],lat =dc_hq$lat[1], zoom = 13)
m

# Restoring default view
m %>%
  clearBounds()

# Clearing markers
# Add pop-up when click on icon with markers
m2 <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat, popup = dc_hq$hq)
m2

m2 %>% # do it this way to not add markers on top of each other by clearing bounds
  clearBounds() %>%
  clearMarkers()
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
# Working with IPEDS Dataset
ipeds1 <- read.csv("IPEDS.csv");colnames(ipeds)[1]<- "name" # for late ruse

# add states
ipeds <- read.csv("IPEDS.csv");colnames(ipeds)[1]<- "name"
states <-sample(c("CA","MI","NY","NC","NM","UT"), size = 3097, replace = TRUE, prob = c(.40, .20,.15,.10,.05,.05)) # make random factor column
ipeds$state <- states # add state column
# Which states have most colleges
ipeds %>% group_by(state) %>% count %>% arrange(desc(n))

# filter for california colleges (wont be all in california because its simulated)
cali_colleges <- ipeds %>% filter(state == "CA")

#Filter for California state colleges
cali_college_map <- 
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addMarkers(data = cali_colleges) # leaflet can identify lng and lat column names as longitude and lattitude without renaming
#Assuming "lng" and "lat" are longitude and latitude, respectively

cali_college_map

# Adding circle markers instead of pin markers
cali_college_map %>%
  clearMarkers() %>% # need to remove previous pin markers
  addCircleMarkers(data =cali_colleges, radius =3) # This adds the circle marker function

# Adding circle markers instead of pin markers
# adding color change and pop up function as well
cali_college_map2 <- 
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = cali_colleges, radius = 3, color = "red", popup = ~name)

cali_college_map2

#############################################################################################################################
#############################################################################################################################
# Labels and popus using the (~ operator) or piping the data

# Piping the data 
cali_colleges %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   popup = ~name,
                   color = "#FF0000",
                   radius = 2)

# Same Thing just calling the data frame by column
leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = cali_colleges$lng,
                   lat = cali_colleges$lat,
                   popup = cali_colleges$name,
                   color = "#FF0000",
                   radius = 2)

#############################################################################################################################
#############################################################################################################################
# HTML in popup function for better pop ups
# Piping the data 
cali_colleges %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   popup = ~paste0("<b>",name, "<b>", "<br/>",sector_label,"<br/>","<b>",state,"<b>"), # making bold and add break
                   color = "#FF0000",
                   radius = 2)

library( htmltools )
# Instead of having to click just use label instead of popup 
cali_colleges %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   label =  ~name , # making bold and add break
                   color = "#FF0000",
                   radius = 2)

# To make multiple variables for hover make new column in dataframe is easiest
cali_colleges$popup_text <-   paste0('<strong>', cali_colleges$name, '</strong>', '<br/>', '<strong>', cali_colleges$sector_label, '</strong>') %>%   lapply(htmltools::HTML)

# When hover over text it will result in multiple things popped up
cali_colleges %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   label = ~popup_text , # making bold and add break
                   color = "#FF0000",
                   radius = 2)
#############################################################################################################################
#############################################################################################################################
# Add color factors along with the new pop ups
pal <- colorFactor(palette = c("red","blue","#9b3a11"),
                   levels = c("Public","Private","For-Profit"))

m3 <- cali_colleges %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   color = ~pal(sector_label), # if sectorlabel is public pallet will be red private it will be blue and non for profit is other
                   label = ~popup_text , # making bold and add break
                   radius = 2)
m3

m3 %>% addLegend(pal = pal, 
          values = c("Public", "Private", "For-Profit"),
          # opacity of .5, title of Sector, and position of topright
          opacity = 0.5, title = "Sector", position = "topright")

################################################
###############################################
# Fill color now with numeric variable instead
# create numeric variable column
nrow(cali_colleges) #1291
cali_colleges$rate <- runif(1291, 0, 100) # admission rate

# filter for rate less than 50 and above zero
cali_colleges <-cali_colleges %>%
  filter(rate < 50, rate > 0)
cali_colleges$rate <- round(cali_colleges$rate,digits = 0)

################################################
# make pal NUMERIC!!!!!!!! 
# also make pal range from 0-50 because that is what we filtered for in step above
pal <- colorNumeric(palette = "Reds", domain = c(1:50),reverse = TRUE)


cali_colleges_admit <- cali_colleges %>%
  leaflet() %>%
  addProviderTiles('CartoDB') %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   color = ~pal(rate), # if sectorlabel is public pallet will be red private it will be blue and non for profit is other
                   label = ~popup_text , # making bold and add break
                   radius = 2) %>%
  addLegend(title = "Admit Rate",pal = pal, values = c(1:50))

cali_colleges_admit

#################################
# Use color brewer for better color displays
# http://colorbrewer2.org interactive examples
library(RColorBrewer)
display.brewer.all()


#############################################################################################################################
#############################################################################################################################
# Leaflets extras package#
#library(RgoogleMaps)
devtools::install_github('rstudio/leaflet')
devtools::install_github('bhaskarvk/leaflet.extras') # Cran too old install from github

install.packages("leaflet.extras")
library(leaflet.extras)

# Make map searchable
leaflet() %>%
  addTiles() %>%
  addSearchOSM()

# Search just by clicking on map
# Make map searchable
leaflet() %>%
  addTiles() %>%
  addSearchOSM() %>%
  addReverseSearchOSM()
  
  # Make map resetable
leaflet() %>%
  addTiles() %>%
  addSearchOSM() %>%
  addReverseSearchOSM() %>%
  addResetMapButton()
#############################################################################################################################
#############################################################################################################################
# Overlay Groups 

#####################################################################

# also make pal range from 0-50 because that is what we filtered for in step above
pal <- colorNumeric(palette = "Reds", domain = c(1:50),reverse = TRUE)


colleges2 <- cali_colleges %>%
  leaflet() %>%
  addProviderTiles('CartoDB') 
  
colleges2 %>%
  addCircleMarkers(data = cali_colleges, color = ~pal(sector_label), group = "Public") %>%
  addCircleMarkers(data = cali_colleges, color = ~pal(sector_label), group = "Private") %>%
  addCircleMarkers(data = cali_colleges, color = ~pal(sector_label), group = "For-Profit") %>%
   addLayersControl(overlayGroups = c("Public","Private","For-Profit"))
  

#########################################
# Load the htmltools package
library(htmltools)

# Create data frame called public with only public colleges
public <- filter(ipeds, sector_label == "Public")  

# Create a leaflet map of public colleges called m3 
m3 <- leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(data = public, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Public")

m3

######################################
# OVERLAY SELECTION
# Create data frame called private with only private colleges
private <- filter(ipeds, sector_label == "Private")  

# Add private colleges to `m3` as a new layer
m3 <- m3 %>% 
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Private") %>% 
  addLayersControl(overlayGroups = c("Public", "Private"))

m3

######################################
# Over lay 
#Mapping All Colleges

# Create data frame called profit with only for-profit colleges
profit <- filter(ipeds, sector_label == "For-Profit")  

# Add for-profit colleges to `m3` as a new layer
m3 <- m3 %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),   group = "For-Profit")  %>% 
  addLayersControl(overlayGroups = c("Public", "Private", "For-Profit"))  

# Center the map on the middle of the US with a zoom of 4
m4 <- m3 %>%
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 

m4

######################################################################################################################
######################################################################################################################
######################################################################################################################
# Base Groups

# Changing base gropus
leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB",group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  addLayersControl(baseGroups = c("OSM","Carto","Esri"),position = "topleft")


######################################################################################################################
######################################################################################################################
######################################################################################################################
# Base Groups
ipeds <- read.csv("IPEDS.csv");colnames(ipeds)[1]<- "name"
states <-sample(c("CA","MI","NY","NC","NM","UT"), size = 3097, replace = TRUE, prob = c(.40, .20,.15,.10,.05,.05)) # make random factor column
ipeds$state <- states # add state column

# filter for california colleges (wont be all in california because its simulated)
cali_colleges <- ipeds %>% filter(state == "CA")

# Make many drop down options
profit <- filter(ipeds, sector_label == "For-Profit")  
private <- filter(ipeds, sector_label == "Private")  
public <- filter(ipeds, sector_label == "Public") 

# Add color factors along with the new pop ups
pal <- colorFactor(palette = c("red","blue","#9b3a11"),
                   levels = c("Public","Private","For-Profit"))

# cali_colleges$popup_text <-   paste0('<strong>', cali_colleges$name, '</strong>', '<br/>', '<strong>', cali_colleges$sector_label, '</strong>') %>%   lapply(htmltools::HTML)
popup_text <-   paste0('<strong>', cali_colleges$name, '</strong>', '<br/>', '<strong>', cali_colleges$sector_label, '</strong>') %>%   lapply(htmltools::HTML)

# Adding it all together 
m4 <- leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addCircleMarkers(data = public, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Private")  %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "For-Profit")  %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), 
                   overlayGroups = c("Public", "Private", "For-Profit")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 

m4

####################################################
# Add them all together
m5 <- leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addCircleMarkers(data = public, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "Private")  %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "For-Profit")  %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), 
                   overlayGroups = c("Public", "Private", "For-Profit")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 

m5

####################################################
# Add them all together with Numeric rate variable
# create numeric variable column
nrow(cali_colleges) #1291
cali_colleges$rate <- runif(1335, 0, 100) # admission rate

# filter for rate less than 50 and above zero
cali_colleges <-cali_colleges %>%  filter(rate < 50, rate > 0)
cali_colleges$rate <- round(cali_colleges$rate,digits = 0)
rate <- round(cali_colleges$rate,digits = 0) # make rate variable

# also make pal range from 0-50 because that is what we filtered for in step above
pal2 <- colorNumeric(palette = "Reds", domain = c(1:50),reverse = TRUE)

m6 <- leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addCircleMarkers(data = public, radius = 2, label = ~popup_text,
                   color = ~pal2(rate), group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~popup_text,
                   color = ~pal2(rate), group = "Private")  %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~popup_text,
                   color = ~pal2(rate), group = "For-Profit")  %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), 
                   overlayGroups = c("Public", "Private", "For-Profit")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) %>%
addLegend(title = "Admit Rate",pal = pal2, values = c(1:50))

m6

####################################################################################################################
####################################################################################################################
####################################################################################################################
# Clustering colleges

m4 <- leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addCircleMarkers(data = public, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Private")  %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "For-Profit")  %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private", "For-Profit")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 

m4

# Make each sector of colleges searchable 
m4_search <- m4  %>% 
  addSearchFeatures(
    targetGroups = c("Public", "Private", "For-Profit"), 
    # Set the search zoom level to 18
    options = searchFeaturesOptions(zoom = 18)) 

# Try searching the map for a college
m4_search
#######################################################################
#######################################################################
# CLUSTER APPROACH
ipeds %>% 
  leaflet() %>% 
  addTiles() %>% 
  # Sanitize any html in our labels
  addCircleMarkers(radius = 2, label = ~htmlEscape(name),
                   # Color code colleges by sector using the `pal` color palette
                   color = ~pal(sector_label),
                   # Cluster all colleges using `clusterOptions`
                   clusterOptions = markerClusterOptions()) %>%
  addLegend("bottomright",pal = pal, values = ~sector_label,title = "Colleges Type", opacity = -.6)


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# SPATIAL POLYGON coropleth
#Load shp and wealthy_zip
#load("C:/Users/blake/OneDrive/Stryker Project/Learning Leaflet/nc_zips.R")
#load("C:/Users/blake/OneDrive/Stryker Project/Learning Leaflet/wealthiest_zips.R")

# Load dataframe
nc_income <- read.csv("income.csv");colnames(nc_income)[1] <- "zipcode"
# make zipcode column same type as in shape file
nc_income$zipcode <- as.character(nc_income$zipcode)

# left join information to shape file
shp@data <- shp@data %>%
  left_join(nc_income, by = c("GEOID10" = "zipcode"))

# look at data
glimpse(shp@data)

############################################################################################################################################
# Plotting only one polygon
shp@polygons[[1]]%>%
  leaflet() %>%
  addPolygons()

# add the map portion 
shp@polygons[[1]]%>%
  leaflet() %>%
  addTiles() %>%
  addPolygons()

# which zips were not in the income data?
shp_na <- shp[is.na(shp$mean_income),]

# map the polygons in shp_na
shp_na %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons()

# summarize the mean income variable
summary(shp$mean_income)

# subset shp to include only zip codes in the top quartile of mean income
high_inc <- shp[!is.na(shp$mean_income) & shp$mean_income > 55917,]

# map the boundaries of the zip codes in the top quartile of mean income
high_inc %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons()
#####################################################################################################################################################
#####################################################################################################################################################
# Plotting polygons

dollar <- function(x){x <- paste0("$",x)} # to make income look like income

# Too many points mapped doesnt make sense
shp %>%
  leaflet() %>%
  addTiles()%>%
  addPolygons()


shp %>%
  leaflet() %>%
  addTiles()%>%
  addPolygons(weight =1,
              color = "grey",
              label = ~paste0("Total Income: ", dollar(income)),
              highlight = highlightOptions(weight = 3,color = "red",bringToFront =  TRUE))

###################################################################################################################
# Coloring methods (colorNumeric, colorBin, colorQuantile)

#Color numeric
nc_pal <- colorNumeric(palette = "Blues",domain = shp@data$mean_income)
# See how it will look
previewColors(pal = nc_pal, values = c(seq(100000,600000, by = 100000)))

# Try color numeric
shp %>%
  leaflet() %>%
  addTiles()%>%
  addPolygons(weight =1,
              fillOpacity = 1,
              color = ~nc_pal(mean_income),
              label = ~paste0("Total Income: ", dollar(income)),
              highlight = highlightOptions(weight = 3,color = "red",bringToFront =  TRUE))

# Log transform mean_income to make mean more toward center
ggplot(shp@data,aes(mean_income)) + geom_histogram()
ggplot(shp@data,aes(log(mean_income))) + geom_histogram()
shp@data$logmean <- log(shp@data$mean_income)

# understand column makeup
 quantile(shp@data$logmean,na.rm= TRUE)

 # filter the logmean now through the plot
 nc_pal2 <- colorNumeric(palette = "Blues",domain = shp@data$logmean)
 
# Plot again with log of median
# Try color numeric
shp %>%
  leaflet() %>%
  addTiles()%>%
  addPolygons(weight =1,
              fillOpacity = 1,
              color = ~nc_pal2(logmean),
              label = ~paste0("Total Income: ", dollar(income)),
              highlight = highlightOptions(weight = 3,color = "red",bringToFront =  TRUE))




# Color bin
nc_pal3 <- colorBin(palette = "Blues",bins = 5,domain = shp@data$mean_income)

# Try color bin
shp %>%
  leaflet() %>%
  addTiles()%>%
  addPolygons(weight =1,
              fillOpacity = 1,
              color = ~nc_pal3(mean_income),
              label = ~paste0("Total Income: ", dollar(income)),
              highlight = highlightOptions(weight = 3,color = "red",bringToFront =  TRUE))



# colorQuantile
nc_pal4 <- colorQuantile(palette = "Blues", n = 4,domain = shp@data$mean_income)

# Try color Quantile
shp %>%
  leaflet() %>%
  addTiles()%>%
  addPolygons(weight =1,
              fillOpacity = 1,
              color = ~nc_pal4(mean_income),
              label = ~paste0("Total Income: ", dollar(income)),
              highlight = highlightOptions(weight = 3,color = "red",bringToFront =  TRUE))

##############################################################################################################
# NOW LETS EXPLORE!!!!!!!!!!!!!
# summarize the mean income variable
summary(shp$mean_income)

# subset shp to include only zip codes in the top quartile of mean income
high_inc <- shp[!is.na(shp$mean_income) & shp$mean_income > 55917,]

# create color palette with colorNumeric()
nc_pal <- colorNumeric("YlGn", domain = high_inc@data$mean_income)

high_inc %>%
  leaflet() %>%
  addTiles() %>%
  # set boundary thickness to 1 and color polygons
  addPolygons(weight = 1, color = ~nc_pal(mean_income),
              # add labels that display mean income
              label = ~paste0("Mean Income: ", dollar(mean_income)),
              # highlight polygons on hover
              highlightOptions = highlightOptions(weight = 5, color = "red",
                                                  bringToFront = TRUE))

# Take the log
# Create a logged version of the nc_pal color palette
nc_pal <- colorNumeric("YlGn", domain = log(high_inc@data$mean_income))

# apply the nc_pal
high_inc %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(weight = 1, color = ~nc_pal(log(mean_income)), fillOpacity = 1,
              label = ~paste0("Mean Income: ", dollar(mean_income)),
              highlightOptions = highlightOptions(weight = 5, color = "red", bringToFront = TRUE))

##############################################################################################################
##############################################################################################################
##############################################################################################################
# Putting it all together

# LOAD POINT DATA
ipeds <- read.csv("IPEDS.csv");colnames(ipeds)[1]<- "name"
states <-sample(c("CA","MI","NY","NC","NM","UT"), size = 3097, replace = TRUE, prob = c(.40, .20,.15,.10,.05,.05)) # make random factor column
ipeds$state <- states # add state column

# filter for california colleges (wont be all in california because its simulated)
cali_colleges <- ipeds %>% filter(state == "CA")

# Make many drop down options
profit <- filter(ipeds, sector_label == "For-Profit")  
private <- filter(ipeds, sector_label == "Private")  
public <- filter(ipeds, sector_label == "Public") 

# Add color factors along with the new pop ups
pal <- colorFactor(palette = c("red","blue","#9b3a11"),
                   levels = c("Public","Private","For-Profit"))

popup_text <-   paste0('<strong>', cali_colleges$name, '</strong>', '<br/>', '<strong>', cali_colleges$sector_label, '</strong>') %>%   lapply(htmltools::HTML)

# LOAD SHAPE DATA NOW

#load("C:/Users/blake/OneDrive/Stryker Project/Learning Leaflet/nc_zips.R")
#load("C:/Users/blake/OneDrive/Stryker Project/Learning Leaflet/wealthiest_zips.R")

# Load dataframe
nc_income <- read.csv("income.csv");colnames(nc_income)[1] <- "zipcode"
# make zipcode column same type as in shape file
nc_income$zipcode <- as.character(nc_income$zipcode)

# left join information to shape file
shp@data <- shp@data %>%
  left_join(nc_income, by = c("GEOID10" = "zipcode"))

# look at data
glimpse(shp@data)


# add log column for distinguising difference
shp@data$log_income <- log(shp@data$mean_income)

# Add color scale 
nc_pal <- colorNumeric(palette = "Blues",domain = shp@data$log_income)


dollar <- function(x){x <- paste0("$",x)} # to make income look like income


# Putting it all together coropleth and points
leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  addPolygons(data = shp, weight = 1, fillOpacity = .75,
              color = ~nc_pal(log_income),
              label = ~paste0("Mean Income: ", dollar(mean_income)),
              group = "Mean Income") %>%
  addCircleMarkers(data = public, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "Private")  %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "For-Profit")  %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private", "For-Profit"))%>%
  setView(lng = -79.0193,lat = 35.7596, zoom = 6)


# Saving leaflet map
# Putting it all together coropleth and points
mf <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  addPolygons(data = shp, weight = 1, fillOpacity = .75,color = ~nc_pal(log_income),label = ~paste0("Mean Income: ", dollar(mean_income)),group = "Mean Income") %>%
  addCircleMarkers(data = public, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "Private")  %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~popup_text,
                   color = ~pal(sector_label), group = "For-Profit")  %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private", "For-Profit"))%>%
  setView(lng = -79.0193,lat = 35.7596, zoom = 6)

# saving map
library(htmlwidgets)
saveWidget(mf,file = "myMap.html")


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
# Plot wealthy zipcodes
# plot zip codes with mean incomes >= $200k
wealthy_zips %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  # set color to green and create Wealth Zipcodes group
  addPolygons(weight = 1, fillOpacity = .7, color = "green",  group = "Wealthy Zipcodes", 
              label = ~paste0("Mean Income: ", dollar(mean_income)),
              highlightOptions = highlightOptions(weight = 5, color = "white", bringToFront = TRUE))

# Putting it all together
# Base Groups
ipeds <- read.csv("IPEDS.csv");colnames(ipeds)[1]<- "name"
states <-sample(c("CA","MI","NY","NC","NM","UT"), size = 3097, replace = TRUE, prob = c(.40, .20,.15,.10,.05,.05)) # make random factor column
ipeds$state <- states # add state column

# filter for california colleges (wont be all in california because its simulated)
cali_colleges <- ipeds %>% filter(state == "CA")

# Make many drop down options
profit <- filter(ipeds, sector_label == "For-Profit")  
private <- filter(ipeds, sector_label == "Private")  
public <- filter(ipeds, sector_label == "Public") 

# Add color factors along with the new pop ups
pal <- colorFactor(palette = c("red","blue","#9b3a11"),
                   levels = c("Public","Private","For-Profit"))

# cali_colleges$popup_text <-   paste0('<strong>', cali_colleges$name, '</strong>', '<br/>', '<strong>', cali_colleges$sector_label, '</strong>') %>%   lapply(htmltools::HTML)
popup_text <-   paste0('<strong>', cali_colleges$name, '</strong>', '<br/>', '<strong>', cali_colleges$sector_label, '</strong>') %>%   lapply(htmltools::HTML)

# Adding it all together 
m4 <- leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>% 
  addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
  addCircleMarkers(data = public, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Private")  %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "For-Profit")  %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri","CartoDB.DarkMatter"), 
                   overlayGroups = c("Public", "Private", "For-Profit")) %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom = 4) 

m4


#Add polygons using wealthy_zips
final_map <- m4 %>% 
  addPolygons(data = wealthy_zips, weight = 1, fillOpacity = .5, color = "Grey",  group = "Wealthy Zip Codes", 
              label = ~paste0("Mean Income: ", dollar(mean_income)),
              highlightOptions = highlightOptions(weight = 5, color = "white", bringToFront = TRUE)) %>% 
  # Update layer controls including "Wealthy Zip Codes"
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private", "For-Profit", "Wealthy Zip Codes"))     
final_map


# saving map
library(htmlwidgets)
saveWidget(final_map,file = "myFinalMap.html")
