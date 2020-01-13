# 1.data processing ---------------------------------------------------------

# 1.1 library the packages we need ------------------------------------------

library(dplyr)
library(sp)
library(spatstat)
library(gstat)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(rgeos)
library(geojson)
library(geojsonio)
library(tmaptools)
library(spatialEco)
library(devtools)
library(ggplot2)
library(rpivotTable)
library(tidyverse)
library(readxl)
library(rJava)
library(XLConnectJars)
library(XLConnect)
library(pivottabler)
library(raster)
library(fpc)
library(plyr)
library(OpenStreetMap)
library(rgdal)
library(spdep)
library(grid)
library(corrplot)
library(mapproj)
library(spatialreg)

# 1.2 import all data from online ---------------------------------------------
# 1.2.1. London Borough Boundaries --------------------------------------
EW <- geojson_read(
  "https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_4.geojson",
  what = "sp")
BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]

# 1.2.2  all infruastructures from dataset of Cultural Infrastructure --------

# first we download all of them one by one as they are uploaded seperately
# we found that not all of their columns are sorted in the same way
# so we have to arrange them with the function "select" of pacakage "dplyr" (distinguish "select" from the library "MASS")while importing and drop some information we don't need
order<-c("name","borough_code","borough_name","longitude","latitude","easting","northing")
Theatres <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/c9bda99b-2156-4ecd-b09d-c207ae5947e2/Theatres.csv")%>%dplyr::select(order)
TheatreRehearsalStudios<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/db3e3507-4ae1-4795-b838-2e0293bc722c/Theatre_rehearsal_studio.csv")%>% dplyr::select(order)
TextileDesign <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/2ff48612-b14d-4289-abf6-6294d40e5fb5/Textile_design.csv")%>% dplyr::select(order)
SkateParks <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/acf74a29-212f-4cd4-b01e-464bf83fcd99/Skate_Parks.csv")%>% dplyr::select(order)
SetandExhibition <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/4d364e07-e01b-4f2b-8916-3b7e2be7cadb/Set_and_exhibition_building.csv")%>% dplyr::select(order)
Pubs<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/7e6b0f36-da01-4029-99d7-640d89c4a0a4/Pubs.csv")%>% dplyr::select(order)
PropandCostumeMaking<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/5d3de727-af1d-4e67-8d3d-7e245cbaa9ac/Prop_and_costume_making.csv")%>% dplyr::select(order)
OutdoorSpaces<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/6e16c2db-bd27-4e1e-9ab7-ea6369966393/Outdoor_spaces_for_cultural_use.csv")%>% dplyr::select(order)
MusicRecording<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/277ad0a9-5cfd-45c7-9962-0e464337530a/Music_recording_studios.csv")%>% dplyr::select(order)
MusicGrassrootVenues<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/5effe6f3-3697-49c4-8dcb-d9db58e7e00b/Music_venues_grassroots.csv")%>% dplyr::select(order)
MusicVenues<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/ceb42f72-aee0-4bdc-8eb8-973c66ec9f81/Music_venues_all.csv")%>% dplyr::select(order)
MusicRehearsalStudios<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/e3ee048e-b661-4eeb-89ed-f96b485378db/Music_rehearsal_studios.csv")%>% dplyr::select(order)
MusicOfficeBusiness<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/fa9e6157-807f-48d3-8ac4-6a4030490131/Music_office_based_businesses.csv")%>% dplyr::select(order)
MuseumandPublicGalleries<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/d091e270-182a-444d-b4ee-64d9d9682749/Museums_and_public_galleries.csv")%>% dplyr::select(order)
MakingandManufacturing<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/9261b9e3-29dc-4c88-890b-12d3d5a226f6/Making_and_manufacturing.csv")%>%dplyr::select(order)
Makerspaces<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/a19ebe8a-c026-4f4e-ac56-53d3f48b72fb/Makerspaces.csv")%>% dplyr::select(order)
LiveinArtistsWorkplace<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/3bce412e-b1c8-4554-a120-a5920008ab17/Live_in_artists_workspace.csv")%>% dplyr::select(order)
Libraries<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/2dd6ce1e-61a9-499d-8e3e-c6fb26726e7b/Libraries.csv")%>% dplyr::select(order)
LGBTVenues<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/4f9b2984-b171-49ff-b72a-31430803d3b9/LGBT_night_time_venues.csv")%>% dplyr::select(order)
LegalStreetArtWalls<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/21cd5ed4-e846-44ea-b6c1-1afdf244e292/Legal_street_art_walls.csv")%>% dplyr::select(order)
MediaProductionStudios<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/d149fb3f-c6b2-4d8b-8cfb-89efe74f0c5f/Large_media_production_studios.csv")%>% dplyr::select(order)
JewelleryDesign<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/4d9469ea-6bee-4836-9a95-9082a4728f12/Jewellery_design.csv")%>% dplyr::select(order)
FashionDesignManufacturing<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/4eb7a9c2-c4e4-4032-a54a-9e38ef3fe6e8/Fashion_and_design.csv")%>% dplyr::select(order)
DanceRehearsalStudios<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/5e278953-8445-46df-ae48-1fa7e9f16157/Dance_rehearsal_studios.csv")%>%dplyr:: select(order)
DancePerformanceVenues<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/730a2942-39ad-4a34-87fe-efbbb5cc7c71/Dance_performance_venues.csv")%>% dplyr::select(order)
CreativeWorkspace<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/d74cdb33-1528-41c5-9849-06afd60c2678/Creative_workspaces.csv")%>% dplyr::select(order)
CreativeCOworkingDeskSpace<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/40bf1a44-98ac-40d1-b558-9698f99bd050/Creative_coworking_desk_space.csv")%>% dplyr::select(order)
CommercialGalleries<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/27c653ae-080e-469d-b0d9-4a663b52d726/Commercial_galleries.csv")%>% dplyr::select(order)
CommunityCentres<- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/a8625bba-addb-4fae-a737-244b2281f429/Community_centres.csv")%>% dplyr::select(order)
Cinemas <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/f353f97b-a1fb-41fa-810a-825586839b4d/Cinemas.csv")%>% dplyr::select(order)
ArtCentres <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/bec79216-7a51-4810-89d5-da8cc44d8458/Arts_centres.csv")%>% dplyr::select(order)
ArtistsWorkspaces <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/f4254814-935e-4ffd-b9f6-f841dfd1cd82/Artists_workspaces.csv")%>% dplyr::select(order)
Archives <- read.csv("https://data.london.gov.uk/download/cultural-infrastructure-map/21358543-f1cd-42e1-9af4-17ab2c0bb010/Archives.csv")%>% dplyr::select(order)

# have a quick look
head(Archives)

# 1.2.3 other datasets ----------------------------------------------------
# public transport accessibility level 
PTAL <- read.csv("https://data.london.gov.uk/download/public-transport-accessibility-levels/8e520b81-dd06-4ce6-aaa0-972dccf84b57/Borough%20AvPTAI2015.csv")
PTAL <-data.frame(PTAL)
names(PTAL)<- c("code","name","AvPTAI2015","PTAL")
head(PTAL)

# the official estimates of average (mean) household income at the middle layer super output area (MSOA) level in England
download.file("https://data.london.gov.uk/download/ons-model-based-income-estimates--msoa/ad11a43a-048b-437c-9f7e-116df59eb811/ons-model-based-income-estimates-msoa.xls",
              "Income.xls",mode="wb")
Income <- readxl::read_excel("Income.xls", sheet =2)
# convert table to dataframe and select the data we need
Income <- Income[,c(3:4,7)][grep("^E09",Income$"Local authority code"),]
names(Income)
Income2<- tapply(Income$`Total annual income (¡ê)`,list(Income$`Local authority code`),
                 FUN=mean,default=1)
Income3 <- data.frame(Income2)
Income3$code <- PopDen$code
names(Income3)<- c("Income2015","code")
head(Income3)


# Land area and population density figures for 2001 to 2050 for wards and boroughs. 
download.file("https://data.london.gov.uk/download/land-area-and-population-density-ward-and-borough/cc4e7e08-3071-488f-bdd9-a62cb1ed1c5c/land-area-population-density-london.xlsx",
              "PopulationDensity.xlsx",mode="wb")
# select the data we need
PopDen <- readxl::read_excel("PopulationDensity.xlsx", sheet =2,
                             range = cell_rows(3:36))
# convert table to dataframe
PopDen <- data.frame(PopDen)
PopDen <- PopDen[,c(1:2,9)]
transform(PopDen, ...9 = as.numeric(...9))
names(PopDen)<- c("code","name","PopulationDensity2015")
head(PopDen)

# Average House Prices
download.file("https://data.london.gov.uk/download/average-house-prices/f01b1cc7-6daa-4256-bd6c-94d8c83ee000/land-registry-house-prices-borough.xls",
              "HousePrice.xls",mode="wb")
# select the data we need
HousePrice <- readxl::read_excel("HousePrice.xls", sheet =2)
names(HousePrice)
HousePrice <- HousePrice[2:34,c("Code","Area","Year ending Dec 2015")]
names(HousePrice)<- c("code","name","HousePrice2015")
head(HousePrice)

# 1.3 recategorize them in a more general type ---------------------------

# we recategorize them according to their function
# for example: all musical infrustructrues can be arranged in a new type "music"
# first we should have a quick view to check if columns are sorted in a same order
# then we can rebind datasets which in a same type


# type--1--literature
# contents: archives and libraries
literature<- rbind(Archives, Libraries)
literature$type<-("literature")

# type--2--music
# contents: music office business, music recording studios, music rehearsal studios, music venues and music grassroot venues 
music<- rbind(MusicGrassrootVenues,MusicOfficeBusiness,MusicRecording,MusicRehearsalStudios,MusicVenues)
music$type<-("music")

# type--3--dance
# contents: dance performance venues, dance rehearsal studios
dance<- rbind(DancePerformanceVenues,DanceRehearsalStudios)
dance$type<-("dance")

# type--4--theatre
# contents: theatres, dance rehearsal studios
theatre<- rbind(Theatres,TheatreRehearsalStudios)
theatre$type<-("theatre")

# type--5--fashion and design
# contents: fashion design and manufacturing, jewellery design and munufacturing, making and manufacturing, textile design and finishing service
fashiondesign<- rbind(FashionDesignManufacturing,JewelleryDesign,TextileDesign,MakingandManufacturing)
fashiondesign$type<-("fashiondesign")

# type--6--pub
# contents: pubs
pub<- Pubs
pub$type<-("pub")

# type--7--LGBT
# contents: LGBT venues
LGBT<- LGBTVenues
LGBT$type<-("LGBT")

# type--8--community
# contents: community centres
community<- CommunityCentres
community$type<-("community")

# type--9--creative
# contents: artists workspaces, creative coworking desk space, creative workspaces, live in artists'workspaces, makerspaces
creative<- rbind(ArtistsWorkspaces,CreativeWorkspace,CreativeCOworkingDeskSpace,LiveinArtistsWorkplace,Makerspaces)
creative$type<-("creative")

# type--10--film
# contents: cinemas, large media production studios, prop and costume making and service
film<- rbind(Cinemas,MediaProductionStudios,PropandCostumeMaking)
film$type<-("film")

# type--11--openspace
# contents: legal street art walls, outdoor spaces for cultural use, skate parks
openspace<- rbind(LegalStreetArtWalls,OutdoorSpaces,SkateParks)
openspace$type<-("openspace")

# type--12--exhibition
# contents: art centres, museum and public galleries, set and exhibition design and building, commercial galleries
exhibition<- rbind(ArtCentres,MuseumandPublicGalleries,SetandExhibition,CommercialGalleries)
exhibition$type<-("exhibition")

# and then we rebind all items into one dataframe
all<-rbind(community,creative,dance,exhibition,fashiondesign,film,LGBT,literature,music,openspace,pub,theatre)
categories<-unique(all$type)


# 1.4 plot them on map ----------------------------------------------------
# first we have to verify that the data "longitude"and"latitude" are numeric and drop all the NAs
mode(all$longitude)
mode(all$latitude)
all <- all[!is.na(all$latitude),]
all <- all[!is.na(all$longitude),]

# convert it to a spatial point dataframe
allSP <- SpatialPointsDataFrame(coords = c(all[,c("longitude", "latitude")]),
                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                data = all)
tmap_mode("view")
qtm(BoroughMap)+qtm(allSP)
# we found that some points flied so far away
# we looked back to the initial data, we found that some longitude and latitude data of the infrastructures were not missed but they were wrong
# and some information about borough code and name were missed
# so we have to pick up all the right points inside the boundary of boroughs and complete their administrative information

#drop those away points
LondonMAP<- gUnaryUnion(BoroughMap) 
allSP2<-intersect(allSP,LondonMAP)

#set the projection 
BNG = "+init=epsg:27700"
allSPBNG<- spTransform(allSP2,BNG)
BoroughMapBNG <- spTransform(BoroughMap,BNG)

#join points to borough
BoroughMapBNG@data$poly.ids <- BoroughMapBNG@data[["lad15cd"]]
allSP3 <- point.in.poly(allSPBNG, BoroughMapBNG)
head(allSP3@data)
#now every item has its poly.ids, telling us which borough it belongs to
allSP3_points <- allSP3 %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = proj4)
qtm(BoroughMapBNG)+qtm(allSP3)

tm_shape(BoroughMapBNG) +
  tm_polygons('#f0f0f0f0') +
tm_shape(allSP3_points) + 
  tm_dots("type")
  
# 2. clustering analysis --------------------------------------------------

# 2.1 Kernel Density Estimation -------------------------------------------------------------

#set a window as the borough boundary
window <- as.owin(BoroughMapBNG)
#remove duplicates
allSP4 <- remove.duplicates(allSP3)

#create ppp objects
all.ppp <- ppp(x=allSP4@coords[,1],y=allSP4@coords[,2],window=window)
plot(density(all.ppp, sigma = 1000))

#for other kind of infrastructures
community.ppp <- ppp(x=allSP4[allSP4@data$type=="community",]@coords[,1],
                     y=allSP4[allSP4@data$type=="community",]@coords[,2],window=window)
creative.ppp <- ppp(x=allSP4[allSP4@data$type=="creative",]@coords[,1],
                    y=allSP4[allSP4@data$type=="creative",]@coords[,2],window=window)
dance.ppp <- ppp(x=allSP4[allSP4@data$type=="dance",]@coords[,1],
                 y=allSP4[allSP4@data$type=="dance",]@coords[,2],window=window)                                                                          
exhibition.ppp <- ppp(x=allSP4[allSP4@data$type=="exhibition",]@coords[,1],
                      y=allSP4[allSP4@data$type=="exhibition",]@coords[,2],window=window)
fashiondesign.ppp <- ppp(x=allSP4[allSP4@data$type=="fashiondesign",]@coords[,1],
                         y=allSP4[allSP4@data$type=="fashiondesign",]@coords[,2],window=window)
film.ppp <- ppp(x=allSP4[allSP4@data$type=="film",]@coords[,1],
                y=allSP4[allSP4@data$type=="film",]@coords[,2],window=window)
LGBT.ppp <- ppp(x=allSP4[allSP4@data$type=="LGBT",]@coords[,1],
                y=allSP4[allSP4@data$type=="LGBT",]@coords[,2],window=window)
literature.ppp <- ppp(x=allSP4[allSP4@data$type=="literature",]@coords[,1],
                      y=allSP4[allSP4@data$type=="literature",]@coords[,2],window=window)
music.ppp <- ppp(x=allSP4[allSP4@data$type=="music",]@coords[,1],
                 y=allSP4[allSP4@data$type=="music",]@coords[,2],window=window)
openspace.ppp <- ppp(x=allSP4[allSP4@data$type=="openspace",]@coords[,1],
                     y=allSP4[allSP4@data$type=="openspace",]@coords[,2],window=window)
pub.ppp <- ppp(x=allSP4[allSP4@data$type=="pub",]@coords[,1],
               y=allSP4[allSP4@data$type=="pub",]@coords[,2],window=window)
theatre.ppp <- ppp(x=allSP4[allSP4@data$type=="theatre",]@coords[,1],
                   y=allSP4[allSP4@data$type=="theatre",]@coords[,2],window=window)
plot(density(community.ppp, sigma = 1000))
plot(density(creative.ppp, sigma = 1000))
plot(density(dance.ppp, sigma = 1000))
plot(density(exhibition.ppp, sigma = 1000))
plot(density(fashiondesign.ppp, sigma = 1000))
plot(density(film.ppp, sigma = 1000))
plot(density(LGBT.ppp, sigma = 1000))
plot(density(literature.ppp, sigma = 1000))
plot(density(music.ppp, sigma = 1000))
plot(density(openspace.ppp, sigma = 1000))
plot(density(pub.ppp, sigma = 1000))
plot(density(theatre.ppp, sigma = 1000))

# Standard deviational ellipse
allSP4SF <- data.frame(allSP4)
ggplot(BoroughMap) + 
  aes(long,lat) + geom_polygon(alpha = 0.05) +
  geom_point(data = allSP4SF,aes(x=longitude, y=latitude,color=type,),alpha=0.1)+
  stat_ellipse(data = allSP4SF,type = 'norm',level = 0.68,aes(x=longitude, y=latitude,color=type),size=1,alpha = 1)

# 2.2 spatial autocorrelation ---------------------------------------------------------------
# 2.2.1 calculate Moran's I and General G---------------------------------------------------------
# count all of the cultural infrastructures within each bourough in the City
res <- poly.counts(allSP4, BoroughMapBNG)
BoroughMapBNG@data$allcount<-res
BoroughMapBNG@data$allDensity <- BoroughMapBNG$allcount/poly.areas(BoroughMapBNG)
# have a quick look
tm_shape(BoroughMapBNG) +
  tm_polygons("allDensity",
              style="jenks",
              palette="RdYlBu",
              midpoint=NA) 
# we found that infrastructures clustered in central borough in London
# their might be spatial autocorrelation among them
# so we will check with the method of Moran's I and General G
# calculate the centroids of all borough in London
coordsW <- coordinates(BoroughMapBNG)
plot(coordsW)
#generate a spatial weights matrix with queen??s case neighbours
#this method means that polygons with a shared edge or a corner will be included in computations for the target polygon
#create a neighbours list
LBorough_nb <- poly2nb(BoroughMapBNG, queen=T)

#plot them and add a map underneath
plot(BoroughMapBNG)
plot(LBorough_nb, coordinates(coordsW), col="red",add=T)

#create a spatial weights object from these weights
LBorough.lw <- nb2listw(LBorough_nb, style="C")
head(LBorough.lw$neighbours)

# Moran??s I test 
I_LBorough_Global_Density <- moran.test(BoroughMapBNG@data$allDensity, LBorough.lw)
I_LBorough_Global_Density

# Getis Ord General G
G_LBorough_Global_Density <- globalG.test(BoroughMapBNG@data$allDensity, LBorough.lw)
G_LBorough_Global_Density

# 2.2.2 plot the result of calculation on map -----------------------------


#use the localmoran function to generate I for each borough in the city
I_LBorough_Local <- localmoran(BoroughMapBNG@data$allcount, LBorough.lw)
I_LBorough_Local_Density <- localmoran(BoroughMapBNG@data$allDensity, LBorough.lw)
#have a quick look
head(I_LBorough_Local_Density)

# copy data to borough's spatialPolygonsDataframe
BoroughMapBNG@data$BLocI <- I_LBorough_Local[,1]
BoroughMapBNG@data$BLocIz <- I_LBorough_Local[,4]
BoroughMapBNG@data$BLocIR <- I_LBorough_Local_Density [,1]
BoroughMapBNG@data$BLocIRz <- I_LBorough_Local_Density [,4]

#plot a map of the local Moran¡®s I and General G
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))
GIColours<- rev(brewer.pal(8, "RdBu"))

Gi_LBorough_Local_Density <- localG(BoroughMapBNG@data$allDensity, LBorough.lw)
BoroughMapBNG@data$BLocGiRz <- Gi_LBorough_Local_Density

tm_shape(BoroughMapBNG) +
  tm_polygons(col=c("BLocIRz","BLocGiRz"),
              style="fixed",
              breaks=breaks1,
              alpha = 0.5,
              palette=MoranColours,
              midpoint=NA,
              title=c("Moran's I","General G"))+
  tm_facets(sync = TRUE, ncol = 2,free.coords=TRUE)

# 2.3 spatial lagged regression -------------------------------------------

#merge boundaries and data
#check all of the columns have been read in OK
BoroughMapBNG@data<- left_join(BoroughMapBNG@data, PopDen, by = c("lad15cd" = "code"))
BoroughMapBNG@data<- left_join(BoroughMapBNG@data, PTAL, by = c("lad15cd" = "code"))
BoroughMapBNG@data<- left_join(BoroughMapBNG@data, HousePrice, by = c("lad15cd" = "code"))
BoroughMapBNG@data<- left_join(BoroughMapBNG@data, Income3, by = c("lad15cd" = "code"))


tm_shape(BoroughMapBNG) +
  tm_polygons(col=c("PopulationDensity2015","AvPTAI2015","HousePrice2015","Income2015"),
              palette=MoranColours,
              style="jenks")+
  tm_facets(sync = TRUE, ncol = 4)
# we found that these factors have some similar trends of distribution         
# so we would like to check possible linear relationship by plotting
# affect of population density
affectPD <- ggplot(data=BoroughMapBNG@data,
                   aes_string(x = "PopulationDensity2015", y = "allDensity"))+
  stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()+
  xlab("Population density2015") + ylab("density of infrastructures")
# affect of transport
affectTP <- ggplot(data=BoroughMapBNG@data,
                   aes_string(x = "AvPTAI2015", y = "allDensity"))+
  stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()+
  xlab("AvPTAI2015") + ylab("density of infrastructures")
# affect of house price
affectHP <- ggplot(data=BoroughMapBNG@data,
                   aes_string(x = "HousePrice2015", y = "allDensity"))+
  stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()+
  xlab("HousePrice2015") + ylab("density of infrastructures")
# affect of income
affectIN <- ggplot(data=BoroughMapBNG@data,
                   aes_string(x = "Income2015", y = "allDensity"))+
  stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()+
  xlab("Income2015") + ylab("density of infrastructures")


# scatter plots
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,4)))
print(affectPD, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(affectTP, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(affectHP, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(affectIN, vp = viewport(layout.pos.row = 1, layout.pos.col = 4))


# Running a simple linear Regression Model in R
modelTP <- lm(`AvPTAI2015` ~ `allcount`, data = BoroughMapBNG@data)
summary(modelTP)

modelPD <- lm(`PopulationDensity2015` ~ `allDensity`, data = BoroughMapBNG@data)
summary(modelPD)

modelHP <- lm(`HousePrice2015` ~ `allDensity`, data = BoroughMapBNG@data)
summary(modelHP)

modelIN <- lm(`Income2015` ~ `allDensity`, data = BoroughMapBNG@data)
summary(modelIN)
# we found that the model of popultion density didn' work well
# so we tried to tranform the variables
modelPD2 <- lm(log(`PopulationDensity2015`) ~ `allDensity`, data = BoroughMapBNG@data)
summary(modelPD2)
modelPD3<- lm((`PopulationDensity2015`)^2 ~ `allDensity`, data = BoroughMapBNG@data)
summary(modelPD3)
# they were better but nor ideal
# so we decided to drop this variable

# check if the residuals of models are normally distributed
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(qplot(modelTP$residuals)+geom_histogram(bins =15) , vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(qplot(modelHP$residuals)+geom_histogram(bins =15) , vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(qplot(modelIN$residuals)+geom_histogram(bins =15) , vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
# despite some outliers, residuals were distributed normally

# test Multicolinearity
modelMulti <- lm(`allcount` ~ `AvPTAI2015`+`HousePrice2015`+`Income2015`, data = BoroughMapBNG@data)
summary(modelMulti)
# compute the Pearson correlation coefficient between the variables
cormat <- cor(BoroughMapBNG@data%>%dplyr::select(AvPTAI2015,HousePrice2015,Income2015,allDensity), use="complete.obs", method="pearson")
corrplot(cormat)
# try to dop out some redundant variables
modelMultiStep<-step(modelMulti)
summary(modelMultiStep)
# the results showed we should keep them all


# check for spatial-autocorrelation
BoroughMapBNG@data$modelMulti_resi <- residuals(modelMulti)
tm_shape(BoroughMapBNG) +
  tm_polygons("modelMulti_resi",
              palette = "RdYlBu") 
# we can find  spatial autocorrelation biasing our model
# so let's use Moran??s I to check it
moran.test(BoroughMapBNG@data$modelMulti_resi, LBorough.lw )





