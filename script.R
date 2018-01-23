require(rvest) || install.packages("rvest")
require(dplyr) || install.packages("dplyr")
require(tidyr) || install.packages("tidyr")
require(sp)    || install.packages("sp")
require(maps)  || install.packages("maps")
require(maptools) || install.packages("maptools")
require(rvest) || install.packages("rvest")
require(dplyr) || install.packages("dplyr")
require(tidyr) || install.packages("tidyr")
require(sp)    || install.packages("sp")
require(maps)  || install.packages("maps")
require(maptools) || install.packages("maptools")
require(deldir) || install.packages("deldir")
require(rgeos)  || install.packages("rgeos")
library(GISTools)
library(tigris)
library(raster)
library(rgdal)
library(foreign)
library(RColorBrewer)
library(ggplot2)

satations_url <- "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
stations <- readLines(satations_url)
stations_raw <- read.csv(text = c(paste(1,2,3,4,5,6,7,8,9, sep = "  "), stations), sep = "", stringsAsFactors = FALSE)

stations_df <- stations_raw %>%
  filter(grepl("^US", X1) & X2 != "") %>%
  .[,1:3] %>%
  select("STATION" = X1, "LON" = X3, "LAT" = X2  ) %>%
  mutate(LON = as.numeric(LON),
         LAT = as.numeric(LAT))


counties <- map('county', fill = TRUE, col = "transparent", plot = FALSE)
IDs <- counties$names
counties_sp <- map2SpatialPolygons(counties, 
                                   IDs = IDs,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))

points_SP <- SpatialPoints(stations_df[,2:3], 
                           proj4string = CRS("+proj=longlat +datum=WGS84") )

indices <- over(points_SP, counties_sp)
countyNames <- sapply(counties_sp@polygons, function(x) x@ID)

stations_df$cnames <- countyNames[indices]

data(county.fips)
FIPS <- setNames(county.fips$fips, county.fips$polyname)


stations_df$STATE <- state.abb[ match(gsub("(.*),.*", "\\1", stations_df$cnames), tolower(state.name)) ]
stations_df$FIPS <- ifelse( nchar(FIPS[stations_df$cnames]) < 5, paste0(0, FIPS[stations_df$cnames]), FIPS[stations_df$cnames])
stations_df$CZ_NAME <- toupper(gsub(".*,(.*)", "\\1", stations_df$cnames))



seperated <- read.csv("2017.csv", stringsAsFactors = FALSE, colClasses = "character", header = FALSE) %>%
  filter(V3 %in% c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")) %>%
  filter(grepl("^US", V1)) %>%
  select("ID" = V1, "DATE" = V2, "ELEMENT" = V3, "VALUE" = V4, "OBSTIME" = V8)

data <- seperated %>%
  mutate(DATE = as.Date(DATE, format = "%Y%m%d")) %>%
  spread(ELEMENT, VALUE) %>%
  mutate(YEAR = format(DATE,'%Y'),
         MONTH = format(DATE,'%m'),
         DAY = format(DATE,'%d'),
         TMAX = (as.numeric(TMAX)/10) * 1.8 + 32,
         TMIN = (as.numeric(TMIN)/10) * 1.8 + 32) 



year <- as.numeric(data$YEAR[1])
D <- as.numeric(max(data$DATE) - min(data$DATE))

N <- data %>%
  group_by(ID, DATE) %>%
  summarise(n = any(!is.na(TMAX)) & any(!is.na(TMIN))) %>%
  filter(DATE != min(data$DATE)) %>%
  group_by(ID) %>%
  summarise(M = sum(n) / D)

final <- data %>%
  ungroup() %>%
  left_join(N) %>%
  left_join(stations_df, by = c("ID" = "STATION")) %>%
  filter(M == 1) 

final$DATE <- NULL
final$OBSTIME <- NULL
final$M <- NULL
final$LON <- NULL
final$LAT <- NULL

data.table::fwrite(final,  "processed.csv")



satations_url <- "ghcnd-stations.txt"
stations <- readLines(satations_url)
stations_raw <- read.csv(text = c(paste(1,2,3,4,5,6,7,8,9, sep = "  "), stations), sep = "", stringsAsFactors = FALSE)

#Selecting US stations
stations_df <- stations_raw %>%
  dplyr::filter(grepl("^US", X1) & X2 != "") %>%
  .[,1:3] %>%
  dplyr::select("STATION" = X1, "LON" = X3, "LAT" = X2  ) %>%
  dplyr::mutate(LON = as.numeric(LON),
                LAT = as.numeric(LAT))
points_SP <- SpatialPoints(stations_df[,2:3], proj4string = CRS("+proj=longlat +datum=WGS84"))

#Selecting US mainland stations
usa <- map('usa', fill = TRUE, col = "transparent", plot = FALSE)
usa_sp <- map2SpatialPolygons(usa, IDs = usa$names, proj4string=CRS("+proj=longlat +datum=WGS84"))
indices <- over(points_SP, usa_sp)
stations_df_main <- stations_df[ifelse(!is.na(indices), T, F),]


  
#loading the data
data <- data.table::fread("processed.csv")
data <- dplyr::select(data, -c(cnames, STATE, CZ_NAME)) %>%
    left_join(stations_df_main, by = c("ID" = "STATION") ) %>%
    mutate(FIPS = as.character(ifelse(nchar(FIPS) < 5, paste0(0, FIPS), FIPS)))
  
  #Selecting unique stations from the data
data_stations <- data %>%
    dplyr::filter(!is.na(LAT) & !is.na(LON)) %>%
    dplyr::filter(!is.na(TMAX) & !is.na(TMIN))  %>%
    dplyr::distinct(ID, .keep_all = T) 
  
  #Computing voronoi cells
vr <- deldir(data_stations$LON, data_stations$LAT)
  
  
  #Converting voronoi cells to spatial polygons
  titles <- tile.list(vr)
  polys <- list()
  require(sp)
  for (i in seq(along=titles)) {
    pcrds = cbind(titles[[i]]$x, titles[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID = paste0(titles[[i]]$pt, collapse = "_"))
  }
  vr_sp <- SpatialPolygons(polys, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  CRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0.0 +y_0=0.0
+ellps=GRS80 +units=m +datum=NAD83 +no_defs +towgs84=0,0,0"
  
  ID_vr <- data.frame( ID = sapply(vr_sp@polygons, function(x) x@ID), area_vr = sapply(vr_sp@polygons, function(x) x@area) )
  rownames(ID_vr) <- ID_vr$ID
  vr_spdf <- SpatialPolygonsDataFrame(vr_sp, data.frame(dat = ID_vr))
  vr_spdf <- spTransform(vr_spdf, CRS)
    
  usa_sps <- gSimplify(usa_sp, tol = 0.00001)
  vr_spdf_intersected <- intersect(vr_spdf, usa_sps)
  vr_spdf_intersected <- spTransform(vr_spdf_intersected, CRS)
  
  vr_spdf2 <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(vr_spdf_intersected)){
   coords <-  vr_spdf_intersected@polygons[[i]]@Polygons[[1]]@coords
   id <- vr_spdf_intersected@data$dat.ID[i]
   df <- data.frame(id, coords, stringsAsFactors = FALSE)
   vr_spdf2 <- rbind(vr_spdf2, df)
  }
  
  vr_spdf4 <-  data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(vr_spdf)){
    coords <-  vr_spdf@polygons[[i]]@Polygons[[1]]@coords
    id <- vr_spdf@polygons[[i]]@ID
    df <- data.frame(id, coords, stringsAsFactors = FALSE)
    vr_spdf4 <- rbind(vr_spdf4, df)
  }
    
  
colnames(vr_spdf2) <- c("ID", "long", "lat")
colnames(vr_spdf4) <- c("ID", "long", "lat")

data2 <- data %>%
  dplyr::filter(!is.na(TMAX) | !is.na(TMIN)) 
  
vr_spdf3 <- vr_spdf2 %>%
  mutate(ID = as.character(ID)) %>%
  separate(ID, c("LON", "LAT"), sep = "_", convert = T) %>%
  left_join(data2, by = c("LON", "LAT")) %>%
  mutate(TAVG = (TMAX + TMIN)/2 ) %>%
  dplyr::select(ID, long, lat, TAVG, MONTH, DAY) %>%
  mutate(DATE =  as.Date(paste(MONTH, DAY, "2017"), format = "%m %d %Y"))
  

feather::write_feather(vr_spdf4, "voronoi.feather")

require(data.table)
require(feather)
vr_spdf3 <- as.data.table(vr_spdf3)

dates <- unique(vr_spdf3$DATE)
for(i in seq_along(dates)){
  write_feather(vr_spdf3[DATE == dates[i]], paste0(dates[i], "_voronoi.feather"))
}

  
  
  #Loading counties polygons
  counties <- map('county', fill = TRUE, col = "transparent", plot = FALSE)
  counties_sp <- map2SpatialPolygons(counties, IDs = counties$names, proj4string=CRS("+proj=longlat +datum=WGS84"))
  counties_sps <- gSimplify(counties_sp, tol = 0.00001)
  
  ID_co <- data.frame( ID = sapply(counties_sps@polygons, function(x) x@ID), area_co =  sapply(counties_sps@polygons, function(x) x@area))
  rownames(ID_co) <- ID_co$ID
  counties_spdf <- SpatialPolygonsDataFrame(counties_sps, data.frame(dat = ID_co)) 
  
  #Computing intersection areas of counties and voronoi cells
  intersection_areas <- raster::intersect(counties_spdf, vr_spdf)
  
  
  data(county.fips)
  FIPS <- setNames(county.fips$fips, county.fips$polyname)
  
  data$FIPS <- NULL
  aggregated_data <- intersection_areas@data %>%
    mutate(FIPS = as.character(ifelse( nchar(FIPS[dat.ID.1]) < 5, paste0(0, FIPS[dat.ID.1]), FIPS[dat.ID.1])),
           int_area = sapply(intersection_areas@polygons, function(x) x@area),
           dat.ID.2  = as.character(dat.ID.2)) %>%
    group_by(FIPS) %>%
    mutate(dat.area_co = sum(unique(dat.area_co))) %>%
    ungroup() %>%
    mutate(WEIGHT = int_area / dat.area_co) %>% 
    separate(dat.ID.2, c("LON", "LAT"), sep = "_", convert = T) %>%
    select(FIPS, LON, LAT, WEIGHT) %>%
    left_join(data, by = c("LON", "LAT"))
  
  
  
  same_coordinates <- aggregated_data %>%
    group_by(LON, LAT) %>%
    summarise(g = length(unique(ID))) %>%
    filter(g != 1)
  
aggregated_data[aggregated_data$LON %in% same_coordinates$LON & aggregated_data$LAT %in% same_coordinates$LAT,]$WEIGHT <- aggregated_data[aggregated_data$LON %in% same_coordinates$LON & aggregated_data$LAT %in% same_coordinates$LAT,]$WEIGHT / 2
  
data.table::fwrite(select(aggregated_data, -c(LON, LAT)), paste0(aggregated_data$YEAR[[1]], "_weighted.csv") )
  
aggregated_data <- data.table::fread("2017_weighted.csv", colClasses = c("character", "integer", "character", "integer", "integer","integer", "double", "double", "integer", "integer", "integer"))

day <- aggregated_data %>% 
  mutate(TAVG = WEIGHT*((TMAX + TMIN)/2)) %>%
  group_by(FIPS, MONTH, DAY) %>%
  summarise(TAVG = sum(TAVG, na.rm = T)) 

map.county <- map_data('county')
map.county$county_names <- paste0(map.county$region, ',', map.county$subregion)
map.county$FIPS <- ifelse( nchar(FIPS[map.county$county_names]) < 5, paste0(0, FIPS[map.county$county_names]), FIPS[map.county$county_names])

day$DATE <-  as.Date(paste(day$MONTH, day$DAY, "2017"), format = "%m %d %Y")

day2 <- ungroup(day) %>% dplyr::select(FIPS, DATE, TAVG) 



new.map.county <- left_join(map.county, day2) %>% dplyr::select(-c(county_names, order, region, subregion, county_names))
spcounty <- SpatialPointsDataFrame(coords = new.map.county[,c("long", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84"), data = new.map.county[,3:6])
spcounty2 <- spTransform(spcounty, CRS)
spcounty2 <- as.data.frame(spcounty2)

require(data.table)
require(feather)
spcounty3 <- as.data.table(spcounty2)

dates <- unique(spcounty3$DATE)
for(i in seq_along(dates)){
  write_feather(spcounty3[DATE == dates[i]], paste0(dates[i], "_county.feather"))
}

ggplot(spcounty2, aes(x=long, y=lat, group=group, fill = TAVG)) + 
  geom_polygon() + 
  coord_map("lagrange") + 
  guides(fill = guide_colorbar(title = expression(paste("Average temperature [",degree,"F]")), 
                                title.position = "top",
                               title.theme = element_text(size = 12, family = "mono", angle = 0) )) +
  theme_void() + 
  scale_fill_gradientn(colours = rev(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")),
                       limits = c(-20, 120),
                       breaks = c(0, 50, 100)) +
  theme(
  legend.position = "bottom",
  plot.margin = unit(c(0,0,0,0), "cm"),
legend.text = element_text(size = 10, family = "mono"),
legend.key = element_rect(fill = "black"),
legend.key.height = unit(8, "pt"),
legend.key.width = unit(40, "pt"),
legend.box.margin = unit(c(0, 0, 0.1, 0), 'cm')) 


library(ggvis)

test <- dplyr::filter(new.map.county, region == "texas" )
test %>% ggvis(~long, ~lat)  %>% group_by(group) %>% layer_paths(fill = input_select(label="DATE:",

                                                                                     