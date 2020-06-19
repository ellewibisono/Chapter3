#CPUE analysis

library(DBI)
library(RPostgreSQL)
library(ifish)
library(dplyr)
library(purrr)
library(data.table)
library(tidyr)
library(ggplot2)
library(raster)
library(ncdf4)
library(raster)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(rmapshaper)
library(sf)
library(spatialEco)

#Load database as dfifsh and dftracker
m <- dbDriver("PostgreSQL")
con <- dbConnect(m,host='localhost', port='5432',user="postgres", password="fishy", dbname="ifish_03242020")
rs <- dbSendQuery(con, "SELECT b.boat_name, b.program_type, b.registration_port, b.fishing_gear, b.gt_estimate, b.oid,
                  t.boat_id, t.tracker_id,t.tracker_status, t.tracker_start_date, t.tracker_end_date, s.*
                  FROM ifish_boat b
                  INNER JOIN ifish_boat_tracker t on b.oid= t.boat_id
                  INNER JOIN ifish_findmespot s on t.tracker_id=s.tracker_id
                  WHERE t.tracker_status=1 and s.date_time >= t.tracker_start_date
                  UNION
                  SELECT b.boat_name, b.program_type, b.registration_port, b.fishing_gear, b.gt_estimate, b.oid,
                  t.boat_id, t.tracker_id, t.tracker_status, t.tracker_start_date, t.tracker_end_date, s.*
                  FROM ifish_boat b
                  INNER JOIN ifish_boat_tracker t on b.oid= t.boat_id
                  INNER JOIN ifish_findmespot s on t.tracker_id=s.tracker_id
                  WHERE t.tracker_status=0 and s.date_time >= t.tracker_start_date and
                  s.date_time <= t.tracker_end_date;")

dftracker <- fetch(rs, n=-1)
dbHasCompleted(rs)
rs2 <-dbSendQuery(con, "SELECT f.oid, f.var_a, f.var_b, f.fish_genus, f.fish_species, f.lmat, f.lopt, f.linf, f.lmax, s.cm,
                  d.boat_id, d.landing_date, d.wpp1, d.first_codrs_picture_date,
                  d.expenses_fuel, d.expenses_bait, d.expenses_ice,
                  s.landing_id,
                  b.oid, b.registration_port,
                  b.gt_estimate, b.gt_declared, b.program_type, b.fishing_gear, b. boat_name
                  FROM ifish_fish f
                  INNER JOIN ifish_sizing s on f.oid= s.fish_id
                  INNER JOIN ifish_deepslope d on s.landing_id = d.oid
                  INNER JOIN ifish_boat b on d.boat_id= b.oid
                  WHERE s.fish_id > 0 and s.data_quality = 1
                  ORDER BY s.landing_id")
dffish <- fetch(rs2, n=-1)

#load some depth information
depth <- raster("/Users/ElleWibisono/Desktop/Dissertation/R codes/gebco.tif")
# Read the shape file with WPP boundaries
WPP <- readOGR(dsn="/Users/ElleWibisono/Desktop/Dissertation/R codes/GIS",layer="WPP_boundaries")
eez <- st_read('/Users/ellewibisono/Desktop/Dissertation/GearSelectivity/Rcode/eez/eez.shp')
idn <- getData('GADM', country = 'idn', level = 0)
#convert into simple features 
idn <- ms_simplify(idn) %>% st_as_sf(idn)
WPP_sf2 <- rmapshaper::ms_simplify(WPP) %>% st_as_sf()
eez <- ms_simplify(eez) %>% st_as_sf()

dffish <- dffish %>%setNames(make.unique(names(.))) %>%
    dplyr::filter(program_type=='Snapper') %>%
    unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
    dplyr::mutate(weight=(var_a *(cm^var_b)/1000))

#Data exploration on the prices (expenses)
cost <- dffish %>% dplyr::select(expenses_fuel, expenses_bait, expenses_ice, fishing_gear, gt_estimate, boat_name, wpp1) %>%
  filter(expenses_bait >0 | expenses_fuel >0 | expenses_ice>0) %>% distinct(boat_name, .keep_all=TRUE) %>%
  gather("cost_type", "expenses", 1:3) #%>%
 # group_by(wpp1, fishing_gear) %>%
 # summarise(mean(expenses_bait), mean(expenses_fuel), mean(expenses_ice))
expensesplot <- ggplot()+ geom_point(data=cost, aes(x=fishing_gear, y=expenses, color=cost_type), alpha=0.7)+
  facet_wrap(~wpp1)
expensesBoxplot <- ggplot()+ geom_boxplot(data=cost, aes(x=fishing_gear, y=expenses))+
  facet_wrap(~wpp1 + cost_type)

catchweight <- dffish %>% group_by(landing_id, fishname) %>%
  mutate(totalweight=sum(weight))
joindf <- ifish::catch_location(dffish, dftracker, depthraster=depth)
joindf <- joindf %>% group_by(landing_id, fishname) %>%
  mutate(totalweight=sum(weight))
fishing <- fishing_grounds(dftracker, depth)

#Filter dataframe for just one species for now, set latlong as sf object 
Abrevis <- joindf %>% filter(fishname=='Atrobucca brevis')
Abrevis_sf <-  st_as_sf(Abrevis, coords=c('longitude', 'latitude'))
Abrevis_sf  <- st_set_crs(Abrevis_sf , "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#Create grid across the EEZ 
grid <- create_grid(eez)

#Intersect grid and points (fishing coordinates)
Abrevis_intersect <- point.in.poly(Abrevis_sf, grid, sp=FALSE) #Points in the same grid have the same ID number 
#Compute CPUE per grid 
Abrevis_CPUE <- Abrevis_intersect %>% mutate(year=year(first_codrs_picture_date.x)) %>%
                                               group_by(ID, year) %>% 
  mutate(CPUE=n()/sum(distance))




