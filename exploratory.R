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
rs <- dbSendQuery(con, "SELECT b.boat_name, b.program_type, b.registration_port, b.fishing_gear, b.gt_estimate, b.oid, b.category,
                  t.boat_id, t.tracker_id,t.tracker_status, t.tracker_start_date, t.tracker_end_date, s.*
                  FROM ifish_boat b
                  INNER JOIN ifish_boat_tracker t on b.oid= t.boat_id
                  INNER JOIN ifish_findmespot s on t.tracker_id=s.tracker_id
                  WHERE t.tracker_status=1 and s.date_time >= t.tracker_start_date
                  UNION
                  SELECT b.boat_name, b.program_type, b.registration_port, b.fishing_gear, b.gt_estimate, b.oid, b.category,
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
                  d.expenses_fuel, d.expenses_bait, d.expenses_ice, d.doc_status, d.post_status,
                  s.landing_id, s.codrs_picture_date,
                  b.oid, b.registration_port,
                  b.gt_estimate, b.gt_declared, b.program_type, b.fishing_gear, b. boat_name, b.category
                  FROM ifish_fish f
                  INNER JOIN ifish_sizing s on f.oid= s.fish_id
                  INNER JOIN ifish_deepslope d on s.landing_id = d.oid
                  INNER JOIN ifish_boat b on d.boat_id= b.oid
                  WHERE s.fish_id > 0 and s.data_quality = 1 and d.doc_status= 'Posted' and d.post_status= 'Posted'
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
    unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
    dplyr::mutate(weight=(var_a *(cm^var_b)/1000))
dffish <- dffish[dffish$fishname!=' ',]

#Calculate average fishing days (code from Steve Saul) per vessel category 
FishingDays = subset(dftracker,select=c(boat_id,date_time))
dateTimeVec_vec = lapply(X=FishingDays$date_time,FUN=function(x) {unlist(strsplit(as.character(x)," "))})
dateTimeVec_vec = do.call(rbind.data.frame,dateTimeVec_vec)
rownames(dateTimeVec_vec)=NULL
names(dateTimeVec_vec) = "Date"
dateOnlyVec_vec = lapply(X=dateTimeVec_vec$Date,FUN=function(x) {unlist(strsplit(as.character(x),"-"))})
dateOnlyVec_vec = do.call(rbind.data.frame,dateOnlyVec_vec)
rownames(dateOnlyVec_vec)=NULL
names(dateOnlyVec_vec) = c("Year_OutFishing","Month_OutFishing","Day_OutFishing")
dateOnlyVec_vec$Year_OutFishing = as.numeric(as.character(dateOnlyVec_vec$Year_OutFishing))
dateOnlyVec_vec$Month_OutFishing = as.numeric(as.character(dateOnlyVec_vec$Month_OutFishing))
dateOnlyVec_vec$Day_OutFishing = as.numeric(as.character(dateOnlyVec_vec$Day_OutFishing))
FishingDays = cbind(FishingDays,dateOnlyVec_vec)
FishingDays$date_time=NULL
FishingDays = unique(FishingDays)
FishingDays = FishingDays[FishingDays$Year_OutFishing==(as.numeric(strsplit(date()," ")[[1]][5])),] #Get only data from last year 
FishingDays = FishingDays[order(FishingDays$boat_id,FishingDays$Year_OutFishing,FishingDays$Month_OutFishing,FishingDays$Day_OutFishing),]
FishingDays = data.frame(table(FishingDays$boat_id)) #tally how many days in that year, the boat goes fishing (has a fishing day)
names(FishingDays) = c("boat_id","DaysFishingLastYear")
VesselCharacteristics = subset(dftracker,select=c(boat_id,fishing_gear,gt_estimate, category))
VesselCharacteristics = unique(VesselCharacteristics)
VesselCharacteristics$VesselCategory=""
VesselCharacteristics$VesselCategory[VesselCharacteristics$gt_estimate<5]="Nano"
VesselCharacteristics$VesselCategory[VesselCharacteristics$gt_estimate>=5 & VesselCharacteristics$gt_estimate<15]="Small"
VesselCharacteristics$VesselCategory[VesselCharacteristics$gt_estimate>=15 & VesselCharacteristics$gt_estimate<30]="Medium"
VesselCharacteristics$VesselCategory[VesselCharacteristics$gt_estimate>=30]="Large"
FishingDays = merge(FishingDays,VesselCharacteristics,all.x=TRUE,by=c("boat_id"))
AvgDaysFishedPerYear_withCategory = aggregate.data.frame(FishingDays$DaysFishingLastYear,by=list(FishingDays$category,FishingDays$fishing_gear,FishingDays$VesselCategory),FUN=mean,na.rm=TRUE)
names(AvgDaysFishedPerYear_withCategory) = c("category","fishing_gear","VesselSize","AvgDaysFished")
AvgDaysFishedPerYear_noCategory = aggregate.data.frame(FishingDays$DaysFishingLastYear,by=list(FishingDays$fishing_gear,FishingDays$VesselCategory),FUN=mean,na.rm=TRUE)
names(AvgDaysFishedPerYear_noCategory) = c("fishing_gear","VesselSize","AvgDaysFished")
AvgDaysFishedPerYear_withCategory_Table = AvgDaysFishedPerYear_withCategory
AvgDaysFishedPerYear_withCategory_Table$Size_Category = paste(AvgDaysFishedPerYear_withCategory_Table$VesselSize,AvgDaysFishedPerYear_withCategory_Table$category,sep=" - ")
AvgDaysFishedPerYear_withCategory_Table = reshape(AvgDaysFishedPerYear_withCategory_Table,v.names="AvgDaysFished",idvar="Size_Category",timevar="fishing_gear",direction="wide")
#Get average fishing trips per year per boat 
n_trips <- dffish %>% tidyr::separate(codrs_picture_date, c('Year', 'Month', 'Day'), sep="-") %>%
  group_by(Year, boat_id) %>% dplyr::summarize(n=n_distinct(landing_id))

ggplot(n_trips) + geom_point(aes(x=Year, y= n))+ facet_wrap(vars(boat_id))

n_trips_mean <- n_trips %>% group_by(boat_id) %>% summarise(mean=mean(n))
ggplot(n_trips_mean) + geom_boxplot(aes(x=boat_name, y= mean)) + theme(axis.text.x = element_text(angle = 45))

#Average fishing trip duration per year (FishingDays / n_trips_mean)
FishingDaysPerTrip <- left_join(FishingDays, n_trips_mean, by=c('boat_id')) %>% mutate (DaysFishingPerTrip=DaysFishingLastYear/mean)

################### Prepare SpotTrace Data and Get Depths #######################################
SpotTrace = data.table(SpotTrace)
names(SpotTrace) = c("pseudonym","RegisteredPort","Gear","GrossTon","Message","Latitude","Longitude","DateTime","Battery")
SpotTrace = subset(SpotTrace,select=c(pseudonym,RegisteredPort,Gear,Latitude,Longitude,DateTime))
SpotTrace = unique(SpotTrace)            
#Split date into separate columns and also in one column coded as date
dday_new_vec = lapply(X=SpotTrace$DateTime,FUN=function(x) {(strsplit(as.character(x)," "))})
dday_new_vec_unlist = data.frame(Values=unlist(dday_new_vec))
dday_new_vec_unlist$Index=rep(c(1,2),length(dday_new_vec))
dday_new_vec_unlist_Date = subset(dday_new_vec_unlist,dday_new_vec_unlist$Index==1)
dday_new_vec_unlist_Time = subset(dday_new_vec_unlist,dday_new_vec_unlist$Index==2)
dday_new_vec_unlist_Date$Values = as.character(dday_new_vec_unlist_Date$Values)
dday_new_vec_unlist_Time$Values = as.character(dday_new_vec_unlist_Time$Values)
dday_new_vec_unlist_Date$Index=NULL
dday_new_vec_unlist_Time$Index=NULL
names(dday_new_vec_unlist_Date) = c("Date")
names(dday_new_vec_unlist_Time) = c("Time")
SpotTrace = cbind(SpotTrace,dday_new_vec_unlist_Date,dday_new_vec_unlist_Time)
dday_new_vec = lapply(X=dday_new_vec_unlist_Date$Date,FUN=function(x) {(strsplit(as.character(x),"/"))})
dday_new_vec_unlist = data.frame(Values=unlist(dday_new_vec))
dday_new_vec_unlist$Values = as.numeric(as.character(dday_new_vec_unlist$Values))
dday_new_vec_unlist$Index=rep(c(1,2,3),length(dday_new_vec))
SpotTrace$Month = subset(dday_new_vec_unlist,dday_new_vec_unlist$Index==1)$Values
SpotTrace$Day = subset(dday_new_vec_unlist,dday_new_vec_unlist$Index==2)$Values
SpotTrace$Year = subset(dday_new_vec_unlist,dday_new_vec_unlist$Index==3)$Values
#Get depth for each spot trace location
SpotTrace_XY = subset(SpotTrace,select=c(Longitude,Latitude))
SpotTrace_XY = unique(SpotTrace_XY)
SpotTrace_XY$Depth = extract(DEM,SpotTrace_XY)          #Depth is in meters in the DEM
SpotTrace = merge(SpotTrace,SpotTrace_XY,all.x=TRUE,by=c("Longitude","Latitude"))
SpotTrace = subset(SpotTrace,!is.na(SpotTrace$Depth))
#Convert pseudonym to boat_id in the SpotTrace Using the Vessel data which has both fields and can be used to link the two 
VesselPseudoNameConvert = subset(Vessels,select=c(oid,pseudonyms))
VesselPseudoNameConvert = unique(VesselPseudoNameConvert)
setnames(VesselPseudoNameConvert,"pseudonyms","pseudonym")
setnames(VesselPseudoNameConvert,"oid","boat_id")
VesselPseudoNameConvert = data.table(VesselPseudoNameConvert)
SpotTrace = merge(SpotTrace,VesselPseudoNameConvert,all.x=TRUE,by=c("pseudonym"))
SpotTrace$pseudonym=NULL
SpotTrace$DateTime=NULL
SpotTrace$Date=NULL
SpotTrace$Time=NULL

################ Combine SpotTrace and Catch ###########################################
#First, determine the depth for each sampling unit as multiple pings were taken per sampling events...
#...Determine and identify "fishing event" or "sampling event" - defined here as the date of photos related to each catch - and figure out how to deal with multiple depths reported for each day
Catches = subset(dffish,!is.na(dffish$codrs_picture_date))
dateTimeSplit = lapply(X=Catches$codrs_picture_date,FUN=function(x) {unlist(strsplit(as.character(x)," "))})
dateTimeSplit=data.frame(t(as.data.frame(dateTimeSplit)))
rownames(dateTimeSplit)=NULL
names(dateTimeSplit) = c("Date")
dateTimeSplit$Date = as.character(dateTimeSplit$Date)
dateTimeSplit = lapply(X=dateTimeSplit$Date,FUN=function(x) {unlist(strsplit(as.character(x),"-"))})
dateTimeSplit=data.frame(t(as.data.frame(dateTimeSplit)))
rownames(dateTimeSplit)=NULL                
names(dateTimeSplit) = c("Year","Month","Day")
dateTimeSplit$Month = as.numeric(as.character(dateTimeSplit$Month))
dateTimeSplit$Day = as.numeric(as.character(dateTimeSplit$Day))
dateTimeSplit$Year = as.numeric(as.character(dateTimeSplit$Year))                                            
Catches = cbind(Catches,dateTimeSplit)                            #

FishingEvent = subset(Catches,select=c(boat_id,Year,Month,Day))
FishingEvent = unique(FishingEvent)
FishingEvent$fishingEvent = seq(1,dim(FishingEvent)[1],by=1)
dateTimeSplit = lapply(X=dateTimeSplit$Date,FUN=function(x) {unlist(strsplit(as.character(x),"-"))})
dateTimeSplit=data.frame(t(as.data.frame(dateTimeSplit)))
rownames(dateTimeSplit)=NULL                
names(dateTimeSplit) = c("Year","Month","Day")
dateTimeSplit$Month = as.numeric(as.character(dateTimeSplit$Month))
dateTimeSplit$Day = as.numeric(as.character(dateTimeSplit$Day))
dateTimeSplit$Year = as.numeric(as.character(dateTimeSplit$Year))                                            
dftracker = cbind(dftracker,dateTimeSplit)       
SpotTrace = merge(dftracker,FishingEvent,all.x=TRUE,by=c("boat_id","Year","Month","Day"))
SpotTrace = subset(SpotTrace,!is.na(SpotTrace$fishingEvent)) 
SpotTrace_avgLongitude = aggregate.data.frame(SpotTrace$longitude,by=list(SpotTrace$fishingEvent),FUN=mean)
names(SpotTrace_avgLongitude) = c("fishingEvent","longitude_mean")
SpotTrace_avgLatitude = aggregate.data.frame(SpotTrace$latitude,by=list(SpotTrace$fishingEvent),FUN=mean)
names(SpotTrace_avgLatitude) = c("fishingEvent","latitude_mean")
SpotTrace = subset(SpotTrace,select=c(fishingEvent,boat_id,Year,Month,Day,registration_port))
SpotTrace = unique(SpotTrace)
SpotTrace = merge(SpotTrace,SpotTrace_avgLongitude,all.x=TRUE,by=c("fishingEvent"))
SpotTrace = merge(SpotTrace,SpotTrace_avgLatitude,all.x=TRUE,by=c("fishingEvent"))
CatchArea = merge(Catches,SpotTrace,all.x=TRUE,by=c("boat_id","Year","Month","Day"))
CatchArea = CatchArea %>% na.omit()

FishingLocationsOnly = subset(CatchArea,select=c(longitude_mean,latitude_mean,fish_species))
FishingLocationsOnly = unique(FishingLocationsOnly)
FishingLocationsOnly = SpatialPointsDataFrame(coords=FishingLocationsOnly[,1:2],proj4string=CRS("+proj=longlat +ellps=WGS84"),data=data.frame(FishingLocationsOnly$fish_species))

#Count fishing days based on the number of fishingEvents per landing_id 
nDays <- CatchArea %>%  dplyr::group_by(landing_id) %>% mutate(FishingDaysPerTrip= n_distinct(fishingEvent))

#Calculate nominal CPUE for ONE gear type, ONE species, ONE WPP 
one_wpp_one_geartype <- nDays %>% filter(fishing_gear== 'Dropline', wpp1== '573') 
#Find out which trips caught multidens 
trips_multi <- one_wpp_one_geartype %>% filter(fishname== 'Pristipomoides multidens') %>% distinct(landing_id) %>% 
  dplyr::mutate(presence='1')
one_wpp_one_geartype <- left_join(one_wpp_one_geartype, trips_multi, by='landing_id')
one_wpp_one_geartype$presence[is.na(one_wpp_one_geartype$presence)] <- 0
#Calculate nominal CPUE per trip basis 
CPUE_multidens <- one_wpp_one_geartype %>% group_by(landing_id, fishname) %>% 
  mutate(CPUE=(sum(weight))/FishingDaysPerTrip) %>% na.omit()
#Filter CPUE for just multidens 
CPUE_multidens <- CPUE_multidens %>% filter(fishname=='Pristipomoides multidens') %>% distinct(landing_id, .keep_all=TRUE)
ggplot()+ geom_histogram(data=CPUE_multidens, aes(x=CPUE), binwidth = 30)

#nDays is a dataframe that has the fishing_gear, wpp1, landing_id, weight data (basically a catch data table)
#The nomimalCPUE function calculates the CPUE per species based on the catch weight and fishing days 
#It also adds a presence/ absence column but we won't use that until a lot later on, so can disregard for now 
#Make this into a function 
nominalCPUE <- function(nDays, fishinggear, wpp, fishname){
  one_wpp_one_geartype <- nDays %>% filter(fishing_gear== fishinggear, wpp1== wpp) 
  #Find out which trips caught the species
tripname <-  one_wpp_one_geartype %>% filter(fishname== fishname) %>% distinct(landing_id) %>% 
    dplyr::mutate(presence='1')
  one_wpp_one_geartype <- left_join(one_wpp_one_geartype, tripname)
  one_wpp_one_geartype$presence[is.na(one_wpp_one_geartype$presence)] <- 0
  #Calculate nominal CPUE per trip basis 
  cpuename <-  one_wpp_one_geartype %>% group_by(landing_id, fishname) %>% 
    mutate(CPUE=(sum(weight))/FishingDaysPerTrip) %>% na.omit()
  #Filter CPUE for just the fishname 
  cpuname <- cpuename %>% dplyr::select(fishname, fishing_gear, wpp1, CPUE)
}

#Now I need to iterate the function across the top species, all WPPs, and all fishing gears.... 

arr <- array(NA, dim=c(length(nom_vec$CPUE),1,1))
nom <- nominalCPUE(nDays, 'Dropline', '573', 'Pristipomoides multidens')
nom_vec <- as.vector(nom[,5])
arr[1,1,1] <- nom_vec

landing_id <- as.vector(unique(dffish$landing_id))
fishinggears <- c('Dropline', 'Longline', 'Mixgears')
wpps <- as.vector(unique(dffish$wpp1))
fishes <- as.vector('Pristipomoides multidens', 'Pristipomoides typus', 'Lutjanus malabaricus', 'Etelis sp', 'Epinephelus areolatus', 'Atrobucca brevis')

library(wrapr) 

CPUEdata <- array(NA,dim=c(length(landing_id), length(fishinggears),length(wpps),length(fishes)))
for (i in 1:length(landing_id)){
    for(j in 1: length(fishinggears)){ 
      for (k in 1: length(wpps)){
         for (l in 1: length(fishes)){
            nomCPUE <- nominalCPUE(nDays, qc(fishinggears[i]), qc(wpps[j]), qc(fishes[k]))
            CPUEdata[i,j,k,l] <-nomCPUE [,5]
            }
          }
        }
      }



one_wpp_one_geartype2 <- nDays %>% filter(fishing_gear== 'Longline', wpp1== '718') 
#Find out which trips caught multidens 
trips_multi2 <- one_wpp_one_geartype2 %>% filter(fishname== 'Pristipomoides multidens') %>% distinct(landing_id) %>% 
  dplyr::mutate(presence='1')
one_wpp_one_geartype2 <- left_join(one_wpp_one_geartype2, trips_multi, by='landing_id')
one_wpp_one_geartype2$presence[is.na(one_wpp_one_geartype2$presence)] <- 0
#Calculate nominal CPUE per trip basis 
CPUE_multidens2 <- one_wpp_one_geartype2 %>% group_by(landing_id, fishname) %>% 
  mutate(CPUE=(sum(weight))/FishingDaysPerTrip) %>% na.omit()
#Filter CPUE for just multidens 
CPUE_multidens2 <- CPUE_multidens2 %>% filter(fishname=='Pristipomoides multidens') %>% distinct(landing_id, .keep_all=TRUE)
ggplot()+ geom_histogram(data=CPUE_multidens2, aes(x=CPUE), binwidth = 5)



#Effort= FishingDays 
#Merge avg fishing days PER boat with the fish catch db 
FishingDays$boat_id <- as.numeric(as.character(FishingDays$boat_id))
catch_and_effort <- merge(dffish, FishingDays, all.x=TRUE, by=c('boat_id'))

#Calculate nominal CPUE for ONE gear type, ONE species, ONE WPP 
one_wpp_one_geartype <- catch_and_effort %>% filter(fishing_gear.x== 'Dropline', wpp1== '573') 
#Create 1-0 column to mark if trip contained multidens or not 
#Find out which trips caught multidens 
trips_multi <- one_wpp_one_geartype %>% filter(fishname== 'Pristipomoides multidens') %>% distinct(landing_id) %>% 
  mutate(presence='1')
one_wpp_one_geartype <- left_join(one_wpp_one_geartype, trips_multi, by='landing_id')
one_wpp_one_geartype$presence[is.na(one_wpp_one_geartype$presence)] <- 0
#Calculate nominal CPUE per trip basis 
CPUE_multidens <- one_wpp_one_geartype %>% group_by(landing_id, fishname) %>% 
  mutate(CPUE=(sum(weight))/DaysFishingLastYear) %>% na.omit()

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
  mutate(CPUE=sum(weight)/sum(distance))

#Visualize data distribution 
Abrevis_clean <- Abrevis_CPUE %>% distinct(landing_id, .keep_all=TRUE)
Abrevis_CPUE <- Abrevis_CPUE %>% filter(CPUE <10)
ggplot()+geom_histogram(data=Abrevis_CPUE, aes(x=CPUE), binwidth = 0.001)
#Do GLM with delta-x error model? 




