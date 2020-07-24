#Load in data fron PgAdmin
m <- dbDriver("PostgreSQL")
con <- dbConnect(m,host='localhost', port='5432',user="postgres", password="fishy", dbname="ifish_03242020")
rs2 <-dbSendQuery(con, "SELECT f.oid, f.var_a, f.var_b, f.fish_genus, f.fish_species, f.lmat, f.lopt, f.linf, f.lmax, f.fish_code, s.cm,
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
dffish <- dffish %>%setNames(make.unique(names(.))) %>%
  unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
  dplyr::mutate(weight=(var_a *(cm^var_b)/1000))

#******************* Prepare Data for Stephens and McCall ****************
#Create a data set that just has the trip id value and the species codes caught on that trip
scheduleSpec = data.frame(SCHEDULE=dffish$landing_id, SPECIES=dffish$fish_code)
#remove any duplicates:  get unique presence/absence those species caught on each trip
scheduleSpec = unique(scheduleSpec)
#count number of trips that each species was caught on
numSpeciesOnEachTrip = as.data.frame(table(scheduleSpec$SPECIES))
names(numSpeciesOnEachTrip) = c("SPECIES", "numTrips")
numSpeciesOnEachTrip = numSpeciesOnEachTrip[order(numSpeciesOnEachTrip$numTrips, decreasing=TRUE),]
numSpeciesOnEachTrip$percent = (numSpeciesOnEachTrip$numTrips/sum(numSpeciesOnEachTrip$numTrips))*100
#get a list of the species that occur on 1% or more of trips
numSpeciesOnEachTrip = subset(numSpeciesOnEachTrip, numSpeciesOnEachTrip$percent >= 1)
#Construct the Presence-Absence Matrix for Analysis in Stephens and McCall Approach
dateOnlyVec_vec = lapply(X=dffish$codrs_picture_date,FUN=function(x) {unlist(strsplit(as.character(x),"-"))})
dateOnlyVec_vec = do.call(rbind.data.frame,dateOnlyVec_vec)
rownames(dateOnlyVec_vec)=NULL
names(dateOnlyVec_vec) = c("Year","Month","Day")
dateOnlyVec_vec$Year = as.numeric(as.character(dateOnlyVec_vec$Year))
dateOnlyVec_vec$Month = as.numeric(as.character(dateOnlyVec_vec$Month))
dateOnlyVec_vec$Day = as.numeric(as.character(dateOnlyVec_vec$Day))
dffish = cbind(dffish,dateOnlyVec_vec)

scheduleSpecYear = data.frame(SCHEDULE=dffish$landing_id, SPECIES=dffish$fish_code, year=dffish$Year)
scheduleSpecYear = unique(scheduleSpecYear)  #must remove any duplicate species codes on a trip
scheduleSpecYear$count <- 1     #add count variable
options(scipen=100)
presAbsMatrix_temp=xtabs(count~ SCHEDULE + SPECIES, data=scheduleSpecYear)
presAbsMatrix=as.data.frame.matrix(presAbsMatrix_temp)
#SAME SPEDIES AS Nancie used last time hardcoded here for continuity
#keep only those species that occur on at least one percent of the trips across the time period
speciesOnePerc = as.vector(as.character(factor(numSpeciesOnEachTrip[['SPECIES']])))
isTargetPresent = length(subset(speciesOnePerc, speciesOnePerc==as.character('LL021')))
if(isTargetPresent==0){  #If the target species is not in the 1%, keep that column anyway
  speciesOnePerc = c(speciesOnePerc,'LL021')
}
presAbsMatrix = subset(presAbsMatrix, select=speciesOnePerc)
colNames_old = names(presAbsMatrix)
#provide a letter in front of column names in preparation of formula in S & M
for(i in 1:length(colNames_old)){
  if(i==1)
  {
    colNames = paste("x",colNames_old[i],sep="")
  }
  if(i!=1)
  {
    temp = paste("x",colNames_old[i],sep="")
    colNames = c(colNames,temp)
  }
}
names(presAbsMatrix) = colNames
presAbsMatrix$SCHEDULE = rownames(presAbsMatrix)
tripYear = data.frame(SCHEDULE=scheduleSpecYear$SCHEDULE,year=scheduleSpecYear$year)
tripYear = unique(tripYear)
presAbsMatrix = merge(presAbsMatrix, tripYear)

#*************** Stephens and McCall Species Association Approach ***************#
spec4formula = subset(colNames, colNames!=paste("x",as.character('LL021'),sep=""))
names(presAbsMatrix)[names(presAbsMatrix)==paste("x",as.character('LL021'),sep="")] = "TARGET"  #re-name as target
for(i in 1:length(spec4formula)){
  if(i==1)
  {
    Formula = paste("TARGET ~",spec4formula[i])
  }
  if(i!=1)
  {
    Formula = paste(paste(Formula,"+"),spec4formula[i])
  }
}
Formula=as.formula(Formula)
presAbsMatrix$TARGET <- as.factor(presAbsMatrix$TARGET)
my.lm=glm(formula=Formula,family=binomial,data=presAbsMatrix) # Regress on all species
presAbsMatrix$TARGET <- as.numeric(presAbsMatrix$TARGET)
obs = sum(presAbsMatrix$TARGET)
thresh=seq(0,1,by=0.01)
thresh.effect=thresh
thresh.count=thresh
for(i in 1:length(thresh)){
    thresh.effect[i] = abs(obs - sum(fitted.values(my.lm) > thresh[i]))
    thresh.count[i] = sum(fitted.values(my.lm) > thresh[i])
}
mythresh=cbind(thresh,thresh.effect,thresh.count)
best = min(thresh.effect)
best.thresh = thresh[thresh.effect == best]
TARGET_Trips.pred=ifelse(fitted.values(my.lm) > best.thresh,1,0)
False.neg = sum(ifelse(presAbsMatrix$TARGET > TARGET_Trips.pred,1,0))
False.pos = sum(ifelse(presAbsMatrix$TARGET < TARGET_Trips.pred,1,0))
Correct.pred = sum(ifelse(presAbsMatrix$TARGET == TARGET_Trips.pred,1,0))
Trips = length(presAbsMatrix[,1])
Pct.correct = Correct.pred/Trips*100
Pct.correct
False.neg/Trips*100
False.pos/Trips*100
years = seq(min(scheduleSpecYear$year),max(scheduleSpecYear$year))
foo = hist(fitted.values(my.lm),plot=F, breaks=9)
myhist = data.frame(cbind(foo$mids,foo$count))
foo = coefficients(my.lm)
mycoeffs = data.frame(cbind(names(foo), foo))
#Threshold Plot
#windows(5,5)
plot(thresh, thresh.effect, main='Lutjanus malabaricus', ylab = 'Difference between actual and predicted trips', xlab = 'Probablility Threshold', pch=16)
lines(thresh, thresh.effect)
#jpeg(paste(PATH,'Threshold_Plot.jpg',sep='/'),units="in",width=5,height=5,res=144)
plot(thresh, thresh.effect, main='Lutjanus malabaricus', ylab = 'Difference between actual and predicted trips', xlab = 'Probablility Threshold', pch=16)
lines(thresh, thresh.effect)
dev.off()
#Actual and Predicted Plot
yr.byspcs=data.frame(Year=years,Actual=rep(0,length(years)),Predicted=rep(0,length(years)))
for (i in 1:length(years)){
    yr.byspcs$Actual[i] = sum(presAbsMatrix$TARGET[presAbsMatrix$year == years[i]])
    yr.byspcs$Predicted[i] = sum(fitted.values(my.lm)[presAbsMatrix$year == years[i]])
}
#windows(5,5)
leg.txt = c("Observed", "Predicted")
plot(yr.byspcs[,1], yr.byspcs[,2], xlab='Year', ylab='Trips (Actual and Predicted)', main='Lutjanus malabaricus', pch=15)
lines(yr.byspcs[,1], yr.byspcs[,3], col=2)
lines(yr.byspcs[,1], yr.byspcs[,2])
points(yr.byspcs[,1], yr.byspcs[,3], col=2, pch=16)
legend("topleft", legend=leg.txt, col=1:2, pch=15:16)
#jpeg(paste(PATH,'Predicted_Trips.jpg',sep='/'),units="in",width=5,height=5,res=144)
plot(yr.byspcs[,1], yr.byspcs[,2], xlab='Year', ylab='Trips (Actual and Predicted)', main='Lutjanus malabaricus', pch=15)
lines(yr.byspcs[,1], yr.byspcs[,3], col=2)
lines(yr.byspcs[,1], yr.byspcs[,2])
points(yr.byspcs[,1], yr.byspcs[,3], col=2, pch=16)
legend("topleft", legend=leg.txt, col=1:2, pch=15:16)
dev.off()
#Probability histograms
#windows(5,5)
hist(fitted.values(my.lm), xlab='Probability', ylab='Frequency', main='Lutjanus malabaricus')
#jpeg(paste(PATH,'probability_hist.jpg',sep='/'),units="in",width=5,height=5,res=144)
#hist(fitted.values(my.lm), xlab='Probability', ylab='Frequency', main=Title)
dev.off()
#Write Diagnostic Tables if Desired
#if(steph_mccall_diagnos_tables==TRUE)
#{
#write.table(yr.byspcs, quote=F,row=F,sep=',', file =(paste(PATH,'yr_byspcs_out.csv',sep='/')))
#write.table(myhist, quote=F,row=F,sep=',', file =(paste(PATH,'hist_out.csv',sep='/')))
#write.table(mycoeffs, quote=F,row=F,sep=',', file =(paste(PATH,'coeffs_out.csv',sep='/')))
#write.table(mythresh, quote=F,row=F,sep=',', file =(paste(PATH,'thresh_out.csv',sep='/')))
#}

#Finally....Select the trips based on the threshold determined by the regression
selectedTrips = as.data.frame((presAbsMatrix[fitted.values(my.lm)> best.thresh,])$SCHEDULE)
names(selectedTrips) = "SCHEDULE"

#My coeffs
#SpecCor = mycoeffs
#names(SpecCor) = c("sp_code_x","corr")
#rownames(SpecCor)=NULL
#SpecCor = SpecCor[-1,]
#SpecCor$sp_code_x = as.character(SpecCor$sp_code_x)
#SpecCor$SPECIES = as.numeric(as.character((unlist(strsplit(SpecCor$sp_code_x,split="x",fixed=TRUE)))[seq(0,length(SpecCor[,1])*2,by=2)]))
#numSpeciesOnEachTrip$SPECIES = as.numeric(as.character(numSpeciesOnEachTrip$SPECIES))
#speciesNames = data.frame(SPECIES = dataIN$SPECIES, COMMON=dataIN$COMMON)
#speciesNames = unique(speciesNames)
#pecCor = merge(SpecCor, speciesNames)
#SpecCor = SpecCor[order(SpecCor$corr),]
#write.table(SpecCor, paste(PATH,"spec_corr.csv",sep="/"),col.names=TRUE,row.names=FALSE,sep=",")



