library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
library(leaflet)
source("util.R")



baseurl <- "https://opensky-network.org/api"

#
endpointS <- "/states/all"
endpointT <- "/tracks/all"

#
queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)


#### NORTH SEA ####
lamin=52.055129
lamax=56.196869
lomin=-6.065140
lomax=4.305954

fullurl=paste0("https://opensky-network.org/api/states/all?lamin=",lamin,"&lomin=",lomin,"&lamax=",lamax,"&lomax=",lomax)
fullurl=paste0(baseurl,endpointS,"?lamin=",lamin,"&lomin=",lomin,"&lamax=",lamax,"&lomax=",lomax)

# one take over NS (skateboard)
token=getToken()
res <- httr::GET(fullurl,add_headers(Authorization = paste("Bearer", token)))
rescontent <- httr::content(res, as="text")
resretval <- jsonlite::fromJSON(rescontent)
statedfpr <- as.data.frame(resretval$states)

# find all icaos and put in vector
icaov=statedfpr$V1

# find all tracks for each normal icao
# start with one test icao
ictest=icaov[[8]]
token=getToken()
turl=paste0(baseurl,endpointT,"?icao24=",ictest, "&time=0")
res <- httr::GET(turl,add_headers(Authorization = paste("Bearer", token)))
res$status_code
resretval <- fromJSON(content(res, as="text"))
statedf <- as.data.frame(resretval)
colnv=c("icao24","callsign","startTime","endTime","time","lat","lng","alt","crs","grd")
colnames(statedf)=colnv

#I Wulfs kode <- Vi skal lige have definere vores plots inden vi kan køre dem. 
myMapPlot <- function(df) {
  leaflet(df) %>% 
    addTiles() %>% 
    addPolylines(lat = ~lat, lng = ~lng)
}

myEDAPlots <- function(df) {
  par(mfcol=c(1,3), ann=F)
  plot(df$lat,df$lng)
  plot(df$crs,df$time)
  plot(df$alt,df$time)
}



###########
myEDAPlots(statedf)
myMapPlot(statedf)


# load training-data
fl=list.files(path = "./train",pattern = "*rds", full.names = T)
collist=lapply(fl,readRDS)

##### Vi får en fejl her
### Pause til den her basse #####testdf=collist[[4]] ###Chatten siger vi bare kan slette denne linje, da linjen under overskriver den alligevel, så vi 
#sætter den lige på pause
testdf=readRDS("circMT.rds")

# plots to explore data
myEDAPlots(testdf)
# map-plot to verify flight
myMapPlot(testdf)
par(mfrow=c(1,1))
plot(testdf$lng,testdf$lat)
plot(testdf$time,testdf$crs)

dlatv=diff(testdf$lat)
dlnv=diff(testdf$lng)
dlatv=c(0,dlatv)
dlnv=c(0,dlnv)
testdf$dlat=dlatv
testdf$dlng=dlnv

ggplot(testdf,aes(x=time,y=lat))+geom_line()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=35))


# apply circmetrics
# lm
cmodel=lm(lat ~ lng+crs+alt, data=testdf)
nmodel=lm(lat ~ lng+crs+alt, data=statedf)
summary(cmodel)
summary(nmodel)
# sd
csd=sd(testdf$crs)
nsd=sd(statedf$crs)

# myOwnDetectionAlg
# use diff to track change of fortegn


myMapPlot <- function(df) {
  leaflet(df) %>% 
    addTiles() %>% 
    addPolylines(lat = ~lat, lng = ~lng)
}

myEDAPlots <- function(df) {
  par(mfcol=c(1,3), ann=F)
  plot(df$lat,df$lng)
  plot(df$crs,df$time)
  plot(df$alt,df$time)
}

