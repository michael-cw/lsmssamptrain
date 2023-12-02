#' Function for processing the population data
#'
#'
#' @keywords internal
#' @noRd


create.pop.hh<-function(population){
  ###Use hhsize for initial map
  lon.hh<-population[, .(lon=median(lon, na.rm=T)), by = hhidg]
  data.table::setkey(lon.hh, hhidg)
  names(lon.hh)<-c("hhidg","lon.hh")

  lat.hh<-population[, .(lat=median(lat, na.rm=T)), by = hhidg]
  data.table::setkey(lat.hh, hhidg)
  names(lat.hh)<-c("hhidg","lat.hh")
  population<-data.table::setkey(population, hhidg)
  population<-population[lon.hh, nomatch=0]
  population<-population[lat.hh, nomatch=0]
  population[, stratum:=as.factor(stratum)]

  population.hh<-population[, .(countHH=first(countHH), count=first(count),
                                income=mean(income, na.rm=T), lon=mean(lon, na.rm=T),
                                lat=median(lat, na.rm=T), hhsize=median(hhsize, na.rm=T),
                                lon.hh=first(lon.hh), lat.hh=first(lat.hh)),
                            by = .(stratum, cluster, hhidg)]
  data.table::setkey(population.hh, hhidg)

  population.hh[,cluster:=as.factor(cluster)]
  population.hh[,hhsize:=as.numeric(hhsize)]
  population.hh[,count:=as.numeric(count)]
  return(population.hh)

}


#' Function to calculate the distance between two points on a sphere
#'
#' @keywords internal
#' @noRd

hav_dist <- function(lon1, lat1, lon2=38.7578, lat2=8.9806) {
  deg2rad <- function(deg) return(deg*pi/180)
  long1<-deg2rad(lon1)
  lat1<-deg2rad(lat1)
  long2<-deg2rad(lon2)
  lat2<-deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

