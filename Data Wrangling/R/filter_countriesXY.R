#' filter_countriesXY
#'
#' Identify and removes mismatches between geographic coordinates and their reported country
#'
#' @param my.df Data.frame with at least three columns which contains the longitude and latitude in degrees and the ISO3 country code
#' @param longitude.name The name of the column which contains the longitude in degrees
#' @param latitude.name The name of the column which contains the latitude in degrees
#' @param countrycode.name The name of the column which contains ISO3 country code code
#'
#' @return A data.frame containing the records considered correct. Records with NA as countrycode are not removed
#' @export
#' @examples 
#' my.df<-data.frame(decimallatitude=c(34.96388, 34.75000, 21.11120), decimallongitude=c(33.66284, 32.63333, 33.25000),  
#' countrycode=c("CYP","CYP","CYP"))
#' prueba<-filter_countriesXY(my.df,latitude.name="decimallatitude", longitude.name="decimallongitude", countrycode.name="countrycode")

filter_countriesXY = function(my.df, longitude.name, latitude.name, countrycode.name)
{  
require(sp)
require(rworldmap) #low resolution map
require(rworldxtra)#High resolution map
require(dplyr)

pts<-my.df[,c(longitude.name,latitude.name)]

  countriesSP <- getMap(resolution='high') 

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap   (WGS84)
  pointsSP = SpatialPoints(pts, proj4string=CRS(proj4string(countriesSP)))  

    # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
    #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  indices$ISO3 # returns the ISO3 code 
  my.df$iso3<- indices$ISO3
  flag<- ifelse(is.na(my.df[,countrycode.name]) | as.character(my.df[,countrycode.name])==as.character(my.df$iso3), "YES", "NO")
my.df$flag<-flag
my.df2<-my.df %>% dplyr::filter(flag %in% "YES")
my.df2<-dplyr::select(my.df2, -c(flag,iso3))
dif<-nrow(my.df)-nrow(my.df2)
message(sprintf("Removed %s records.", dif))
my.df2
#id<-my.df2$uniqID
}

######Example
#my.df<-data.frame(decimallatitude=c(34.96388, 34.75000, 21.11120), decimallongitude=c(33.66284, 32.63333, 33.25000), countrycode=c("CYP","CYP","CYP"))
#prueba<-filter_countriesXY(my.df,latitude.name="decimallatitude", longitude.name="decimallongitude", countrycode.name="countrycode" )
