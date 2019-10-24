#A function to stimate how many decimal places have each case lat/long coordiante and to remove cases without decimal places

filter_decimals = function(x,y,DF) { #x is a vector with longitude, y is a vector with latitud, DF is a data.frame
  require(magrittr)
  require(dplyr)
 
  #Longitude
  x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  x_int = floor(x) %>% abs() %>% nchar()
  x_nchr = x_nchr - 1 - x_int
  x_nchr[x_nchr < 0] = 0
  DF$x_nchr<-x_nchr
  #Latitude
  y_nchr = y %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  y_int = floor(y) %>% abs() %>% nchar()
  y_nchr = y_nchr - 1 - y_int
  y_nchr[y_nchr < 0] = 0
  DF$y_nchr <- y_nchr
  DF<-filter(DF, y_nchr!=0 & x_nchr!=0)
  return(DF)
} 
