#' filter_decimalsXY
#'
#' This function stimate how many decimal places have each case lat/long coordiante and remove imprecise locations without decimal places.
#'
#' @param DF  Data.frame is a data.frame containing at least longitude and latitude
#' @param x is a vector with longitude
#' @param y is a vector with latitude
#'
#' @return A data.frame containing the records considered correct.
#' @export
#' @examples 
#' my.df<-data.frame(decimallatitude=c(34.96388, 34.75000, 21.11120), decimallongitude=c(33.66284, 32.63333, 33.25000))
#' prueba<-filter_decimalsXY(DF=my.df,x="decimallatitude", y="decimallongitude")

filter_decimalsXY <- function(x,y,DF) { 
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
  rm_col <- c("x_nchr", "y_nchr")
  DF<-DF[, !(colnames(DF) %in% rm_col), drop = FALSE]
  return(DF)
} 
