#Outputs from this function are shapefiles, one for  each unique level identified by "splitname"
#shapefile is the name of the shapefile without ".shp" that will be subjected to splitting.
#splitname identify the attribute used for splitting.
#data source name is a folder)

ShpByAttributes = function(shapefile, splitname, dsn) {
  SPLIT <- unique(shapefile[[splitname]])
  for (i in 1:length(SPLIT)) {
    shp <- shapefile[shapefile[[splitname]] == SPLIT[i], ]
    writeOGR(shp, dsn=dsn, SPLIT[i], driver="ESRI Shapefile",
             overwrite_layer=TRUE)
  }
}
