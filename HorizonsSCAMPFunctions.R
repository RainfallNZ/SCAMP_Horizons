#' A function to use spatial data source of land use to look-up leach rates based on a look up table. The processing is carried out at a 250 m x 250 m grid scale.
#'
#'This function generates a raster object of leach rates
#'
#'@param LanduseData A spatial data file (in ESRI polygon shapefile format) of the land use, as used by Horizons. Prepared by Rainfall.NZ. 
#'Must include an attribute called "Type" and "Zone_Code"
#'@param LeachRateData A csv file with attributes: "Type";"TN Loss Rate" and "TP Loss Rate".
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of leach rates
#'@keywords SCAMP
#'@export
SCAMPLeachRateRasterCreator <- function(LanduseData=SubZoneLanduseLUCShapeFileName,
                                   LeachRateData = LeachRateLUTFile){

  #Load lookup table of leaching rates based on land use
  LeachRateLookUpTable<- read.csv(LeachRateData)

  #Load the spatial data with the landuse and sub-management zones 
  SubZoneLanduseLUCSpatial <- readOGR(dsn=dirname(LanduseData),layer=basename(LanduseData),stringsAsFactors = FALSE)
  SubZoneLanduseLUCSpatial <- spTransform(SubZoneLanduseLUCSpatial,CRS("+init=epsg:2193") )
  
  #Add "TN" and "TP" loss rates
  SubZoneLanduseLUCSpatial$TN <- LeachRateLookUpTable$TN.Loss.Rate[match(SubZoneLanduseLUCSpatial$Type,LeachRateLookUpTable$Type)]
  SubZoneLanduseLUCSpatial$TP <- LeachRateLookUpTable$TP.Loss.Rate[match(SubZoneLanduseLUCSpatial$Type,LeachRateLookUpTable$Type)]
  
  #Convert to raster, note the creation of a base raster, which all subsequent raster's align to
  #Also need to fill holes as 0. These are an artefact from the LRI slope data which doesn't exist for large water areas.
  RasterBase <- raster(resolution = 250, ext = extent(SubZoneLanduseLUCSpatial), crs = proj4string(SubZoneLanduseLUCSpatial) )
  TNRaster <- rasterize(SubZoneLanduseLUCSpatial,RasterBase,"TN")
  TNRaster <- focal(TNRaster,w=matrix(1,5,5),fun=function(x,...){if (sum(is.na(x)) > 10) NA else 0},NAonly=TRUE)
  
  TPRaster <- rasterize(SubZoneLanduseLUCSpatial,RasterBase,"TP")
  TPRaster <- focal(TPRaster,w=matrix(1,5,5),fun=function(x,...){if (sum(is.na(x)) > 10) NA else 0},NAonly=TRUE)
  LossRateRaster <- brick(TNRaster,TPRaster)
  return(LossRateRaster)
}

#' A function to  rasterize the zone-landuse data.
#'
#'This function generates a raster object of the zone-landuse data
#'
#'@param ZoneLandusePolygons A polygon spatial data file of the water management zone - land use - LUC intersection.
#'@param LeachRates A raster of leach rates. The output from the SCAMPLeachRateRasterCreator function. Used to align the output raster
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster of the zone-landuse combination
#'@keywords REC River Environment Classification CASM
#'@export

SCAMPZoneLanduseRasterCreator <- function(ZoneLandusePolygons=SubZoneLanduseSpatial,LeachRates=LeachRateRaster){
  
  #Convert the "ZoneLndUse" name into levels
  ZoneLandusePolygons$ZoneLndUse <- as.factor(ZoneLandusePolygons$ZoneLndUse)
  
  #Create the raster, and aligns to the Leach rate raster
  ZoneLanduseRaster <- rasterize(ZoneLandusePolygons,LeachRates,"ZoneLndUse")
  NoOfLevels <- length(levels(ZoneLandusePolygons$ZoneLndUse))
  levels(ZoneLanduseRaster) <- data.frame(ID=seq(1:NoOfLevels),Type=levels(ZoneLandusePolygons$ZoneLndUse))

  return(ZoneLanduseRaster)
}


#' A function to associate the leach rate with each zone-landuse combination and find the average leach rate and area for each combination.
#'
#'@param ZoneLanduseRaster A raster of the water management zone - land use intersection.
#'@param LeachRates A raster of leach rates. The output from the SCAMPLeachRateRasterCreator function.
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A data frame of average leach rates for each water management zone-landuse combination
#'@keywords REC River Environment Classification CASM
#'@export

SCAMPDiffuseLoadTableCreator <- function(ZoneLanduseRaster=ZoneLanduseRaster,
                                         LeachRates=LeachRateRaster){
  
  #get all the leach rate values for all grid cells, and all the combined class names for all the cells
  RasterData <- data.frame(TN = values(LeachRates[[1]]), TP = values(LeachRates[[2]]),CombinedClassNameLevel = values(ZoneLanduseRaster))
  RasterData <- RasterData[complete.cases(RasterData$CombinedClassNameLevel),]
  RasterData$TN[is.na(RasterData$TN)] <- min(RasterData$TN,na.rm = TRUE)
  RasterData$TP[is.na(RasterData$TP)] <- min(RasterData$TP,na.rm = TRUE)
  
  #I can then count the number of cells in each combined-class, and figure out the total area
  CellAreaHectares <- prod(res(LeachRates)) / 10000
  
  #Get the mean leach rate for each combined-class level
  #I don't need to get area weighted mean because all cells are the same size
  
  ClassSummaries <- ddply(RasterData,.(CombinedClassNameLevel), function(x) c(TN =round(mean(x$TN),1),TP =round(mean(x$TP),1), count =  length(x$TN), Hectares = length(x$TN) * CellAreaHectares))
  
  ClassSummaries$CombinedClassName <- ZoneLanduseRaster@data@attributes[[1]][ClassSummaries$CombinedClassNameLevel,2]
  return(ClassSummaries)
  
}

#' A function to reformat CASM input data into SCAMP input data
#'
#'[CASMToSCAMP()]  Takes the data frame 'DiffuseInputsSiteExtendedTable' generated by 
#'SouthlandCASMPreProcess.Rmd' and creates three data.frames ready for transfer 
#'to an excel spreadsheet in the format specified in 'SCAMP input files for Tim K.xlsx'
#'These dataframes describe "Catchment Physical Inputs" and "Catchment WQ Inputs TN" "and Catchment WQ Inputs TP".
#'SCAMP is a revision of CASM. The new tables divide the original into physical and nutrient properties
#'and have a wide, rather than a long format based on land use.
#'@param CASMData A data frame formatted ready for CASM diffuse input. Intended 
#'to be the DiffuseInputsSiteExtendedTable data frame generated by "HorizonsSCAMPPreProcess.Rmd
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return Three data frames
#'@keywords REC River Environment Classification
#'@export
CASMToSCAMP <- function(CASMData = DiffuseInputsSiteExtendedTable){
  
  #Load libraries
  if (!require(tidyr)) install.packages('tidyr'); library(tidyr)
  
  #Parse the Node name to extract the land use
  #CASMData$Landuse <- sub("^.*-","",CASMData$'Node Name')
  
  #Create a "Catchment Name" made up of the nzsegment and the Site name
  CASMData <- dplyr::rename(CASMData,'Catchment Name' = Zone_Code,SCAMPBasin = Parent_Cat)
  #CASMData$'Catchment Name' <- paste(CASMData$nzsegment,CASMData$`Site Name or No.`,sep="-")
  
  #Create the area distribution table
  #Go from long to wide three times using the Land use variables.
  AreaTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='Land Area (ha)'))
  
  Landuses <- names(AreaTable)[-1]
  
  #Replace NA with 0 for the Landuse values
  AreaTable[,Landuses][is.na(AreaTable[,Landuses])] <- 0
  
  #Calculate total land in each node area
  AreaTable[,'Drainage Area (ha)'] <- rowSums(AreaTable[,Landuses],na.rm=TRUE)
  
  #Convert the landuse into percentages
  AreaTable[,Landuses] <- t(
    apply(AreaTable[,c(Landuses,'Drainage Area (ha)')], 1, function(x) {
      x <- x[Landuses] / x['Drainage Area (ha)'] * 100
      x
    }))
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream location
  AreaTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(AreaTable$'Catchment Name', CASMData$'Catchment Name')]
  AreaTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(AreaTable$'Catchment Name', CASMData$'Catchment Name')]
  AreaTable$'SCAMPBasin' <- CASMData$'SCAMPBasin'[match(AreaTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Re-order the columns to match the SCAMP specification
  AreaTable <- AreaTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)','Drainage Area (ha)',
                            "CON","FOR","DAI","SBD","HORT","VEG","URB","TRAN","HYDR","LIF","OAN","ARA","OTH","UKN")]
  AreaTable <- AreaTable[order(AreaTable$SCAMPBasin,AreaTable$'Receiving Stream Name'),]
  
  #create an N Export Coefficients table
  NTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='TN Export Coeff (kg/ha/yr)'))
  
  Landuses <- names(NTable)[-1]
  
  #Replace NA with 0 for the Landuse values
  NTable[,Landuses][is.na(NTable[,Landuses])] <- 0
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream loaction
  NTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  NTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  #NTable$'Diffuse Path Attenuation Coefficient' <- CASMData$'TN Physiographic-based attenuation scale estimate'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  NTable$'SCAMPBasin' <- CASMData$'SCAMPBasin'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Re-order the columns to match the SCAMP specification
  NTable <- NTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)',
                      "CON","FOR","DAI","SBD","HORT","VEG","URB","TRAN","HYDR","LIF","OAN","ARA","OTH","UKN")]
  NTable <- NTable[order(NTable$SCAMPBasin,NTable$'Receiving Stream Name'),]
  
  #create a P Export coefficients table
  PTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='TP Export Coeff (kg/ha/yr)'))
  
  Landuses <- names(PTable)[-1]
  
  #Replace NA with 0 for the Landuse values
  PTable[,Landuses][is.na(PTable[,Landuses])] <- 0
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream loaction
  PTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  PTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  #PTable$'Diffuse Path Attenuation Coefficient' <- CASMData$'TP Physiographic-based attenuation scale estimate'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  PTable$'SCAMPBasin' <- CASMData$'SCAMPBasin'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Re-order the columns to match the SCAMP specification
  PTable <- PTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)',
                      "CON","FOR","DAI","SBD","HORT","VEG","URB","TRAN","HYDR","LIF","OAN","ARA","OTH","UKN")]
  PTable <- PTable[order(PTable$SCAMPBasin,PTable$'Receiving Stream Name'),]
  
  return(list(AreaTable = AreaTable, NTable = NTable, PTable = PTable))
  
}