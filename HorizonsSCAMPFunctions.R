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
#'@keywords REC River Environment Classification SCAMP
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
#'@keywords REC River Environment Classification SCAMP
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
#'@return a list of Three data frames
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
  AreaTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='Land Area (ha)',values_fn=first,values_fill = 0))
  
  Landuses <- names(AreaTable)[-1]
  
  #Replace NA with 0 for the Landuse values
  #AreaTable[,Landuses][is.na(AreaTable[,Landuses])] <- 0
  
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
  
  #Add any missing columns
  RequiredLandUses <- c("CON","FOR","DAI","SBD","HORT","VEG","URB","TRAN","HYDR","LIF","OAN","ARA","OTH","UKN")
  RequiredNames <- c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)','Drainage Area (ha)',
                     RequiredLandUses)
  MissingNames <- which(!RequiredNames  %in% names(AreaTable))
  AreaTable[,RequiredNames[MissingNames]] <- 0
  
  #Re-order the columns to match the SCAMP specification
  AreaTable <- AreaTable[,RequiredNames]
  AreaTable <- AreaTable[order(AreaTable$SCAMPBasin,AreaTable$'Receiving Stream Name',AreaTable$'Receiving Stream Location (km)'),]

  #create an N Export Coefficients table
  NTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='TN Export Coeff (kg/ha/yr)',values_fn = first, values_fill = 0 ))
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream location
  NTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  NTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  #NTable$'Diffuse Path Attenuation Coefficient' <- CASMData$'TN Physiographic-based attenuation scale estimate'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  NTable$'SCAMPBasin' <- CASMData$'SCAMPBasin'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Add in any missing land use column names
  MissingNames <- which(!RequiredLandUses  %in% names(NTable))
  NTable[,RequiredLandUses[MissingNames]] <- 0
  
  #Re-order the columns to match the SCAMP specification
  NTable <- NTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)',
                      "CON","FOR","DAI","SBD","HORT","VEG","URB","TRAN","HYDR","LIF","OAN","ARA","OTH","UKN")]
  NTable <- NTable[order(NTable$SCAMPBasin,NTable$'Receiving Stream Name',NTable$'Receiving Stream Location (km)'),]
  
  #create a P Export coefficients table
  PTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='TP Export Coeff (kg/ha/yr)',values_fn = first, values_fill = 0 ))
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream loaction
  PTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  PTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  #PTable$'Diffuse Path Attenuation Coefficient' <- CASMData$'TP Physiographic-based attenuation scale estimate'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  PTable$'SCAMPBasin' <- CASMData$'SCAMPBasin'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Add in any missing land use column names
  MissingNames <- which(!RequiredLandUses  %in% names(PTable))
  PTable[,RequiredLandUses[MissingNames]] <- 0
  
  #Re-order the columns to match the SCAMP specification
  PTable <- PTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)',
                      "CON","FOR","DAI","SBD","HORT","VEG","URB","TRAN","HYDR","LIF","OAN","ARA","OTH","UKN")]
  PTable <- PTable[order(PTable$SCAMPBasin,PTable$'Receiving Stream Name',PTable$'Receiving Stream Location (km)'),]
  
  return(list(AreaTable = AreaTable, NTable = NTable, PTable = PTable))
  
}

#' Label stream reaches
#' 
#' This function names reach segments with real-world names.
#' Some names are looked up using the SCAMPRECReachNamer() function. These
#' names need to be manually checked.
#' The rest are manually named using the SCAMPNetworkNamer() function.
#' @param RECV2Network a spatial river network
#' @param ExtraRECReachLabelsFile A two column csv file of RECV2 nzsegment numbers 
#' and related names. Columns must be called "nzsegment" and "name" respectively
#' @param EstuarySites A dataframe that must include a column of nzsegment numbers 
#' and of Estuary names. Columns must be called "nzsegment" and "Name" respectively.
#' @return a labelled version of the spatial river network

SCAMPNetworkLabeller <- function(RECV2Network = CompleteSpatialNetwork,
                            ExtraRECReachLabelsFile = NA,
                            EstuarySites = NA){
  #Name as much as possible from the LINZ river naming spatial data sets

  NamedCompleteSpatialNetwork <- SCAMPRECReachNamer(RECV2Network = RECV2Network)
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(NamedCompleteSpatialNetwork)[which(names(NamedCompleteSpatialNetwork) == "nzsgmnt")] <- "nzsegment"
  
  #Talk amongst yourselves while this layer is manually checked in QGIS
  #st_write(NamedCompleteSpatialNetwork, "CASM-StreamNetworkForNaming", driver="ESRI Shapefile",overwrite_layer=TRUE)

  #If a manual override name file is specified, use it to update the names
  if (!is.na(ExtraRECReachLabelsFile)){
    #Load in the csv file with a direct nzsegment-to-name relationship (manually prepared!)
    ExtraRECReachLabels <- read.table(ExtraRECReachLabelsFile,sep=",",stringsAsFactors = FALSE, header=TRUE)
    #Find which reach numbers need updating
    IndexOfNamesToUpdate <- which(NamedCompleteSpatialNetwork$nzsegment %in% ExtraRECReachLabels$nzsegment)
    #Then update them
    NamedCompleteSpatialNetwork$name[IndexOfNamesToUpdate] <- ExtraRECReachLabels$name[match(NamedCompleteSpatialNetwork$nzsegment[IndexOfNamesToUpdate],ExtraRECReachLabels$nzsegment)]
  }
  #If an estuary naming dataframe is specified, use it to update any reaches that don't already have a name (useful for all the outlet reaches)

  if (!is.na(EstuarySites)){
    #Find which of the estuary reach numbers need updating
    EstuaryReachesToBeUpdated <- which(NamedCompleteSpatialNetwork$nzsegment %in% EstuarySites$nzsegment & NamedCompleteSpatialNetwork$name %in% c("TBC",NA))
    #Then update them
    NamedCompleteSpatialNetwork$name[EstuaryReachesToBeUpdated] <- EstuarySites$Name[match(NamedCompleteSpatialNetwork$nzsegment[EstuaryReachesToBeUpdated],EstuarySites$nzsegment)]
  }
  #Name all remaining reaches by checking  the name immediately downstream or by getting manual input
  NetworkLabelList <- SCAMPNetworkNamer(RECNetwork = NamedCompleteSpatialNetwork)
  
  #Add the tributary labels to the network
  #SegmentToLabelLookUpTable <- do.call(rbind,NetworkLabelList)
  #RECV2Network@data$Label <- SegmentToLabelLookUpTable$name[match(RECV2Network@data$nzsegment,SegmentToLabelLookUpTable$nzsegment)]
  return(NetworkLabelList)
}


#' A function to name REC reaches
#'
#'This function accepts a vector of RECV2 river network as a spatial data frame and does it's best to name the reaches through using the \href{https://data.linz.govt.nz/layer/103631-nz-river-name-polygons-pilot/}{LINZ name polygons} and \href{https://data.linz.govt.nz/layer/103632-nz-river-name-lines-pilot/}{LINZ name lines} data
#'
#'@param RECV2Network A spatial line object that needs to be named. Either a spatial data frame, or a simple feature object of the REC network to be named
#'@param LINZNameLines The LINZ name line spatial data frame that  covers the area of interest
#'@param LINZNamePolygons The LINZ name polygon spatial data frame that covers the area of interest
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A copy of the input REC network with an additional attribute called "Name"
#'@keywords REC LINZ river names
#'@export
SCAMPRECReachNamer <- function(RECV2Network, LINZNameLinesFile="D:\\Projects\\LWP\\Horizons\\SCAMP\\Data\\GIS\\LINZRiverNames\\nz-river-name-lines-pilot.shp",
                          LINZNamePolygonsFile="D:\\Projects\\LWP\\Horizons\\SCAMP\\Data\\GIS\\LINZRiverNames\\nz-river-name-polygons-pilot.shp" ){
  
  if (!require(sf)) install.packages("sf"); library(sf)                         # simple features packages for handling vector GIS data
  if (!require(httr)) install.packages("httr"); library(httr)                   # generic webservice package
  if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)    # a suite of packages for data wrangling, transformation, plotting, ...
  if (!require(data.table)) install.packages("data.table"); library(data.table) # useful for summarising data
  
  LINZNameLines <- st_read(dsn =LINZNameLinesFile, stringsAsFactors = FALSE)
  LINZNamePolygons <- st_read(dsn =LINZNamePolygonsFile, stringsAsFactors = FALSE)
  
  if(class(RECV2Network)[1] != "sf")  RECV2Network <- st_as_sf(RECV2Network)
  RECV2Network <- st_transform(RECV2Network, crs=st_crs(LINZNameLines))
  
  #Undertake a spatial join between the REC network and the river name line data
  NamedNetwork <- RECV2Network %>% 
    st_join(LINZNamePolygons[,"name"], largest = TRUE ) %>% 
    #st_transform(st_crs(RECV2Network)) %>%                  #This is needed as some of the projection details get lost in the join
    st_join(LINZNameLines[,"name"], suffixes =c(".polygons",".lines"),largest = TRUE )
  
  #Default assumption is that the line name is correct
  NamedNetwork$name <- NamedNetwork$name.x
  NamedNetwork$name[is.na(NamedNetwork$name.x)] <- NamedNetwork$name.y[is.na(NamedNetwork$name.x)]
  
  #For reaches that get named from both the polygon names and the line names, make a sensible decision based on connectivity
  #Find the features with a name clash. This can happen where the REC network is not a perfect match to the LINZ network, or on first reaches of tributaries of a polygon river
  NameClashIndices <- which(NamedNetwork$name.x != NamedNetwork$name.y)
  
  #Work through the name clashes one by one and attempt to fix them.
  #The rules for name allocation are (applied sequentially, so in reverse order of priority):
  #1/ The name is "TBC" indicating it needs to be confirmed manually
  #2/ If an immediately upstream reach is named the same as the downstream reach (and they don't also have clashes) then this is the name to use
  #3/ If a sibling reach has the same name as the downstream reach (and they don't also have name clashes) then the name is the choice that is NOT the downstream name
  
  ClashNames <- sapply(NameClashIndices, function(NameClashIndex){
    
    #Get the possible names
    Names <- NamedNetwork %>% st_drop_geometry() %>% dplyr::select(name.x, name.y) %>% slice(NameClashIndex) %>% unlist()
    
    #Other clash indices except this one
    OtherClashIndices <- NameClashIndices[NameClashIndices != NameClashIndex]
    
    #Find the name of the reach immediately downstream
    DownstreamName <- NamedNetwork$name[NamedNetwork$FROM_NODE == NamedNetwork$TO_NODE[NameClashIndex]]
    
    #Find the upstream names
    UpstreamNames <- NamedNetwork$name[NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[NameClashIndex]]
    
    #Find the name of any sibling tributaries (i.e. that connect at the downstream end of this reach)
    SiblingIndices <- which(NamedNetwork$TO_NODE == NamedNetwork$TO_NODE[NameClashIndex])
    SiblingName <- NamedNetwork$name[SiblingIndices[SiblingIndices != NameClashIndex]]
    #SiblingName <- NamedNetwork$name[NamedNetwork$TO_NODE == NamedNetwork$TO_NODE[NameClashIndex]]
    
    #Check for sequential clashes, as this indicates manual intervention is required 
    #Check if the reach immediately downstream has a name clash
    DownstreamClash <- which(NamedNetwork$FROM_NODE == NamedNetwork$TO_NODE[NameClashIndex]) %in% OtherClashIndices
    
    #Check if any of the immediately upstream reaches also have a name clash
    UpstreamClash <- any(which(NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[NameClashIndex]) %in% OtherClashIndices)
    
    #Check if any of the sibling tributaries have a name clash
    SiblingClash <- any(which(NamedNetwork$TO_NODE == NamedNetwork$TO_NODE[NameClashIndex]) %in% OtherClashIndices)
    
    #Rule 1
    Name <- "TBC"
    
    # Rule 2
    #If an upstream and downstream names are the same
    #I need to "sum" the condition in case some of the arguments don't exist
    if (sum(DownstreamName %in% UpstreamNames & !UpstreamClash & !DownstreamClash & !is.na(DownstreamName))) Name <- DownstreamName
    
    #Rule 3
    #If a sibling has the same name as the downstream name, then remove that name from the options and allocate the other name.
    #I need to "sum" the condition in case some of the arguments don't exist
    if (sum(DownstreamName %in% SiblingName & !SiblingClash & !DownstreamClash & !is.na(SiblingName))) Name <- Names[which(Names != SiblingName)]
    
    return(Name)
  })
  
  if(length(NameClashIndices) > 0) NamedNetwork$name[NameClashIndices] <- ClashNames  
  
  #Find siblings named the same and resolve them
  SiblingToNodes <- unique(NamedNetwork$TO_NODE[which(duplicated(NamedNetwork$TO_NODE))])
  
  #Loop through the siblings and correct them where possible, and allocating a "TBC" to the name where not.
  #The rules are
  #1/ If the named reaches immediately upstream of the siblings have different names, and one of them matches the 
  # sibling name and the downstream name (if it exists), then rename the other sibling to match the other upstream name
  #2/ If one of the upstream names is not known, rename its sibling as "TBC"
  
  for (SiblingToNode in SiblingToNodes) {
    
    #Find the cases where the names for siblings are the same. Do this by testing to see if there is only one unique name in the sibling name set
    SiblingNames <- NamedNetwork$name[NamedNetwork$TO_NODE == SiblingToNode]
    SingleSiblingName <- length(unique(SiblingNames)) == 1
    
    if(SingleSiblingName & all(!is.na(SiblingNames))) {
      
      #Get the indices of the siblings
      SiblingIndices <- which(NamedNetwork$TO_NODE == SiblingToNode)
      
      SiblingName <- NamedNetwork$name[SiblingIndices[1]]
      
      #Check the downstream name
      DownstreamName <- NamedNetwork$name[NamedNetwork$FROM_NODE == SiblingToNode]
      if(length(DownstreamName) == 0)DownstreamName <- NA
      
      #Get the upstream name of both siblings
      UpStreamNames <- sapply(SiblingIndices, function(SiblingIndex) {
        NamedNetwork$name[which(NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[SiblingIndex])]
      })
      
      
      #If the upstream name of each sibling is different, and one of them matches the sibling name and the downstream name (if it exists), then
      #change the name of one of the siblings.
      #browser()
      if(length(unique(UpStreamNames)) > 1 & SiblingName %in% UpStreamNames & (SiblingName == DownstreamName | sum(!is.na(DownstreamName)) == 0)) {
        
        #Figure out which sibling to change. The one with the upstream name that doesn't match the duplicated sibling name
        SiblingToChange <- which(!UpStreamNames %in% SiblingName)
        
        #Figure out what the new name should be
        NewSiblingName <- unlist(UpStreamNames[SiblingToChange])
        if(length(NewSiblingName) == 0 | sum(!is.na(NewSiblingName))==0) NewSiblingName <- "TBC"
        
        #Rename it
        NamedNetwork$name[SiblingIndices[SiblingToChange]] <- NewSiblingName
      } #end of renaming if condition
    } #end of renaming test if condition
  } #end of for loop
  
  
  #Check for gaps in naming
  #In some cases a name is missing even though the reaches immediately upstream and downstream have names. In these cases, allocating a name to the unnamed reach can be done
  #Start by identifying all the reaches that do not have a name
  UnnamedIndices <- which(is.na(NamedNetwork$name))
  #work through them and name them if possible
  for (UnnamedIndex in UnnamedIndices) {
    #Get the Upstream name(s)
    UpstreamName <- NamedNetwork$name[NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[UnnamedIndex]]
    
    #Get the downstream name
    DownstreamName <- NamedNetwork$name[NamedNetwork$FROM_NODE == NamedNetwork$TO_NODE[UnnamedIndex]]
    
    #rename the reach of interest if one of the upstream reaches has the same name as the downstream reach
    if(sum(DownstreamName %in% UpstreamName & (sum(!is.na(DownstreamName)) != 0)) == 1) NamedNetwork$name[UnnamedIndex] <- DownstreamName
  }
  return(NamedNetwork)
}


#' A function to name an REC network based on available names
#'
#'This function crawls up an REC network and names unnamed reaches. It starts from the outlet of a network and works up to the next branch, naming each unnamed reach with the same name as the immediately downstream name. If the start of a branch is not named it requests a name from user input. It uses the REC V2.4 network. See \href{https://niwa.co.nz/freshwater-and-estuaries/management-tools/river-environment-classification-0}{NIWA REC V2} for details about REC V2.
#'@param RECNetwork An REC V2 network (either dataframe of spatial dataframe), with at least nzsegment, TO_NODE, FROM_NO and hdw_dst attributes
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A list (one for each each independent network/diconnected catchment)) of vectors of reach names, and a side effect of a file called "OutletReaches.csv" listing the nzsegment attributes of the reaches that are considered to be at the bottom of the network.
#'@keywords REC River Environment Classification
#'@export
SCAMPNetworkNamer <- function(RECNetwork=MyREC){
  
  #Load libraries
  if (!require(svDialogs)) install.packages('svDialogs'); library(svDialogs)     #Enables dialog boxes
  
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(RECNetwork)[which(names(RECNetwork) == "nzsgmnt")] <- "nzsegment"
  names(RECNetwork)[which(names(RECNetwork) == "FROM_NO")] <- "FROM_NODE"
  names(RECNetwork)[which(names(RECNetwork) == "hdw_dst")] <- "headw_dist"
  names(RECNetwork)[which(names(RECNetwork) == "LENGTHD")] <- "LENGTHDOWN"
  names(RECNetwork)[which(names(RECNetwork) == "Headwtr")] <- "Headwater"
  
  #Find the row indices of all the outlet reach's by finding which "to" nodes  don't have a corresponding "from" node
  OutletReachIndices <- which(!RECNetwork$TO_NODE %in% RECNetwork$FROM_NODE)
  #names(OutletReachIndices) <- 1:length(OutletReachIndices)

  #Save the outlet reaches to an external file so that we can figure out their names manually
  #write.table(RECNetwork@data$nzsegment[OutletReachIndices],file.path(DataDirectory,"OutletReaches.csv"),sep=",",row.names = FALSE)
  
  #Crawl each network in turn
  ReachNames <- lapply(OutletReachIndices, function(OutletReachIndex) {
    
    #Initialise the list of name details
    RowNumber          <- 1
    CurrentName       <- RECNetwork$name[OutletReachIndex]
    if(CurrentName %in% c("TBC",NA)) {     #get a name from the audience
      RECNetwork$name[OutletReachIndex] <- dlg_input(message = paste("Reach Name for nzsegment",RECNetwork$nzsegment[OutletReachIndex]) , default = "", gui = .GUI)$res
      CurrentName <- RECNetwork$name[OutletReachIndex]
    }  
    
    names             <- data.frame(nzsegment=as.numeric(NA),name=as.character(NA),stringsAsFactors = FALSE)
    names[RowNumber,] <- list(RECNetwork$nzsegment[OutletReachIndex],CurrentName)
    
    CurrentReachIndex  <- OutletReachIndex
    upstream_indices   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NODE[CurrentReachIndex])
    LeftToDoIndices    <- upstream_indices
    
    while (length(LeftToDoIndices) > 0) {
      
      CurrentReachIndex  <- upstream_indices[1]
      LeftToDoIndices    <- LeftToDoIndices[LeftToDoIndices != CurrentReachIndex]
      RowNumber          <- RowNumber + 1
      #if(RECNetwork$nzsegment[CurrentReachIndex]==15263054) browser()
      IsBranch <- length(upstream_indices) > 1
      #If it is a branch, check that both of the branch reaches have a name
      if(IsBranch){
        if(RECNetwork$name[upstream_indices[1]] %in% c("TBC",NA)) {#get a name from the audience
          #Update the name in the network data
          RECNetwork$name[upstream_indices[1]] <- dlg_input(message = paste("Reach Name for nzsegment",RECNetwork$nzsegment[upstream_indices[1]]) , default = "", gui = .GUI)$res
          #And update the name being collected for the nzsegment to name table
          #CurrentName <- RECNetwork$name[CurrentReachIndex]
        }
        #Check the name of the other branch
        if(RECNetwork$name[upstream_indices[2]] %in% c("TBC",NA)) {     #get a name from the audience
          #Update the network data as well. Note that the nzsegment to name table does not nee to be updated for this reach here, as it will be crawled later on
          RECNetwork$name[upstream_indices[2]] <- dlg_input(message = paste("Reach Name for nzsegment",RECNetwork$nzsegment[upstream_indices[2]]) , default = "", gui = .GUI)$res
        } 
        #Under the condition it is not a branch , check if the name is missing.
        #If the name is not missing, get the name
      } else if(!RECNetwork$name[CurrentReachIndex] %in% c("TBC",NA)){CurrentName <- RECNetwork$name[CurrentReachIndex]} else 
        #If the name is missing, use the name immediately downstream
      { RECNetwork$name[CurrentReachIndex] <- RECNetwork$name[which(RECNetwork$FROM_NODE==RECNetwork$TO_NODE[CurrentReachIndex])]
      #CurrentName <- RECNetwork$name[which(RECNetwork$FROM_NODE==RECNetwork$TO_NODE[CurrentReachIndex])]
      }
      
      names[RowNumber,] <- list(RECNetwork$nzsegment[CurrentReachIndex],RECNetwork$name[CurrentReachIndex])
      
      upstream_indices   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NODE[CurrentReachIndex])
      
      #Check if there are no more upstream reaches
      if (length(upstream_indices) == 0) {
        upstream_indices <- LeftToDoIndices[1]
      } else {
        LeftToDoIndices <- c(upstream_indices,LeftToDoIndices)
      }
    }
    return(names)
  })
  
  
  return(ReachNames)
}


#' A function to create a SCAMP node location table given an REC network with additional attributes of SCAMP tributary labels and SCAMP tributary distances
#'
#'This function generates a data frame of SCAMP Node names, SCAMP tributary, SCAMP tributary location
#'
#'@param SCAMPRECNetwork An REC V2 network (either dataframe of spatial dataframe), with at least nzsegment, SCAMPTrib and SCAMPTrib_Loc attributes
#'@param SCAMPNodes A dataframe of node names and REC V2 reach number (i.e. nzsegment attribute) of the nodes for which the SCAMP table is to be prepared
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A dataframe of SCAMP Node names, SCAMP tributary, SCAMP tributary location
#'@keywords REC River Environment Classification SCAMP
#'@export
SCAMPNodeTablePreparer <- function(SCAMPRECNetwork=RECReachNetwork, NetworkLabelList = NetworkLabelList, TributaryConnectionTable = TributaryConnectionTable, SCAMPNodes=data.frame(NodeName = c("test","Manawatu at Teachers College"),nzsegment= c(7140020,7239110))){
  #Make sure the nzsegment attribute is correctly named. This is needed because the RECV2 version available from NIWA has altered attribute names (to meet ESRI column naming limitations)
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(SCAMPRECNetwork)[which(names(SCAMPRECNetwork) == "nzsgmnt")] <- "nzsegment"
  names(SCAMPRECNetwork)[which(names(SCAMPRECNetwork) == "hdw_dst")] <- "headw_dist"
  NetworkLabelList <- lapply(NetworkLabelList, function(x) {names(x)[1] = "nzsegment"; return(x)})
  names(SCAMPNodes)[which(names(SCAMPNodes) == "nzsgmnt")] <- "nzsegment"
  names(TributaryConnectionTable)[which(names(TributaryConnectionTable) == "nzsgmnt")] <- "nzsegment"

  #Work through each catchment
  AllTribLocations <- lapply(NetworkLabelList, function(SingleCatchmentNetworkLabels) {
    #browser()
    #Get the catchment name
    #if(exists("OutletReachNames")) {
    CatchmentName <- OutletReachNames$Name[OutletReachNames$nzsegment %in% SingleCatchmentNetworkLabels$nzsegment]
    #} else { CatchmentName = "NoName"}
    print(CatchmentName)
    
    #Find which SCAMPNodes are within the current catchment
    CatchmentNodes <- SCAMPNodes[(SCAMPNodes$nzsegment %in% SingleCatchmentNetworkLabels$nzsegment),]
    
    #Work through all the Nodes that are in this catchment
    if (nrow(CatchmentNodes) > 0) {
      #Now work through the nodes that are within the catchment, find the outlet reach, and then get the distance.
      TribLocations <- sapply(seq_along(CatchmentNodes$NodeName), function(NodeIndex){
        
        SCAMPNode <- CatchmentNodes[NodeIndex,]
        #NodeNumber  <- SingleCatchmentNetworkLabels$Label[SingleCatchmentNetworkLabels$nzsegment == SCAMPNode$nzsegment]
        #NodeTribName <- paste0(CatchmentName,"-",NodeNumber)
        NodeTribName  <- SingleCatchmentNetworkLabels$name[SingleCatchmentNetworkLabels$nzsegment == SCAMPNode$nzsegment]
        
        #Find the headwater distance of the node's reach 
        #TributaryLocations <-  SCAMPRECNetwork$headw_dist[SCAMPRECNetwork$nzsegment == SCAMPNode$nzsegment]
        TributaryLocations <-  SCAMPRECNetwork$TribHeadDist[SCAMPRECNetwork$nzsegment == SCAMPNode$nzsegment]

        Result <- c(nzsegment=SCAMPNode$nzsegment,SCAMPNodeName=as.character(SCAMPNode$NodeName),TribName = NodeTribName,TribLocn = round(TributaryLocations/1000,0))
        return(Result)
      })
    } else {NULL}
    
    
  })
  
  #Turn the catchment-based list into a data frame
  SCAMPNodeTable <- data.frame(t(do.call(cbind,AllTribLocations)))
  #Convert the numbers into numbers
  SCAMPNodeTable$nzsegment <- as.numeric(SCAMPNodeTable$nzsegment)
  SCAMPNodeTable$TribLocn <- as.numeric(SCAMPNodeTable$TribLocn)
  return(SCAMPNodeTable)
}


#' A function to determine where each tributary connects to its parent river
#'
#'This function provides the river network connectivity table used by SCAMP
#'SCAMP understands a network in terms of Tributary Names, Total Length of tributary, Confluence Names and Confluence Locations.
#'Total Length of the tributary is the distance (in kilometres) from the confluence to the end of the tributary 
#'Confluence Name is the name of the river that a tributary flows into.
#'Confluence Location is the distance along the confluence from its start, that the tributary joins it.
#'
#'@param RECNetwork An REC V2 network (either dataframe of spatial dataframe)
#'@param TributaryLabelList A list of tributary labels. One list for each independent/disconnected catchment. The output of the NetworkLabeler() function
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A list of two items: 1:a dataframe. One row for each tributary. Columns of nzsgmnt, tributary name, total length in kilometres, confluence name, confluence location in kilometres. RECNetwork of the SCAMP network with TribHeadDist appended
#'@keywords REC River Environment Classification SCAMP
#'@export
SCAMPTributaryConnectionCreatorV2 <- function(RECNetwork = CompleteSpatialNetwork, TributaryLabelList = TribLabelList){
  
  #Make sure the nzsegment, headw_dist and LENGTHDOWN attributes are correctly named. This is needed because the RECV2 version available from NIWA has altered attribute names (to meet ESRI column naming limitations)
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(RECNetwork)[which(names(RECNetwork) == "nzsgmnt")] <- "nzsegment"
  names(RECNetwork)[which(names(RECNetwork) == "FROM_NO")] <- "FROM_NODE"
  names(RECNetwork)[which(names(RECNetwork) == "hdw_dst")] <- "headw_dist"
  names(RECNetwork)[which(names(RECNetwork) == "LENGTHD")] <- "LENGTHDOWN"
  names(RECNetwork)[which(names(RECNetwork) == "Headwtr")] <- "Headwater"
  
  #Add an attribute to the REC reaches that provides The distance from the current reach to the end of the current named tributary
  for (Reach in RECNetwork$nzsegment) {
    CurrentTrib <- RECNetwork$Label[RECNetwork$nzsegment == Reach]
    #Get all the upstream reaches in the same tributary
    
    CurrentReachIndex <- which(RECNetwork$nzsegment == Reach)
    TribIndices <- CurrentReachIndex
    NextIndex   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NODE[CurrentReachIndex] & RECNetwork$Label == CurrentTrib)
    #browser()
    while (!length(NextIndex) == 0){
      TribIndices    <- c(TribIndices,NextIndex)
      CurrentReachIndex <- NextIndex
      NextIndex   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NODE[CurrentReachIndex] & RECNetwork$Label == CurrentTrib)
    }
    
    #Get the distance upstream in the same tributary
    TribHeadDist <- sum(RECNetwork$Shap_Lng[TribIndices])
    RECNetwork$TribHeadDist[RECNetwork$nzsegment == Reach] <- TribHeadDist
    
  }
  #This function works through "independent catchments" which are defined as networks that are not connected, with their own outlet
  #For each independent catchment, figure out where the tributaries connect to their parent tributary.
  CatchmentTribLinkages <- lapply(seq_along(TributaryLabelList), function(CatchmentIndex) {
    
    #Get the tributary labels for all REC reaches within the current independent catchment
    CatchmentTribLabels <- TributaryLabelList[[CatchmentIndex]]
    
    #For backward compatibility rename "name" to "label"
    names(CatchmentTribLabels)[which(names(CatchmentTribLabels) == "name")] <- "label"
    
    #Find all the unique tributary labels
    UniqueTribs <- unique(CatchmentTribLabels$label)
    
    #for each tributary find the maximum TribHeadDist attribute from the REC data, the REC reach immediately downstream of the tributary, and the tributary label of the REC reach that is immediately downstream. This builds a matrix of 4 numbers for each tributary, giving the minimum LENGTHD, the lowest nzsgmnt, the highest nzsgmnt below the tributary, and the label of the tributary below (i.e. the confluence name)
    AllDistances <- sapply(UniqueTribs, function(TribLabel) {
      
      #if (TribLabel == "Oroua River") browser()
      #Get the REC data for the current tributary
      ReachData <- RECNetwork[RECNetwork$nzsegment %in% CatchmentTribLabels$nzsegment[CatchmentTribLabels$label == TribLabel],]
      
      #Find the length of the tributary from the maximum tributary head distance
      TribConnectionTotalDistance <- max(ReachData$TribHeadDist,na.rm=TRUE)
      
      #Find which reach is the lowest in the tributary, based on the headw_dist attribute
      LowestReach <- ReachData$nzsegment[which.max(ReachData$headw_dist)]
      
      #Find the reach immediately below the lowest reach
      ReachBelow <- RECNetwork$nzsegment[which(RECNetwork$FROM_NODE == RECNetwork$TO_NODE[RECNetwork$nzsegment == LowestReach])]
      

      
      #Special case if it is the lowest tributary, as it is effectively the mainstem
      if(length(ReachBelow)==0) {
        ReachBelow <- LowestReach
        TribBelow <- TribLabel
        TributaryLocations <-  RECNetwork$TribHeadDist[RECNetwork$nzsegment == ReachBelow]
        }
      else {
        #Find the tributary headwater distance of the reach immediately below the tributary confluence and subtract its length
        TributaryLocations <-  RECNetwork$TribHeadDist[RECNetwork$nzsegment == ReachBelow] - RECNetwork$Shap_Lng[RECNetwork$nzsegment == ReachBelow]
        TribBelow <- CatchmentTribLabels$label[CatchmentTribLabels$nzsegment == ReachBelow]
        }
      
      #Find the tributary headwater distance of the reach immediately below the tributary confluence and subtract its length

      #TributaryLocations <-  RECNetwork$TribHeadDist[RECNetwork$nzsegment == ReachBelow] - RECNetwork$Shap_Lng[RECNetwork$nzsegment == ReachBelow]
      
      #Special case where the headwater distance is 0 but it is not a headwater reach which means the data are wrong. This has occurred in at least one situation (nzsegment = 15400003)
      #Move down stream and get the tributary head distance from the reach below
      #if(TributaryLocations == 0 & RECNetwork$Headwater[RECNetwork$nzsegment == ReachBelow] == 0){
      #  #Move down the network one more step
      #  ReachBelow <- RECNetwork$nzsegment[which(RECNetwork$FROM_NODE == RECNetwork$TO_NODE[RECNetwork$nzsegment == ReachBelow])]
      #  #And get the tributary head distance from this reach. This assumes that there are not two reaches in a row missing the headwater distance data
      #  TributaryLocations <-  RECNetwork$TribHeadDist[RECNetwork$nzsegment == ReachBelow] - RECNetwork$Shap_Lng[RECNetwork$nzsegment == ReachBelow]
      #}
      
      return(c(TribConnectionTotalDistance,LowestReach,ReachBelow,TribBelow,TributaryLocations))
    })
    

    #Lastly, just the useful information is retained, and the labels are formatted to distinguish one catchment from another
    #I now want the tributary label, and the distance along the parent tributary, and the parent tributary label
    TributaryDetails <- data.frame("nzsegment"=AllDistances[2,],"Tributary Name" = colnames(AllDistances), "Total Length (km)" = round(as.numeric(AllDistances[1,])/1000,0),"Confluence Stream" = AllDistances[4,], "Confluence Location (km)" = round(as.numeric(AllDistances[5,])/1000,0), check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  #Combine all the catchment information into a single data frame.
  AllCatchments <- do.call(rbind,CatchmentTribLinkages)
  rownames(AllCatchments) <- NULL

  return(list(AllCatchments,RECNetwork))
}



#' A function to combine spatial data sources of land "types" with potential nutrient loss-rate reductions.
#'
#'This function generates a raster object of loss rate reductions
#'
#'The data and loss-rate reductions are derived from McDowell, R.W., Monaghan, R.M., 
#'Smith, C., Manderson, A., Basher, L., Burger, D.F., Laurenson, S., Pletnyakov, P., 
#'Spiekermann, R., Depree, C., 2021. Quantifying contaminant losses to water from 
#'pastoral land uses in New Zealand III. What could be achieved by 2035? New Zealand 
#'Journal of Agricultural Research 64, 390–410. https://doi.org/10.1080/00288233.2020.1844763
#'
#'
#'@param LandTypes A spatial data file of 
#''typologies' as used in McDowell et al (2020). Requires an attribute called 'Typology' 
#'that matches the 'Typology' provided in the MitigationLookupTable. These data are ideally
#'originally sourced from 
#'https://databox.datacomcloud.co.nz/shares/folder/mYouM1LtlEc/
#'@param MitigationLookupTable A dataframe of the values to be mapped for each land 
#''type' (row). The intention was to use data generated from the tables provided in the 
#'McDowell et al (2020) supplementary material, but any Type-to-fraction-reduction 
#'data frame will work as long as the 'types' match the types in McDowell et al. (2020).
#'Ross Monaghan provided expert guidance on loss rates for types not included in McDowell et al. (2020)
#'@param ReferenceRaster A reference raster to align to. Ideally use the loss-rate-raster
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of 
#'@keywords Water Quality, CASM, SCAMP, leach
#'@export
LossRaterReductionRasterCreator <- function(LandTypes=LanduseShapeFile,
                                            MitigationLookUpTable=NA,
                                            ReferenceRaster = LossRateRaster){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing

  #Join the look up table to the spatial data
  LandTypes$LossRateReduction <- MitigationLookUpTable[match(LandTypes$Typology,MitigationLookUpTable$Typology),2]
  
  #Convert the spatial data to raster
  TypologyRaster <- rasterize(LandTypes,ReferenceRaster,"LossRateReduction")
  
  
  return(TypologyRaster)
}


#' A function to adjust the leachrate rasters for the Our Land and Water Mitigation scenarios
#'
#'This function generates a raster object of adjusted leach rates
#'
#'@param LeachRates A raster stack of leach rates. Raster 1 is for N, raster 2 is for P
#'@param MitigationDataFile A csv file of Our Land and Water mitigation loads. This file
#'was generated from the tables provided in the McDowell et al (2020) supplementary material.
#'@param MitigationOfInterest The name of our Land and Water mitigaion scenario to apply.
#'Currently limited to one of "Potential2015", the default, and "Potential2035",  
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster stack of leach rates adjusted by the mitigation scenario 
#'@keywords Water Quality, CASM, SCAMP, leach
#'@export
LeachRateAdjuster <- function(LeachRates=LeachRateRaster,
                              MitigationDataFile = "D:\\Projects\\LWP\\Horizons\\SCAMP\\Data\\OurLandAndWaterMitigationLoadsV2.csv",
                              MitigationOfInterest = "Potential2015",
                              DairyLandTypeSpatialDataFile = "D:\\Projects\\LWP\\Horizons\\SCAMP\\Data\\GIS\\OLW_Dairy_Horizons\\OLW_Dairy_Horizons.shp",
                              SheepAndBeefLandTypeSpatialDataFile = "D:\\Projects\\LWP\\Horizons\\SCAMP\\Data\\GIS\\OLW_SnB_Horizons\\OLW_SnB_Horizons.shp"){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis)                #used for plotting discrete rasters
  if (!require(sf)) install.packages("sf"); library(sf)                #used for spatial data

  #Read in the Mitigation data
  MitigationData <- read.csv(MitigationDataFile, check.names = FALSE)
  
  #Read in the Our Land and Water Spatial data, and combine into a single spatial file
  DairySpatialData <- sf::st_read(dsn = DairyLandTypeSpatialDataFile)
  SheepAndBeefSpatialData <- sf::st_read(dsn = SheepAndBeefLandTypeSpatialDataFile)
  
  #Clean up the sheep and beef typology names to exactly match those in Mitigation Lookup Table
  SheepAndBeefSpatialData$Typology <- sub("^.*\\. ","",SheepAndBeefSpatialData$Classifica)
  
  #Merge spatial data
  AllTypes <- do.call(rbind,list(DairySpatialData[,c("Typology","geometry")],SheepAndBeefSpatialData[,c("Typology","geometry")]))
  
  for (Nutrient in c('N','P')){
    
    #Calculate the change in load associated with the mitigation
    #ColumnsOfInterest <- colnames(MitigationData)[which(grepl(paste0(Nutrient,"_loss"),colnames(MitigationLookUpTable)))]
    #Select the mitigation scenario of choice.
    CurrentColumn <-  which(colnames(MitigationData) == paste0("Current",Nutrient,"_loss"))
    ScenarioColumn <- which(colnames(MitigationData) == paste0(MitigationOfInterest,Nutrient,"_loss"))
    MitigationData$ScenarioMitigationFraction <- (MitigationData[,CurrentColumn] - MitigationData[,ScenarioColumn]) / MitigationData[,CurrentColumn]
    MitigationLookUpTable <- MitigationData[,c("Typology","ScenarioMitigationFraction")]  
    
    #create the loss rate reduction raster
    LossRateReductionRaster <- LossRaterReductionRasterCreator(LandTypes = AllTypes,
                                                               MitigationLookUpTable = MitigationLookUpTable,
                                                               ReferenceRaster = LeachRates[[1]])
    #Set no data values to 0
    LossRateReductionRaster[is.na(LossRateReductionRaster[])] <- 0
    if (Nutrient =='N'){
      adjustedRaster <- stack(LeachRates[[1]] * (1 - LossRateReductionRaster))
    } else {
      adjustedRaster[[2]] <- LeachRates[[2]] * (1 - LossRateReductionRaster)
    }
  }
  
  
  return(adjustedRaster)
}