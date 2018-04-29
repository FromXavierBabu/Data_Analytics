#*******************************************************************************
# return_city_name_data_frame -- function to create a dataframe for city list 
# Usage Example:
# return_city_name_data_frame(NtnDataGeo, 2, 3)
#*******************************************************************************
return_city_name_data_frame <- function(geo_dataset, lat, lon) {
  l<- nrow(geo_dataset)
  tmp<-lapply(1:l, function(y) {
      if(!is.na(geo_dataset[y,lat]) && !is.na(geo_dataset[y,lon])){
          print(geo_dataset[y,lon])
          return(return_city_name(as.numeric(geo_dataset[y,lat]), as.numeric(geo_dataset[y,lon])))
      } 
      else{
        return(NA)
      }
  })
}

#*******************************************************************************
# return_city_name() -- function to get city name using latitude and longitude 
# by calling Google RESTful API
# Usage Example:
# city_name<-return_city_name(40.714224,-83.961452)
# city_name<-return_city_name("40.714224","-83.961452")
# print(return_city_name("40.714224","-83.961452"))
#*******************************************************************************
return_city_name <- function(x, y) {
# library(httr)
  google_url = paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=",toString(x),",",toString(y),"&key=AIzaSyAsEVON3jEWSjdSKzdyKnSIyb0OZpnUGFU",sep="")
  output <- GET(url=google_url)
  http_status(output)
  data <- content(output)
  addr<-as.list(strsplit(data[["results"]][[1]][["formatted_address"]], ",")[[1]])
  return(trimws(addr[2]))
}


#*******************************************************************************
# load_libraries() -- function to load all necessary libraries 
# Make sure all necessary library lists are added in the function
# Usage Example:
# load_libraries()
#*******************************************************************************
load_libraries <- function() {
# Please include all necessary library names below
#  x<-c("httr", "jsonlite", "rjson", "RJSONIO", "gdata", ""rgdal")
  x<-c("httr", "RJSONIO", "gdata", "rgdal", "ggplot2")
  #do.call("require", as.list(x))
  lapply(x, require, character.only = TRUE)
}


#*******************************************************************************
# return_json_data() -- function to fetch data from JSON file
# for a  specific data type
# Usage Example:
# return_json_data(PATH="C:/Users/", FILE="abc.json", DATA_TYPE = 'data')
#*******************************************************************************
return_json_data <- function(PATH="", FILE="", DATA_TYPE="data") {
# library("rjson")
# library("RJSONIO")
  file_with_path = paste(PATH, FILE, sep="/")
  print(file_with_path)
  result <- fromJSON(file_with_path)

  if (DATA_TYPE == 'data') {
      return(result[["data"]])  
  }
  else if (DATA_TYPE == 'meta') {
      return(result[["meta"]]) 
  }
  else { 
      print("Error while reading json data")
  }
}

#*******************************************************************************
# return_field_names() -- function to return all field names of JSON dataset 
# It may contain list in a list on a certain column for geographic information
# which is under subColumnTypes
# Usage Example:
# return_field_names(columns)
#*******************************************************************************
return_field_names <- function(columns) {
  getNames<-function(x){
    if(is.null(columns[[x]]$subColumnTypes)){
      return(columns[[x]]$name)
    }else{
      return(columns[[x]]$subColumnTypes)
    }
  }
  return(unlist(sapply(1:length(columns), getNames)))
}

#*******************************************************************************
# return_GeoInfo() -- function to return geo info for all JSON dataset records 
# It will be a list in a list on a particular field of JSON dataset
# Usage Example:
# return_field_names(field_num, json_dataset)
#*******************************************************************************
return_GeoInfo<-function(field_num, j_dataset){
  l<- length(j_dataset[[1]][[field_num]])
  tmp<-lapply(1:l, function(y)
    sapply(j_dataset, function(x){
      if(!is.null(x[[field_num]][[y]])){
        return(x[[field_num]][[y]])
      }else{
        return(NA)
      }
    })     
  )
}


#*******************************************************************************
# return_DataFrame() -- function to return a data frame for a specific field
# in a JSON dataset based on the field number
# Usage Example:
# return_DataFrame(field_number, json_dataset)
#*******************************************************************************
#library(gdata) # for the trim function
return_DataFrame<-function(fieldNum, jdataset) {
#  print(fieldNum)
  print(paste("Variable", fieldNum, sep=" "))  
  sapply(jdataset, function(x) (return_Data(x, fieldNum)))
}

#*******************************************************************************
# return_Data() -- function to trim and return data for a specific field
# in a JSON dataset and fill the NULL data with NA.
# Usage Example:
# return_Data(json_dataset, field_number)
#*******************************************************************************
return_Data<-function(x, fNum){
#  print(fNum)
  if(!is.null(x[[fNum]])){
    return( trim(x[[fNum]]))
  }
  else{
    return(NA)
  }
}

#*******************************************************************************
# return_json_field_data() -- returns the data of a specific column 
# Usage Example:
# fieldData<-return_json_field_data(NtnDataFinal,32)
#*******************************************************************************
return_json_field_data <- function(data, field_num) {
  return(as.data.frame(data[,field_num]))
}    

#*******************************************************************************
# return_unique_json_field_data() -- returns unique data of a specific column 
# Usage Example:
# uData<-return_unique_json_field_data(NtnDataFinal,32)
#*******************************************************************************
return_unique_json_field_data <- function(data, field_num) {
  return(unique(as.data.frame(data[,field_num])))
}    

#*******************************************************************************
# exit() -- function to exit from the function execution 
# For example, if JSON data is invalid, it helps to exit from the data analysis
# Usage Example:
# exit()
#*******************************************************************************
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}  

####################### Main Program ###################
run_data_analysis_program <- function() {

  # Load all necessary Libraries
  load_libraries()

  # File Path and Name
  path = "C:/Users/bxavier/Babu/pers/MBA/online_campus/Data_Engg_for_Business_Analytics/Data"
  file = "500 cities analysis for better health Small.json"
  
  # Validate JSON
  # If it is a invalid JSON file, exit from run_data_analysis_program function
  file_name <- paste(path,file,sep="/")
  isValid <- isValidJSON((file(file_name, "r")), FALSE)
  if (isValid != TRUE) {
     print("Invalid JSON File")
     exit()
  }

  # Parase and Convert JSON into a Data Frame
  json_dataset <- return_json_data(PATH=path, FILE=file, DATA_TYPE = 'data')
  json_metadataset <- return_json_data(PATH=path, FILE=file, DATA_TYPE = 'meta')

  # do the data extraction and assemble the data frame  
  NtnDataPart1<-data.frame(sapply(1:26, return_DataFrame, jdataset=json_dataset), stringsAsFactors=FALSE)
  NtnDataPart2<-data.frame(sapply(28:32, return_DataFrame, jdataset=json_dataset), stringsAsFactors=FALSE)
  NtnDataGeo<-return_GeoInfo(27, json_dataset)
  NtnDataGeo<-data.frame(do.call("cbind", NtnDataGeo), stringsAsFactors=FALSE)
#  citylist_dataframe <- t(as.data.frame(return_city_name_data_frame(NtnDataGeo, 2, 3)))
#  NtnDataFinal<-cbind(NtnDataPart1, NtnDataGeo, NtnDataPart2, citylist_dataframe)
  NtnDataFinal<-cbind(NtnDataPart1, NtnDataGeo, NtnDataPart2)
  
  columns<-json_metadataset[['view']][['columns']]
  NtnFieldNames<-return_field_names(columns)
#  NtnFieldNames[length(NtnFieldNames)+1] <- "City_Name"
  names(NtnDataFinal)<-NtnFieldNames

  head(NtnDataFinal)
  

######## Do Geoplot of all Cities where the survey has been taken, #########
######## especially in NY State using latitude and Longitude data  #########
  
  #library('rgdal', lib.loc = 'C:/libs')
  # library(ggplot2)
  
  state<-readOGR("C:/Users/bxavier/Babu/pers/MBA/online_campus/Data_Engg_for_Business_Analytics/Data/nys/nys.shp", layer="nys")
  
  fmDataDF1<-NtnDataFinal[!is.na(NtnDataFinal$latitude) & !is.na(NtnDataFinal$longitude) & NtnDataFinal$StateAbbr=='NY',]
  #fmDataDF1[["Data_Value"]]<-as.numeric(fmDataDF1[["Data_Value"]])
  fmDataDF1[["longitude"]]<-as.numeric(fmDataDF1[["longitude"]])
  fmDataDF1[["latitude"]]<-as.numeric(fmDataDF1[["latitude"]])
  
  #write.csv(fmDataDF1[,c("Year", "DataSource", "latitude", "longitude", "PopulationCount")],
  #          "c:/temp/health_analysis.csv", row.names=FALSE)
  
  coordinates(fmDataDF1)<-~longitude+latitude
  proj4string(fmDataDF1)<-CRS("+proj=longlat +datum=NAD83") #set the coordinate system
  fmDataDF1<-spTransform(fmDataDF1, CRS(proj4string(state)))
  geodata<-data.frame(coordinates(fmDataDF1))
  names(geodata)<-c("x", "y")
  
  ggplot() +  
    geom_polygon(data=state, aes(x=long, y=lat, group=group), fill="grey40", 
                 colour="grey90", alpha=1)+
    labs(x="", y="", title="Better Health Analysis NYS")+ #labels
    theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1))+ # make title bold and add space
    geom_point(aes(x=x, y=y), data=geodata, alpha=1, size=3, color="grey20")+ # to get outline
    geom_point(aes(x=x, y=y), data=geodata, alpha=1, size=2, color="darkred")+
    coord_equal(ratio=1)

#  return(1)
  
############# Retur more than one object from a function ##############
 return(list(NtnDataFinal, NtnDataGeo))
  
}

########### Calling the Main Program ###########
result <- 0
result <- run_data_analysis_program()
# result <- 1
if (result == 1) {
  print("Successfully Analysed the Data, Thanks for watching")
}

# Further Data Analysis

if (result == 1) {
#  merge(aggregate(as.numeric(Data_Value)~as.numeric(Low_Confidence_Limit), NtnDataFinal[NtnDataFinal$StateAbbr=="MA",], function(x) {
#    c(mean=mean(x, na.rm=TRUE), quantile(x, na.rm=TRUE))
#  }), list(Data_Value=unique(NtnDataFinal$Data_Value)), all=TRUE)

  names(result[[1]])
  rtnNtnDataFinal <- result[[1]]
  
  SQData<- return_unique_json_field_data(rtnNtnDataFinal, 36)
  
  aa<-NULL
     
  aa[c("variable", "value")] <- unique(rtnNtnDataFinal[c("Short_Question_Text", "Data_Value")])  
  
  aa[["value"]] <- as.numeric(aa[["value"]])
  
  aa <- aggregate(value ~ variable, aa, sum)

  library(ggplot2)
  ggplot(transform(transform(aa, value=value/sum(value)), labPos=cumsum(value)-(value/2)), 
         aes(x="", y = value, fill = variable)) +
    geom_bar(width = 1, stat = "identity") +
    scale_fill_manual(values = c("red", "grey","blue", "green", "cyan")) +
    coord_polar(theta = "y") +
    labs(title = "Short Question Survey Analysis") + 
    geom_text(aes(y=labPos, label=scales::percent(value)), size=5, color="white")


      
  pie(aa$B, labels=aa$A, main="Pie Chart of Countries")
  
  library(data.table)
  DT <- data.table(aa)
  bb<- DT[, sum(B), by = A]
  pie(bb$V1, labels=bb$A, main="Pie Chart of Countries")
  

  
  head(aa)
  nrow(aa)
  unique(df[c("yad", "per")])
  
  a<-aggregate(as.numeric(NtnDataFinal$Data_Value), by=list(NtnDataFinal$GeographicLevel), FUN=mean, na.rm = TRUE)

  a<-aggregate(as.numeric(NtnDataFinal$Data_Value), by=list(NtnDataFinal$MeasureId, NtnDataFinal$GeographicLevel), FUN=mean, na.rm = TRUE)

  pie(aa$Data_Value, labels = aa$Short_Question_Text, main="Pie Chart of Countries")

  hist(a$x)
}






##### Installing Libs using Local Zip files ############

.libPaths("c:/temp/rgdal")

install.packages("C:/Users/bxavier/Babu/pers/MBA/online_campus/Data_Engg_for_Business_Analytics/Data/rgdal_1.2-20.zip",
                  destdir = 'C:/temp',  # no "/" after the path
                 lib = 'C:/libs', 
                 repos = NULL)

install.packages("rgdal", lib = "C:/Program Files/R/R-3.4.3/library")

install.packages('sp')

setInternet2(TRUE)

library('rgdal', lib.loc = 'C:/libs')

#setwd("C:/Users/bxavier/Babu/pers/MBA/online_campus/Data_Engg_for_Business_Analytics/Data")
#unzip("rgdal_1.2-20.zip")
#file.rename("R/rgdal", "rdgal")
#shell("R CMD build C:/Users/bxavier/Babu/pers/MBA/online_campus/Data_Engg_for_Business_Analytics/Data/R/rgdal")

