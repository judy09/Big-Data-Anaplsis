library(xml2)
library(plyr)
library(rvest)
library(ggplot2)

catch_data <- function(url)
{
  xml_response <- read_xml(url)
  dimensions <- xml_find_all(xml_response, "//Results/Dimensions") %>% xml_attrs() %>% ldply()
  data <- ldply(xml_find_all(xml_response, "//Results/Data") %>% xml_attrs(), .fun = function(row){
    if("NoteRef" %in% names(row))
      return(NULL) # ignore rows that have "NoteRef" parameter.
    return(row)
  })
  return(data)
}

check <- function(x)
{
  if(x$Industry == "First")
    return(data.frame(GDP=sum(x$"GDP")))
  else if(x$Industry == "Second")
    return(data.frame(GDP=sum(x$"GDP")))
  else if(x$Industry == "Third")
    return(data.frame(GDP=sum(x$"GDP")))
}

#myfun <- function(x)
#{
  userid <- "70359C80-BAFB-4436-9DBA-8A3FE9CE485A"
  tablename <- "SQGDP9"

  url <- paste("https://apps.bea.gov/api/data/?&UserID=", userid, "&method=GetParameterValuesFiltered&datasetname=Regional&TargetParameter=LineCode&TableName=", tablename, "&ResultFormat=XML", sep = "")
  result <- read_xml(url)
  parameter <- xml_find_all(result, "//Results/ParamValue") %>% xml_attrs() %>% ldply()
  linecode <- parameter$Key

  urls <- paste("https://apps.bea.gov/api/data/?UserID=", userid, "&method=GetData&datasetname=Regional&TableName=", tablename, "&LineCode=", linecode, "&Year=ALL&GeoFips=STATE&ResultFormat=xml", sep = "")

  datas <- ldply(urls, catch_data)

  newdata<-datas
  newdata<-subset(newdata,Code!="SQGDP9-1" & Code!="SQGDP9-2" & Code!="SQGDP9-25" & Code!="SQGDP9-13" & Code!="SQGDP9-83",select = c(Code,GeoFips,GeoName,TimePeriod,DataValue))
  newdata<-subset(newdata,TimePeriod=="2005Q4" | TimePeriod=="2006Q4" | TimePeriod=="2007Q4" | TimePeriod=="2008Q4" | TimePeriod=="2009Q4" | TimePeriod=="2010Q4",select = c(Code,GeoFips,GeoName,TimePeriod,DataValue))
  newdata$Code<-gsub(("SQGDP9-82"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-79"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-76"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-70"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-69"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-65"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-64"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-60"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-56"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-51"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-45"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-36"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-35"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-34"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-10"),"Third",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-12"),"Second",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-11"),"Second",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-6"),"First",newdata$Code)
  newdata$Code<-gsub(("SQGDP9-3"),"First",newdata$Code)
  newdata$TimePeriod<-gsub(("Q4"),"",newdata$TimePeriod)
  newdata$DataValue<-gsub((","),"",newdata$DataValue)
  newdata$DataValue<-as.double(newdata$"DataValue")
  
  colnames(newdata)<-c("Industry","GeoFips","Region","Year","GDP")
  
  newdata <- subset(newdata,Region==x,select = c(Industry,GeoFips,Region,Year,GDP))  
  area <- ddply(newdata,.variables = c("Industry","GeoFips","Region","Year"),.fun = check)
  result_2 <- ggplot(area, aes(x = Year,y=GDP, fill = Industry)) + geom_col(position = "dodge")
  print(result_2)
#}