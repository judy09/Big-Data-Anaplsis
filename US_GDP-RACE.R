library(xml2)
library(plyr)
library(rvest)
library(ggplot2)
library(Rmisc)

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
    return(data.frame(GDP=sum(x$"GDP")))
}

data <- function()
{
  userid <- "70359C80-BAFB-4436-9DBA-8A3FE9CE485A"
  tablename <- "SQGDP9"
  
  url <- paste("https://apps.bea.gov/api/data/?&UserID=", userid, "&method=GetParameterValuesFiltered&datasetname=Regional&TargetParameter=LineCode&TableName=", tablename, "&ResultFormat=XML", sep = "")
  result <- read_xml(url)
  parameter <- xml_find_all(result, "//Results/ParamValue") %>% xml_attrs() %>% ldply()
  linecode <- parameter$Key
  
  urls <- paste("https://apps.bea.gov/api/data/?UserID=", userid, "&method=GetData&datasetname=Regional&TableName=", tablename, "&LineCode=", linecode, "&Year=ALL&GeoFips=STATE&ResultFormat=xml", sep = "")
  
  datas <- ldply(urls, catch_data)
  return(datas)
}

myfun <- function(x)
{
  newdata<-data()
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
  newdata$Year <- as.numeric(newdata$Year)
  
  newdata <- subset(newdata,Region==x,select = c(Industry,GeoFips,Region,Year,GDP))  
  area <- ddply(newdata,.variables = c("Industry","GeoFips","Region","Year"),.fun = check)
  total <- data.frame(c("total","total","total","total","total","total"))
  colnames(total)<-c("Industry")
  area_1 <- ddply(newdata,.variables = c("GeoFips","Region","Year"),.fun = check)
  area_1 <-cbind(total,area_1)
  area <-rbind(area,area_1)
  
  file <- readRDS(file = "Race_data")
  file$RACE <- gsub("0", "TOTAL", file$RACE)
  file$RACE <- gsub("1", "WHITE MAN", file$RACE)
  file$RACE <- gsub("2", "BLACK MAN", file$RACE)
  file$RACE <- gsub("3", "ASIAN", file$RACE)
  file$RACE <- gsub("4", "LATINO", file$RACE)
  file$RACE <- gsub("5", "INDIAN", file$RACE)
  file$RACE <- gsub("6", "OTHERS", file$RACE)
  file<-file[,-6:-11]   #刪掉2000-2004
  file<-file[,-11]      #刪掉其中一個2010
  colnames(file) <- c("REGION", "DIVISION", "STATE", "NAME", "RACE" , "2005" , "2006" , "2007", "2008" , "2009", "2010")
  
  tmp <- cbind(file[1:5], unlist(file[6:NCOL(file)]))
  result <- cbind(tmp,data.frame(Year = c(rep("2005",364),rep("2006",364),rep("2007",364),rep("2008",364),rep("2009",364),rep("2010",364))))
  
  colnames(result) <- c("REGION", "DIVISION", "STATE", "NAME", "RACE","NUM","year")
  result$year <- as.numeric(result$year)
  result$year <- result$year+2004
  
  result <- subset(result,NAME==x,select = c(REGION,DIVISION,STATE,NAME,RACE,NUM,year))
  
  ratio <- c()
  for(i in 2005:2010){
    A <- result[result[7]==i,]
    ratio <- append(ratio, round(as.double((A[,6]/A[1,6]*100)),digits = 2)) 
  }
  result <- cbind(result, ratio)
  
  a <- paste(x,"_GDP")
  b <- paste(x,"_RACE")
  
  saveRDS(area, file = a)
  saveRDS(result, file = b)
  
  race <- readRDS(file = b)
  gdp <- readRDS(file = a)
  r <- ggplot(race, aes(x=year, y=ratio, color=RACE)) + geom_point() + geom_line()
  g <- ggplot(gdp, aes(x=Year, y=GDP, color=Industry)) + geom_line() + geom_point()
  print(multiplot(r, g))
}