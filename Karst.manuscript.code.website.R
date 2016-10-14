# Karst Project 2013-2016
# R code to pull data from websites for weather and stream flow and incorporate into one file

####______________________
# scrape weather data for nearest National Weather Service site (Cahokia, IL, USA, website site code LSX) for time period encompassing sampling
# create a function called "get.weather.from.forecast" that pulls the data from the website
get.weather.from.forecast <- function(version,site="LSX",issuedby="CPS",product="CF6") {
  result.table <- c()
  for (v.num in version) {
    thepage <- readLines(paste0("http://forecast.weather.gov/product.php?",
                                "site=",site,
                                "&issuedby=",issuedby,
                                "&product=",product,
                                "&format=txt&glossary=0",
                                "&version=",v.num))
    # only collect the part of the webpage that has the actual weather data
    table.start <- which(grepl("^DY MAX",thepage))
    table.stop <- which(grepl("^SM \\d+",thepage))-2
    # put the data into a table
    the.table <- paste(collapse="\n",grep("^=*$",invert=TRUE,value=TRUE,thepage[table.start:table.stop]))
    temp <- read.table(textConnection(the.table),header=TRUE,fill=TRUE,stringsAsFactors=FALSE)
    shifted.wx <- is.na(temp$DR) | temp$DR==""
    temp[shifted.wx,c("SPD.2","DR")] <- temp[shifted.wx,c("WX","SPD.2")]
    temp[shifted.wx,"WX"] <- ""
    # add column that pastes the month of the data
    temp$Month <- gsub(".+MONTH:\\s+(\\S+)","\\1",thepage[grepl("MONTH:",thepage)])[1]
    # add column that pastes that year of the data
    temp$Year <- gsub(".+YEAR:\\s+(\\d+)","\\1",thepage[grepl("YEAR:",thepage)])[1]
    result.table <-
      rbind(result.table,
            temp)
  }
  result.table
}

# use function to retrieve x months of data, where x is defined by the first argument to the function
# i.e., 1 gives you the current month, and 1:3 gives you the past three months, etc

# accessing data in october 2015; want weather data from april 2015 to november 2013
weather.forecast <- get.weather.from.forecast(7:24,site="LSX",issuedby="CPS",product="CF6")

# put collected weather data into dataframe
weather <- data.frame(weather.forecast)
# parse dataframe to only retain day, max temp, min temp, average temp, precipitation, month, and year, name columns, and rearrange columns
weather <- weather[,c(1,2,3,4,8,20,21)]
colnames(weather) <- c("Day","Max.Air.Temp.F", "Min.Air.Temp.F", "Avg.Air.Temp.F", "Total.Water.In", "Month.Name", "Year")
weather <- weather[,c(7,6,1,2,3,4,5)]

# replace month-names with month-numbers
pairs<-NULL
pairs <- data.frame(c("JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY",
                      "AUGUST", "SEPTEMBER", "OCTOBER","NOVEMBER", "DECEMBER"),c(1,2,3,4,5,6,7,8,9,10,11,12))
colnames(pairs) <- c("Name","Number")

for (Month in levels(factor(weather$Month.Name))) {
  weather[weather$Month.Name==Month, "Month"] <- pairs[pairs$Name==Month,"Number"]
}

# change units to numeric type
weather$Max.Air.Temp.F <- as.numeric(weather$Max.Air.Temp.F)
weather$Min.Air.Temp.F <- as.numeric(weather$Min.Air.Temp.F)
weather$Avg.Air.Temp.F <- as.numeric(weather$Avg.Air.Temp.F)
weather$Total.Water.In <- as.numeric(weather$Total.Water.In)

# change units in weather.table to metric
weather$Max.Air.Temp.C <- (weather$Max.Air.Temp.F  - 32)/1.8
weather$Min.Air.Temp.C <- (weather$Min.Air.Temp.F  - 32)/1.8
weather$Avg.Air.Temp.C <- (weather$Avg.Air.Temp.F  - 32)/1.8
weather$Total.Water.cm <- weather$Total.Water.In * 2.54

# remove English units
weathermetric <- weather[,c(1,8,3,9,10,11,12)]
weathermetric$Order <- as.numeric(nrow(weathermetric) +1 -row(weathermetric[1]))

# save weather data
write.table(weathermetric, file= "Karst.weather.csv", sep=",", col.names=T, row.names=F)

####___________________
# USGS has very nice format to download data as a txt file so will use that instead of parsing the website
# go to webpage
# http://nwis.waterdata.usgs.gov/nwis/dv?referred_module=sw&search_site_no=05595200&search_site_no_match_type=exact&state_cd=il&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&range_selection=date_range&begin_date=2013-12-01&end_date=2015-05-01&format=html_table&date_format=YYYY-MM-DD&rdb_compression=file&list_of_search_criteria=state_cd%2Csearch_site_no%2Crealtime_parameter_selection
# download txt file for sample date range
# remove everything except columns for year, month, day, and discharge

# save as "karst.stream.flow.txt"

####_____________________
# read csv of weather and save to object
weather <- read.csv("Karst.weather.csv", header=T)
# read file that has all sample data
master <- read.csv("Karst.all.data.csv", header=T)
# read file of stream flow
stream <- read.delim("karst.stream.flow.txt", header=T)

# perform for loop to summarize the current day, current and previous day, or current and 6 previous days of weather
# and put that info into weathercalc file arranged by each date
weathercalc <- NULL
for (Year in levels(factor(master$Year))) {
  for (Month in levels(factor(master$Month))) {
    for (Day in levels(factor(master$Day))) {
      if (sum (master$Year==Year & master$Month==Month & master$Day==Day)<1 ) {next}
      CurrentRow <- weather$Year==Year & weather$Month==Month & weather$Day==Day 
      OneDay.MaxTemp.C <- weather[CurrentRow, "Max.Air.Temp.C"]
      OneDay.MinTemp.C <- weather[CurrentRow, "Min.Air.Temp.C"]
      OneDay.AvgTemp.C <- weather[CurrentRow, "Avg.Air.Temp.C"]
      OneDay.TotalPrecip.cm <- weather[CurrentRow, "Total.Water.cm"]
      TwoDay.MaxTemp.C <- mean(weather[which(CurrentRow):(which(CurrentRow)-1), "Max.Air.Temp.C"], na.rm=T)
      TwoDay.MinTemp.C <- mean(weather[which(CurrentRow):(which(CurrentRow)-1), "Min.Air.Temp.C"], na.rm=T)
      TwoDay.AvgTemp.C <- mean(weather[which(CurrentRow):(which(CurrentRow)-1), "Avg.Air.Temp.C"], na.rm=T)
      TwoDay.TotalPrecip.cm <- sum(weather[which(CurrentRow):(which(CurrentRow)-1), "Total.Water.cm"], na.rm=T)
      SevenDay.MaxTemp.C <- mean(weather[which(CurrentRow):(which(CurrentRow)-6), "Max.Air.Temp.C"], na.rm=T)
      SevenDay.MinTemp.C <- mean(weather[which(CurrentRow):(which(CurrentRow)-6), "Min.Air.Temp.C"], na.rm=T)
      SevenDay.AvgTemp.C <- mean(weather[which(CurrentRow):(which(CurrentRow)-6), "Avg.Air.Temp.C"], na.rm=T)
      SevenDay.TotalPrecip.cm <- sum(weather[which(CurrentRow):(which(CurrentRow)-6), "Total.Water.cm"], na.rm=T)
      weathercalc <- rbind(weathercalc,data.frame(Year, Month, Day, 
                                                  OneDay.MaxTemp.C, OneDay.MinTemp.C, OneDay.AvgTemp.C, OneDay.TotalPrecip.cm,
                                                  TwoDay.MaxTemp.C, TwoDay.MinTemp.C, TwoDay.AvgTemp.C, TwoDay.TotalPrecip.cm,
                                                  SevenDay.MaxTemp.C, SevenDay.MinTemp.C, SevenDay.AvgTemp.C, SevenDay.TotalPrecip.cm))
    }}}
# merge the master file with the weatehr data for sample dates
newmaster <- merge (master, weathercalc, by=c("Year", "Month", "Day"))

# perform for loop to summarize the current day, current and previous day, or current and 6 previous days of stream flow
# and put that info into weather calc file arranged by each date
streamcalc <- NULL
for (Year in levels(factor(master$Year))) {
  for (Month in levels(factor(master$Month))) {
    for (Day in levels(factor(master$Day))) {
      if (sum (master$Year==Year & master$Month==Month & master$Day==Day)<1 ) {next}
      CurrentRow <- stream$Year==Year & stream$Month==Month & stream$Day==Day 
      OneDay.Mean.Discharge.cuft.per.s <- stream[CurrentRow, "Discharge.cuft.per.s"]
      TwoDay.Mean.Discharge.cuft.per.s <- mean(stream[which(CurrentRow):(which(CurrentRow)-1), "Discharge.cuft.per.s"], na.rm=T)
      SevenDay.Mean.Discharge.cuft.per.s <- mean(stream[which(CurrentRow):(which(CurrentRow)-6), "Discharge.cuft.per.s"], na.rm=T)
      streamcalc <- rbind(streamcalc,data.frame(Year, Month, Day, OneDay.Mean.Discharge.cuft.per.s,
                                                  TwoDay.Mean.Discharge.cuft.per.s,
                                                  SevenDay.Mean.Discharge.cuft.per.s))
    }}}
newnewmaster <- merge (newmaster, streamcalc, by=c("Year", "Month", "Day"))

# save summarized data
write.table(newnewmaster, file= "Karst.all.data.csv", sep=",", col.names=T, row.names=F)