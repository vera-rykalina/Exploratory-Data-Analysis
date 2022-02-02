#### ************************ ####
#### IoT Analytics #####
#### ************************ ####

#### ************************ ####
#### Domain Research and Exploratory Data Analysis ####
#### ************************ ####
# install.packages("RMySQL", type = "source")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("lubridate")


library(RMySQL)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table) # overwrites lubridates' wday function ! Solution - lubridate::wday

#### Obtain data using SQL ####
# Create database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List tables contained in database 
dbListTables(con)

# List attributes contained in table
dbListFields(con,'yr_2006')
dbListFields(con,'yr_2007')
dbListFields(con,'yr_2008')
dbListFields(con,'yr_2009')
dbListFields(con,'yr_2010')

# Use attribute names to specify specific attributes for download
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

head(yr_2006) # Does not span entire year!
tail(yr_2006)
str(yr_2006) # Date and Time attributes as 'chr' !
summary(yr_2006)

head(yr_2007)
tail(yr_2007)
str(yr_2007) # Date and Time attributes as 'chr' !
summary(yr_2007)

head(yr_2008)
tail(yr_2008)
str(yr_2008)# Date and Time attributes as 'chr' !
summary(yr_2008)

head(yr_2009)
tail(yr_2009)
str(yr_2009) # Date and Time attributes as 'chr' !
summary(yr_2009)

head(yr_2010)  # Does not span entire year!
tail(yr_2010)
str(yr_2010) # Date and Time attributes as 'chr' !
summary(yr_2010)


# Combine tables into one dataframe using dplyr
df3yrs <- bind_rows(yr_2007, yr_2008, yr_2009)
df3yrs <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)
head(df3yrs)  
tail(df3yrs)
str(df3yrs) # Date and Time attributes as 'chr' !
summary(df3yrs)
sum(is.na(df3yrs))
sum(is.nan(df3yrs$Date))
sum(is.nan(df3yrs$Time))
slice(df3yrs, 179:181)

#### Preprocessing ####
# Combine Date and Time attribute values in a new attribute column
df3yrs <-cbind(df3yrs,paste(df3yrs$Date,df3yrs$Time), stringsAsFactors=FALSE)
head(df3yrs)
df3yrs$Date <- NULL
df3yrs$Time <- NULL
head(df3yrs)

# Give the new attribute in the 6th column a header name 
colnames(df3yrs)[4] <-"DateTime"
head(df3yrs)

# Move the DateTime attribute within the dataset
df3yrs <- df3yrs[,c(ncol(df3yrs), 1:(ncol(df3yrs)-1))] # I do not fully understand this code
head(df3yrs)

# Convert DateTime from POSIXlt to POSIXct 
df3yrs$DateTime <- as.POSIXct(df3yrs$DateTime, "%Y/%m/%d %H:%M:%S")
# Add the time zone
attr(df3yrs$DateTime, "tzone") <- "Europe/Paris"
# Inspect the data types
str(df3yrs)
class(df3yrs$DateTime)
tz(df3yrs$DateTime)



# Data Processing using dplyr and lubridate (my alternative way)
date_time <- paste(df3yrs$Date,df3yrs$Time)
df3yrs <-mutate(df3yrs, DateTime=date_time) # dplyr mutate
df3yrs$Date <- NULL
df3yrs$Time <- NULL
colnames(df3yrs)
df3yrs <- select (df3yrs, DateTime, Sub_metering_1, Sub_metering_2, Sub_metering_3) # dplyr select
head(df3yrs)
tail(df3yrs)

df3yrs$DateTime<- ymd_hms(df3yrs$DateTime, tz = "Europe/Paris") # 180 failed to parse
str(df3yrs)
df3yrs <- as.data.table(df3yrs)
# Checking up on consistency (comparison with PoA code)
class(df3yrs$DateTime) # "POSIXct" "POSIXt" 
tz(df3yrs$DateTime) # "Europe/Paris"
head(df3yrs)
tail(df3yrs)
sum((is.na(df3yrs)))
nrow(df3yrs)
slice(df3yrs, 179:181) # I cannot see any problem with row 180

# Create "year" attribute with lubridate
df3yrs$year <- year(df3yrs$DateTime)
head(df3yrs)
class(df3yrs$year) # integer
typeof(df3yrs$year) # integer

# Create "quarter" attribute with lubridate
df3yrs$quarter <- quarter(df3yrs$DateTime)

# Create "month" attribute with lubridate
df3yrs$month <- month(df3yrs$DateTime)

# Create "week" attribute with lubridate
df3yrs$week <- week(df3yrs$DateTime)

# Create "weekday" attribute with lubridate
df3yrs$dayofweek1 <- wday(df3yrs$DateTime) 
df3yrs$dayofweek2 <- lubridate::wday(df3yrs$DateTime, label=TRUE, abbr = TRUE) # shows days as Mon, Tue, etc

# Create "day" attribute with lubridate
df3yrs$day <- day(df3yrs$DateTime)

# Create "hour" attribute with lubridate
df3yrs$hour <- hour(df3yrs$DateTime)

# Create "minute" attribute with lubridate
df3yrs$minute <- minute(df3yrs$DateTime)

head(df3yrs)
is.data.frame(df3yrs)
any(is.na(df3yrs))
