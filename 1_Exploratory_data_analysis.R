#### ************************ ####
#### IoT Analytics #####
#### ************************ ####

#### ************************ ####
#### Domain Research and Exploratory Data Analysis ####
#### ************************ ####
# install.packages("pacman")
# install.packages("RMySQL", type = "source")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("imputeTS")
# install.packages("plotly")
# install.packages("Cairo")

# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/ (ggplot color palettes)
# https://ro-che.info/articles/2017-02-22-group_by_month_r (float_date)
# https://www.rdocumentation.org/packages/imputeTS/versions/2.7/topics/na.interpolation (imputeTS)

# https://plot.ly/r/ (plotly)
# na.locf()
# which(is.na(xxxxx) == TRUE)
# gimp (open source PhotoShop)
# merge() merge to dataframes by common columns or row names
# ggplotly() to convert ggplot graph to plotly graph

#### Packages  ####

## libraries

pacman::p_load("RMySQL","dplyr", "tidyr","lubridate","esquisse","padr","imputeTS", "ggplot2", "chron","plotly", "shiny", "wesanderson", "openair", "Cairo", "forecast", "broom", "caret")

#### SQL ####

## Create database connection 
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List tables contained in database 
dbListTables(con)
 
## List attributes contained in table
dbListFields(con,'yr_2006')
dbListFields(con,'yr_2007')
dbListFields(con,'yr_2008')
dbListFields(con,'yr_2009')
dbListFields(con,'yr_2010')

## Build query

query <- "
SELECT * FROM yr_2006
UNION ALL
SELECT * FROM yr_2007
UNION ALL
SELECT * FROM yr_2008
UNION ALL
SELECT * FROM yr_2009
UNION ALL
SELECT * FROM yr_2010
" # sql union all operator (combines data from similar tables without removing duplicate rows)

## Dataframe with queried data
sql.table <- dbGetQuery(con, query)


## Dataframe overview
summary (sql.table)
str(sql.table)
head(sql.table)
tail(sql.table)
nrow(sql.table)
ncol(sql.table)
class(sql.table)

#### Preprocessing ####

## Select necessary columns
colnames(sql.table)
preprocessed.table <- select(sql.table, Date, Time, Global_reactive_power, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3)
head(preprocessed.table)

## Convert Date to POSIXct format 
#preprocessed.table$Date <- lubridate::ymd(preprocessed.table$Date)
preprocessed.table$Date <- lubridate::ymd(preprocessed.table$Date)
range(preprocessed.table$Date)  # 2006-12-16 - 2010-11-26


## Create new DateTime feature by combining Date and Time 
preprocessed.table$DateTime<-lubridate::ymd_hms(paste(preprocessed.table$Date, preprocessed.table$Time))
class(preprocessed.table$DateTime)


## Convert unites for variables
preprocessed.table$Global_active_power <- preprocessed.table$Global_active_power*1000/60 #Watt/hour
preprocessed.table$Global_reactive_power <- preprocessed.table$Global_reactive_power*1000/60 #Watt/hour


head(preprocessed.table)
colnames(preprocessed.table)[3] <- 'GRP'
colnames(preprocessed.table)[4] <- 'GAP'
colnames(preprocessed.table)[5] <- 'Submeter_1'
colnames(preprocessed.table)[6] <- 'Submeter_2'
colnames(preprocessed.table)[7] <- 'Submeter_3'

preprocessed.table$Other <- preprocessed.table$GAP-preprocessed.table$Submeter_1-preprocessed.table$Submeter_2-preprocessed.table$Submeter_3 #Watt/hour
preprocessed.table$Submeters <- preprocessed.table$Submeter_1+preprocessed.table$Submeter_2+preprocessed.table$Submeter_3 #Watt/hour



## calendar heat to see missing records 
# txt.table = read.table('~/Desktop/DA_Online/IoT Analytics/1 Domain Research and Exploratory Data Analysis/household_power_consumption.txt', sep=";", header=T, 
#                    na.strings=c("?",""), stringsAsFactors=FALSE)
# txt.table$Date <- lubridate::dmy(txt.table$Date)
# date.table.txt <- txt.table %>%
#   select(Date, Global_active_power) %>%
#   mutate(Missing = ifelse(is.na(txt.table$Global_active_power), 1, 0)) %>%
#   group_by(Date) %>%
#   summarise(counts=sum(Missing)) # Use dplyr's summarize function to summarize by our NA indicator (where 1 = 1 minute with NA)
# source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
# txt.calendar <- calendarHeat(date.table.txt$Date, date.table.txt$counts, varname="Missing Data", color = "r2b")


## Pad
pad.table <- pad(preprocessed.table, by = "DateTime", break_above = 3) #adding blank rows
sum(is.na(pad.table)) # check blank rows presence
nrow(pad.table)
summary(pad.table) # with NA now


#### NA's ####
sum(is.na(pad.table))

## NA CalanderMap of pad.table
date.table <- pad.table %>%
  select(DateTime, GAP) %>%
  separate(col=DateTime, into=c("Date", "Time"), sep=" ") %>%
  mutate(Date =lubridate::ymd(Date)) %>%
  mutate(year=year(Date)) %>% # can be omitted for 2006
  filter(year!=2006) %>% # can be omitted for 2006
  mutate(Missing = ifelse(is.na(GAP), 1, 0)) %>%
  group_by(Date) %>%
  summarise(counts=sum(Missing))
head(date.table)
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R") 
pad.calendar<-calendarHeat(date.table$Date, date.table$counts, varname="Missing Data", color = "w2b") #"r2b"
  

## Arrange by DateTime
pad.table <- arrange(pad.table, DateTime)
pad.table <-select(pad.table, Date, DateTime, GRP, GAP, Submeter_1, Submeter_2, Submeter_3, Submeters, Other)
head(pad.table)
summary(pad.table)



#### Imputing values in missing values ####
imputed.table <- na_interpolation(pad.table, option = "linear")
sum(is.na(imputed.table))
nrow(imputed.table)
str(imputed.table)


#### Feature Engineering ####
head(imputed.table)
#imputed.table$year <- lubridate::floor_date(imputed.table$DateTime, unit="year")
imputed.table$year <- lubridate::year(imputed.table$DateTime)
imputed.table$quarter <- lubridate::quarter(imputed.table$DateTime)
imputed.table$month <- lubridate::month(imputed.table$DateTime, label=TRUE, abbr=TRUE)
imputed.table$week <- lubridate::week(imputed.table$DateTime)
imputed.table$day <- lubridate::day(imputed.table$DateTime)
imputed.table$wday <- lubridate::wday(imputed.table$DateTime, label=TRUE, abbr=TRUE, week_start = getOption("lubridate.week.start", 1))
imputed.table$hour <- hour(imputed.table$DateTime)
imputed.table$minute <- minute(imputed.table$DateTime)
imputed.table$mday <- mday(imputed.table$DateTime)
str(imputed.table)
summary(imputed.table)
head(imputed.table)



## Create long form of data set
tidy.table <- imputed.table %>%
  gather(Consumption, Watt_hour, "Submeter_1", "Submeter_2", "Submeter_3", "Other")  

str(tidy.table)
tidy.table$Consumption <- factor(tidy.table$Consumption)
summary(tidy.table)

box.tidy.table <- tidy.table %>%
  filter(!year %in% c(2006,2010))  %>%
  #group_by(Consumption) %>%
  ggplot(aes(x=factor(Consumption), y=Watt_hour,color=Consumption)) + 
  geom_boxplot()

#### Exploratory Analysis ####

## Year consumption
year.total <- tidy.table %>% 
  filter(!year %in% c(2006,2010))  %>%
  group_by(year, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(year), y=sum)) +
  labs(x='Year', y='kWh', title='Total Energy Consumption', subtitle = "By Year (2007-2009)") +
  geom_bar(aes(fill=Consumption), stat='identity', color='darkgrey') +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)) +
  theme(panel.border=element_rect(colour='darkgrey', fill=NA)) +
  theme(text = element_text(size = 14))
ggplotly(year.total)
              

## Year consumption with plotly
plotly.year <- imputed.table %>% 
  filter(!year %in% c(2006,2010))  %>%
  group_by(year=factor(year)) %>% #grouping by year
  mutate(Submeter_1 = round(Submeter_1/1000, digits=4),
         Submeter_2 = round(Submeter_2/1000, digits=4),
         Submeter_3 = round(Submeter_3/1000, digits=4),
         Other = round(Other/1000, digits=4)) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>%
  plot_ly(x = ~year, y = ~Submeter_1, type = 'bar', name = 'Kitchen') %>%
  add_trace(y = ~Submeter_2, name = 'Laundry Room') %>%
  add_trace(y = ~Submeter_3, name = 'Water Heater & AC') %>%
  add_trace(y = ~Other, name = 'Other Areas') %>%
  layout(title = "Total Energy Consumption by Year",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Energy Consumption (kWh)"),
         barmode="stack")

## Quarter consumption
quarter.total <-
  tidy.table %>% 
  filter(!year %in% c(2006,2010))  %>%
  group_by(quarter, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(quarter), y=sum)) +
  labs(x='Quarter', y='kWh', title='Total Energy Consumption', subtitle = "By Quarter of Year (2007-2009)") +
  geom_bar(aes(fill=Consumption), stat='identity', color='darkgrey') +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)) +
  theme(panel.border=element_rect(colour='darkgrey', fill=NA)) +
  theme(text = element_text(size = 16))



# Quarter consumption with plotly
plotly.quarter <- imputed.table %>%
  filter(!year %in% c(2006,2010))  %>%
  group_by(quarter=factor(quarter)) %>%
  mutate(Submeter_1 = round(Submeter_1/1000, digits=4),
         Submeter_2 = round(Submeter_2/1000, digits=4),
         Submeter_3 = round(Submeter_3/1000, digits=4),
         Other = round(Other/1000, digits=4)) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>%
  plot_ly(x = ~quarter, y = ~Submeter_1, type = 'bar', name = 'Kitchen') %>%
  add_trace(y = ~Submeter_2, name = 'Laundry Room') %>%
  add_trace(y = ~Submeter_3, name = 'Water Heater & AC') %>%
  add_trace(y = ~Other, name = 'Other Areas') %>%
  layout(title = "Total Energy Consumption by Quarter",
         xaxis = list(title = "Quarter of Year"),
         yaxis = list (title = "Energy Consumption (kWh)"),
         barmode="stack")

# Quarter consumption with plotly (lines)
plotly.quarter.lines <- imputed.table %>%
  #filter(!year %in% c(2006,2010))  %>%
  group_by(quarter=floor_date(DateTime, "quarter")) %>%
  mutate(Submeter_1 = round(Submeter_1/1000, digits=4),
         Submeter_2 = round(Submeter_2/1000, digits=4),
         Submeter_3 = round(Submeter_3/1000, digits=4),
         Other = round(Other/1000, digits=4)) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>%
  plot_ly(x = ~quarter, y = ~Submeter_1, 
          name = 'Kitchen', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Submeter_2, name = 'Laundry Room', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Submeter_3, name = 'Water Heater & AC', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Other, name = 'Other Areas', type = 'scatter', mode="lines") %>%
  layout(title = "Total Energy Consumption by Quarter",
         xaxis = list(title = "Quarter of Year"),
         yaxis = list (title = "Energy Consumption (kWh)"),
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.3))



## Month consumption
month.total <-
  tidy.table %>% 
  filter(!year %in% c(2006,2010))  %>%
  group_by(month, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(month), y=sum)) +
  labs(x='Month', y='kWh', title='Total Energy Consumption', subtitle = "By Month of Year (2007-2009)") +
  geom_bar(aes(fill=Consumption), stat='identity', color='darkgrey') +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)) +
  theme(panel.border=element_rect(colour='darkgrey', fill=NA)) +
  theme(text = element_text(size = 16))
ggplotly(month.total)


plotly.month <-imputed.table %>%
  filter(!year %in% c(2006,2010))  %>%
  group_by(month=factor(month)) %>%
  mutate(Submeter_1 = round(Submeter_1/1000, digits=4),
         Submeter_2 = round(Submeter_2/1000, digits=4),
         Submeter_3 = round(Submeter_3/1000, digits=4),
         Other = round(Other/1000, digits=4)) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>%
  plot_ly(x = ~month, y = ~Submeter_1, type = 'bar', name = 'Kitchen') %>%
  add_trace(y = ~Submeter_2, name = 'Laundry Room') %>%
  add_trace(y = ~Submeter_3, name = 'Water Heater & AC') %>%
  add_trace(y = ~Other, name = 'Other Areas') %>%
  layout(title = "Total Energy Consumption by Month",
         xaxis = list(title = "Month of Year"),
         yaxis = list (title = "Energy Consumption (kWh)"),
         barmode="stack")


# Month consumption with plotly (lines)
plotly.month.lines <- imputed.table %>%
  #filter(!year %in% c(2006,2010))  %>%
  group_by(month=floor_date(DateTime, "month")) %>%
  mutate(Submeter_1 = round(Submeter_1/1000, digits=4),
         Submeter_2 = round(Submeter_2/1000, digits=4),
         Submeter_3 = round(Submeter_3/1000, digits=4),
         Other = round(Other/1000, digits=4)) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>%
  plot_ly(x = ~month, y = ~Submeter_1, 
          name = 'Kitchen', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Submeter_2, name = 'Laundry Room', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Submeter_3, name = 'Water Heater & AC', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Other, name = 'Other Areas', type = 'scatter', mode="lines") %>%
  layout(title = "Total Energy Consumption by Month",
         xaxis = list(title = "Month of Year"),
         yaxis = list (title = "Energy Consumption (kWh)"),
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.3))



## Week consumption
week.total <-
  tidy.table %>% 
  group_by(week, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(week), y=sum)) +
  labs(x='Week', y='kWh', title='Total Energy Consumption', subtitle = "By Week of Year (2006-2010)") +
  geom_bar(aes(fill=Consumption), stat='identity', color='darkgrey') +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)) +
  theme(panel.border=element_rect(colour='darkgrey', fill=NA)) +
  theme(text = element_text(size = 12))
ggplotly(week.total)

plotly.week <-imputed.table %>%
  group_by(week=factor(week)) %>%
  mutate(Submeter_1 = round(Submeter_1/1000, digits=4),
         Submeter_2 = round(Submeter_2/1000, digits=4),
         Submeter_3 = round(Submeter_3/1000, digits=4),
         Other = round(Other/1000, digits=4)) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>%
  plot_ly(x = ~week, y = ~Submeter_1, type = 'bar', name = 'Kitchen') %>%
  add_trace(y = ~Submeter_2, name = 'Laundry Room') %>%
  add_trace(y = ~Submeter_3, name = 'Water Heater & AC') %>%
  add_trace(y = ~Other, name = 'Other Areas') %>%
  layout(title = "Total Energy Consumption by Week of Year",
         xaxis = list(title = "Week of Year"),
         yaxis = list (title = "Energy Consumption (kWh)"),
         barmode="stack")



## Day of the week consumption
weekday.total <-
  tidy.table %>% 
  group_by(wday, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(wday), y=sum)) +
  labs(x='Day of week', y='kWh', title='Total Energy Consumption', subtitle = "By Day of Week (2006-2010)") +
  geom_bar(aes(fill=Consumption), stat='identity', color='darkgrey') +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)) +
  theme(panel.border=element_rect(colour='darkgrey', fill=NA)) +
  theme(text = element_text(size = 12))
ggplotly(weekday.total)

## Time of the day consumption
hour.total <-
  tidy.table %>% 
  group_by(hour, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(hour), y=sum)) +
  labs(x='Hour', y='kWh', title='Total Energy Consumption', subtitle = "By Time of Day (2006-2010)") +
  geom_bar(aes(fill=Consumption), stat='identity', color='darkgrey') +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)) +
  theme(panel.border=element_rect(colour='darkgrey', fill=NA)) +
  theme(text = element_text(size = 12))
ggplotly(weekday.total)


# Hour consumption with plotly (lines)
plotly.hour.lines <- imputed.table %>%
  #filter(!year %in% c(2006,2010))  %>%
  group_by(hour=floor_date(DateTime, "hour")) %>%
  mutate(Submeter_1 = round(Submeter_1/1000, digits=4),
         Submeter_2 = round(Submeter_2/1000, digits=4),
         Submeter_3 = round(Submeter_3/1000, digits=4),
         Other = round(Other/1000, digits=4)) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>%
  plot_ly(x = ~hour, y = ~Submeter_1, 
          name = 'Kitchen', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Submeter_2, name = 'Laundry Room', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Submeter_3, name = 'Water Heater & AC', type = 'scatter', mode="lines") %>%
  add_trace(y = ~Other, name = 'Other Areas', type = 'scatter', mode="lines") %>%
  layout(title = "Total Energy Consumption by Hour",
         xaxis = list(title = "Hour of Day"),
         yaxis = list (title = "Energy Consumption (kWh)"),
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.3))




## Month of Year facets
month.total.facets <-
  tidy.table %>% 
  filter(year!= 2006)  %>%
  group_by(year, month, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(month), y=sum)) +
  #labs(x="Month", y="kWh", title="Total Energy Consumption", subtitle = "By Month of Year (2007-2010)") +
  labs(x="Month", y="Electric power (kWh)")+
  geom_bar(aes(fill=Consumption), stat="identity", color="black") +
  scale_fill_manual(labels = c("Other Areas", "Kitchen", "Laundry Room", "Water Heater & AC"), values = wes_palette("Royal1", n = 4)) +
  theme(text = element_text(size = 12), panel.border=element_rect(color="gray65", fill=NA)) +
  # Add a vertical line at 1000 kWh level
  #geom_hline(aes(yintercept = 1000), size=2, color="darkgrey")+
  # Change legend text color
  #theme(legend.text=element_text(color="darkgrey"))+
  facet_wrap(facets= ~as.factor(year)) +
  theme(strip.background = element_rect(fill="white", color="gray65"), 
  strip.text = element_text(color="black", size=14)) +
  theme(legend.position="bottom", legend.title=element_blank())
ggplotly(month.total.facets)
  
## Week Day of Year facets

weekday.total.facets <-
  tidy.table %>% 
  filter(year!= 2006)  %>%
  group_by(year, wday, Consumption) %>%
  summarise(sum=round(sum(Watt_hour/1000)), 3)%>%
  ggplot(aes(x=factor(wday), y=sum)) +
  #labs(x="Month", y="kWh", title="Total Energy Consumption", subtitle = "By Month of Year (2007-2010)") +
  labs(x="Day of week", y="Electric power (kWh)")+
  geom_bar(aes(fill=Consumption), stat="identity", color="black") +
  scale_fill_manual(labels = c("Other Areas", "Kitchen", "Laundry Room", "Water Heater & AC"), values = wes_palette("Royal1", n = 4)) +
  theme(text = element_text(size = 12), panel.border=element_rect(color="gray65", fill=NA)) +
  facet_wrap(facets= ~as.factor(year)) +
  theme(strip.background = element_rect(fill="white", color="gray65"), 
        strip.text = element_text(color="black", size=14)) +
  theme(legend.position="bottom", legend.title=element_blank())
ggplotly(weekday.total.facets)


## Create object
save(tidy.table, pad.table, imputed.table,
     file = "~/Desktop/DA_Online/IoT Analytics/1 Domain Research and Exploratory Data Analysis/tables.RData")  

#### GROUP & VISUALIZE GRANULARITY ####
## Seasons
## ifelse to set the season attribute
season.plotly.lines <- imputed.table %>%
  #filter(year==2009) %>%
  mutate(Season = ifelse(month == "Dec"| month == "Jan" | month == "Feb","Winter", 
                  ifelse(month == "Mar" | month == "Apr" | month == "May", "Spring",
                  ifelse(month == "Jun" | month == "Jul" | month == "Aug", "Summer", "Autumn")))) %>%
  group_by(Season) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>% 
  plot_ly(x = ~Season, y = ~Submeter_1,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Submeter_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Submeter_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~Other,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Seasonal Power Consumption",
         xaxis = list(title = "Season"),
         yaxis = list (title = "Power (watt-hours)"))



##### WEEKDAY MEAN CONSUMPTION #####
## Mean consumption based on the day of the week of a general day of 2007, 2008, 2009
by_wday <- imputed.table %>% 
  group_by(day=floor_date(DateTime, "day")) %>%
  summarise(Other = sum(Other/1000), Submeter_1=sum(Submeter_1/1000), 
            Submeter_2=sum(Submeter_2/1000), Submeter_3 = sum(Submeter_3/1000)) %>%
  filter(year(day) %in% c(2007,2008,2009)) %>%
  mutate(Weekday = wday(day, label = TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(Weekday) %>%
  summarise(Other = mean(Other), Submeter_1=mean(Submeter_1),
            Submeter_2=mean(Submeter_2), Submeter_3 = mean(Submeter_3)) %>%
  ggplot(aes(x = as.factor(Weekday))) +
  geom_line(aes(y = Other, group=1, color = "Other Areas")) +
  geom_line(aes(y = Submeter_1, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3, group=1, color = "Water Heater & AC")) +
  theme_minimal() +
  labs(title = "Mean Energy consumption for general day of week",
       x = "Day of week", y = "Mean energy consumption (kWh)") +
  theme(legend.position="bottom", legend.title=element_blank())


## Mean consumption based on the month of the year of a general month of 2007, 2008, 2009
by_month <- imputed.table %>% 
  group_by(month=floor_date(DateTime, "month")) %>% # month granularity
  summarise(Other = sum(Other/1000), Submeter_1=sum(Submeter_1/1000), 
            Submeter_2=sum(Submeter_2/1000), Submeter_3 = sum(Submeter_3/1000)) %>%
  filter(year(month) %in% c(2007,2008,2009)) %>%
  mutate(mean_month = month(month, label = TRUE, abbr = FALSE)) %>%
  group_by(mean_month) %>%
  summarise(Other = mean(Other), Submeter_1=mean(Submeter_1),
            Submeter_2=mean(Submeter_2), Submeter_3 = mean(Submeter_3)) %>%
  ggplot(aes(x = as.factor(mean_month))) +
  geom_line(aes(y = Other, group=1, color = "Other Areas")) +
  geom_line(aes(y = Submeter_1, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3, group=1, color = "Water Heater & AC")) +
  theme_minimal() +
  labs(title = "Mean Energy consumption for general month of year",
       x = "Month of year", y = "Mean energy consumption (kWh)") +
  theme(legend.position="bottom", legend.title=element_blank())


## Mean consumption based on the hour of the day of a general hour of 2007, 2008, 2009
by_hour <- imputed.table %>% 
  group_by(hour=floor_date(DateTime, "hour")) %>%
  summarise(Other = sum(Other/1000), Submeter_1=sum(Submeter_1/1000), 
            Submeter_2=sum(Submeter_2/1000), Submeter_3 = sum(Submeter_3/1000)) %>%
  filter(year(hour) %in% c(2007,2008,2009)) %>%
  mutate(mean_hour = hour(hour)) %>%
  group_by(mean_hour) %>%
  summarise(Other = mean(Other), Submeter_1=mean(Submeter_1),
            Submeter_2=mean(Submeter_2), Submeter_3 = mean(Submeter_3)) %>%
  ggplot(aes(x = as.factor(mean_hour))) +
  geom_line(aes(y = Other, group=1, color = "Other Areas")) +
  geom_line(aes(y = Submeter_1, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3, group=1, color = "Water Heater & AC")) +
  theme_minimal() +
  labs(title = "Mean Energy consumption for general hour of day",
       x = "Hour of day", y = "Mean energy consumption (kWh)") +
  theme(legend.position="bottom", legend.title=element_blank())



### Mean consumption based on the day of the week of a general day of 2007, 2008 and 2009

## Preparing daily granularity

by_day <- imputed.table %>% 
  group_by(day=floor_date(DateTime, "day")) %>%
  summarise(Other = sum(Other/1000), Submeter_1=sum(Submeter_1/1000), Submeter_2=sum(Submeter_2/1000), Submeter_3 = sum(Submeter_3/1000))


## Save the dataframe to use it in other scripts
save(by_day, file = "by_day.rda")

##Filter years 2007, 2008, 2009, 2010 and create a new variable with weekday detail labeled
year_wday_filter <- by_day %>% 
  filter(year(day) %in% c(2007,2008,2009)) %>%
  mutate(Weekday = wday(day, label = TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(year=year(day))

#Create the mean consumption of each week day to know the general consumption behavior of on any given weekday

mean_wday <- year_day_filter %>% group_by(year, Weekday) %>%
  summarise(Other = mean(Other), Submeter_1=mean(Submeter_1),
            Submeter_2=mean(Submeter_2), Submeter_3 = mean(Submeter_3))

ggplot(data=mean_wday, aes(x = as.factor(Weekday))) +
  geom_line(aes(y = Submeter_1, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3, group=1, color = "Water Heater & AC")) +
  geom_line(aes(y = Other, group=1, color = "Other Areas")) +
  theme_minimal() +
  labs(title = "Mean Energy consumption for general day of week",
       x = "Day of week",y = "Mean energy consumption (Watt/hour)") +
  theme(legend.position="bottom", legend.title=element_blank()) +



## Seasonal plot
#-Subset data for weeks 1-8 and assign to variable w
w <- tidy.table %>%
  filter(week == c(1:8)) %>%
  filter(Consumption == "Submeter_3") %>% 
  group_by(wday, Consumption) %>%
  summarise(sum=sum(Watt_hour/1000))

## Subset data for weeks 18-25 and assign to variable ww
ww <- tidy.table %>%
  filter(week == c(18:25)) %>%
  filter(Consumption == "Submeter_3") %>% 
  group_by(wday, Consumption) %>%
  summarise(sum=sum(Watt_hour/1000))

## Overlay line plots of the two 8-week time periods
ggplot(w) +
  labs(x='Day of the Week', y='kWh', 
       title="Mean energy consumption on Water Heater & AC", 
       subtitle = "High consumption periods in winter and summer months") +
  geom_line(aes(x=wday, y=sum, group=1,colour='Winter')) +
  geom_line(data = ww, aes(x=wday, y=sum, group=1, color='Summer')) +
  theme_minimal() +
  scale_colour_manual(values=c('Winter'='black', 'Summer'='red')) +
  labs(colour='Season') +
  guides(colour=guide_legend(reverse=TRUE)) +
  theme(panel.border=element_rect(colour='black', fill=NA))+
  theme(text = element_text(size = 10)) +
  theme(legend.position="bottom", legend.title=element_blank())



##### VISUALIZATION to study granularity ####
aggregated_df <- c()
plots.gap.sub <- c()
plots.gap.sumsub <- c()


granularity <- c("year", "month", "day", "week", "hour", "30 mins")

for(g in granularity){
  aggregated_df[[g]] <- imputed.table %>%
    group_by(DateTime=floor_date(DateTime, g)) %>%
    dplyr::summarize_at(vars(
      Submeter_1,
      Submeter_2,
      Submeter_3,
      GAP, 
      Other,
      Submeters),
      funs(sum))

# PLOT -> Global Power vs Submetering Records in [[g]] 
  plots.gap.sumsub[[g]] <- ggplot(data = aggregated_df[[g]], aes(x = DateTime)) +
    geom_line(aes(y = Submeters, color = "Submeters")) +
    geom_line(aes(y = GAP, color = "Global Power")) +
    theme_minimal()+
    labs(title = paste("Global power vs submetering records", g),
         x = "Time",
         y = "Power")
}
plots.gap.sumsub[["month"]]
plots.gap.sumsub[["year"]]
plots.gap.sumsub[["week"]]
plots.gap.sumsub[["day"]]
plots.gap.sumsub[["hour"]]



head(imputed.table)
## Subset data by month and summarise total energy usage
consumption_month <-imputed.table %>%
  filter(year %in% c(2007, 2008, 2009, 2010)) %>%
  group_by(year, month) %>%
  summarise(GAP=round(sum(GAP/1000), 3),
            Other = round(sum(Other/1000), 3), 
            Submeter_1=round(sum(Submeter_1/1000), 3), 
            Submeter_2=round(sum(Submeter_2/1000), 3), 
            Submeter_3=round(sum(Submeter_3/1000), 3), 
            Submeters=round(sum(Submeters/1000), 3)) 
            
## Create monthly time series 2007-2009
ts_month_2009 <- ts(consumption_month, frequency = 12, start=c(2007,1), end=c(2010,1))
ts_month_2009_table <- as.data.frame(ts_month_2009)
tail(ts_month_2009)

## Create monthly time series 2007-10.2010
ts_month_2010 <- ts(consumption_month, frequency = 12, start=c(2007,1), end=c(2010,11))
ts_month_2010_table <- as.data.frame(ts_month_2010)
tail(ts_month_2010)
tail(ts_month_2010_table)
colnames(ts_month_2010)

## Plot times series up to 12.2009 with autoplot()
autoplot(ts_month_2009[, c("Submeter_1", "Submeter_2", "Submeter_3", "Other")]) +
  labs(title="Total Monthly Consumption", x="Year", y="kWh") +
  theme_minimal() +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())

## Plot times series up 11.2010 with autoplot()
autoplot(ts_month_2010[, c("Submeter_1", "Submeter_2", "Submeter_3", "Other")]) +
  labs(title="Total Monthly Consumption", x="Year", y="kWh") +
  theme_minimal() +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())


## Plot times series with plot.ts()
plot.ts(ts_month_2009[, c("Submeter_1", "Submeter_2", "Submeter_3", "Other")], xlab = "Time", ylab = "kWh", main = "Total Monthly Consumption")

## Plot times series with ggsubseriesplot()
ggsubseriesplot(ts_month_2009[,"GAP"]) +
  ylab("kWh")+ggtitle("Seasonal subseries plot: total monthly consumption")

## Plot times series with ggseasonplot()
ggseasonplot(ts_month_2009[,"GAP"], year.labels=FALSE, continuous=TRUE, polar = TRUE)+ylab("kWh")+ggtitle("Seasonal subseries plot: total monthly consumption")


#### Linear Regression ####
## Fit linear model to monthly time series for Submeter 3
fit_month_2009_SM3 <- tslm(ts_month_2009[,7] ~ trend + season)
fit_month_2010_SM3 <- tslm(ts_month_2010[,7] ~ trend + season)

## Statistical summary of monthly linear model
summary(fit_month_2009_SM3)
glance(fit_month_2009_SM3)
summary(fit_month_2010_SM3)
glance(fit_month_2010_SM3)


## Comaparison plot of fitted and actual date
autoplot(ts_month_2010[,"Submeter_3"], series="Actual") +
  autolayer(fit_month_2010_SM3, series="Fitted") +
  xlab("Year") + ylab("kWh") +
  ggtitle("Actual versus Fitted consumption") +
  guides(colour=guide_legend(title=" "))



## Scatterplot of fitted and actual data
cbind(Actual=ts_month_2010[,"Submeter_3"],
      Fitted = fitted(fit_month_2010_SM3)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Actual, y=Fitted)) +
  geom_point() +
  ylab("Fitted") +
  xlab("Actual") +
  ggtitle("Actual versus Fitted consumption") +
  geom_abline(intercept=0, slope=1)

## Residuals check
checkresiduals(fit_month_2010_SM3)


## Forecast for Submeter 3. Forecast ahead 6 time periods (months) 
forecastfit2009SM3 <- forecast(fit_month_2009_SM3, h=11, level=c(80,95))
forecastfit2010SM3 <- forecast(fit_month_2010_SM3, h=11, level=c(80,95))

## Plot the forecast for Submeter 3
plot(forecastfit2009SM3)
plot(forecastfit2010SM3)

plot(forecastfit2009SM3, showgap=FALSE, include=4,
     shadecols=c('slategray3','slategray'),
     xlab ='Year',
     ylab=' kWh',
     main='11-Month Forecast of Monthly Energy Consumption')

## Comaparison plot of fitted and actual date
autoplot(ts_month_2010[,"Submeter_3"], series="Actual") +
  autolayer(forecastfit2010SM3, series="Fitted") +
  xlab("Year") + ylab("kWh") +
  ggtitle("Actual versus Fitted consumption") +
  guides(colour=guide_legend(title=" "))



## Summary of 11-month forecast
tidy(forecastfit2009SM3)


#### Decomposition of time series object ####
## Decompose Submeter 3 into trend, seasonal and remainder
components_ts_month_2009 <- decompose(ts_month_2009[, "Submeter_3"])
## Plot decomposed Submeter 3 
plot(components_ts_month_2009)
## Check summary statistics for decomposed sub-meter 3 
summary(components_ts_month_2009)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
ts_month_2009_SM3_Adjusted <- ts_month_2009[, "Submeter_3"] - components_ts_month_2009$seasonal
autoplot(ts_month_2009_SM3_Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(ts_month_2009_SM3_Adjusted))

## Holt Winters Exponential Smoothing & Plot
ts_month_2009_SM3_HW <- HoltWinters(ts_month_2009_SM3_Adjusted, beta=FALSE, gamma=FALSE)
plot(ts_month_2009_SM3_HW, ylim = c(150, 350))

## HoltWinters forecast & plot
ts_month_2009_SM3_HWforcast <- forecast(ts_month_2009_SM3_HW, h=11)
plot(ts_month_2009_SM3_HWforcast, ylab= "kWh", xlab="Time - Submeter 3")


## Forecast HoltWinters with diminished confidence levels
ts_month_2009_SM3_HWforcastC <- forecast(ts_month_2009_SM3_HW, h=11, level=c(10,25))
## Plot only the forecasted area
plot(ts_month_2009_SM3_HWforcastC, ylim = c(310, 360), ylab= "kWh", xlab="Time - Submeter 3", start(2010))

#### DATA PARTITION and  FORECASTING ####

## create data partition from ts_month (2007-2010.11)
ts_month <- ts(consumption_month, frequency = 12, start=c(2007,1))
train <- window(ts_month[,"GAP"], start = c(2007,1), end = c(2010,1))
test <- window(ts_month[,"GAP"], start= c(2010,1))

## Linear regression
modelTSLM <-tslm(train ~ trend + season)
predictionTSLM = forecast(modelTSLM, h=11) # forecast 11 months ahead (start point train)
predictionTSLM
plot(predictionTSLM)
accuracy(predictionTSLM, test) #check metrics comparing prediction in test

## Classical decomposition
classicalDecomposition <-
  decompose(train, type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of GAP")

## STL decomposition
stlDecomposition <-stl(train, s.window = 12)
autoplot(stlDecomposition) +xlab("Year") +
  ggtitle("STL decomposition of GAP")


## Halt-Winter
# Seasonal adjusting GAP by subtracting the seasonal component & plot
train_adjusted <- train - stlDecomposition$seasonal
autoplot(train_adjusted)


modelHW <- HoltWinters(train)
predictionHW <- forecast(modelHW, h= 11) # forecast 11 months ahead (start point train)
predictionHW
plot(predictionHW)
autoplot(predictionHW)
accuracy(predictionHW, test) 

## Comaparison plot of fitted and actual date
autoplot(ts_month[,"GAP"], series="Actual") +
  autolayer(predictionHW, series="HW", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Actual versus Fitted consumption") +
  guides(colour=guide_legend(title=" "))




