
library(weatherData)
library(tidyverse)
library(scales)
library(lubridate)


years <- data_frame(weatheryear = seq(2002,2016,by=1))


yearlyweather <- function(weatheryear,station = "EGPE") {
  getSummarizedWeather(station,start_date = paste0(weatheryear,"-01-01"),
                       end_date = paste0(weatheryear, "-12-31"),
                       opt_all_columns = TRUE)
}

sw2016 <- yearlyweather(2016)
sw2015 <- yearlyweather(2015)
sw2014 <- yearlyweather(2014)
sw2013 <- yearlyweather(2013)
sw2017 <- getSummarizedWeather("EGPE",start_date = "2017-01-01",end_date = "2017-07-31",opt_all_columns = TRUE)


sw2012 <- getSummarizedWeather("EGPE",start_date = "2012-01-01",end_date = "2012-12-31",opt_all_columns = TRUE)
sw2011 <- getSummarizedWeather("EGPE",start_date = "2011-01-01",end_date = "2011-12-31",opt_all_columns = TRUE)
sw2010 <- getSummarizedWeather("EGPE",start_date = "2010-01-01",end_date = "2010-12-31",opt_all_columns = TRUE)
sw2009 <- getSummarizedWeather("EGPE",start_date = "2009-01-01",end_date = "2009-12-31",opt_all_columns = TRUE)
sw2008 <- getSummarizedWeather("EGPE",start_date = "2008-01-01",end_date = "2008-12-31",opt_all_columns = TRUE)
sw2007 <- getSummarizedWeather("EGPE",start_date = "2007-01-01",end_date = "2007-12-31",opt_all_columns = TRUE)
sw2006 <- getSummarizedWeather("EGPE",start_date = "2006-01-01",end_date = "2006-12-31",opt_all_columns = TRUE)
sw2005 <- getSummarizedWeather("EGPE",start_date = "2005-01-01",end_date = "2005-12-31",opt_all_columns = TRUE)
sw2004 <- getSummarizedWeather("EGPE",start_date = "2004-01-01",end_date = "2004-12-31",opt_all_columns = TRUE)
sw2003 <- getSummarizedWeather("EGPE",start_date = "2003-01-01",end_date = "2003-12-31",opt_all_columns = TRUE)
sw2002 <- getSummarizedWeather("EGPE",start_date = "2002-01-01",end_date = "2002-12-31",opt_all_columns = TRUE)

sw2002$Date <- as.Date(sw2002$Date, format ="%Y-%m-%d")
sw2003$Date <- as.Date(sw2003$Date, format ="%Y-%m-%d")
sw2004$Date <- as.Date(sw2004$Date, format ="%Y-%m-%d")
sw2005$Date <- as.Date(sw2005$Date, format ="%Y-%m-%d")

sw2006$Date <- as.Date(sw2006$Date, format ="%Y-%m-%d")
sw2007$Date <- as.Date(sw2007$Date, format ="%Y-%m-%d")
sw2008$Date <- as.Date(sw2008$Date, format ="%Y-%m-%d")
sw2009$Date <- as.Date(sw2009$Date, format ="%Y-%m-%d")

sw2010$Date <- as.Date(sw2010$Date, format ="%Y-%m-%d")
sw2011$Date <- as.Date(sw2011$Date, format ="%Y-%m-%d")
sw2012$Date <- as.Date(sw2012$Date, format ="%Y-%m-%d")
sw2013$Date <- as.Date(sw2013$Date, format ="%Y-%m-%d")

sw2014$Date <- as.Date(sw2014$Date, format ="%Y-%m-%d")
sw2015$Date <- as.Date(sw2015$Date, format ="%Y-%m-%d")
sw2016$Date <- as.Date(sw2016$Date, format ="%Y-%m-%d")
sw2017$Date <- as.Date(sw2017$Date, format ="%Y-%m-%d")


swdata<- bind_rows(sw2016,sw2015,sw2014,sw2013,sw2012,sw2011,
                   sw2010, sw2009,sw2008,sw2007,sw2006,sw2005,
                   sw2004,sw2003,sw2002,sw2017)

######end of import and processing ##############
path <- "C:/Users/datag/Documents/RScripts/InvernessWeather"

saveRDS(data, file="sw.RData")

####### start here once data processed#########


swdata <- load("sw.RData")
swdata$Year <- lubridate::year(swdata$Date)


p <- ggplot(sw2016,aes(Date,Mean_TemperatureC,colour=Events))+
  geom_point(alpha=0.7) +
  #scale_x_date(date_breaks ="1 month",labels="%y-%m-$d")+
  theme_bw()+
  #facet_wrap(~Events)+
  ggtitle(label = "Mean Daily Temperatures (Celsius) Inverness 2016",
          subtitle = "https://www.wunderground.com")+
  labs(x="")+
  ggExtra::rotateTextX()+
  theme(legend.position="bottom")+
  scale_color_brewer(type="qual",palette = "Paired")
p

p <- ggplot(sw2017,aes(Date,Mean_TemperatureC,colour=Events))+
  geom_point(alpha=0.7) +
  #scale_x_date(date_breaks ="1 month",labels="%y-%m-$d")+
  theme_bw()+
  #facet_wrap(~Events)+
  ggtitle(label = "Mean Daily Temperatures (Celsius) Inverness 2017 to date",
          subtitle = "https://www.wunderground.com")+
  labs(x="")+
  ggExtra::rotateTextX()+
  theme(legend.position="bottom")+
  scale_color_brewer(type="qual",palette = "Paired")
p

