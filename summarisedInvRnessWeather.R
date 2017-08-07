library(weatherData)
library(tidyverse)
library(scales)
library(lubridate)

sw2016 <- getSummarizedWeather("EGPE",start_date = "2016-01-01",end_date = "2016-12-31",opt_all_columns = TRUE)
sw2015 <- getSummarizedWeather("EGPE",start_date = "2015-01-01",end_date = "2015-12-31",opt_all_columns = TRUE)
sw2014 <- getSummarizedWeather("EGPE",start_date = "2014-01-01",end_date = "2014-12-31",opt_all_columns = TRUE)
sw2013 <- getSummarizedWeather("EGPE",start_date = "2013-01-01",end_date = "2013-12-31",opt_all_columns = TRUE)
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


swdata<- bind_rows(sw2016,sw2015,sw2014,sw2013,sw2012,sw2011,
sw2010, sw2009,sw2008,sw2007,sw2006,sw2005,
sw2004,sw2003,sw2002)

######end of import and processing ##############
saveRDS(data, file="swInvdf.Rda")

####### start here once data processed#########


swdata <- readRDS(file="swInvdf.Rda")

p <- ggplot(swdata,aes(Date,Mean_TemperatureC,colour=Events))+geom_point(alpha=0.3) +
  facet_wrap(~Events)+
  ggtitle(label = "Mean Temperatures(C) Inverness 2002 - 2016",
          subtitle = "https://www.wunderground.com")
p

