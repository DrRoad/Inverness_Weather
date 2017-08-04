library(weatherData)
library(ggplot2)
library(dplyr) # data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(lubridate)



#######import and processing from website ###############
w2002<-getWeatherForYear("EGPE",year=2002)
w2003<-getWeatherForYear("EGPE",year=2003)
w2004<-getWeatherForYear("EGPE",year=2004)
w2005<-getWeatherForYear("EGPE",year=2005)
w2006<-getWeatherForYear("EGPE",year=2006)
w2007<-getWeatherForYear("EGPE",year=2007)
w2008<-getWeatherForYear("EGPE",year=2008)
w2009<-getWeatherForYear("EGPE",year=2009)
w2010<-getWeatherForYear("EGPE",year=2010)
w2011<-getWeatherForYear("EGPE",year=2011)
w2012<-getWeatherForYear("EGPE",year=2012)
w2013<-getWeatherForYear("EGPE",year=2013)
w2014<-getWeatherForYear("EGPE",year=2014)
w2015<-getWeatherForYear("EGPE",year=2015)
w2016<-getWeatherForYear("EGPE",year=2016)


w2002$Date <- as.Date(w2002$Date, format ="%Y-%m-%d")
w2003$Date <- as.Date(w2003$Date, format ="%Y-%m-%d")
w2004$Date <- as.Date(w2004$Date, format ="%Y-%m-%d")
w2005$Date <- as.Date(w2005$Date, format ="%Y-%m-%d")

w2006$Date <- as.Date(w2006$Date, format ="%Y-%m-%d")
w2007$Date <- as.Date(w2007$Date, format ="%Y-%m-%d")
w2008$Date <- as.Date(w2008$Date, format ="%Y-%m-%d")
w2009$Date <- as.Date(w2009$Date, format ="%Y-%m-%d")

w2010$Date <- as.Date(w2010$Date, format ="%Y-%m-%d")
w2011$Date <- as.Date(w2011$Date, format ="%Y-%m-%d")
w2012$Date <- as.Date(w2012$Date, format ="%Y-%m-%d")
w2013$Date <- as.Date(w2013$Date, format ="%Y-%m-%d")

w2014$Date <- as.Date(w2014$Date, format ="%Y-%m-%d")
w2015$Date <- as.Date(w2015$Date, format ="%Y-%m-%d")
w2016$Date <- as.Date(w2016$Date, format ="%Y-%m-%d")

data<- bind_rows(w2002,w2003,w2004,w2005,
                 w2006,w2007,w2008,w2009,
                 w2010,w2011,w2012,w2013,
                 w2014,w2015,w2016)

######end of import and processing ##############
saveRDS(data, file="Invdf.Rda")

####### start here once data processed#########

data <- readRDS(file="Invdf.Rda")

df<- tbl_df(data)
rm(data)


df<- df %>% mutate(year = year(Date),
                   month = lubridate::month(Date),
                   week = week(Date),
                   day = lubridate::day(Date),
                   wday = lubridate::wday(Date,label=TRUE),
                   yday = lubridate ::yday(Date))

df.means <-df %>% select(day,wday,week,month,year,Mean_TemperatureC)
df.means$dow = with(df.means, factor(wday, levels = rev(levels(wday))))

p <-ggplot(df.means,aes(week,dow,fill=Mean_TemperatureC))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name=" Mean Daily Temp C",option ="C")
p <-p + facet_wrap(~year,ncol=2)
p <-p + scale_x_continuous(breaks = unique(df.means$week))
#p <-p + scale_x_date(date_breaks ="2 weeks",labels="%d-%m-$y")
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= "Mean Daily Temps Inverness UK", x="Week #", y="")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  ggExtra::removeGrid()

# you will want to expand your plot screen before this bit!
p #awesomeness


df.max <-df %>% select(day,wday,yday,week,month,year,Max_TemperatureC)
df.max$dow = with(df.max, factor(wday, levels = rev(levels(wday))))

p <-ggplot(df.max,aes(week,dow,fill=Max_TemperatureC))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name=" Max Daily Temps C",option ="C")
p <-p + facet_wrap(~year,ncol=2)
p <-p + scale_x_continuous(breaks = unique(df.max$week))
#p <-p + scale_x_date(date_breaks ="2 weeks",labels="%d-%m-$y")
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= "Max Daily Temp Inverness UK", x="Week #", y="")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  ggExtra::removeGrid()

# you will want to expand your plot screen before this bit!
p #awesomeness


df.min <-df %>% select(day,wday,week,month,year,Min_TemperatureC)
df.min$dow = with(df.min, factor(wday, levels = rev(levels(wday))))

p <-ggplot(df.min,aes(week,dow,fill=Min_TemperatureC))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Min Daily Temp C",option ="C")
p <-p + facet_wrap(~year,ncol=2)
p <-p + scale_x_continuous(breaks = unique(df.min$week))
#p <-p + scale_x_date(date_breaks ="2 weeks",labels="%d-%m-$y")
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= "Min Daily Temps Inverness UK", x="Week #", y="")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  ggExtra::removeGrid()

# you will want to expand your plot screen before this bit!
p #awesomeness


#### try day by year
p <-ggplot(df.max,aes(yday,year,fill=Max_TemperatureC))+
  geom_tile(color= "white",size=0.1)+
  scale_fill_viridis(name="# Daily Temps C",option ="C")+
  coord_fixed(ratio = 7)
p <-p + theme_minimal(base_size = 8)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df.max$year))
p <-p + labs(title= "Max Daily Temps Inverness UK", x="Day #", y="")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  ggExtra::removeGrid()

# you will want to expand your plot screen before this bit!
p #awesomeness
