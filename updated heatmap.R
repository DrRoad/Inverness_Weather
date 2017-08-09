df<- tbl_df(swdata)
#rm(data)


df<- df %>% mutate(year = year(Date),
                   month = lubridate::month(Date),
                   week = week(Date),
                   day = lubridate::day(Date),
                   wday = lubridate::wday(Date,label=TRUE),
                   yday = lubridate ::yday(Date))

df.means <-df %>% select(day,wday,week,month,year,Events)
df.means$dow = with(df.means, factor(wday, levels = rev(levels(wday))))

p <-ggplot(df.means,aes(week,dow,fill=Events))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_brewer(type="qual",palette = "Paired")
p <-p + facet_wrap(~year,ncol=2)
p <-p + scale_x_continuous(breaks = unique(df.means$week))
#p <-p + scale_x_date(date_breaks ="2 weeks",labels="%d-%m-$y")
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= "Main Daily Weather Events Inverness UK, 2002 : 2017", x="Week #", y="")
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
p 
ggsave("Main Daily Weather Events 2002 2017.png",height = 8.5,width = 20)

