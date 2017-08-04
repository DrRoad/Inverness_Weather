w2016$dow <-wday(w2016$Date,label=TRUE)
w2016$dow = with(w2016, factor(dow, levels = rev(levels(dow))))
w2016$week<-week(w2016$Date)
w2016$weeks <- format(w2016$Date, "%W") # was originally "%Y/%W"
w2016$weeks <- factor(w2016$weeks, levels = unique(w2016$weeks))

w2016$weekStart <- w2016$Date - as.POSIXlt(w2016$Date)$wday

w2016$Month <-month(w2016$Date,label=TRUE)

p2<-ggplot(w2016,aes(weekStart,dow,fill=Max_TemperatureC))+
  geom_tile(colour="white",size=.1) +
  scale_fill_viridis(discrete = FALSE,option = "C", direction = -1)+
  guides(fill=guide_legend(title="Max Temp C"))+
  scale_x_date(date_breaks = "1 week",date_labels="%d-%b-%y")+
  theme_minimal(base_size = 10, base_family = "Trebuchet MS")+
  ggExtra::removeGrid()+ggExtra::rotateTextX()+
  ggtitle("Inverness UK Daily Max Temps 2016",subtitle = "")+
  labs(x="Week Beginning", y=NULL)+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  theme(legend.position="none")
p2<-p2+facet_wrap(~Month,nrow = 3,scales="free")
p2
ggsave("2016-11-27-MetricCalendarHeatmap.png",height = 5.84,width = 8.74)
