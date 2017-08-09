w2017 <-sw2017
w2017$dow <-wday(w2017$Date,label=TRUE)
w2017$dow = with(w2017, factor(dow, levels = rev(levels(dow))))
w2017$week<-week(w2017$Date)
w2017$weeks <- format(w2017$Date, "%W") # was originally "%Y/%W"
w2017$weeks <- factor(w2017$weeks, levels = unique(w2017$weeks))

w2017$weekStart <- w2017$Date - as.POSIXlt(w2017$Date)$wday

w2017$Month <-month(w2017$Date,label=TRUE)

p2<-ggplot(w2017,aes(weekStart,dow,fill=Events))+
  geom_tile(colour="white",size=.1) +
  scale_fill_brewer(type="qual",palette = "Paired")+
 # scale_fill_viridis(discrete = TRUE,option = "C", direction = -1)+
  guides(fill=guide_legend(title="Max Temp C"))+
  scale_x_date(date_breaks = "1 week",date_labels="%d-%b-%y")+
  theme_minimal(base_size = 10)+
  ggExtra::removeGrid()+ggExtra::rotateTextX()+
  ggtitle("Inverness Main Daily Weather Event 2017",subtitle = "")+
  labs(x="Week Beginning", y=NULL)+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  theme(legend.position="bottom")
p2<-p2+facet_wrap(~Month,nrow = 3,scales="free")
p2
ggsave("2017 Main Daily Weather Event.png",height = 5.84,width = 8.74)
