library(gtrendsR)
library(dplyr)
library(ggplot2)
library(ggforce)
library(spData)
library(ggrepel)
library(tmap)
library(lubridate)

trends <- gtrends(keyword = c("Iran"),time="now 7-d")

View(trends)
View(trends$interest_over_time)
View(trends$interest_by_country)
View(trends$interest_by_region)
View(trends$interest_by_city)
View(trends$interest_by_dma)
View(trends$related_topics)
View(trends$related_queries)

summary(trends)
summary(trends$interest_over_time)
summary(trends$interest_by_country)
summary(trends$interest_by_region)
summary(trends$interest_by_city)
summary(trends$interest_by_dma)
summary(trends$related_topics)
summary(trends$related_queries)


trends_timeseries<- ts(trends$interest_over_time,start = decimal_date(dmy("23-11-2022")),
                         frequency =0.5)  
# Printing the timeseries data.  
print(trends_timeseries)  
plot(trends_timeseries,xlab ="Weekly Data",
     ylab ="count",
     main ="Iran search trends",
     col.main ="darkgreen")

dev.off()





iot<-trends$interest_over_time
iot %>% +top_n(5,hits) %>% + arrange(desc(hits))


iot%>% ggplot(aes(x=date,y = hits,group=keyword ,color = keyword))  +
                                                   theme_bw()+
                                      labs(title = "Google Web searches for 'Iran' in last 7 Days",
                                  caption = "Obs: 3/22 was the day with the most searches",
                                                            x= NULL, y = "Interest")+
                                                       ggforce::facet_zoom(xlim = c(as.POSIXct(as.Date("2022-11-25")),as.POSIXct(as.Date("2022-11-25")))) +
                                                       geom_smooth(span=0.1,se=FALSE) + geom_vline(xintercept = as.POSIXct(as.Date("2022-11-25")),color = "red", lwd = 0.5,linetype="dashed")+
                                                       theme(legend.position = "none") +
                                                       geom_point(color="black")+
                                                       geom_label_repel(data = subset(iot2020, hits == 100),
                                                                        aes(label = as.character(date)),
                                                                        size = 5,
                                                                        box.padding = unit(0.35, "lines"),
                                                                        point.padding = unit(0.3, "lines"))



trends$related_queries %>%
                                                       filter(related_queries=="top") %>%
                                                       mutate(value=factor(value,levels=rev(as.character(value))),
                                                              subject=as.numeric(subject)) %>%
                                                       top_n(10,value) %>%
                                                       ggplot(aes(x=value,y=subject,fill="red")) + 
                                                       geom_bar(stat='identity',show.legend = F) + 
                                                       coord_flip() + labs(title="Queries most related with 'Iran'")





countries <- spData::world %>%
               left_join(y=trends$interest_by_country,by = c("name_long" = "location"),keep=T)


tm_shape(countries) +
                                                       tm_fill("hits",
                                                               title = "Iran Keyword Search Interest",
                                                               legend.reverse = T,
                                                               id = "name_long", 
                                                               popup.vars=c(Name = "name_long",Search.Interest = "hits",Population = "pop",Life.Expectancy = "lifeExp", GDP.per.capita = "gdpPercap"))


