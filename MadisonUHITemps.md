this function will grab a year of the temperature data and summarize it produces a table for each year, but it has a dependency on the tidyverse package

```
install.packages("tidyverse")
library("tidyverse")

bydate <- function(url){
  t <- read.csv(url(url),header = T) %>%
  group_by(., sample_date, SID) %>% summarise(
    maxT = max(VALUE, na.rm=T),
    minT = min(VALUE, na.rm=T),
    meanT = mean(VALUE, na.rm=T)) %>%
    inner_join(., read.csv(url("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-ntl.324.2&entityid=1ff0131966f1881c2c062c4dc36131b0"), header = T)) 
  t$year=matrix(unlist(strsplit(as.character(t$sample_date), "-")), ncol=3, byrow=T)[,1] 
  t$month=matrix(unlist(strsplit(as.character(t$sample_date), "-")), ncol=3, byrow=T)[,2]
  t$day=matrix(unlist(strsplit(as.character(t$sample_date), "-")), ncol=3, byrow=T)[,3]
    
  return(t)
}
```

run like this for year 2013

```
foo <- bydate("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-ntl.324.2&entityid=ec015fbe8de882f2e987d846c071c5c8")
```
and you can plot monthly averages like this
```
foo %>% group_by(SID, LATITUDE, LONGITUDE, month) %>% summarise(min= mean(minT, na.rm=T),max = mean(maxT, na.rm=T), avg = mean(meanT, na.rm=T)) %>%
  ggplot(.,aes(x=LONGITUDE, y=LATITUDE, color=max)) + geom_point() + facet_wrap(~month) + scale_colour_gradient(low = "blue", high = "red") 
```
![alt text](https://github.com/vargovargo/LvilleHeatModeling/blob/master/Rplot.png "Example Monthly Plot")

you can loop through all the years and save to Desktop with this
```  
urls <- c("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-ntl.324.2&entityid=6ef6f0fe0b7d07a564d8985c4d2be677",
          "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-ntl.324.2&entityid=ec015fbe8de882f2e987d846c071c5c8",
          "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-ntl.324.2&entityid=a181213c454b5310aa6463548e154be7",
          "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-ntl.324.2&entityid=6e8db0944a79fc32b7294d9f7da3f7b0",
          "https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-ntl.324.2&entityid=792ce0cd4c19955b3036fabcee36f507"
          )
for(i in 1:5){
   foo <- bydate(urls[i])
   write.csv(foo,paste("~/Desktop/",2011+i,"madisonUHITemps.csv", sep=""), row.names=F)
}
```
