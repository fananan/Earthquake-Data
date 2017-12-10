library(readxl)
library(dplyr)
library(ggplot2)
library(gganimate)
library(animation)
library(animation)
library(caTools)
install.packages("animation")
devtools::install_github("dgrtwo/gganimate")
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
install.packages("caTools")
install.packages("Image.Magick")   #package ‘Image.Magick’ is not available (for R version 3.3.2)


data<-read_excel("/Users/fanliu/Desktop/earthquake\ .xlsx")
head(data, 10)

animdata<-data[, c(2,3,4,5,6,16,24)]
animdata<-as.data.frame(animdata)
View(animdata)

animdata$year<-substr(animdata$time, 1,4)


freq<-animdata %>%
  group_by(year,country) %>%
  summarise(mag_mean=mean(mag),count=n(), depth_mean=mean(depth))

freq<-as.data.frame(freq)
View(freq)
nrow(freq)

f<-ggplot(freq, aes(depth_mean, count, size = mag_mean, color = country, frame=year)) + geom_point() 
gganimate(f)
#outliers

hist(freq$depth_mean)
dep<-freq$depth_mean[freq$depth_mean>100]
summary(freq$depth_mean)
boxplot(freq$depth_mean)
#300.0000 117.5000 133.4667 581.2000 395.6333 115.9500 131.0000 102.4883 100.3833

hist(freq$count)
summary(freq$count)
boxplot(freq$count)
quantile(freq$count, 0.9) #90% data is under 257 counts
freq$count[freq$count>257]


#depth
row_outlier1<-NULL
for (i in length(dep)){
  row_outlier1[i]<-which(grepl(dep[i], freq$depth_mean))
}
row_outlier1 #the reason it doesn't work is dep[i] negelects the digits
#300.0000 117.5000 133.4667 581.2000 395.6333 115.9500 131.0000 102.4883 100.3833
which(grepl(300.0000, freq$depth_mean)) #6
which(grepl(117.5000, freq$depth_mean)) 
which(grepl(133.4667, freq$depth_mean))
which(grepl(581.2000, freq$depth_mean))
which(grepl(395.6333, freq$depth_mean))
which(grepl(115.9500, freq$depth_mean))
which(grepl(131.0000, freq$depth_mean))
which(grepl(102.4883, freq$depth_mean)) #238
which(grepl(100.3833, freq$depth_mean)) #346

row1<-c(6, 50, 54, 107, 146, 147,193, 238, 346)

#count
c<-freq$count[freq$count>257]
row2<-NULL
for (i in length(freq$count)){
  row2[i]<-which(grepl(c[i], freq$count))
}  #error: which is not logical??????
fun<-function(x){
  which(grepl(x, freq$count)) 
}
#excel
row2<-c(210,
        214,
        230,
        246,
        250,
        254,
        256,
        258,
        262,
        266,
        274,
        278,
        286,
        290,
        294,
        298,
        302,
        306,
        318,
        322,
        330,
        334,
        338,
        344,
        349,
        350,
        356,
        358,
        360,
        362,
        366,
        370,
        372,
        374,
        376,
        378,
        302,
        382,
        386)
class(row2)

row<-c(210,
       214,
       230,
       246,
       250,
       254,
       256,
       258,
       262,
       266,
       274,
       278,
       286,
       290,
       294,
       298,
       302,
       306,
       318,
       322,
       330,
       334,
       338,
       344,
       349,
       350,
       356,
       358,
       360,
       362,
       366,
       370,
       372,
       374,
       376,
       378,
       382,
       386, 6, 50, 54, 107, 146, 147,193, 238, 346)

#remove row 6, 107, 146, 356, 362
freq[row,]
freq2<-freq[-row,]
f2<-ggplot(freq2, aes(depth_mean, count, size = mag_mean, color = country, frame=year)) + geom_point() + theme_bw() +ggtitle("Earthquake Frequency Changes Over Time") 
gganimate(f2, interval=0.3)

install.packages("leaflet")
library(leaflet)
outlier<-read_excel(file.choose())
View(outlier)
leaflet() %>%
  addTiles() %>%
  addCircles(lng=outlier$longitude, lat = outlier$latitude)

      
         







