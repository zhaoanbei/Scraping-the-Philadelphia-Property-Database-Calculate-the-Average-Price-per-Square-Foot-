#SEARCH NYC REAL ESTATE RECORDS BY NAME

library('RSelenium')
library('rvest')

setwd("C:/Users/zhaoa/Documents/GitHub/MUSA-620-Week-7-master")
#### CONNECTING VIA SAUCE LABS SERVER

#user <- "anbei1" # Your Sauce Labs Username
#key <- "a8f57249-523f-425d-bee7-2a3bc167dda1" # Your Sauce Labs access key (should be in a form similar to this one)

#port <- 80
#ip <- paste0(user, ':', key, "@ondemand.saucelabs.com")
#rdBrowser <- "chrome" #The brower, version and platform here were chosen arbitrarily. Choose another if you want: https://saucelabs.com/platforms
#version <- "65"
#platform <- "Windows 10"
#extraCapabilities <- list(name = "RSelenium", username = user
#                          , accessKey = key, tags = list("RSelenium-vignette", "OS/Browsers-vignette"))
#remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, browserName = rdBrowser,
#                          version = version, platform = platform,
#                          extraCapabilities = extraCapabilities)
#C:\Users\zhaoa\Documents\GitHub\MUSA-620-Week-7-master>java -jar ./selenium-server-standalone-3.11.jar -port 3000


remDr<-remoteDriver(remoteServerAddr = "localhost",port = 3000, browserName = "chrome")
remDr$open()
remDr$navigate("http://property.phila.gov/")

# PLEASE USE THE TEST DATA UNTIL YOU ARE READY TO MAKE THE FINAL RUN
#condos <- read.csv("condos-rittenhouse.csv") 

myresults <- data.frame()

for (i in 1:nrow(condos)){
  #Find fields
  addressField <- remDr$findElement("css selector", "#search-address")
  unitField <- remDr$findElement("css selector", "#search-unit")
  # addr and unit from csv
  
  addr = as.character(condos[i,1]) 
  addr
  unit = as.character(condos[i,2]) 
  unit
  # fill in
  addressField$sendKeysToElement(list(addr))
  unitField$sendKeysToElement(list(unit))
  # press enter
  
  unitField$sendKeysToElement(list(key = 'enter'))
  Sys.sleep(10)
  # Find 
  website = read_html(remDr$findElement("css selector", "html")$getElementAttribute("innerHTML")[[1]])
  #[data-hook=valuation] > tr:nth-child(1) > td:nth-child(2) .tablesaw-cell-content
  val4 <- html_nodes(website,"[data-hook=valuation] > tr:nth-child(1) > td:nth-child(2) .tablesaw-cell-content") %>% html_text()
  sqft2<-html_nodes(website, "#maincontent > div:nth-child(3) > div.property-side.large-10.columns > div.panel.mbm > div:nth-child(6) > div.medium-14.columns > strong") %>% html_text()
  # bind
  tryCatch(thisresult <- data.frame(addr,unit,val4,sqft2),error = function(err){0})
  myresults <- rbind(myresults,thisresult)
  remDr$navigate("http://property.phila.gov/")
  Sys.sleep(10)
}
write.csv(myresults, file = "result.csv")

result<- read.csv("result.csv")
setwd("C:/Users/zhaoa/Documents/GitHub/MUSA-620-Week-7-master")
result<- read.csv("result.csv")

result$price_per_sf <- result$val/result$sqft
result2 <- result[apply(result!=0, 1, all),]

# 3.2  geocoding the address
install.packages("googleway")
library("googleway")
library("sf")
re <- read.csv("Result3.csv")
re2 <-re[,c(3:9)]
re3<- na.omit(re2)
library(dplyr)

re4 <- re3 %>%
  group_by(addr) %>%
  dplyr::summarise(mean = mean(price_per_sf)) 


re4$addr<-as.character(re4$addr)



re4$lng<-NA
re4$lat<-NA
re4$formatted_address<-NA

#result2<-result2[c(1:5,8,9),]
theplotgeocode<-re4$formatted_address

for (i in 1:nrow(re4)){
  theplotgeocode <- google_geocode(address = re4[i,]$addr,
                                   key= "AIzaSyBvlR72ApYLwCH4kBndo-eq7QeIxWH5c08",
                                   simplify = TRUE, region = "us")
  theplotgeocode$results$formatted_address
  coord<-theplotgeocode$results$geometry$location
  coord
  re4[i,]$lat<-coord$lat
  re4[i,]$lng<-coord$lng
  re4$formatted_address[i] <-theplotgeocode$results$formatted_address
} # Several adrresses have multi-locations so 

write.csv(re4, file = "Result3.csv")


library(ggmap)
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

baseMap<-get_map(location = c(lon = -75.172, lat = 39.949), 
                 source = "stamen", 
                 zoom = 17, 
                 maptype= 'toner')
library(ggplot2)
re<- read.csv('C:/Users/zhaoa/Documents/GitHub/MUSA-620-Week-7-master/mean.csv')
re$mean <- floor(re$mean)
Map <- 
  ggmap(baseMap) +
  geom_point(data = re, shape=19,
             aes(x=lng, y=lat, color=mean, size=mean), 
             size = 11) + 
  scale_color_distiller(palette = "RdPu")+
  geom_text(data = re, aes(x=lng, y=lat, label=addr),angle = 30,check_overlap = T,size = 3,
            hjust = -0.2)+
  labs(title="Mean Housing Price per Square Foot, Pennsylvania",
       subtitle="Source: http://property.phila.gov/") +
  mapTheme()



Map
