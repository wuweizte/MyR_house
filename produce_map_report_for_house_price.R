library(ggplot2)
library(mapdata)
library(maps)
library(Hmisc)

world.data <- map_data("world")
china.data <- world.data[world.data$region == "China",]

setwd("d:/MyR/house")
city.data <- read.csv("city_lat_long.csv")

price.data <- read.csv("ershouzhuzhai2017-11.csv")
price.data[[1]] <- gsub("　　", "",  price.data[[1]])
price.data[[1]] <- gsub(" ", "",  price.data[[1]])
names(price.data) <- c("chinese.name", "price", "price.tongbi")

final.data <- merge(city.data, price.data, by.x = "chinese.name", by.y = "chinese.name")
final.data$`当前价格指数` <- cut2(final.data$price, cuts = c(100, 110,125, 140))

final.data$`同比上年同月价格` <- cut2(final.data$price.tongbi, cuts = c(98,102))

## It is found that no city price declines comparing to the same month last year since June,2016

if(sum(final.data$price.tongbi < 98) > 0){

        final.data$`同比上年同月价格` <- factor(final.data$`同比上年同月价格`, labels = c("下跌","持平","上涨"))
        shape.ind <- c(25, 22, 24)
}else{
        final.data$`同比上年同月价格` <- factor(final.data$`同比上年同月价格`, labels = c("持平","上涨"))
        shape.ind <- c(22, 24)
}


plot.title.text <- "'90' * m ^ 2 * '以下二手住宅价格指数的地理化分布图（2017-11）'"

p <- ggplot(china.data, aes(long, lat)) +
        geom_polygon(aes(group = group), fill = "white", color = "black") +
        
        geom_point(aes(x=long,y=lat,
                       col = `当前价格指数`, shape=`同比上年同月价格`, fill = `当前价格指数`),
                   size = 3,
                   data=final.data) +

        scale_colour_manual(values = c("royalblue", "skyblue", "pink1", "red", "purple")) +

        scale_shape_manual(values = shape.ind) +
        scale_fill_manual(values = c("royalblue", "skyblue", "pink1", "red", "purple")) +
        
        geom_text(aes(x=long,y=lat, label = chinese.name),check_overlap = FALSE,
                  col="darkgray", size = 3, hjust = 0, nudge_x = 0.5,
                  data=city.data) +
        
        coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
        xlab("经度") +
        ylab("纬度") +
        ggtitle("")  +
        annotate("text", x = mean(china.data$long), y = 60,
                 label = plot.title.text,
                 vjust = 0, size = 6, parse = TRUE) 
        
print(p)



##为城市添加经纬度信息

# city.translation <- read.csv("city_translation.csv",stringsAsFactors = FALSE)
# 
# city.house <-  read.csv("ershouzhuzhai2016-1.csv",stringsAsFactors = FALSE)[1]
# city.house[[1]] <- gsub("　　", "",  city.house[[1]])
# city.house[[1]] <- gsub(" ", "",  city.house[[1]])
# 
# required.city <- city.translation[city.translation[[1]] %in% city.house[[1]],]
# required.city[[2]] <- gsub("'", "",  required.city[[2]])
# required.city[[2]] <- tolower(required.city[[2]])
# 
# city.data <- world.cities[world.cities$country.etc == "China", ]
# city.data[[1]] <- tolower(city.data[[1]])
# prepare.city <- city.data[city.data[[1]] %in% required.city[[2]],]
# prepare.city <- prepare.city[!(row.names(prepare.city) %in% c(13640, 16316, 16332, 21740, 21742)), ]
# 
# prepare.city <- rbind(prepare.city, data.frame(name = "xiangyang", country.etc = "China", 
#                                pop = 0, lat = 32, long = 112, capital = 0))
# 
# required.city[!required.city[[2]] %in% prepare.city[[1]],]
# 
# names(required.city) <- c("chinese.name", "english.name")
# 
# result <- merge(required.city, prepare.city, by.x = "english.name", by.y = "name")
# 
# write.csv(result, "city_lat_long.csv")
