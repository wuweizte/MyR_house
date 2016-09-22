library(ggplot2)
library(mapdata)
library(maps)
library(Hmisc)

world.data <- map_data("world")
china.data <- world.data[world.data$region == "China",]

setwd("d:/MyR/house")
city.data <- read.csv("city_lat_long.csv")

price.data <- read.csv("ershouzhuzhai2016-8.csv")
price.data[[1]] <- gsub("����", "",  price.data[[1]])
price.data[[1]] <- gsub(" ", "",  price.data[[1]])
names(price.data) <- c("chinese.name", "price", "price.tongbi")

final.data <- merge(city.data, price.data, by.x = "chinese.name", by.y = "chinese.name")
final.data$`��ǰ�۸�ָ��` <- cut2(final.data$price, cuts = c(100, 105,115, 125))

final.data$`ͬ������ͬ�¼۸�` <- cut2(final.data$price.tongbi, cuts = c(99,101))
final.data$`ͬ������ͬ�¼۸�` <- factor(final.data$`ͬ������ͬ�¼۸�`, labels = c("�µ�","��ƽ","����"))

plot.title.text <- "'90' * m ^ 2 * '���¶���סլ�۸�ָ���ĵ������ֲ�ͼ��2016-8��'"

p <- ggplot(china.data, aes(long, lat)) +
        geom_polygon(aes(group = group), fill = "white", color = "black") +
        
        geom_point(aes(x=long,y=lat,
                       col = `��ǰ�۸�ָ��`, shape=`ͬ������ͬ�¼۸�`, fill = `��ǰ�۸�ָ��`),
                   size = 3,
                   data=final.data) +

        scale_colour_manual(values = c("royalblue", "skyblue", "pink1", "red", "purple")) +
        scale_shape_manual(values = c(25, 22, 24)) +
        scale_fill_manual(values = c("royalblue", "skyblue", "pink1", "red", "purple")) +
        
        geom_text(aes(x=long,y=lat, label = chinese.name),check_overlap = FALSE,
                  col="darkgray", size = 3, hjust = 0, nudge_x = 0.5,
                  data=city.data) +
        
        coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
        xlab("����") +
        ylab("γ��") +
        ggtitle("")  +
        annotate("text", x = mean(china.data$long), y = 60,
                 label = plot.title.text,
                 vjust = 0, size = 6, parse = TRUE) 
        
print(p)



##Ϊ�������Ӿ�γ����Ϣ

# city.translation <- read.csv("city_translation.csv",stringsAsFactors = FALSE)
# 
# city.house <-  read.csv("ershouzhuzhai2016-1.csv",stringsAsFactors = FALSE)[1]
# city.house[[1]] <- gsub("����", "",  city.house[[1]])
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