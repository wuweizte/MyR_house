#### Author Comment Part
# modified on 2016-12-20

#### File Descriptiong Part
# 代码目的：用于比较月度房价信息

#### Library Quoting Part
rm(list = ls())

suppressMessages(library(ggplot2))
suppressMessages(library(dplyr, warn.conflicts = FALSE))
suppressMessages(library(grid))

suppressMessages(library(RColorBrewer))
suppressMessages(library(reshape2))
suppressMessages(library(lubridate))

#### Function Definition Part
InputHouseData <- function(arg.start.month, arg.end.month, arg.month.interval) {
        # 调用读取数据文件的子函数读取记录存放到全局列表中
        #
        # Args:
        #   arg.start.month: 绘画针对的起始月份，以单向量形式输入
        #   arg.end.month: 绘画针对的结束月份，以单向量形式输入
        #
        # Returns:
        #   返回已经填好数据的列表
        
        ls.value.data <- list()

        ls.value.data <- InputSpecifiedTypeData(ls.value.data, 
                                                "xinjianshangpinfang", 
                                                arg.start.month, 
                                                arg.end.month, 
                                                arg.month.interval)

        ls.value.data <- InputSpecifiedTypeData(ls.value.data, 
                                                "ershouzhuzhai", 
                                                arg.start.month, 
                                                arg.end.month, 
                                                arg.month.interval)
        
        return(ls.value.data)
}

InputSpecifiedTypeData <- function(arg.ls.value.data, 
                                   arg.type, 
                                   arg.start.month, 
                                   arg.end.month, 
                                   arg.month.interval) {
        # 读取指定类型的数据文件，存放数据到全局列表中
        #
        # Args:
        #   arg.ls.value.data  全局列表的值引用
        #   arg.type: 读取的文件类型
        #   arg.start.month: 绘画针对的起始月份，以单向量形式输入
        #   arg.end.month: 绘画针对的结束月份，以单向量形式输入
        #
        # Returns:
        #   返回已经填好数据的列表
        
        ls.value.data <- arg.ls.value.data
        
        # browser()
        if(arg.month.interval > 1){
                month.interval <- paste0(arg.month.interval," months")
        }else{
                month.interval <- "month"
        }
        month.list <- seq(arg.start.month, arg.end.month, month.interval)
        
        for(i in 1:length(month.list)){
                var.year <- year(month.list[i])
                var.month <- month(month.list[i])
                
                csv.file.name <- paste(arg.type, var.year, "-", var.month,  
                                       ".csv", sep = "")
                csv.file.content <- read.csv(csv.file.name, stringsAsFactors = FALSE)
                
                csv.file.content <- csv.file.content[, 1:2]
                names(csv.file.content) <- c("city", "price")
                if(var.month > 9) {
                        csv.file.content[["time"]] <- paste(var.year, 
                                                            "-", 
                                                            var.month,
                                                            sep = "")         
                }else{
                        csv.file.content[["time"]] <- paste(var.year, 
                                                            "-0", 
                                                            var.month,
                                                            sep = "")
                }
                
                        
                if (i == 1) {
                        ls.value.data[[arg.type]] <- csv.file.content   
                } else {
                        ls.value.data[[arg.type]] <-
                                rbind(ls.value.data[[arg.type]], csv.file.content)
                }
        }
        
        data.source <- ls.value.data[[arg.type]]
        data.source <- data.source[order(data.source$city, data.source$time),]
        data.source$city <- gsub("　　", "",  data.source$city) 
        
        ls.value.data[[arg.type]] <- data.source
        
        return(ls.value.data)
}

DrawSpecifiedCitiesPlot <- function(arg.ls.value.data, arg.type, arg.cities){
        # 为指定城市出框图
        #
        # Args:
        #   arg.ls.value.data  已经存储了新建商品住宅或二手房月度价格指数的全局列表的值引用
        #   arg.type:  出图是针对新建商品住宅 or 二手房
        #   arg.cities: 指定的城市，以向量形式输入
        #
        # Returns:
        #   返回返回ggplot图层对象
        
        data.source <- arg.ls.value.data[[arg.type]]

        data.source <- data.source[(data.source$city %in% arg.cities),]
        if (arg.type == "xinjianshangpinfang") {
                plot.title.text <- "'90' * m ^ 2 * '以下新建商品房价格指数'"
        } else {
                plot.title.text <- "'90' * m ^ 2 * '以下二手住宅价格指数'"
        }
        
        # browser()
        p <- ggplot(data.source, aes(x = city, y = price, color = time, fill = time)) +
                geom_bar(position = "dodge", stat = "identity", alpha = .5) +

                geom_text(aes(label = format(price, nsmall = 1),
                              vjust = -0.9 ),
                          position = position_dodge(.9),size = 3,
                          colour = "black") +
                xlab("注：2015年全年平均价格水平对应指数为100") +
                ylab("价格指数") +
                ggtitle("   ")  +
                annotate("text", x = mean(1:length(arg.cities)), y = Inf, 
                         label = plot.title.text, 
                         vjust = 1.5, size = 7, parse = TRUE) +
                coord_cartesian(ylim = c(75, 175))

        return(p)
        
}

FindDensityCurveMedianYCoordinate <- function(arg.data.to.plot){
        # 找到输入数据的中值在对应密度图上的Y坐标
        #
        # Args:
        #   arg.data.to.plot: 输入数据
        #
        # Returns:
        #   输入数据的中值在对应密度图上的Y坐标
        
        r <- density(arg.data.to.plot)
        data.median <- median(arg.data.to.plot)
        seq <- length(r$x[r$x < data.median]) + 1
        return(r$y[seq])
        
}


DrawGlobalPriceCurve <- function(arg.ls.value.data, arg.type){
        # 为新建商品住宅或二手房创建各月密度曲线
        #
        # Args:
        #   arg.ls.value.data  已经存储了新建商品住宅或二手房月度价格指数的全局列表的值引用
        #   arg.type:  出图是针对新建商品住宅 or 二手房
        #
        # Returns:
        #   输入数据的中值在对应密度图上的Y坐标
        
        data.source <- arg.ls.value.data[[arg.type]]
        median.data <- tapply(data.source$price, data.source$time, median)
        
        min.median.month <- names(which.min(median.data))
        min.median.month.price <- median.data[min.median.month]

        max.median.month <- names(which.max(median.data))
        max.median.month.price <- median.data[max.median.month]

        min.price <- floor(min(data.source$price))
        max.price <- ceiling(max(data.source$price))
        
        if (arg.type == "xinjianshangpinfang") {
                plot.title.text <- "'90' * m ^ 2 * '以下新建商品房价格指数的分布密度曲线（70个城市）'"
        } else {
                plot.title.text <- "'90' * m ^ 2 * '以下二手住宅价格指数的分布密度曲线（70个城市）'"
        }

        y.coordinate.min.median <- 
                FindDensityCurveMedianYCoordinate(data.source$price[as.character(data.source$time) == min.median.month])
        y.coordinate.max.median <- 
                FindDensityCurveMedianYCoordinate(data.source$price[as.character(data.source$time) == max.median.month])
        
        p <- ggplot(data.source[c("price", "time")], aes(x = price, colour = time, fill = time)) +
                geom_density(alpha = .5) +
                scale_colour_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2") +
                annotate("segment",
                         x = min.median.month.price,
                         xend = min.median.month.price,
                         y = 0,
                         yend = y.coordinate.min.median,
                         linetype = "dashed",
                         color = "red") +
                annotate("text",
                         x = min.median.month.price,
                         y = y.coordinate.min.median,
                         label = paste(" 月度价格指数中值的最小值出现在 ",
                                 min.median.month, " ，为 ",
                                       format(min.median.month.price, digits = 4),
                                       sep = ""),
                         hjust = -.1, vjust = .1,size = 4, color = "red" ) +

                annotate("segment",
                         x = max.median.month.price,
                         xend = max.median.month.price,
                         y = 0,
                         yend = y.coordinate.max.median,
                         linetype = "dashed",
                         color = "red") +
                annotate("text",
                         x = max.median.month.price,
                         y = y.coordinate.max.median,
                         label = paste(" 月度价格指数中值的最大值出现在 ",
                                       max.median.month, " ， 为 ",
                                       format(max.median.month.price, digits = 4),
                                       sep = ""),
                         hjust = -.1, vjust = .1,size = 4, color = "red" ) +

                ggtitle("   ")  +
                annotate("text", x = mean(c(min.price, max.price)), y = Inf,
                         label = plot.title.text,
                         vjust = 1.5, size = 6, parse = TRUE) +

                xlab("注：2015年全年平均价格水平对应指数为100") +
                ylab("分布密度") +
                coord_cartesian(xlim = c(min.price, max.price), ylim = c(0, 0.13))
                
        return(p)
}



######Execution Part

setwd("d:/MyR/house")

##Specify the year and month range to draw plots

start.month <- as.Date("2016-8-1")
end.month <- as.Date("2018-4-1")
        
# specied.cities <- c("深圳", "广州", "北京", "上海", "厦门", "南京",  
#                     "西安")

month.interval <- 4  ## used to defind every month or every 2/3 months
ls.value <- InputHouseData(start.month, end.month, month.interval)

# plot.xinjian <- DrawSpecifiedCitiesPlot(ls.value, "xinjianshangpinfang", specied.cities)
# plot.ershou <- DrawSpecifiedCitiesPlot(ls.value, "ershouzhuzhai", specied.cities)

curve.xinjian <- DrawGlobalPriceCurve(ls.value, "xinjianshangpinfang")
curve.ershou <- DrawGlobalPriceCurve(ls.value, "ershouzhuzhai")

### The first plot

# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 1)))
# vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# 
# print(plot.xinjian, vp = vplayout(1, 1))
# print(plot.ershou, vp = vplayout(2, 1))

### The second plot
# 
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(curve.xinjian, vp = vplayout(1, 1))
print(curve.ershou, vp = vplayout(2, 1))

dev.copy(png, file = "produce_house_density_report.png", units= "px", width=1000, height=600)

dev.off()
