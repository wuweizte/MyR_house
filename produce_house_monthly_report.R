#### Author Comment Part
# modified on 2016-7-8

#### File Descriptiong Part
# 代码目的：用于比较月度房价信息

#### Library Quoting Part
library(dplyr, warn.conflicts = FALSE)
library(grid)
library(ggplot2)
library(RColorBrewer)
library(reshape2)


#### Function Definition Part
InputData <- function(arg.year = 2016, arg.month = 2) {
        # 调用读取数据文件的子函数读取记录存放到全局列表中
        #
        # Args:
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回已经填好数据的列表
        
        ls.value.data <- list()

        ls.value.data <- InputSpecifiedTypeData(ls.value.data, 
                                                "xinjianshangpinfang", 
                                                arg.year,
                                                arg.month)

        ls.value.data <- InputSpecifiedTypeData(ls.value.data, 
                                                "ershouzhuzhai", 
                                                arg.year,
                                                arg.month)
        
        return(ls.value.data)
}

InputSpecifiedTypeData <- function(arg.ls.value.data, arg.type, arg.year = 2016, 
                                   arg.month = 2) {
        # 读取指定类型的数据文件，存放数据到全局列表中
        #
        # Args:
        #   arg.ls.value.data  全局列表的值引用
        #   arg.type: 读取的文件类型
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回已经填好数据的列表
        
        ls.value.data <- arg.ls.value.data
        
        for(i in arg.month){
                csv.file.name <- paste(arg.type, arg.year, "-", i,  
                                       ".csv", sep = "")
                csv.file.content <- read.csv(csv.file.name, stringsAsFactors = FALSE)
                
                if (i == 1) {
                        ls.value.data[[arg.type]] <- csv.file.content        
                } else {
                        ls.value.data[[arg.type]] <- 
                                merge(ls.value.data[[arg.type]], 
                                      csv.file.content, by = "城市")
                }
                names(ls.value.data[[arg.type]])[length(ls.value.data[[arg.type]])] <- 
                        paste(arg.year, "-", i,sep = "")
        }
        
        
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
        data.source <- melt(data.source, id = names(data.source)[1],
                            measure.vars = names(data.source)[-1])
        
        names(data.source) <- c("city", "time", "price")
        data.source <- data.source[order(data.source[1],data.source[2]),]
        data.source[[1]] <- gsub("　　", "",  data.source[[1]]) 
        #browser()
         
        data.source <- data.source[(data.source[[1]] %in% arg.cities),]
        #browser() 
        if (arg.type == "xinjianshangpinfang") {
                plot.title.text <- "'90' * m ^ 2 * '以下新建商品房价格指数'"
        } else {
                plot.title.text <- "'90' * m ^ 2 * '以下二手住宅价格指数'"
        }
        
        p <- ggplot(data.source, aes(x = city, y = price, color = time, fill = time)) +
                geom_bar(position = "dodge", stat = "identity", alpha = .5) +

                geom_text(aes(label = format(price, nsmall = 1),
                              vjust = -0.9 ),
                          position = position_dodge(.9),size = 3,
                          colour = "black") +
                xlab("注：2015年全年平均价格水平对应指数为100") +
                ylab("价格指数") +
                ggtitle("   ")  +
                #theme(plot.title = element_text(size = 20))  +

                annotate("text", x = mean(1:length(arg.cities)), y = Inf, 
                         label = plot.title.text, 
                         vjust = 1.5, size = 7, parse = TRUE) +
                coord_cartesian(ylim = c(75, 160))

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
        
        min.median.month <- names(which.min(apply(data.source[-1], 2, median)))
        min.median.month.price <- data.source[min.median.month][[1]]
        
        max.median.month <- names(which.max(apply(data.source[-1], 2, median)))
        max.median.month.price <- data.source[max.median.month][[1]]
        
        #browser()
        data.source <- melt(data.source, id = names(data.source)[1],
                            measure.vars = names(data.source)[-1])
        
        names(data.source) <- c("city", "time", "price")
        data.source <- data.source[order(data.source[1],data.source[2]),]
        
        data.source[[2]] <- factor(data.source[[2]])
        min.price <- floor(min(data.source[[3]]))
        max.price <- ceiling(max(data.source[[3]]))
        
        if (arg.type == "xinjianshangpinfang") {
                plot.title.text <- "'90' * m ^ 2 * '以下新建商品房价格指数分布密度曲线'"
        } else {
                plot.title.text <- "'90' * m ^ 2 * '以下二手住宅价格指数分布密度曲线'"
        }

        
                
        #browser()
        p <- ggplot(data.source[2:3], aes(x = price, colour = time, fill = time)) +
                geom_density(alpha = .5) +
                scale_colour_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2") +
                annotate("segment", 
                         x = median(min.median.month.price), 
                         xend = median(min.median.month.price),
                         y = 0, 
                         yend = FindDensityCurveMedianYCoordinate(min.median.month.price), 
                         linetype = "dashed",
                         color = "red") +
                annotate("text", 
                         x = median(min.median.month.price),
                         y = FindDensityCurveMedianYCoordinate(min.median.month.price),
                         label = paste(min.median.month,
                                 " 当月价格指数中值：",
                                       format(median(min.median.month.price), digits = 4), 
                                       sep = ""),
                         hjust = -.1, vjust = .1,size = 3.3, color = "red" ) +

                annotate("segment", 
                         x = median(max.median.month.price), 
                         xend = median(max.median.month.price),
                         y = 0, 
                         yend = FindDensityCurveMedianYCoordinate(max.median.month.price), 
                         linetype = "dashed",
                         color = "red") +
                annotate("text", 
                         x = median(max.median.month.price),
                         y = FindDensityCurveMedianYCoordinate(max.median.month.price),
                         label = paste(max.median.month,
                                       " 当月价格指数中值：",
                                       format(median(max.median.month.price), digits = 4), 
                                       sep = ""),
                         hjust = -.1, vjust = .1,size = 3.3, color = "red" ) +
                
                ggtitle("   ")  +
                #theme(plot.title = element_text(size = 20))  +
                
                annotate("text", x = mean(c(min.price, max.price)), y = Inf, 
                         label = plot.title.text, 
                         vjust = 1.5, size = 7, parse = TRUE) +
                
                xlab("注：2015年全年平均价格水平对应指数为100") +
                ylab("分布密度") +
                coord_cartesian(xlim = c(min.price, max.price), ylim = c(0, 0.35))
                
                
        return(p)
}



######Execution Part

setwd("d:/MyR/house")

##Specify the year and month range to draw plots
##Usually only numeric_Specied_Month need to be changed

specied.year <- 2016
specied.month <- 1:5  ## change here every time!
specied.cities <- c("深圳", "广州", "北京", "上海", "成都", "西安",  
                    "武汉", "杭州")

##read csv files to get data. The input months length can be larger than 3
ls.value <- InputData(specied.year, specied.month)
plot.xinjian <- DrawSpecifiedCitiesPlot(ls.value, "xinjianshangpinfang", specied.cities)
plot.ershou <- DrawSpecifiedCitiesPlot(ls.value, "ershouzhuzhai", specied.cities)

curve.xinjian <- DrawGlobalPriceCurve(ls.value, "xinjianshangpinfang")
curve.ershou <- DrawGlobalPriceCurve(ls.value, "ershouzhuzhai")

### The first plot

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(plot.xinjian, vp = vplayout(1, 1))
print(plot.ershou, vp = vplayout(2, 1))

### The second plot

# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 1)))
# vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(curve.xinjian, vp = vplayout(1, 1))
# print(curve.ershou, vp = vplayout(2, 1))
