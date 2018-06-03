#### Author Comment Part
# modified on 2018-1-10

#### File Descriptiong Part
# 代码目的：用于比较指定楼盘月度成交价信息

#### Library Quoting Part
rm(list = ls())

suppressMessages(library(ggplot2))
suppressMessages(library(dplyr, warn.conflicts = FALSE))
suppressMessages(library(grid))

suppressMessages(library(RColorBrewer))
suppressMessages(library(reshape2))
suppressMessages(library(lubridate))


#### Function Definition Part
InputData <- function(arg.start.month, 
                      arg.end.month, 
                      arg.month.interval,
                      arg.specied.cities) {
        # 调用读取数据文件的子函数返回已经填好指定的城市楼盘价格的列表，筛取指定月份数据，
        # 存放到全局列表中
        
        # Args:
        #   arg.start.month: 绘画针对的起始月份，以单向量形式输入
        #   arg.end.month: 绘画针对的结束月份，以单向量形式输入
        #   arg.month.interval: 间隔月数
        #   arg.specied.cities:  指定的城市及楼盘
        # Returns:
        #   返回已经填好数据的列表
        
        ls.value.data <- list()

        ls.value.data <- InputSpecifiedTypeData(ls.value.data, 
                                                arg.specied.cities)

        if(arg.month.interval > 1){
                month.interval <- paste0(arg.month.interval," months")
        }else{
                month.interval <- "month"
        }
        month.list <- seq(arg.start.month, arg.end.month, month.interval)

        ls.value.data <- ls.value.data[as.Date(ls.value.data$date) %in% month.list,]
        #

        var.year <- year(as.Date(ls.value.data$date))
        var.month <- month(as.Date(ls.value.data$date))
        

        for(i in 1:length(var.month)){
                if(var.month[i] > 9) {
                        ls.value.data$date[i] <- paste(var.year[i],
                                                            "-",
                                                            var.month[i],
                                                            sep = "")
                }else{
                        ls.value.data$date[i] <- paste(var.year[i],
                                                            "-0",
                                                            var.month[i],
                                                            sep = "")
                }
                
        }
        
        return(ls.value.data)
}

InputSpecifiedTypeData <- function(arg.ls.value.data, 
                                   arg.specied.cities) {
        # 读取指定类型的数据文件，存放数据到全局列表中
        #
        # Args:
        #   arg.ls.value.data  全局data.frame的值引用
        #   arg.specied.cities: 指定的城市及楼盘
        #
        # Returns:
        #   返回已经填好指定的城市楼盘价格的列表
        
        ls.value.data <- arg.ls.value.data
        
        # browser()
        
        for(i in 1:length(arg.specied.cities)){

                csv.file.name <- paste(arg.specied.cities[i],".csv", sep = "")
                csv.file.content <- read.csv(csv.file.name, stringsAsFactors = FALSE)
                
                csv.file.content <- cbind(csv.file.content,
                                          rep(arg.specied.cities[i],nrow(csv.file.content)))
                names(csv.file.content) <- c("date", "price","name")
                
                        
                if (i == 1) {
                        ls.value.data <- csv.file.content   
                } else {
                        ls.value.data <-
                                rbind(ls.value.data, csv.file.content)
                }
        }
        

        return(ls.value.data)
}

DrawSpecifiedCitiesPlot <- function(arg.ls.value.data){
        # 为指定城市出框图
        #
        # Args:
        #   arg.ls.value.data  已经存储了二手房成交价的data.frame的值引用
        #
        # Returns:
        #   返回返回ggplot图层对象
        
        data.source <- arg.ls.value.data

        plot.title.text <- "链家网佛广深二手房成交价"

        # browser()
        
        price.show <- data.source$price
        
        price.show[c(5,8,11,17,23,33)] <- NA
        
        p <- ggplot(data.source, aes(x = name, y = price, color = date, fill = date)) +
                geom_bar(
                  # position = position_dodge(.9), 
                  position = "dodge",
                         width = 0.9,
                         stat = "identity", 
                         alpha = .5) +
                
                theme(axis.text.x=element_text(size=14,face = "bold")) +
                
                scale_x_discrete(labels = c("佛山中海千灯湖花园(2013年建成)" = "佛山中海千灯湖花园\n(2013年建成)",
                                            "佛山中海万锦豪园(2005年建成)" = "佛山中海万锦豪园\n(2005年建成)",
                                            "广州华景新城陶然庭园(2007年建成)" = "广州华景新城陶然庭园\n(2007年建成)",
                                            "广州珠岛花园二期(1995年建成)" = "广州珠岛花园二期\n(1995年建成)",
                                            "深圳和成世纪名园(2014年建成)" = "深圳和成世纪名园\n(2014年建成)",
                                            "深圳阳光棕榈园三期(2004年建成)" = "深圳阳光棕榈园三期\n(2004年建成)")) +
                
                geom_text(aes(label = price.show,
                              vjust = -0.9 ),
                          position = position_dodge(.9),size = 3,
                          colour = "black") +
                xlab("") +
                ylab("单价：元 / 平方米") +
                ggtitle("    ")  +
                annotate("text", x = 3.5, y = Inf,
                         label = plot.title.text,
                         vjust = 1.5, size = 8, parse = TRUE) +
                coord_cartesian(ylim = c(0, 75000))

        return(p)
        
}

######Execution Part

setwd("d:/MyR/house")

##Specify the year and month range to draw plots

start.month <- as.Date("2016-8-1")
end.month <- as.Date("2018-4-1")
        
specied.cities <- c("佛山中海千灯湖花园(2013年建成)", 
                    "佛山中海万锦豪园(2005年建成)", 
                    "广州华景新城陶然庭园(2007年建成)", 
                    "广州珠岛花园二期(1995年建成)", 
                    "深圳和成世纪名园(2014年建成)", 
                    "深圳阳光棕榈园三期(2004年建成)")

##read csv files to get data. The input months length can be larger than 3

month.interval <- 4  ## used to defind every month or every 2/3 months
ls.value <- InputData(start.month, end.month, month.interval, specied.cities)

p <- DrawSpecifiedCitiesPlot(ls.value)
print(p)
 

dev.copy(png, file = "produce_fgs_house_monthly_report.png", units= "px", width=1000, height=600)

dev.off()
