#### Author Comment Part
# modified on 2018-1-10

#### File Descriptiong Part
# ����Ŀ�ģ����ڱȽ�ָ��¥���¶ȳɽ�����Ϣ

#### Library Quoting Part
library(dplyr, warn.conflicts = FALSE)
library(grid)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(lubridate)

#### Function Definition Part
InputData <- function(arg.start.month, 
                      arg.end.month, 
                      arg.month.interval,
                      arg.specied.cities) {
        # ���ö�ȡ�����ļ����Ӻ��������Ѿ����ָ���ĳ���¥�̼۸���б���ɸȡָ���·����ݣ�
        # ��ŵ�ȫ���б���
        
        # Args:
        #   arg.start.month: �滭��Ե���ʼ�·ݣ��Ե�������ʽ����
        #   arg.end.month: �滭��ԵĽ����·ݣ��Ե�������ʽ����
        #   arg.month.interval: �������
        #   arg.specied.cities:  ָ���ĳ��м�¥��
        # Returns:
        #   �����Ѿ�������ݵ��б�
        
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
        # ��ȡָ�����͵������ļ���������ݵ�ȫ���б���
        #
        # Args:
        #   arg.ls.value.data  ȫ��data.frame��ֵ����
        #   arg.specied.cities: ָ���ĳ��м�¥��
        #
        # Returns:
        #   �����Ѿ����ָ���ĳ���¥�̼۸���б�
        
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
        # Ϊָ�����г���ͼ
        #
        # Args:
        #   arg.ls.value.data  �Ѿ��洢�˶��ַ��ɽ��۵�data.frame��ֵ����
        #
        # Returns:
        #   ���ط���ggplotͼ�����
        
        data.source <- arg.ls.value.data

        plot.title.text <- "�������������ַ��ɽ���"

        # browser()
        p <- ggplot(data.source, aes(x = name, y = price, color = date, fill = date)) +
                geom_bar(position = "dodge", stat = "identity", alpha = .5) +

                geom_text(aes(label = format(price, nsmall = 1),
                              vjust = -0.9 ),
                          position = position_dodge(.9),size = 3,
                          colour = "black") +
                xlab("") +
                ylab("���ۣ�Ԫ / ƽ����") +
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
end.month <- as.Date("2017-11-1")
        
specied.cities <- c("��ɽ�к�ǧ�ƺ���԰(2013�꽨��)", 
                    "��ɽ�к������԰(2005�꽨��)", 
                    "���ݻ����³���Ȼͥ԰(2007�꽨��)", 
                    "�����鵺��԰����(1995�꽨��)", 
                    "���ںͳ�������԰(2014�꽨��)", 
                    "�����������԰����(2004�꽨��)")

##read csv files to get data. The input months length can be larger than 3

month.interval <- 3  ## used to defind every month or every 2/3 months
ls.value <- InputData(start.month, end.month, month.interval, specied.cities)

DrawSpecifiedCitiesPlot(ls.value)
# 
# 