rm(list = ls())

layout(matrix(c(1)))

ProduceMianJi <- function(filename){
        
        
        mianji.data <- read.csv(file = filename, header = TRUE)
        # mianji.data <- tail(mianji.data, arg.month.number)
        
        # row.number <- nrow(B)
        
        ###ȡ������������ݣ���� monthnumber ��

        # browser()

        xiaoshou.mianji <- mianji.data[,2]

        daishou.mianji <- mianji.data[,3]
        
        
        ylim.min <- min(xiaoshou.mianji,daishou.mianji,na.rm = TRUE) - 1
        ylim.max <- max(xiaoshou.mianji,daishou.mianji,na.rm = TRUE) + 1
        
        plot(xiaoshou.mianji,
             col="blue",
             type = "l",
             ylim = c(ylim.min,ylim.max),
             axes = FALSE,
             xlab = "��˵��������ͳ�ƾ��ṩ��������2012����ǰ��������������ݴ���ȱʧ��������Ժ�����2012����ǰ�ж�����",
             ylab = "",
             main = "��Ʒ������/����������ۼ�ͬ��(%)")
        
        # text(1:arg.month.number,
        #      ch.mianji + 0.4,
        #      round(ch.mianji,digits = 2),
        #      col = "blue")
        
        lines(daishou.mianji,col="red",type = "l")
        # text(1:arg.month.number,
        #      us.mianji + 0.4,
        #      us.mianji,
        #      col = "red")
        
        legend("topright",
               legend = c("�������","�������"),
               col = c("blue",'red'),
               lty = 1,
               # ncol = 3,
               # bty = "n"
               # lwd = c(1,2,3),
               # inset = .02,
               text.width = strwidth("100000000")#,
               # adj = c(1,0.5)
               )

        
        x.point <- seq(1,nrow(mianji.data),11)
 
        abline(v = x.point , lty = 2,col = "grey")       
        axis(1, at = x.point, labels = substr(mianji.data[x.point,1],1,4))
        
        y.point <- seq(-15,45,10)
        abline(h = y.point , lty = 2,col = "grey")   
        axis(2,at = y.point, labels = y.point, las = 1)
        box()
}


ProduceMianJi("D:\\MyR\\house\\xiaoshoumianji.csv")