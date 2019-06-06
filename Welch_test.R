library(readxl)
library(stats)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)

#Reading data
Data_1Y <- read_excel("/home/pennyworth/Documents/Bus/Data/Data_1Y_raw.xlsx")
View(Data_1Y)

#Preparing data
Data_1Y$m<- as.numeric(substring(Data_1Y$...1,6,7))
df<-data.frame(m=Data_1Y$m,tt=Data_1Y$`29.6`)

#Creating matrix to enter values
Mat=matrix(NA, nrow = 12, ncol = 12)
Datf=as.data.frame(Mat)

#Loop through all 12C2 combinations
for (i in c(1:12)){
  for (j in c(1:12)){
    cat("T test between month",i," & ",j," :\n")
    x<-subset(df,m==i)$tt
    y<-subset(df,m==j)$tt
    Datf[i,j]=t.test(x,y,paired = FALSE)[3]
  }
}

Datf$V13=c(1:12)
molten_Datf=melt(Datf, id = c("V13"))
View(molten_Datf)

#Binning the data in order to plot ranges in HM
molten_Datf$valuefactor<-cut(molten_Datf$value,breaks=c(0,0.05,max(molten_Datf$value)),labels = c("Insignificant","Significant Difference"))

#Plotting the heatmap with ggplot
Datf_heatmap <- ggplot(data = molten_Datf, mapping = aes(x = variable, y = V13,fill = valuefactor)) + geom_tile() + #geom_title specifies that it is a heatmap
  xlab(label = "Months") + ylab(label = "Months")+ggtitle(label = "Heatmap for Significance among Months") +
#  scale_y_discrete(expand = c(0,0), breaks=c("1","2","3","4","5","6"))+ # To fix the labels in the x axis
  scale_fill_manual(values = rev(brewer.pal(n = 5,name = "RdYlBu")),name="Similarity") # Scale_fill_manual is used for discrete filling  
#                             scale_fill_distiller(name=expression(Time ~ (seconds^{-3})), palette = "RdYlBu") # Scale_fill_distiller is from Rbrewer package and for continuos fill
Datf_heatmap

