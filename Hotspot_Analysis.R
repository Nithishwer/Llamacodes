library(readxl)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
Data_1Y <- read.csv("/home/pennyworth/Documents/Bus/Data/Data_1Y_processed.csv",header = FALSE)
Data_1Y$V149=NULL
Data_1Y$V150=NULL
Data_1Y$V151=NULL
Data_1Y$V1=NULL
View(Data_1Y)
Data_1Y$ID=c(1:nrow(Data_1Y))
molten_Data=melt(Data_1Y, id = c("ID"))
View(molten_Data)

#Cutting the data into bins for plotting
molten_Data$valuefactor<-cut(molten_Data$value,breaks=c(0,25,50,75,100,max(molten_Data$value)),labels = c("0-25","25-50","50-75","75-100",">100"))

#Plotting the heatmap with ggplot
Data_heatmap <- ggplot(data = molten_Data, mapping = aes(x = variable, y = ID,fill = valuefactor)) + geom_tile() + #geom_title specifies that it is a heatmap
  xlab(label = "Road Segment") + ylab(label = "Observation Number")+ggtitle(label = "Heatmap for Entire Data") +
  scale_x_discrete(expand = c(0,0), breaks=c("V1","V25","V50","V75","V100","V125"))+
  scale_fill_manual(values = rev(brewer.pal(n = 5,name = "RdYlGn")),name="Travel time (s)") # Scale_fill_distiller is from Rbrewer package and now uses the RdBu palette
  #  scale_fill_m(name=expression(Time ~ (seconds^{-3})), palette = "RdYlBu") # Scale_fill_distiller is from Rbrewer package and now uses the RdBu palette
Data_heatmap

#Calculating % Variation
Data_1Y$ID=NULL
Mean_col=colMeans(Data_1Y)
M_seg=matrix(NA, nrow = length(Mean_col), ncol = 2)
df_seg=as.data.frame(M_seg)
df_seg$V1=c(1:length(Mean_col))
df_seg$V2=Mean_col

#Identifying segments with max mean
top_times=subset(df_seg,V2>2.5*mean(Mean_col) & V1!=1)$V2

# Hourly Data Analysis
Data_1Y<-read_excel("/home/pennyworth/Documents/Bus/Data/Data_1Y_raw.xlsx")

# Add a column corresponding to time in Hrs
Data_1Y$td=(Data_1Y$...3)/3600
#View(small_data)

# Pretty self explanatory
M24=matrix(NA, nrow = 24, ncol = 147)
df24=as.data.frame(M24)
#View(df24)

# Entering values into the dataframe
for (ti in 1:24){
  
# Selecting data from Data_1Y    
sel<-as.data.frame(subset(Data_1Y,td<ti & td>=ti-1))
sel<-sel[,1:(ncol(sel)-2)]
sel<-sel[,4:ncol(sel)]
  
#Processing and adding the data to df_24
if (dim(sel)[1]!=0){
    
  mean_vector=as.vector(colMeans(sel))
  df24[ti,]=mean_vector
    
}
  
}

#View(df24)
df24=df24[-c(2,3,24),]
rmean_24=rowMeans(df24)
M_seg24=matrix(NA, nrow = length(cmean_24), ncol = 2)
df_seg24=as.data.frame(M_seg24)
df_seg24$V1=c(1:length(cmean_24))
df_seg24$V2=cmean_24
ttimes_24=subset(df_seg24,V2>2.5*mean(cmean_24) & V1!=1)$V2

#####################Month

Data_1Y<-read_excel("/home/pennyworth/Documents/Bus/Data/Data_1Y_raw.xlsx")
#View(Data_1Y)
Data_1Y$m<- as.numeric(substring(Data_1Y$...1,6,7))
#Data_1Y$tdr=as.integer(Data_1Y$td)
#View(small_data)
M12=matrix(NA, nrow = 12, ncol = 147)
df12=as.data.frame(M12)
#View(df12)
for (mi in 1:12){
  
  sel<-as.data.frame(subset(Data_1Y,m==mi))
  sel<-sel[,1:(ncol(sel)-2)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    df12[mi,]=mean_vector
    
  }
  
}
  
yrange<-range(df12)
xrange<- range(c(1:147))
plot(xrange,yrange,type="n",xlab = "Segments",ylab="Travel time",main = "Travel time across Months")
colors<-rainbow(147)
linetype<-c(1:147)
plotchar=seq(18,18+147,1)
colours=brewer.pal(8,'Set1')

for (i in 1:147){
  lines(c(1:147),df12[i,],type="l",lwd=1.5,lty=linetype[i],col=colours[i])
}

rmm<-rowMeans(df12)


# ##########################################################################
#  Weekday Heatmap
# #################

Data_1Y<-read_excel("/home/pennyworth/Documents/Bus/Data/Data_1Y_raw.xlsx")
#View(Data_1Y)
Data_1Y$day<- as.POSIXlt(Data_1Y$...1)$wday
M7=matrix(NA, nrow = 7, ncol = 147)
df7=as.data.frame(M7)
View(df7)
for (di in 0:6){
  
  sel<-as.data.frame(subset(Data_1Y,day==di))
  sel<-sel[,1:(ncol(sel)-2)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    df7[di+1,]=mean_vector
    
  }
  
}

# Plotting Change in Weekdays
yrange<-range(df7)
xrange<- range(c(1:147))
plot(xrange,yrange,type="n",xlab = "Segments",ylab="Travel time",main = "Travel time across Months")
for (i in 1:7){
  lines(c(1:147),df7[i,],type="l",lwd=1.5,lty=linetype[i],col=colours[i])
}

#Change in Weekdays %
rmeans=rowMeans(df7)

#References

#https://ggplot2.tidyverse.org/reference/scale_brewer.html#palettes
#http://www.roymfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/
#http://www.roymfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r/
