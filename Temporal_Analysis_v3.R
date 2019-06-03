library(readxl)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
Data_1Y <- read_excel("/home/dinofelis/Downloads/Traffic/Data/Data_1Y_raw.xlsx")
View(Data_1Y)

# Add a column corresponding to time in Hrs
Data_1Y$td=(Data_1Y$...3)/3600
#View(small_data)

# Pretty self explanatory
M24=matrix(NA, nrow = 24, ncol = 148)
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
    mean_vector=c(mean_vector,ti)
    df24[ti,]=mean_vector
    
  }

}

#View(df24)
df24=df24[-c(2,3,24),]

# Melting the df24 dataframe to help with the heatmap plotting and using the ggplot package
molten_df24=melt(df24, id = c("V148"))
View(molten_df24)

#Plotting the heatmap with ggplot
df24_heatmap <- ggplot(data = molten_df24, mapping = aes(x = variable, y = V148,fill = value)) + geom_tile() + #geom_title specifies that it is a heatmap
                              xlab(label = "Road Segment") + ylab(label = "Time in Hours")+ggtitle(label = "Heatmap for Hourly bins") +
                              scale_fill_distiller(name=" Time in seconds", palette = "RdYlBu") # Scale_fill_distiller is from Rbrewer package and now uses the RdBu palette
df24_heatmap

# ##########################################################################
#  .5 Hr Heatmap
# ##############

M48=matrix(NA, nrow = 48, ncol = 148)
df48=as.data.frame(M48)
#View(df24)
for (ti in 1:48){
  
  sel<-as.data.frame(subset(Data_1Y,td<ti/2 & td>=(ti-1)/2))
  sel<-sel[,1:(ncol(sel)-2)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    mean_vector=c(mean_vector,ti)
    df48[ti,]=mean_vector
    
  }
  
}
molten_df48=melt(df48, id = c("V148"))
View(molten_df48)
df48_heatmap <- ggplot(data = molten_df48, mapping = aes(x = variable, y = V148,fill = value)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Time in 0.5 Hours")+ggtitle(label = "Heatmap for 30 Minute bins") +
  scale_fill_distiller(name=" Time in seconds", palette = "RdYlBu") 
df48_heatmap

# ##########################################################################
#  .25 Hr Heatmap
# ###############

M96=matrix(NA, nrow = 96, ncol = 148)
df96=as.data.frame(M96)
#View(df24)
for (ti in 1:96){
  
  sel<-as.data.frame(subset(Data_1Y,td>=(ti-1)/4 & td<ti/4))
  sel<-sel[,1:(ncol(sel)-2)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    mean_vector=c(mean_vector,ti)
    df96[ti,]=mean_vector
    
  }
  
}

df96=df96[-c(c(2:15),93:96,19),]
molten_df96=melt(df96, id = c("V148"))
df96_heatmap <- ggplot(data = molten_df96, mapping = aes(x = variable, y = V148,fill = value)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Time in 0.25 Hours")+ggtitle(label = "Heatmap for 15 Minute bins") +
  scale_fill_distiller(name=" Time in seconds", palette = "RdYlBu") 
df96_heatmap


# ##########################################################################
#  12months Heatmap
# #################

Data_1Y <- read_excel("/home/dinofelis/Downloads/Traffic/Data/Data_1Y_raw.xlsx")
#View(Data_1Y)
Data_1Y$m<- as.numeric(substring(Data_1Y$...1,6,7))
#Data_1Y$tdr=as.integer(Data_1Y$td)
#View(small_data)
M12=matrix(NA, nrow = 12, ncol = 148)
df12=as.data.frame(M12)
#View(df12)
for (mi in 1:12){
  
  sel<-as.data.frame(subset(Data_1Y,m==mi))
  sel<-sel[,1:(ncol(sel)-2)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    mean_vector=c(mean_vector,mi)
    df12[mi,]=mean_vector
    
  }
  
}
#df24=df24[-c(2,3,24),]
molten_df12=melt(df12, id = c("V148"))
df12_heatmap <- ggplot(data = molten_df12, mapping = aes(x = variable, y = V148,fill = value)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Month Number")+ggtitle(label = "Heatmap for Monthly bins") +
  scale_fill_distiller(name=" Time in seconds", palette = "RdYlBu") 
df12_heatmap

# ##########################################################################
#  Weekday Heatmap
# #################

Data_1Y <- read_excel("/home/dinofelis/Downloads/Traffic/Data/Data_1Y_raw.xlsx")
#View(Data_1Y)
Data_1Y$day<- as.POSIXlt(Data_1Y$...1)$wday
M7=matrix(NA, nrow = 7, ncol = 148)
df7=as.data.frame(M7)
View(df7)
for (di in 0:6){
  
  sel<-as.data.frame(subset(Data_1Y,day==di))
  sel<-sel[,1:(ncol(sel)-2)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    mean_vector=c(mean_vector,di)
    df7[di+1,]=mean_vector
    
  }
  
}
m7=as.matrix(df7)
molten_df7=melt(df7, id = c("V148"))
df7_heatmap <- ggplot(data = molten_df7, mapping = aes(x = variable, y = V148,fill = value)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Day Number")+ggtitle(label = "Heatmap for Weekday bins") +
  scale_fill_distiller(name=" Time in seconds", palette = "RdYlBu") 
df7_heatmap
