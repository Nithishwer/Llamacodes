library(readxl)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
Data_1Y <- read_excel("somefile.xlsx")
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

#Melting the df24 dataframe to help with the heatmap plotting and using the ggplot package
molten_df24=melt(df24, id = c("V148"))
View(molten_df24)

#Binning the data in order to plot ranges in HM
molten_df24$valuefactor<-cut(molten_df24$value,breaks=c(0,25,50,75,100,max(molten_df24$value)),labels = c("0-25","25-50","50-75","75-100",">100"))

#Plotting the heatmap with ggplot
df24_heatmap <- ggplot(data = molten_df24, mapping = aes(x = variable, y = V148,fill = valuefactor)) + geom_tile() + #geom_title specifies that it is a heatmap
                              xlab(label = "Road Segment") + ylab(label = "Time in Hours")+ggtitle(label = "Heatmap for Hourly bins") +
                              scale_x_discrete(expand = c(0,0), breaks=c("V1","V25","V50","V75","V100","V125"))+ # To fix the labels in the x axis
                              scale_fill_manual(values = rev(brewer.pal(n = 5,name = "RdYlGn")),name="Travel time (s)") # Scale_fill_manual is used for discrete filling
#                             scale_fill_distiller(name=expression(Time ~ (seconds^{-3})), palette = "RdYlBu") # Scale_fill_distiller is from Rbrewer package and for continuos fill
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
df48<-df48[-c(2,3,4,5,6,7,47,48),]
molten_df48=melt(df48, id = c("V148"))
View(molten_df48)
molten_df48$valuefactor<-cut(molten_df48$value,breaks=c(0,25,50,75,100,max(molten_df48$value)),labels = c("0-25","25-50","50-75","75-100",">100"))
df48_heatmap <- ggplot(data = molten_df48, mapping = aes(x = variable, y = V148,fill = valuefactor)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Time in 0.5 Hours")+ggtitle(label = "Heatmap for 30 Minute bins") +
  scale_x_discrete(expand = c(0,0), breaks=c("V1","V25","V50","V75","V100","V125"))+
  scale_fill_manual(values = rev(brewer.pal(n = 5,name = "RdYlGn")),name="Travel time (s)") # Scale_fill_distiller is from Rbrewer package and now uses the RdBu palette
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
molten_df96$valuefactor<-cut(molten_df96$value,breaks=c(0,25,50,75,100,max(molten_df96$value)),labels = c("0-25","25-50","50-75","75-100",">100"))
df96_heatmap <- ggplot(data = molten_df96, mapping = aes(x = variable, y = V148,fill = valuefactor)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Time in 0.25 Hours")+ggtitle(label = "Heatmap for 15 Minute bins") +
  scale_x_discrete(expand = c(0,0), breaks=c("V1","V25","V50","V75","V100","V125"))+
  scale_fill_manual(values = rev(brewer.pal(n = 5,name = "RdYlGn")),name="Travel time (s)") # Scale_fill_distiller is from Rbrewer package and now uses the RdBu palette

df96_heatmap


# ##########################################################################
#  12months Heatmap
# #################

Data_1Y <- read_excel("somefile.xlsx")
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

#Something to fix y axis labels
number_ticks<-function(n){function(limits) pretty(limits,n)}

#df24=df24[-c(2,3,24),]
molten_df12=melt(df12, id = c("V148"))
molten_df12$valuefactor<-cut(molten_df12$value,breaks=c(0,25,50,75,100,max(molten_df12$value)),labels = c("0-25","25-50","50-75","75-100",">100"))
df12_heatmap <- ggplot(data = molten_df12, mapping = aes(x = variable, y = V148,fill = valuefactor)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Month Number")+ggtitle(label = "Heatmap for Monthly bins") +
  scale_x_discrete(expand = c(0,0), breaks=c("V1","V25","V50","V75","V100","V125"))+
  scale_y_continuous(breaks=number_ticks(8))+
  scale_fill_manual(values = rev(brewer.pal(n = 5,name = "RdYlGn")),name="Travel time (s)") # Scale_fill_distiller is from Rbrewer package and now uses the RdBu palette
df12_heatmap

# ##########################################################################
#  Weekday Heatmap
# #################

Data_1Y <- read_excel("somefile.xlsx")
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
molten_df7$valuefactor<-cut(molten_df7$value,breaks=c(0,25,50,75,100,max(molten_df7$value)),labels = c("0-25","25-50","50-75","75-100",">100"))
df7_heatmap <- ggplot(data = molten_df7, mapping = aes(x = variable, y = V148,fill = valuefactor)) + geom_tile() + 
  xlab(label = "Road Segment") + ylab(label = "Day Number")+ggtitle(label = "Heatmap for Weekday bins") +
  scale_x_discrete(expand = c(0,0), breaks=c("V1","V25","V50","V75","V100","V125"))+
  scale_y_continuous(breaks=number_ticks(8))+
  scale_fill_manual(values = rev(brewer.pal(n = 5,name = "RdYlGn")),name="Travel time (s)") # Scale_fill_distiller is from Rbrewer package and now uses the RdBu palette
df7_heatmap
