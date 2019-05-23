library(readxl)
Data_1Y <- read_excel("/home/pennyworth/Documents/Bus/Data_One Year_200 m.xlsx")
View(Data_1Y)
Data_1Y$td=(Data_1Y$...3)/3600
#Data_1Y$tdr=as.integer(Data_1Y$td)
#View(small_data)
M24=matrix(NA, nrow = 24, ncol = 148)
df24=as.data.frame(M24)
#View(df24)
for (ti in 1:24){
  
  sel<-as.data.frame(subset(Data_1Y,td<ti & td>=ti-1))
  sel<-sel[,1:(ncol(sel)-1)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    df24[ti,]=mean_vector
    
  }

}
df24=df24[-c(2,3,24),]
m24=as.matrix(df24)
heatmap(m24, scale="column")
heatmap(m24, Colv = NA, Rowv = NA, scale="column")

# ##########################################################################
#  .5 Hr Heatmap
# ##############

M48=matrix(NA, nrow = 48, ncol = 148)
df48=as.data.frame(M48)
#View(df24)
for (ti in 1:48){
  
  sel<-as.data.frame(subset(Data_1Y,td<ti/2 & td>=(ti-1)/2))
  sel<-sel[,1:(ncol(sel)-1)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    df48[ti,]=mean_vector
    
  }
  
}
# To do manually
df48=df48[-c(2,3,,4,5,6,7,47,48),]
m48=as.matrix(df48)
heatmap(m48, scale="column")
heatmap(m48, Colv = NA, Rowv = NA, scale="column")

# ##########################################################################
#  .25 Hr Heatmap
# ###############

M96=matrix(NA, nrow = 96, ncol = 148)
df96=as.data.frame(M96)
#View(df24)
for (ti in 1:96){
  
  sel<-as.data.frame(subset(Data_1Y,td>=(ti-1)/4 & td<ti/4))
  sel<-sel[,1:(ncol(sel)-1)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    df96[ti,]=mean_vector
    
  }
  
}
# To do manually
df96=df96[-c(c(2:15),93:96,19),]
m96=as.matrix(df96)
heatmap(m96, scale="row")
heatmap(m96, Colv = NA, Rowv = NA, scale="column")


# ##########################################################################
#  12months Heatmap
# #################

Data_1Y <- read_excel("/home/pennyworth/Documents/Bus/Data_One Year_200 m.xlsx")
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
    df12[mi,]=mean_vector
    
  }
  
}
#df24=df24[-c(2,3,24),]
m12=as.matrix(df12)
heatmap(m12, scale="column")
heatmap(m12, Colv = NA, Rowv = NA, scale="column")

# ##########################################################################
#  Weekday Heatmap
# #################

Data_1Y <- read_excel("/home/pennyworth/Documents/Bus/Data_One Year_200 m.xlsx")
#View(Data_1Y)
Data_1Y$day<- as.POSIXlt(Data_1Y$...1)$wday
M7=matrix(NA, nrow = 7, ncol = 148)
df7=as.data.frame(M7)
View(df7)
for (di in 0:6){
  
  sel<-as.data.frame(subset(Data_1Y,day==di))
  sel<-sel[,1:(ncol(sel)-3)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    df7[di+1,]=mean_vector
    
  }
  
}
m7=as.matrix(df7)
heatmap(m7, scale="column")
heatmap(m7, Colv = NA, Rowv = NA, scale="column")
