library(readxl)
Data_1Y <- read_excel("somefile.xlsx")
#View(Data_1Y)
Data_1Y$td=(Data_1Y$...3)/3600
Data_1Y$tdr=as.integer(Data_1Y$td)
#View(small_data)
M24=matrix(NA, nrow = 24, ncol = 148)
df24=as.data.frame(M24)
#View(df24)
for (ti in 1:24){
 
  sel<-as.data.frame(subset(Data_1Y,tdr==ti))
  sel<-sel[,1:(ncol(sel)-2)]
  sel<-sel[,4:ncol(sel)]
  if (dim(sel)[1]!=0){
    
    mean_vector=as.vector(colMeans(sel))
    df24[ti,]=mean_vector
    
  }
#range(Data_1Y$tdr) is screwed up      
   
}
