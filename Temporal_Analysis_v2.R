library(readxl)
Data_1Y <- read_excel("/home/pennyworth/Documents/Bus/Data_One Year_200 m.xlsx")
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
 

# 
# for (ti in 1:24){
#   ti_df = df24[FALSE,]
#   index=0
#   for (row in 1:148) 
#   {
#     t=Data_1Y[row,"td"]  # Works Fine 
#     t_24=as.integer(t)   # Works Fine
#     if (ti == t_24){     
#       index=index+1      
#       for (seg in 1:5)   # Works Fine
#       {
#         ti_df[index,seg]=Data_1Y[row,seg+3]
#       }
#     
#     }
#   }
#   for (col in 1:ncol(ti_df)){
#     mn=mean(ti_df[[col]])
#     df24[t_24+1,col]=mn
#   }
# }

# Works fine for individual ti values