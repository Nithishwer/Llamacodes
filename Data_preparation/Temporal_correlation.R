library(readxl)
library(corrplot)
Data_1Y <- read_excel("somefile.xlsx")
#View(Data_1Y)
Data_1Y$td=(Data_1Y$...3)/3600
M24=matrix(NA, nrow = 160, ncol = 13)
df24=as.data.frame(M24)
#View(df7)
# for Seg-1
for (ti in 7:20){
  sel<-subset(Data_1Y,td<ti & td>=ti-1,"0.2")
  sel<-sel$"0.2"
  sel<-sel[1:160]
  print(length(sel))
  df24[ti-6]<-sel
}
m24=as.matrix(df24)
m24.cor = cor(m24)
corrplot(m24.cor)

#heatmap(m7, scale="column")
#heatmap(m7, Colv = NA, Rowv = NA, scale="column")
