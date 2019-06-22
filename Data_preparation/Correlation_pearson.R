library(readxl)
Data_1Y <- read_excel("/home/dinofelis/Downloads/Traffic/Data/Data_1Y_raw.xlsx")
Data_1Y$...1=NULL
Data_1Y$...2=NULL
Data_1Y$...3=NULL
Data_1Y$`29.6`=NULL
df<-t(Data_1Y)
rownames(df)<-c(1:147)
scaled.df<-scale(df)
dist.eucl <- dist(df.scaled, method = "euclidean")
library("factoextra")
dist.cor <- get_dist(df.scaled, method = "pearson")
# Display a subset
round(as.matrix(dist.cor)[1:3, 1:3], 1)
library(factoextra)
fviz_dist(dist.eucl)
