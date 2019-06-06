#First load some data:
myData<-read.csv("F:/Statistics by Peter/Rstudio files/Friedman.csv", sep=";", na.strings=c("", "NA"))

#Data in different formats
#With an ID variable:
myData2<-myData 
myData2$ID<-seq.int(nrow(myData2))
#As a matrix:
myMatrix<-data.matrix(myData)
#In long format:
install.packages("reshape2")
library(reshape2)
myDataLong<-melt(myData2, id.vars=c("ID"))

#----FRIEDMAN TEST----

#No Friedman test in base, so need a package
#Using the stats package:
install.packages("stats")
library(stats)
friedman.test(myMatrix)
friedman.test(myDataLong$value,myDataLong$variable, myDataLong$ID)

#Using the agricolae package:
install.packages("agricolae")
library(agricolae)
friedman(myDataLong$ID,myDataLong$variable,myDataLong$value,console=TRUE)

#Using the coin package
install.packages("coin")
library(coin)
myDataLong[,'ID']<-factor(myDataLong[,'ID'])
friedman_test(value ~ variable | ID, myDataLong)

#Using the scamp package that requires a few others:
#Note this will give the Friedman test, uncorrected for ties
source("https://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("Rgraphviz")
install.packages("scmamp")
library(scmamp)
multipleComparisonTest(myMatrix, test="friedman")

