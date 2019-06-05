library(readxl)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

#Reading data
Data_1Y <- read_excel("somefile.xlsx")
View(Data_1Y)

#Preparing data
Data_1Y$m<- as.numeric(substring(Data_1Y$...1,6,7))
df<-data.frame(m=Data_1Y$m,tt=Data_1Y$`29.6`)
attach(df) # adds the dataframe to path to enable easy access
names(df)

#Assumption 1: All samples are independent, and collected in >2 independent categorical groups
#Label groups and set as categorical factors

df$m <- as.factor(df$m)
df$m = factor(df$m,labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

class(df$m)

#Assumption 2: Dependent variable is continuous

#Assumption 3: Normal distributions of each group, no major outliers

Group1 <- subset(df, m == "Jan")
Group2 <- subset(df, m == "Jul")
Group3 <- subset(df, m == "Dec")

qqnorm(Group1$tt)
qqline(Group1$tt)

qqnorm(Group2$tt)
qqline(Group2$tt)

qqnorm(Group2$tt)
qqline(Group2$tt)

#Assumption 4: Homogeneneity of variances; need a p value that is higher than 0.05
bartlett.test(tt ~ m, data = df)



#########################################################################

#One Way ANOVA - Test if the means of the k populations are equal

model1 = lm(tt ~ m, data = df)
anova(model1)

#Post-hoc test TukeyHSD - Test which of the groups have different means ; p value smaller than 0.05 indicate that their travel times are
TukeyHSD(aov(model1))

#########################################################################


#Data visualisation

library("ggplot2")

ggplot(df, aes(x = m, y = tt)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Month") +
  ylab("travel time (s)")







