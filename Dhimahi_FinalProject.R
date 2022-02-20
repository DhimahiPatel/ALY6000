#My name
print("Dhimahi Patel")

pacman:: p_load(FSA,FSAdata,magrittr,dplyr,plotrix,ggplot2,moments, tidyr, tidyverse, plyr)
install.packages("ggplot2")
library(ggplot2)


#Importing dataset flipkart.csv
library(readr)
df = read.csv(file= 'C://Users//DELL//Downloads//flipkart.csv', header=TRUE, sep=",")
view(df)

#Cleaning the data by removing the NA's
df <- data.frame(df)

df <- na.omit(df)


#first and last 3 data of dataset
headtail(df,3)

#Strucrure of the dataset df
str(df)

#The summary of the dataset
summary(df)

#First analysis
#Bar graph: took oppo as sample data to analyse rating 

sapply(df, class)
df$Brand <- as.numeric(as.factor(df$Brand))
class(df$Brand)

sample1=filter(df,Brand=="13")
sample1

w<-table(sample1$Brand, sample1$Rating)
w<-as.data.frame(w)
str(w)
names(w)[1]='Brand'
names(w)[2]='Rating'
w

barplot(w$Freq, main= "Samsung mobile rating summary", ylab = "Number of Devices", xlab="Rating", 
        col="lightgreen", las=1, cex.names = 0.80, ylim=c(0,120),
        names.arg = c("0","3", "3.1","3.7","3.8","4","4.1","4.2","4.3","4.4","4.5","4.6"))


#Second analysis
#Scatter plot : Overall selling price of different brands

plot(Selling.Price ~ Brand, data = df, las= 1, pch=16, col="green", cex.main= 1, xlim=c(1,15),ylim=c(0,160000),
     cex.axis=0.8, ylab = "Price", xlab ="Brand", main="Different Price range segments for mobiles in India")


#Third analysis
#Most commonly offered colors by all Brands

sapply(df, class)
df$Color <- as.numeric(as.factor(df$Color))
class(df$Color)

sample1=filter(df,Brand %in% c("13"))
sample1
headtail(sample1,3)

x<-table(sample1$Brand, sample1$Color)
x<-as.data.frame(x)
str(x)
names(x)[1]='Brand'
names(x)[2]='Color'
x

plot(x$Freq,type = "o", col = "red", xlab = "Color", ylab = "Types of models", las=1,
     main = "Most commonly offered color by samsung")


