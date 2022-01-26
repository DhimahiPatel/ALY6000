#Printing my last name
print("Plotting Basics: Patel")

#Loading all the required packages
pacman:: p_load(FSA,FSAdata,magrittr,dplyr,plotrix,ggplot2,moments)

#Print the first and last 3 records from the BullTroutRMS2 dataset 
headtail(BullTroutRML2,3)

#Remove all records except those from Harrison
sample1=filter(BullTroutRML2,lake=="Harrison")
sample1

#first and last 5 records from the filtered sample1 dataset
headtail(sample1,5)

#the structure of the filtered sample1 dataset 
str(sample1)

#summary of the filtered sample1 dataset
summary(sample1)

#a scatterplot for "age" and "fl"
plot(age ~ fl, data = sample1, xlim=c(0,500), ylim=c(0,15), las=2,
     ylab = "Age (yrs)", xlab ="Fork Length (mm)", main="Plot 1: Harrison Lake Trout")

#an "Age" histogram 
hist(sample1$age, xlab = "Age (yrs)", main = "Plot 2: Harrison Fish Age Distribution", 
     xlim =c(0,15), ylim =c(0,15), las=2, col = "cadetblue")

#an overdense plot
plot(age ~ fl, data = sample1, xlim=c(0,500), ylim=c(0,15), las= 2, pch=16, col="green",
     ylab = "Age (yrs)", xlab ="Fork Length (mm)", main="Plot 3: Harrison Density Shaded by Era")

#an object called "tmp" that includes the first 3 and last 3 records of the sample1 dataset
tmp = headtail(BullTroutRML2,3) 
tmp

#display the "era" column (variable) in the new "tmp" object
select(tmp,'era')

#pchs vector with the argument values for + and x
pchs <- c(3, 4)

#cols vector with the two elements "red" and "gray60"
cols <- c("red", "gray60")

#Convert the tmp era values to numeric values
sapply(tmp, class)
te <- as.numeric(tmp$era)
typeof(te)

#Initialize the cols vector with the tmp era values
cols[sample1$era]

#a plot of "Age (yrs)" versus "Fork Length (mm)"
plot(age ~ fl, data = sample1, xlim=c(0,500), ylim=c(0,15), las= 2, pch=pchs[sample1$era], col=cols[sample1$era],
     ylab = "Age (yrs)", xlab ="Fork Length (mm)", main="Plot 4: Symbol & Color by Era")

#a regression line overlay
plot(age ~ fl, data = sample1, xlim=c(0,500), ylim=c(0,15), las= 2, pch=pchs[sample1$era], col=cols[sample1$era],
     ylab = "Age (yrs)", xlab ="Fork Length (mm)", main="Plot 5: Regression Overlay")
abline(lm(age ~ fl, data= sample1))

#Place a legend
plot(age ~ fl, data = sample1, xlim=c(0,500), ylim=c(0,15), las= 2, pch=pchs[sample1$era], col=cols[sample1$era],
     ylab = "Age (yrs)", xlab ="Fork Length (mm)", main="Plot 6: :Legend Overlay")
abline(lm(age ~ fl, data= sample1))
legend(1,14,legend = c("1977-80", "1997-01"), col=c("red", "gray60"), pch=c(3,4), bty="n")
