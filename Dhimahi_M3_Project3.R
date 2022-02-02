#Printing my name
print("Plotting Basics: Dhimahi Patel")

#loading the libraries
pacman::p_load(FSA, FSAdata, magrittr, dplyr, tidyr, plyr, tidyverse)

#Importing the inchBio.csv dataset
bio <- read.csv(file = 'C://Users//DELL//Downloads//inchBio.csv', header=TRUE, sep=",")

#displaying the head,tail and structure of bio dataset
headtail(bio)
str(bio)

#Creating an object that counts and lists all the species records
counts <- count(bio, "species")
counts

#Displaing the 8 levels (names) of the species
unique(bio$species)

#Creating an object that displays the different species and the number of record of each species in the dataset 
tmp <- select(bio, species, netID)
c <- count(tmp$species)
c

#Creating a subset of just the species variable to display the first five records
tmp2 <- head(c,5)
tmp2

#Creating a table of the species variable and finding the class of that table
w<-table(tmp$species)
class(w)
w

#Converting a table to a data frame and displaying the results
t<-as.data.frame(w)
str(t)

#Extract and display the frequency values from the above data frame
names(t)[1]='species'
t

#Creating a table from the bio species attribute and displays the number of species in the data set
cSpec<- table(bio$species)
cSpec<-as.data.frame(cSpec)
str(cSpec)
names(cSpec)[1]='species'
cSpec

#Creating a table that displays the species and percentage of records for each species
cSpecPct <- table(bio$species, bio$netID)
cSpecPct <- rowSums(prop.table(cSpecPct))
cSpecPct
u <- data.frame(cSpecPct)
str(u)


#Converting the table to a data frame
u <- data.frame(cSpecPct)
str(u)


#Barplot of cSpec
barplot(cSpec$Freq, main= "Fish Count", ylab = "COUNTS", col="lightgreen", las=2, cex.names = 0.60, 
        names.arg = c("Black Crappie", "Bluegill", "Bluntnose Minnow", "Iowa Darter", " Largemouth Bass",
                      "Pumpkinseed", "Tadpole Madtom", "Yellow Perch"))

#Barplot of cSpecPct
barplot(u$cSpecPct, main="Fish Relative Frequency", ylim = c(0,0.4),col="lightblue", cex.names = 0.60, las= 2, 
        names.arg = c("Black Crappie", "Bluegill", "Bluntnose Minnow", "Iowa Darter", " Largemouth Bass",
                      "Pumpkinseed", "Tadpole Madtom", "Yellow Perch"))


# Rearranging the data frame in descending order of relative frequency
attach(d)
newdata <- u[order(cSpecPct), ]
newdata
detach(d)
str(d) 
d

#Add new table columns
counts
t$Freq
tdesc <- t[order(-t$Freq),]
tdesc$Freq
d <- d %>% mutate(cumfreq=cumsum(d$RelFreq), counts=tdesc$Freq, cumcounts=cumsum(tdesc$Freq))
d

#Create a parameter variable to store parameter variables
def_par<-par(no.readonly = TRUE)

#Plotting a Bar plot 
pc <- barplot(d$counts, width = 1, space = 0.15, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", 
              names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto", d$counts, na.rm=TRUE)

#Adding a cumulative counts line to the pc plot
pc <- barplot(d$counts, width = 1, space = 0.15, border = NA, axes = F, main = "Species Pareto",
              ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, 
              cex.names = 0.70)
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

#Placeing a grey box around the pareto plot
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")

#Adding a left side axis
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)

#Adding axis details on right side of box
axis(side = 4, at = c(0, d$cumcounts), labels = c(0, d$cumfreq),las = 1, 
     col.axis = "cyan3", col = "cyan4", cex.axis = 0.8)

#Displaying the finished Species Pareto Plot
pc <- barplot(d$counts, width = 1, space = 0.15,axes = F, main = "Species Pareto\n Dhimahi Patel",
              ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, 
              las=2, cex.names = 0.70)
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumcounts), labels = c(0, d$cumfreq),las = 1, 
     col.axis = "cyan3", col = "cyan4", cex.axis = 0.8)
 
