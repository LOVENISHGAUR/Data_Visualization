#Author : Lovenish

#Data Visualization

#*********************************************************************************
#Base Package
#GGPLOT2 Package
#Grid Graphics
#Lattice Graphics

#*********************************************************************************
#Types of Graphs
#Basic Visualization
#Scatter Plot
#Line Chart
#Bar Plot
#Pie Chart
#Histogram
#Density Plot
#Box Plot

#Advanced Visualization
#Mosiac Plot
#Heat Map
#3D Graph
#CorrelationPlot
#Word Cloud
#*********************************************************************************
#Installing the package and data set to be analysed
install.packages("MASS")
mtcars <- datasets::mtcars
#*********************************************************************************
#Scatter Plot :
#Type : type
#"p" for points,
#"l" for lines,
#"b" for both,
#"c" for the lines part alone of "b",
#"o" for both ‘over plotted’,
#"h" for ‘histogram’ like (or ‘high-density’) vertical lines,
#"s" for stair steps,
#"S" for other steps, see ‘Details’ below,
#"n" for no plotting.

#xlim : Limit of x axis
#ylim : Limit of y axis

#xlab : Label on x axis
#ylab : Label on y axis
#main : Label of complete graph

max_hp <- max(mtcars$hp) # Maximum of hp
max_mpg <- max(mtcars$mpg) #maximum of mpg
plot(mtcars$hp, mtcars$mpg, type = "p",
     xlim = c(0, max_hp),
     ylim = c(0, max_mpg),
     ylab = "Miles Per Galon", xlab = "Hourse Power",
     main = "Hourse Power Vs Mileage"
     )

plot_par <- par()
names(plot_par)
length(plot_par)

par(mfrow = c(1,1))

plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)


xmin <- min(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4)
xmax <- max(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4)
ymin <- min(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
ymax <- max(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)

plot(anscombe$x1, anscombe$y1,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x Value1",
     ylab = "y Value1",
     main = "First Data Set"
     )

plot(anscombe$x2, anscombe$y2,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x Value2",
     ylab = "y Value2",
     main = "Second Data Set"
)

plot(anscombe$x3, anscombe$y3,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x Value3",
     ylab = "y Value3",
     main = "Third Data Set"
)

plot(anscombe$x4, anscombe$y4,
     xlim = c(xmin, xmax),
     ylim = c(ymin, ymax),
     xlab = "x Value4",
     ylab = "y Value4",
     main = "Fourth Data Set"
)

#Adding details using par() function

plot(mtcars$hp, mtcars$mpg,
     xlim = c(0, max_hp),
     ylim = c(0, max_mpg),
     xlab = "HoursePower",
     ylab = "Miles Per Gallon",
     main = "Mileage",
     type = "n",
)

#Using pch : "Plotting character
points(mtcars$hp, mtcars$mpg, pch = 56)
points(mtcars$hp, mtcars$mpg, pch = mtcars$cyl)#will display the
points(mtcars$hp, mtcars$mpg, pch = as.character(mtcars$cyl))#will display values as it is in the graph

index6 <- which(mtcars$cyl == 6)

points(mtcars$hp[index6],
       mtcars$mpg[index6],
       pch = 19)


text(mtcars$hp[index6],
     mtcars$mpg[index6],
     adj = -0.9, cex = 1.3, font = 4)
#Blank Graph
plot(mtcars$hp, mtcars$mpg,
     xlim = c(0, max_hp),
     ylim = c(0, max_mpg),
     xlab = "HoursePower",
     ylab = "Miles Per Gallon",
     main = "Mileage",
     type = "n",
)


index4 <- which(mtcars$cyl == 4)
index6 <- which(mtcars$cyl == 6)
index8<- which(mtcars$cyl == 8)
points(mtcars$hp[index4],
       mtcars$mpg[index4],
       pch = 17)
points(mtcars$hp[index6],
       mtcars$mpg[index6],
       pch = 1)
points(mtcars$hp[index8],
       mtcars$mpg[index8],
       pch = 15)

legend("topright",pch = c(1,15,17),legend = c("4","6","8"))

#color

#*********************************************************************************

#line chart :Mostly preferred when we want to analyze data that changes over time
plot(AirPassengers, type = "l")

plot(mtcars$mpg, type = "o", col = "red")
lines(mtcars$cyl, type = "p", col = "blue")

#*********************************************************************************

#Bar Plots : This plot is used when the x axis is factor have few unique values.
#it is used for showing comparison between cumulative value fo server factor

counts <- table(mtcars$gear)
barplot(counts)
barplot(counts, main = "Distribution", 
        xlab = "Gear",ylab = "Count",
        legend = row.names(counts),
        col = c("red","green","blue"),
        horiz = TRUE)

#STACKBARPLOT

co <- table(mtcars$vs, mtcars$gear)
barplot(co, main = "Car distribution of vs VS gear",
        xlab = "Gear",ylab = "vs",
        legend = row.names(co),
        col = c("red", "skyblue"))

#Group Bar Plot : we only add parameter "besides = TRUE"
co <- table(mtcars$vs, mtcars$gear)
barplot(co, main = "Car distribution of vs VS gear",
        xlab = "Gear",ylab = "vs",
        legend = row.names(co),
        col = c("red", "skyblue"),
        beside = TRUE)
#*********************************************************************************

#PIE CHART

slices <- c(10,12,4,16,8,15)
lbls <- c("UK", "USA","GHANA", "CHINA","SPAIN","INDIA")
pie(slices, labels = lbls, main = "Pie chart of countries")

#3D pie chart
install.packages("plotrix")
library(plotrix)
slices <- c(10,12,4,16,8,15)
lbls <- c("UK", "USA","GHANA", "CHINA","SPAIN","INDIA")
pie3D(slices, labels = lbls,explode = .4, main = "Pie chart of countries")

#*********************************************************************************

#Histogram : when we can divide the data in bin

hist(mtcars$gear, breaks = 5, xlab = "No of gears", main = "DATA", col = rainbow(7))

hist(mtcars$gear, breaks = seq(1,10,1), xlab = "No of gears", main = "DATA", col = rainbow(7))


#*********************************************************************************

#Density Plot : Kerenal density plot are usually used for analyzing distribution of variables

density_data <- density(mtcars$mpg)
plot(density_data)

#TO FILL THE COLOR IN THE DENSITY PLOT WE NEED TO USE POLYGON FUNCTION

polygon(density_data, col = "skyblue", border = "black")
#*********************************************************************************

#BOX PLOT 
# Box plot shows statistically 5 significant numbers: minimum, 25th percentile,
#median, 75th percentile Maximum

#Used for analyzing the spread of data and also helps in identifying outliers

boxplot(mpg~gear, data = mtcars)
#*********************************************************************************

#Mosiac Plot : used for plotting categorical data

data("HairEyeColor")
mosaicplot(HairEyeColor)


#*********************************************************************************

#Heat map : 

heatmap(as.matrix(mtcars),Rowv = NA, Colv = NA, scale = "column", col = cm.colors(256))


#*********************************************************************************

#3D plot
install.packages("lattice")
library(lattice)

cloud(mtcars$hp~mtcars$mpg*mtcars$wt|am, data= mtcars, main = "3D Plot", pch = 17)


install.packages("plotly")
library(plotly)
plot_ly(mtcars, x = ~mtcars$hp, y= ~mtcars$mpg, z = ~mtcars$qsec )


#*********************************************************************************

#Correlation plot and word count

install.packages("corrplot")
library(corrplot)

#NOTE : CORRELATION PLOT ONLY WORKS ON NUMERICAL DATA 
str(mtcars)
corrmat <- cor(mtcars)
corrplot(corrmat, method = "ellipse")


#Word Cloud

install.packages("wordcloud")
install.packages("RColorBrewer")

library(wordcloud)
library(RColorBrewer)
row.names(mtcars)
model_table <- table(row.names(mtcars))
model_table
wordcloud(words = names(model_table),
        freq = as.numeric(model_table),scale = c(1,.45))
