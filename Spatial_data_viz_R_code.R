#setting our working directory
setwd("/cloud/project")

#loading and renaming our csv files to be used in the project based on our working directory path
  Ethnicity <- read.csv("data/Camden/tables/KS201EW_oa11.csv")
  Rooms <- read.csv("data/Camden/tables/KS403EW_oa11.csv")
  Qualifications <-read.csv("data/Camden/tables/KS501EW_oa11.csv")
  Employment <-read.csv("data/Camden/tables/KS601EW_oa11.csv")

#viewing column names of our dataframe
  names (Ethnicity)
  names (Employment)
  names (Rooms)
  names (Qualifications)

# selecting specific columns only
# note this action overwrites the labels you made for the original data, 
  Ethnicity <- Ethnicity[, c(1, 21)]
  Rooms <- Rooms[, c(1, 13)]
  Employment <- Employment[, c(1, 20)]
  Qualifications <- Qualifications[, c(1, 20)]

# Renaming our column headers
# to change both column names
  names(Ethnicity)<- c("OA", "White_British")
  names(Rooms)<- c("OA", "Low_Occupancy")
  names(Employment)<- c("OA", "Unemployed")
  names(Qualifications)<- c("OA", "Qualification")

#1 Merge Ethnicity and Rooms to create a new object called "merged_data_1"
  merged_data_1 <- merge(Ethnicity, Rooms, by="OA")

#2 Merge the "merged_data_1" object with Employment to create a new merged data object
  merged_data_2 <- merge(merged_data_1, Employment, by="OA")

#3 Merge the "merged_data_2" object with Qualifications to create a new data object
  Census.Data <- merge(merged_data_2, Qualifications, by="OA")

#4 Remove the "merged_data" objects as we won't need them anymore
  rm(merged_data_1, merged_data_2)

# Writes the data to a csv named "practical_data" in your file directory
  write.csv(Census.Data, "project_data.csv", row.names=F)
  
# prints the selected data within the console (first 10 rows and first 5 columns which is the whole column)
  print(Census.Data[1:20,1:5])
  
# Calculating mean, median, 25th and 75th quartiles, min, max for our descriptive statistics
  summary(Census.Data)
  
# Creates a histogram
  hist(Census.Data$Unemployed)

# Creates a histogram, enters more commands about the visualisation
  hist(Census.Data$Unemployed, breaks=20, col= "blue", main="% in full-time employment", xlab="Percentage")
  
# box and whisker plots
  boxplot(Census.Data[,2:5]) 
  
# Installing a violin plot for combination of both histogram and box plot
# NB: When you hit enter R will ask you to select a mirror to download the package contents from. It doesn't really matter which one you choose, I tend to pick the UK based ones.
  install.packages("vioplot")  
  
# loads the violin plot package
  library(vioplot)
  
# creates a violin plot for 4 variables, uses 3 shades of blue and add names to the plot
  vioplot(Census.Data$Unemployed, Census.Data$Qualification, Census.Data$White_British, Census.Data$Low_Occupancy, ylim=c(0,100), col = "dodgerblue", rectCol="dodgerblue3", colMed="dodgerblue4", names=c("Unemployed", "Qualifications", "White British", "Occupancy"))

# plotting a simple scatter plot
#left of the comma is the x-axis, right is the y-axis. We are using the $ command to select the columns of the data frame we want and the xlab and ylab to name the axis.
  plot(Census.Data$Unemployed,Census.Data$Qualification, xlab="% unemployed", ylab="% with qualification")
  
# Create a proportional symbols plot or a bubble plot with a dotted regression line
  symbols(Census.Data$Unemployed, Census.Data$Qualification,  circles = Census.Data$White_British, fg="white", bg ="purple", inches = 0.2,  xlab="% unemployed", ylab="% With a Qualification") +
# adds a regression line, sets the colour to red, line width set to 2, and line type set to 2
  abline(lm(Census.Data$Qualification~ Census.Data$Unemployed), col="red", lwd=2, lty=2)
  
# using ggplot2
  # Loads an installed package
  library("ggplot2")

# plotting a simple scatter plot and regression line with ggplot2
  p <- ggplot(Census.Data, aes(Unemployed,Qualification))
  p + geom_point()+
  geom_smooth(method = lm, se = FALSE)
  
# plotting a ggplot2 with 4 variables, including size and color, with a regression line
  ggplot(Census.Data, aes(Unemployed,Qualification)) + 
  geom_point(aes(colour = White_British, size = Low_Occupancy)) + 
  geom_smooth(method='lm')
  
# Runs a Pearson's correlation
  cor(Census.Data$Unemployed, Census.Data$Qualification)

# Runs a Pearson's correlation with other relevant statistics
  cor.test(Census.Data$Unemployed, Census.Data$Qualification)
  
# Runs a Spearman's correlation
  cor.test(Census.Data$Unemployed, Census.Data$Qualification, method="spearman")
 
# We want to create a correlation matrix, which would require us to remove the 1st column (ID) since it won't allow us to do this if using the whole Census.Data
# function below creates a cormatrix_data object which does not include the 1st column from the original data 
  cormatrix_data <- Census.Data[,2:5]  
  
# creates correlation matrix
  cor(cormatrix_data)
  
# creates a rounded correlation matrix and name it corr
  corr <- round(cor(cormatrix_data),2)
  
# We would love to create heatmap for our correlation matrix (corr)
  
  #loads a new library reshape2 for our heatmap
    library(reshape2)
  # creates a qplot from our corr matrix
    qplot(x=Var1, y=Var2, data=melt(corr), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
    
# installs and loads the library corrplot to make another type of correlation plot
    install.packages("corrplot")
    library(corrplot)
    
# creates a lower triangular corrplot from our corr matrix
    corrplot(corr, type="lower", tl.col="black", tl.srt=45, method = c("pie"))
    
# runs a regressions model (y ~ x, data frame)
# The summary() function helps us to simply see the basic results from running the model.
    model_1 <- lm(Qualification~ Unemployed, Census.Data)
    summary(model_1)

# based on our model result, we can predict y, when value of x = 'n'. 10 is used here as an example    
    predict(model_1,data.frame(Unemployed= 10), interval = "confidence")
    
# 95% confidence interval of our model1
    confint(model_1, level= 0.95)
    
# a multiple regression model
    model_2 <- lm(Qualification ~ Unemployed + White_British, Census.Data)
    summary(model_2)
  
# install and load our packages for spatial analysis
    install.packages("rgdal")
    install.packages("rgeos")
    library("rgdal")
    library("rgeos")
    
# Load the output area shapefiles
    Output.Areas<- readOGR("data/Camden/shapefiles", "Camden_oa11")
    
# plots the shapefile
    plot(Output.Areas)
    
# joins data to the shapefile
    OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")
    
# sets the coordinate system to the British National Grid
    proj4string(OA.Census) <- CRS("+init=EPSG:27700")

# install and load our packages for the mapping of our data
    install.packages("tmap")
    install.packages("leaflet")
    library(tmap)
    library(leaflet)
    
# this will produce a quick map of our qualification variable
    qtm(OA.Census, fill = "Qualification")