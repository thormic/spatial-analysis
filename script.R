library(ggplot2)
library(reshape2)
library(dplyr)
library("rgdal")
library("rgeos")
library(tmap)
library(leaflet)
library(RColorBrewer)
library("sp")
library(raster)
library(dismo)
library(adehabitatHR)
library(spdep)
library("spgwr")
library(grid)
library(gridExtra)
library(spatstat)
library(maptools)
library(gstat)
library(xts)
library(rgl)
library(automap)
library(corrplot)

# Load prepared dataset
Census.Data <- read.csv("census_data.csv")
houseData <- read.csv("house_data.csv")

# Create spatial DF for houses
House.Points <-SpatialPointsDataFrame(houseData[,6:7], houseData,
                                      proj4string = CRS("+init=EPSG:27700"))

hist_df <- gather(Census.Data[,-1], key = "name", value = "value")

# plot histograms of all variables
ggplot(hist_df) +
  geom_histogram(aes(value)) +
  facet_wrap(~name, ncol = 5) +
  ggtitle("Histograms of all variables")

# Boxplots for chosen variables
boxplot(Census.Data$christian, Census.Data$muslim, Census.Data$jewish,
        Census.Data$no_religion, ylim=c(0,100),
        col = "dodgerblue", rectCol="dodgerblue3", colMed="dodgerblue4",
        names=c("Christian", "Muslim", "Jewish", "No religion"),
        main = "Distribution of religions per OA")

boxplot(Census.Data$white, Census.Data$black_african, Census.Data$asian,
        Census.Data$other_arab, ylim=c(0,100),
        col = "dodgerblue", rectCol="dodgerblue3", colMed="dodgerblue4",
        names=c("White people", "Black/African", "Asian", "Other/Arabs"),
        main = "Distribution of religions per OA")

boxplot(Census.Data$employed, Census.Data$unemployed, Census.Data$kids_english, Census.Data$male, Census.Data$white,
        Census.Data$Age_30_44, Census.Data$married, Census.Data$owned,  ylim=c(0,100),
        col = "dodgerblue", rectCol="dodgerblue3", colMed="dodgerblue4",
        names=c("Employed", "Unemployed", "Kids speaking english", "Male", "White people", "Age 30-44", "Married", "Property owned"),
        main = "Distribution of certain variables per OA")




# Scatterplots with regression line

p <- ggplot(Census.Data, aes(unemployed, highest_quali))
p + geom_point(aes(colour = muslim, size = black_african)) 

p2 <- ggplot(Census.Data, aes(highest_quali, no_religion))
p2 + geom_point(aes(colour = white, size = other_eu))


# Correlation plot

M<-cor(Census.Data[,-1])
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


# Load spatial files for Kensington and Chelsea
Output.Areas <- readOGR("data/statistical-gis-boundaries-london/ESRI", "OA_2011_London_gen_MHW")
Output.Areas <- Output.Areas[Output.Areas$LAD11NM=="Kensington and Chelsea",]



# Combine variables with prices
OA.Census <- merge(Output.Areas, Census.Data, by.y ="OA", by.x="OA11CD")
proj4string(OA.Census) <- CRS("+init=EPSG:27700")

# Maps with filling 
tmap_mode("view")

tm_shape(OA.Census) + tm_fill("highest_quali", palette = "Purples", style = "quantile",
                              title = "% with a highest Qualifications") + tm_borders(alpha=.4)

tm_shape(OA.Census) + tm_fill("black_african", palette = "YlGn", style = "quantile",
                              title = "% of Black/African") + tm_borders(alpha=.4)

tm_shape(OA.Census) + tm_fill("white", palette = "GnBu", style = "quantile",
                              title = "% of Muslim") + tm_borders(alpha=.4)


# Plot maps with house prices and variables
tm_shape(OA.Census) + tm_fill("white", palette = "Greens",
                              style = "quantile", title = "% Whites") +
  tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_bubbles(col = "price_paid", size = 0.2,
                                      palette = "Blues", style = "quantile",
                                      legend.size.show = FALSE,
                                      title.col = "Price Paid (£)",
                                      border.col = "black", border.lwd = 0.1,
                                      border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)


tm_shape(OA.Census) + tm_fill("unemployed", palette = "Oranges",
                              style = "quantile", title = "% Unemployed") +
  tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_bubbles(col = "price_paid", size = 0.2,
                                      palette = "Blues", style = "quantile",
                                      legend.size.show = FALSE,
                                      title.col = "Price Paid (£)",
                                      border.col = "black", border.lwd = 0.1,
                                      border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)


# Creating kernels
kde.output <- kernelUD(House.Points, h="href", grid = 1000)
kde <- raster(kde.output)
projection(kde) <- CRS("+init=EPSG:27700")
tmap_mode("plot")
bounding_box <- bbox(Output.Areas)
masked_kde <- mask(kde, Output.Areas)

tm_shape(masked_kde, bbox = bounding_box) + tm_raster("ud", style = "quantile",
                                                      n = 100,
                                                      legend.show = FALSE,
                                                      palette = "YlGnBu") +
  tm_shape(Output.Areas) + tm_borders(alpha=.3, col = "white") +
  tm_layout(frame = FALSE) 

range75 <- getverticeshr(kde.output, percent = 75)
range50 <- getverticeshr(kde.output, percent = 50)
range25 <- getverticeshr(kde.output, percent = 25)

tm_shape(Output.Areas) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "white") +
  tm_shape(House.Points) + tm_dots(col = "blue") +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) +
tm_fill(alpha=.1, col = "#fb6a4a") +
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) +
  tm_fill(alpha=.1, col = "#de2d26") +
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) +
  tm_fill(alpha=.1, col = "#a50f15") +
  tm_layout(frame = FALSE)
