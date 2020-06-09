# Load prepared dataset
censusData <- read.csv("census_data.csv")
houseData <- read.csv("house_data.csv")

# Create spatial DF for houses
housePoints <-SpatialPointsDataFrame(houseData[,6:7], houseData,
                                      proj4string = CRS("+init=EPSG:27700"))


# Plotting variables histograms
hist(censusData$Unemployed, breaks=20, col= "blue",
     main="% in full-time employment", xlab="Percentage")

p <- ggplot(censusData, aes(unemployed, highest_quali))
p + geom_point(aes(colour = white, size = black_african))


# Variable correlation
cor.test(censusData$Age_0_17, censusData$other_arab)


# Load spatial files for Kensington and Chelsea
outputAreas <- readOGR("data/statistical-gis-boundaries-london/ESRI", "OA_2011_London_gen_MHW")
outputAreas <- outputAreas[outputAreas$LAD11NM=="Kensington and Chelsea",]
plot(outputAreas)

# Combine variables with prices
OACensus <- merge(outputAreas, censusData, by.y ="OA", by.x="OA11CD")
proj4string(OACensus) <- CRS("+init=EPSG:27700")


qtm(OACensus, fill = "three_and_more_cars")

tm_shape(OACensus) + tm_fill("asian")

tm_shape(OACensus) + tm_fill("asian", style = "quantile", n = 7,
                              palette = "Reds")

tm_shape(OACensus) + tm_fill("Qualification", palette = "Reds") +
  tm_borders(alpha=.4) +
  tm_compass()


# Plot maps with house prices and variables
tm_shape(OACensus) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "price_paid", style = "quantile")

tm_shape(OACensus) + tm_fill("Qualification", palette = "Reds",
                              style = "quantile", title = "% Qualification") +
  tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_bubbles(size = "price_paid", col = "price_paid",
                                      palette = "Blues", style = "quantile",
                                      legend.size.show = FALSE,
                                      title.col = "Price Paid (£)",
                                      border.col = "black", border.lwd = 0.1,
                                      border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)




# Creating kernels
kde.output <- kernelUD(House.Points, h="href", grid = 1000)
plot(kde.output)

kde <- raster(kde.output)
# sets projection to British National Grid
projection(kde) <- CRS("+init=EPSG:27700")

# maps the raster in tmap, "ud" is the density variable
tm_shape(kde) + tm_raster("ud")

bounding_box <- bbox(outputAreas)
# maps the raster within the bounding box
tm_shape(kde, bbox = bounding_box) + tm_raster("ud")


# mask the raster by the output area polygon
masked_kde <- mask(kde, outputAreas)
# maps the masked raster, also maps white output area boundaries
tm_shape(masked_kde, bbox = bounding_box) + tm_raster("ud", style = "quantile",
                                                      n = 100,
                                                      legend.show = FALSE,
                                                      palette = "YlGnBu") +
  tm_shape(outputAreas) + tm_borders(alpha=.3, col = "white") +
  tm_layout(frame = FALSE)



# define sample grid based on the extent of the House.Points file
grid <-spsample(House.Points, type = 'regular', n = 10000)
# runs the idw for the Price variable of House.Points
idw <- idw(House.Points$price_paid ~ 1, House.Points, newdata= grid)

idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("long", "lat", "prediction")

spg <- idw.output
coordinates(spg) <- ~ long + lat
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
raster_idw <- raster(spg)
# sets projection to British National Grid
projection(raster_idw) <- CRS("+init=EPSG:27700")
# we can quickly plot the raster to check its okay
plot(raster_idw)
persp(raster_idw)


tm_shape(raster_idw) + tm_raster("prediction", style = "quantile", n = 100,
                                 palette = "Reds",legend.show = FALSE) +
  tm_shape(outputAreas) + tm_borders(alpha=.5,) +
  tm_shape(House.Points) + tm_bubbles(size = "price_paid", col = "price_paid",
                                      palette = "Blues", style = "quantile",
                                      legend.size.show = FALSE,
                                      title.col = "Price Paid (£)") +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 1.1,
            legend.title.size = 1.4, frame = FALSE, legend.bg.color = "white",
            legend.bg.alpha = 0.5)
