censusData <- read.csv("census_data.csv")
houseData <- read.csv("house_data.csv")

# Create spatial DF for houses
House.Points <-SpatialPointsDataFrame(houseData[,6:7], houseData,
                                     proj4string = CRS("+init=EPSG:27700"))

# Load spatial files for Kensington and Chelsea
Output.Areas <- readOGR("data/statistical-gis-boundaries-london/ESRI", "OA_2011_London_gen_MHW")
Output.Areas <- Output.Areas[Output.Areas$LAD11NM=="Kensington and Chelsea",]
plot(Output.Areas)

# Combine variables with prices
OA.Census <- merge(Output.Areas, censusData, by.y ="OA", by.x="OA11CD")
proj4string(OA.Census) <- CRS("+init=EPSG:27700")



# ------------------- Finding neighbours -------------------

neighbours <- poly2nb(OA.Census)
neighbours

plot(OA.Census, border = 'lightgrey')
plot(neighbours, coordinates(OA.Census), add=TRUE, col='red')

neighbours2 <- poly2nb(OA.Census, queen = FALSE)
neighbours2

# ONE AT A TIME, SLOWLY
plot(OA.Census, border = 'lightgrey')
plot(neighbours, coordinates(OA.Census), add=TRUE, col='blue')
plot(neighbours2, coordinates(OA.Census), add=TRUE, col='red')


# ------------------- Running a global spatial autocorrelation -------------------
listw <- nb2listw(neighbours2)
listw

# MORAN TEST ON EMPLOYED VARIABLE
moran.test(OA.Census$employed, listw)
# employed has 0.34 moran statistic so it has a slight postitive autocorrelation - we may say that the data does spatially cluster

# ------------------- Running a local spatial autocorrelation -------------------
# run it two times to make it work
moran <- moran.plot(OA.Census$employed, listw = nb2listw(neighbours2, style = "W"))

# creates a local moran output
local <- localmoran(x = OA.Census$employed,
                    listw = nb2listw(neighbours2, style = "W"))
moran.map <- cbind(OA.Census, local)
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile",
                              title = "local moran statistic")


# -------------------  to create LISA cluster map ------------------- 

quadrant <- vector(mode="numeric",length=nrow(local))
# centers the variable of interest around its mean
m.employed <- OA.Census$employed - mean(OA.Census$employed)
# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])
# significance threshold
signif <- 0.1
# builds a data quadrant
quadrant[m.employed >0 & m.local>0] <- 4
quadrant[m.employed <0 & m.local<0] <- 1
quadrant[m.employed <0 & m.local>0] <- 2
quadrant[m.employed >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(OA.Census,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

# -------------------  GETIS-ORD ------------------- 

# whole map goes brr red - NOT WORKING (maybe other variable will make it work)

# creates centroid and joins neighbours within 0 and x units
nb <- dnearneigh(coordinates(OA.Census),0,800)
# creates listw
nb_lw <- nb2listw(nb, style = 'B')
# plot the data and neighbours
plot(OA.Census, border = 'lightgrey')
plot(nb, coordinates(OA.Census), add=TRUE, col = 'red')


# compute Getis-Ord Gi statistic - NOT WORKING
local_g <- localG(OA.Census$employed, nb_lw)
local_g <- cbind(OA.Census, as.matrix(local_g))
names(local_g)[6] <- "gstat"

tm_shape(local_g) + tm_fill("gstat", palette = "RdBu", style = "pretty") +
  tm_borders(alpha=.4)


# -------------------  LINEAR MODEL ------------------- 

model <- lm(OA.Census$employed ~ OA.Census$white+OA.Census$males)
summary(model)
par(mfrow=c(2,2))
plot(model)


# DOES NOT WORK - try different variables 

resids<-residuals(model)
map.resids <- cbind(OA.Census, resids)
# we need to rename the column header from the resids file
# in this case its the 6th column of map.resids
names(map.resids)[6] <- "resids"
# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")




# ------------------- GWR ------------------- 

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(OA.Census$employed ~ OA.Census$males +
                          OA.Census$white, data=OA.Census, adapt =TRUE)


gwr.model = gwr(OA.Census$employed ~ OA.Census$males+OA.Census$white,
                data = OA.Census, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
#print the results of the model
gwr.model

results <-as.data.frame(gwr.model$SDF)
names(results)

gwr.map <- cbind(OA.Census, as.matrix(results))
qtm(gwr.map, fill = "localR2")


# create tmap objects
map1 <- tm_shape(gwr.map) + tm_fill("white", n = 5, style = "quantile",
                                    title = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map2 <- tm_shape(gwr.map) + tm_fill("OA.Census.white", n = 5, style = "quantile",
                                    title = "white Coefficient") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map3 <- tm_shape(gwr.map) + tm_fill("males", n = 5, style = "quantile",
                                    title = "males") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map4 <- tm_shape(gwr.map) + tm_fill("OA.Census.males", n = 5, style = "quantile",
                                    title = "males Coefficient") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)

grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(2,2)))
# prints a map object into a defined cell
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))


# ------------------- Interpolation ------------------- 

# Create a tessellated surface
dat.pp <- as(dirichlet(as.ppp(House.Points)), "SpatialPolygons")
dat.pp <- as(dat.pp,"SpatialPolygons")
# Sets the projection to British National Grid
proj4string(dat.pp) <- CRS("+init=EPSG:27700")
proj4string(House.Points) <- CRS("+init=EPSG:27700")
# Assign to each polygon the data from House.Points
int.Z <- over(dat.pp,House.Points, fn=mean)
# Create a SpatialPolygonsDataFrame
thiessen <- SpatialPolygonsDataFrame(dat.pp, int.Z)
# maps the thiessen polygons and House.Points
tm_shape(Output.Areas) + tm_fill(alpha=.3, col = "grey") +
  tm_shape(thiessen) + tm_borders(alpha=.5, col = "black") +
  tm_shape(House.Points) + tm_dots(col = "blue", scale = 0.5)



thiessen.crop <-crop(thiessen, Output.Areas)

tm_shape(Output.Areas) + tm_fill(alpha=.3, col = "grey") +
  tm_shape(thiessen.crop) + tm_borders(alpha=.5, col = "black") +
  tm_shape(House.Points) + tm_dots(col = "blue", scale = 0.5)


# maps house prices across thiessen polygons
tm_shape(thiessen.crop) + tm_fill(col = "price_paid", style = "quantile", palette = "Reds",
                                  title = "Price Paid (£)") +
  tm_borders(alpha=.3, col = "black") +
  tm_shape(House.Points) + tm_dots(col = "black", scale = 0.5) +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 1.05,
            legend.title.size = 1.2, frame = FALSE)


# ------------------- Inverse distance weighting ------------------- 

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


# ------------------- 3D BACKFLIP SUPERPOWER NO PANTIES ------------------- 
persp(raster_idw)

idw2 <- as.matrix(raster_idw)
persp3d(idw2, col = "red")

tm_shape(raster_idw) + tm_raster("prediction", style = "quantile", n = 100,
                                 palette = "Reds", legend.show = FALSE)


tm_shape(raster_idw) + tm_raster("prediction", style = "quantile", n = 100,
                                 palette = "Reds", legend.show = FALSE) +
  tm_shape(Output.Areas) + tm_borders(alpha=.5)


tm_shape(raster_idw) + tm_raster("prediction", style = "quantile", n = 100,
                                 palette = "Reds",legend.show = FALSE) +
  tm_shape(Output.Areas) + tm_borders(alpha=.5,) +
  tm_shape(House.Points) + tm_bubbles(size = "price_paid", col = "price_paid",
                                      palette = "Blues", style = "quantile",
                                      legend.size.show = FALSE,
                                      title.col = "Price Paid (£)") +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 1.1,
            legend.title.size = 1.4, frame = FALSE, legend.bg.color = "white",
            legend.bg.alpha = 0.5)


# masks our raster by our output areas polygon file
masked_idw <- mask(raster_idw, Output.Areas)
# plots the masked raster
tm_shape(masked_idw) + tm_raster("prediction", style = "quantile", n = 100,
                                 legend.show = FALSE) +
  tm_shape(House.Points) + tm_bubbles(size = "Price", col = "Price",
                                      palette = "Blues", style = "quantile",
                                      legend.size.show = FALSE,
                                      title.col = "Price Paid (£)") +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 1.1,
            legend.title.size = 1.4, frame = FALSE)


# ------------------- Geostatistical interpolation ------------------- 

grid <-spsample(House.Points, type = 'regular', n = 10000)
# runs the kriging
kriging_result = autoKrige(log(price_paid)~1, House.Points, grid)
plot(kriging_result)


