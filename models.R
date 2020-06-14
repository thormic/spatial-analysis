censusData <- read.csv("census_data.csv")
houseData <- read.csv("house_data.csv")

# Create spatial DF for houses
House.Points <- SpatialPointsDataFrame(houseData[,6:7], houseData,
                                     proj4string = CRS("+init=EPSG:27700"))


House.Agg <- houseData %>%
  group_by(oa11) %>%
  dplyr::summarize(mean_price = mean(price_paid, na.rm=TRUE))

houses_merged <-  censusData %>%
  inner_join(House.Agg, by = c("OA" = "oa11"))


# Load spatial files for Kensington and Chelsea
Output.Areas <- readOGR("data/statistical-gis-boundaries-london/ESRI", "OA_2011_London_gen_MHW")
Output.Areas <- Output.Areas[Output.Areas$LAD11NM=="Kensington and Chelsea",]

# Combine variables with prices
OA.Census <- merge(Output.Areas, censusData, by.y ="OA", by.x="OA11CD")
proj4string(OA.Census) <- CRS("+init=EPSG:27700")

OA.Census.mp <- merge(Output.Areas, houses_merged, by.y ="OA", by.x="OA11CD", all = FALSE)
proj4string(OA.Census.mp) <- CRS("+init=EPSG:27700")




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
# employed has 0.34 moran statistic so it has a slight postitive autocorrelation - 
# we may say that the data does spatially cluster


# ------------------- Running a local spatial autocorrelation -------------------
# run it two times to make it work
moran <- moran.plot(OA.Census$employed, listw = nb2listw(neighbours2, style = "W"))

# creates a local moran output
local <- localmoran(x = OA.Census$employed,
                    listw = nb2listw(neighbours2, style = "W"))
moran.map <- cbind(OA.Census, local)
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", palette = "GnBu", title = "local moran statistic")


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
names(local_g)[58] <- "gstat"

tm_shape(local_g) + tm_fill("gstat", palette = "RdBu", style = "pretty") +
  tm_borders(alpha=.4)


# -------------------  LINEAR MODEL ------------------- 
names(OA.Census[,18:56])

reg_cols <- c('Age_0_17', 'Age_18_29', 'Age_30_44','Age_45_64', 'Age_65_up',
              'males', 'united_kingdom', 'ireland', 'other_eu','other_countries',
              'white', 'black_african', 'kids_english', 'single', 'lowest_quali',
              'highest_quali', 'christian', 'muslim', 'jewish', 'no_religion', 'owned')

model <- lm(OA.Census$employed ~ ., data = OA.Census[,reg_cols])
summary(model)
# model <- lm(OA.Census$employed ~ ., data = OA.Census[,18:56])
# k <- ols_step_backward_p(model, details=TRUE)

sig_cols <- c('males','white', 'black_african', 'kids_english', 'single', 'lowest_quali',
              'highest_quali')

model_sig <- lm(OA.Census$employed ~ ., data = OA.Census[,sig_cols])
summary(model_sig)


sig_cols_2 <- c('white', 'black_african', 'single', 'lowest_quali','highest_quali')
model_sig_2 <- lm(OA.Census$employed ~ .-1, data = OA.Census[,sig_cols_2])
summary(model_sig_2)

# par(mfrow=c(2,2))
plot(model_sig_2)

# plot resids
resids<-residuals(model_sig_2)
map.resids <- cbind(OA.Census, resids)
# we need to rename the column header from the resids file
# in this case its the 6th column of map.resids
names(map.resids)[58] <- "resids"
# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")



# ------------------- GWR ------------------- 

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(OA.Census$employed ~ .-1, data = OA.Census[,sig_cols_2], adapt =TRUE)


gwr.model = gwr(OA.Census$employed ~ .-1,
                data = OA.Census[,sig_cols_2], adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
#print the results of the model
gwr.model

results <-as.data.frame(gwr.model$SDF)
names(results)

gwr.map <- cbind(OA.Census[,sig_cols_2], as.matrix(results))
qtm(gwr.map, fill = "localR2")
names(gwr.map)

# create tmap objects
map1 <- tm_shape(gwr.map) + tm_fill("white", n = 5, style = "quantile", palette="Blues",
                                    title = "white") +
 tm_layout(frame = FALSE)
map2 <- tm_shape(gwr.map) + tm_fill("white.1", n = 5, style = "quantile", palette="Blues",
                                    title = "white coefficient") +
  tm_layout(frame = FALSE)
map3 <- tm_shape(gwr.map) + tm_fill("black_african", n = 5, style = "quantile", palette="Blues",
                                    title = "black") +
  tm_layout(frame = FALSE)
map4 <- tm_shape(gwr.map) + tm_fill("black_african.1", n = 5, style = "quantile", palette="Blues",
                                    title = "black coefficient") +
  tm_layout(frame = FALSE)
map5 <- tm_shape(gwr.map) + tm_fill("single", n = 5, style = "quantile", palette="Blues",
                                    title = "single") +
  tm_layout(frame = FALSE)
map6 <- tm_shape(gwr.map) + tm_fill("single.1", n = 5, style = "quantile", palette="Blues",
                                    title = "single coefficient") +
  tm_layout(frame = FALSE)
map7 <- tm_shape(gwr.map) + tm_fill("lowest_quali", n = 5, style = "quantile", palette="Blues",
                                    title = "lowest qualifications") +
  tm_layout(frame = FALSE)
map8 <- tm_shape(gwr.map) + tm_fill("lowest_quali.1", n = 5, style = "quantile", palette="Blues",
                                    title = "lowest qualifications coefficient") +
  tm_layout(frame = FALSE)
map9 <- tm_shape(gwr.map) + tm_fill("highest_quali", n = 5, style = "quantile", palette="Blues",
                                    title = "highest qualifications") +
  tm_layout(frame = FALSE)
map10 <- tm_shape(gwr.map) + tm_fill("highest_quali.1", n = 5, style = "quantile", palette="Blues",
                                    title = "highest qualifications coefficient") +
  tm_layout(frame = FALSE)

# --- FIRST 2 VARIABLES
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))

# --- SECOND 2 VARIABLES
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(map5, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map7, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map8, vp=viewport(layout.pos.col = 2, layout.pos.row =2))


# --- LAST VARIABLE
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))

print(map9, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map10, vp=viewport(layout.pos.col = 2, layout.pos.row =1))







# -------------------  LINEAR MODEL - PRICE ------------------- 
names(OA.Census[,18:56])

model <- lm(OA.Census.mp$mean_price ~ ., data = OA.Census.mp[,18:58])
k <- ols_step_backward_p(model, details=TRUE)

sig_cols <- c( 'single', 'muslim','highest_quali', 'jewish', 'asian', 'one_car', 'no_cars',
               'Age_30_44', 'employed', 'private_rent')
model_sig <- lm(OA.Census.mp$mean_price ~ ., data = OA.Census.mp[sig_cols])
summary(model_sig)

# plot resids
resids<-residuals(model_sig)
map.resids <- cbind(OA.Census.mp, resids)
# we need to rename the column header from the resids file
# in this case its the 6th column of map.resids
names(map.resids)[58] <- "resids"
# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")



# ------------------- GWR - PRICE ------------------- 

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(OA.Census.mp$mean_price ~ ., data = OA.Census.mp[,sig_cols], adapt =TRUE)


gwr.model = gwr(OA.Census.mp$mean_price ~ .,
                data = OA.Census.mp[,sig_cols], adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
#print the results of the model
gwr.model

results <-as.data.frame(gwr.model$SDF)
names(results)

gwr.map <- cbind(OA.Census.mp[,sig_cols], as.matrix(results))
qtm(gwr.map, fill = "localR2")

names(gwr.map)

sig_cols <- c( 'single', 'muslim','highest_quali', 'jewish', 'asian', 'one_car', 'no_cars',
               'Age_30_44', 'employed', 'private_rent')

# create tmap objects
map1 <- tm_shape(gwr.map) + tm_fill("single", n = 5, style = "quantile", palette="Greens",
                                    title = "single") +
  tm_layout(frame = FALSE)
map2 <- tm_shape(gwr.map) + tm_fill("single.1", n = 5, style = "quantile", palette="Blues",
                                    title = "single coefficient") +
  tm_layout(frame = FALSE)
map3 <- tm_shape(gwr.map) + tm_fill("muslim", n = 5, style = "quantile", palette="Greens",
                                    title = "muslim") +
  tm_layout(frame = FALSE)
map4 <- tm_shape(gwr.map) + tm_fill("muslim.1", n = 5, style = "quantile", palette="Blues",
                                    title = "muslim coefficient") +
  tm_layout(frame = FALSE)

map5 <- tm_shape(gwr.map) + tm_fill("jewish", n = 5, style = "quantile", palette="Greens",
                                    title = "jewish") +
  tm_layout(frame = FALSE)
map6 <- tm_shape(gwr.map) + tm_fill("jewish.1", n = 5, style = "quantile", palette="Blues",
                                    title = "jewish coefficient") +
  tm_layout(frame = FALSE)
map7 <- tm_shape(gwr.map) + tm_fill("asian", n = 5, style = "quantile", palette="Greens",
                                    title = "asian") +
  tm_layout(frame = FALSE)
map8 <- tm_shape(gwr.map) + tm_fill("asian.1", n = 5, style = "quantile", palette="Blues",
                                    title = "asian coefficient") +
  tm_layout(frame = FALSE)
map9 <- tm_shape(gwr.map) + tm_fill("one_car", n = 5, style = "quantile", palette="Greens",
                                    title = "one_car") +
  tm_layout(frame = FALSE)
map10 <- tm_shape(gwr.map) + tm_fill("one_car.1", n = 5, style = "quantile", palette="Blues",
                                     title = "one_car coefficient") +
  tm_layout(frame = FALSE)

map11 <- tm_shape(gwr.map) + tm_fill("no_cars", n = 5, style = "quantile", palette="Greens",
                                    title = "no_cars") +
  tm_layout(frame = FALSE)
map12 <- tm_shape(gwr.map) + tm_fill("no_cars.1", n = 5, style = "quantile", palette="Blues",
                                    title = "no_cars coefficient") +
  tm_layout(frame = FALSE)
map13 <- tm_shape(gwr.map) + tm_fill("Age_30_44", n = 5, style = "quantile", palette="Greens",
                                     title = "Age_30_44") +
  tm_layout(frame = FALSE)
map14 <- tm_shape(gwr.map) + tm_fill("Age_30_44.1", n = 5, style = "quantile", palette="Blues",
                                     title = "Age_30_44 coefficient") +
  tm_layout(frame = FALSE)
map15 <- tm_shape(gwr.map) + tm_fill("employed", n = 5, style = "quantile", palette="Greens",
                                     title = "employed") +
  tm_layout(frame = FALSE)
map16 <- tm_shape(gwr.map) + tm_fill("employed.1", n = 5, style = "quantile", palette="Blues",
                                     title = "employed coefficient") +
  tm_layout(frame = FALSE)
map17 <- tm_shape(gwr.map) + tm_fill("private_rent", n = 5, style = "quantile", palette="Greens",
                                     title = "private_rent") +
  tm_layout(frame = FALSE)
map18 <- tm_shape(gwr.map) + tm_fill("private_rent.1", n = 5, style = "quantile", palette="Blues",
                                     title = "private_rent coefficient") +
  tm_layout(frame = FALSE)
map19 <- tm_shape(gwr.map) + tm_fill("highest_quali", n = 5, style = "quantile", palette="Greens",
                                     title = "highest_quali") +
  tm_layout(frame = FALSE)
map20 <- tm_shape(gwr.map) + tm_fill("highest_quali.1", n = 5, style = "quantile", palette="Blues",
                                     title = "highest_quali coefficient") +
  tm_layout(frame = FALSE)


# --- FIRST 2 VARIABLES
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))

# --- SECOND 2 VARIABLES
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(map5, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map7, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map8, vp=viewport(layout.pos.col = 2, layout.pos.row =2))


# --- THIRD 2 VARIABLES
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(map9, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map10, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map11, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map12, vp=viewport(layout.pos.col = 2, layout.pos.row =2))

# --- FOURTH 2 VARIABLES
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(map13, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map14, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map15, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map16, vp=viewport(layout.pos.col = 2, layout.pos.row =2))

# --- FIFTH 2 VARIABLES
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(map17, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map18, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map19, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map20, vp=viewport(layout.pos.col = 2, layout.pos.row =2))



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
tm_shape(thiessen.crop) + tm_fill(col = "price_paid", style = "quantile", palette = "Blues",
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
  tm_shape(House.Points) + tm_bubbles(size = "price_paid", col = "price_paid",
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


