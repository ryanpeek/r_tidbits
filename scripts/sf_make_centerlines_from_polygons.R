# find centerline from polygons (for rivers)
# see this package: https://github.com/AntoniusGolly/cmgo
# or here: https://stackoverflow.com/questions/9595117/identify-a-linear-feature-on-a-raster-map-and-return-a-linear-shape-object-using/9643004#9643004

# data: https://data.cnra.ca.gov/dataset/san-francisco-bay-and-sacramento-san-joaquin-delta-dem-for-modeling-version-4-2


# Libraries ---------------------------------------------------------------


library(sf)
sf_use_s2(use_s2 = FALSE)
library(tidyverse)
library(glue)
library(mapview)
mapviewOptions(fgb = FALSE)

# Files -------------------------------------------------------------------

fpath <- "~/Downloads/TEMP_data/baydelta_dem/"
st_layers(glue("{fpath}/data_source_dem_20201209/data_source_dem_20201209.shp"))

# Polygs ------------------------------------------------------------------

df_poly <- st_read(glue("{fpath}/data_source_dem_20201209/data_source_dem_20201209.shp"))
df_poly_trim <- df_poly %>% filter(OBJECTID==80)
st_crs(df_poly_trim)$epsg
df_poly_trim <- df_poly_trim %>% st_transform(5070) # AEA Conic
st_crs(df_poly_trim)$epsg
mapview(df_poly_trim)


# Dissolve and Make Boundary Line ------------------------------------------

df_diss <- st_union(df_poly_trim) #%>% as('Spatial') %>% st_as_sf()
df_singlepart <- st_cast(df_diss, 'POLYGON') %>% st_as_sf() %>%
  mutate(id = as.factor(row_number()))


thalweg <- st_cast(df_singlepart$x, "LINESTRING")
mapview(thalweg)


# Make Voronoi ------------------------------------------------------------

df_vor <- st_voronoi(thalweg, bOnlyEdges = TRUE)
# true returns lines, else polys
df_vor_p <- st_voronoi(thalweg, bOnlyEdges = FALSE) %>%
  st_sfc()

# if polys
plot(st_geometry(df_vor_p), col=0, border="black")

# if lines
plot(st_geometry(df_vor), col="black")

# clip?
df_vor_clip <- st_intersection(df_vor_p, thalweg)
plot(df_vor_clip)

# Extra -------------------------------------------------------------------


# # lines and points
# line <- st_read(dsn = linepath) %>% st_transform(epsg)
# point <- st_read(dsn = pointpath) %>% st_transform(epsg)
#
# # Buffer point
# bufferpoint <- st_buffer(point, dist = 1)  %>%
#   st_transform(epsg)
#
# # Cut line with point
# # A helper function that erases all of y from x:
# st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
#
# splitline <- st_erase(line, bufferpoint) %>%
#   mutate(id = as.factor(row_number()))
# buffered <- st_buffer(splitline, dist = 0.5)
# dissolved <- st_union(buffered) %>% as('Spatial') %>% st_as_sf()
# singlepart <- st_cast(dissolved, 'POLYGON') %>%
#   mutate(id = as.factor(row_number()))
#
# thalweg <- st_cast(singlepart$geometry, "LINESTRING")



# FUNCTIONS ------------------------------------------------------

densify <- function(xy,n=5){
  ## densify a 2-col matrix
  cbind(dens(xy[,1],n=n),dens(xy[,2],n=n))
}

dens <- function(x,n=5){
  ## densify a vector
  out = rep(NA,1+(length(x)-1)*(n+1))
  ss = seq(1,length(out),by=(n+1))
  out[ss]=x
  for(s in 1:(length(x)-1)){
    out[(1+ss[s]):(ss[s+1]-1)]=seq(x[s],x[s+1],len=(n+2))[-c(1,n+2)]
  }
  out
}


f_thalweg <- function(xyP,dense){
  require(deldir)
  require(splancs)
  require(igraph)
  require(rgeos)

  ### optionally add extra points
  if(!missing(dense)){
    xy = densify(xyP,dense)
  } else {
    xy = xyP
  }

  ### compute triangulation
  d=deldir(xy[,1],xy[,2])

  ### find midpoints of triangle sides
  mids=cbind((d$delsgs[,'x1']+d$delsgs[,'x2'])/2,
             (d$delsgs[,'y1']+d$delsgs[,'y2'])/2)

  ### get points that are inside the polygon
  sr = SpatialPolygons(list(Polygons(list(Polygon(xyP)),ID=1)))
  ins = over(SpatialPoints(mids),sr)

  ### select the points
  pts = mids[!is.na(ins),]

  dPoly = gDistance(as(sr,"SpatialLines"),SpatialPoints(pts),byid=TRUE)
  pts = pts[dPoly > max(dPoly/1.5),]

  ### now build a minimum spanning tree weighted on the distance
  G = graph.adjacency(as.matrix(dist(pts)),weighted=TRUE,mode="upper")
  T = minimum.spanning.tree(G,weighted=TRUE)

  ### get a diameter
  path = get.diameter(T)

  if(length(path)!=vcount(T)){
    stop("Path not linear - try increasing dens parameter")
  }

  ### path should be the sequence of points in order
  list(pts=pts[path + 1,],tree=T)

}

# get coords of one ring from spatial polys
f_onering <- function(p){p@polygons[[1]]@Polygons[[1]]@coords}

# capture spatial lines from features
f_capture <- function(){
  p=locator(type="l")
  SpatialLines(list(Lines(list(Line(cbind(p$x,p$y))),ID=1)))
}

# Implement  --------------------------------------------------------------

# densify the polygon vertices:
library(smoothr)

df_dens <- smooth(df_diss, method = "densify", n=12)

plot(df_diss, border="black", lwd=4)
plot(df_dens, add=T, border=alpha("yellow", 0.5))

# make sp ver of poly
df_sp <- df_dens %>% st_zm() %>%
  as_Spatial()

# capture or trace over a line
#s <- f_capture()
# buffer the line trace
#p <- gBuffer(df_sp, width=0.2)
plot(df_sp, col="#cdeaff")

# make thalweg
df_thal <- f_thalweg(f_onering(df_sp), dense = 12)
lines(df_thal$pts, col="white")

