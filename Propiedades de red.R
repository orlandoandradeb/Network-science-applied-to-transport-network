
###### Propiedads de red 

#opentripplanner es package  que es un fork de otrp de leed university, de acuerdo con el autor esta mas verga este
library(remotes)
library(opentripplanner)   
library(tmap)
library(sp)  #necesario  para cargar la libreria raster
library(raster)
library(sf)   # pasamos un spatial data frame a un SF, contiene en una columa la geometria del vector
library(tidyr)    # para usar la funcion separate y separar  1 columna en 2
library(geodist)   # para la distancia euclediana
library(spatialEco) # con esto se usa la funcion point.in.poly que vincula puntos con areas geoespaciales
library(ggplot2)
library(ggthemes)  # mas temas para ggplot 2
library(mapdeck)
library(stplanr) # libreria para transport planning
library(dplyr)
library(igraph) # esta es una libreria para analisis de redes   https://github.com/igraph/rigraph
library(data.table)
library(magrittr)
library(geosphere)
library(fasttime)
library(tidytransit)
library(MASS)
library(viridis)
library (ineq)   # Me saca indice de gini y curva de lorenz 
library(readxl)


rm(list = ls(all=TRUE))
getwd()
setwd("C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data")





##############            MONTAJE DEL OTP            ############

# Descarar archivo java OTP : 
path_otp <- otp_dl_jar("C:/Users/Hola/Documents/UPC/PhD/Papers/Paper 3/Data/OTP")

# Descarga de datos ejemplo: 
otp_dl_demo("C:/Users/Hola/Documents/UPC/PhD/Papers/Paper 3/Data/OTP/graphs/default")


## Generacion del objeto graph:
log <- otp_build_graph(otp = "C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data/OTP/otp.jar", dir = "C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data/OTP", memory =5120)


# Set up
log2 <- otp_setup(otp =  "C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data/OTP/otp.jar", dir = "C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data/OTP")

# Conexion del OTP a R 
otpcon <- otp_connect()

# Matar al java generado (esto se tiene que hacfer si quieres cambiar de localizacion)
otp_stop(warn = TRUE, kill_all = TRUE)


## pruebas motor de busqueda 
route <- otp_plan(otpcon, fromPlace = c(-103.43997,20.68314 ), toPlace = c(-103.40641, 20.61957), mode = "WALK") # En modalidad de caminando 
route <- otp_plan(otpcon, fromPlace = c(-103.45361,20.72416 ), toPlace = c(-103.45018,20.64829 ), mode = "CAR") 
route <- otp_plan(otpcon, fromPlace = c(-103.43997,20.68314), toPlace = c(-103.40641, 20.61957), mode = c("WALK","TRANSIT"), ncores = 5, numItineraries = 1) # NumItinierari me dara el numero maximo de posibles rutas a tomar


#ploteo de la ruta 
tmap_mode("view")
qtm(route)
###### UNIDADES #### 
# time en segundos 
# distance en metros 
#waiting time en minutos 



############       CREACION DE LA MATRIX        #############

##### MUY IMPORTANTE: Los centroides que se lean deben de tener una geometria de POINT, algunas veces estos puntos se guardan como MultiPoint. 
# Esto acarre problemas, en Qgis esto se puede resolver con la funcion "Convert Multipoints to points" 


######   Lectura de centroides de ORIGEN Y DESTINO 1,2 Y 3   #######
centroides_1 <- read_sf("centroides_1_final.shp")

# Verificar la proyeccion que tiene este SF
st_crs(centroides_1)

### Asignar nuevamente el tipo de proyeccion 4326 
centroides_1 <- st_set_crs(centroides_1, 4326)
st_crs(centroides_1)
plot(centroides_1, pch=20)


centroides_2 <- read_sf("centroides_2_final.shp")
centroides_2 <- st_set_crs(centroides_2, 4326)
plot(centroides_2, pch=20)


centroides_3 <- read_sf("centroides_3_final.shp")
centroides_3 <- st_set_crs(centroides_3, 4326)
plot(centroides_3, pch=20)



### Hacer que la columna ID sea tipo caracter: 
str(centroides_1)
centroides_1$ID <- as.character(centroides_1$ID)
centroides_2$ID <- as.character(centroides_2$ID)
centroides_3$ID <- as.character(centroides_3$ID)
str(centroides_1)



### Generar origenes y destginos para  matriz N*N 
toPlace_1   = centroides_1[rep(seq(1, nrow(centroides_1)), times = nrow(centroides_1)),]
fromPlace_1 = centroides_1[rep(seq(1, nrow(centroides_1)), each  = nrow(centroides_1)),]

toPlace_2   = centroides_2[rep(seq(1, nrow(centroides_2)), times = nrow(centroides_2)),]
fromPlace_2 = centroides_2[rep(seq(1, nrow(centroides_2)), each  = nrow(centroides_2)),]

toPlace_3   = centroides_3[rep(seq(1, nrow(centroides_3)), times = nrow(centroides_3)),]
fromPlace_3 = centroides_3[rep(seq(1, nrow(centroides_3)), each  = nrow(centroides_3)),]


### Verificacion que las capas son POINT (esto se vendría eredando desde la lectura de los mismo centroides)
summary(st_geometry_type(fromPlace_1))
summary(st_geometry_type(fromPlace_2))
summary(st_geometry_type(fromPlace_3))





### GENERACION DE LA MATRIZ #1 
routes_1_1 <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace_1,
                   toPlace = toPlace_1,
                   fromID = fromPlace_1$ID,
                   toID = toPlace_1$ID,
                   get_geometry = FALSE,
                   mode = c("WALK","TRANSIT"),
                   ncores = 5, numItineraries = 1,
                   maxWalkDistance = 1500)

### GENERACION DE LA MATRIZ #2
routes_1_2 <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace_1,
                   toPlace = toPlace_2,
                   fromID = fromPlace_1$ID,
                   toID = toPlace_2$ID,
                   get_geometry = FALSE,
                   mode = c("WALK","TRANSIT"),
                   ncores = 5, numItineraries = 1,
                   maxWalkDistance = 1100)

### GENERACION DE LA MATRIZ #3
routes_1_3 <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace_1,
                   toPlace = toPlace_3,
                   fromID = fromPlace_1$ID,
                   toID = toPlace_3$ID,
                   get_geometry = FALSE,
                   mode = c("WALK","TRANSIT"),
                   ncores = 5, numItineraries = 1,
                   maxWalkDistance = 1100)

### GENERACION DE LA MATRIZ #4
routes_2_1 <- otp_plan(otpcon = otpcon,
                       fromPlace = fromPlace_2,
                       toPlace = toPlace_1,
                       fromID = fromPlace_2$ID,
                       toID = toPlace_1$ID,
                       get_geometry = FALSE,
                       mode = c("WALK","TRANSIT"),
                       ncores = 5, numItineraries = 1,
                       maxWalkDistance = 1100)

### GENERACION DE LA MATRIZ #5
routes_2_2 <- otp_plan(otpcon = otpcon,
                       fromPlace = fromPlace_2,
                       toPlace = toPlace_2,
                       fromID = fromPlace_2$ID,
                       toID = toPlace_2$ID,
                       get_geometry = FALSE,
                       mode = c("WALK","TRANSIT"),
                       ncores = 5, numItineraries = 1,
                       maxWalkDistance = 1100)

### GENERACION DE LA MATRIZ #6
routes_2_3 <- otp_plan(otpcon = otpcon,
                       fromPlace = fromPlace_2,
                       toPlace = toPlace_3,
                       fromID = fromPlace_2$ID,
                       toID = toPlace_3$ID,
                       get_geometry = FALSE,
                       mode = c("WALK","TRANSIT"),
                       ncores = 5, numItineraries = 1,
                       maxWalkDistance = 1300)

### GENERACION DE LA MATRIZ #7
routes_3_1 <- otp_plan(otpcon = otpcon,
                       fromPlace = fromPlace_3,
                       toPlace = toPlace_1,
                       fromID = fromPlace_3$ID,
                       toID = toPlace_1$ID,
                       get_geometry = FALSE,
                       mode = c("WALK","TRANSIT"),
                       ncores = 5, numItineraries = 1,
                       maxWalkDistance = 1300)

### GENERACION DE LA MATRIZ #8
routes_3_2 <- otp_plan(otpcon = otpcon,
                       fromPlace = fromPlace_3,
                       toPlace = toPlace_2,
                       fromID = fromPlace_3$ID,
                       toID = toPlace_2$ID,
                       get_geometry = FALSE,
                       mode = c("WALK","TRANSIT"),
                       ncores = 5, numItineraries = 1,
                       maxWalkDistance = 1100)

### GENERACION DE LA MATRIZ #9
routes_3_3 <- otp_plan(otpcon = otpcon,
                       fromPlace = fromPlace_3,
                       toPlace = toPlace_3,
                       fromID = fromPlace_3$ID,
                       toID = toPlace_3$ID,
                       get_geometry = FALSE,
                       mode = c("WALK","TRANSIT"),
                       ncores = 5, numItineraries = 1,
                       maxWalkDistance = 1100)



### Unir los dataframe 
routes <- rbind(routes_1_1,routes_1_2,routes_1_3,routes_2_1,routes_2_2,routes_2_3,routes_3_1,routes_3_2,routes_3_3)
routes_bus <- filter(routes, mode == "BUS")

str(routes_bus)
routes_bus$fromPlace <- as.numeric((as.character(routes_bus$fromPlace))) 
routes_bus$toPlace <- as.numeric((as.character(routes_bus$toPlace))) 
str(routes_bus)


# Guadar la matriz
write.csv(routes_bus, file =  "matriz_OD_FINAL_paper3.csv")

# Lectura de la matriz OD FINAL
routes_bus <- read.csv("matriz_OD_FINAL_paper3.csv")




  
#######################                    GRADO DE ACECIBILIDAD                   #########################  

# Lectura de zonificacion 
Hexagonos <- shapefile("hexagonos_final.shp")
plot(Hexagonos)
proj4string(Hexagonos) <- CRS("+init=EPSG:4326")
Hexagonos@data   # ver el data frame del poligono


#Creamos un mismo DF
OD_Matriz <- routes_bus

# MEddia actual de tiempos de viaje , en minutos 
mean(OD_Matriz$duration)/60    # da un valor de 87.178 que es 1 hora con 45 minutos 

# Funcion de eliminacion de ouliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.10, .90), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


### remocion 
OD_Matriz$duration <- remove_outliers(OD_Matriz$duration)


# Eliminimacion de  lecturas con NA (la remocion genera NA en las filas removidas) y elimiancion de columnas que ya contienen NA
OD_Matriz$fare <- NULL
OD_Matriz$fare_currency <- NULL 
OD_Matriz$flexDrtAdvanceBookMin <- NULL 
OD_Matriz$elevationGained <- NULL 
OD_Matriz$elevationLost <- NULL 

OD_Matriz<- OD_Matriz[complete.cases(OD_Matriz[ , 1]),]  # Solo tomando en consideracion la columna 1 
mean(OD_Matriz$duration)/60  # da una media de 87.01 minutos


# Restar el tiempo de espera extra dado a trasnfers inecesarias por  fallo en el routing 
OD_Matriz$transfers_corregidor [OD_Matriz$transfers == 0 ] <-1
OD_Matriz$transfers_corregidor [OD_Matriz$transfers == 1 ] <-2
OD_Matriz$transfers_corregidor [OD_Matriz$transfers == 2 ] <-3
OD_Matriz$transfers_corregidor [OD_Matriz$transfers == 3 ] <-3
OD_Matriz$transfers_corregidor [OD_Matriz$transfers == 4 ] <-3  

# Duracion final corregida por fallas en el routing (que da rutas de mas trasnfers),modificacion de la velocidad comercial y outliers 
OD_Matriz$duration <- OD_Matriz$walkTime + OD_Matriz$transitTime + (OD_Matriz$transfers_corregidor*180)
mean(OD_Matriz$duration)/60  # da una media de 69 minutos



# MOdificacion de la velocidad comercial a 21 km/h y remocion de tiempos  de espera inecesarios por falla en el routing 
# Velocidad media en los  GTFS = 20 km/h 
# Modificacion de la velocidada 21 km/h (es decir 1 km/h más)
1/20 # 1 km/h más es 5 % mayor velocidad
OD_Matriz$duration <- OD_Matriz$duration * 0.95 
mean(OD_Matriz$duration)/60  # da una media de 66  minutos




###### AGRUPAMIENTO y generacion de tiempos medios por centroide 

OD_Matriz$count <- 1 

#Sumaoria de tiempo 
contabilizacion <- aggregate(OD_Matriz$duration, by=list(fromPlace=OD_Matriz$fromPlace, toPlace=OD_Matriz$toPlace), FUN=sum) 
names(contabilizacion)[names(contabilizacion) == "x"] <- "time"

correccion <- aggregate(OD_Matriz$count, by=list(fromPlace=OD_Matriz$fromPlace, toPlace=OD_Matriz$toPlace), FUN=sum) 
names(correccion)[names(correccion) == "x"] <- "correcion"

# Unirlos 
contabilizacion <- merge(correccion, contabilizacion, by=c("fromPlace","toPlace")) # NA's match
contabilizacion$time <- contabilizacion$time / contabilizacion$correcion

# lo pasamos a minutos 
contabilizacion$time <- contabilizacion$time / 60 

#generamos los tiempos medios por centroide 
tiempos_medios <-aggregate(contabilizacion$time, by=list(fromPlace=contabilizacion$fromPlace), FUN=mean) 
names(tiempos_medios)[names(tiempos_medios) == "x"] <- "time"
names(tiempos_medios)[names(tiempos_medios) == "fromPlace"] <- "id"

## Media de los tiempos medios de todos los centroides 
mean(tiempos_medios$time)   ### El mean final me da 65 minutos 


### Generamos el SHIMBEL INDEX 
# La formula es:     N-1 / sumatoria de tiempo 
(nrow(tiempos_medios) - 1 ) / (sum(tiempos_medios$time))






#### IMPORTNATE: el DF de tiempos_medios es el que contiene el los tiempos medios ya de todos los centroides 
# no tiene coordenadas se las vamos a poner con los DF de los centroides antes leidos, esto lo necesitaré para poder vincular 

centroides_1$Longitude<- NULL
centroides_1$Latitude<- NULL

centroides_totales <- rbind(centroides_1, centroides_2)
centroides_totales <- rbind(centroides_totales, centroides_3)


# Vincular el DF de centroides totales con el DF que contiene los tiempos medios de desplazamiento 
colnames(tiempos_medios)[which(names(tiempos_medios) == "id")] <- "ID"  # Cambio de nombre 


# UNION 
centroides_totales <- merge(centroides_totales, tiempos_medios, by="ID", all = TRUE)



######        UNION DE LOS POINTS QUE CONTIENE LOS TIEMPOS MEDIOS CON EL POLIGONO DE LA ZONIFICACION    ######
### La funcion points in poly requiere que los puntos esten en spatialpoint dataframe por lo que se parasa de SF 
# a spatial point data frame
centroides_totales <- as(centroides_totales, 'Spatial')

#### Union de puntos y poligonos 
plot(Hexagonos)
points(centroides_totales, pch=20)
pts.poly <- point.in.poly(centroides_totales, Hexagonos)
head(pts.poly@data)
plot(pts.poly)



##### Union ya en el DF de los hexagonos 
## dado que el id de los centrpoides y la de los poligonos son el mismo hay que agregar el valor medio de tiempo de estos centrpoides 
# directamente a los poligonos 

Hexagonos@data  
Hexagonos@data = data.frame(Hexagonos@data, pts.poly@data[match(Hexagonos@data [, "id"], pts.poly@data[,"id"]),])
Hexagonos@data  

Hexagonos2<- st_as_sf(Hexagonos)  # lo pasamos a datafrmae SF para ponder usarlo  senciillamente con Mapdeck 
Hexagonos2$bottom.1 <- NULL
Hexagonos2$left <- NULL
Hexagonos2$top <- NULL
Hexagonos2$right <- NULL
Hexagonos2$bottom <- NULL
Hexagonos2$id.1 <- NULL


# Plotear con mapbox
MAPBOX=   ### YOUR KEY 
set_token(Sys.getenv("MAPBOX"))
key=MAPBOX

mapdeck(token = key, style = "mapbox://styles/orlandoandradeb/cjwrsrdko09mi1cpm91ptszfs") %>%
  add_polygon(
    data = Hexagonos2
    , layer = "polygon_layer"
    , fill_colour = "time"
    , legend = TRUE
    , stroke_width=20
    , stroke_opacity=0.9
    ,fill_opacity=0.9 ) 


####### EXPORATCION DEL SHAPE 
shape_acesibilidad <- Hexagonos2 
shape_acesibilidad <- as_Spatial(shape_acesibilidad)


library(rgdal)
writeOGR(shape_acesibilidad, dsn = 'C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data', layer = 'shape_acesibilidad', driver = "ESRI Shapefile")





####  BOX PLOTS
# Box plot  de los tiempos medios por centroide 
boxplot(tiempos_medios$time)



# plot con tm_shape         
tm_shape(Hexagonos2) + tm_fill(col = "time", palette = "-inferno", breaks =  c(40,45,50,55,60,65,70,75,80,85,90,95,100, Inf)) + tm_basemap(leaflet::providers$CartoDB.Positron) +
  tm_layout(title = "Grade of acesability", legend.outside= TRUE, frame = FALSE)  + tm_borders(col = "#797776", lwd = 1) + tm_scale_bar(breaks = c(0, 10), position=c("left", "bottom")) 



##### Analisis estadisicto descritivo:
OD_Matriz$duration_m <- OD_Matriz$duration/60  # lo pasamos en una nueva variable a minutos
OD_Matriz$distance_km <- OD_Matriz$distance/1000 # lo pasamos a km

# Distribucion de tiempos medios de despalzamientos en minutos 
ggplot(OD_Matriz, aes(x=duration_m))  +  labs( title = "Histogramas de tiempos de desplazamientos",  y= "Frecuencia", x= "Minutos") + 
  geom_histogram(binwidth = 1, aes(fill = ..count..) ) +    #bindwidth indicara el salto de paso entre los grupos de medicion de la  frecuencia 
  theme_hc(style ="darkunica" ) +  theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6")) +
  ylim(0, 4000)  + xlim(0,170)  #     + scale_fill_gradient(low="white", high="blue")   # para agregar otro difusion de colores


# Box plot de los tiempos de todos los OD pairs de la matriz 
boxplot(OD_Matriz$duration_m)


# Distribucion de las longitudes medias de despalzamientos  por nodo en km 
ggplot(OD_Matriz, aes(x=distance_km))  +  labs( title = "Histograma de distancias",  y= "Frecuencia", x= "Km") + 
  geom_histogram(binwidth = 1, aes(fill = ..count..) ) +    #bindwidth indicara el salto de paso entre los grupos de medicion de la  frecuencia 
  theme_hc(style ="darkunica" ) +  theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6")) +
  ylim(0, 20000)  + xlim(0,30)  #     + scale_fill_gradient(low="white", high="blue")   # para agregar otro difusion de colores


## Box plot de la distance de los viajes 
boxplot(OD_Matriz$distance_km)


# Ploteo de tiempo vs distancia con densidad de puntos 
theme_set(theme_bw(base_size = 16))

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

ggplot(OD_Matriz) + geom_point(aes(distance_km,duration_m))
OD_Matriz$density <- get_density(OD_Matriz$distance_km, OD_Matriz$duration_m, n=85)


ggplot(OD_Matriz) + geom_point(aes(distance_km,duration_m, color = density)) + scale_colour_viridis() +
  labs( title = "Time vs Distance",  y= "Minutes", x= "Km") +
  theme_hc(style ="darkunica" ) +  theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6")) 
# Fuente: https://slowkow.com/notes/ggplot2-color-by-density/








###############                    DEUTER INDEX           #################

# Exctraccion de id de viajes y sus distancias 
deuter <-as.data.frame(OD_Matriz$fromPlace) 
deuter$toPlace <- OD_Matriz$toPlace
deuter$distancia <- OD_Matriz$distance  # esta en metros 
deuter$walk_distancia <- OD_Matriz$walkDistance # Esta en metros 
names(deuter)[names(deuter) == "OD_Matriz$fromPlace"] <- "fromPlace"
deuter$distancia_total <- deuter$distancia + deuter$walk_distancia   # Distancia total 


## Agrupamiento de distancias para  los viajes que hacen 1 o más trasnferencias. 
deuter <- aggregate(deuter$distancia_total, by=list(fromPlace=deuter$fromPlace, toPlace=deuter$toPlace), FUN=sum)
names(deuter)[names(deuter) == "x"] <- "transit_distance"


## Lectura de coordenadas 
cordenadas <- rbind(centroides_1,centroides_2)
cordenadas  <- rbind(cordenadas, centroides_3)



##### Separar las coordenadas de la columna geometry en columnas latitud y longtud 
library(tidyverse)

cordenadas <- cordenadas %>% mutate(latitude = unlist(map(cordenadas$geometry,1)),
         longitude = unlist(map(cordenadas$geometry,2)))


# cambio de nombres de las columnas y eliminar columna de geometria
names(cordenadas)[names(cordenadas) == "ID"] <- "fromPlace"
cordenadas$geometry <- NULL
cordenadas


#Combinar 
deuter <- merge(deuter, cordenadas, by="fromPlace")
names(deuter)[names(deuter) == "latitude"] <- "latitude_o"
names(deuter)[names(deuter) == "longitude"] <- "longitude_o"


####  AHORA PONER LAS COORDENDASD DE DESTINO 
names(cordenadas)[names(cordenadas) == "fromPlace"] <- "toPlace"
deuter <- merge(deuter, cordenadas, by="toPlace")
names(deuter)[names(deuter) == "latitude"] <- "latitude_d"
names(deuter)[names(deuter) == "longitude"] <- "longiude_d"


#### Generación de la Distancia euclediana 
library(geodist)

deuter$disntace_eucle <-distHaversine(deuter[,4:5], deuter[,6:7])


# Fraccion de distancia euclediana vs distancia en transito 
deuter$fraccion <- deuter$disntace_eucle / deuter$transit_distance 



### Limpieza de outliers 
### remocion (uso la funcion ya creada anteriormnete)
deuter$percentage <- remove_outliers(deuter$fraccion)  # Percentaje es la nueva que se le removieron los outliers 



# Remover las lectruras incorrectas 
deuter_2 <- na.omit(deuter)

# Box plot de distancia euclediana 
deuter_2$transit_distance <- deuter_2$transit_distance / 1000 # lo pasamos a km 
deuter_2$disntace_eucle <- deuter_2$disntace_eucle / 1000 # lo pasamos a km

boxplot(deuter_2$disntace_eucle) # euclediana 
boxplot(deuter_2$transit_distance)


########## RESULTADOS FINALES    ######

### Deuter index de toda la red 
sum(deuter_2$disntace_eucle) / sum(deuter_2$transit_distance)

# Media de distancia sobre red de transporte 
mean(deuter_2$transit_distance)

# Media de distancia euclediania
mean(deuter_2$disntace_eucle)







############               RESILIENCIA DE RED:average.path.length , betweness centrality, NODE DEGREE         ##############

# Conversion de los  GTFS a un objeto igraph para en analisis de  propiedades de red,
# fuente: https://github.com/rafapereirabr/gtfs_to_igraph/blob/master/gtfs_to_igraph.R

library(igraph)
library(data.table)
library(dplyr)
library(magrittr)
library(sp)
library(geosphere)
library(fasttime)



############ 0. read GTFS files   -----------------
my_gtfs_feeds <- list.files(path = "C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data/gtfs gdl propriedades de red/", pattern ="gtfs gdl.zip", full.names = T)  

   list_gtfs= my_gtfs_feeds
   dist_threshold = 5    # el buffer distancia de simpificacin de paradas 
   
  cat("reading GTFS data \n")
  
  
  # function to read and rbind files from a list with different GTFS.zip
  tmpd <- tempdir()
  unzip_fread_gtfs <- function(zip, file) { unzip(zip, file, exdir=tmpd) %>% fread(colClasses = "character") }
  unzip_fread_routes <- function(zip, file) { unzip(zip, file, exdir=tmpd) %>% fread(colClasses = "character", select= c('route_id', 'route_short_name', 'route_type', 'route_long_name')) }
  unzip_fread_trips <- function(zip, file) { unzip(zip, file, exdir=tmpd) %>% fread(colClasses = "character", select= c('route_id', 'service_id', 'trip_id', 'direction_id')) }
  unzip_fread_stops <- function(zip, file) { unzip(zip, file, exdir=tmpd) %>% fread(colClasses = "character", select= c('stop_id', 'stop_name', 'stop_lat', 'stop_lon', 'parent_station', 'location_type')) }
  unzip_fread_stoptimes <- function(zip, file) { unzip(zip, file, exdir=tmpd) %>% fread(colClasses = "character", select= c('trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence')) }
  
  # Read
  stops <- lapply( list_gtfs , unzip_fread_stops, file="stops.txt")  %>% rbindlist()
  stop_times <- lapply( list_gtfs , unzip_fread_stoptimes, file="stop_times.txt")  %>% rbindlist()
  routes <- lapply( list_gtfs , unzip_fread_routes, file="routes.txt")  %>% rbindlist()
  trips <- lapply( list_gtfs , unzip_fread_trips, file="trips.txt")  %>% rbindlist()
  calendar <- lapply( list_gtfs , unzip_fread_gtfs, file="calendar.txt")  %>% rbindlist()
  
  
### Modificacion propia 
  
  stops$location_type = 1 
  
  
  
  
  
  # make sure lat long are numeric, and text is encoded
  stops[, stop_lon := as.numeric(stop_lon) ][, stop_lat := as.numeric(stop_lat) ]
  Encoding(stops$stop_name)  <- "UTF-8" 
  
  
  
  ############  1. Identify stops that closee than distance Threshold in meters  ------------------
  cat("calculating distances between stops \n")
  
  ### Convert stops into SpatialPointsDataFrame
  
  # lat long projection
  myprojection_latlong <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # convert stops into spatial points
  coordinates(stops) <- c("stop_lon", "stop_lat")
  proj4string(stops) <- myprojection_latlong
  stops <- spTransform(stops, myprojection_latlong)
  
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(stops, stops, fun=distHaversine)
  
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  stops@data$clust <- cutree(hc, h=dist_threshold)
  gc(reset = T)
  
  
  # convert stops back into  data frame
  df <- as.data.frame(stops) %>% setDT()
  df <- df[order(clust)]
  df <- unique(df) # remove duplicate of identical stops
  head(df)
  
  # identify how many stops per cluster
  df[, quant := .N, by = clust]
  table(df$quant)
  
  plot(df$stop_lon, df$stop_lat, col=df$clust)
  
  
  
  
  ############ 2. Identify and update Parent Stations  ------------------
  cat("Identifying and updating Parent Stations \n")

  # How many stops have a Parent Station 
  nrow(df[ parent_station !=""])
  
  # How many stops without Parent Station 
  nrow(df[ parent_station ==""])
  
  # Stops which are Parent Stations (location_type==1) will be Parent Stations of themselves
  df[ location_type==1, parent_station := stop_id ]
  
  # in case the field location_type is missinformed 
  df[ parent_station=="" & stop_id %in% df$parent_station, parent_station := stop_id ]
  df[ quant > 1 , parent_station:= ifelse( parent_station !="" , parent_station,
                                           ifelse( parent_station=="" & stop_id %in% df$parent_station, stop_id, "")), by=clust]

  #total number of stops without Parent Station
  nrow(df[ parent_station ==""])
  
  
  
  
  # Update Parent Stations for each cluster
  
  # a) Stops which alread have parent stations stay the same
  # b) stations with no parent, will receive the parent of the cluster
  df[ quant > 1 , parent_station:= ifelse( parent_station !="" , parent_station,
                                           ifelse( parent_station=="", max(parent_station), "")), by=clust]
  
  nrow(df[ parent_station ==""])
  
  
  # d) For those clusters with no parent stations, get the 1st stop to be a Parent
  df[ quant > 1 , parent_station:= ifelse( parent_station !="" , parent_station,
                                           ifelse( parent_station== "", stop_id[1L], "")), by=clust]
  
  nrow(df[ parent_station ==""])
  
  # all clusters > 1 have a parent station                                     
  df[quant > 1 & parent_station==""][order(clust)] # should be empty
  
  # Remaining stops without Parent Station
  nrow(df[ parent_station ==""]) 
  
  # make sure parent stations are consistent within each cluster with more than one stop
  df[ quant > 1 , parent_station := max(parent_station), by=clust]
  unique(df$parent_station) %>% length()
  
  # for the lonly stops, make sure they are the Parent station of themselves
  df[ quant ==1 & parent_station=="" , parent_station := stop_id , by=stop_id]
  nrow(df[ parent_station ==""]) == 0
  
  
  
  
  ############ 3. Update Lat long of stops based on parent_station -----------------------
  
  # Update in stops data: get lat long to be the same as 1st Parent Station
  df[, stop_lon := stop_lon[1],  by=parent_station]
  df[, stop_lat := stop_lat[1],  by=parent_station]
  
  
  # Update in stop_times data: get lat long to be the same as 1st Parent Station
  
  
  # Add parent_station info to stop_times
  # merge stops and stop_times based on correspondence btwn stop_times$stop_id and df$stop_id
  stop_times[df, on= 'stop_id', c('clust', 'parent_station') := list(i.clust, i.parent_station) ]
  
  
  # CRUX: Replace stop_id with parent_station
  stop_times[ !is.na(parent_station) , stop_id := parent_station ]
  df[ , stop_id := parent_station ]
  
  # remove repeated stops
  df <- unique(df)
  
  
  
  
  ############ 4. identify transport modes, route and service level for each trip  -----------------------
  
  routes <- routes[,.(route_id, route_type)]                # keep only necessary cols
  trips <- trips[,.(route_id, trip_id, service_id)]         # keep only necessary cols
  trips[routes, on=.(route_id), route_type := i.route_type] # add route_type to trips
  
  # add these columns to stop_times: route_id, route_type, service_id
  stop_times[trips, on=.(trip_id), c('route_id', 'route_type', 'service_id') := list(i.route_id, i.route_type, i.service_id) ]
  gc(reset = T)
  
  # # Only keep trips during weekdays 
  #   # remove columns with weekends
  #   calendar <- calendar[, -c('saturday', 'sunday')]
  #   
  #   # keep only rows that are not zero (i.e. that have service during weekday)
  #   calendar <- calendar[rowMeans(calendar >0)==T,]
  #   
  #   # Only keep those trips which run on weekdays
  #   stop_times2 <- subset(stop_times, service_id %in% calendar$service_id)
  
  
  # Get edited info for stop_times and stops
  stops_edited <- df[, .(stop_id, stop_name, parent_station, location_type, stop_lon, stop_lat)]
  stop_times_edited <- stop_times[, .(route_type, route_id, trip_id, stop_id, stop_sequence, arrival_time, departure_time)]
  
  # make sure stop_sequence is numeric
  stop_times_edited[, stop_sequence := as.numeric(stop_sequence) ]
  
  # make sure stops are in the right sequence for each group (in this case, each group is a trip_id)
  setorder(stop_times_edited, trip_id, stop_sequence, route_id)
  gc(reset = T)
  
  
  
  ############ 5. Indentify links between stops   -----------------
  cat("Identifying links between stops \n")
  
  
  # calculate travel-time (in minutes) between stops
  
  # Convert times to POSIX to do calculations
  stop_times_edited[, arrival_time :=   paste("2000-01-01",arrival_time)  ] # add full date. The date doesnt' matter much
  stop_times_edited[, arrival_time :=   fastPOSIXct(arrival_time)  ] # fast conversion to POSIX
  
  # calculate travel time (in minutes) between stops 
  stop_times_edited[ , travel_time := difftime( data.table::shift(arrival_time, type = "lead"), arrival_time, units="mins")  , by=trip_id][ , travel_time := as.numeric(travel_time) ]
  
  
  # create three new columns by shifting the stop_id, arrival_time and departure_time of the following row up 
  # you can do the same operation on multiple columns at the same time
  stop_times_edited[, `:=`(stop_id_to = shift(stop_id, type = "lead"), 
                           arrival_time_stop_to = shift(arrival_time, type = "lead"),
                           departure_time_stop_to = shift(departure_time, type = "lead")
  ),
  by = .(trip_id, route_id)]
  
  # you will have NAs at this point because the last stop doesn't go anywhere. Let's remove them
  stop_times_edited <- na.omit(stop_times_edited) 
  
  # frequency of trips per route (weight)
  relations <- stop_times_edited[, .(weight = .N), by= .(stop_id, stop_id_to, route_id, route_type)]
  relations <- unique(relations)
  
  # reorder columns
  setcolorder(relations, c('stop_id', 'stop_id_to', 'weight', 'route_id', 'route_type'))
  
  
  # now we have 'from' and 'to' columns from which we can create an igraph
  head(relations)
  
  # plot densit distribution of trip frequency
  density(relations$weight) %>% plot() 
  
  
  # subset stop columns
  temp_stops <- stops_edited[, .(stop_id, stop_lon, stop_lat)]     #
  temp_stops <- unique(temp_stops)
  
  # remove stops with no connections, and remove connections with ghost stops
  e <- unique(c(relations$stop_id, relations$stop_id_to))
  v <- unique(temp_stops$stop_id)      
  
  d <- setdiff(v,e)  # stops in vertex data frame that are not present in edges data 
  dd <- setdiff(e,v) # stops in edges data frame that are not present in vertex data
  
  temp_stops <- temp_stops[ !(stop_id %in% d) ]   # stops with no connections
  relations <- relations[ !(stop_id %in% dd) ]    # trips with ghost stops
  relations <- relations[ !(stop_id_to %in% dd) ] # trips with ghost stops
  
  
  #######  Overview of the network being built 
  cat("Number of nodes:", unique(relations$stop_id) %>% length(), " \n")
  cat("Number of Edges:", nrow(relations), " \n")
  cat("Number of routes:",  unique(relations$route_id) %>% length(), " \n")
  
  
  
  ############ 6. Build igraph ---------------------
  cat("building igraph \n")
  
  g <- graph_from_data_frame(relations, directed=TRUE, vertices=temp_stops)
  
  
#Fuente:   https://github.com/rafapereirabr/gtfs_to_igraph
  
  
  
  
### Guardar el objeto igraph 
write.graph(g,"C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data/g_ultimo", format="edgelist")  # eesta mejor edgelist FORMAT 
  
## sino  lo lees del archivo exportado: 
g_ultimo <- g


  
### Lectuta del objeto igraph 
g_ultimo <- read.graph("g_ultimo", format = "edgelist")
##############################################################################################################################  
  
  



  
  
#########        PROPIEDADES DE RED  RESILIENCIA DE LA RED BETWENEES CENTRALITY   y la CENTRALIDAD   ###### 
  
#### node degree , es el numero de links que pasamos sobre un nodo (numero de rutas que pasan por un stop)
network_proprieties <- data.frame(degree(g_ultimo))
network_proprieties$stop_id <- rownames(network_proprieties)  ### Le doy  un ID a cada uno de los nodos (paradas) que ya contienen su nodal degree


### Vinculacion de las coordenadas y ploteo 
  
# se pasa el stops de spatial poligon dataframe a un data frame 
stops2 <- st_as_sf(stops)
# stops  <- as_Spatial(stops)   # si quisiese parasrlo de  SF  a spatial points data frame de nuevo  

#Existen realmente 3361 paradas, el analisis de redes me da 3273 NODOS, la diferencia es de 88, estos 88 nodos o paradas se eliminaron por estar dentro 
# de una cercania menor a 30 metros, valores distintos de este radio entre paradas pueden modiciar la diferencia entre nodos y paradas  

# Los unimos por la columa de stop_id 
network_proprieties <-merge( stops2 ,  network_proprieties, by="stop_id")

names(network_proprieties)[names(network_proprieties) == "degree.g."] <- "node_degree"



####  betwneeness centrality 
betwnees_centrality <-  data.frame(betweenness(g_ultimo)) 
betwnees_centrality$stop_id <- rownames(betwnees_centrality) # obtenemos el ID de los stops 

network_proprieties <-merge(  betwnees_centrality,  network_proprieties, by="stop_id")
names(network_proprieties)[names(network_proprieties) == "betweenness.g."] <- "betwenes_centrality"
network_proprieties <- st_as_sf(network_proprieties)


#### Media de betwneess centrality
mean(network_proprieties$betweenness.g_ultimo.)


# Box plot de la distribucion de la centralidad 
boxplot(network_proprieties$betweenness.g_ultimo.)



## Cambio de nombre de las columnas  de network proprieties 
names(network_proprieties)[names(network_proprieties) == "betweenness.g_ultimo."] <- "betwenes_centrality"
names(network_proprieties)[names(network_proprieties) == "degree.g_ultimo."] <- "node_degree"


## El codigo anterio hace una contagilizacion de las veces que cada nodo es usado como el camino mas corto entre 2 pares de nodos. Sin embargo, esto no es el betwness centrality 
# el betwness centrality tambien toma en consideracion el numero TOTAL de caminos cortos posibles entre los distintos nodos. En este sentido se hace un ratio entre 
# el numero de veces que ese nodo fue usado como parte del camino mas corto / el numero total de caminos posible entre los nodos 


####### Medición de la CENTRALIDAD  (Closness centralidty)
# El clonsess cenaltritgy me mide que tan centraedo esta un nodo, Se basa en calcular la suma o bien el promedio de las distancias geodésicas (o longitudes de los caminos más cortos) desde un nodo hacia todos los demás. Note que mientras mayor sea la «distancia» entre dos vértices, menor será la «cercanía» entre estos.


centralidad <- closeness(g_ultimo)
centralidad <- as.data.frame(centralidad)


# Le damos el ID que est en el indice 
centralidad$stop_id <- rownames(centralidad)

# Los unimos con el DF de stops que contiene las coordenadas  y acumula todas las propiedades de red geenradas hasta ahora 
network_proprieties <-merge(network_proprieties, centralidad, by="stop_id")


# Visualizacion de la centralidade de los nodos 
library(mapdeck)

MAPBOX= # YOUR KEY 
set_token(Sys.getenv("MAPBOX"))
key=MAPBOX

mapdeck(token = key, style = "mapbox://styles/orlandoandradeb/cjwrsrdko09mi1cpm91ptszfs", pitch = 45) %>%
  add_scatterplot(
    data = network_proprieties
    , layer = "polygon_layer"
    , fill_colour = "betwenes_centrality"
    , legend = TRUE
    , palette = "plasma"
    , radius = 200
    , na_colour = "#808080FF"
  ) # elevation = "Time" 


### Exportacion del shape con el betwnees centrality 
shape_betwenes_centrality <- network_proprieties
shape_betwenes_centrality <- as_Spatial(shape_betwenes_centrality)

writeOGR(shape_betwenes_centrality, dsn = 'C:/Users/orlan/Documents/UPC/PhD/Papers/Paper 3/Data', layer = 'shape_betwnes_centrality', driver = "ESRI Shapefile")




mapdeck(token = key, style = "mapbox://styles/orlandoandradeb/cjwrsrdko09mi1cpm91ptszfs") %>%
  add_scatterplot(
    data = network_proprieties
    , layer = "polygon_layer"
    , fill_colour = "node_degree"
    , legend = TRUE
    , palette = "plasma"
    , radius = 200
    , na_colour = "#808080FF"
  ) # elevation = "Time" 


mapdeck(token = key, style = "mapbox://styles/orlandoandradeb/cjwrsrdko09mi1cpm91ptszfs", pitch = 45) %>%
  add_scatterplot(
    data = network_proprieties
    , layer = "polygon_layer"
    , fill_colour = "centralidad"
    , legend = TRUE
    , palette = "plasma"
    , radius = 200
    , na_colour = "#808080FF"
  ) # elevation = "Time" 


## NOTA IMPORANTE: en el caso del betwnees centrality el OD matriz que se genera es de cada stop, es decir cada cluster
# de stops generados ira haciendo viajes N*N en este caso una matrix de alrededor de 2,000 elevada a la 2,000








#######     Analisis de distribucion del betwness centrality    ######

# Reordenar el dataframe de menor a mayor 
network_proprieties2 <- network_proprieties
network_proprieties2<-   network_proprieties2[order(network_proprieties2$betwenes_centrality),]  # re ordenamos 

summary(network_proprieties2$betwenes_centrality)



### demosle un ID de 1 a N con el nuevo orden 
network_proprieties2$observation <- 1:nrow(network_proprieties2) 


## Ploteo 
ggplot(network_proprieties2) +  geom_line(aes( x= observation, y=cumsum(betwenes_centrality)/sum(betwenes_centrality) ), colour="#1c8fa9", size=1.5 ) +  labs( title = "Frecuencia acumulativa",  y= "Frecuencia", x= "Nodos") + 
  theme_hc(style ="darkunica" ) +  theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6")) 

ggplot(network_proprieties2) +  geom_line(aes( x= observation, y=cumsum(betwenes_centrality)), colour="#1c8fa9", size=1.5 ) +  labs( title = "Frecuencia acumulativa",  y= "Frecuencia", x= "Nodos") + 
  theme_hc(style ="darkunica" ) +  theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6")) 
# http://www.r-tutor.com/elementary-statistics/quantitative-data/cumulative-frequency-graph
# https://www.r-tutor.com/elementary-statistics/quantitative-data/cumulative-relative-frequency-graph



### Index distribution GINI INDEX DISTRIBUTION 

ineq(network_proprieties$betwenes_centrality, type = "Gini")   # sacamos el indice GINI de la distribucion de variable betwness 
# Explicacion de los varlores de GINI:   

# El coeficiente de Gini es un número entre 0 y 1, en donde 0 se corresponde con la perfecta igualdad (todos tienen los mismos ingresos)
# y donde el valor 1 se corresponde con la perfecta desigualdad (una persona tiene todos los ingresos y los demás ninguno)

plot(Lc(network_proprieties$betwenes_centrality), main="Curva de Lorenz",  xlab="Porcentaje de nodos", ylab="Porcentaje acumulativo de lecturas")
# Explicacioan de la curva de Lorenz 




# Identificacion espacial del 20 % que tiene mas influencia
tail(network_proprieties2, n=10)  # EJEMPLO 
nrow(network_proprieties)*0.2  # sacamos cuantos lineas es el 20%
nodos_influencia <- (tail(network_proprieties2, n = 655 ))

# sacamos el porcentaje de influencia que tiene el 20 % con mayor influencia 
(sum(nodos_influencia$betwenes_centrality) / sum(network_proprieties$betwenes_centrality)) * 100  


# PLOTEO DE los nodos  mas infuyentes 
mapdeck(token = key, style = "mapbox://styles/orlandoandradeb/cjwrsrdko09mi1cpm91ptszfs") %>%
  add_scatterplot(
    data = nodos_influencia
    , layer = "polygon_layer"
    , fill_colour = "#d62a2a"
    , legend = TRUE
    , palette = "plasma"
    , radius = 75
    , na_colour = "#808080FF"
  ) # elevation = "Time" 


write.csv(network_proprieties2, file =  "network_proprieties.csv")







########          AVERAGE PATH LENTGH , NODOS CONECTADOS Y  SU AFECATION EN REMOCIÓN ALEATORIA           ################

### Algoritmos de optimización de rutas: 
# 1) Dijkstra alogotimo  ( este es el que usaré en este caso), este no se si te tome los weigths de los links
# 2) Bellman ford algoritmo
# 3) Floyd algoritmo  (eeste estoy seguro que te toma los weigths de los links, en este caso sería tiempos de nodo a nodo )

# Cantidad de edges (links)
ecount(g_ultimo)

# Cantidad de vertices (nodos)
gorder(g_ultimo)


###### AVERAGE PATH LENGTH Y CONEXIONES TOTALES ENTRE NODOS DE LA RED, escenario sin perturvaciones   ##########
# 1) El average path length nos indicara la media de steps que da cada uno de los posibles caminos entre nodos   
average.path.length(g_ultimo) 


#conexiones totales entre nodos 
OD_nodal <- shortest.paths(g_ultimo) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)

#Nodos conectados 
#sum(Nodos_totales_conectados$Nodos_totales_conectados) / ecount(g_ultimo)




# 2) Eliominacion aleatoria del 5 %  average path legnth y nodos totales conectados 
promedio_5 = 0
promedio_5= as.data.frame(promedio_5)

for (i in 1:1000) {
random.deletes <- runif(n=93, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
promedio_5[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_5<-  head(promedio_5, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_5= t(promedio_5)   # transposición
promedio_5 <- as.data.frame(promedio_5)
mean(promedio_5$`1`)

OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra, ESTA ES EL segundo generacion de datos con la contabilización de STEPS de los caminos mas optimos
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)

#Nodos conectados 
sum(Nodos_totales_conectados$Nodos_totales_conectados) / ecount(g_ultimo)




# 3) Eliminación aleatoria de nodos (el 10% aleatorio) average path length y conexiones totales entre nodos 
promedio_10 = 0
promedio_10= as.data.frame(promedio_10)

for (i in 1:1000) {
  random.deletes <- runif(n=186, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_10[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_10<-  head(promedio_10, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_10= t(promedio_10)   # transposición
promedio_10 <- as.data.frame(promedio_10)
mean(promedio_10$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)



# 4) Eliminación aleatoria de nodos (el 15% aleatorio) average path length y conexiones totales entre nodos 
promedio_15 = 0
promedio_15= as.data.frame(promedio_15)

for (i in 1:1000) {
  random.deletes <- runif(n=280, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_15[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_15<-  head(promedio_15, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_15= t(promedio_15)   # transposición
promedio_15 <- as.data.frame(promedio_15)
mean(promedio_15$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# 5) Eliminación aleatoria de nodos (el 20% aleatorio) average path length y conexiones totales entre nodos 
promedio_20 = 0
promedio_20= as.data.frame(promedio_20)

for (i in 1:1000) {
  random.deletes <- runif(n=373, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_20[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_20<-  head(promedio_20, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_20= t(promedio_20)   # transposición
promedio_20 <- as.data.frame(promedio_20)
mean(promedio_20$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# Remoción del 25 % de nodos aleatorio 
promedio_25 = 0
promedio_25= as.data.frame(promedio_25)

for (i in 1:1000) {
  random.deletes <- runif(n=466, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_25[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_25<-  head(promedio_25, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_25= t(promedio_25)   # transposición
promedio_25 <- as.data.frame(promedio_25)
mean(promedio_25$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# 6) Eliminación aleatoria de nodos (el 30 % aleatorio) average path length y conexiones totales entre nodos 
promedio_30 = 0
promedio_30= as.data.frame(promedio_30)

for (i in 1:1000) {
  random.deletes <- runif(n=560, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_30[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_30<-  head(promedio_30, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_30= t(promedio_30)   # transposición
promedio_30 <- as.data.frame(promedio_30)
mean(promedio_30$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# 7) Eliminación aleatoria de nodos (el 35 % aleatorio) average path length y conexiones totales entre nodos 
promedio_35 = 0
promedio_35= as.data.frame(promedio_35)

for (i in 1:1000) {
  random.deletes <- runif(n=653, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_35[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_35 <-head(promedio_35, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_35= t(promedio_35)   # transposición
promedio_35 <- as.data.frame(promedio_35)
mean(promedio_35$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)





# 8) Eliminación aleatoria de nodos (el 40 % aleatorio) average path length y conexiones totales entre nodos 
promedio_40 = 0
promedio_40= as.data.frame(promedio_40)

for (i in 1:1000) {
  random.deletes <- runif(n=746, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_40[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_40 <-head(promedio_40, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_40= t(promedio_40)   # transposición
promedio_40 <- as.data.frame(promedio_40)
mean(promedio_40$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# 9) Eliminación aleatoria de nodos (el 45 % aleatorio) average path length y conexiones totales entre nodos 
promedio_45 = 0
promedio_45= as.data.frame(promedio_45)

for (i in 1:1000) {
  random.deletes <- runif(n=840, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_45[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_45 <-head(promedio_45, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_45= t(promedio_45)   # transposición
promedio_45 <- as.data.frame(promedio_45)
mean(promedio_45$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# 10) Eliminación aleatoria de nodos (el 50 % aleatorio) average path length y conexiones totales entre nodos 
promedio_50 = 0
promedio_50 = as.data.frame(promedio_50)

for (i in 1:1000) {
  random.deletes <- runif(n=933, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_50[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_50 <-head(promedio_50, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_50= t(promedio_50)   # transposición
promedio_50 <- as.data.frame(promedio_50)
mean(promedio_50$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)



# 11) Eliminación aleatoria de nodos (el 55 % aleatorio) average path length y conexiones totales entre nodos 
promedio_55 = 0
promedio_55 = as.data.frame(promedio_55)

for (i in 1:1000) {
  random.deletes <- runif(n=1026, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_55[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_55 <-head(promedio_55, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_55= t(promedio_55)   # transposición
promedio_55 <- as.data.frame(promedio_55)
mean(promedio_55$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# 12) Eliminación aleatoria de nodos (el 60 % aleatorio) average path length y conexiones totales entre nodos 
promedio_60 = 0
promedio_60 = as.data.frame(promedio_60)

for (i in 1:1000) {
  random.deletes <- runif(n=1120, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_60[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_60 <-head(promedio_60, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_60= t(promedio_60)   # transposición
promedio_60 <- as.data.frame(promedio_60)
mean(promedio_60$`1`)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)



# 13) Eliminación aleatoria de nodos (el 65 % aleatorio) average path length y conexiones totales entre nodos 
promedio_65 = 0
promedio_65 = as.data.frame(promedio_65)

for (i in 1:1000) {
  random.deletes <- runif(n=1213, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_65[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_65 <-head(promedio_65, 1) # ver bien esto para cada caso, quedarse solo con la primera fila si toda la columna repite la lectura 
promedio_65= t(promedio_65)   # transposición
promedio_65 <- as.data.frame(promedio_65)
mean(promedio_65$V1)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)




# 14) Eliminación aleatoria de nodos (el 70 % aleatorio) average path length y conexiones totales entre nodos 
promedio_70 = 0
promedio_70 = as.data.frame(promedio_70)

for (i in 1:1000) {
  random.deletes <- runif(n=1307, min=1, max=1867)  # el n son es la cantidad de numeros aleatorios, en el max pondremos la cantidad total de vertice4s (nodos)
  my.new.graph <- delete.vertices(g_ultimo, random.deletes) # Nueva grafgica con el 10% menos de nodos 
  promedio_70[i] <- average.path.length(my.new.graph) }   # aqui se acaba el loop

promedio_70= t(promedio_70)   # transposición
promedio_70 <- as.data.frame(promedio_70)
mean(promedio_70$V1)

# cantidad total de conexciones entre nodos 
OD_nodal <- shortest.paths(my.new.graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)









#############      AVERAGE PATH LENGTH Y  CONEXIONES ENTRE NODOS  EN LA REMOCION LOS NODOS ESTRATEGICOS     ##############

### CONEXIONES ENTRE NODOS 

library(sfheaders)

# Visualización de solo el DF de stops 
temp_stops


# Netowkor proprities es nuestro DF que contiene los aspectos topologicos ya sacados para todos los nodos como lo es betwness centrality y nodal degree
network_proprieties


## verificar bien, pero la columna de betweness centrality esta repetida 
network_proprieties [7] <- NULL            # CHECAR BIEN ESTO PORQUE YA AGREGE LA CENTRALIDAD 


# el DF de relations, fue creado en el proceso de la generación del objeto igraph y es la base del objeto igraph, en el se encuentra los nodos y la secuenciacion de los nodos 
red_restructurada <- relations 
red_restructurada


# Unir red estructurada con el DF que contiene la centralidades para cada nodo 
red_restructurada <- merge (red_restructurada, network_proprieties, by="stop_id") 
red_restructurada <- merge (red_restructurada , temp_stops, by="stop_id")


## Identificación y eliminación del 5 % de los nodos con mayor centralidad (betwness centrality)
red_restructurada <- head(red_restructurada[order(red_restructurada$centralidad,decreasing=FALSE),],.75*nrow(red_restructurada))  


# Se crea el nuevo objeto igraph 
new_graph <- graph_from_data_frame(red_restructurada, directed=TRUE, vertices=NULL)



# Codigo de cantidad de conexiones  entre nodos 
OD_nodal <- shortest.paths(new_graph) # esto lo hace con el algoritmo Dijkstra 
OD_nodal <-as.data.frame(OD_nodal)

Nodos_totales_conectados <-  OD_nodal
Nodos_totales_conectados[Nodos_totales_conectados > 0] <- 1 
Nodos_totales_conectados <- colSums(Nodos_totales_conectados)
Nodos_totales_conectados <- as.data.frame(Nodos_totales_conectados) # TODOS ESTAN CONECADS CON TODOS por algún camino 
sum(Nodos_totales_conectados$Nodos_totales_conectados)


# Red original TAMAÑO 
ecount(g_ultimo) # red original, links 
gorder(g_ultimo) # red original, nodos 

ecount(new_graph) # Cantidad de edges (links)
gorder(new_graph)# Cantidad de vertices (nodos)




##### AVERAGE PATH LENGTH EN LA REMOCION DE NODOS ESTRATEGICOS (CON MAYOR BETWNESS CENATRALITY)

## Eliminacion del top x % de nodos con mayor BC
# el DF de relations, fue creado en el proceso de la generación del objeto igraph y es la base del objeto igraph, en el se encuentra los nodos y la secuenciacion de los nodos 
red_restructurada <- relations 
red_restructurada

# Unir red estructurada con el DF que contiene la centralidades para cada nodo 
red_restructurada <- merge (red_restructurada, network_proprieties, by="stop_id") 
red_restructurada <- merge (red_restructurada , temp_stops, by="stop_id")

# Identificacion  y eliminacion del top 5 BC 
red_restructurada <- head(red_restructurada[order(red_restructurada$betwenes_centrality,decreasing=FALSE),],.30*nrow(red_restructurada))  

# Se crea el nuevo objeto igraph 
new_graph <- graph_from_data_frame(red_restructurada, directed=TRUE, vertices=NULL)

#Medicion del average path length
average.path.length(new_graph) 



##########             SPECTRAL GAP        ###########
library(netrankr)

# Spectrla gap 
spectral_gap(g_ultimo, method = "frac")


###############      CENTRAL POINT DOMINANCE         ############
# es la diferencia media entre el nodo con la mayor centralidad (betweness centrality) y el resto de los nodos 
central_point_dominance <-  max(network_proprieties$betwenes_centrality) -  mean(network_proprieties$betwenes_centrality)
central_point_dominance



