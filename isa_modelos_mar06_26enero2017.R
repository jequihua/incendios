
# load packages
library("raster")
library("rgdal")
library("date")
library("randomForest")

# limpiar memoria
rm(list = ls())

# quitar los rasters temporales
removeTmpFiles(h=0)

# a donde se escriben los archivos temporales?
rasterOptions()

# asignar una carpeta donde aventar los archivos temporales para no tener problemas de espacio
rasterOptions(tmpdir="/home/jequihua/Documents/isaincendios/temp")

# cargar funciones
source("/home/jequihua/Documents/repositories/incendios/data_munging_tools.R")
source("/home/jequihua/Documents/repositories/incendios/modelling_tools.R")

#######################################################
#######################################################
# agregagacion de variables y generacion de composites

# puntos, datos de campo # se corre en dos ocaciones para caracterizar y cuando se calcular? la EQ 
puntos <- readOGR("/home/jequihua/Documents/isaincendios/ameyalli/variables/AQ06_060.shp","AQ06_060",stringsAsFactors=FALSE) # cambiar el nombre del shape y el siguiente ""



# ahora que ya se tienen la funcionalidad para encontrar las imagenes "mas cercanas" a la fecha de cada punto,
# podemos generar los composites aqui mismo asi como asociarle las otras variables

# para hacer esto conviene poner todas las capas que no se har?n composites en una carpeta aparte
carpeta_noncomposites <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/non_composites"

ladrillo_noncomposites <- ladrilloCarpeta(carpeta_noncomposites)

# lista para los composites
carpeta_composites_evi <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/composites_evi"

carpeta_composites_ndvi <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/composites_ndvi"

carpeta_composites_hum <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/humedad"

carpeta_composites_hum10h <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/humedad10h"    #PARA HUMEDAD 10 H


lista_composites_evi <- list.files(carpeta_composites_evi,pattern="\\.tif$",full.names=TRUE,recursive=FALSE)

#lista_composites_ndvi <- list.files(carpeta_composites_ndvi,pattern="\\.tif$",full.names=TRUE,recursive=FALSE) 

lista_composites_hum <- list.files(carpeta_composites_hum,pattern="\\.tif$",full.names=TRUE,recursive=FALSE) #aqu? cambie

lista_composites_hum10h <- list.files(carpeta_composites_hum10h,pattern="\\.tif$",full.names=TRUE,recursive=FALSE) #PARA HUMEDAD 10 H

ladrillo_composites_evi <- ladrilloCarpeta(carpeta_composites_evi)

#ladrillo_composites_ndvi <- ladrilloCarpeta(carpeta_composites_ndvi)

ladrillo_composites_hum <- ladrilloCarpeta(carpeta_composites_hum)

ladrillo_composites_hum10h <- ladrilloCarpeta(carpeta_composites_hum10h) #PARA HUMEDAD 10 H



################################################################################
# ahora barremos los puntos y extraemos su correspondiente valor para todos las capas incluso los compuestos


# inicializamos matriz para meter datos

salida_06mar <- data.frame(matrix(0,nrow(puntos),dim(ladrillo_noncomposites)[3]+12)) #cambia a 12 porque se eliminaron varios campos no necesarios

colnames(salida_06mar) <- c("punto_id",names(ladrillo_noncomposites),
                      "comp_evi_antes",
                      "comp_evi_correspondiente",
                      "comp_evi_despues",
                      "evi_dif_antes_despues",
                      "evi_dif_correspondiente_despues",
                      "comp_hum_correspondiente",
                      "comp_hum10h_correspondiente",
                      "Posi_fecha","id_veg","GRID_CODE","FID2")


# guardar progreso
write.table()

head(salida_06mar)

getwd()
write.table(salida_06mar,"backup_salida_403_20161109.csv",sep=",",row.names=FALSE)

i

for (i in 1:nrow(puntos))
{

  #i=207  # es util para hacer pruebas con un punto, cuando se desee correr todo es necesario documentarlo

  print(i)

  punto <- puntos[i,] 
  
  fecha_del_punto <- punto$Posi_fecha
  
  salida_06mar[i,1]<-as.numeric(punto$Crontol_is) 
  
  ################################################# non-composites
  
  salida_06mar[i,2:((dim(ladrillo_noncomposites)[3])+1)]<- extract(ladrillo_noncomposites,punto)

  #################################################
  
  #INDICES DE VEGETACION
  
  ################################################# evi
  
  # imagenes m?s cercanas (antes y despu?s)
  imagenes <- fecha_mas_cercana(lista_composites_evi,punto$Posi_fecha)
  
  # variables en objeto "imagenes"
  names(imagenes)
  
  punto$Posi_fecha

  # compuesto anterior
  idx_antes <- imagenes$comp_antes

  # compuesto correspondiente
  idx_corresp <- imagenes$comp_correspondiente

  # compuesto despues
  idx_despues <- imagenes$comp_despues

  # descartamos las bandas que no se usar?n en este punto (i-?simo) en particular
  subconjunto_antes <- subset(ladrillo_composites_evi,subset=idx_antes$idx)
  subconjunto_despues <- subset(ladrillo_composites_evi,subset=idx_despues$idx)

  
  # hacemos los compuestos (funcion media)
  compuesto_antes_media <- calc(subconjunto_antes,fun=mean)
  compuesto_correspondiente <- subset(ladrillo_composites_evi,subset=idx_corresp$idx)
  compuesto_despues_media <- calc(subconjunto_despues,fun=mean)

  # extraemos la data para el punto (i-?simo) (operaci?n espacial) y la introducimos a nuestras listas
  
  # compuesto antes 
  insert = extract(compuesto_antes_media,punto)
  if (!is.null(insert))
  {
    salida_06mar[i,"comp_evi_antes"] <- insert
  }
  
  
  # compuesto correspondiente 
  insert = extract(compuesto_correspondiente,punto)
  if(!is.null(insert))
  {
    salida_06mar[i,"comp_evi_correspondiente"] <- insert
  }
  
  
  # compuesto despues
  insert = extract(compuesto_despues_media,punto)
  if (!is.null(insert))
  {
    salida_06mar[i,"comp_evi_despues"]=insert
  }
   
  
  # resta antes - despues
  (compuesto_antes_media-compuesto_despues_media)
  (compuesto_antes_media-compuesto_despues_media)/compuesto_antes_media
  
  
  insert = extract((compuesto_antes_media-compuesto_despues_media)/compuesto_antes_media,punto)
  if (!is.null(insert))
  {
    salida_06mar[i,"evi_dif_antes_despues"] <- insert
  }
   
  tempfile() 
  
  # resta correspondiente - despues 
  insert = extract((compuesto_correspondiente-compuesto_despues_media)/compuesto_correspondiente,punto)
  if (!is.null(insert))
  {
    salida_06mar[i,"evi_dif_correspondiente_despues"] <- insert
  }
  
  
  ##################################################
  
  ################################################# ndvi
  
  # imagenes m?s cercanas (antes y despu?s)
  #imagenes <- fecha_mas_cercana(lista_composites_ndvi,punto$Posi_fecha)
  
  # variables en objeto "imagenes"
  #names(imagenes)
  
  # compuesto anterior
  #idx_antes <- imagenes$comp_antes
  
  # compuesto correspondiente
  #idx_corresp <- imagenes$comp_correspondiente
  
  # compuesto despues
  #idx_despues <- imagenes$comp_despues
  
  # descartamos las bandas que no se usar?n en este punto (i-?simo) en particular
  #subconjunto_antes <- subset(ladrillo_composites_ndvi,subset=idx_antes$idx)
  #subconjunto_despues <- subset(ladrillo_composites_ndvi,subset=idx_despues$idx)
  
  # hacemos los compuestos (funcion media)
  #compuesto_antes_media <- calc(subconjunto_antes,fun=mean)
  #compuesto_correspondiente <- subset(ladrillo_composites_ndvi,subset=idx_corresp$idx)
  #compuesto_despues_media <- calc(subconjunto_despues,fun=mean)
  
  # extraemos la data para el punto (i-?simo) (operaci?n espacial) y la introducimos a nuestras listas
  
  # compuesto antes 
  #salida_06mar[i,"comp_ndvi_antes"] <- extract(compuesto_antes_media,punto)
  
  # compuesto correspondiente 
  #salida_06mar[i,"comp_ndvi_correspondiente"] <- extract(compuesto_correspondiente,punto)
  
  # compuesto despues
  #salida_06mar[i,"comp_ndvi_despues"] <- extract(compuesto_despues_media,punto)
  
  # resta antes - despues
  #salida_06mar[i,"ndvi_dif_antes_despues"] <- extract((compuesto_antes_media-compuesto_despues_media)/compuesto_antes_media,punto)
  
  # resta correspondiente - despues 
  #salida_06mar[i,"ndvi_dif_correspondiente_despues"] <- extract((compuesto_correspondiente-compuesto_despues_media)/compuesto_correspondiente,punto)
  
  ##################################################  
  
  # HUMEDAD
  
  ################################################# HUMEDAD 100 horas lo realizo Julian
  
  # imagenes m?s cercanas (antes y despu?s)
  imagenes <- fecha_mas_cercana(lista_composites_hum,punto$Posi_fecha,mode=2)
  
  
  # variables en objeto "imagenes"
  names(imagenes)
  
  # compuesto anterior
  idx_antes <- imagenes$comp_antes
  
  # compuesto correspondiente
  idx_corresp <- imagenes$comp_correspondiente
  
  # compuesto despues
  idx_despues <- imagenes$comp_despues
  
     
  # descartamos las bandas que no se usar?n en este punto (i-?simo) en particular
  subconjunto_antes <- subset(ladrillo_composites_hum,subset=idx_antes$idx)
  subconjunto_despues <- subset(ladrillo_composites_hum,subset=idx_despues$idx)

  # hacemos los compuestos (funcion media)
  #compuesto_antes_media <- subconjunto_antes
  compuesto_correspondiente <- subset(ladrillo_composites_hum,subset=idx_corresp$idx)
  #compuesto_despues_media <- subconjunto_despues

  
  # extraemos la data para el punto (i-?simo) (operaci?n espacial) y la introducimos a nuestras listas
  
  # compuesto antes 
  #salida_06mar[i,"comp_hum_antes"] <- extract(compuesto_antes_media,punto)
  
  # compuesto correspondiente 
  
  insert = extract(compuesto_correspondiente,punto)
  if (!is.null(insert))
  {
    salida_06mar[i,"comp_hum_correspondiente"] <- insert
  }
  
   
  
  # compuesto despues
  #salida_06mar[i,"comp_hum_despues"] <- extract(compuesto_despues_media,punto)
  
    
  # resta antes - despues
  #salida[i,"hum_dif_antes_despues"] <- extract((compuesto_antes_media-compuesto_despues_media)/compuesto_antes_media,punto) #porque hay valores nulos isa
  
  # resta correspondiente - despues 
  #salida[i,"hum_dif_correspondiente_despues"] <- extract((compuesto_correspondiente-compuesto_despues_media)/compuesto_correspondiente,punto) #porque hay valores nulos isa
  
  
  ################################################## HUMEDAD 10 HORAS isa
    
  # imagenes m?s cercanas (antes y despu?s 10 HR)
  imagenes <- fecha_mas_cercana(lista_composites_hum10h,punto$Posi_fecha,mode=2)
  
  punto$Posi_fecha
  
  lista_composites_hum10h
  
  # variables en objeto "imagenes"
  names(imagenes)
  
  # compuesto anterior
  idx_antes <- imagenes$comp_antes
  
  # compuesto correspondiente
  idx_corresp <- imagenes$comp_correspondiente
  
  # compuesto despues
  idx_despues <- imagenes$comp_despues
  
  
  # descartamos las bandas que no se usar?n en este punto (i-?simo) en particular
  subconjunto_antes <- subset(ladrillo_composites_hum10h,subset=idx_antes$idx)
  subconjunto_despues <- subset(ladrillo_composites_hum10h,subset=idx_despues$idx)
  
  # hacemos los compuestos (funcion media)
  #compuesto_antes_media <- subconjunto_antes
  compuesto_correspondiente <- subset(ladrillo_composites_hum10h,subset=idx_corresp$idx)
  #compuesto_despues_media <- subconjunto_despues

  
  # extraemos la data para el punto (i-?simo) (operaci?n espacial) y la introducimos a nuestras listas
  
  # compuesto antes 
  #salida_06mar[i,"comp_hum10h_antes"] <- extract(compuesto_antes_media,punto)
  
  # compuesto correspondiente 
  insert = extract(compuesto_correspondiente,punto)
  if (!is.null(insert))
  {
    salida_06mar[i,"comp_hum10h_correspondiente"] <- insert
  }
  
  
  # compuesto despues
  #salida_06mar[i,"comp_hum10h_despues"] <- extract(compuesto_despues_media,punto)
  
 
  
  #####################################################
  punto <- as.data.frame(punto)
  
  salida_06mar <- as.data.frame(salida_06mar)
  
  # otras variables
  salida_06mar[i,c("Posi_fecha","id_veg","GRID_CODE","FID2")]<-punto[,c("Posi_fecha","id_veg","GRID_CODE","FID2")]
 

  
}


head(salida_06mar)
tail(salida_06mar)

# clean temporal rasters
#removeTmpFiles(h=0)

# hacer prediccion
###########################################################
load(file = "C:/ISA_2008/DOCTORADO/TESIS/CAPITULO3/Mejoras_enero2017/B20enero2017/modelo_todos.rf.RData")

pred_table=read.table("backup_salida_51_20170126.csv",sep=",",header=TRUE)

puntos_xy = data.frame(puntos)

pred_table$x = x=puntos_xy$coords.x1
pred_table$y = x=puntos_xy$coords.x2

pred_table = pred_table[complete.cases(pred_table),]

importance(modelo.rf)

for (k in 1:23)
{
  pred_table[,k]=as.numeric(pred_table[,k])
}

pred_table$id_veg<-factor(pred_table$id_veg)
pred_table$CS_2005<-factor(pred_table$CS_2005)
pred_table$CS_2010<-factor(pred_table$CS_2010)
pred_table$Posi_fecha<-factor(pred_table$Posi_fecha)

levels(pred_table$id_veg) <- levels(salida.posi$id_veg)
levels(pred_table$CS_2005) <- levels(salida.posi$CS_2005) # agregue isa

prediccion = predict(modelo.rf,pred_table)

# escribir a disco
salidafinal = data.frame(x=pred_table$x,y=pred_table$y,prediccion=prediccion)

coordinates(salidafinal)=~x+y

projection(salidafinal)=projection(ladrillo_composites_evi)

# write out a new shapefile (including .prj component) (to documents)

writeOGR(salidafinal, "C:/ISA_2008/DOCTORADO/TESIS/prueba/EQmar06b20B.shp", "EQmar06b20B", driver="ESRI Shapefile")
