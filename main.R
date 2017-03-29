
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

# para hacer esto conviene poner todas las capas que no se haran composites en una carpeta aparte
carpeta_noncomposites <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/non_composites"

ladrillo_noncomposites <- ladrilloCarpeta(carpeta_noncomposites)

# lista para los composites
carpeta_composites_evi <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/composites_evi"

carpeta_composites_ndvi <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/composites_ndvi"

carpeta_composites_hum <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/humedad"

carpeta_composites_hum10h <- "/home/jequihua/Documents/isaincendios/ameyalli/variables/humedad10h"    #PARA HUMEDAD 10 H


lista_composites_evi <- list.files(carpeta_composites_evi,pattern="\\.tif$",full.names=TRUE,recursive=FALSE)

lista_composites_ndvi <- list.files(carpeta_composites_ndvi,pattern="\\.tif$",full.names=TRUE,recursive=FALSE) 

lista_composites_hum <- list.files(carpeta_composites_hum,pattern="\\.tif$",full.names=TRUE,recursive=FALSE) #aqu? cambie

lista_composites_hum10h <- list.files(carpeta_composites_hum10h,pattern="\\.tif$",full.names=TRUE,recursive=FALSE) #PARA HUMEDAD 10 H

ladrillo_composites_evi <- ladrilloCarpeta(carpeta_composites_evi)

ladrillo_composites_ndvi <- ladrilloCarpeta(carpeta_composites_ndvi)

ladrillo_composites_hum <- ladrilloCarpeta(carpeta_composites_hum)

ladrillo_composites_hum10h <- ladrilloCarpeta(carpeta_composites_hum10h) #PARA HUMEDAD 10 H



################################################################################
# ahora barremos los puntos y extraemos su correspondiente valor para todos las capas incluso los compuestos


# que variables se van a procesar?
variables_a_procesar = c("punto_id",names(ladrillo_noncomposites),
                         "comp_evi_antes",
                         "comp_evi_correspondiente",
                         "comp_evi_despues",
                         "comp_ndvi_antes",
                         "comp_ndvi_correspondiente",
                         "comp_ndvi_despues",
                         "evi_dif_antes_despues",
                         "evi_dif_correspondiente_despues",
                         "ndvi_dif_antes_despues",
                         "ndvi_dif_correspondiente_despues",
                         "comp_hum_correspondiente",
                         "comp_hum10h_correspondiente",
                         "Posi_fecha","id_veg","GRID_CODE","FID2")

# que variables son:
variables_a_procesar

# inicializamos matriz para meter datos
salida_06mar <- data.frame(matrix(0,nrow(puntos),length(variables_a_procesar))) #cambia a 12 porque se eliminaron varios campos no necesarios
colnames(salida_06mar) <- variables_a_procesar

# poblar tabla
for (i in 1:nrow(puntos))
{

  #i=207  # es util para hacer pruebas con un punto, cuando se desee correr todo es necesario documentarlo

  print(i)

  punto <- puntos[i,] 
  
  fecha_del_punto <- punto$Posi_fecha
  
  salida_06mar[i,1]<-as.numeric(punto$Crontol_is) 
  
  ################################################# 
  
  # procesar NO-COMPOSITES
  
  salida_06mar[i,2:((dim(ladrillo_noncomposites)[3])+1)]<- extract(ladrillo_noncomposites,punto)

  #################################################

  # procesar COMPOSITES
  
  ################################################# evi
  
  extraccion = extract_fecha_mas_cercana(ladrillo_composites=ladrillo_composites_evi,
                                         lista_composites=lista_composites_evi,
                                         punto=punto,
                                         missing_value=-999,
                                         mode=4)
  
  salida_06mar[i,"comp_evi_antes"] <- extraccion$extraccion_antes
  salida_06mar[i,"comp_evi_correspondiente"] <- extraccion$extraccion_correspondiente
  salida_06mar[i,"comp_evi_despues"] <- extraccion$extraccion_despues
  salida_06mar[i,"evi_dif_antes_despues"] <- resta_estandarizada(extraccion$extraccion_antes,
                                                                 extraccion$extraccion_despues) 
  salida_06mar[i,"evi_dif_correspondiente_despues"] <- resta_estandarizada(extraccion$extraccion_correspondiente,
                                                                 extraccion$extraccion_despues)
  
  ################################################# ndvi
  
  extraccion = extract_fecha_mas_cercana(ladrillo_composites=ladrillo_composites_ndvi,
                                         lista_composites=lista_composites_ndvi,
                                         punto=punto,
                                         missing_value=-999,
                                         mode=4)
  
  salida_06mar[i,"comp_ndvi_correspondiente"] <- extraccion$extraccion_correspondiente
  salida_06mar[i,"comp_ndvi_despues"] <- extraccion$extraccion_despues
  salida_06mar[i,"ndvi_dif_antes_despues"] <- resta_estandarizada(extraccion$extraccion_antes,
                                                                 extraccion$extraccion_despues) 
  salida_06mar[i,"ndvi_dif_correspondiente_despues"] <- resta_estandarizada(extraccion$extraccion_correspondiente,
                                                                           extraccion$extraccion_despues)
  
  ################################################# hum 100 horas
  
  extraccion = extract_fecha_mas_cercana(ladrillo_composites=ladrillo_composites_hum,
                                         lista_composites=lista_composites_hum,
                                         punto=punto,
                                         missing_value=-999,
                                         mode=2)
  
  salida_06mar[i,"comp_hum_correspondiente"] <- extraccion$extraccion_correspondiente
  
  ################################################# hum 10 horas
  
  extraccion = extract_fecha_mas_cercana(ladrillo_composites=ladrillo_composites_hum10h,
                                         lista_composites=lista_composites_hum10h,
                                         punto=punto,
                                         missing_value=-999,
                                         mode=2)
  
  salida_06mar[i,"comp_hum10h_correspondiente"] <- extraccion$extraccion_correspondiente

}
# fin de llenado de tabla





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
