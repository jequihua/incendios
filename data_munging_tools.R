# load packages
library("raster")
library("rgdal")
library("date")


#####################################################
### funciones para hacer imagenes multiespectrales custom
add_harmonizedLayer <- function(ladrillo,capa)
{
  # agrega una capa a un brick, harmoniza los extents
  if (extent(capa)!=extent(ladrillo))
  {
    capa <- extend(crop(capa,ladrillo),ladrillo)
  }
  ladrillo <- addLayer(ladrillo,capa)
  return(ladrillo)
}


ladrilloCarpeta <- function(carpeta,extension="tif")
{
  # imagenes en carpeta 
  lista_nueva <- list.files(carpeta,
                            pattern=paste0("\\.",extension,"$"),full.names=TRUE,recursive=FALSE)
  
  # inicializamos una imagen multiespectral (brick)
  ladrillo <- brick()
  
  # primero le metemos la imagen base
  ladrillo <- addLayer(ladrillo,brick(lista_nueva[1]))
  
  # lo rellenamos con todo lo dem?s
  for (i in 2:length(lista_nueva))
  {
    
    # cargamos la i-?sima imagen
    imagen_aux <- brick(lista_nueva[[i]])
    
    print(imagen_aux)
    
    # la introducimos al ladrillo
    ladrillo <- add_harmonizedLayer(ladrillo,capa=imagen_aux)
  }
  
  return(ladrillo)
}



#####################################################
### funciones para manejar fechas

# funcion para modis
fecha_modis <- function(nombre_archivo,mode=1)
{
  
  if(mode==1)
  {
    fecha_aux <- strsplit(nombre_archivo,split="_")[[1]][5]
    
    anio <- substr(fecha_aux,1,4)
    
    dia_juliano <- substr(fecha_aux,5,nchar(fecha_aux))
    
    dia_juliano_a_normal <- date.mmddyyyy(as.integer(dia_juliano))
    
    dia_de_juliano <- strsplit(dia_juliano_a_normal,split="/")[[1]][1]
    
    if(nchar(dia_de_juliano)==1)
    {
      dia_de_juliano <- paste0("0",dia_de_juliano)
    }
    
    mes_de_juliano <- strsplit(dia_juliano_a_normal,split="/")[[1]][1]
    
    if(nchar(mes_de_juliano)==1)
    {
      mes_de_juliano <- paste0("0",mes_de_juliano)
    }
    
    fecha <- paste0(anio,mes_de_juliano,dia_de_juliano)
    
    return(as.integer(fecha))  
  }
  else
  {
    fecha_aux <- strsplit(nombre_archivo,split="_")[[1]][2]
    
    anio <- substr(fecha_aux,1,4)
    
    dia_juliano <- substr(fecha_aux,5,nchar(fecha_aux))
    
    dia_juliano_a_normal <- date.mmddyyyy(as.integer(dia_juliano))
    
    dia_de_juliano <- strsplit(dia_juliano_a_normal,split="/")[[1]][2]
    
    if(nchar(dia_de_juliano)==1)
    {
      dia_de_juliano <- paste0("0",dia_de_juliano)
    }
    
    mes_de_juliano <- strsplit(dia_juliano_a_normal,split="/")[[1]][1]
    
    if(nchar(mes_de_juliano)==1)
    {
      mes_de_juliano <- paste0("0",mes_de_juliano)
    }
    
    fecha <- paste0(anio,mes_de_juliano,dia_de_juliano)
    
    return(as.integer(fecha))
  }
}


# funcion puntos
fecha_puntos <- function(string_fecha,prefix="20")
{
  dia <- substr(string_fecha,1,2)
  mes <- substr(string_fecha,3,4)
  anio <- paste0(prefix,substr(string_fecha,5,6))
  
  return(as.integer(paste0(anio,mes,dia)))
}

# funcion para distancia entre fechas, regresa la fecha mas cercana
fecha_mas_cercana <- function(lista_imagenes,fecha_punto,numero_imagenes=1,mode=1)
{

  distancias_antes <- list()
  cont_antes <- 0
  nombre_antes <- list()
  idx_antes <- list()
  
  distancias_despues <- list()
  cont_despues <- 0
  nombre_despues <- list()
  idx_despues <- list()
  
  idx_lista_imagenes <- seq(1,length(lista_imagenes))
  
  for (i in 1:length(lista_imagenes))
  {
    
    fecha_modis(lista_imagenes[i],mode=mode)
    
    if ((fecha_puntos(fecha_punto)-fecha_modis(lista_imagenes[i],mode=mode))>=0)
    {
      cont_antes <- cont_antes+1
      
      distancias_antes[[cont_antes]] <- abs(fecha_puntos(fecha_punto)-fecha_modis(lista_imagenes[i],mode=mode))
      nombre_antes[[cont_antes]] <- lista_imagenes[i]
      idx_antes[[cont_antes]] <- idx_lista_imagenes[i]
    }
    else if ((fecha_puntos(fecha_punto)-fecha_modis(lista_imagenes[i],mode=mode))<0)
    {
      cont_despues <- cont_despues+1
      
      distancias_despues[[cont_despues]] <- abs(fecha_puntos(fecha_punto)-fecha_modis(lista_imagenes[i],mode=mode))
      nombre_despues[[cont_despues]] <- lista_imagenes[i]
      idx_despues[[cont_despues]] <- idx_lista_imagenes[i]
    }
  }
  
  # to data.frames plus index
  distancias_antes <- data.frame(distancias=unlist(distancias_antes),idx=unlist(idx_antes),archivo=unlist(nombre_antes))
  distancias_despues <- data.frame(distancias=unlist(distancias_despues),idx=unlist(idx_despues),archivo=unlist(nombre_despues))
  
  # sort by distance
  distancias_antes <- distancias_antes[with(distancias_antes, order(distancias)),]
  distancias_despues <- distancias_despues[with(distancias_despues, order(distancias)),]
  
  if (numero_imagenes==1)
  {
    comp_antes <- distancias_antes[2,]
  }
  else
  {
    comp_antes <- distancias_antes[2:(1+numero_imagenes),]
  }
  
  comp_correspondiente <- distancias_antes[1,]
  comp_despues <- distancias_despues[1:numero_imagenes,]
  
  # la salida es una lista en donde en la primera entrada 
  # se tiene el indice de las n imagenes pasadas mas cercanas
  # y en la segunda las n imagenes futuras mas cercanas
  salida_06mar <- list(comp_antes=comp_antes,   #agrague _06mar
                       comp_correspondiente=comp_correspondiente,
                       comp_despues=comp_despues,
                       num_compuestos=numero_imagenes)
  return(salida_06mar)  
}