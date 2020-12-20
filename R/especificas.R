# FUNCIONES ESPECÍFICAS DE AUTOMATIZACIÓN DE TABULADOS Y VALIDACIONES DE SP Y J

# Author: Daniel M. Giménez
# Date: 2020-10-05
# Modified: 2020-10-30
# Description: Funciones específicas para la generación automatizada de tabulados
#   de PDI y CCH
# Packages Used: tidyverse, pacman, here, openxlsx, shiny   
# Blog Reference: Disponible en github
# Version: 0.1

# 1. INGRESO DE DATOS ---- 

# 1.1 Crear diccionario y cargar datos ----

ingresar_diccionario <- function(archivo) {
  
  # si no se específica en el llamado a la función, se ingresa la ruta y el nombre del archivo del diccionario ----
  
  if(!missing(archivo)) {
    
    archivo <- archivo
    
  } else if(missing(archivo) & interactive()) {
    
    repeat {
      
      archivo <- readline(prompt = "Archivo del diccionario (incluye la ruta completa): ")
      
      if(file_ext(archivo) %notin% c("xlsx", "XLSX") ) {
        
        cat("El diccionario sólo puede estar en formato Excel y su extensión sólo puede ser .xlsx.\nIntenta de nuevo.")
        
      } else {
        break
      }
      
    }
    
  } else {
    
    stop("Necesitamos ingresar el archivo del diccionario. Y no hay forma de hacerlo asi. Intenta ingresarlo como argumento de la funcion")
    
  }
  
  # Se crea el diccionario mismo ----
  
  diccionario <- list()
  
  hojas <- getSheetNames(archivo)
  
  for(i in 1:length(hojas)) {
    
    if(hojas[i] == "meta") {
      
      meta <- read.xlsx(archivo, sheet = "meta", cols = 1:6)
      
      diccionario <- listar(diccionario, meta, rm = F)
      
    } else {
      
      obj <- read.xlsx(archivo, sheet = i)
    
      diccionario <- listar(diccionario, obj, rm = F)
      
      names(diccionario)[length(diccionario)] <- hojas[i]
    
    }
    
  }

  diccionario <<- diccionario
  
  
  # Cargamos los archivos para validar si están disponibles ----
  
  if(any(names(diccionario) == "meta")) {
    
    if(any(!is.na(diccionario$meta$cont[which(diccionario$meta$tipo == "validacion")]))) {
 
     cat("Cargando validadores...\n\n")
    
     archivos_validacion <-
        data.frame("id" = diccionario$meta$id[
          which(diccionario$meta$tipo == "validacion" &
                  !is.na(diccionario$meta$cont))],
          "archivo" = diccionario$meta$cont[
            which(diccionario$meta$tipo == "validacion" &
                    !is.na(diccionario$meta$cont))]
        )
      
      for(i in seq_len(NROW(archivos_validacion))) {
        
        hojas <- getSheetNames(archivos_validacion$archivo[i])
        
        validador <- list()
        
        if(length(hojas) > 0) {
        
          for(j in 1:length(hojas)) {
            
            obj <- read.xlsx(archivos_validacion$archivo[i], sheet = j, sep.names = " ")
            
            validador <- listar(validador, obj, rm = F)
            
            names(validador)[length(validador)] <- hojas[j]
           
          }
          
          assign(noquote(archivos_validacion$id[i]), validador, envir = .GlobalEnv)
        
        }
        
      }
        
    
    }
    
  }
    
  
  # Se crean los objetos con las bases de datos. ----
  # Toma los nombres de los archivos de la hoja "meta", de la columna "id.  
  
  if(any(names(diccionario) == "meta")) {
   
    if(any(!is.na(diccionario$meta$cont[which(diccionario$meta$tipo == "datos")]))) {
      
      fuentes <- unique(diccionario$meta$fuente[which(diccionario$meta$tipo == "datos")])
      
      
      for(i in seq_len(length(fuentes))) {
        
        archivos_fuente <- diccionario$meta %>% 
          filter(diccionario$meta$tipo == "datos" &
                   diccionario$meta$fuente ==  fuentes[i]
                 )
        
        if(any(!is.na(archivos_fuente$cont)) ) {
        
          cat("Cargando bases de datos de", str_to_upper(fuentes[i]) , "\n\n")
        
        }
          
        lista_archivos <- list()
          
        for(j in seq_len(NROW(archivos_fuente))) {
            
          if(is.na(archivos_fuente[j,"cont"])) {
            next
          } else if(file_ext(archivos_fuente[j,"cont"]) %notin% c("xlsx", "XLSX", "csv", "CSV", "RData", "rdata", "rda", "RDa", "Rda", "RDATA", "RDA", "RDS", "rds", "RDs", "Rds")) {
            stop("El archivo ingresado para ", archivos_fuente$id[j], " no tiene una de las extensiones adminitidas: xlsx, rdata, rds  o csv. Ingresa la ruta y el nombre correcto de archivo en el diccionario")
          } else if (file_ext(archivos_fuente[j,"cont"]) %in% c("csv", "CSV" ) & archivos_fuente[j,"delim"] == "") {
            stop("Ingresaste un archivo con extensión .csv para", archivos_fuente$id[j], ". Para procesarlo, es necesario que indiques el delimitador en la columna \"delim\" del diccionario.") 
          } else if (file_ext(archivos_fuente[j,"cont"]) %in% c("csv", "CSV" )) {
            el_archivo <- as.data.frame(read_delim(archivos_fuente[j,"cont"], delim = archivos_fuente[j,"delim"], locale = locale(decimal_mark = ",", grouping_mark = ".")))
          } else if (file_ext(archivos_fuente[j,"cont"]) %in% c("xlsx", "XLSX")) {
            el_archivo <-  read.xlsx(xlsxFile = archivos_fuente[j,"cont"], detectDates = T)
          } else if (file_ext(archivos_fuente[j,"cont"]) %in% c("RDS", "rds", "RDs", "Rds")) {
            el_archivo <-  readRDS(archivos_fuente[j,"cont"])
          }
          
          lista_archivos <- listar(lista_archivos, el_archivo, rm = F)  
          
          names(lista_archivos)[length(lista_archivos)] <- archivos_fuente[j, "id"]
          
        }
        
        if(length(lista_archivos) > 0) {
          
          assign(paste0("datos_", noquote(archivos_fuente$fuente[1])), lista_archivos, envir = .GlobalEnv)
        
        }  
        
        if(any(!is.na(archivos_fuente$cont)) ) {
          
          if(NROW(archivos_fuente) > length(which(!is.na(archivos_fuente$cont)))) {
            
            cat("No se han cargado todos los archivos necesarios para generar y/o validar los tabulados de",
                str_to_upper(fuentes[i]), "\b. Se creó un listado con los archivos que estaban ingresados en el diccionario, pero no se podrán generar todos los tabulados y el script se detendrá con un error apenas se requiera el(los) archivo(s) que debían cargarse y no se cargaron\n\n")
            
            Sys.sleep(3)
            
          }
          
        }
        
      }
      
      
    }
    
    
     
  }
  

  
  
}


# 2. FUNCIONES AUXILIARES ----



ordenar_dmcs_pdi <- function(tabla) {
  
  for (i in seq_len(NROW(tabla))) {
    
    if(tabla[i, "dmcs"] == "DELITOS SEXUALES") {
      tabla[i, c("DMCS", "orden")] <- c("Delitos sexuales", 1)
    } else if(tabla[i, "dmcs"] == "HOMICIDIOS") {
      tabla[i, c("DMCS", "orden")] <- c("Homicidios", 2)
    } else if(tabla[i, "dmcs"] == "LESIONES") {
      tabla[i, c("DMCS", "orden")] <- c("Lesiones", 3)
    } else if(tabla[i, "dmcs"] == "HURTOS") {
      tabla[i, c("DMCS", "orden")] <- c("Hurtos", 4)
    } else if(tabla[i, "dmcs"] == "ROBOS") {
      tabla[i, c("DMCS", "orden")] <- c("Robos", 5)
    } else if(tabla[i, "dmcs"] == "DELITOS ECONOMICOS") {
      tabla[i, c("DMCS", "orden")] <- c("Delitos económicos", 6)
    } else if(tabla[i, "dmcs"] == "DROGAS") {
      tabla[i, c("DMCS", "orden")] <- c("Drogas", 7)
    } else if(tabla[i, "dmcs"] == "VIOLENCIA INTRAFAMILIAR") {
      tabla[i, c("DMCS", "orden")] <- c("Violencia Intrafamiliar", 8)
    } else if(tabla[i, "dmcs"] == "DELITOS INFORMATICOS") {
      tabla[i, c("DMCS", "orden")] <- c("Delitos informáticos", 9)
    } else if(tabla[i, "dmcs"] == "DELITOS MEDIO AMBIENTALES") {
      tabla[i, c("DMCS", "orden")] <- c("Delitos medioambientales", 10)
    } else if(tabla[i, "dmcs"] == "PRESUNTAS DESGRACIAS") {
      tabla[i, c("DMCS", "orden")] <- c("Presuntas desgracias", 11)
    } 
    
  }
  
  tabla <- tabla[order(as.numeric(tabla$orden)),]
  
  tabla[, c("dmcs", "orden")] <- NULL
  
  columnas <- colnames(tabla[which(names(tabla) %notin% c("DMCS"))])
  
  tabla <- tabla[,c("DMCS", columnas)]
  
  return(tabla)
  
}


nombrar_col_mes <- function (tabla) {
  for (i in seq_len(NCOL(tabla))) {
    if (colnames(tabla)[i] == 1 ) {
      colnames(tabla)[i] <- "Enero"
    } else if (colnames(tabla)[i] == 2 ) {
      colnames(tabla)[i] <- "Febrero"
    } else if (colnames(tabla)[i] == 3 ) {
      colnames(tabla)[i] <- "Marzo"
    } else if (colnames(tabla)[i] == 4 ) {
      colnames(tabla)[i] <- "Abril"
    } else if (colnames(tabla)[i] == 5 ) {
      colnames(tabla)[i] <- "Mayo"
    } else if (colnames(tabla)[i] == 6 ) {
      colnames(tabla)[i] <- "Junio"
    } else if (colnames(tabla)[i] == 7 ) {
      colnames(tabla)[i] <- "Julio"
    } else if (colnames(tabla)[i] == 8 ) {
      colnames(tabla)[i] <- "Agosto"
    } else if (colnames(tabla)[i] == 9 ) {
      colnames(tabla)[i] <- "Septiembre"
    } else if (colnames(tabla)[i] == 10 ) {
      colnames(tabla)[i] <- "Octubre"
    } else if (colnames(tabla)[i] == 11 ) {
      colnames(tabla)[i] <- "Noviembre"
    } else if (colnames(tabla)[i] == 12 ) {
      colnames(tabla)[i] <- "Diciembre"
    } else { }
  }
  return(tabla)
}


nombrar_dia <- function (tabla) {
  for (i in seq_len(NCOL(tabla))) {
    if (colnames(tabla)[i] == 1 ) {
      colnames(tabla)[i] <- "Lunes"
    } else if (colnames(tabla)[i] == 2 ) {
      colnames(tabla)[i] <- "Martes"
    } else if (colnames(tabla)[i] == 3 ) {
      colnames(tabla)[i] <- "Miércoles"
    } else if (colnames(tabla)[i] == 4 ) {
      colnames(tabla)[i] <- "Jueves"
    } else if (colnames(tabla)[i] == 5 ) {
      colnames(tabla)[i] <- "Viernes"
    } else if (colnames(tabla)[i] == 6 ) {
      colnames(tabla)[i] <- "Sábado"
    } else if (colnames(tabla)[i] == 7 ) {
      colnames(tabla)[i] <- "Domingo"
    } 
  }
  return(tabla)
}


# Nombres de regiones a números. Para CCH y PJUD

asignar_regiones <- function(tabla) {
  
  tabla$region <- sapply(tabla$region, function(x) {
  
    if(str_trim(x) %in% c("REGIÓN METROPOLITANA DE SANTIAGO",
                "REGIÓN DE AYSÉN DEL GENERAL CARLOS IBÁÑEZ DEL CAMPO" )) {
    
      x <- diccionario$regiones$id[which(str_trim(x) == str_to_upper(with(diccionario$regiones,
                                                                paste(prefijo2, nombre,
                                                                       sufijo1, sep = " "))))]
        
    } else if(str_trim(x) %in% c("REGIÓN DEL LIBERTADOR GENERAL BERNARDO O'HIGGINS") ) {
      
      x <- diccionario$regiones$id[which(str_trim(x) == str_to_upper(with(diccionario$regiones,
                                                                paste0(prefijo2,
                                                                       prefijo3,
                                                                       " ",
                                                                       nombre
                                                                      ))))]
    
    } else if(str_trim(x) %in% c("REGIÓN DE MAGALLANES Y ANTÁRTICA CHILENA") ) {
      
      x <- 12
        
    } else {
      
      if(str_trim(x) %in% c("REGIÓN DEL BÍO-BÍO")) {
        
        x <- "REGIÓN DEL BIOBÍO"
        
      }
      
      x <- diccionario$regiones$id[which(str_trim(x) == str_to_upper(with(diccionario$regiones,
                                                                paste(prefijo2,
                                                                nombre, sep = " "))))]
    }
    
    }, simplify = "vector" 
  )
  
  return(tabla)
  
}



# 3. FUNCIONES DE TABULACION ----

tb_serie_anual <- function(policia, ddbb, agrupador, num_cuadro, filtro = NA, filtrar = NA) {
  
  {if(policia %notin% c("pdi", "cch")) {
    stop("El argumento \"policia\" sólo puede ser \"pdi\" (para Policía de Investigaciones) o \"cch\" (para Carabineros de Chile)\n\n
    
    La sigla siempre debe ir en minúscula y entrecomillada")
  }}
  
  {if(ddbb %notin% c("denuncias", "delitos", "detenides", "casos")) {
    stop("El argumento \"ddbb\" sólo puede ser \"denuncias\", \"delitos\", \"detenides\", \"casos\"\n\n
         
         El nombre de la base de datos siempre debe ir en minúscula y entrecomillada")
  }}
  
  {if(agrupador %notin% c("region", "dmcs")) {
    stop("El argumento \"agrupador\" sólo puede ser \"region\" o \"dmcs\"\n\n
         
         El nombre del agrupador siempre debe ir en minúscula y entrecomillada")
  }}
  
  datos <- paste0("datos_", policia)
  
  bbdd <- paste0(ddbb, "_", policia)
  
  cuadro <- get(datos)
  
  cuadro <- as.data.frame(cuadro[names(cuadro) == bbdd])
  
  colnames(cuadro) <- sub(paste0(bbdd,"."), "", colnames(cuadro))
  
  historico <- as.data.frame(diccionario[names(diccionario) == paste0("c", num_cuadro, "_", policia)])
  
  colnames(historico) <- sub(paste0("c", num_cuadro, "_", policia, "."), "", colnames(historico))
  
  if(agrupador == "region") {
    columna <- "Región"
  } else if(agrupador == "dmcs") {
    columna <- "DMCS"
  }
  
  if(!is.na(filtro)) {
    cuadro <- cuadro %>%
      filter(get(filtro) %in% filtrar)
  }  
  
  cuadro <- cuadro %>% 
    group_by(get(agrupador)) %>% 
    summarise(dels=n()) %>% 
    ungroup() %>% 
    as.data.frame()
  
  colnames(cuadro) <- c(agrupador,
                        diccionario$meta[which(diccionario$meta$id == "agno"), "cont"])
  
  
  if(agrupador == "region") {  
    cuadro <- cuadro %>% 
      ordenar_regiones() %>%  
      nombrar_regiones() %>%
      sumar_columnas(columnas = 2, pos = 1) %>% 
      as.data.frame()
  }
  
  cuadro <- cbind(historico, cuadro)
  
  cuadro[,NCOL(cuadro) - 1] <- NULL
  
  cuadro <- a_numeros(cuadro, columnas = 2:NCOL(cuadro))
  
  return(cuadro)
  
}


tb_delitos <- function(policia, ddbb, agrupador, filtro = NA, filtrar = NA) {
  
  {if(policia %notin% c("pdi", "cch")) {
    stop("El argumento \"policia\" sólo puede ser \"pdi\" (para Policía de Investigaciones) o \"cch\" (para Carabineros de Chile)\n\n
    
    La sigla siempre debe ir en minúscula y entrecomillada")
  }}
  
  {if(ddbb %notin% c("denuncias", "delitos", "detenides", "casos")) {
    stop("El argumento \"ddbb\" sólo puede ser \"denuncias\", \"delitos\", \"detenides\", \"casos\"\n\n
         
         El nombre de la base de datos siempre debe ir en minúscula y entrecomillada")
  }}
  
  {if(agrupador %notin% c("region", "dmcs")) {
    stop("El argumento \"agrupador\" sólo puede ser \"region\" o \"dmcs\"\n\n
         
         El nombre del agrupador siempre debe ir en minúscula y entrecomillada")
  }}
  
  datos <- paste0("datos_", policia)
  
  bbdd <- paste0(ddbb, "_", policia)
  
  cuadro <- get(datos)
  
  cuadro <- as.data.frame(cuadro[names(cuadro) == bbdd])
  
  colnames(cuadro) <- sub(paste0(bbdd,"."), "", colnames(cuadro))
  
  if(!is.na(filtro)) {
    cuadro <- cuadro %>%
      filter(get(filtro) %in% filtrar)
  }  
  
  cuadro <- cuadro %>% 
    group_by(delito, get(agrupador))  %>% 
    summarise(dels=n()) %>% 
    pivot_wider(names_from = `get(agrupador)`, values_from = dels ) %>% 
    ungroup() %>% 
    as.data.frame()
  
  cuadro$familia <- sapply(cuadro$delito, function(x) {
    if(x %in% (diccionario$delitos$id)) {
      x <- diccionario$delitos$familia[which(x == diccionario$delitos$id)]
    } 
  }, simplify = "vector" )
  
  totales <- c("", "TOTAL", colSums(cuadro[,3:(NCOL(cuadro)-1)], na.rm = T), "")
  
  cuadro <- split(cuadro, as.factor(cuadro$familia))
  
  for(i in seq_len(length(cuadro))) {
    cuadro[[i]] <- rbind(c("",str_to_upper(names(cuadro)[[i]]),colSums(cuadro[[i]][,3:(NCOL(cuadro[[i]])-1)], na.rm = T), "" ), cuadro[[i]])
  }
  
  # cuadro <- cuadro[names(orden_familias)]
  
  cuadro <- do.call("rbind", cuadro)
  
  cuadro$familia <- NULL
  
  cuadro[names(cuadro[3:NCOL(cuadro)])] <- sapply( cuadro[names(cuadro[3:NCOL(cuadro)])], as.numeric )
  
  cuadro[,1] <- as.numeric(cuadro[,1])
  
}




# Changelog
#
# Versión 0.1
# Se agregan las siguientes funciones:
# - ingresar_diccionario
# - ordenar_regiones  
# - nombrar_regiones
# - ordenar_dmcs_pdi
# - nombrar_col_mes
# - nombrar_dia
# - tb_serie_anual
# - nombrar_dia
# - tb_serie_anual
# - tb_delitos