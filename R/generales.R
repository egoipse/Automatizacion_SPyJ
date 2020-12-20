# Author: Daniel M. Giménez
# Date: 2020-10-05
# Modified: 2020-10-30
# Description: Funciones que forman parte de un paquete en proceso de desarrollo.
#   Se copian en este archivo mientras tanto para hacer operativo el script y
#   facilitar su actualización en caso de que se hagan ajustes al código.
# Packages Used: lubripack, remotes, devtools, dplyr   
# Blog Reference: No publicados
# Version: 0.1


# Generales ----

`%notin%` <- Negate(`%in%`) # La función "notin"


# Listar ----

listar <- function(lista, obj, pos = 0, rm = T,...) {
  # if (!exists(lista)) lista <- list()
  
  # Es necesario crear antes la lista
  
  if(pos %notin% c(1,0,"final","inicio")) {
    stop("La posición en la lista sólo admite cuatro valores: \"1\" o \"inicio\" para agregar el objeto al principio de la lista; \"0\" o \"final\" para agregarlo al final. Si no ingresas el argumento, el objeto se agrega al final.")
  }
  
  if(pos %in% c(0, "final")) {
    lista <- c(lista, list(obj))
    names(lista)[length(lista)] <- deparse(substitute(obj))
  } else if (pos %in% c(1, "inicio")) {
    lista <- c(list(obj),lista)
    names(lista)[1] <- deparse(substitute(obj))
  }
  
  if(isTRUE(rm)) {
    rm(list=deparse(substitute(obj)), envir = .GlobalEnv)
  }
  
  return(lista)  
  
}


# Convertir a números columnas en otros formatos ----
## Admite argumento de columnas como vector numérico

a_numeros <- function(tabla, columnas,...) {
  
  # tabla <- as.data.frame(tabla)
  
  # for(i in seq_len(length(columnas))) {
  
  #  tabla[,columnas[i]] <- sub(",",".", tabla[,columnas[i]], fixed = TRUE)
  
  # }
  
  tabla[names(tabla[,columnas])] <- sapply( tabla[names(tabla[,columnas])], as.numeric )
  
  return(tabla)
  
}


# Convertir a 0 los NA de las columnas indicadas ----
## Admite argumento de columnas como vector numérico

a_cero <- function(tabla, columnas) {
  
  tabla[, columnas] <- replace(tabla[, columnas], is.na(tabla[, columnas]), 0)
  
  return(tabla)
  
}



# Crear fila de totales sumando columnas ----

sumar_columnas <- function(tabla, columnas, decs = 0, pos = 0,...) {
  
  tabla <- as.data.frame(dplyr::ungroup(tabla))
  
  if(is.factor(tabla[,1])) {
    tabla[,1] <-  factor(tabla[,1], levels = c(levels(tabla[,1]), "Total"))
  }
  
  for(i in seq_len(length(columnas))) {
    proceso <- try({
      if(!is.numeric(tabla[,columnas[i]])) {
        tabla[,columnas[i]] <- sub(",",".", tabla[,columnas[i]], fixed = TRUE)
        tabla[,columnas[i]] <- sapply(tabla[,columnas[i]], as.numeric )
      }
    }, FALSE)
    if(isTRUE(class(proceso)=="try-error")) {
      cat("La columna ", colnames(columnas[i]), " no se puede convertir a tipo numérico y, por ello, no se puede sumar. Por favor revisa el tipo de dato que contiene.\n")
      next
    } else {  }
  }
  
  if(pos %in% c(0, "final")) {
    tabla <- rbind(tabla, c("Total", round(colSums(tabla[,columnas, drop = FALSE], na.rm = TRUE), digits = decs)))
  } else  if(pos %in% c(1, "inicio")) {
    tabla <- rbind(c("Total", round(colSums(tabla[,columnas, drop = FALSE], na.rm = TRUE), digits = decs)), tabla)
  }
  
  return(tabla)
}


# Crear columnas de totales sumando fila ----

sumar_filas <- function(tabla, columnas, decs = 0, pos = 0, con_colnames = F,...) {
  
  tabla <- as.data.frame(dplyr::ungroup(tabla))
  
  for(i in seq_len(length(columnas))) {
    proceso <- try({
      if(!is.numeric(tabla[,columnas[i]])) {
        tabla[,columnas[i]] <- sub(",",".", tabla[,columnas[i]], fixed = TRUE)
        tabla[,columnas[i]] <- sapply(tabla[,columnas[i]], as.numeric )
      }
    }, FALSE)
    if(isTRUE(class(proceso)=="try-error")) {
      cat("La columna ", colnames(columnas[i]), " no se puede convertir a tipo numérico y, por ello, no se puede sumar. Por favor revisa el tipo de dato que contiene.\n")
      next
    } else {  }
  }
  
  if(pos %in% c(0, "final")) {
    tabla <- cbind(tabla, rowSums(tabla[ ,columnas], na.rm = TRUE))
    colnames(tabla)[NCOL(tabla)] <- "Total"
  } else  if(pos %in% c(1, "inicio") & isFALSE(con_colnames)) {
    tabla <- cbind(tabla, rowSums(tabla[ ,columnas], na.rm = TRUE))
    colnames(tabla)[NCOL(tabla)] <- "Total"
    col_1 <- colnames(tabla)[1] 
    cols_otras <- colnames(tabla)[2:(NCOL(tabla)-1)]
    tabla <- tabla[, c(col_1, "Total", cols_otras)]
  } else  if(pos %in% c(1, "inicio") & isTRUE(con_colnames)) {
    tabla <- cbind(rowSums(tabla[ ,columnas], na.rm = TRUE), tabla)
    colnames(tabla)[1] <- "Total"
  }
  
  return(tabla)
}


# Convertir la primera fila en nombres de columnas ----

fila1_a_colnames <- function(tabla) {
  
  if(is.matrix(tabla)) {
    
    colnames(tabla) <- as.character(tabla[1,])
    
  } else if(is.data.frame(tabla)) {
    
    colnames(tabla) <- as.character(unlist(tabla[1,]))
    
  } else { stop("Sólo se pueden procesar matrices y dataframes. Intenta de nuevo") }
  
  tabla <- tabla[-1,]
  
  return(tabla)
  
}

unificar_archivos <- function(directorio, fuente, bbdd, output = "RDS") {
  
  if(!require(readr)) {
    
    install.packages("readr")
    
    library(readr)
    
  }
  
  if(!require(openxlsx)) {
    
    install.packages("readr")
    
    library(readr)
    
  }
  
  
  
  lista_archivos <- list.files(path = directorio)
  
  archivo_final <- data.frame()
  
  for(i in 1:length(lista_archivos)) {
    
    archivo <- read.xlsx(paste0(directorio, lista_archivos[i]))
    
    archivo_final <- rbind(archivo_final, archivo)
    
  }
  
  if(output %in% c("RDS", "rds", "RDs", "Rds")) {
    
    saveRDS(archivo_final,
            paste0(directorio, bbdd, "_", fuente, ".RDS"))
    
  } else if(output %in% c("csv", "CSV", "CSv", "Csv")) {
    
    write_csv2(archivo_final, file = paste0(directorio, bbdd, "_", fuente, ".csv"))
    
  } else {
    
    cat("No es posible generar archivos de fomato", output, ". Únicamente se pueden generar archivos de formato rds y csv")
    
  }
  
}
  

