# FUNCIONES QUE REQUIEREN LA CARGA PREVIA DEL DICCIONARIO

# Author: Daniel M. Giménez
# Date: 2020-10-05
# Modified: 2020-10-30
# Description: Funciones específicas para la generación automatizada de tabulados
#   de PDI y CCH que requieren que se cargue primero el diccionario. Estas funciones
#   piden el diccionario. Por lo tanto, este archivo es llamado y cargado una vez que
#   que se carga el diccionario.
# Packages Used: tidyverse, openxlsx 
# Blog Reference: Disponible en github
# Version: 0.1

# Funciones para PDI, CCH y PJUD ----

ordenar_regiones <- function(tabla) {
  
  tabla$orden <- sapply(tabla$region, function(x) {
    x <- diccionario$regiones$orden[which(x == diccionario$regiones$id)]
  }, simplify = "vector" 
  )
  
  tabla <- tabla[order(tabla$orden),]
  
  tabla$orden <- NULL
  
  return(tabla)
  
}


nombrar_regiones <- function(tabla) {
  
  tabla$`Región` <- sapply(tabla$region, function(x) {
    x <- diccionario$regiones$nombre[which(x == diccionario$regiones$id)]
    
  }, simplify = "vector" 
  )
  
  tabla$region <- NULL
  
  columnas <- colnames(tabla[which(names(tabla) != "Región")])
  
  tabla <- tabla[ , c("Región", columnas)]
  
  return(tabla)
  
}

agregar_familias <- function(tabla) {
  
  tabla$familia <- sapply(tabla$delito, function(x) {
    if(x %in% (diccionario$delitos$id)) {
      x <- diccionario$delitos$familia[which(x == diccionario$delitos$id)]
    } 
  }, simplify = "vector" )
  
  return(tabla)
  
}


orden_familias_delitos <- as.list(diccionario$familias_delitos[,"cont"])

for(i in 1:length(orden_familias_delitos)) {
  names(orden_familias_delitos)[[i]] <-(orden_familias_delitos)[[i]][1]
  # orden_familias[[i]] <- c()
}


organizar_delitos <- function(tabla) {
  
  # Esta función crea el tabulado de los delitos. Se requiere un df que :
  # 1. Tenga la lista de códigos de delitos en la primera columna;
  # 2. Las categorías de la variable por la que se cruza distribuidos en el resto
  # de las columnas;
  # 3. Que la primera columna, la que contiene los códigos de los delitos, se llame
  # "delito". La función arrojará error si se llama de cualquier otra forma.
  # 4. Un diccionario con un df llamado "delitos" que contenga las columnas "familia"
  # y "materia" con los nombres de las familias y las materias, respectivamente.
  
  tabla$familia <- sapply(tabla$delito, function(x) {
    if(str_trim(x) %in% (diccionario$delitos$id)) {
      x <- diccionario$delitos$familia[which(str_trim(x) == diccionario$delitos$id)]
    } 
  }, simplify = "vector" )
  
  tabla$materia <- sapply(tabla$delito, function(x) {
    if(str_trim(x) %in% (diccionario$delitos$id)) {
      x <- diccionario$delitos$materia[which(str_trim(x) == diccionario$delitos$id)]
    } 
  }, simplify = "vector" )
  
  columnas <- colnames(tabla[which(names(tabla) %notin% c("delito", "materia"))])
  
  tabla <- tabla[ , c("delito", "materia", columnas)]
  
  totales <- c("", "Total", colSums(tabla[,3:(NCOL(tabla)-1)], na.rm = T))
  
  tabla <- split(tabla, as.factor(tabla$familia))
  
  for(i in seq_len(length(tabla))) {
    
    tabla[[i]] <- as.data.frame(tabla[[i]])
    
    tabla[[i]] <- tabla[[i]][order(as.numeric(str_trim(tabla[[i]][,1]))), ]
    
    tabla[[i]] <- rbind(c("",str_to_upper(names(tabla)[[i]]),colSums(tabla[[i]][,3:(NCOL(tabla[[i]])-1)], na.rm = T), ""), tabla[[i]])
    
  }
  
  tabla <- tabla[names(orden_familias_delitos)]
  
  tabla <- do.call("rbind", tabla)
  
  rownames(tabla) <- c()
  
  tabla <- rbind(totales,tabla)
  
  tabla$familia <- NULL
  
  tabla <- a_numeros(tabla, 3:NCOL(tabla))
  
  tabla <- sumar_filas(tabla = tabla, columnas = 3:NCOL(tabla), pos = 1)
  
  columnas <- colnames(tabla[which(names(tabla) %notin% c("delito", "Total", "materia"))])
  
  tabla <- tabla[ , c("delito", "materia", "Total", columnas)]
  
  colnames(tabla)[which(colnames(tabla) %in% c("delito", "materia"))] <- c("Código", "Familia y materias")
  
  tabla <- a_numeros(tabla, c(1, 3:NCOL(tabla))) 
  
  tabla[, 3:NCOL(tabla)] <- replace(tabla[, 3:NCOL(tabla)], is.na(tabla[, 3:NCOL(tabla)]), 0)
  
  return(tabla)
  
}



# Funciones para CCH ----
  
orden_regiones <- diccionario$regiones %>% 
  arrange(orden) %>% 
  select(nombre) %>% 
  as.list()

orden_regiones <- as.list(orden_regiones[[1]])

for(i in 1:length(orden_regiones)) {
  names(orden_regiones)[[i]] <- str_to_upper((orden_regiones)[[i]][1])
  orden_regiones[[i]][1] <- names(orden_regiones)[[i]] 
}

 
orden_dmcs_cch <- list(Robos = c(),
                       Homicidios = c(),
                       Violaciones = c(),
                       Lesiones = c(),
                       Hurtos = c()
)

organizar_provincias <- function(tabla) {
  
  # Limpiamos el desastre de la bbdd
  
  tabla[,1] <- sub("PROVINCIA DE LA ", "", tabla[,1])
  
  tabla[,1] <- sub("PROVINCIA DE ", "", tabla[,1])
  
  tabla[,1] <- sub("PROVINCIA DEL ", "", tabla[,1])
  
  tabla[,1] <- sub("PROVINCIA. ", "", tabla[,1])  
  
  tabla[,1] <- sub("PROVINCA DE ", "", tabla[,1]) 
  
  tabla[,1] <- sub("PROVINCIA ", "", tabla[,1]) 
  
  
  # Asignamos regiones
  
  tabla$region <- sapply(tabla$provincia, function(x) {
    if( str_trim(x) %in% c(str_to_upper(diccionario$provincia$nombre))) {
      x <- diccionario$provincia$region[which( str_trim(x) == str_to_upper(diccionario$provincia$nombre))]
    } 
  }, simplify = "vector" )
  
  tabla[,"provincia"] <- str_to_title(tabla[,"provincia"])
  
  tabla <- nombrar_regiones(tabla)
  
  colnames(tabla)[which(colnames(tabla) == "Región")] <- "region"
  
  tabla[,"region"] <- str_to_upper(tabla[,"region"])
  
  totales <- c("", "Total", colSums(tabla[,3:(NCOL(tabla)-1)], na.rm = T))
  
  tabla <- split(tabla, as.factor(tabla$region))
  
  for(i in seq_len(length(tabla))) {
    
    tabla[[i]] <- tabla[[i]][order(tabla[[i]][,2]), ] 
    
    tabla[[i]] <- rbind(c("", str_to_upper(names(tabla)[[i]]),colSums(tabla[[i]][,3:(NCOL(tabla[[i]]))], na.rm = T), ""), tabla[[i]])
    
  }
  
  tabla <- tabla[names(orden_regiones)]
  
  tabla <- do.call("rbind", tabla)
  
  rownames(tabla) <- c()
  
  tabla <- rbind(totales,tabla)
  
  tabla$region <- NULL
  
  colnames(tabla)[1] <- "Región y provincia"
  
  return(tabla)
  
}


pivotar_casos_cch <- function(df, por, segun) {
  
  if(segun == "familia") {
    
    df <- agregar_familias(df)
    
  }
  
  
  if(segun == "familia") {
    
    las_cols <- unlist(orden_familias_delitos)
    
  } else {
    
    las_cols <- unique(df[, segun])
    
  }
  
  tabla <- as.data.frame(ungroup(df)) %>% 
    group_by(get(por)) %>% 
    summarise(`TOTAL: Total casos` = sum(casos),
              `TOTAL: Casos sin detenciones` = sum(denuncias),
              `TOTAL: Casos con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(tabla)[1] <- noquote(por)
  
  
  for(i in 1:length(las_cols)) {
    
    nuevo <- df %>% 
      filter(get(segun) == las_cols[i]) %>% 
      group_by(get(por)) %>% 
      summarise(`Total casos` = sum(casos),
                `Casos sin detenciones` = sum(denuncias),
                `Casos con detenciones`= sum(detenciones)
      ) %>% 
      as.data.frame()
    
    colnames(nuevo)[1:4] <- c(noquote(por),
                              paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                              paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                              paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
    )
    
    tabla <- left_join(tabla, nuevo)
    
  }
  
  colnames(tabla) <- sub("constitución", "Constitución", colnames(tabla))
  colnames(tabla) <- sub("gendarmería", "Gendarmería", colnames(tabla))
  colnames(tabla) <- sub("investigaciones", "Investigaciones", colnames(tabla))
  
  return(tabla)
  
}


# Funciones para PJUD ----

ordenar_cortes <- function(tabla) {
  
  tabla$orden <- sapply(tabla$codigo_ca, function(x) {
    x <- diccionario$cortes_apelaciones$orden[which(x == diccionario$cortes_apelaciones$id)]
   }, simplify = "vector" 
  )
  
  tabla$ca <- sapply(tabla$codigo_ca, function(x) {
    x <- diccionario$cortes_apelaciones$cont[which(x == diccionario$cortes_apelaciones$id)]
    }, simplify = "vector" 
  )
  
  tabla <- tabla[order(tabla$orden),] %>% 
    ungroup() %>% 
    select(!c(codigo_ca, orden))
  
  columnas <- colnames(tabla[which(names(tabla) %notin% c("ca"))])
  
  tabla <- tabla[ , c("ca", columnas)]
  
  return(tabla)
  
}

organizar_ca_x_materia <- function(df, cols_tot, ...) {
  
  
  if(length(cols_tot) == 1) {
  
    totales <- as.character(c("", "Total",  sum(df[,cols_tot], na.rm = T)))
    
  } else if(length(cols_tot) > 1) {
    
    totales <- as.character(c("", "Total",  colSums(df[,cols_tot], na.rm = T)))
    
  }
  
  df <- split(df, as.factor(df$ca))
  
  for(i in seq_len(length(df))) {
    
    df[[i]] <- as.data.frame(df[[i]])
    
    df[[i]] <- as.data.frame(df[[i]][order(as.numeric(df[[i]][,2])), ]) 
    
    if(length(cols_tot) == 1) {
    
      df[[i]] <- rbind(c("","",names(df)[[i]],sum(df[[i]][,cols_tot], na.rm = T)), df[[i]])
    
    } else if(length(cols_tot) > 1) {
      
      df[[i]] <- rbind(c("","",names(df)[[i]],colSums(df[[i]][,cols_tot], na.rm = T)), df[[i]])
        
    }
      
  }
  
  
  orden <- diccionario$cortes_apelaciones[, c("cont", "orden")] %>%  
    arrange(orden) %>% 
    select(!orden)
  
  orden <- as.list(orden$cont)
  
  for(i in 1:length(orden)) {
    names(orden)[[i]] <- orden[[i]][1]
    orden[[i]][1] <- names(orden)[[i]] 
  }
    
  
  df <- df[names(orden)] %>% 
    do.call("rbind", .)
  
  rownames(df) <- c()
  
  df$ca <- NULL
  
  df <- rbind(totales,df)
  
  
  
}
  


encuadrar <- function(cuadro, fuente) {
  
  # Con esta función se genera cada cuadro en una hoja de excel. La fuente debe
  # ir entrecomillada
  
  # Ajustamos el número de cuadro
  
  if(!exists(paste0("validador_", fuente))) {
  
    num_cuadro <<- num_cuadro + 1
  
  }
  
  # Primero definimos títulos, footer, notas
  
  titulos <- diccionario[names(diccionario) == paste0("titulos_", fuente)] %>% 
    .[[1]] %>% 
    filter(cuadro == num_cuadro) %>% 
    select(-cuadro)
  
  titulos <- as.vector(titulos$titulo)
  
  # titulos <- diccionario$titulos_pdi[which(diccionario$titulos_pdi$cuadro == num_cuadro), 2]
  
  footer <- diccionario$meta[which(diccionario$meta$tipo == "info_fuente" & diccionario$meta$fuente == fuente), "cont"]
  
  # footer <- diccionario$meta[which(diccionario$meta$id == "fuente_pdi"), "cont"]
  
  las_notas <-  diccionario[names(diccionario) == paste0("notas_", fuente)] %>% 
    .[[1]] 
  
  
  
  # Cargamos las notas, si las hay para el cuadro
  
  if(any(las_notas$cuadro == num_cuadro)) {
    
    notas <- las_notas[which(las_notas$cuadro == num_cuadro), 1:6]
    
    notas_a_footer <- c()
    
    for(i in 1:NROW(notas)) {
      valor.nuevo <- paste(notas$numero[i], notas$texto[i], sep = " " )
      notas_a_footer <- rbind(notas_a_footer, valor.nuevo)
    }
    
  }
  
  # Definimos posiciones de elementos del cuadro
  
  fin_cabecera <- NROW(titulos)
  fin_cuadro <- NROW(cuadro) + fin_cabecera + 2
  
  
  # Conformamos el footer
  
  if(any(cuadro[1:NROW(cuadro), 1:NCOL(cuadro)] == 0) & exists("notas") ) {
        
      footer <- rbind(para_ceros, notas_a_footer, footer)
        
    } else if(any(cuadro[1:NROW(cuadro), 1:NCOL(cuadro)] == 0) & !exists("notas") ) {
        
      footer <- rbind(para_ceros, footer)
        
    } else if(all(cuadro[1:NROW(cuadro), 1:NCOL(cuadro)] != 0) & exists("notas")) {
        
        footer <- rbind(notas_a_footer, footer)
      
  } 
  
  
  # Agregar notas al cuadro
  
  if(exists("notas")) {
    
    for(i in 1:NROW(notas)) {
      
      if(notas$fila[i] <= fin_cabecera ) {
        
        con_nota <- unlist(str_split(titulos[notas$fila[i]], " "))
        
        con_nota[which(con_nota == notas[i,"ancla"])] <- paste0(con_nota[which(con_nota == notas[i,"ancla"])], " (", notas$numero[i], ")")
        
        con_nota <- str_c(con_nota, collapse = " ")
        
        titulos[notas$fila[i]] <- con_nota
        
        rm(con_nota)
        
        # titulos[notas$fila[i]] <- paste0(titulos[notas$fila[i]], " (", notas$numero[i], ")")  
        
      } else if (notas$fila[i] == (fin_cabecera + 2)) {
        
        con_nota <- unlist(str_split(colnames(cuadro)[notas$columna[i]], " "))
        
        con_nota[which(con_nota == notas[i,"ancla"])] <- paste0(con_nota[which(con_nota == notas[i,"ancla"])], " (", notas$numero[i], ")")
        
        con_nota <- str_c(con_nota, collapse = " ")
        
        colnames(cuadro)[notas$columna[i]] <- con_nota
        
        rm(con_nota)
        
        # colnames(cuadro)[notas$columna[i]] <- paste0(colnames(cuadro)[notas$columna[i]], " (", notas$numero[i], ")")
        
      } else {
        
        if(fin_cabecera == 3 ) {
          
          fila <- notas$fila[i] - 5
          
        } else {
        
          fila <- notas$fila[i] - 6
          
        }
        
        con_nota <- unlist(str_split(cuadro[fila, notas$columna[i]], " "))
        
        con_nota[which(con_nota == notas[i,"ancla"])] <- paste0(con_nota[which(con_nota == notas[i,"ancla"])], " (", notas$numero[i], ")")
        
        con_nota <- str_c(con_nota, collapse = " ")
        
        cuadro[fila, notas$columna[i]] <- con_nota
        
        rm(con_nota)
        
        # cuadro[fila, notas$columna[i]] <- paste0(cuadro[fila, notas$columna[i]], " (", notas$numero[i], ")")
        
      }
      
    }
    
  }
 
  
  # Creamos la hoja de cálculo
  
  addWorksheet(get(paste0("cuadros_", fuente)), sheetName = num_cuadro, gridLines = F) 
  
  writeData(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
            titulos, colNames = F)
  
  writeData(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
            cuadro,  startRow = fin_cabecera + 2)
  
  writeData(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
            footer,  startRow = fin_cuadro + 1, colNames = F) 
  
  
  # FORMATOS
  
    # Toda la tabla
  
  addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
           formatos$general, rows = (fin_cabecera +1):(fin_cuadro+NROW(footer)),
           cols = 1:NCOL(cuadro), gridExpand = T)
  
  
    # Los títulos del cuadro
  
  addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
           formatos$titulos, rows = 1:fin_cabecera, cols = 1, gridExpand = T, stack = T)
  
    # El encabezado de la tabla
  
  addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
           formatos$cabecera, rows = fin_cabecera + 2, cols = 1:NCOL(cuadro), stack = T)
  
    # La línea inferior
  
  addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
           formatos$abajo, rows = fin_cuadro, cols = 1:NCOL(cuadro), stack = T)
  
    # Fila del Total
  
  if(any(unlist(str_split(cuadro[1,], " ")) == "Total")) {
  
  # if(any(cuadro[1,] == "Total")) {
    
    addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
             formatos$negrear, rows = fin_cabecera + 3, cols = 1:NCOL(cuadro), stack = T)
    
  }
  
  
    # Columnas del Total
  
  columnas_totales <- grep("Total", colnames(cuadro))
  
  columnas_totales <- columnas_totales[columnas_totales %notin% sapply(c(
    "divorciadas", "anuladas", "conviviente civil", "solteras"
    ), function(x) {
         grep(x, colnames(cuadro)) }, USE.NAMES = F)]

  
     addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
             formatos$negrear, rows = fin_cabecera + 2:fin_cuadro, cols = columnas_totales,
             gridExpand = T, stack = T)
     
     
     
     # Filas especiales 1: Familias de delitos en negritas
     
     for(i in seq_len(NROW(cuadro))) {
       
       if(cuadro[i, 2] %in% names(orden_familias_delitos)) {
         
         addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                  formatos$negrear, rows = i + fin_cabecera + 2, cols = 2:NCOL(cuadro),
                  stack = T)
         
       }
       
     }
     
     
     # Filas especiales 2: todas las otras filas con mayúsculas en negritas
     
     for(i in seq_len(NROW(cuadro))) {
       
       if(cuadro[i, 1] %in% c(
         
         "TOTAL HOMBRES", "TOTAL MUJERES", "NACIONALIDAD", "SEXO", "EDAD", "ESTADO CIVIL", "ACTIVIDAD", "NIVEL DE EDUCACIÓN"
         
       )) {
         
         addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                  formatos$negrear, rows = i + 3, cols = 1:NCOL(cuadro),
                  stack = T)
         
       }
       
     }
    
    
    # Los números
    
    addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
             formatos$numeros, rows = (fin_cabecera + 3):fin_cuadro, cols = 2:NCOL(cuadro),
             stack = T, gridExpand = T)
    
    
    # La fila de "Variación porcentual"
    
    if(isTRUE(length(unlist(str_split(cuadro[2, 1], " "))) > 1) &
       isTRUE(str_c(unlist(str_split(cuadro[2, 1], " "))[1:2], collapse = " ")
              == "Variación porcentual")) {
      
      addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
               formatos$var_anual, rows = fin_cabecera + 4, cols = 1:NCOL(cuadro), stack = T)
      
    }
    
    # ANCHOS DE COLUMNAS
    
    
    # Primeras columnas
    
    if(colnames(cuadro)[1] == "Código") {
      
      addStyle(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),               formatos$centrar, rows = (fin_cabecera + 3):fin_cuadro, cols = 1,

               stack = T, gridExpand = T) 
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = c(1:3), widths =c(8, 88, 10))
      
    } else if (colnames(cuadro)[1] == "Región" ) {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = c(1:NCOL(cuadro)), widths =c(30, rep(10, NCOL(cuadro)-1)))
      
    } else if (colnames(cuadro)[1] == "DMCS" &
               colnames(cuadro)[2] == "Total" ) {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = c(1:2), widths =c(24, 10))
      
    } else if (colnames(cuadro)[1] == "DMCS" ) {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = c(1:NCOL(cuadro)), widths =c(24, rep(10, NCOL(cuadro)-1)))
      
    } else if (colnames(cuadro)[1] == "Variable" ) {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = c(1:2), widths =c(25, 10))
      
    } else if (colnames(cuadro)[1] == "Familias" ) {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = c(1:2), widths =c(66, 10))
      
    }
    
    
    # Siguientes columnas
    
    # No es necesario formatear años
    
    if(tail(colnames(cuadro), 1) == "Magallanes y de la Antártica Chilena") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-15):NCOL(cuadro)), widths = rep(14, 16))
      
    } else if(tail(colnames(cuadro), 1) == "Diciembre") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-11):NCOL(cuadro)), widths = rep(12, 12))
      
    } else if(tail(colnames(cuadro), 2) == "Domingo") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-7):NCOL(cuadro)), widths = rep(11, 8))
      
    } else if(tail(colnames(cuadro), 1) == "Domingo") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-6):NCOL(cuadro)), widths = rep(11, 7))
      
    } else if(tail(colnames(cuadro), 1) == "51 o más") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-8):NCOL(cuadro)), widths = rep(12.8, 9))
      
    } else if(tail(colnames(cuadro), 2) == "8:01 a 12:00" |
              tail(colnames(cuadro), 2) == "20:01 a 24:00") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-6):NCOL(cuadro)), widths = rep(13.3, 7))
      
    } else if(tail(colnames(cuadro), 1) == "20:01 a 24:00") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-5):NCOL(cuadro)), widths = rep(13.3, 6))
      
    } else if(any(colnames(cuadro) == "Boliviana")) {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-10):NCOL(cuadro)), widths = rep(12.3, 11))
      
    }  else if(tail(colnames(cuadro), 2) == "Divorciadas" |
               tail(colnames(cuadro), 2) == "Jubilados" |
               tail(colnames(cuadro), 2) == "Técnico") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-10):NCOL(cuadro)), widths = rep(13, 11))
      
    }   else if(tail(colnames(cuadro), 1) == "Divorciadas" |
                tail(colnames(cuadro), 1) == "Jubilados" |
                tail(colnames(cuadro), 1) == "Técnico") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-9):NCOL(cuadro)), widths = rep(13, 10))
      
    } else if(tail(colnames(cuadro), 2) == "Posgrado") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-6):NCOL(cuadro)), widths = rep(12.5, 7))
      
    } else if(tail(colnames(cuadro), 1) == "Posgrado") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-5):NCOL(cuadro)), widths = rep(12.5, 6))
      
    } else if(tail(colnames(cuadro), 1) == "Nacionalidad: Extranjera") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-12):NCOL(cuadro)), widths = rep(12.8, 13))
      
    } else if(tail(colnames(cuadro), 2) == "Total divorciadas") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-13):NCOL(cuadro)), widths = rep(12, 14))
      
    } else if(tail(colnames(cuadro), 2) == "Otros") {
      
      setColWidths(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                   cols = ((NCOL(cuadro)-7):NCOL(cuadro)), widths = rep(12.5, 8))
      
    }
    
    # ALTOS DE FILAS
    
    setRowHeights(get(paste0("cuadros_", fuente)), sheet = as.character(num_cuadro),
                    rows = fin_cabecera + 2, heights = 40)
      


 
  cat("Listo el cuadro", num_cuadro, "de", str_to_upper(fuente), "\n\n")
  
  rm(cuadro, envir = .GlobalEnv)
  
}


a_validar <- function(cuadro, fuente) {
  
  num_cuadro <<- num_cuadro + 1
  
  # Cargamos títulos sólo para acomodar las filas en que se encuentra el contenido de discrepancia
  
  titulos <- diccionario[names(diccionario) == paste0("titulos_", fuente)] %>% 
    .[[1]] %>% 
    filter(cuadro == num_cuadro) %>% 
    select(-cuadro)
  
  titulos <- as.vector(titulos$titulo)
  
  fin_cabecera <- as.numeric(NROW(titulos))
  
  
  # Enlistamos el cuadro para validar
  
  validando <- get(paste0("validando_", fuente)) %>% 
    listar(cuadro, rm = F)
  
  names(validando)[length(validando)] <- num_cuadro
  
  assign(paste0("validando_", fuente), validando, envir = .GlobalEnv)
  
  
  # Se llaman los cuadros desde las listas y se lo compara
  
  validando <- get(paste0("validando_", fuente ))[[as.character(num_cuadro)]]
  
  validador <- get(paste0("validador_", fuente ))[[as.character(num_cuadro)]]
  
  if((NCOL(validando) != NCOL(validador) ) |
     (NROW(validando) != NROW(validador) ) ) {
    
    if(NCOL(validando) > NCOL(validador) ) {
      
      col <- which(colnames(validando) %notin% colnames(validador))
      
      row <- rep(0, length(col))
      
      automatizacion <- rep("Esta columna está en la automatización pero a) o no está en el archivo de control o b) es distinta a ella", length(col))
      
      control <- rep("Revisar la correspondencia de esta columna en ambos archivos", length(col))

            
    } else if(NCOL(validando) < NCOL(validador) ) {
      
      col <- which(colnames(validador) %notin% colnames(validando)) 
      
      row <- rep(0, length(col))
      
      automatizacion <- rep("Revisar la correspondencia de esta columna en ambos archivos", length(col))
      
      control <- rep("Esta columna está en el archivo de control pero a) o no está en el tabulado resultado de la automatización, o b) es distinta a ella", length(col))
      

    } else if(NROW(validando) > NROW(validador) ) {
      
      row <- vector()
      
      for(i in 1:(NROW(validando[,1]))) {
        
        if(isFALSE(validador[i,1] == validando[i,1])) {
          
          distinto <- i
          
          row <- c(row, distinto) 
          
        }
        
      }
      
      col <- rep(0, length(row) ) 
      
      automatizacion <- validando[row,1]
      
      control <- rep("No está esta fila en el archivo de control. O tiene contenido distinto", length(row) ) 
                                   
    } else if(NROW(validando) < NROW(validador) ) {
      
      row <- vector()
      
      for(i in 1:(NROW(validador[,1]))) {
        
        if(isFALSE(validando[i,1] == validador[i,1])) {
        
          distinto <- i
        
          row <- c(row, distinto) 
        
        }
        
      }
      
      col <- rep(0, length(row) ) 
                                   
      automatizacion <- rep("No está esta fila en la automatización. O tiene contenido distinto", length(row) )
    
      control <- validador[row,1]
      
    }
    
    comparacion <- data.frame( "row" = row,
                               "col" = col,
                               "automatizacion" = automatizacion,
                               "control" = control )
    
        
  } else {
    
    comparacion <- as.data.frame(which(validando != validador, arr.ind=TRUE ))
    
    for(i in seq_len(NROW(comparacion))) {
      
      comparacion$automatizacion[i] <- validando[comparacion$row[i], comparacion$col[i]]
      
      comparacion$control[i] <- validador[comparacion$row[i], comparacion$col[i]]
      
    }
    
  }
  
  comparacion$row <- comparacion$row + fin_cabecera + 2
  
  comparacion$cuadro <- rep(num_cuadro, NROW(comparacion))
  
  if(NROW(comparacion) == 0) {
    
    comparacion <- comparacion
  
  } else if(comparacion$col[1] == 0) {
    
    comparacion$col <- rep("Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón", NROW(comparacion))
    
  } else {
  
    comparacion$col <- LETTERS[comparacion$col]
    
  }
  
  resultado_validacion <- resultado_validacion %>% 
    listar(comparacion, rm = F)
  
  names(resultado_validacion)[length(resultado_validacion)] <- num_cuadro
  
  resultado_validacion <<- resultado_validacion
  
}



# Changelog
#
# Versión 0.1
# Se agregan las siguientes funciones u objetos:
# - orden_familias (lista con los nombres de las familias de delitos)
# - organizar_delitos
# - agregar_familias
# - encuadrar
# - a_validar
