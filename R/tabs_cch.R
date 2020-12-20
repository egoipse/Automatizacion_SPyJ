# Definiciones previas ----

cat("Creando los tabulados de CCH...\n\n")

if(exists("validador_cch")) {
  
  validacion_cch <- createWorkbook()
  
  validando_cch <- list()
  
  resultado_validacion <- list()
  
}


if(exists("datos_cch")) {
  
  cuadros_cch <- createWorkbook()
  
}


num_cuadro <- 0


# Limpieza de bases de datos ----

if("familia" %notin% colnames(datos_cch$casos_cch)) {

  datos_cch$casos_cch <- agregar_familias(datos_cch$casos_cch)  %>% 
    mutate(provincia = ifelse(provincia == "PUNTILLA",
                              "PUNILLA",
                              provincia),
           region = ifelse(region == "REGIÓN DE MAGALLANES Y ANTÁRTICA CHILENA",
                           "REGIÓN DE MAGALLANES Y DE LA ANTÁRTICA CHILENA",
                           region)
    ) %>% 
    asignar_regiones()
  
  saveRDS(datos_cch$casos_cch, diccionario$meta[which(diccionario$meta$id == "casos_cch"), "cont"])

}


if("familia" %notin% colnames(datos_cch$detenides_cch)) {

  datos_cch$detenides_cch <- agregar_familias(datos_cch$detenides_cch) %>% 
  asignar_regiones()
  
  saveRDS(datos_cch$detenides_cch, diccionario$meta[which(diccionario$meta$id == "detenides_cch"), "cont"])
  
}


# 1 ----

cuadro <- datos_cch$casos_cch %>%
  group_by(region) %>% 
  summarise(`2019`=sum(casos)) %>% 
  ungroup()  %>% 
  as.data.frame() %>% 
  ordenar_regiones() %>% 
  nombrar_regiones()

total_cuadro <- sum(cuadro$`2019`)

var_cuadro <- (total_cuadro - diccionario$c1_cch[which(diccionario$c1_cch$Región == "Total"),
                                                 NCOL(diccionario$c1_cch)] ) / diccionario$c1_cch[which(diccionario$c1_cch$Región == "Total"),
                                                                                                  NCOL(diccionario$c1_cch)] 
cuadro <- rbind(total_cuadro, var_cuadro, cuadro) %>% 
  cbind(diccionario$c1_cch, .) %>%  
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rm(total_cuadro, var_cuadro)

if(colnames(cuadro)[NCOL(cuadro) - 1] == "Región") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 2 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(provincia) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()
  
las_cols <- unlist(orden_familias_delitos)

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(familia == las_cols[i]) %>% 
    group_by(provincia) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[2:4] <- c(paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_provincias(cuadro) %>% 
  a_cero(2:NCOL(.)) %>% 
  a_numeros(2:NCOL(.))


colnames(cuadro) <- sub("constitución", "Constitución", colnames(cuadro))
colnames(cuadro) <- sub("gendarmería", "Gendarmería", colnames(cuadro))
colnames(cuadro) <- sub("investigaciones", "Investigaciones", colnames(cuadro))
colnames(cuadro) <- sub("Juzpol", "JuzPol", colnames(cuadro))

rm(nuevo, i, las_cols)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 3 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(delito) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()

las_cols <- diccionario$regiones$id[order(diccionario$regiones$orden)]

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(region == las_cols[i]) %>% 
    group_by(delito) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[2:4] <- c(paste0(diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])], ": Total casos"),
                            paste0(diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])], ": Casos sin detenciones"),
                            paste0(diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])], ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_delitos(cuadro) %>% 
  select(-3)

rm(nuevo, i, las_cols)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 4 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(provincia) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()

las_cols <- format(ISOdate(2000, 1:12, 1), "%B")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(mes == las_cols[i]) %>% 
    group_by(provincia) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[2:4] <- c(paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_provincias(cuadro) %>% 
  a_cero(2:NCOL(.)) %>% 
  a_numeros(2:NCOL(.))

rm(nuevo, i, las_cols)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 5 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(delito) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()

las_cols <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(dia == las_cols[i]) %>% 
    group_by(delito) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[2:4] <- c(paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_delitos(cuadro) %>% 
  select(-3)

rm(nuevo, i, las_cols)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }



# 6 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(delito) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()

las_cols <- sort(unique(datos_cch$casos_cch$tramo_hora))

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(tramo_hora == las_cols[i]) %>% 
    group_by(delito) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[2:4] <- c(paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_delitos(cuadro) %>% 
  select(-3) %>% 
  rename_with(~ sub("No ingresada", "Sin información", .x))

rm(nuevo, i, las_cols)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 7 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()

las_cols <- diccionario$regiones$id

las_cols <- las_cols[order(diccionario$regiones$orden)]

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(region == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:6] <- c(paste0(diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])], ": Total casos"),
                            paste0(diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])], ": Casos sin detenciones"),
                            paste0(diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])], ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),]

cuadro <- split(cuadro, as.factor(cuadro$`1dmcs`)) 

for(i in 1:length(cuadro)) {
  
  if(names(cuadro)[i] %notin% c("Robos")) {
    
    cuadro[[i]][,2:3] <- NULL
    
    cuadro[[i]] <- c("", str_to_sentence(names(cuadro)[[i]]),colSums(cuadro[[i]][,2:(NCOL(cuadro[[i]]))], na.rm = T), "")
    
  }
  
}

cuadro <- cuadro[names(orden_dmcs_cch)]

cuadro$Robos <- split(cuadro$Robos, as.factor(cuadro$Robos$`2dmcs`))


for(i in 1:length(cuadro$Robos)) {
  
    cuadro[[i]] <- c("", "", "", str_to_sentence(names(cuadro)[[i]]),colSums(cuadro[[i]][,2:(NCOL(cuadro[[i]]))], na.rm = T), "")
    
  
}


rm(nuevo, i, las_cols)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }  


# 12 ----

cuadro <- datos_cch$detenides_cch %>%
  group_by(region) %>% 
  summarise(`2019`=n()) %>% 
  ungroup()  %>% 
  as.data.frame() %>%
  ordenar_regiones() %>% 
  nombrar_regiones()

total_cuadro <- sum(cuadro$`2019`)

var_cuadro <- (total_cuadro - diccionario$c1_cch[which(diccionario$c1_cch$Región == "Total"),
                                                 NCOL(diccionario$c1_cch)] ) / diccionario$c1_cch[which(diccionario$c1_cch$Región == "Total"),
                                                                                                  NCOL(diccionario$c1_cch)] 
cuadro <- rbind(total_cuadro, var_cuadro, cuadro) %>% 
  cbind(diccionario$c12_cch, .) %>%  
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rm(total_cuadro, var_cuadro)

if(colnames(cuadro)[NCOL(cuadro) - 1] == "Región") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 13 ----

cuadro <- data.frame(delito = as.numeric())

las_cols <- diccionario$regiones$id[order(diccionario$regiones$orden)]

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(region == las_cols[i]) %>% 
    group_by(delito, provincia) %>% 
    summarise(detenides = n()) %>% 
    pivot_wider(names_from = provincia, values_from = detenides) %>% 
    rename_with(~ gsub("PROV\\.", "", .x)) %>% 
    rename_with(str_to_title, colnames(.)[2:NCOL(.)]) %>% 
    rename_with(~ paste(diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])], .x, sep = ": "),
                colnames(.)[2:NCOL(.)] ) %>% 
    as.data.frame()
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_delitos(cuadro) 

rm(nuevo,i)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 14 ----

cuadro.total <- datos_cch$detenides_cch %>% 
  group_by(familia, region) %>% 
  summarise(detenides = n()) %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>% 
  pivot_wider(names_from = Región, values_from = detenides) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  as.data.frame()

cuadro.hombres <- datos_cch$detenides_cch %>% 
  filter(sexo == "MASCULINO") %>% 
  group_by(familia) %>% 
  summarise(`Total hombres` = n()) %>% 
  as.data.frame()

cuadro.mujeres <- datos_cch$detenides_cch %>% 
  filter(sexo == "FEMENINO") %>% 
  group_by(familia) %>% 
  summarise(`Total mujeres` = n()) %>% 
  as.data.frame()

cuadro <- full_join(cuadro.total, cuadro.hombres) %>% 
  full_join(cuadro.mujeres) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.)) %>% 
  relocate(`Total hombres`, .after = )
