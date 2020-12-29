# Definiciones previas ----

cat("Creando los tabulados de CCH...\n\n")

if(exists("validador_cch")) {
  
  validacion_cch <- createWorkbook()
  
  validando_cch <- list()
  
  resultado_validacion <- list()
  
}


if(!is.na(diccionario$meta$cont[which(diccionario$meta$id == "procesar_cch")])) {
  
  cuadros_cch <- createWorkbook()
  
}


num_cuadro <- 0


# Limpieza y corrección de bases de datos ----

if("casos_cch" %in% names(datos_cch) &
   "familia" %notin% colnames(datos_cch$casos_cch)) {

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


if("detenides_cch" %in% names(datos_cch) &
   "familia" %notin% colnames(datos_cch$detenides_cch)) {

  datos_cch$detenides_cch <- agregar_familias(datos_cch$detenides_cch) %>% 
  asignar_regiones()
  
  saveRDS(datos_cch$detenides_cch, diccionario$meta[which(diccionario$meta$id == "detenides_cch"), "cont"])
  
}


if("accidentes_cch" %in% names(datos_cch) &
   "familia" %notin% colnames(datos_cch$accidentes_cch)) {
 
  for(i in seq_len(NROW(datos_cch$accidentes_cch))) {
    
    datos_cch$accidentes_cch[i, c("familia", "orden_causa", "nombre")] <-
      diccionario$accidentes[which(str_trim(diccionario$accidentes$glosa) == 
                                     str_trim(datos_cch$accidentes_cch$causas[i])), 
                  c("familia", "orden_causa", "nombre")]
    
  }
  
  datos_cch$accidentes_cch$causas <- NULL
  
  colnames(datos_cch$accidentes_cch)[which(colnames(datos_cch$accidentes_cch) == "nombre")] <-
    "causas"
  
  saveRDS(datos_cch$accidentes_cch, diccionario$meta[which(diccionario$meta$id == "accidentes_cch"), "cont"])
  
}


if("vehiculos_cch" %in% names(datos_cch) &
   "fecha" %notin% colnames(datos_cch$vehiculos_cch)) {

  datos_cch$vehiculos_cch$fecha <- as.Date(rep(NA, NROW(datos_cch$vehiculos_cch)))
    
  # datos_cch$vehiculos_cch$fecha <- sapply(datos_cch$vehiculos_cch$id, function(x) {
  #    x <- as.Date(datos_cch$accidentes_cch$fecha[which(x == datos_cch$accidentes_cch$id)])
  #  }, simplify = "vector" 
  # )
  
  for(i in seq_len(NROW(datos_cch$vehiculos_cch))) {
    
    datos_cch$vehiculos_cch$fecha[i] <-
      datos_cch$accidentes_cch$fecha[which(datos_cch$vehiculos_cch$id[i] == datos_cch$accidentes_cch$id)]
    
  }
  
  saveRDS(datos_cch$vehiculos_cch, diccionario$meta[which(diccionario$meta$id == "vehiculos_cch"), "cont"])
  
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
  a_numeros(2:NCOL(.)) %>% 
  rename_with(~ gsub("constitución", "Constitución", .x)) %>% 
  rename_with(~ gsub("gendarmería", "Gendarmería", .x)) %>% 
  rename_with(~ gsub("investigaciones", "Investigaciones", .x)) %>% 
  rename_with(~ gsub("chile", "Chile", .x)) %>% 
  rename_with(~ gsub("Juzpol", "JuzPol", .x))


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

las_cols <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))

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
  rename_with(~ gsub("No ingresada", "Sin información", .x))

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

las_cols <- diccionario$regiones$id[order(diccionario$regiones$orden)]

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

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 8 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()


las_cols <- format(ISOdate(2000, 1:12, 1), "%B")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(mes == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:6] <- c(paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 



# 9 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()


las_cols <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))


for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(dia == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:6] <- c(paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 10 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(`TOTAL: Total casos` = sum(casos),
            `TOTAL: Casos sin detenciones` = sum(denuncias),
            `TOTAL: Casos con detenciones`= sum(detenciones)
  ) %>% 
  as.data.frame()


las_cols <- sort( unique(datos_cch$detenides_cch$tramo_hora))

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(tramo_hora == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Total` = sum(casos),
              `Sin detenciones` = sum(denuncias),
              `Con detenciones`= sum(detenciones)
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:6] <- c(paste0(str_to_sentence(las_cols[i]), ": Total casos"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos sin detenciones"),
                            paste0(str_to_sentence(las_cols[i]), ": Casos con detenciones")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 11 ----

cuadro <- datos_cch$casos_cch %>% 
  group_by(grupo1_vif, grupo2_vif) %>% 
  summarise(casos = n()) %>% 
  as.data.frame()

las_cols <- diccionario$regiones$id[order(diccionario$regiones$orden)]

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$casos_cch %>% 
    filter(region == las_cols[i]) %>% 
    group_by(grupo1_vif, grupo2_vif) %>% 
    summarise(casos = n()) %>% 
    as.data.frame()
    
  colnames(nuevo)[NCOL(nuevo)] <- diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])]

  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] 

totales <- c("Total", colSums(cuadro[,3:(NCOL(cuadro))], na.rm = T))

cuadro <- split(cuadro, as.factor(cuadro$grupo1_vif)) 

for(i in 1:length(cuadro)) {
  
  if(names(cuadro)[i] == "Violencia Intrafamiliar") {
    
    cuadro[[i]][1,2] <- "MALTRATO HABITUAL (VIF)"
    
  } else {
  
    # cuadro[[i]][,2] <- paste0(cuadro[[i]][,1], ": ", str_to_sentence(cuadro[[i]][,2]))
    
    cuadro[[i]] <- rbind(c("", str_to_upper(names(cuadro)[[i]]),colSums(cuadro[[i]][,3:(NCOL(cuadro[[i]]))], na.rm = T)), cuadro[[i]]) 
    
  }
  
  cuadro[[i]][,1] <- NULL
  
} 


cuadro <- cuadro[names(orden_vif)] %>% 
  do.call("rbind", .) %>% 
  rbind(totales, .) %>% 
  # mutate_at("grupo2_vif", ~ gsub("^.*: ", "", .x)) %>% 
  rename(`Tipo de víctima, consecuencia` = 1,
         Total = 2) %>% 
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rownames(cuadro) <- c()

rm(las_cols, nuevo, i)

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
  a_cero(2:NCOL(.)) %>% 
  relocate(`Total hombres`, .after = Total) %>% 
  relocate(`Total mujeres`, .after = `Total hombres`) %>% 
  split(., as.factor(.$familia)) %>% 
  .[names(orden_familias_delitos)] %>% 
  do.call("rbind", .) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  mutate_at("familia", str_to_sentence) %>%
  mutate_at("familia", ~gsub("chile", "Chile", .x)) %>% 
  rename(`Familia de delitos` = familia) %>% 
  a_numeros(2:NCOL(.)) %>% 
  as.data.frame()

rm(cuadro.hombres, cuadro.mujeres, cuadro.total)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 15 ----

cuadro <- data.frame(delito = as.numeric())

las_cols <- format(ISOdate(2000, 1:12, 1), "%B")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(mes == las_cols[i]) %>% 
    group_by(delito, sexo) %>% 
    summarise(detenides = n()) %>% 
    pivot_wider(names_from = sexo, values_from = detenides) %>% 
    rename(Hombres = MASCULINO,
           Mujeres = FEMENINO
    ) %>% 
    relocate(Hombres, .after = delito) %>% 
    rename_with(~ paste(str_to_sentence(las_cols[i]), .x, sep = ": "),
                colnames(.)[2:NCOL(.)] ) %>% 
    as.data.frame()
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_delitos(cuadro) 

rm(nuevo,i)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }

  
# 16 ----

cuadro <- data.frame(delito = as.numeric())

las_cols <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(dia == las_cols[i]) %>% 
    group_by(delito, sexo) %>% 
    summarise(detenides = n()) %>% 
    pivot_wider(names_from = sexo, values_from = detenides) %>% 
    rename(Hombres = MASCULINO,
           Mujeres = FEMENINO
    ) %>% 
    relocate(Hombres, .after = delito) %>% 
    rename_with(~ paste(str_to_sentence(las_cols[i]), .x, sep = ": "),
                colnames(.)[2:NCOL(.)] ) %>% 
    as.data.frame()
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_delitos(cuadro) 

rm(nuevo,i)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 17 ----

cuadro <- data.frame(delito = as.numeric())

las_cols <- sort( unique(datos_cch$detenides_cch$tramo_hora))

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(tramo_hora == las_cols[i]) %>% 
    group_by(delito, sexo) %>% 
    summarise(detenides = n()) %>% 
    pivot_wider(names_from = sexo, values_from = detenides) %>% 
    rename(Hombres = MASCULINO,
           Mujeres = FEMENINO
    ) %>% 
    relocate(Hombres, .after = delito) %>% 
    rename_with(~ paste(str_to_sentence(las_cols[i]), .x, sep = ": "),
                colnames(.)[2:NCOL(.)] ) %>% 
    as.data.frame()
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- organizar_delitos(cuadro) %>% 
  rename_with(~ gsub("hrs", "horas", .x))

rm(nuevo,i)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 18 ----


cuadro.tramo <- datos_cch$detenides_cch %>%
  group_by(grupo_etario, sexo) %>%
  summarise(Detenides=n()) %>% 
  pivot_wider(names_from = sexo, values_from = Detenides) %>% 
  rename(Hombres = MASCULINO,
         Mujeres = FEMENINO,
         `VARIABLES SOCIODEMOGRÁFICAS` = 1
  ) %>% 
  relocate(Hombres, .after = `VARIABLES SOCIODEMOGRÁFICAS`) %>% 
  rename_with(str_to_sentence, colnames(2:NCOL(.))) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  mutate_at("VARIABLES SOCIODEMOGRÁFICAS", str_to_sentence) %>% 
  mutate(`VARIABLES SOCIODEMOGRÁFICAS` = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "Total" ~
                                                     "EDAD",
                                                   TRUE ~ as.character(`VARIABLES SOCIODEMOGRÁFICAS`))) %>% 
  ungroup() %>%
  as.data.frame()


cuadro.estado <- datos_cch$detenides_cch %>%
  group_by(est_civil, sexo) %>%
  summarise(Detenides=n()) %>% 
  pivot_wider(names_from = sexo, values_from = Detenides) %>% 
  rename(Hombres = MASCULINO,
         Mujeres = FEMENINO,
         `VARIABLES SOCIODEMOGRÁFICAS` = 1
  ) %>% 
  relocate(Hombres, .after = `VARIABLES SOCIODEMOGRÁFICAS`) %>% 
  rename_with(str_to_sentence, colnames(2:NCOL(.))) %>% 
  mutate(orden = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "SOLTEROS" ~ 1,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "CASADOS" ~ 2,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "VIUDOS" ~ 3,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "NO ESPECIFICADOS" ~ 4)) %>% 
  arrange(orden) %>% 
  select(!orden) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  mutate_at("VARIABLES SOCIODEMOGRÁFICAS", str_to_sentence) %>% 
  mutate(`VARIABLES SOCIODEMOGRÁFICAS` = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "Total" ~
                                                     "ESTADO CIVIL",
                                                   `VARIABLES SOCIODEMOGRÁFICAS` == "No especificados" ~
                                                     "Sin información",
                                                   TRUE ~ as.character(`VARIABLES SOCIODEMOGRÁFICAS`))) %>% 
  ungroup() %>%
  as.data.frame()


cuadro.estudios <- datos_cch$detenides_cch %>%
  group_by(escolaridad, sexo) %>%
  summarise(Detenides=n()) %>% 
  pivot_wider(names_from = sexo, values_from = Detenides) %>% 
  rename(Hombres = MASCULINO,
         Mujeres = FEMENINO,
         `VARIABLES SOCIODEMOGRÁFICAS` = 1
  ) %>% 
  relocate(Hombres, .after = `VARIABLES SOCIODEMOGRÁFICAS`) %>% 
  rename_with(str_to_sentence, colnames(2:NCOL(.))) %>% 
  mutate(orden = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "ANALFABETO" ~ 1,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "BASICOS" ~ 2,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "MEDIOS" ~ 3,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "SUPERIORES" ~ 4,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "NO ESPECIFICADOS" ~ 5)) %>% 
  arrange(orden) %>% 
  select(!orden) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  mutate_at("VARIABLES SOCIODEMOGRÁFICAS", str_to_sentence) %>% 
  mutate(`VARIABLES SOCIODEMOGRÁFICAS` = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "Total" ~
                                                     "NIVEL DE EDUCACIÓN",
                                                   `VARIABLES SOCIODEMOGRÁFICAS` == "No especificados" ~
                                                     "Sin información",
                                                   `VARIABLES SOCIODEMOGRÁFICAS` == "Basicos" ~
                                                     "Básicos",
                                                   TRUE ~ as.character(`VARIABLES SOCIODEMOGRÁFICAS`))) %>% 
  ungroup() %>%
  as.data.frame()


cuadro.nacionalidad <- datos_cch$detenides_cch %>%
  group_by(fct_other(nacion, keep = c("CHILENA", "VENEZOLANA", "PERUANA", "HAITIANA", "COLOMBIANA",
                                      "BOLIVIANA", "ARGENTINA", "ECUATORIANA", "DOMINICANA", "PAÍS NO DECLARADO")),
           sexo) %>%
  # group_by(nacion, sexo) %>%
  summarise(Detenides=n()) %>% 
  pivot_wider(names_from = sexo, values_from = Detenides) %>% 
  rename(Hombres = MASCULINO,
         Mujeres = FEMENINO,
         `VARIABLES SOCIODEMOGRÁFICAS` = 1
  ) %>% 
  relocate(Hombres, .after = `VARIABLES SOCIODEMOGRÁFICAS`) %>% 
  rename_with(str_to_sentence, colnames(2:NCOL(.))) %>% 
  mutate(orden = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "CHILENA" ~ 1,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "VENEZOLANA" ~ 2,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "PERUANA" ~ 3,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "HAITIANA" ~ 4,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "COLOMBIANA" ~ 5,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "BOLIVIANA" ~ 6,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "ARGENTINA" ~ 7,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "ECUATORIANA" ~ 8,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "DOMINICANA" ~ 9,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "Other" ~ 10,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "PAÍS NO DECLARADO" ~ 11
                           )) %>% 
  arrange(orden) %>% 
  select(!orden) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  mutate_at("VARIABLES SOCIODEMOGRÁFICAS", str_to_sentence) %>% 
  mutate(`VARIABLES SOCIODEMOGRÁFICAS` = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "Total" ~
                                                     "NACIONALIDAD",
                                                   `VARIABLES SOCIODEMOGRÁFICAS` == "Other" ~
                                                     "Otros países",
                                                   TRUE ~ as.character(`VARIABLES SOCIODEMOGRÁFICAS`))) %>% 
  ungroup() %>%
  as.data.frame()


cuadro.oficio <- datos_cch$detenides_cch %>%
  group_by(oficio, sexo) %>%
  summarise(Detenides=n()) %>% 
  pivot_wider(names_from = sexo, values_from = Detenides) %>% 
  rename(Hombres = MASCULINO,
         Mujeres = FEMENINO,
         `VARIABLES SOCIODEMOGRÁFICAS` = 1
  ) %>% 
  relocate(Hombres, .after = `VARIABLES SOCIODEMOGRÁFICAS`) %>% 
  rename_with(str_to_sentence, colnames(2:NCOL(.))) %>% 
  mutate(orden = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "COMERCIANTES" ~ 1,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "EMPLEADOS" ~ 2,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "ESTUDIANTES" ~ 3,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "OBREROS" ~ 4,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "CHOFERES" ~ 5,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "PROFESIONALES" ~ 6,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "SIN PROFESION U OFIC." ~ 7,
                           `VARIABLES SOCIODEMOGRÁFICAS` == "OTRAS" ~ 8
  )) %>% 
  arrange(orden) %>% 
  select(!orden) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  mutate_at("VARIABLES SOCIODEMOGRÁFICAS", str_to_sentence) %>% 
  mutate(`VARIABLES SOCIODEMOGRÁFICAS` = case_when(`VARIABLES SOCIODEMOGRÁFICAS` == "Total" ~
                                                     "PROFESIÓN U OFICIO",
                                                   `VARIABLES SOCIODEMOGRÁFICAS` == "Sin profesion u ofic." ~
                                                     "Sin profesion u oficio",
                                                   TRUE ~ as.character(`VARIABLES SOCIODEMOGRÁFICAS`))) %>% 
  ungroup() %>%
  as.data.frame()


cuadro <- full_join(cuadro.tramo, cuadro.estado) %>% 
  full_join(cuadro.estudios) %>% 
  full_join(cuadro.nacionalidad) %>% 
  full_join(cuadro.oficio) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  a_numeros(2:NCOL(.)) %>% 
  as.data.frame()


rm(cuadro.tramo, cuadro.estado, cuadro.estudios, cuadro.nacionalidad, cuadro.oficio)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 19 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- format(ISOdate(2000, 1:12, 1), "%B")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(mes == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }   


# 20 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(dia == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }   


# 21 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- sort(unique(datos_cch$detenides_cch$tramo_hora))

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(tramo_hora == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }   


# 22 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- sort(unique(datos_cch$detenides_cch$grupo_etario))

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(grupo_etario == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }   


# 23 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- c("SOLTEROS", "CASADOS", "VIUDOS", "NO ESPECIFICADOS")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(est_civil == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 24 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- c("ANALFABETO" , "BASICOS", "MEDIOS" , "SUPERIORES", "NO ESPECIFICADOS")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(escolaridad == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 25 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- c("COMERCIANTES" , "EMPLEADOS", "ESTUDIANTES", "OBREROS" , "CHOFERES",
              "PROFESIONALES", "SIN PROFESION U OFIC.", "OTRAS")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(oficio == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  organizar_dmcs_cch()

rm(nuevo, i, las_cols)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 26 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO")) %>% 
  as.data.frame()

las_cols <- c("CHILENA", "VENEZOLANA", "PERUANA", "HAITIANA", "COLOMBIANA",
              "BOLIVIANA", "ARGENTINA", "ECUATORIANA", "DOMINICANA", "PAÍS NO DECLARADO")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(nacion == las_cols[i]) %>% 
    group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
    summarise(`Hombres` = sum(sexo == "MASCULINO"),
              `Mujeres` = sum(sexo == "FEMENINO")
    ) %>% 
    as.data.frame()
  
  colnames(nuevo)[4:5] <- c(paste0(str_to_sentence(las_cols[i]), ": Hombres"),
                            paste0(str_to_sentence(las_cols[i]), ": Mujeres")
  )
  
  cuadro <- full_join(cuadro, nuevo)
  
}


otros <-   nuevo <- datos_cch$detenides_cch %>% 
  filter(nacion %notin% c("CHILENA", "VENEZOLANA", "PERUANA", "HAITIANA", "COLOMBIANA",
                          "BOLIVIANA", "ARGENTINA", "ECUATORIANA", "DOMINICANA", "PAÍS NO DECLARADO")) %>% 
  group_by(`1dmcs`, `2dmcs`, `3dmcs`) %>% 
  summarise(`Otros países: Hombres` = sum(sexo == "MASCULINO"),
            `Otros países: Mujeres` = sum(sexo == "FEMENINO")
  ) %>% 
  as.data.frame()

cuadro <- full_join(cuadro, otros) %>%  
  .[-which(is.na(.[,1])),] %>% 
  organizar_dmcs_cch() %>% 
  relocate(`Otros países: Hombres`, .after = `Dominicana: Mujeres`) %>% 
  relocate(`Otros países: Mujeres`, .before = `País no declarado: Hombres`)

rm(nuevo, i, las_cols, otros)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 27 ----

cuadro <- datos_cch$detenides_cch %>% 
  group_by(grupo1_vif, grupo2_vif, factor(sexo,
                                          levels = c("MASCULINO", "FEMENINO"),
                                          labels = c("Hombre", "Mujer"))) %>% 
  summarise(casos = n()) %>% 
  rename(sexo = 3) %>% 
  as.data.frame()

las_cols <- diccionario$regiones$id[order(diccionario$regiones$orden)]

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$detenides_cch %>% 
    filter(region == las_cols[i]) %>% 
    group_by(grupo1_vif, grupo2_vif, factor(sexo,
                                            levels = c("MASCULINO", "FEMENINO"),
                                            labels = c("Hombre", "Mujer"))) %>%  
    summarise(casos = n()) %>% 
    rename(sexo = 3) %>% 
    as.data.frame()
  
  colnames(nuevo)[NCOL(nuevo)] <- diccionario$regiones$nombre[which(diccionario$regiones$id == las_cols[i])]
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- cuadro[-which(is.na(cuadro[,1])),] %>% 
  mutate_at("sexo", as.character)

totales <- c("Total", colSums(cuadro[,4:(NCOL(cuadro))], na.rm = T))

cuadro <- split(cuadro, as.factor(cuadro$grupo1_vif)) 

for(i in 1:length(cuadro)) {
  
  if(names(cuadro)[i] != "Violencia Intrafamiliar") {
    
    suma <- c(str_to_upper(names(cuadro[i])),colSums(cuadro[[i]][,4:(NCOL(cuadro[[i]]))], na.rm = T))
    
    cuadro[[i]] <- split(cuadro[[i]], as.factor(cuadro[[i]]$grupo2_vif))
    
    for(j in 1:length(cuadro[[i]])) {
      
      suma2 <- c("", "", str_to_sentence(names(cuadro[[i]])[[j]]),colSums(cuadro[[i]][[j]][,4:(NCOL(cuadro[[i]][[j]]))], na.rm = T))
      
      cuadro[[i]][[j]] <- rbind(suma2, cuadro[[i]][[j]])
      
      cuadro[[i]][[j]][,1:2] <- NULL 
      
      colnames(cuadro[[i]][[j]])[1] <- "ya"
      
    }
    
    cuadro[[i]] <- do.call("rbind", cuadro[[i]])
    
    cuadro[[i]] <- rbind(suma, cuadro[[i]])
    
  } else if(names(cuadro)[i] == "Violencia Intrafamiliar") {
    
    suma <- c("", "", "MALTRATO HABITUAL (VIF)",colSums(cuadro[[i]][,4:(NCOL(cuadro[[i]]))], na.rm = T))
    
    cuadro[[i]] <- rbind(suma, cuadro[[i]])
    
    cuadro[[i]][,1:2] <- NULL 
    
    colnames(cuadro[[i]])[1] <- "ya"
    
  }
  
} 


cuadro <- cuadro[names(orden_vif)] %>% 
  do.call("rbind", .) %>% 
  rbind(totales, .) %>% 
  rename(`Tipo de víctima, consecuencia y sexo de la persona detenida` = 1,
         Total = 2) %>% 
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rownames(cuadro) <- c()

rm(las_cols, nuevo, i)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 28 ----

cuadro <- datos_cch$accidentes_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(region,tipo) %>% 
  summarise(accidentes = n()) %>% 
  pivot_wider(names_from = tipo, values_from = accidentes) %>% 
  relocate(Colisión, .after = Caída) %>% 
  relocate(Otros, .after = Volcadura) %>% 
  regiones_accidentes() %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()
  

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 29 ----

cuadro <- datos_cch$accidentes_cch  %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(familia, causas, tipo, orden_causa) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = tipo, values_from = n) %>%
  as.data.frame()

totales <- c("Total", colSums(cuadro[,4:(NCOL(cuadro))], na.rm = T))

cuadro <- split(cuadro, as.factor(cuadro$familia))

for(i in 1:length(cuadro)) {
  
  suma <- c("", str_to_upper(names(cuadro[i])),"",colSums(cuadro[[i]][,4:(NCOL(cuadro[[i]]))], na.rm = T))
  
  cuadro[[i]] <- arrange(cuadro[[i]], as.numeric(orden_causa)) %>% 
    rbind(suma, .)
    
  
  cuadro[[i]][,c("familia", "orden_causa")] <- NULL 
  
}

cuadro <- cuadro[
  names(list(
    `FALLAS MECÁNICAS` = c(),
    ADELANTAMIENTOS = c(),
    CONDUCCIÓN = c(),
    `NO RESPETAR DERECHO PREFERENTE DE PASO` = c(),
    PASAJERO = c(),
    PEATÓN = c(),
    SEÑALIZACIÓN = c(),
    VELOCIDAD = c(),
    `CARGA Y/O DESCARGA` = c(),
    `OTRAS INFRACCIONES` = c()
    )
)] %>% 
  do.call("rbind", .) %>% 
  rbind(totales, .) %>% 
  rename(`Causas que lo origina` = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>%
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

columnas <- c("Causas que lo origina", "Total", "Atropello", "Caída",
              "Colisión", "Choque", "Volcadura", "Otros")

if (any(colnames(cuadro) %notin% columnas)) {
  
  no_estan <- colnames(cuadro)[colnames(cuadro) %notin% columnas]
  
  cuadro$Otros <- rowSums(cuadro[, c("Otros", no_estan)], na.rm = T)
    
  cuadro <- cuadro[, !names(cuadro) %in% no_estan, drop=FALSE]

}
  

cuadro <- cuadro[, columnas]

rownames(cuadro) <- c()

rm(totales, suma, i, columnas, no_estan)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 30 ----

cuadro <- datos_cch$personas_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(region) %>% 
  summarise(personas = n()) %>% 
  regiones_accidentes() %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  pivot_wider(names_from = Región, values_from = personas) %>% 
  cbind("Total", .) %>% 
  rename(resultado = 1) %>%
  as.data.frame()

las_cols <- c("Atropello", "Caída", "Colisión", "Choque", "Volcadura", "Otros")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$personas_cch %>% 
    filter(tipo == las_cols[i] & clase == "Accdte. Tránsito" ) %>% 
    group_by(region, resultado) %>% 
    summarise(personas = n()) %>% 
    regiones_accidentes() %>% 
    ordenar_regiones() %>% 
    nombrar_regiones() %>%
    pivot_wider(names_from = Región, values_from = personas) %>% 
    resultados_accidente() %>% 
    sumar_columnas(2:NCOL(.), pos = 1) %>% 
    mutate(resultado = case_when(resultado == "Total" ~ str_to_upper(las_cols[i]),
                                 TRUE ~ as.character(resultado))) %>% 
    a_numeros(2:NCOL(.)) %>% 
    as.data.frame()
  
  cuadro <- full_join(cuadro, nuevo)
  
}

cuadro <- rename(cuadro, `Tipo de accidente y consecuencia` = resultado) %>% 
  a_cero(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  as.data.frame()

rm(nuevo,i)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 31 ----

cuadro.actual <- datos_cch$accidentes_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(format(ISOdate(2000, as.factor(month(ymd(fecha))), 1), 	"%B") ) %>% 
  summarise(`Personas ilesas` = sum(ilesos),
            `Personas lesionadas leves` = sum(leves),
            `Personas lesionadas menos graves` = sum(menos_grave),
            `Personas lesionadas graves` = sum(graves),
            `Personas fallecidas` = sum(muerto)
            ) %>% 
  rename(fecha = 1) %>% 
  ordenar_meses() %>% 
  mutate_at("fecha", str_to_sentence) %>%
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  rename(`Total personas participantes` = Total) %>% 
  mutate(`Total personas accidentadas` = `Personas lesionadas leves` + `Personas lesionadas menos graves` + `Personas lesionadas graves` + `Personas fallecidas`
  ) %>% 
  relocate(`Total personas accidentadas`, .before = `Personas lesionadas leves`) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  mutate(fecha = case_when(fecha == "Total" ~
                             diccionario$meta$cont[which(diccionario$meta$id == "agno")],
                           TRUE ~ as.character(fecha))) %>% 
  rename(`Mes y año` = 1) %>% 
  a_numeros(2:NCOL(.)) %>% 
  as.data.frame()
  

cuadro <- diccionario$c31_cch %>% 
  rename_with(~ gsub("\\.", " ", .x)) %>% 
  rename_with(str_trim, colnames(.)) %>% 
  rbind(., cuadro.actual) %>% 
  as.data.frame()

rm(cuadro.actual)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 32 ----  

cuadro <- datos_cch$personas_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(resultado) %>% 
  summarise(Total = n(),
    `Total hombres` = sum(sexo == "MASCULINO"),
    `Total mujeres` = sum(sexo == "FEMENINO"),
    `Hombres menores` = sum(sexo == "MASCULINO" & mayoria_edad == "< 18 años"),
    `Mujeres menores` = sum(sexo == "FEMENINO" & mayoria_edad == "< 18 años"),
    `Hombres mayores` = sum(sexo == "MASCULINO" & mayoria_edad == ">= 18 años"),
    `Mujeres mayores` = sum(sexo == "FEMENINO" & mayoria_edad == ">= 18 años"),
    `Hombres sin información` = sum(sexo == "MASCULINO" & mayoria_edad == "Sin Dato"),
    `Mujeres sin información` = sum(sexo == "FEMENINO" & mayoria_edad == "Sin Dato")
  ) %>% 
  resultados_accidente() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  as.data.frame()

las_cols <- unique(datos_cch$personas_cch$calidad)[order(unique(datos_cch$personas_cch$calidad))]

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$personas_cch %>% 
    filter(calidad == las_cols[i] & clase == "Accdte. Tránsito") %>% 
    group_by(resultado) %>% 
    summarise(Total = n(),
              `Total hombres` = sum(sexo == "MASCULINO"),
              `Total mujeres` = sum(sexo == "FEMENINO"),
              `Hombres menores` = sum(sexo == "MASCULINO" & mayoria_edad == "< 18 años"),
              `Mujeres menores` = sum(sexo == "FEMENINO" & mayoria_edad == "< 18 años"),
              `Hombres mayores` = sum(sexo == "MASCULINO" & mayoria_edad == ">= 18 años"),
              `Mujeres mayores` = sum(sexo == "FEMENINO" & mayoria_edad == ">= 18 años"),
              `Hombres sin información` = sum(sexo == "MASCULINO" & mayoria_edad == "Sin Dato"),
              `Mujeres sin información` = sum(sexo == "FEMENINO" & mayoria_edad == "Sin Dato")
              ) %>% 
    resultados_accidente() %>% 
    sumar_columnas(2:NCOL(.), pos = 1) %>% 
    mutate(resultado = case_when(resultado == "Total" ~ str_to_upper(las_cols[i]),
                                 TRUE ~ as.character(resultado))) %>% 
    as.data.frame()
  
  
 
  cuadro <- rbind(cuadro, nuevo)
   
}


cuadro <- a_numeros(cuadro, 2:NCOL(cuadro)) %>% 
  mutate(`Total menores` = `Hombres menores` + `Mujeres menores`,
       `Total mayores` = `Hombres mayores` + `Mujeres mayores`,
       `Total sin información` = `Hombres sin información` + `Mujeres sin información`) %>% 
  relocate(`Total menores`, .before = `Hombres menores`) %>% 
  relocate(`Total mayores`, .before = `Hombres mayores`) %>% 
  relocate(`Total sin información`, .before = `Hombres sin información`) %>%
  rename(`Rol de la persona y tipo de consecuencia` = 1) %>% 
  as.data.frame()

rm(nuevo,i)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 33 ----

cuadro.actual <- datos_cch$vehiculos_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(format(ISOdate(2000, as.factor(month(ymd(fecha))), 1), "%B"), vehiculo  ) %>% 
  summarise(total = n()) %>% 
  pivot_wider(names_from = vehiculo, values_from = total) %>% 
  rename(fecha = 1) %>% 
  ordenar_meses() %>% 
  mutate_at("fecha", str_to_sentence) %>%
  sumar_filas(2:NCOL(.), pos = 1) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  mutate(fecha = case_when(fecha == "Total" ~
                             diccionario$meta$cont[which(diccionario$meta$id == "agno")],
                           TRUE ~ as.character(fecha))) %>% 
  rename(`Año y mes` = `fecha`,
         `Locomoción colectiva` = `LOC. COLECTIVA`,
         `Automóviles` = `AUTOMOVIL`,
         `Camiones` = `CAMION`,
         `Camionetas` = `CAMIONETA`,
         `Furgones` = `FURGON`,
         `Motocicletas` = `MOTOCICLETA`,
         `Bicicletas` = `BICICLETA`
         ) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[1:NCOL(.)])) %>% 
  a_numeros(2:NCOL(.)) %>% 
  as.data.frame()

cuadro.actual <- cuadro.actual[, gsub("\\.", " ", colnames(diccionario$c33_cch))]

cuadro <- diccionario$c33_cch %>% 
  rename_with(~ gsub("\\.", " ", .x)) %>% 
  rename_with(str_trim, colnames(.)) %>% 
  rbind(., cuadro.actual) %>% 
  as.data.frame()

rm(cuadro.actual)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 34 ----

cuadro <- datos_cch$vehiculos_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(region, vehiculo ) %>% 
  summarise(total = n()) %>% 
  pivot_wider(names_from = vehiculo, values_from = total) %>% 
  regiones_accidentes() %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  sumar_filas(2:NCOL(.), pos = 1) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  a_numeros(2:NCOL(.)) %>% 
  as.data.frame()


las_cols <- c("Atropello", "Caída", "Colisión", "Choque", "Volcadura", "Otros")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$vehiculos_cch %>%
    filter(tipo == las_cols[i] & clase == "Accdte. Tránsito") %>% 
    group_by(region, vehiculo ) %>% 
    summarise(total = n()) %>% 
    pivot_wider(names_from = vehiculo, values_from = total) %>% 
    regiones_accidentes() %>% 
    ordenar_regiones() %>% 
    nombrar_regiones() %>%
    sumar_filas(2:NCOL(.), pos = 1) %>%
    sumar_columnas(2:NCOL(.), pos = 1) %>%
    a_numeros(2:NCOL(.)) %>% 
    mutate(`Región` =
             case_when(`Región` == "Total" ~ str_to_upper(las_cols[i]),
                                 TRUE ~ as.character(`Región`))) %>% 
    as.data.frame()
  
  
  if(length(colnames(nuevo)) < length(colnames(cuadro)) ) {
    
    faltan <- colnames(cuadro)[which(colnames(cuadro) %notin% colnames(nuevo))]
    
    posics <- which(colnames(cuadro) %notin% colnames(nuevo))
    
    for(j in 1:length(faltan)) {
      
      ya <- rep(NA, NROW(nuevo))
      
      # mitad1 <- nuevo[,1:(posics[j]-1)]
      
      # mitad2 <- nuevo[,posics[j]:NCOL(nuevo)]
      
      nuevo <- cbind(nuevo, ya)  
    
      colnames(nuevo)[which(colnames(nuevo) == "ya")] <- faltan[j]
      
    }
    
  }
  
  cuadro <- rbind(cuadro, nuevo)
  
}


cuadro <- rename(cuadro,
  `Región y tipo de accidente` = `Región`,
  `Locomoción colectiva` = `LOC. COLECTIVA`,
  `Automóviles` = `AUTOMOVIL`,
  `Camiones` = `CAMION`,
  `Camionetas` = `CAMIONETA`,
  `Furgones` = `FURGON`,
  `Motocicletas` = `MOTOCICLETA`,
  `Bicicletas` = `BICICLETA`
  ) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[1:NCOL(.)])) %>% 
  .[, c(colnames(.)[1],
             gsub("\\.", " ", colnames(diccionario$c33_cch)[2:NCOL(diccionario$c33_cch)]))] %>% 
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()


rm(nuevo,i, j, las_cols, faltan, ya)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 35 ----

cuadro.actual <- datos_cch$vehiculos_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(format(ISOdate(2000, as.factor(month(ymd(fecha))), 1), "%B"), calidad  ) %>% 
  summarise(total = n()) %>% 
  pivot_wider(names_from = calidad, values_from = total) %>% 
  rename(fecha = 1) %>% 
  ordenar_meses() %>% 
  mutate_at("fecha", str_to_sentence) %>%
  sumar_filas(2:NCOL(.), pos = 1) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  a_numeros(2:NCOL(.)) %>% 
  mutate(Taxis = `TAXI BASICO` + `TAXI COLECTIVO`,
         `TAXI BASICO` = NULL,
         `TAXI COLECTIVO` = NULL,
         fecha = case_when(fecha == "Total" ~
                             diccionario$meta$cont[which(diccionario$meta$id == "agno")],
                           TRUE ~ as.character(fecha))) %>% 
  rename(`Mes` = `fecha`,
         `Fiscales` = `FISCAL`,
         `Particulares` = `PARTICULAR`,
         `Transporte escolar` = `TRANSP. ESCOLAR`,
         `Locomoción colectiva` = `LOC. COLECTIVA`
  ) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[1:NCOL(.)])) %>% 
  a_numeros(2:NCOL(.)) %>% 
  as.data.frame()

cuadro.actual <- cuadro.actual[, gsub("\\.", " ", colnames(diccionario$c35_cch))]

cuadro <- diccionario$c35_cch %>% 
  rename_with(~ gsub("\\.", " ", .x)) %>% 
  rename_with(str_trim, colnames(.)) %>% 
  rbind(., cuadro.actual) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rm(cuadro.actual)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 36 ----

cuadro <- datos_cch$vehiculos_cch %>% 
  filter(clase == "Accdte. Tránsito") %>%
  group_by(region, calidad ) %>% 
  summarise(total = n()) %>% 
  pivot_wider(names_from = calidad, values_from = total) %>% 
  regiones_accidentes() %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  sumar_filas(2:NCOL(.), pos = 1) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>%
  a_numeros(2:NCOL(.)) %>%
  as.data.frame()


las_cols <- c("Atropello", "Caída", "Colisión", "Choque", "Volcadura", "Otros")

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$vehiculos_cch %>%
    filter(tipo == las_cols[i] & clase == "Accdte. Tránsito") %>% 
    group_by(region, calidad ) %>% 
    summarise(total = n()) %>% 
    pivot_wider(names_from = calidad, values_from = total) %>% 
    regiones_accidentes() %>% 
    ordenar_regiones() %>% 
    nombrar_regiones() %>%
    sumar_filas(2:NCOL(.), pos = 1) %>%
    sumar_columnas(2:NCOL(.), pos = 1) %>%
    a_numeros(2:NCOL(.)) %>% 
    mutate(`Región` =
             case_when(`Región` == "Total" ~ str_to_upper(las_cols[i]),
                       TRUE ~ as.character(`Región`))) %>% 
    as.data.frame()
  
  
  if(length(colnames(nuevo)) < length(colnames(cuadro)) ) {
    
    faltan <- colnames(cuadro)[which(colnames(cuadro) %notin% colnames(nuevo))]
    
    posics <- which(colnames(cuadro) %notin% colnames(nuevo))
    
    for(j in 1:length(faltan)) {
      
      ya <- rep(NA, NROW(nuevo))
      
      # mitad1 <- nuevo[,1:(posics[j]-1)]
      
      # mitad2 <- nuevo[,posics[j]:NCOL(nuevo)]
      
      nuevo <- cbind(nuevo, ya)  
      
      colnames(nuevo)[which(colnames(nuevo) == "ya")] <- faltan[j]
      
    }
    
  }
  
  cuadro <- rbind(cuadro, nuevo)
  
}



cuadro <-  cuadro %>% 
  mutate(Taxis = `TAXI BASICO` + `TAXI COLECTIVO`,
         `TAXI BASICO` = NULL,
         `TAXI COLECTIVO` = NULL
         ) %>% 
  rename( Mes = Región,
         `Fiscales` = `FISCAL`,
         `Particulares` = `PARTICULAR`,
         `Transporte escolar` = `TRANSP. ESCOLAR`,
         `Locomoción colectiva` = `LOC. COLECTIVA`
  ) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[1:NCOL(.)])) %>% 
  .[, gsub("\\.", " ", colnames(diccionario$c35_cch))] %>% 
  rename(`Región y tipo de accidente` = 1) %>% 
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()


rm(nuevo,i, j, las_cols, faltan, ya)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 37 ----

cuadro.actual <- datos_cch$accidentes_cch %>% 
  filter(clase == "Accdte. Ferroviario") %>%
  group_by(format(ISOdate(2000, as.factor(month(ymd(fecha))), 1), "%B"), tipo) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = tipo, values_from = n) %>% 
  rename(fecha = 1) %>%
  ordenar_meses() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  mutate(fecha = case_when(fecha == "Total" ~
                             diccionario$meta$cont[which(diccionario$meta$id == "agno")],
                           TRUE ~ as.character(fecha))) %>% 
  mutate_at("fecha", str_to_sentence) %>% 
  rename(`Año y mes` = 1) %>% 
  as.data.frame()

if(length(colnames(cuadro.actual)) < length(colnames(diccionario$c37_cch)) ) {
  
  faltan <-
    gsub("\\.", " ",
         colnames(diccionario$c37_cch)[which(gsub("\\.", " ", colnames(diccionario$c37_cch)) %notin% colnames(cuadro.actual))])
  
  for(j in 1:length(faltan)) {
    
    ya <- rep(NA, NROW(cuadro.actual))
    
    cuadro.actual <- cbind(cuadro.actual, ya)  
    
    colnames(cuadro.actual)[which(colnames(cuadro.actual) == "ya")] <- faltan[j]
    
  }
  
}


cuadro <- diccionario$c37_cch %>% 
  rename_with(~ gsub("\\.", " ", .x)) %>% 
  rename_with(str_trim, colnames(.)) %>% 
  rbind(., cuadro.actual) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rm(cuadro.actual, faltan, ya)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 38 ----

cuadro <- datos_cch$accidentes_cch %>% 
  filter(clase == "Accdte. Ferroviario") %>%
  group_by(region,tipo) %>% 
  summarise(accidentes = n()) %>% 
  pivot_wider(names_from = tipo, values_from = accidentes) %>% 
  relocate(Colisión, .after = Atropello) %>% 
  relocate(Otros, .after = Descarrilamiento) %>% 
  regiones_accidentes() %>% 
  as.data.frame()

if(NROW(cuadro) < NROW(diccionario$regiones)) {
  
  no_estan <-
    diccionario$regiones$id[which(diccionario$regiones$id %notin% cuadro$region)] 

  for(i in 1:length(no_estan)) {
    
    nueva <- rep(NA, NCOL(cuadro))
    
    nueva[1] <- no_estan[i] 
    
    cuadro <- rbind(cuadro, nueva)
    
  }
  
}

cuadro <- ordenar_regiones(cuadro) %>% 
  nombrar_regiones() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rm(nueva, no_estan)

if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") }


# 39 ----

cuadro <- datos_cch$accidentes_cch %>% 
  filter(clase == "Accdte. Ferroviario") %>%
  group_by(region) %>% 
  summarise(`Personas ilesas` = sum(ilesos),
            `Personas lesionadas leves` = sum(leves),
            `Personas lesionadas menos graves` = sum(menos_grave),
            `Personas lesionadas graves` = sum(graves),
            `Personas fallecidas` = sum(muerto)
  ) %>% 
  regiones_accidentes() %>% 
  as.data.frame()

if(NROW(cuadro) < NROW(diccionario$regiones)) {
  
  no_estan <-
    diccionario$regiones$id[which(diccionario$regiones$id %notin% cuadro$region)] 
  
  for(i in 1:length(no_estan)) {
    
    nueva <- rep(NA, NCOL(cuadro))
    
    nueva[1] <- no_estan[i] 
    
    cuadro <- rbind(cuadro, nueva)
    
  }
  
}

cuadro <- ordenar_regiones(cuadro) %>% 
  nombrar_regiones() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  a_numeros(2:NCOL(.)) %>% 
  a_cero(2:NCOL(.)) %>% 
  as.data.frame()

rm(nueva, no_estan)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# 40 ----

cuadro <- datos_cch$personas_cch %>% 
  filter(clase == "Accdte. Ferroviario") %>%
  group_by(resultado) %>% 
  summarise(Total = n(),
            `Total hombres` = sum(sexo == "MASCULINO"),
            `Total mujeres` = sum(sexo == "FEMENINO"),
            `Hombres menores` = sum(sexo == "MASCULINO" & mayoria_edad == "< 18 años"),
            `Mujeres menores` = sum(sexo == "FEMENINO" & mayoria_edad == "< 18 años"),
            `Hombres mayores` = sum(sexo == "MASCULINO" & mayoria_edad == ">= 18 años"),
            `Mujeres mayores` = sum(sexo == "FEMENINO" & mayoria_edad == ">= 18 años"),
            `Hombres sin información` = sum(sexo == "MASCULINO" & mayoria_edad == "Sin Dato"),
            `Mujeres sin información` = sum(sexo == "FEMENINO" & mayoria_edad == "Sin Dato")
  ) %>% 
  resultados_accidente() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  as.data.frame()

las_cols <- unique(datos_cch$personas_cch$calidad)[order(unique(datos_cch$personas_cch$calidad))]

for(i in 1:length(las_cols)) {
  
  nuevo <- datos_cch$personas_cch %>% 
    filter(calidad == las_cols[i] & clase == "Accdte. Ferroviario") %>% 
    group_by(resultado) %>% 
    summarise(Total = n(),
              `Total hombres` = sum(sexo == "MASCULINO"),
              `Total mujeres` = sum(sexo == "FEMENINO"),
              `Hombres menores` = sum(sexo == "MASCULINO" & mayoria_edad == "< 18 años"),
              `Mujeres menores` = sum(sexo == "FEMENINO" & mayoria_edad == "< 18 años"),
              `Hombres mayores` = sum(sexo == "MASCULINO" & mayoria_edad == ">= 18 años"),
              `Mujeres mayores` = sum(sexo == "FEMENINO" & mayoria_edad == ">= 18 años"),
              `Hombres sin información` = sum(sexo == "MASCULINO" & mayoria_edad == "Sin Dato"),
              `Mujeres sin información` = sum(sexo == "FEMENINO" & mayoria_edad == "Sin Dato")
    ) %>% 
    resultados_accidente() %>% 
    sumar_columnas(2:NCOL(.), pos = 1) %>% 
    mutate(resultado = case_when(resultado == "Total" ~ str_to_upper(las_cols[i]),
                                 TRUE ~ as.character(resultado))) %>% 
    as.data.frame()
  
  
  
  cuadro <- rbind(cuadro, nuevo)
  
}


cuadro <- a_numeros(cuadro, 2:NCOL(cuadro)) %>% 
  mutate(`Total menores` = `Hombres menores` + `Mujeres menores`,
         `Total mayores` = `Hombres mayores` + `Mujeres mayores`,
         `Total sin información` = `Hombres sin información` + `Mujeres sin información`) %>% 
  relocate(`Total menores`, .before = `Hombres menores`) %>% 
  relocate(`Total mayores`, .before = `Hombres mayores`) %>% 
  relocate(`Total sin información`, .before = `Hombres sin información`) %>%
  rename(`Rol de la persona y tipo de consecuencia` = 1) %>% 
  as.data.frame()

rm(nuevo,i)


if(exists("validando_cch")) { a_validar(cuadro, "cch") }

if(exists("cuadros_cch")) { encuadrar(cuadro, "cch") } 


# Guardamos ----

if(!is.na(diccionario$meta$cont[which(diccionario$meta$id == "validador_cch")]))  {
  
  if(NROW(resultado_validacion) > 0 ) {
    
    cat("Guardando archivo con validaciones de CCH...\n\n")
    
    resultado_validacion <- do.call("rbind", resultado_validacion)
    
    resultado_validacion <- 
      resultado_validacion[,c("cuadro", "row", "col", "automatizacion", "control")]
    
    colnames(resultado_validacion)[2:3] <- c("fila", "columna")
    
    addWorksheet(validacion_cch, sheetName = "Validación", gridLines = T) 
    
    writeData(validacion_cch, sheet = "Validación",
              resultado_validacion, colNames = T)
    
    addStyle(validacion_cch, sheet = "Validación",
             formatos$general, rows = 1:NROW(resultado_validacion),
             cols = 1:NCOL(resultado_validacion), gridExpand = T)
    
    addStyle(validacion_cch, sheet = "Validación",
             formatos$tab_validacion, rows = 1:NROW(resultado_validacion),
             cols = 1:NCOL(resultado_validacion), gridExpand = T)
    
    addStyle(validacion_cch, sheet = "Validación",
             formatos$cabecera, rows = 1,
             cols = 1:NCOL(resultado_validacion), gridExpand = T, stack = T)
    
    if(any(resultado_validacion$col == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón" )) {
      
      las_filas <- (which(resultado_validacion$col == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón")) + 1
      
      
    } else if(any(resultado_validacion$row == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón" ))
    {
      
      las_filas <- (which(resultado_validacion$row == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón")) + 1
      
    }
    
    
    
    addStyle(validacion_cch, sheet = "Validación",
             formatos$rojo,
             rows = las_filas, cols = 1:NCOL(resultado_validacion),
             gridExpand = T, stack = T)
    
    
    
    setColWidths(validacion_cch, sheet = "Validación", cols = c(1:5),
                 widths =c(8, 8, 8, 60, 60))
    
    saveWorkbook(validacion_cch, paste0(here("output"), "/validacion_cch.xlsx"), overwrite = T)
    
    estado_cch <- "Ok"
    
  }
  
}


if(exists("cuadros_cch")) {
  
  cat("Guardando tabulados de CCH...\n\n")
  
  saveWorkbook(cuadros_cch, paste0(here("output"), "/tabulados_cch.xlsx"), overwrite = T)
  
  estado_cch <- "Ok"
  
}