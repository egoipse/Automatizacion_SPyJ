# Definiciones previas ----

cat("Creando los tabulados de PDI...\n\n")

if(exists("validador_pdi")) {
  
  validacion_pdi <- createWorkbook()
  
  validando_pdi <- list()
  
  resultado_validacion <- list()
  
}


if(exists("datos_pdi")) {
  
  cuadros_pdi <- createWorkbook()
  
}


num_cuadro <- 0


# 1 ----

# c1 <- tb_serie_anual("pdi", "delitos", "region", 1)

cuadro <- datos_pdi$delitos_pdi %>% 
  group_by(region) %>% 
  summarise(`2019`=n()) %>% 
  ungroup() %>% 
  ordenar_regiones() %>%  
  nombrar_regiones() %>%
  sumar_columnas(columnas = 2, pos = 1) %>% 
  as.data.frame() %>% 
  a_numeros(1:2) %>% 
  cbind(diccionario$c1_pdi, .)

if(colnames(cuadro)[NCOL(cuadro) - 1] == "Región") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 2 ----

cuadro <- datos_pdi$delitos_pdi %>%
  group_by(region, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>%   
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  ungroup() %>%
  t() %>% 
  fila1_a_colnames() %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("delito") %>% 
  a_numeros(1:NCOL(.))  %>% 
  organizar_delitos()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# c2 <- tb_delitos("pdi", "delitos", "region")


# 3 ----

cuadro <- datos_pdi$delitos_pdi %>%
  group_by(mes, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = mes, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  nombrar_col_mes()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 4 ----

cuadro <- datos_pdi$delitos_pdi %>%
  group_by(dia, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dia, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  nombrar_dia()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 5 ----

cuadro <- datos_pdi$delitos_pdi %>%
  group_by(tramo_hora, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = tramo_hora, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  mutate(a = rowSums(.[names(.)[2:3]], na.rm = TRUE)) %>% 
  select(!c(`0:01 A 4:00`, `0:01 a 4:00`)) %>% 
  rename(`20:01 a 24:00` = `20:01 a 24:00 `,
         `0:01 a 4:00` = a,
         `Sin información` = `Sin informacion`) %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

cuadro <- cuadro[,c("Código", "Familia y materias", "Total", "0:01 a 4:00","4:01 a 8:00",
                    "8:01 a 12:00", "12:01 a 16:00", "16:01 a 20:00", "20:01 a 24:00", "Sin información")]

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 6 ----

cuadro <- datos_pdi$delitos_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(dmcs) %>% 
  summarise(`2019`=n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  ordenar_dmcs_pdi() %>% 
  as.data.frame() %>% 
  a_numeros(1:2) 

total_cuadro <- sum(cuadro$`2019`)

var_cuadro <- (total_cuadro - diccionario$c6_pdi[which(diccionario$c6_pdi$DMCS == "Total"),
                                                 NCOL(diccionario$c6_pdi)] ) / diccionario$c6_pdi[which(diccionario$c6_pdi$DMCS == "Total"),
                                                                                                  NCOL(diccionario$c6_pdi)] 
cuadro <- rbind(total_cuadro, var_cuadro, cuadro) %>% 
  cbind(diccionario$c6_pdi, .) %>%  
  a_cero(2:NCOL(.))

rm(total_cuadro, var_cuadro)

if(colnames(cuadro)[NCOL(cuadro) - 1] == "DMCS") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 7 ----

cuadro <- datos_pdi$delitos_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(region, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dmcs, values_from = Delitos) %>%   
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  ungroup() %>% 
  t() %>% 
  fila1_a_colnames() %>% 
  as.data.frame()  %>% 
  tibble::rownames_to_column("dmcs") %>%   
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>% 
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 8 ----

cuadro <- datos_pdi$delitos_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(mes, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = mes, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  nombrar_col_mes() %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 9 ----

cuadro <- datos_pdi$delitos_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(dia, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dia, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  nombrar_dia() %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 10 ----

cuadro <- datos_pdi$delitos_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(tramo_hora, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = tramo_hora, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>%  
  mutate(a = rowSums(.[names(.)[2:3]], na.rm = TRUE)) %>% 
  select(!c(`0:01 A 4:00`, `0:01 a 4:00`)) %>% 
  rename(`20:01 a 24:00` = `20:01 a 24:00 `,
         `0:01 a 4:00` = a,
         `Sin información` = `Sin informacion`) %>%  
  ordenar_dmcs_pdi() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) %>% 
  as.data.frame()

cuadro <- cuadro[,c("DMCS", "Total", "0:01 a 4:00","4:01 a 8:00",
                    "8:01 a 12:00", "12:01 a 16:00", "16:01 a 20:00", "20:01 a 24:00", "Sin información")]

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 11 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  group_by(region, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>%   
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  ungroup() %>%
  t() %>% 
  fila1_a_colnames() %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("delito") %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 12 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  group_by(mes, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = mes, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  nombrar_col_mes()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 13 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  group_by(dia, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dia, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  nombrar_dia()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 14 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  group_by(tramo_hora, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = tramo_hora, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 15 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(dmcs) %>% 
  summarise(`2019`=n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  ordenar_dmcs_pdi() %>% 
  as.data.frame() %>% 
  a_numeros(1:2)

total_cuadro <- sum(cuadro$`2019`)

var_cuadro <- (total_cuadro - diccionario$c15_pdi[which(diccionario$c15_pdi$DMCS == "Total"),
                                                  NCOL(diccionario$c15_pdi)] ) / diccionario$c15_pdi[which(diccionario$c15_pdi$DMCS == "Total"),
                                                                                                     NCOL(diccionario$c15_pdi)] 
cuadro <- rbind(total_cuadro, var_cuadro, cuadro) %>% 
  cbind(diccionario$c15_pdi, .) %>%  
  a_cero(2:NCOL(.))

rm(total_cuadro, var_cuadro)

if(colnames(cuadro)[NCOL(cuadro) - 1] == "DMCS") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 16 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(region, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dmcs, values_from = Delitos) %>%   
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  ungroup() %>% 
  t() %>% 
  fila1_a_colnames() %>% 
  as.data.frame()  %>% 
  tibble::rownames_to_column("dmcs") %>%   
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>% 
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 17 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(mes, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = mes, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  nombrar_col_mes() %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 18 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(dia, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dia, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  nombrar_dia() %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 19 ----

cuadro <- datos_pdi$denuncias_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(tramo_hora, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = tramo_hora, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 20 ----

cuadro <- datos_pdi$detenides_pdi %>% 
  group_by(region) %>% 
  summarise(`2019`=n()) %>% 
  ungroup() %>% 
  ordenar_regiones() %>%  
  nombrar_regiones() %>%
  sumar_columnas(columnas = 2, pos = 1) %>% 
  as.data.frame() %>% 
  a_numeros(1:2) %>% 
  cbind(diccionario$c20_pdi, .) 


if(colnames(cuadro)[NCOL(cuadro) - 1] == "Región") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 21 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(region, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>%   
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  ungroup() %>%
  t() %>% 
  fila1_a_colnames() %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("delito") %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 22 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(mes, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = mes, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  nombrar_col_mes()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 23 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(dia, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dia, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  nombrar_dia()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 24 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(tramo_hora, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = tramo_hora, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 25 ----

# Por nacionalidad

cuadro.chilenes <- datos_pdi$detenides_pdi %>%
  filter(nacionalidad == "CHILENA") %>%
  group_by(region) %>%
  summarise(Chilenas=n() ) %>%
  ungroup() %>% 
  as.data.frame()

cuadro.extranjeres <- datos_pdi$detenides_pdi %>%
  filter(nacionalidad != "CHILENA") %>%
  group_by(region) %>%
  summarise(Extranjeras=n() ) %>%
  ungroup() %>% 
  as.data.frame()

cuadro.nacionalidad <- cbind(cuadro.chilenes, cuadro.extranjeres) %>% 
  select(-3) %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(NACIONALIDAD = Total) %>% 
  relocate(NACIONALIDAD, .after = region) %>% 
  as.data.frame()


# Por género

cuadro.genero <- datos_pdi$detenides_pdi %>%
  group_by(region,sexo) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = sexo, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(SEXO = Total) %>% 
  relocate(SEXO, .after = region)  %>% 
  as.data.frame()


# Por tramo etario

cuadro.tramo <- datos_pdi$detenides_pdi %>%
  group_by(region,edad) %>%
  mutate(edad = cut(edad, breaks = c(-Inf,15,17,20,30,40,50,Inf),
                    labels=c("Menos de 16", "16 a 17", "18 a 20", "21 a 30", "31 a 40", "41 a 50", "51 o más")
  ) 
  ) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = edad, values_from = PersonasDetenidas) %>%
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(EDAD = Total) %>% 
  relocate(EDAD, .after = region)  %>% 
  as.data.frame()

# Por Estado civil

cuadro.estado <- datos_pdi$detenides_pdi %>%
  group_by(region,est_civil) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = est_civil, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>%  
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(`ESTADO CIVIL` = Total) %>% 
  a_cero(2:NCOL(.))

if(any(colnames(cuadro.estado) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))) {
  colnames(cuadro.estado)[which(colnames(cuadro.estado) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))] <- "ANULADAS"
  cuadro.estado <- cuadro.estado[,c("region", "ESTADO CIVIL", "CASADO/A", "CASAD.C/CONVIVIENTE", "SOLTERO/A", "CONVIVIENTE CIVIL", "SEPARADO/A", "SEPARACION JUDICIAL", "ANULADAS", "VIUDO/A", "VIUDO C./CONVIVIENTE", "DIVORCIADO/A", "NO INDICA")]
  colnames(cuadro.estado)[3:NCOL(cuadro.estado)] <- c("Casadas", "Casadas con conviviente", "Solteras", "Conviviente civil", "Separadas", "Separación judicial", "Anuladas", "Viudas", "Viudas con conviviente", "Divorciadas", "Sin información")
} else {  
  cuadro.estado <- cuadro.estado[,c("region", "ESTADO CIVIL", "CASADO/A", "CASAD.C/CONVIVIENTE", "SOLTERO/A", "CONVIVIENTE CIVIL", "SEPARADO/A", "SEPARACION JUDICIAL", "VIUDO/A", "VIUDO C./CONVIVIENTE", "DIVORCIADO/A", "NO INDICA")]
  colnames(cuadro.estado)[3:NCOL(cuadro.estado)] <- c("Casadas", "Casadas con conviviente", "Solteras", "Conviviente civil", "Separadas", "Separación judicial", "Viudas", "Viudas con conviviente", "Divorciadas", "Sin información")
}


# Por Actividad

cuadro.actividad <- datos_pdi$detenides_pdi %>%
  group_by(region,profesion) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = profesion, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(ACTIVIDAD = Total) %>% 
  .[ , c("region", "ACTIVIDAD", "OBREROS", "EMPLEADOS", "PROFESIONALES", "TECNICO", "ESTUDIANTES", "DUEÑA DE CASA", "COMERCIANTES", "ASESORAS DE HOGAR", "CHOFERES", "JUBILADOS", "SE IGNORA")] %>% 
  as.data.frame()

colnames(cuadro.actividad)[3:NCOL(cuadro.actividad)] <- c("Obreros", "Empleados", "Profesionales", "Técnicos", "Estudiantes", "Dueñas de casa", "Comerciantes", "Asesoras del hogar", "Choferes", "Jubilados", "Sin información")


# Por Educación

cuadro.educacion <- datos_pdi$detenides_pdi %>%
  group_by(region,escolaridad) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = escolaridad, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(`NIVEL DE EDUCACIÓN` = Total)  %>% 
  .[ , c("region", "NIVEL DE EDUCACIÓN", "ANALFABETO", "BASICOS", "MEDIOS", "TECNICOS", "UNIVERSITARIOS", "ESTUDIOS DE POST GRADO", "OTROS", "SIN INFORMACION")] %>% 
  as.data.frame()

colnames(cuadro.educacion)[3:NCOL(cuadro.educacion)] <- c("Analfabeto", "Básica", "Media", "Técnica", "Universitaria o superior", "Estudios postgrado", "Otros",  "Sin información")

# Unimos todas las tablas

cuadro <- cbind(cuadro.nacionalidad,cuadro.genero,cuadro.tramo,cuadro.estado,cuadro.actividad,cuadro.educacion) %>% 
  a_cero(2:NCOL(.))

colnames(cuadro)[1] <- "Región"

cuadro <- cuadro[, -(which(grepl("region", colnames(cuadro))))]

colnames(cuadro)[1] <- "region"

cuadro <- cuadro %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  t()  %>% 
  as.data.frame() %>% 
  fila1_a_colnames()  %>%
  a_numeros(1:NCOL(.))  %>%
  tibble::rownames_to_column("Variable")  

cuadro[which(cuadro$Variable %in% c("Sin información.1", "Sin información.2")), "Variable"] <- "Sin información"

rm(cuadro.actividad,cuadro.chilenes,cuadro.educacion,cuadro.estado,cuadro.extranjeres,cuadro.genero,cuadro.nacionalidad,cuadro.tramo)

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 26 ----

# Hombres

cuadro.hombres <- datos_pdi$detenides_pdi %>% 
  filter(sexo == "Hombres") %>%
  agregar_familias() %>% 
  group_by(region, familia) %>% 
  summarise(Delitos=n())  %>%   
  pivot_wider(names_from = familia, values_from = Delitos)  %>% 
  ordenar_regiones()  %>% 
  ungroup() %>% 
  as.list() %>% 
  .[names(orden_familias_delitos)] %>% 
  do.call("rbind", .) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Familias") %>% 
  a_numeros(2:NCOL(.)) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) 

cuadro.hombres[,1] <-  str_to_sentence(cuadro.hombres[,1])

cuadro.hombres[1,"Familias"] <- "TOTAL HOMBRES"


# Mujeres

cuadro.mujeres <- datos_pdi$detenides_pdi %>% 
  filter(sexo == "Mujeres") %>%
  agregar_familias() %>% 
  group_by(region, familia) %>% 
  summarise(Delitos=n()) %>%   
  pivot_wider(names_from = familia, values_from = Delitos) %>%  
  ordenar_regiones() %>% 
  ungroup() %>%
  as.list() %>% 
  .[names(orden_familias_delitos)] %>% 
  do.call("rbind", .) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Familias") %>% 
  a_numeros(2:NCOL(.)) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) 

cuadro.mujeres[,1] <-  str_to_sentence(cuadro.mujeres[,1])

cuadro.mujeres[1,"Familias"] <- "TOTAL MUJERES"

# Se unen las dos tablas

cuadro <- rbind(cuadro.hombres, cuadro.mujeres) %>% 
  a_cero(2:NCOL(.))

colnames(cuadro)[3:NCOL(cuadro)] <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "Metropolitana", "O'Higgins", "Maule", "Ñuble", "Biobío", "La Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes y de la Antártica Chilena")

rm(cuadro.hombres, cuadro.mujeres)

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 27 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(edad, delito) %>% 
  mutate(edad = cut(edad, breaks = c(-Inf,15,17,20,30,40,50,Inf),
                    labels=c("Menos de 16", "16 a 17", "18 a 20", "21 a 30", "31 a 40", "41 a 50", "51 o más")
  ) 
  ) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = edad, values_from = Delitos) %>% 
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  mutate(Menores = `Menos de 16` + `16 a 17`,
         Adultos = `18 a 20` + `21 a 30` + `31 a 40` + `41 a 50` + `51 o más`
  ) %>% 
  relocate(Menores, .after = Total) %>% 
  relocate(Adultos, .after = `16 a 17`)

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 28 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(nacionalidad, delito) %>% 
  summarise(Delitos=n()) %>%
  pivot_wider(names_from = nacionalidad, values_from = Delitos) %>% 
  a_numeros(1:NCOL(.))  

cuadro[is.na(cuadro)] <- 0

cuadro <-  cuadro %>% 
  mutate(Otros = `CHINA REP. POPULAR` + `PAKISTANI` + `CUBANO(A)` + `ESTADOUNIDENSE` + `SIRIA` + `ESPAÑOL` + `SUIZA` + `FILIPINAS REP. DE` + `SERBIO` + `GUATEMALTECO` + `NEERLANDESAS ANT.` + `TURCO` + `MEXICANO(A)` + `INDIO` + `BIRMANIA` + `ISRAELI` + `PALESTINO` + `CROATA` + `KOREA` + `RUMANO` + `PARAGUAYO` + `BRASILEÑA` + `CHINA NACIONALISTA` + `ITALIANO` + `BELGA` + `URUGUAYA` + `PORTORIQUEÑO` + `CANADIENSE` + `SALVADOREÑO` + `NICARAGUENSE` + `HOLANDA` + `RUSO` + `INGLES` + `LETONIA` + `FRANCIA` + `ALEMANIA` + `AUSTRALIANO` + `JAPONES` + `INDONES` + `SUDAFRICA REP. DE`) %>% 
  select(!c("CHINA REP. POPULAR", "PAKISTANI", "CUBANO(A)", "ESTADOUNIDENSE", "SIRIA", "ESPAÑOL", "SUIZA", "FILIPINAS REP. DE", "SERBIO", "GUATEMALTECO", "NEERLANDESAS ANT.", "TURCO", "MEXICANO(A)", "INDIO", "BIRMANIA", "ISRAELI", "PALESTINO", "CROATA", "KOREA", "RUMANO", "PARAGUAYO", "BRASILEÑA", "CHINA NACIONALISTA", "ITALIANO", "BELGA", "URUGUAYA", "PORTORIQUEÑO", "CANADIENSE", "SALVADOREÑO", "NICARAGUENSE", "HOLANDA", "RUSO", "INGLES", "LETONIA", "FRANCIA", "ALEMANIA", "AUSTRALIANO", "JAPONES", "INDONES", "SUDAFRICA REP. DE")) %>% 
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

colnames(cuadro)[4:(NCOL(cuadro)-1)] <- c("Argentina", "Boliviana", "Chilena", "Colombiana", "Dominicana", "Ecuatoriana", "Haitiana", "Peruana", "Venezolana")

cuadro <- cuadro %>% 
  mutate(Extranjera = Total - Chilena) %>% 
  .[, c("Código", "Familia y materias", "Total", "Chilena", "Extranjera", "Haitiana", "Argentina", "Colombiana", "Boliviana", "Peruana", "Ecuatoriana", "Venezolana", "Dominicana", "Otros")]

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 29 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(est_civil, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = est_civil, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

if(any(colnames(cuadro) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))) {
  colnames(cuadro)[which(colnames(cuadro) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))] <- "ANULADAS"
} else {  
  cuadro$ANULADAS <- rep(0, NROW(cuadro))
}

cuadro <- cuadro[,c("Código", "Familia y materias", "Total", "CASADO/A", "CASAD.C/CONVIVIENTE", "SOLTERO/A", "CONVIVIENTE CIVIL", "SEPARADO/A", "SEPARACION JUDICIAL", "ANULADAS", "VIUDO/A", "VIUDO C./CONVIVIENTE", "DIVORCIADO/A", "NO INDICA")]

colnames(cuadro)[4:NCOL(cuadro)] <- c("Casado/a", "Casadas con conviviente", "Solteras", "Conviviente civil", "Separadas", "Separación judicial", "Anuladas", "Viudo/a", "Viudas con conviviente", "Divorciadas", "Sin información")

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 30 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(profesion, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = profesion, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  .[,c("Código", "Familia y materias", "Total", "OBREROS", "EMPLEADOS", "PROFESIONALES", "TECNICO", "ESTUDIANTES", "DUEÑA DE CASA", "COMERCIANTES", "ASESORAS DE HOGAR", "CHOFERES", "JUBILADOS", "SE IGNORA")]

colnames(cuadro)[which(colnames(cuadro) %in% c("TECNICO", "ASESORAS DE HOGAR"))] <-
  c("TÉCNICO", "ASESORAS DEL HOGAR") 

colnames(cuadro) <- str_to_sentence(colnames(cuadro))

colnames(cuadro)[which(colnames(cuadro) == "Se ignora")] <- "Sin información"

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 31 ----

cuadro <- datos_pdi$detenides_pdi %>%
  group_by(escolaridad, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = escolaridad, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()  %>%
  mutate(`Sin información` = OTROS + `SIN INFORMACION`) %>% 
  select(!(c("OTROS", "SIN INFORMACION"))) %>% 
  .[,c("Código", "Familia y materias", "Total", "ANALFABETO", "BASICOS", "MEDIOS", "TECNICOS", "UNIVERSITARIOS", "ESTUDIOS DE POST GRADO", "Sin información")]

colnames(cuadro)[4:NCOL(cuadro)] <- c("Analfabeto", "Básica", "Media", "Técnica", "Universitaria o superior", "Posgrado", "Sin información")

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 32 ----

cuadro <- datos_pdi$detenides_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(dmcs) %>% 
  summarise(`2019`=n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  ordenar_dmcs_pdi() %>% 
  sumar_columnas(columnas = 2, pos = 1) %>% 
  as.data.frame() %>% 
  a_numeros(1:2) %>% 
  cbind(diccionario$c32_pdi, .)

if(colnames(cuadro)[NCOL(cuadro) - 1] == "DMCS") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 33 ----

cuadro <- datos_pdi$detenides_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(region, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dmcs, values_from = Delitos) %>%   
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  ungroup() %>% 
  t() %>% 
  fila1_a_colnames() %>% 
  as.data.frame()  %>% 
  tibble::rownames_to_column("dmcs") %>%   
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>% 
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 34 ----

cuadro <- datos_pdi$detenides_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(mes, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = mes, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  nombrar_col_mes() %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 35 ----

cuadro <- datos_pdi$detenides_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(dia, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = dia, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  nombrar_dia() %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 36 ----

cuadro <- datos_pdi$detenides_pdi %>%
  filter(dmcs %notin% c("OTROS")) %>% 
  group_by(tramo_hora, dmcs) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = tramo_hora, values_from = Delitos) %>%   
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  ordenar_dmcs_pdi() %>%
  sumar_columnas(columnas = 2:NCOL(.), pos = 1) %>% 
  sumar_filas(columnas = 2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.))

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 37 ----

cuadro <- datos_pdi$victimas_pdi %>% 
  group_by(region) %>% 
  summarise(`2019`=n()) %>% 
  ungroup() %>% 
  ordenar_regiones() %>%  
  nombrar_regiones() %>%
  sumar_columnas(columnas = 2, pos = 1) %>% 
  as.data.frame() %>%
  a_numeros(1:2) %>% 
  cbind(diccionario$c37_pdi, .)

if(colnames(cuadro)[NCOL(cuadro) - 1] == "Región") {
  cuadro[,NCOL(cuadro) - 1] <- NULL
}

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 38 ----

# Por nacionalidad

cuadro.chilenes <- datos_pdi$victimas_pdi %>%
  filter(nacionalidad == "CHILENA") %>%
  group_by(region) %>%
  summarise(Chilena=n() ) %>%
  ungroup() %>% 
  as.data.frame()

cuadro.extranjeres <- datos_pdi$victimas_pdi %>%
  filter(nacionalidad != "CHILENA") %>%
  group_by(region) %>%
  summarise(Extranjera=n() ) %>%
  ungroup() %>% 
  as.data.frame()

cuadro.nacionalidad <- cbind(cuadro.chilenes, cuadro.extranjeres) %>% 
  select(-3) %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(NACIONALIDAD = Total) %>% 
  relocate(NACIONALIDAD, .after = region) %>% 
  as.data.frame()


# Por género

cuadro.genero <- datos_pdi$victimas_pdi %>%
  group_by(region,sexo) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = sexo, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(SEXO = Total) %>% 
  relocate(SEXO, .after = region)  %>% 
  as.data.frame()


# Por tramo etario

cuadro.tramo <- datos_pdi$victimas_pdi %>%
  group_by(region,edad) %>%
  mutate(edad = cut(edad, breaks = c(-Inf,15,17,20,30,40,50,Inf),
                    labels=c("Menos de 16", "16 a 17", "18 a 20", "21 a 30", "31 a 40", "41 a 50", "51 o más")
  ) 
  ) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = edad, values_from = PersonasDetenidas) %>%
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(EDAD = Total) %>% 
  relocate(EDAD, .after = region)  %>% 
  as.data.frame()

# Por Estado civil

cuadro.estado <- datos_pdi$victimas_pdi %>%
  group_by(region,est_civil) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = est_civil, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>%  
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(`ESTADO CIVIL` = Total) %>% 
  a_cero(2:NCOL(.))

if(any(colnames(cuadro.estado) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))) {
  colnames(cuadro.estado)[which(colnames(cuadro.estado) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))] <- "ANULADAS"
  cuadro.estado <- cuadro.estado[,c("region", "ESTADO CIVIL", "CASADO/A", "CASAD.C/CONVIVIENTE", "SOLTERO/A", "CONVIVIENTE CIVIL", "SEPARADO/A", "SEPARACION JUDICIAL", "ANULADAS", "VIUDO/A", "VIUDO C./CONVIVIENTE", "DIVORCIADO/A", "NO INDICA")]
  colnames(cuadro.estado)[3:NCOL(cuadro.estado)] <- c("Casadas", "Casadas con conviviente", "Solteras", "Conviviente civil", "Separadas", "Separación judicial", "Anuladas", "Viudas", "Viudas con conviviente", "Divorciadas", "Sin información")
} else {  
  cuadro.estado <- cuadro.estado[,c("region", "ESTADO CIVIL", "CASADO/A", "CASAD.C/CONVIVIENTE", "SOLTERO/A", "CONVIVIENTE CIVIL", "SEPARADO/A", "SEPARACION JUDICIAL", "VIUDO/A", "VIUDO C./CONVIVIENTE", "DIVORCIADO/A", "NO INDICA")]
  colnames(cuadro.estado)[3:NCOL(cuadro.estado)] <- c("Casadas", "Casadas con conviviente", "Solteras", "Conviviente civil", "Separadas", "Separación judicial", "Viudas", "Viudas con conviviente", "Divorciadas", "Sin información")
}


# Por Actividad

cuadro.actividad <- datos_pdi$victimas_pdi %>%
  group_by(region,profesion) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = profesion, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  rename(ACTIVIDAD = Total) %>% 
  .[ , c("region", "ACTIVIDAD", "OBREROS", "EMPLEADOS", "PROFESIONALES", "TECNICO", "ESTUDIANTES", "DUEÑA DE CASA", "COMERCIANTES", "ASESORAS DE HOGAR", "CHOFERES", "JUBILADOS", "SE IGNORA")] %>% 
  as.data.frame()

colnames(cuadro.actividad)[3:NCOL(cuadro.actividad)] <- c("Obreros", "Empleados", "Profesionales", "Técnicos", "Estudiantes", "Dueñas de casa", "Comerciantes", "Asesoras del hogar", "Choferes", "Jubilados", "Sin información")


# Por Educación

cuadro.educacion <- datos_pdi$victimas_pdi %>%
  group_by(region,escolaridad) %>%
  summarise(PersonasDetenidas=n()) %>%
  pivot_wider(names_from = escolaridad, values_from = PersonasDetenidas) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.)) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  mutate(`Sin información` = `0` + `SE IGNORA`) %>% 
  select(!(c("0", "SE IGNORA"))) %>% 
  rename(`NIVEL DE EDUCACIÓN` = Total) %>%   
  .[ , c("region", "NIVEL DE EDUCACIÓN", "ANALFABETO", "BASICOS", "MEDIOS", "TECNICOS", "UNIVERSITARIOS", "ESTUDIOS DE POST GRADO", "OTROS", "Sin información")] %>% 
  as.data.frame()

colnames(cuadro.educacion)[3:NCOL(cuadro.educacion)] <- c("Analfabeto", "Básica", "Media", "Técnica", "Universitaria o superior", "Otros", "Estudios postgrado", "Sin información")

# Unimos todas las tablas

cuadro <- cbind(cuadro.nacionalidad,cuadro.genero,cuadro.tramo,cuadro.estado,cuadro.actividad,cuadro.educacion) %>% 
  a_cero(2:NCOL(.))

colnames(cuadro)[1] <- "Región"

cuadro <- cuadro[, -(which(grepl("region", colnames(cuadro))))]

colnames(cuadro)[1] <- "region"

cuadro <- cuadro %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  t()  %>% 
  as.data.frame() %>% 
  fila1_a_colnames()  %>%
  a_numeros(1:NCOL(.))  %>%
  tibble::rownames_to_column("Variable")  

cuadro[which(cuadro$Variable %in% c("Sin información.1", "Sin información.2")), "Variable"] <- "Sin información"

rm(cuadro.actividad,cuadro.chilenes,cuadro.educacion,cuadro.estado,cuadro.extranjeres,cuadro.genero,cuadro.nacionalidad,cuadro.tramo)

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 39 ----

cuadro <- datos_pdi$victimas_pdi %>%
  group_by(region, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>%   
  ordenar_regiones() %>% 
  nombrar_regiones() %>%
  ungroup() %>%
  t() %>% 
  fila1_a_colnames() %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("delito") %>% 
  a_numeros(1:NCOL(.))  %>% 
  organizar_delitos()

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 40 ----

# Hombres

cuadro.hombres <- datos_pdi$victimas_pdi %>% 
  filter(sexo == "Hombres") %>%
  agregar_familias() %>% 
  group_by(region, familia) %>% 
  summarise(Delitos=n()) %>%   
  pivot_wider(names_from = familia, values_from = Delitos) %>%  
  ordenar_regiones() %>% 
  ungroup() %>%
  as.list() %>% 
  .[names(orden_familias_delitos)] %>% 
  do.call("rbind", .) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Familias") %>% 
  a_numeros(2:NCOL(.)) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) 

cuadro.hombres[,1] <-  str_to_sentence(cuadro.hombres[,1])

cuadro.hombres[1,"Familias"] <- "TOTAL HOMBRES"


# Mujeres

cuadro.mujeres <- datos_pdi$victimas_pdi %>% 
  filter(sexo == "Mujeres") %>%
  agregar_familias() %>% 
  group_by(region, familia) %>% 
  summarise(Delitos=n()) %>%   
  pivot_wider(names_from = familia, values_from = Delitos) %>%  
  ordenar_regiones() %>% 
  ungroup() %>%
  as.list() %>% 
  .[names(orden_familias_delitos)] %>% 
  do.call("rbind", .) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Familias") %>% 
  a_numeros(2:NCOL(.)) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.), pos = 1) 

cuadro.mujeres[,1] <-  str_to_sentence(cuadro.mujeres[,1])

cuadro.mujeres[1,"Familias"] <- "TOTAL MUJERES"

# Se unen las dos tablas

cuadro <- rbind(cuadro.hombres, cuadro.mujeres) %>% 
  a_cero(2:NCOL(.))

colnames(cuadro)[3:NCOL(cuadro)] <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "Metropolitana", "O'Higgins", "Maule", "Ñuble", "Biobío", "La Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes y de la Antártica Chilena")

rm(cuadro.hombres, cuadro.mujeres)

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 41 ----

# Por edad

cuadro.edad <- datos_pdi$victimas_pdi %>%
  group_by(edad, delito) %>% 
  mutate(edad = cut(edad, breaks = c(-Inf,15,17,20,30,40,50,Inf),
                    labels=c("Menos de 16", "16 a 17", "18 a 20", "21 a 30", "31 a 40", "41 a 50", "51 o más")
  ) 
  ) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = edad, values_from = Delitos) %>% 
  ungroup() %>%
  as.data.frame() 

# Por nacionalidad 

cuadro.chilenes <- datos_pdi$victimas_pdi %>%
  filter(nacionalidad == "CHILENA") %>%
  group_by(nacionalidad, delito) %>%
  summarise(Chilenas=n() ) %>% 
  as.data.frame() %>%
  select(delito, Chilenas) %>% 
  a_numeros(2:NCOL(.)) %>% 
  rename(`Nacionalidad: Chilena` = Chilenas)


cuadro.extrangeres <- datos_pdi$victimas_pdi %>%
  filter(nacionalidad != "CHILENA") %>%
  group_by(nacionalidad, delito) %>%
  summarise(Delitos=n() )  %>%
  pivot_wider(names_from = nacionalidad, values_from = Delitos) %>% 
  sumar_filas(2:NCOL(.))  %>% 
  select(c(delito, Total)) %>% 
  rename(`Nacionalidad: Extranjera` = Total)

# Unimos todo

cuadro <- Reduce(function(x,y) base::merge(x = x, y = y, by = "delito", all = TRUE),
                 list(cuadro.edad, cuadro.chilenes, cuadro.extrangeres)) %>% 
  a_numeros(2:NCOL(.)) %>% 
  organizar_delitos() %>% 
  select(-Total) %>% 
  mutate(`Edad Legal: Menores` = `Menos de 16` + `16 a 17`,
         `Edad Legal: Adultos` = `18 a 20` + `21 a 30` + `31 a 40` + `41 a 50` + `51 o más`,
         `Total tramos` = `Menos de 16` + `16 a 17` + `18 a 20` + `21 a 30` + `31 a 40` + `41 a 50` + `51 o más`,
         `Total edades` = `Edad Legal: Menores` + `Edad Legal: Adultos`,
         `Total Nacionalidades` = `Nacionalidad: Chilena` + `Nacionalidad: Extranjera`
  ) %>% 
  relocate(`Total edades`, .after = `Familia y materias`) %>% 
  relocate(`Edad Legal: Menores`, .after = `Total edades`) %>% 
  relocate(`Edad Legal: Adultos`, .before = `Menos de 16`) %>% 
  relocate(`Total tramos`, .after = `Edad Legal: Adultos`) %>%
  relocate(`Total Nacionalidades`, .before = `Nacionalidad: Chilena`)

colnames(cuadro)[which(colnames(cuadro) %in% c("Total tramos", "Total edades", "Total Nacionalidades"))] <- c("Total", "Total", "Total") 

rm(cuadro.chilenes, cuadro.extrangeres, cuadro.edad)

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 42 ----

cuadro <- datos_pdi$victimas_pdi %>%
  group_by(est_civil, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = est_civil, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()

if(any(colnames(cuadro) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))) {
  colnames(cuadro)[which(colnames(cuadro) %in% c("ANULADO/A", "ANULADO", "ANULADA", "ANULADOS", "ANULADAS"))] <- "ANULADAS"
} else {  
  cuadro$ANULADAS <- rep(0, NROW(cuadro))
}

cuadro <- cuadro[,c("Código", "Familia y materias", "Total", "CASADO/A", "CASAD.C/CONVIVIENTE", "SOLTERO/A", "CONVIVIENTE CIVIL", "SEPARACION JUDICIAL", "SEPARADO/A", "ANULADAS", "VIUDO C./CONVIVIENTE", "VIUDO/A", "DIVORCIADO/A", "NO INDICA")]

colnames(cuadro)[4:NCOL(cuadro)] <- c("Casadas", "Casadas con conviviente", "Total solteras", "Total conviviente civil", "Separación judicial", "Separadas", "Total anuladas", "Viudas con conviviente", "Viudas", "Total divorciadas", "Sin información")

cuadro <- cuadro %>% 
  mutate(`Total casadas` = `Casadas` + `Casadas con conviviente`,
         `Total separadas` = `Separadas` + `Separación judicial`, 
         `Total viudas` = `Viudas con conviviente` + `Viudas`) %>% 
  relocate(`Total casadas`, .after = Total) %>% 
  relocate(`Total separadas`, .after = `Total conviviente civil`) %>% 
  relocate(`Total viudas`, .after = `Total anuladas`)

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 43 ----

cuadro <- datos_pdi$victimas_pdi %>%
  group_by(escolaridad, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = escolaridad, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos()  %>% 
  mutate(`Sin información` = `0` + `SE IGNORA`) %>% 
  select(!(c("0", "SE IGNORA"))) %>% 
  .[,c("Código", "Familia y materias", "Total", "ANALFABETO", "BASICOS", "MEDIOS", "TECNICOS", "UNIVERSITARIOS", "ESTUDIOS DE POST GRADO", "OTROS", "Sin información")]

colnames(cuadro)[4:NCOL(cuadro)] <- c("Analfabeto", "Básica", "Media", "Técnica", "Universitaria o superior", "Posgrado", "Otros", "Sin información")

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# 44 ----

cuadro <- datos_pdi$victimas_pdi %>%
  group_by(profesion, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = profesion, values_from = Delitos) %>%   
  ungroup() %>%
  as.data.frame() %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() 

colnames(cuadro) <- c("Código", "Familia y materias", "Total", "Asesoras del hogar", "Choferes", "Comerciantes", "Dueña de casa", "Empleados", "Estudiantes", "Jubilados", "Obreros", "Profesionales", "Se ignora", "Técnico")

columnas <- colnames(cuadro[which(names(cuadro) %notin% c("Técnico", "Se ignora"))])

cuadro <- cuadro[,c(columnas, "Técnico", "Se ignora")]

if(exists("validando_pdi")) { a_validar(cuadro, "pdi") }

if(exists("cuadros_pdi")) { encuadrar(cuadro, "pdi") }


# Guardamos ----

if(exists("resultado_validacion")) {
  
   if(NROW(resultado_validacion) > 0 ) {
    
     cat("Guardando archivo con validaciones de PDI...\n\n")
 
     resultado_validacion <- do.call("rbind", resultado_validacion)
     
     resultado_validacion <- 
       resultado_validacion[,c("cuadro", "row", "col", "automatizacion", "control")]
     
     colnames(resultado_validacion)[2:3] <- c("fila", "columna")
     
     addWorksheet(validacion_pdi, sheetName = "Validación", gridLines = T) 
     
     writeData(validacion_pdi, sheet = "Validación",
               resultado_validacion, colNames = T)
     
     addStyle(validacion_pdi, sheet = "Validación",
              formatos$general, rows = 1:NROW(resultado_validacion),
              cols = 1:NCOL(resultado_validacion), gridExpand = T)
     
     addStyle(validacion_pdi, sheet = "Validación",
              formatos$tab_validacion, rows = 1:NROW(resultado_validacion),
              cols = 1:NCOL(resultado_validacion), gridExpand = T)
     
     addStyle(validacion_pdi, sheet = "Validación",
              formatos$cabecera, rows = 1,
              cols = 1:NCOL(resultado_validacion), gridExpand = T, stack = T)
     
     if(any(resultado_validacion$col == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón" )) {
       
       las_filas <- (which(resultado_validacion$col == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón")) + 1
         
     
     } else if(any(resultado_validacion$row == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón" ))
      {
       
       las_filas <- (which(resultado_validacion$row == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón")) + 1
       
     }
     
     
     
     addStyle(validacion_pdi, sheet = "Validación",
              formatos$rojo,
              rows = las_filas, cols = 1:NCOL(resultado_validacion),
              gridExpand = T, stack = T)
              
     
     
     setColWidths(validacion_pdi, sheet = "Validación", cols = c(1:5),
                  widths =c(8, 8, 8, 60, 60))
     
     saveWorkbook(validacion_pdi, paste0(here("output"), "/validacion_pdi.xlsx"), overwrite = T)
  
      estado_pdi <- "Ok"
  
  }
  
}


if(exists("cuadros_pdi")) {
  
  cat("Guardando tabulados de PDI...\n\n")
  
  saveWorkbook(cuadros_pdi, paste0(here("output"), "/tabulados_pdi.xlsx"), overwrite = T)
  
  estado_pdi <- "Ok"
  
}