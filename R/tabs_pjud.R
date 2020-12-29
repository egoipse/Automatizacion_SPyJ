# Definiciones previas ----

cat("Creando los tabulados de Justicia...\n\n")

if(exists("validador_pjud")) {
  
  validacion_pjud<- createWorkbook()
  
  validando_pjud <- list()
  
  resultado_validacion <- list()
  
}


if(!is.na(diccionario$meta$cont[which(diccionario$meta$id == "procesar_pjud")])) {
  
  cuadros_pjud <- createWorkbook()
  
}


num_cuadro <- 0


# 1 ----

cuadro.ingresadas <- datos_pjud$suprema_ingresadas_pjud %>% 
  group_by(tipo_causa, subtipo_causa, competencia) %>% 
  summarise(`Causas ingresadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame()

cuadro.pendientes <- datos_pjud$suprema_pendientes_pjud %>% 
  group_by(tipo_causa, subtipo_causa, competencia) %>% 
  summarise(`Causas pendientes` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  mutate_at("competencia", ~ gsub("\\(|\\)", "",.)) %>% 
  mutate(competencia = case_when(competencia == "Laboral" ~ "Reforma Laboral",
                                 competencia == "Cobranza" ~ "Cobranza Laboral",
                                 competencia %in% c("Penal", "PENAL")  ~ "Reforma",
                                 competencia == "Crimen"  ~ "Criminal",
                                 competencia == "Policia local" ~ "Policia Local",
                                 TRUE ~ as.character(competencia)
                                 )
         )


cuadro.terminadas <- datos_pjud$suprema_terminadas_pjud %>% 
  group_by(tipo_causa, subtipo_causa, competencia) %>% 
  summarise(`Causas falladas` = n()) %>% 
  ungroup() %>% 
  as.data.frame()

cuadro <- full_join(cuadro.ingresadas, cuadro.pendientes) %>% 
  full_join(cuadro.terminadas)

cuadro1 <- cuadro %>% 
  filter(tipo_causa %notin% c("Casaciones", "Recurso de Revisión",
                         "Otras Apelaciones", "Recurso de Hecho",
                         "Recurso de Queja", "Extradiciones", "Amparo Económico")) %>% 
  group_by(tipo_causa) %>% 
  summarise(`Causas ingresadas` = sum(`Causas ingresadas`, na.rm = T),
            `Causas pendientes` = sum(`Causas pendientes`, na.rm = T),
            `Causas falladas` = sum(`Causas falladas`, na.rm = T)) %>% 
  mutate(across(where(is.character), str_to_sentence))


cuadro2 <- cuadro %>% 
  filter(tipo_causa %in% c("Casaciones", "Recurso de Revisión",
                              "Otras Apelaciones", "Recurso de Hecho",
                              "Recurso de Queja")) %>% 
  group_by(tipo_causa, subtipo_causa, competencia) %>% 
  summarise(`Causas ingresadas` = sum(`Causas ingresadas`, na.rm = T),
            `Causas pendientes` = sum(`Causas pendientes`, na.rm = T),
            `Causas falladas` = sum(`Causas falladas`, na.rm = T)) %>% 
  mutate(across(where(is.character), str_to_sentence))

cuadro3 <- cuadro %>% 
  filter(tipo_causa %in% c("Extradiciones", "Amparo Económico")) %>% 
  group_by(tipo_causa, subtipo_causa) %>% 
  summarise(`Causas ingresadas` = sum(`Causas ingresadas`, na.rm = T),
            `Causas pendientes` = sum(`Causas pendientes`, na.rm = T),
            `Causas falladas` = sum(`Causas falladas`, na.rm = T)) %>% 
  mutate(across(where(is.character), str_to_sentence))

cuadro <- full_join(cuadro1, cuadro2) %>% 
  full_join(cuadro3) %>% 
  mutate(across(1:NCOL(.), str_trim),
         competencia = case_when(competencia == "Policia local" ~ "Otros",
                                 TRUE ~ as.character(competencia)))

for(i in seq_len(NROW(cuadro))) {
  
  if(any(unlist(str_split(cuadro$subtipo_causa[i], " ")) %in% c("Fondo", "Forma", "Extradiciones", "Amparo"))) {
    
    cuadro$subtipo_causa[i] <- cuadro$subtipo_causa[i]
    
    
  } else {
    
    cuadro$subtipo_causa[i] <- NA
    
  }
  
}

cuadro <- cuadro %>% 
  mutate_at("subtipo_causa",
            ~ gsub(
              "Extradiciones |Amparo económico | Civil| Cobranza| Criminales| Familia| Reforma Laboral| Reforma Penal| Penal|Casaciones ", "", .)) %>%
  mutate(
    across(where(is.character), str_to_sentence),
    orden1 = case_when(tipo_causa == "Casaciones" ~ 1,
                            tipo_causa == "Apelaciones de protección" ~ 2,
                            tipo_causa == "Recurso de revisión" ~ 3,
                            tipo_causa == "Otras apelaciones" ~ 4,
                            tipo_causa == "Recurso de hecho" ~ 5,
                            tipo_causa == "Recurso de queja" ~ 6,
                            tipo_causa == "Recurso de amparo" ~ 7,
                            tipo_causa == "Recusaciones" ~ 8,
                            tipo_causa == "Extradiciones" ~ 9,
                            tipo_causa == "Competencias" ~ 10,
                            tipo_causa == "Exequatur / exhortos" ~ 11,
                            tipo_causa == "Acción constitucional indemnizatoria" ~ 12,
                            tipo_causa == "Reclamaciones" ~ 13,
                            tipo_causa == "Amparo económico" ~ 14,
                            tipo_causa == "Recurso de nulidad" ~ 15,
                            tipo_causa == "Unificación de jurisprudencia" ~ 16,
                            tipo_causa == "Otros recursos" ~ 17
                            ),
         orden2 = case_when(subtipo_causa == "Fondo" ~ 1,
                            subtipo_causa == "Forma" ~ 2,
                            subtipo_causa == "Fondo y Forma" ~ 3),
         orden3 = case_when(competencia == "Civil" ~ 1,
                            competencia == "Criminal" ~ 2,
                            competencia == "Reforma" ~ 3,
                            competencia == "Familia" ~ 4,
                            competencia == "Cobranza laboral" ~ 5,
                            competencia == "Reforma laboral" ~ 6,
                            competencia == "Otros" ~ 7)
         )  %>% 
  arrange(as.numeric(orden1), as.numeric(orden2), as.numeric(orden3)) %>% 
  select(!c(orden1, orden2, orden3)) %>% 
  mutate(competencia = case_when(competencia == "Reforma" ~ "Reforma penal",
                                 TRUE ~ as.character(competencia))) %>% 
  sumar_columnas(2:4, pos = 1) %>% 
  relocate(subtipo_causa, .after = tipo_causa) %>% 
  relocate(competencia, .after = subtipo_causa) %>% 
  rename(Recurso = tipo_causa,
        `Tipo de recurso` = subtipo_causa
        ) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[1:NCOL(.)])) %>% 
  a_numeros(4:6) %>% 
  as.data.frame()
  
if(any(!is.na(cuadro[1, c("Tipo de recurso", "Competencia")]))) {
  
  cuadro[1, c("Tipo de recurso", "Competencia")] <- NA
  
}

filas <- vector()


for(i in 2:NROW(cuadro)) {
  
  if(cuadro[i,1] == "Casaciones") {
     
    if(cuadro[i,3] == "Civil" ) {
    
    cuadro[i,1] <- cuadro[i,1] 
    
    } else if(cuadro[i,3] != "Civil" ) {
      
      cuadro[i,1] <- NA 
      
    }
    
  } else if(cuadro[i,1] == "Apelaciones de protección" ) {
    
    cuadro[i,1] <- cuadro[i,1] 
    
  }  else {
    
    if(cuadro[i,1] == cuadro[(i - 1), 1] | is.na(cuadro[(i - 1), 1]) ) {
    
    filas <- c(filas, i)
    
    }
    
  }
  
}

cuadro[filas, 1] <- NA

rm(cuadro.ingresadas, cuadro.pendientes, cuadro.terminadas, cuadro1, cuadro2, cuadro3, filas, i)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 2 ----

cuadro1 <- datos_pjud$ca_ingresadas_pjud %>% 
  group_by(codigo_ca) %>% 
  summarise(`Total ingresado 2019` =n()) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  ordenar_cortes()
  
cuadro2 <- datos_pjud$ca_terminadas_pjud %>% 
  group_by(codigo_ca) %>% 
  summarise(`Total fallado 2019` =n()) %>%
  ungroup() %>% 
  as.data.frame() %>% 
  ordenar_cortes()

cuadro3 <- diccionario$c2_pjud %>% 
  rename_with(~ gsub("\\.", " ", .x))


cuadro <- full_join(cuadro1, cuadro2) %>% 
  rename(`Corte de Apelaciones` = ca) %>% 
  full_join(cuadro3) %>% 
  cbind(., data_frame(`Total pendientes 2019` = rep(NA, NROW(.)))) %>% 
  relocate(`Total ingresado 2019`, .after = `Total ingresado 2018`) %>% 
  relocate(`Total fallado 2019`, .after = `Total fallado 2018`) %>% 
  relocate(`Total pendientes 2019`, .after = `Total pendientes 2018`) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.)) %>% 
  a_numeros(2:NCOL(.)) %>% 
  as.data.frame()

rm(cuadro1, cuadro2, cuadro3)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }



# 3 ----


cuadro_civil_ingresadas <- datos_pjud$civil_ingresadas_pjud %>% 
  group_by(codigo, glosa, procedimiento) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  rename(Código = codigo,
         `Procedimiento y materia` = glosa
  ) %>% 
  ungroup() %>% 
  as.data.frame()


cuadro_civil_terminadas <- datos_pjud$civil_terminadas_pjud %>% 
  filter(agno_termino == 2019) %>% 
  group_by(codigo, glosa, procedimiento, tipo_termino) %>%
  summarise(`Total causas terminadas` = n()) %>%   
  pivot_wider(names_from = tipo_termino, values_from = `Total causas terminadas`) %>% 
  rename(Código = codigo,
         `Procedimiento y materia` = glosa) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  sumar_filas(4:NCOL(.)) %>% 
  rename(`Total causas terminadas` = Total)

cuadro <- full_join(cuadro_civil_ingresadas, cuadro_civil_terminadas) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[4:NCOL(.)]) ) %>% 
  rename(`Abandono` = `Abandono proced.`,
         `Acumulación` = `Acum. En tribunal`,
         `Avenimiento` = `Avenimiento`,
         `Cheque notificado y no pagado` = `Cheque notif. Y no pagado`,
         `Cheque notificado y pagado` = `Cheque notif. Y pagado`,
         `Conciliación` = `Conciliaciones`,
         `Desistimiento` = `Desistimiento`,
         `Excepciones` = `Excepc. Por vicio insubsanable`,
         `Remisión por incompetencia` = `Incompetencia`,
         `Negativa de deuda` = `Niega deuda`,
         `Negativa de firma` = `Niega firma`,
         `No corresponde el ingreso` = `No corresponde ingreso`,
         `No da curso a la demanda` = `No da curso a la dda.`,
         `Otros motivos` = `Otros_terminos`,
         `Pago contribuciones` = `Pago_contribuciones`,
         `Retiro de la demanda` = `Retiro_demanda`,
         `Retiro de solicitud` = `Retiro_solicitud`,
         `Sentencia definitiva` = `Sentencia definitiva`,
         `Sustituciones de procedimiento` = `Sustitucion_proc`,
         `Téngase por no presentada la demanda` = `Tengase por no presen. La dda.`,
         `Crédito pagado` = `Tiene por pagado cred.`,
         `Transacción` = `Transacciones`) %>% 
  a_numeros(4:NCOL(.))
  
totales_civil <- c("", "TOTAL", colSums(cuadro[,4:NCOL(cuadro)], na.rm = T) )

cuadro <- split(cuadro, as.factor(cuadro$procedimiento))

cuadro$`PROCEDIMIENTO GESTIONES PREPARATORIAS` <-
  cuadro$`Gestiones Preparatorias y Medidas Prejudiciales`

cuadro$`PROCEDIMIENTO GESTIONES PREPARATORIAS` <-
  cuadro$`PROCEDIMIENTO GESTIONES PREPARATORIAS`[grep("M0", cuadro$`PROCEDIMIENTO GESTIONES PREPARATORIAS`$Código,
                                                      invert = T),]

cuadro$`Gestiones Preparatorias y Medidas Prejudiciales` <-
  cuadro$`Gestiones Preparatorias y Medidas Prejudiciales`[grep("M0", cuadro$`Gestiones Preparatorias y Medidas Prejudiciales`$Código),]

names(cuadro)[names(cuadro) == "Gestiones Preparatorias y Medidas Prejudiciales"] <-
  "MEDIDAS PREJUDICIALES"

names(cuadro) <- c("PROCEDIMIENTO EJECUTIVO", "MEDIDAS PREJUDICIALES",
                   "PROCEDIMIENTO ORDINARIO", "PROCEDIMIENTO PARTICULAR",
                   "PROCEDIMIENTO CONCURSAL", "PROCEDIMIENTO QUIEBRAS",
                   "PROCEDIMIENTO SUMARIO", "VIOLENCIA INTRAFAMILIAR",
                   "PROCEDIMIENTO VOLUNTARIO",
                   "PROCEDIMIENTO GESTIONES PREPARATORIAS")


for(i in seq_len(length(cuadro))) {
  
  cuadro[[i]] <- cuadro[[i]][order(cuadro[[i]][,1]), ] 
  
  cuadro[[i]] <- rbind(c("",str_to_upper(names(cuadro)[[i]]),"",colSums(cuadro[[i]][,4:NCOL(cuadro[[i]])], na.rm = T) ), cuadro[[i]])
  
}

cuadro <- cuadro[names(list(`MEDIDAS PREJUDICIALES` = c(),
                            `PROCEDIMIENTO CONCURSAL` = c(),
                            `PROCEDIMIENTO EJECUTIVO` = c(),
                            `PROCEDIMIENTO GESTIONES PREPARATORIAS` = c(),
                            `PROCEDIMIENTO ORDINARIO` = c(),
                            `PROCEDIMIENTO PARTICULAR` = c(),
                            `PROCEDIMIENTO QUIEBRAS` = c(),
                            `PROCEDIMIENTO SUMARIO` = c(),
                            `PROCEDIMIENTO VOLUNTARIO` =c(),
                            `VIOLENCIA INTRAFAMILIAR` = c()
)
)]


  
cuadro <- do.call("rbind", cuadro) %>% 
  select(-procedimiento) %>% 
  rbind(totales_civil,.) %>%
  a_numeros(3:NCOL(.)) %>% 
  relocate(`Total causas ingresadas`, .before = Conciliación) %>% 
  relocate(`Total causas terminadas`, .before = Conciliación) %>% 
  a_cero(3:NCOL(.)) %>% 
  .[,c("Código", "Procedimiento y materia",
      "Total causas ingresadas", "Total causas terminadas",
      "Sentencia definitiva", "Avenimiento", "Transacción",
      "Conciliación", "Desistimiento", "Abandono",
      "Crédito pagado", "Acumulación", "Retiro de la demanda",
      "Remisión por incompetencia", "No da curso a la demanda",
      "Negativa de deuda", "Negativa de firma",
      "Cheque notificado y pagado",
      "Cheque notificado y no pagado", "Otros motivos",
      "Pago contribuciones", "Excepciones",
      "Téngase por no presentada la demanda",
      "Retiro de solicitud", "No corresponde el ingreso",
      "Sustituciones de procedimiento")] %>% 
  mutate_at("Procedimiento y materia", str_trim)

rownames(cuadro) <- c()

rm(cuadro_civil_ingresadas, cuadro_civil_terminadas, totales_civil, i)

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 4 ----

cuadro <- datos_pjud$penal_ingresadas_pjud %>%
  group_by(codigo_ca, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>% 
  ordenar_cortes() %>% 
  t() %>%  
  as.data.frame() %>% 
  tibble::rownames_to_column("ca") %>% 
  fila1_a_colnames() %>% 
  rename(delito = ca ) %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() 


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 5 ----

cuadro <- datos_pjud$penal_terminadas_pjud %>%
  group_by(codigo_ca, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>% 
  ordenar_cortes() %>% 
  t() %>%  
  as.data.frame() %>% 
  tibble::rownames_to_column("ca") %>% 
  fila1_a_colnames() %>% 
  rename(delito = ca ) %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() 


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 6 ----

cuadro <- datos_pjud$penal_ingresadas_pjud %>%
  group_by(region, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>% 
  asignar_regiones() %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>% 
  ungroup() %>%
  t()  %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Región") %>% 
  fila1_a_colnames() %>% 
  rename(delito = Región) %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() 

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 7 ----

cuadro <- datos_pjud$penal_terminadas_pjud %>%
  group_by(region, delito) %>% 
  summarise(Delitos=n()) %>% 
  pivot_wider(names_from = delito, values_from = Delitos) %>% 
  asignar_regiones() %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>% 
  ungroup() %>%
  t()  %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Región") %>% 
  fila1_a_colnames() %>% 
  rename(delito = Región) %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() 

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 8 ----

cuadro1 <- datos_pjud$penal_ingresadas_pjud %>% 
  group_by(region) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones()

cuadro2 <- datos_pjud$penal_terminadas_pjud %>% 
  group_by(region) %>% 
  summarise(`Total causas terminadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones()

cuadro3 <- datos_pjud$penal_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE GARANTÍA") %>% 
  group_by(region) %>% 
  summarise(`Ingresos en juzgados de garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones()


cuadro4 <- datos_pjud$penal_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE GARANTÍA") %>% 
  group_by(region) %>% 
  summarise(`Términos en juzgados de garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones()
  

cuadro5 <- datos_pjud$penal_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(region) %>% 
  summarise(`Ingresos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones()


cuadro6 <- datos_pjud$penal_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(region) %>% 
  summarise(`Términos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones() 


cuadro7 <- datos_pjud$penal_ingresadas_pjud %>% 
  filter(tipo_tribunal == "TRIBUNALES DE JUICIO ORAL EN LO PENAL") %>% 
  group_by(region) %>% 
  summarise(`Ingresos en tribunales de juicio oral en lo penal` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones()


cuadro8 <- datos_pjud$penal_terminadas_pjud %>% 
  filter(tipo_tribunal == "TRIBUNALES DE JUICIO ORAL EN LO PENAL") %>% 
  group_by(region) %>% 
  summarise(`Términos en tribunales de juicio oral en lo penal` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  asignar_regiones()


cuadro <- full_join(cuadro1, cuadro2) %>% 
  full_join(cuadro3) %>% 
  full_join(cuadro4) %>%
  full_join(cuadro5) %>%
  full_join(cuadro6) %>%
  full_join(cuadro7) %>%
  full_join(cuadro8) %>%
  a_numeros(1:NCOL(.)) %>% 
  ordenar_regiones() %>% 
  nombrar_regiones() %>% 
  ungroup() %>%
  as.data.frame() %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.)) %>% 
  a_numeros(2:NCOL(.))

rm(cuadro1, cuadro2, cuadro3, cuadro4, cuadro5, cuadro6, cuadro7, cuadro8)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 9 ----

cuadro1 <- datos_pjud$penal_ingresadas_pjud %>% 
  group_by(delito) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() 

cuadro2 <- datos_pjud$penal_terminadas_pjud %>% 
  group_by(delito) %>% 
  summarise(`Total causas terminadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() 

cuadro3 <- datos_pjud$penal_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE GARANTÍA") %>% 
  group_by(delito) %>% 
  summarise(`Ingresos en juzgados de garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() 

cuadro4 <- datos_pjud$penal_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE GARANTÍA") %>% 
  group_by(delito) %>% 
  summarise(`Términos en juzgados de garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() 

cuadro5 <- datos_pjud$penal_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(delito) %>% 
  summarise(`Ingresos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() 

cuadro6 <- datos_pjud$penal_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(delito) %>% 
  summarise(`Términos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() 

cuadro7 <- datos_pjud$penal_ingresadas_pjud %>% 
  filter(tipo_tribunal == "TRIBUNALES DE JUICIO ORAL EN LO PENAL") %>% 
  group_by(delito) %>% 
  summarise(`Ingresos en tribunales de juicio oral en lo penal` = n()) %>% 
  ungroup() %>% 
  as.data.frame() 

cuadro8 <- datos_pjud$penal_terminadas_pjud %>% 
  filter(tipo_tribunal == "TRIBUNALES DE JUICIO ORAL EN LO PENAL") %>% 
  group_by(delito) %>% 
  summarise(`Términos en tribunales de juicio oral en lo penal` = n()) %>% 
  ungroup() %>% 
  as.data.frame()


cuadro <- full_join(cuadro1, cuadro2) %>% 
  full_join(cuadro3) %>% 
  full_join(cuadro4) %>%
  full_join(cuadro5) %>%
  full_join(cuadro6) %>%
  full_join(cuadro7) %>%
  full_join(cuadro8) %>%
  a_numeros(1:NCOL(.)) %>%  
  organizar_delitos() %>%
  select(!Total) %>% 
  ungroup() %>%
  as.data.frame() %>% 
  a_cero(3:NCOL(.))

rm(cuadro1, cuadro2, cuadro3, cuadro4, cuadro5, cuadro6, cuadro7, cuadro8)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 10 ----

cuadro <- datos_pjud$penal_terminadas_pjud %>% 
  group_by(delito, tipo_termino) %>% 
  summarise(total = n()) %>% 
  arrange(tipo_termino) %>% 
  pivot_wider(names_from = tipo_termino, values_from = total) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  organizar_delitos()

colnames(cuadro) <- str_to_sentence(sub("\\.$", "", colnames(cuadro))) %>% 
  sub("rpa", "RPA", .)

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }


# 11 ----

cuadro <- datos_pjud$crimen_pjud %>% 
  group_by(delito) %>% 
  mutate_at("delito", str_trim) %>% 
  summarise(`Total causas ingresadas` = sum(ingreso),
            `Total causas terminadas` = sum(total_terminos),
            `Sentencia definitiva` = sum(sent_def),
            `Sentencia absolutoria` = sum(sent_abs),
            `Sobreseimiento definitivo` = sum(sobre_def),
            `Sobreseimiento temporal` = sum(sobre_temp),
            `Acumuladas` = sum(acumulacion),
            `Incompetencia` = sum(incompetencia),
            `Otros motivos` = sum(otros_motivos)
            ) %>% 
  a_numeros(1:NCOL(.)) %>% 
  organizar_delitos() %>% 
  select(!Total)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") }



# 12 ----

cuadro <- datos_pjud$familia_ingresadas_pjud %>% 
  group_by(codigo_ca, codigo) %>%
  summarise(Delitos=n()) %>%
  mutate(glosa = NA) %>%  
  ordenar_cortes() %>% 
  pivot_wider(names_from = ca, values_from = Delitos) %>% 
  arrange(codigo) %>%  
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
          .)  %>% 
  sumar_filas(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  rename(Código = codigo,
         Materia = glosa,
         `Total causas ingresadas` = Total) %>% 
  a_numeros(c(1,(3:NCOL(.)))) %>% 
  ungroup() %>% 
  as.data.frame()
  

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 13 ----

cuadro <- datos_pjud$familia_terminadas_pjud %>% 
  group_by(codigo_ca, codigo) %>% 
  summarise(Delitos=n()) %>% 
  mutate(glosa = NA) %>%  
  ordenar_cortes() %>% 
  pivot_wider(names_from = ca, values_from = Delitos) %>% 
  arrange(codigo) %>%  
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .)  %>% 
  sumar_filas(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  rename(Código = codigo,
         Materia = glosa,
         `Total causas terminas` = Total) %>% 
  a_numeros(c(1,(3:NCOL(.)))) %>% 
  ungroup() %>% 
  as.data.frame()


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 14 ----

cuadro <- datos_pjud$familia_ingresadas_pjud %>% 
  group_by(lubridate::month(fecha_ingreso, label = T, abbr = F), codigo) %>% 
  summarise(Delitos=n()) %>%
  mutate(glosa = NA) %>%  
  pivot_wider(names_from = 1, values_from = Delitos) %>% 
  arrange(codigo) %>%   
  sumar_filas(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  rename(Código = codigo,
         Materia = glosa,
         `Total causas ingresadas` = Total) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[3:NCOL(.)]) ) %>%
  ungroup() %>% 
  as.data.frame() %>%
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .) %>% 
  a_numeros(c(1,(3:NCOL(.))))

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 15 ----


cuadro <- datos_pjud$familia_terminadas_pjud %>% 
  group_by(lubridate::month(fecha_termino, label = T, abbr = F), codigo) %>% 
  summarise(Delitos=n()) %>% 
  mutate(glosa = NA) %>%  
  pivot_wider(names_from = 1, values_from = Delitos) %>% 
  arrange(codigo) %>%   
  sumar_filas(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  rename(Código = codigo,
         Materia = glosa,
         `Total causas terminadas` = Total) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[3:NCOL(.)]) ) %>%
  ungroup() %>% 
  as.data.frame() %>%
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .) %>% 
  a_numeros(c(1,(3:NCOL(.))))

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 16 ----

cuadro1 <- datos_pjud$familia_ingresadas_pjud %>% 
  group_by(str_trim(codigo)) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1) %>% 
  group_by()

cuadro2 <- datos_pjud$familia_terminadas_pjud %>% 
  group_by(str_trim(codigo)) %>% 
  summarise(`Total causas terminadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1)

cuadro3 <- datos_pjud$familia_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE FAMILIA") %>% 
  group_by(str_trim(codigo)) %>% 
  summarise(`Ingresos en juzgados de familia` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1)

cuadro4 <- datos_pjud$familia_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE FAMILIA") %>% 
  group_by(str_trim(codigo)) %>% 
  summarise(`Términos en juzgados de familia` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1)

cuadro5 <- datos_pjud$familia_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>% 
  group_by(str_trim(codigo)) %>% 
  summarise(`Ingresos en juzgados de letras` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1)

cuadro6 <- datos_pjud$familia_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>%  
  group_by(str_trim(codigo)) %>% 
  summarise(`Términos en juzgados de letras` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1)

cuadro7 <- datos_pjud$familia_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(str_trim(codigo)) %>% 
  summarise(`Ingresos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1)

cuadro8 <- datos_pjud$familia_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(str_trim(codigo)) %>% 
  summarise(`Términos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  a_numeros(c(1:2)) %>% 
  rename(codigo = 1)


cuadro <- full_join(cuadro1, cuadro2) %>% 
  full_join(cuadro3) %>% 
  full_join(cuadro4) %>%
  full_join(cuadro5) %>%
  full_join(cuadro6) %>%
  full_join(cuadro7) %>%
  full_join(cuadro8) %>%
  mutate(Materia = rep(NA, NROW(.))) %>% 
  relocate(Materia, .after = codigo) %>% 
  arrange(codigo) %>% 
  a_numeros(c(1, (3:NCOL(.))))  %>% 
  ungroup() %>%
  as.data.frame() %>% 
  a_cero(3:NCOL(.)) %>% 
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .) %>% 
  rename(Códgio = codigo) %>% 
  a_numeros(c(1, (3:NCOL(.))))

rm(cuadro1, cuadro2, cuadro3, cuadro4, cuadro5, cuadro6, cuadro7, cuadro8)

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 17 ----

cuadro <- datos_pjud$familia_terminadas_pjud %>% 
  group_by(codigo, tipo_termino) %>% 
  summarise(total = n()) %>%   
  arrange(tipo_termino) %>% 
  pivot_wider(names_from = tipo_termino, values_from = total)   %>% 
  arrange(codigo) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  rbind(c("", colSums(.[,2:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[2:NCOL(.)]) ) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  mutate(Materia = rep(NA, NROW(.))) %>%   
  relocate(Materia, .after = codigo) %>% 
  relocate(Total, .after = Materia) %>% 
  rename(Códgio = codigo,
         `Total causas terminadas` = Total) %>% 
  a_numeros(c(1,(3:NCOL(.)))) %>% 
  a_cero(3:NCOL(.))

cuadro[1,2] <- "Total"


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 18 ----

cuadro1 <- datos_pjud$divorcios_pjud %>% 
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Total causas ingresadas` = sum(ingresos) ) %>%  
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro2 <- datos_pjud$divorcios_pjud %>% 
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Total causas terminadas` = sum(terminos)) %>% 
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro3 <- datos_pjud$divorcios_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE FAMILIA") %>% 
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Ingresos en juzgados de familia` = sum(ingresos)) %>% 
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro4 <- datos_pjud$divorcios_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE FAMILIA") %>% 
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Términos en juzgados de familia` = sum(terminos)) %>% 
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro5 <- datos_pjud$divorcios_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>% 
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Ingresos en juzgados de letras` = sum(ingresos)) %>% 
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro6 <- datos_pjud$divorcios_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>%  
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Términos en juzgados de letras` = sum(terminos)) %>% 
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro7 <- datos_pjud$divorcios_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Ingresos en juzgados de letras y garantía` = sum(ingresos)) %>% 
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro8 <- datos_pjud$divorcios_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(codigo, glosa, codigo_ca) %>% 
  summarise(`Términos en juzgados de letras y garantía` = sum(terminos)) %>% 
  mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>% 
  as.data.frame() %>%  
  ordenar_cortes() %>% 
  mutate_at("ca", as.character) %>% 
  as.data.frame() %>% 
  organizar_ca_x_materia(4) %>% 
  ungroup()


cuadro <- full_join(cuadro1, cuadro2) %>% 
  full_join(cuadro3) %>% 
  full_join(cuadro4) %>%
  full_join(cuadro5) %>%
  full_join(cuadro6) %>%
  full_join(cuadro7) %>%
  full_join(cuadro8) %>%
  select(!id) %>% 
  a_numeros(c(1,(3:NCOL(.)))) %>% 
  a_cero(3:NCOL(.)) %>% 
  mutate_at("glosa", str_to_sentence) %>% 
  rename(Código = codigo,
         `Corte de Apelaciones y tipos de divorcio` = glosa
  ) %>% 
  
  as.data.frame()

cuadro[which(cuadro[,2] == "Divorcio de comun acuerdo"), 2] <- "Divorcio de común acuerdo"


rm(cuadro1, cuadro2, cuadro3, cuadro4, cuadro5, cuadro6, cuadro7, cuadro8)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 



# 19 ----

cuadro <- data.frame(id = NA)

for (var in colnames(datos_pjud$divorcios_pjud)[9:(NCOL(datos_pjud$divorcios_pjud)-1)]) {

  cuadro_n <- datos_pjud$divorcios_pjud %>% 
    group_by(codigo, glosa, codigo_ca) %>% 
    summarise(ya = sum(.data[[var]]) ) %>%
    mutate(id = str_c(codigo_ca, codigo, sep = "_")) %>%
    as.data.frame() %>%  
    ordenar_cortes() %>% 
    mutate_at("ca", as.character) %>% 
    as.data.frame() %>% 
    organizar_ca_x_materia(4) %>% 
    ungroup() %>% 
    as.data.frame()
  
  colnames(cuadro_n)[(which(colnames(cuadro_n) == "ya"))] <- var
  
  cuadro <- cuadro %>% 
    full_join(cuadro_n)

}

cuadro <- cuadro[-is.na(cuadro$glosa), ] %>% 
  select(!id) %>% 
  sumar_filas(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  mutate_at("glosa", str_to_sentence) %>% 
  rename(Código = codigo,
         `Corte de Apelaciones y tipo de divorcio` = glosa) %>% 
  as.data.frame() %>% 
  a_numeros(c(1, (3:NCOL(.)))) %>% 
  filter(Total != 0)


colnames(cuadro) <- str_to_sentence(gsub("\\.", " ", colnames(cuadro)))

cuadro[which(cuadro[,2] == "Divorcio de comun acuerdo"), 2] <- "Divorcio de común acuerdo"

rm(cuadro_n)

if(any(cuadro[1,1:NCOL(cuadro)] == 0)) {
  
  cuadro <- cuadro %>% 
    select(!c(which(cuadro[1,1:NCOL(cuadro)] == 0)))
}




if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 20 ----

cuadro1 <- datos_pjud$vif_pjud %>% 
  group_by(codigo_ca) %>% 
  summarise(`Total causas ingresadas` = sum(ingresos) ) %>%  
  as.data.frame() %>%  
  ordenar_cortes()


cuadro2 <- datos_pjud$vif_pjud %>% 
  group_by(codigo_ca) %>% 
  summarise(`Total causas terminadas` = sum(terminos)) %>% 
  as.data.frame() %>%  
  ordenar_cortes()


cuadro3 <- datos_pjud$vif_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE FAMILIA") %>% 
  group_by(codigo_ca) %>% 
  summarise(`Ingresos en juzgados de familia` = sum(ingresos)) %>% 
  as.data.frame() %>%  
  ordenar_cortes()


cuadro4 <- datos_pjud$vif_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE FAMILIA") %>% 
  group_by(codigo_ca) %>% 
  summarise(`Términos en juzgados de familia` = sum(terminos)) %>% 
  as.data.frame() %>%  
  ordenar_cortes()


cuadro5 <- datos_pjud$vif_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>% 
  group_by(codigo_ca) %>% 
  summarise(`Ingresos en juzgados de letras` = sum(ingresos)) %>% 
  as.data.frame() %>%  
  ordenar_cortes()


cuadro6 <- datos_pjud$vif_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>%  
  group_by(codigo_ca) %>% 
  summarise(`Términos en juzgados de letras` = sum(terminos)) %>% 
  as.data.frame() %>%  
  ordenar_cortes()


cuadro7 <- datos_pjud$vif_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(codigo_ca) %>% 
  summarise(`Ingresos en juzgados de letras y garantía` = sum(ingresos)) %>% 
  as.data.frame() %>%  
  ordenar_cortes()


cuadro8 <- datos_pjud$vif_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(codigo_ca) %>% 
  summarise(`Términos en juzgados de letras y garantía` = sum(terminos)) %>% 
  as.data.frame() %>%  
  ordenar_cortes()


cuadro <- full_join(cuadro1, cuadro2) %>% 
  full_join(cuadro3) %>% 
  full_join(cuadro4) %>%
  full_join(cuadro5) %>%
  full_join(cuadro6) %>%
  full_join(cuadro7) %>%
  full_join(cuadro8) %>%
  a_numeros(2:NCOL(.))  %>%
  a_cero(2:NCOL(.)) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  rename(`Corte de Apelaciones` = ca) %>% 
  a_numeros(2:NCOL(.))  %>%
  ungroup() %>%
  as.data.frame()

rm(cuadro1, cuadro2, cuadro3, cuadro4, cuadro5, cuadro6, cuadro7, cuadro8)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 21 ----

cuadro <- data.frame(ca = NA)

for (var in colnames(datos_pjud$vif_pjud)[9:(NCOL(datos_pjud$vif_pjud)-1)]) {
  
  cuadro_n <- datos_pjud$vif_pjud %>% 
    group_by(codigo_ca) %>% 
    summarise(ya = sum(.data[[var]]) ) %>%
    as.data.frame() %>%  
    ordenar_cortes() %>% 
    as.data.frame() %>% 
    ungroup() 
  
  colnames(cuadro_n)[(which(colnames(cuadro_n) == "ya"))] <- var
  
  cuadro <- cuadro %>% 
    full_join(cuadro_n)
  
}

cuadro <- cuadro[-is.na(cuadro$ca), ] %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  relocate(Total, .after = ca) %>% 
  rename(`Corte de Apelaciones` = ca) %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.))


colnames(cuadro) <- str_to_sentence(gsub("\\.", " ", colnames(cuadro)))

rm(cuadro_n)

if(any(cuadro[1,1:NCOL(cuadro)] == 0)) {
  
  cuadro <- cuadro %>% 
    select(!c(which(cuadro[1,1:NCOL(cuadro)] == 0)))

}


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 22 ----

cuadro <- datos_pjud$laboral_ingresadas_pjud %>% 
  group_by(codigo_ca, codigo, glosa) %>% 
  summarise(Delitos=n()) %>% 
  ordenar_cortes() %>% 
  pivot_wider(names_from = ca, values_from = Delitos) %>% 
  arrange(codigo) %>%  
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .)  %>% 
  sumar_filas(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  rename(Código = codigo,
         Materia = glosa,
         `Total causas ingresadas` = Total) %>% 
  mutate_at("Materia", str_to_sentence) %>% 
  ungroup() %>% 
  as.data.frame()

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 23 ----

cuadro1 <- datos_pjud$laboral_ingresadas_pjud %>% 
  group_by(codigo, glosa) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)

cuadro2 <- datos_pjud$laboral_terminadas_pjud %>% 
  group_by(codigo, glosa) %>% 
  summarise(`Total causas terminadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)


cuadro3 <- datos_pjud$laboral_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>% 
  group_by(codigo, glosa) %>% 
  summarise(`Ingresos en juzgados de letras` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)

cuadro4 <- datos_pjud$laboral_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>%  
  group_by(codigo, glosa) %>% 
  summarise(`Términos en juzgados de letras` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)

cuadro5 <- datos_pjud$laboral_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(codigo, glosa) %>% 
  summarise(`Ingresos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)

cuadro6 <- datos_pjud$laboral_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(codigo, glosa) %>% 
  summarise(`Términos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)

cuadro7 <- datos_pjud$laboral_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS DEL TRABAJO") %>% 
  group_by(codigo, glosa) %>% 
  summarise(`Causas ingresadas en Juzgados de Letras del Trabajo` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)

cuadro8 <- datos_pjud$laboral_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS DEL TRABAJO") %>% 
  group_by(codigo, glosa) %>% 
  summarise(`Causas terminadas en Juzgados de Letras del Trabajo` = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  rename(codigo = 1)

cuadro <- cuadro1 %>% 
  left_join(cuadro2) %>% 
  left_join(cuadro3) %>% 
  left_join(cuadro4) %>% 
  left_join(cuadro5) %>% 
  left_join(cuadro6) %>% 
  left_join(cuadro7) %>% 
  left_join(cuadro8) %>% 
  a_numeros(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.)) %>% 
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .)  %>% 
  rename(Código = codigo,
         Materias = glosa
  ) %>% 
  a_cero(3:NCOL(.)) %>% 
  a_numeros(3:NCOL(.)) %>% 
  as.data.frame()

rm(cuadro1, cuadro2, cuadro3, cuadro4, cuadro5, cuadro6, cuadro7, cuadro8)


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 24 ----

cuadro1 <- datos_pjud$laboral_ingresadas_pjud %>% 
  group_by(procedimiento) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  as.data.frame()


cuadro2 <- datos_pjud$laboral_terminadas_pjud %>% 
  group_by(procedimiento, tipo_termino) %>% 
  summarise(Terminadas = n()) %>% 
  pivot_wider(names_from = tipo_termino, values_from = Terminadas ) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  relocate(Total, .after = procedimiento) %>% 
  rename(`Total causas terminadas` = Total) %>% 
  as.data.frame()

cuadro <- cuadro1 %>% 
  left_join(cuadro2) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.)) %>% 
  a_numeros(2:NCOL(.)) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[2:NCOL(.)]) ) %>% 
  relocate(`Devuélvase exhorto`, .after = Desistimiento)

rm(cuadro1, cuadro2)

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 25 ----

cuadro <- datos_pjud$laboral_terminadas_pjud %>% 
  group_by(codigo, glosa, tipo_termino) %>% 
  summarise(total = n()) %>%   
  arrange(tipo_termino) %>% 
  pivot_wider(names_from = tipo_termino, values_from = total)   %>% 
  arrange(codigo) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  rbind(c("", "Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .) %>% 
  sumar_filas(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[2:NCOL(.)]) ) %>% 
  rename_with(str_trim, any_of(colnames(.)) ) %>%
  rename(Código = codigo,
         Materia = Glosa) %>% 
  a_numeros(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.))


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 26 ----

cuadro1 <- datos_pjud$laboral_ingresadas_pjud %>% 
  group_by(codigo_ca) %>% 
  summarise(`Total causas ingresadas` = n()) %>%
  ordenar_cortes() %>% 
  as.data.frame()

cuadro2 <- datos_pjud$laboral_terminadas_pjud %>% 
  group_by(codigo_ca, tipo_termino) %>% 
  summarise(Terminadas = n()) %>% 
  arrange(tipo_termino) %>% 
  pivot_wider(names_from = tipo_termino, values_from = Terminadas ) %>% 
  ordenar_cortes() %>% 
  sumar_filas(2:NCOL(.)) %>% 
  relocate(Total, .after = ca) %>% 
  rename(`Total causas terminadas` = Total) %>% 
  as.data.frame()

cuadro <- cuadro1 %>% 
  full_join(cuadro2) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.)) %>% 
  a_numeros(2:NCOL(.)) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[2:NCOL(.)]) ) %>% 
  rename_with(str_trim, any_of(colnames(.)) ) %>%
  relocate(`Devuélvase exhorto`, .after = Desistimiento)


rm(cuadro1, cuadro2)

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 



# 27 ----

cuadro <- datos_pjud$laboral_terminadas_pjud%>% 
  group_by(codigo_ca, codigo, glosa) %>% 
  summarise(Delitos=n()) %>% 
  ordenar_cortes() %>% 
  pivot_wider(names_from = ca, values_from = Delitos) %>% 
  arrange(codigo) %>%  
  rbind(c("","Total", colSums(.[,3:NCOL(.), drop = FALSE], na.rm = TRUE)),
        .)  %>% 
  sumar_filas(3:NCOL(.)) %>% 
  a_cero(3:NCOL(.)) %>% 
  relocate(Total, .after = glosa) %>% 
  rename(Código = codigo,
         Materia = glosa,
         `Total causas Terminadas` = Total) %>% 
  mutate_at("Materia", str_to_sentence) %>% 
  ungroup() %>% 
  as.data.frame()


if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 



# 28 ----

cuadro1 <- datos_pjud$cobranza_ingresadas_pjud %>% 
  group_by(procedimiento) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame()

cuadro1[which(cuadro1$procedimiento %in%
                c("Ejecutivo DNP Automaticas", "Otros Titulos Ejecutivos")), 1] <-
  c("Ejecutivo DNP Automáticas", "Otros Títulos Ejecutivos")


cuadro2 <- datos_pjud$cobranza_terminadas_pjud %>% 
  group_by(procedimiento) %>% 
  summarise(`Total causas terminadas` = n()) %>% 
  ungroup() %>% 
  as.data.frame()


cuadro3 <- datos_pjud$cobranza_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE COBRANZA Y PREVISIONAL") %>% 
  group_by(procedimiento) %>% 
  summarise(`Causas ingresadas en Juzgados de Cobranza y Previsional` = n()) %>% 
  ungroup() %>% 
  as.data.frame()

cuadro3[which(cuadro3$procedimiento %in%
                c("Ejecutivo DNP Automaticas", "Otros Titulos Ejecutivos")), 1] <-
  c("Ejecutivo DNP Automáticas", "Otros Títulos Ejecutivos")


cuadro4 <- datos_pjud$cobranza_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE COBRANZA Y PREVISIONAL") %>% 
  group_by(procedimiento) %>% 
  summarise(`Causas terminadas en Juzgados de Cobranza y Previsional` = n()) %>% 
  ungroup() %>% 
  as.data.frame()


cuadro5 <- datos_pjud$cobranza_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS DEL TRABAJO") %>% 
  group_by(procedimiento) %>% 
  summarise(`Causas ingresadas en Juzgados de Letras del Trabajo` = n()) %>% 
  ungroup() %>% 
  as.data.frame()

cuadro5[which(cuadro5$procedimiento %in%
                c("Ejecutivo DNP Automaticas", "Otros Titulos Ejecutivos")), 1] <-
  c("Ejecutivo DNP Automáticas", "Otros Títulos Ejecutivos")


cuadro6 <- datos_pjud$cobranza_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS DEL TRABAJO") %>% 
  group_by(procedimiento) %>% 
  summarise(`Causas terminadas en Juzgados de Letras del Trabajo` = n()) %>% 
  ungroup() %>% 
  as.data.frame()


cuadro7 <- datos_pjud$cobranza_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(procedimiento) %>% 
  summarise(`Ingresos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame()

cuadro7[which(cuadro7$procedimiento %in%
                c("Ejecutivo DNP Automaticas", "Otros Titulos Ejecutivos")), 1] <-
  c("Ejecutivo DNP Automáticas", "Otros Títulos Ejecutivos")


cuadro8 <- datos_pjud$cobranza_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS LETRAS Y GARANTÍA") %>% 
  group_by(procedimiento) %>% 
  summarise(`Términos en juzgados de letras y garantía` = n()) %>% 
  ungroup() %>% 
  as.data.frame()



cuadro9 <- datos_pjud$cobranza_ingresadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>% 
  group_by(procedimiento) %>% 
  summarise(`Ingresos en juzgados de letras` = n()) %>% 
  ungroup() %>% 
  as.data.frame()

cuadro9[which(cuadro9$procedimiento %in%
                c("Ejecutivo DNP Automaticas", "Otros Titulos Ejecutivos")), 1] <-
  c("Ejecutivo DNP Automáticas", "Otros Títulos Ejecutivos")


cuadro10 <- datos_pjud$cobranza_terminadas_pjud %>% 
  filter(tipo_tribunal == "JUZGADOS DE LETRAS") %>%  
  group_by(procedimiento) %>% 
  summarise(`Términos en juzgados de letras` = n()) %>% 
  ungroup() %>% 
  as.data.frame()


cuadro <- full_join(cuadro1, cuadro2) %>% 
  full_join(cuadro3) %>% 
  full_join(cuadro4) %>%
  full_join(cuadro5) %>%
  full_join(cuadro6) %>%
  full_join(cuadro7) %>%
  full_join(cuadro8) %>%
  full_join(cuadro9) %>% 
  full_join(cuadro10)

cuadro11 <- c("Metge", NA, sum(datos_pjud$cobranza_metge_pjud$otros_motivos), rep(NA, (NCOL(cuadro)-4)),
              sum(datos_pjud$cobranza_metge_pjud$otros_motivos))

cuadro <- rbind(cuadro, cuadro11) %>% 
  arrange(procedimiento) %>% 
  a_numeros(2:NCOL(.)) %>%
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.)) %>% 
  rename(Procedimiento = procedimiento) %>% 
  mutate_at("Procedimiento", str_to_sentence) %>% 
  as.data.frame() %>% 
  a_numeros(2:NCOL(.))

cuadro$Procedimiento <- gsub("dnp", "DNP", cuadro$Procedimiento)

rm(cuadro1, cuadro2, cuadro3, cuadro4, cuadro5, cuadro6, cuadro7, cuadro8, cuadro9, cuadro10, cuadro11)

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# 29 ----

cuadro1 <- datos_pjud$cobranza_ingresadas_pjud %>% 
  group_by(procedimiento) %>% 
  summarise(`Total causas ingresadas` = n()) %>% 
  as.data.frame()

cuadro1[which(cuadro1$procedimiento %in%
                c("Ejecutivo DNP Automaticas", "Otros Titulos Ejecutivos")), 1] <-
  c("Ejecutivo DNP Automáticas", "Otros Títulos Ejecutivos")


cuadro2 <- datos_pjud$cobranza_terminadas_pjud %>% 
  group_by(procedimiento, tipo_termino) %>% 
  summarise(Terminadas = n()) %>% 
  pivot_wider(names_from = tipo_termino, values_from = Terminadas ) %>% 
  sumar_filas(2:NCOL(.)) %>% 
  relocate(Total, .after = procedimiento) %>% 
  rename(`Total causas terminadas` = Total) %>% 
  as.data.frame()

cuadro3 <- c("Metge", NA, sum(datos_pjud$cobranza_metge_pjud$otros_motivos), rep(NA, (NCOL(cuadro2)-2)),
             sum(datos_pjud$cobranza_metge_pjud$otros_motivos))


cuadro <- cuadro1 %>% 
  full_join(cuadro2)
  
cuadro$`Otros motivos` <- rep(NA, NROW(cuadro))

cuadro <- rbind(cuadro, cuadro3) %>% 
  arrange(procedimiento) %>% 
  rename_with(str_to_sentence, any_of(colnames(.)[1:NCOL(.)]) ) %>% 
  rename(`Demanda anteriormente presentada` = `Dda. Anteriormente  presentada`) %>% 
  relocate(`Demanda anteriormente presentada`, .before = Desistimiento) %>% 
  relocate(`Pago ya efectuado a institución`, .after = `No da curso a la demanda`) %>% 
  sumar_columnas(2:NCOL(.), pos = 1) %>% 
  a_cero(2:NCOL(.)) %>% 
  a_numeros(2:NCOL(.))
  

rm(cuadro1, cuadro2, cuadro3)

if(exists("validando_pjud")) { a_validar(cuadro, "pjud") }

if(exists("cuadros_pjud")) { encuadrar(cuadro, "pjud") } 


# Guardamos ----

if(!is.na(diccionario$meta$cont[which(diccionario$meta$id == "validador_pjud")]))  {
  
  if(NROW(resultado_validacion) > 0 ) {
    
    cat("Guardando archivo con validaciones de PJUD...\n\n")
    
    resultado_validacion <- do.call("rbind", resultado_validacion)
    
    resultado_validacion <- 
      resultado_validacion[,c("cuadro", "row", "col", "automatizacion", "control")]
    
    colnames(resultado_validacion)[2:3] <- c("fila", "columna")
    
    addWorksheet(validacion_pjud, sheetName = "Validación", gridLines = T) 
    
    writeData(validacion_pjud, sheet = "Validación",
              resultado_validacion, colNames = T)
    
    addStyle(validacion_pjud, sheet = "Validación",
             formatos$general, rows = 1:NROW(resultado_validacion),
             cols = 1:NCOL(resultado_validacion), gridExpand = T)
    
    addStyle(validacion_pjud, sheet = "Validación",
             formatos$tab_validacion, rows = 1:NROW(resultado_validacion),
             cols = 1:NCOL(resultado_validacion), gridExpand = T)
    
    addStyle(validacion_pjud, sheet = "Validación",
             formatos$cabecera, rows = 1,
             cols = 1:NCOL(resultado_validacion), gridExpand = T, stack = T)
    
    if(any(resultado_validacion$col == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón" )) {
      
      las_filas <- (which(resultado_validacion$col == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón")) + 1
      
      
    } else if(any(resultado_validacion$row == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón" ))
    {
      
      las_filas <- (which(resultado_validacion$row == "Las tablas tienen distintas dimensiones. Igualala las filas/columnas de ambas y repite la validacón")) + 1
      
    }
    
    
    
    addStyle(validacion_pjud, sheet = "Validación",
             formatos$rojo,
             rows = las_filas, cols = 1:NCOL(resultado_validacion),
             gridExpand = T, stack = T)
    
    
    
    setColWidths(validacion_pjud, sheet = "Validación", cols = c(1:5),
                 widths =c(8, 8, 8, 60, 60))
    
    saveWorkbook(validacion_pjud, paste0(here("output"), "/validacion_pjud.xlsx"), overwrite = T)
    
    estado_pjud <- "Ok"
    
  }
  
}


if(exists("cuadros_pjud")) {
  
  cat("Guardando tabulados de PJUD...\n\n")
  
  saveWorkbook(cuadros_pjud, paste0(here("output"), "/tabulados_pjud.xlsx"), overwrite = T)
  
  estado_pjud <- "Ok"
  
}