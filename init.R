# AUTOMATIZACIÓN DE TABULADOS Y VALIDACIONES DE SEGURIDAD PÚBLICA Y JUSTICIA

# Author: Daniel M. Giménez
# Date: 2020-10-05
# Modified: 2020-10-23
# Description: Generación automatizada de tabulados de PDI y CCH
# Packages Used: tidyverse, pacman, here, openxlsx, tools, shiny   
# Blog Reference: Disponible en github
# Version: 0.1

# 1. CONFIGURACIÓN INICIAL ----

options(warn=-1)

# 1.1 Carga de paquetes ----

inicio <- Sys.time()

if(!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load("tidyverse", "here", "tools", "openxlsx", "lubridate")


# 1.2 Carga de funciones generales ----

if(!exists('%notin%') & file.exists(here("R", "generales.R"))) {
  
  source(here("R", "generales.R")) 
  
} 


# 1.3 Carga de funciones específicas ----

if(!exists("diccionario") & file.exists(here("R", "especificas.R"))) {
  
  source(here("R", "especificas.R")) 
  
} 


# 1.4 Carga de Formatos --- 

if(!exists('formatos') & file.exists(here("R", "formatos.R"))) {
  
  source(here("R", "formatos.R")) 
  
} 


# 2. CARGA DE DICCIONARIO Y DATOS ----

ingresar_diccionario("~/Git/Automatizacion_SPyJ/data/diccionario_spyj_2019.xlsx")

# ingresar_diccionario()

if(!exists("organizar_delitos") & file.exists(here("R", "especificas.R"))) {
  
  source(here("R", "post_dict.R")) 
  
}

para_ceros <- "- No registró movimiento"



# 3. PDI ----

# 3.1 Tabulados ----

 if(exists("datos_pdi")) {

  source(here("R", "tabs_pdi.R")) 

 }

# 4. CCH ----

# 4.1 Tabulados ----

if(exists("datos_cch")) {
  
  source(here("R", "tabs_cch.R")) 
  
}


# 5. PJUD ----

# 5.1 Tabulados ----

if(exists("datos_pjud")) {
  
  source(here("R", "tabs_pjud.R")) 
  
}

# Cierre ----

# if(exists("estado_pdi")) {
  
#  if(estado_pdi == "Ok") {
  
#  cat("Listo. Puedes encontrar el archivo con los tabulados de PDI en el siguiente directorio:\n\n", here("output"), "\n" )
  
#  } else {
  
#  cat("Algo salió mal. Revisa el archivo del log para saber qué ocurrió")
  
#  }

# }

fin <- Sys.time()

fin - inicio