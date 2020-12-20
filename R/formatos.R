# FORMATOs PARA LAS HOJAS TABULADAS

# Author: Daniel M. Giménez
# Date: 2020-10-23
# Modified: 2020-10-30
# Description: Archivos con los estilos aplicados a los tabulados
# Packages Used: tidyverse, openxlsx   
# Blog Reference: Disponible en github
# Version: 0.1

# 

general <- createStyle(
  fontName = "Verdana",
  fontSize = 8,
  valign = "center"
)

numeros <- createStyle(
  numFmt = "_-#,##_-;-#,##_-;-_-;GENERAL",
  wrapText = T
)


titulos <- createStyle(
  fontName = "Verdana",
  fontSize = 8,
  halign = "left",
  textDecoration = "bold"
)


cabecera <- createStyle(
  halign = "center",
  textDecoration = "bold",
  border = "TopBottom",
  wrapText = T
)


abajo <- createStyle(
  border = "Bottom"
) 


var_anual <- createStyle(
  numFmt = "0.0%",
  textDecoration = "bold"
) 


negrear <- createStyle(
  textDecoration = "bold"
)

centrar <- createStyle(
  halign = "center"
)

rojo <- createStyle(
  fontColour = "red"
)

tab_validacion <- createStyle(
  numFmt = "_-#,##_-;-#,##_-;-_-;GENERAL",
)


formatos <- list() %>%
  listar(general) %>% 
  listar(titulos) %>% 
  listar(cabecera) %>% 
  listar(abajo) %>% 
  listar(var_anual) %>% 
  listar(negrear) %>% 
  listar(centrar) %>% 
  listar(numeros) %>% 
  listar(rojo) %>% 
  listar(tab_validacion)
