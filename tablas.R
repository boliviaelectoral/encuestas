# sobre: tabla con enlace a fuentes primarias  y tabla a drive de acceso público
library(tidyverse)
library(googlesheets)
library(reactable)
library(magrittr)
library(extrafont)
library(htmltools)

extrafont::font_import()
# descarga del archivo
bd <- gs_title("Monitoreo de encuestas, drive privado")

# lista de hojas
Monitoreo de encuestas, drive privado

# seleccion de hoja
df <- gs_read(ss = bd, ws = "fuentes_encuestas")

# sacar nota
nota <- df %>% 
  filter(str_detect(Encuestadora, "\\*")) %>% 
  pull(Encuestadora) 
  
# quitar filas y columnas
df %<>% 
  select(-X7, -codigo_encuesta) %>% 
  filter(str_detect(Encuestadora, "Notas", negate = T)) %>% 
  filter(str_detect(Encuestadora, "\\*", negate = T))

# configurar tabla
reactable(df, filterable = TRUE, pagination = FALSE, columns = list(
  Encuestadora = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Medios de publicación` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Fecha de inicio de la encuesta` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Fecha de finalización de la encuesta` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Nº` = colDef(
    style = list(
      fontFamily = "Roboto"
    )
  ),
  `Enlace a resultados` = colDef(
    cell = function(value) {
      url <- paste0(value)
      tags$a(href = url, target = "_blank", paste0("enlace"))
    },
    style = list(
      fontFamily = "Roboto"
    )
   )
 )
) %>% 
  htmlwidgets::saveWidget(here::here("img", "tabla.html"))



  
  