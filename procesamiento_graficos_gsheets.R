# sobre: procesamiento grafico desde google sheets
library(tidyverse)
library(highcharter)
library(broom)
library(magrittr)
library(googlesheets)

bd <- gs_title("Monitoreo de encuestas, drive privado")

# extaer hoja
encuestas <- gs_read(ss = bd, ws = "Base de datos") %>% janitor::clean_names()

encuestas %>% 
  janitor::clean_names() %>% 
  select(mes, ano) %>% 
  unique() %>% 
  mutate(
    orden = 1:nrow(.)
  ) %>%
  right_join(encuestas, .) %>% 
  rename(codigo_encuesta = codigo_de_la_encuesta) %>% 
  mutate(fecha = paste0(mes, " ", ano)) -> encuestas

fichas <- gs_read(ss = bd, ws = "Fichas de las encuestas") %>% janitor::clean_names() 

fichas %<>%
  select(-nº, -ano) %>% 
  rename(
    codigo_encuesta = codigo_de_la_encuesta,
    fecha_inicio_encuesta = fecha_de_inicio_de_la_encuesta,
    fecha_conclusion_encuesta = fecha_de_conclusion_de_la_encuesta
  ) %>% 
  filter(!is.na(codigo_encuesta)) %>% 
  filter(str_detect(codigo_encuesta, "\\*|:", negate = T)) %>% 
  # esto solo si las fechas salen mal
  mutate(
    fecha_inicio_encuesta = as.Date(fecha_inicio_encuesta, format="%d/%m/%Y"),
    fecha_conclusion_encuesta = as.Date(fecha_conclusion_encuesta, format="%d/%m/%Y")
  ) 


# prueba para ver que todos los resulatdos tengan su ficha técnica
(encuestas$codigo_encuesta %>% unique)[!(encuestas$codigo_encuesta %>% unique) %in% (fichas$codigo_encuesta %>% unique)]

df <- merge(encuestas, fichas, by = "codigo_encuesta") 
df$margen_de_error %<>% gsub("%|\\*", "", .)
df$margen_de_error %<>% gsub(",", ".", .)
df$confianza %<>% gsub("%", "", .)
df$valor %<>% gsub(",", ".", .)
df$valor %<>% as.numeric() 
df$alcance_de_la_muestra %<>% gsub("\\*", "", .)
df$alcance_de_la_muestra %<>% as.numeric()


df %<>% 
  mutate(
    margen_error = as.numeric(margen_de_error),
    confianza = as.numeric(confianza),
    candidato = case_when(
      candidato_a_la_presidencia == "Mesa" ~ "Mesa",
      candidato_a_la_presidencia == "Morales" ~ "Morales",
      candidato_a_la_presidencia == "Ortiz" ~ "Ortiz",
      candidato_a_la_presidencia %in% c("Blanco", "Ninguno", "Nulo", "No contesta", "Indecisos", "Voto secreto", "Blanco/Nulo", "NS/NR") ~ "no declarado",
      T ~ "otros"
    ),
    min = valor - margen_error,
    max = valor + margen_error,
    dia_cierre = lubridate::day(fecha_conclusion_encuesta),
    mes = lubridate::month(fecha_conclusion_encuesta),
    mes = case_when(
      mes == 1 ~ "enero",
      mes == 2 ~ "febrero",
      mes == 3 ~ "marzo",
      mes == 4 ~ "abril",
      mes == 5 ~ "mayo",
      mes == 6 ~ "junio",
      mes == 7 ~ "julio",
      mes == 8 ~ "agosto",
      mes == 9 ~ "septiembre",
      mes == 10 ~ "octubre",
      mes == 11 ~ "noviembre",
      mes == 12 ~ "diciembre"
    ),
    fecha_1 = paste0(dia_cierre, " de ", mes, " de ", lubridate::year(fecha_conclusion_encuesta)),
    fecha_2 = paste0(mes, " ", ano)
  ) %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  filter(!is.na(margen_error))  

df %<>% arrange(fecha_conclusion_encuesta)
df$encuestadora_1 <- df$encuestadora %>% gsub("_", " ", .)
df$encuestadora_1 %<>% str_to_title(.)
df$encuestadora_1 %<>% gsub("Mercados Y Muestras", "Mercados y Muestras", .)

morales <- df %>% filter(candidato == "Morales") %>% 
  arrange(fecha_conclusion_encuesta)
mesa <- df %>% filter(candidato == "Mesa") %>% 
  arrange(fecha_conclusion_encuesta)
ortiz <- df %>% filter(candidato == "Ortiz") %>% 
  arrange(fecha_conclusion_encuesta)

otros <- df %>% 
  filter(candidato == "otros") %>%
  group_by(margen_error, confianza, alcance_de_la_muestra, fecha_inicio_encuesta, fecha_conclusion_encuesta, encuestadora_1, fecha_1) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  mutate(
    min = valor - margen_error,
    max = valor + margen_error,
    candidato = "Otros"
  )

# correcion de NA para valores de otros y no declarados
otros[which(otros$valor == 0), "valor"] <- NA

no_declarado <- df %>% 
  filter(candidato == "no declarado") %>%
  group_by(margen_error, confianza, alcance_de_la_muestra, fecha_inicio_encuesta, fecha_conclusion_encuesta, encuestadora_1, fecha_1) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  mutate(
    min = valor - margen_error,
    max = valor + margen_error,
    candidato = "Voto no declarado"
  )

# correcion de NA para valores de otros y no declarados
no_declarado[which(no_declarado$valor == 0), "valor"] <- NA

# gráfico 2 sin márgenes de error
fecha_actualizacion <- Sys.Date()
fecha_actualizacion <- paste0("Actualizado el: ", lubridate::day(fecha_actualizacion),
                              "-", lubridate::month(fecha_actualizacion), 
                              "-", lubridate::year(fecha_actualizacion))

hc2 <- highchart() %>% 
  hc_add_series(morales, type = "line", color = 'blue', 
                hcaes(x = fecha, y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Evo Morales", id = "morales") %>% 
  hc_add_series(mesa, type = "line", color = 'orange', 
                hcaes(x = as.factor(mesa$fecha_conclusion_encuesta), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa") %>% 
  hc_add_series(ortiz, type = "line", color = 'red', 
                hcaes(x = as.factor(ortiz$fecha_conclusion_encuesta), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz") %>% 
  hc_add_series(otros, type = "line", color = '#A9A9A9', visible = F,  
                hcaes(x = as.factor(fecha_1), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Otros:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Otras candidaturas", id = "otros") %>% 
  hc_add_series(no_declarado, type = "line", color = '#00b200', visible = F,
                hcaes(x = as.factor(fecha_1), y = valor),
                tooltip = list(pointFormat = paste("<b>No declarado:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Voto no declarado", id = "no_declarado") %>%
  hc_xAxis(categories = morales$fecha_2,
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Tendencia electoral") %>% 
  hc_subtitle(text = paste0("Elecciones generales Bolivia 2019", " (", fecha_actualizacion, ")"))  %>% 
  hc_tooltip(shared = F) %>% 
  hc_plotOptions(line = list(
    lineWidth = 4,
    connectNulls = F,
    animation = list(
      duration = 3000 
    ),
    marker = list(
      lineWidth = 300,
      lineColor = "#ffffff",
      enabled = F
    ),
    dataLabels = list(
      enabled = T,
      format = "{point.valor:.0f} %"
    ))
  ) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_credits(enabled = TRUE, text = "BoliviaElectoral.com", href = "http://www.boliviaelectoral.com/") %>% 
  hc_exporting(
    enabled = TRUE
  ) 


htmlwidgets::saveWidget(hc2, here::here("img", "sin_margen_error.html"))

