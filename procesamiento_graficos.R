# sobre: graficos con area range
library(tidyverse)
library(highcharter)
library(broom)
library(magrittr)
View(fichas)
encuestas <- rio::import("output_para_procesar/encuestas.xlsx")

encuestas %>% 
  select(mes, año) %>% 
  unique() %>% 
  mutate(
    orden = 1:nrow(.)
  ) %>% 
  right_join(encuestas, .) %>% 
  mutate(fecha = paste0(mes, " ", año)) -> encuestas

fichas <- readxl::read_excel("output_para_procesar/fichas_hn_suki.xlsx") %>% 
  select(-no, -año) %>% 
  filter(!is.na(codigo_encuesta)) %>% 
  filter(str_detect(codigo_encuesta, "\\*|:", negate = T)) %>% 
  # esto solo si las fechas salen mal
  mutate(
    fecha_inicio_encuesta = janitor::excel_numeric_to_date(fecha_inicio_encuesta),
    fecha_conclusion_encuesta = janitor::excel_numeric_to_date(fecha_conclusion_encuesta)
  )

# prueba para ver que todos los resulatdos tengan su ficha técnica
(encuestas$codigo_encuesta %>% unique)[!(encuestas$codigo_encuesta %>% unique) %in% (fichas$codigo_encuesta %>% unique)]

df <- merge(encuestas, fichas, by = "codigo_encuesta") 

df %<>% 
  mutate(
    margen_error = margen_error * 100,
    confianza = confianza * 100,
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
    fecha_1 = paste0(dia_cierre, " de ", mes, " de ", lubridate::year(fecha_conclusion_encuesta)),
    fecha_2 = paste0(mes, " ", año)
  ) %>% 
  arrange(fecha_inicio_encuesta) %>% 
  filter(!is.na(margen_error))  


temp <- which(df$fecha_inicio_encuesta %>% lubridate::year() == 2918)
df[temp, "fecha_inicio_encuesta"] <- as.Date("2018-05-10")
df %<>% arrange(fecha_inicio_encuesta)
df$encuestadora_1 <- df$encuestadora %>% gsub("_", " ", .)
df$encuestadora_1 %<>% str_to_title(.)
df$encuestadora_1 %<>% gsub("Mercados Y Muestras", "Mercados y Muestras", .)

morales <- df %>% filter(candidato == "Morales") %>% 
  arrange(fecha_conclusion_encuesta)
mesa <- df %>% filter(candidato == "Mesa")
ortiz <- df %>% filter(candidato == "Ortiz")
otros <- df %>% 
  filter(candidato == "otros") %>%
  group_by(margen_error, confianza, alcance_muestra, fecha_inicio_encuesta, fecha_conclusion_encuesta, encuestadora_1, fecha_1) %>% 
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
  group_by(margen_error, confianza, alcance_muestra, fecha_inicio_encuesta, fecha_conclusion_encuesta, encuestadora_1, fecha_1) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  mutate(
    min = valor - margen_error,
    max = valor + margen_error,
    candidato = "Voto no declarado"
  )

# correcion de NA para valores de otros y no declarados
no_declarado[which(no_declarado$valor == 0), "valor"] <- NA

# gráfico 1 con arearange
hc <- highchart() %>% 
  hc_add_series(morales, type = "arearange", color = "#CCFFFF", opacity = 1,
                hcaes(x = as.factor(fecha_1) , low = min, high = max), linkedTo = "morales",
                tooltip = list(pointFormat = paste("<b>Evo Morales<b><br>
                                                   Rango posible votación: {point.min} - {point.max}"))) %>% 
  hc_add_series(morales, type = "line", color = 'blue', 
                hcaes(x = fecha, y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Evo Morales", id = "morales") %>% 
  hc_add_series(mesa, type = "arearange", color = "#FBF6D9", opacity = 1,
                hcaes(x = as.factor(mesa$fecha_conclusion_encuesta), low = min, high = max), linkedTo = "mesa",
                tooltip = list(pointFormat = paste("<b>Carlos Mesa<b><br>
                                                   Rango posible votación: {point.min} - {point.max}"))) %>% 
  hc_add_series(mesa, type = "line", color = 'orange', 
                hcaes(x = as.factor(mesa$fecha_conclusion_encuesta), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa") %>% 
  hc_add_series(ortiz, type = "arearange", color = "#E2A76F", opacity = 1,
                hcaes(x = as.factor(ortiz$fecha_conclusion_encuesta), low = min, high = max), linkedTo = "ortiz",
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz<b><br>
                                                   Rango posible votación: {point.min} - {point.max}"))) %>% 
  hc_add_series(ortiz, type = "line", color = 'red', 
                hcaes(x = as.factor(ortiz$fecha_conclusion_encuesta), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz") %>% 
  hc_add_series(otros, type = "arearange", color = "#DCDCDC", opacity = 1,
                hcaes(x = as.factor(fecha_1) , low = min, high = max), linkedTo = "otros",
                tooltip = list(pointFormat = paste("<b>Otros<b><br>
                                                   Rango posible votación: {point.min} - {point.max}"))) %>% 
  hc_add_series(otros, type = "line", color = '#A9A9A9', 
                hcaes(x = as.factor(fecha_1), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Otros:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Otras candidaturas", id = "otros") %>% 
  hc_add_series(no_declarado, type = "arearange", color = "#7fff7f", opacity = 1,
                hcaes(x = as.factor(fecha_1) , low = min, high = max), linkedTo = "no_declarado",
                tooltip = list(pointFormat = paste("<b>Otros<b><br>
                                                   Rango posible votación: {point.min} - {point.max}"))) %>% 
  hc_add_series(no_declarado, type = "line", color = '#00b200', 
                hcaes(x = as.factor(fecha_1), y = valor),
                tooltip = list(pointFormat = paste("<b>No declarado:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Voto no declarado", id = "no_declarado") %>%
  hc_xAxis(categories = morales$fecha_1,
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Tendencia electoral") %>% 
  hc_subtitle(text = "Elecciones generales Bolivia 2019") %>% 
  hc_tooltip(shared = F) %>% 
  hc_plotOptions(arearange = list(
    marker = list(
      lineWidth = 1/100,
      lineColor = "#ffffff",
      enabled = F
    ))
  ) %>% 
  hc_add_theme(hc_theme_gridlight())


# gráfico 2 sin arearange
hc2 <- highchart() %>% 
  hc_add_series(morales, type = "line", color = 'blue', 
                hcaes(x = fecha, y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Evo Morales", id = "morales") %>% 
  hc_add_series(mesa, type = "line", color = 'orange', 
                hcaes(x = as.factor(mesa$fecha_conclusion_encuesta), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa") %>% 
  hc_add_series(ortiz, type = "line", color = 'red', 
                hcaes(x = as.factor(ortiz$fecha_conclusion_encuesta), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz") %>% 
  hc_add_series(otros, type = "line", color = '#A9A9A9', visible = F,  
                hcaes(x = as.factor(fecha_1), y = valor, group = candidato),
                tooltip = list(pointFormat = paste("<b>Otros:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Otras candidaturas", id = "otros") %>% 
  hc_add_series(no_declarado, type = "line", color = '#00b200', visible = F,
                hcaes(x = as.factor(fecha_1), y = valor),
                tooltip = list(pointFormat = paste("<b>No declarado:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = ""),
                name = "Voto no declarado", id = "no_declarado") %>%
  hc_xAxis(categories = morales$fecha_2,
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Tendencia electoral") %>% 
  hc_subtitle(text = "Elecciones generales Bolivia 2019") %>% 
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
  hc_credits(enabled = TRUE, text = "seleccione las candidaturas") 
hc2
htmlwidgets::saveWidget(hc2, here::here("img", "sin_margen_error.html"))

