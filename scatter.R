# sobre: graficos de scatter 
library(tidyverse)
library(googlesheets)
library(reactable)
library(magrittr)
library(extrafont)
library(htmltools)

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
  arrange(fecha_conclusion_encuesta) %>% 
  mutate(num = 1:nrow(.)) 

evo_pred <- lm(valor ~ poly(num, 3), data = morales)
fit_evo <- augment(evo_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted)

mesa <- df %>% filter(candidato == "Mesa") %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  mutate(num = 1:nrow(.)) %>% 
  fill(valor)

mesa_pred <- lm(valor ~ poly(num, 3), data = mesa)
fit_mesa <- augment(mesa_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted)

ortiz <- df %>% filter(candidato == "Ortiz") %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  mutate(
    num = 1:nrow(.),
    valor_1 = valor
  ) %>% 
  fill(valor_1) %>% 
  fill(valor_1, .direction = "up")

ortiz_pred <- lm(valor_1 ~ poly(num, 3), data = ortiz)
fit_ortiz <- augment(ortiz_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted)


otros <- df %>% 
  filter(candidato == "otros") %>%
  group_by(margen_error, confianza, alcance_de_la_muestra, fecha_inicio_encuesta, fecha_conclusion_encuesta, encuestadora_1, fecha_1, fecha) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  mutate(
    min = valor - margen_error,
    max = valor + margen_error,
    candidato = "Otros"
  ) %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  ungroup() %>% 
  mutate(
    num = 1:nrow(.), 
    valor_1 = valor
  ) %>% 
  fill(valor_1) %>% 
  fill(valor_1, .direction = "up") 

# corrección de NA para valores de otros y no declarados
otros[which(otros$valor == 0), "valor"] <- NA

no_declarado <- df %>% 
  filter(candidato == "no declarado") %>%
  group_by(margen_error, confianza, alcance_de_la_muestra, fecha_inicio_encuesta, fecha_conclusion_encuesta, encuestadora_1, fecha_1, fecha) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  mutate(
    min = valor - margen_error,
    max = valor + margen_error,
    candidato = "Voto no declarado"
  ) %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  ungroup() %>% 
  mutate(
    num = 1:nrow(.), 
    valor_1 = valor
  ) %>% 
  fill(valor_1) %>% 
  fill(valor_1, .direction = "up")

# correcion de NA para valores de otros y no declarados
no_declarado[which(no_declarado$valor == 0), "valor"] <- NA

otros_pred <- lm(valor_1 ~ poly(num, 3), data = otros) 
fit_otros <- augment(otros_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted) %>% 
  filter(no != 51)

no_declarado_pred <- lm(valor_1 ~ poly(num, 3), data = no_declarado) 
fit_no_declarado <- augment(no_declarado_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted)

hc4 <- highchart() %>% 
  hc_add_series(morales, type = "scatter", color = highcharter::hex_to_rgba("blue", alpha = 0.6), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Evo Morales", id = "morales", 
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_evo, type = "line", color = "blue", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = NULL, headerFormat = NULL),
                name = "Fit", linkedTo = "morales", 
                enableMouseTracking =  F) %>% 
  hc_add_series(mesa, type = "scatter", color = highcharter::hex_to_rgba("orange", alpha = 0.6), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa",
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_mesa, type = "line", color = "orange", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "mesa",
                enableMouseTracking =  F) %>% 
  hc_add_series(ortiz, type = "scatter", color = highcharter::hex_to_rgba("red", alpha = 0.6), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz",
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_ortiz, type = "line", color = "red", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "ortiz",
                enableMouseTracking =  F) %>% 
  hc_add_series(otros, type = "scatter", color = highcharter::hex_to_rgba("gray", alpha = 0.6), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Otros candidatos:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Otras candidaturas", id = "otros", visible = F,
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_otros, type = "line", color = "gray", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "otros",
                enableMouseTracking =  F) %>% 
  hc_add_series(no_declarado, type = "scatter", color = highcharter::hex_to_rgba("#E0BBE4", alpha = 0.6), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Voto no declarado:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Voto no declarado", id = "no_declarado", visible = F,
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_no_declarado, type = "line", color = "gray", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "no_declarado",
                enableMouseTracking =  F) %>%
  hc_xAxis(categories = c(morales$fecha_2, "proyección"),
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Intención de voto") %>% 
  hc_subtitle(text = paste0("Elecciones generales Bolivia 2019", " (", fecha_actualizacion, ")"))  %>% 
  hc_tooltip(shared = F) %>% 
  hc_plotOptions(line = list(
    lineWidth = 4,
    connectNulls = F,
    animation = list(
      duration = 3000 
    ),
    marker = list(
      lineColor = "#ffffff",
      enabled = F,
      symbol = 'circle'
    ),
    dataLabels = list(
      enabled = F,
      format = "{point.valor:.0f} %"
    ))
  ) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_credits(enabled = TRUE, text = "BoliviaElectoral.com", href = "http://www.boliviaelectoral.com/") %>% 
  hc_exporting(
    enabled = TRUE
  ) 

htmlwidgets::saveWidget(hc4, here::here("img", "scatter.html"))  

#--------------------------
# scatter de votos válidos
#--------------------------

df$candidato_a_la_presidencia %>% unique

exclusion <- c("NS/NR", "Blanco", "Ninguno", "Nulo", "No contesta", "Indecisos", "Voto secreto", 
"Blanco/Nulo", "Ninguno/blanco /nulo")  

temp <- df %>% 
  filter(!candidato_a_la_presidencia %in% exclusion)

temp %<>% 
  group_by(codigo_encuesta) %>% 
  mutate(total = sum(valor, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(
    valor_1 = (valor/total) * 100,
    valor_1 = round(valor_1, 2)
  ) 


morales <- temp %>% filter(candidato == "Morales") %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  mutate(num = 1:nrow(.)) 

evo_pred <- lm(valor_1 ~ poly(num, 3), data = morales)
fit_evo <- augment(evo_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted)

mesa <- temp %>% filter(candidato == "Mesa") %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  mutate(
    num = 1:nrow(.),
    valor_2 = valor_1
  ) %>%
  fill(valor_2) %>% 
  fill(valor_2, .direction = "up") 


mesa_pred <- lm(valor_2 ~ poly(num, 3), data = mesa)
fit_mesa <- augment(mesa_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted)

ortiz <- temp %>% filter(candidato == "Ortiz") %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  mutate(
    num = 1:nrow(.),
    valor_2 = valor_1
  ) %>% 
  fill(valor_2) %>% 
  fill(valor_2, .direction = "up")

ortiz_pred <- lm(valor_2 ~ poly(num, 3), data = ortiz)
fit_ortiz <- augment(ortiz_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted)


otros <- temp %>% 
  filter(candidato == "otros") %>%
  group_by(margen_error, confianza, alcance_de_la_muestra, fecha_inicio_encuesta, fecha_conclusion_encuesta, encuestadora_1, fecha_1, fecha) %>% 
  summarise(valor_1 = sum(valor_1, na.rm = T)) %>% 
  mutate(
    min = valor_1 - margen_error,
    max = valor_1 + margen_error,
    candidato = "Otros"
  ) %>% 
  arrange(fecha_conclusion_encuesta) %>% 
  ungroup() %>% 
  mutate(
    num = 1:nrow(.), 
    valor_2 = valor_1
  ) %>% 
  fill(valor_2) %>% 
  fill(valor_2, .direction = "up") 

# corrección de NA para valores de otros y no declarados
otros[which(otros$valor_1 == 0), "valor"] <- NA

otros_pred <- lm(valor_2 ~ poly(num, 3), data = otros) 
fit_otros <- augment(otros_pred) %>% 
  mutate(no = 1:nrow(.)) %>% 
  select(no, .fitted) %>% 
  filter(no != 51)


hc5 <- highchart() %>% 
  hc_add_series(morales, type = "scatter", color = highcharter::hex_to_rgba("blue", alpha = 0.6), 
                hcaes(x = fecha, y = valor_1),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Evo Morales", id = "morales", 
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_evo, type = "line", color = "blue", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = NULL, headerFormat = NULL),
                name = "Fit", linkedTo = "morales", 
                enableMouseTracking =  F) %>% 
  hc_add_series(mesa, type = "scatter", color = highcharter::hex_to_rgba("orange", alpha = 0.6), 
                hcaes(x = fecha, y = valor_1),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa",
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_mesa, type = "line", color = "orange", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "mesa",
                enableMouseTracking =  F) %>% 
  hc_add_series(ortiz, type = "scatter", color = highcharter::hex_to_rgba("red", alpha = 0.6), 
                hcaes(x = fecha, y = valor_1),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz",
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_ortiz, type = "line", color = "red", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "ortiz",
                enableMouseTracking =  F) %>% 
  hc_add_series(otros, type = "scatter", color = highcharter::hex_to_rgba("gray", alpha = 0.6), 
                hcaes(x = fecha, y = valor_1),
                tooltip = list(pointFormat = paste("<b>Otros candidatos:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_de_la_muestra}"
                ), headerFormat = ""),
                name = "Otras candidaturas", id = "otros", visible = F,
                marker = list(symbol = "circle")) %>% 
  hc_add_series(fit_otros, type = "line", color = "gray", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "otros",
                enableMouseTracking =  F) %>% 
  hc_xAxis(categories = c(morales$fecha_2, "proyección"),
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Intención de voto válido") %>% 
  hc_subtitle(text = paste0("Elecciones generales Bolivia 2019", " (", fecha_actualizacion, ")"))  %>% 
  hc_tooltip(shared = F) %>% 
  hc_plotOptions(line = list(
    lineWidth = 4,
    connectNulls = F,
    animation = list(
      duration = 3000 
    ),
    marker = list(
      lineColor = "#ffffff",
      enabled = F,
      symbol = 'circle'
    ),
    dataLabels = list(
      enabled = F,
      format = "{point.valor:.0f} %"
    ))
  ) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_credits(enabled = TRUE, text = "BoliviaElectoral.com", href = "http://www.boliviaelectoral.com/") %>% 
  hc_exporting(
    enabled = TRUE
  ) 


htmlwidgets::saveWidget(hc5, here::here("img", "scatter_validos.html"))  






