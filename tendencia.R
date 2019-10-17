#-----------------------
# para datos ponderados
#-----------------------
# extraer archivo
bd <- gs_title("Monitoreo de encuestas, drive privado")

# extaer hoja
df <- gs_read(ss = bd, ws = "Encuestas ponderadas ", literal = T)

# manipulaciones para el gráfico
df %<>% 
  janitor::clean_names() %>% 
  mutate(
    num = 1:nrow(.),
    fecha = paste0(mes, " ", ano),
    valor = case_when(
      valor > 100 ~ valor/100,
      T ~ valor
    )
  )

evo <- df %>% filter(candidato_a_la_presidencia == "Morales")
mesa <- df %>% filter(candidato_a_la_presidencia == "Mesa")
ortiz <- df %>% filter(candidato_a_la_presidencia == "Ortiz")

fecha_actualizacion <- Sys.Date()
fecha_actualizacion <- paste0("Actualizado el: ", lubridate::day(fecha_actualizacion),
                              "-", lubridate::month(fecha_actualizacion), 
                              "-", lubridate::year(fecha_actualizacion))


hc3 <- highchart() %>% 
  hc_add_series(evo, type = "line", color = highcharter::hex_to_rgba("blue", alpha = 0.9), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Evo Morales", id = "morales") %>% 
  # hc_add_series(fit_evo, type = "line", color = "blue", 
  #               hcaes(x = no, y = .fitted),
  #               tooltip = list(pointFormat = NA),
  #               name = "Fit", linkedTo = "morales") %>% 
  hc_add_series(mesa, type = "line", color = highcharter::hex_to_rgba("orange", alpha = 0.9), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa") %>%
  # hc_add_series(fit_mesa, type = "line", color = "orange", 
  #               hcaes(x = no, y = .fitted),
  #               tooltip = list(pointFormat = NA),
  #               name = "Fit", linkedTo = "mesa") %>% 
  hc_add_series(ortiz, type = "line", color = highcharter::hex_to_rgba("red", alpha = 0.9), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz") %>%
  # hc_add_series(ort_fecha, type = "line", color = "red", 
  #               hcaes(x = fecha, y = .fitted),
  #               tooltip = list(pointFormat = NA),
  #               name = "Fit", linkedTo = "ortiz") %>% 
  hc_xAxis(categories = evo$fecha,
           tickmarkPlacement = "on",
           title = list(enabled = T), 
           labels = list(rotation = 315), 
           opposite = F) %>% 
  hc_plotOptions(line = list(
    lineWidth = 4,
    connectNulls = F,
    animation = list(
      duration = 5000 
    ),
    marker = list(
      lineColor = "#ffffff",
      enabled = F,
      symbol = 'circle'
    ),
    dataLabels = list(
      enabled = T,
      format = "{point.valor:.0f} %"
    ))
  ) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>% 
  hc_exporting(
    enabled = TRUE
  ) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Promedio mensual de intención de voto. Ponderación: cobertura 60%, tamaño de muestra 40%") %>% 
  hc_subtitle(text = paste0("Elecciones generales Bolivia 2019", " (", fecha_actualizacion, ")"))
  
htmlwidgets::saveWidget(hc3, here::here("img", "tendencia.html"))  
  
#-------------------------------------
# tendencia ponderada de voto válido
#-------------------------------------
bd <- gs_title("Monitoreo de intención de voto - Bolivia 2019")

df <- gs_read(ss = bd, ws = "Intención de voto") %>% janitor::clean_names()

df %<>% 
  slice(-(5:nrow(.))) %>% 
  janitor::remove_empty()  
  
colnames(df)[1] <- "candidato_a_la_presidencia"

df %<>% 
  gather(fecha, valor, -candidato_a_la_presidencia) %>% 
  mutate(
    valor = str_replace(valor, "%", ""),
    valor = as.numeric(valor),
    fecha = str_replace(fecha, "_", " "),
    candidato_a_la_presidencia = case_when(
      candidato_a_la_presidencia == "Morales" ~ "Evo Morales",
      candidato_a_la_presidencia == "Mesa" ~ "Carlos Mesa",
      candidato_a_la_presidencia == "Ortiz" ~ "Oscar Ortiz",
      T ~ candidato_a_la_presidencia
    )
  ) 

evo <- df %>% filter(candidato_a_la_presidencia == "Evo Morales")
mesa <- df %>% filter(candidato_a_la_presidencia == "Carlos Mesa")
ortiz <- df %>% filter(candidato_a_la_presidencia == "Oscar Ortiz")
ortiz[ortiz$valor == 0, "valor"] <- NA

fecha_actualizacion <- Sys.Date()
fecha_actualizacion <- paste0("Actualizado el: ", lubridate::day(fecha_actualizacion),
                              "-", lubridate::month(fecha_actualizacion), 
                              "-", lubridate::year(fecha_actualizacion))


hc6 <- highchart() %>% 
  hc_add_series(evo, type = "line", color = highcharter::hex_to_rgba("blue", alpha = 0.9), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Evo Morales", id = "morales") %>% 
  # hc_add_series(fit_evo, type = "line", color = "blue", 
  #               hcaes(x = no, y = .fitted),
  #               tooltip = list(pointFormat = NA),
  #               name = "Fit", linkedTo = "morales") %>% 
  hc_add_series(mesa, type = "line", color = highcharter::hex_to_rgba("orange", alpha = 0.9), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa") %>%
  # hc_add_series(fit_mesa, type = "line", color = "orange", 
  #               hcaes(x = no, y = .fitted),
  #               tooltip = list(pointFormat = NA),
  #               name = "Fit", linkedTo = "mesa") %>% 
  hc_add_series(ortiz, type = "line", color = highcharter::hex_to_rgba("red", alpha = 0.9), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz") %>%
  # hc_add_series(ort_fecha, type = "line", color = "red", 
  #               hcaes(x = fecha, y = .fitted),
  #               tooltip = list(pointFormat = NA),
  #               name = "Fit", linkedTo = "ortiz") %>% 
  hc_xAxis(categories = evo$fecha,
           tickmarkPlacement = "on",
           title = list(enabled = T), 
           labels = list(rotation = 315), 
           opposite = F) %>% 
  hc_plotOptions(line = list(
    lineWidth = 4,
    connectNulls = F,
    animation = list(
      duration = 5000 
    ),
    marker = list(
      lineColor = "#ffffff",
      enabled = F,
      symbol = 'circle'
    ),
    dataLabels = list(
      enabled = T,
      format = "{point.valor:.0f} %"
    ))
  ) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>% 
  hc_exporting(
    enabled = TRUE
  ) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Voto válido. Promedio mensual de intención de voto. Ponderación: cobertura 60%, tamaño de muestra 40%") %>% 
  hc_subtitle(text = paste0("Elecciones generales Bolivia 2019", " (", fecha_actualizacion, ")")) %>% 
  hc_chart(style = list(fontFamily = "Roboto"))

htmlwidgets::saveWidget(hc6, here::here("img", "tendencia_validos.html"))  




# <- <- <- <- <- <- <- <- <- <- <- <- <- -
hc_add_series(mesa, type = "line", color = highcharter::hex_to_rgba("orange", alpha = 0.2), 
              hcaes(x = fecha, y = valor),
              tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>"), headerFormat = ""),
              name = "Carlos Mesa", id = "mesa")


  