#-----------------------
# para datos ponderados
#-----------------------
# extraer archivo
bd <- gs_title("Monitoreo de encuestas, drive privado")

# extaer hoja
df <- gs_read(ss = bd, ws = "Encuestas ponderadas ")

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

df[which(df$valor == 1.06), "valor"] <- 10.6
df[which(df$valor == 93.00), "valor"] <- 9.3
df[which(df$valor == 2.48), "valor"] <- 24.8
df[which(df$valor == 3.55), "valor"] <- 35.5
df[which(df$valor == 2.36), "valor"] <- 23.6

evo <- df %>% filter(candidato_a_la_presidencia == "Morales")
mesa <- df %>% filter(candidato_a_la_presidencia == "Mesa")
ortiz <- df %>% filter(candidato_a_la_presidencia == "Ortiz")

evo_pred <- loess(valor ~ num, data = evo, span = 2)
fit_evo <- arrange(augment(evo_pred), num) %>% 
  mutate(no = 1:nrow(.)) %>% 
  filter(no != 25)

mesa_pred <- loess(valor ~ num, data = mesa, span = 2)
fit_mesa <- arrange(augment(mesa_pred), num) %>% 
  mutate(no = 1:nrow(.))

ortiz_pred <- loess(valor ~ num, data = ortiz, span = 2)
fit_ortiz <- arrange(augment(ortiz_pred), num) %>% 
  mutate(no = 1:nrow(.))

ort_fecha <- ortiz %>% 
  select(fecha, valor, num)

ort_fecha %<>% merge(., fit_ortiz, all.x = T) %>% 
  arrange(num)



hc3 <- highchart() %>% 
  hc_add_series(evo, type = "line", color = highcharter::hex_to_rgba("blue", alpha = 0.2), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Evo Morales", id = "morales") %>% 
  hc_add_series(fit_evo, type = "line", color = "blue", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "morales") %>% 
  hc_add_series(mesa, type = "line", color = highcharter::hex_to_rgba("orange", alpha = 0.2), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Carlos Mesa:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Carlos Mesa", id = "mesa") %>%
  hc_add_series(fit_mesa, type = "line", color = "orange", 
                hcaes(x = no, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "mesa") %>% 
  hc_add_series(ortiz, type = "line", color = highcharter::hex_to_rgba("red", alpha = 0.2), 
                hcaes(x = fecha, y = valor),
                tooltip = list(pointFormat = paste("<b>Oscar Ortiz:<b> {point.valor} %<br>"), headerFormat = ""),
                name = "Oscar Ortiz", id = "ortiz") %>%
  hc_add_series(ort_fecha, type = "line", color = "red", 
                hcaes(x = fecha, y = .fitted),
                tooltip = list(pointFormat = "Línea de tendencia", headerFormat = ""),
                name = "Fit", linkedTo = "ortiz") %>% 
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
      lineWidth = 300,
      lineColor = "#ffffff",
      enabled = F
    ),
    dataLabels = list(
      enabled = F,
      format = "{point.valor:.0f} %"
    ))
  ) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>% 
  hc_exporting(
    enabled = TRUE
  ) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Ponderación de encuestas y ajuste de tendencia") %>% 
  hc_subtitle(text = "Elecciones generales Bolivia 2019") 
  
htmlwidgets::saveWidget(hc3, here::here("img", "tendencia.html"))  
   
  