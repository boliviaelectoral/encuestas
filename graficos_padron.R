# sobre: padrón electoral para bolivia electoral
library(countrycode)
library(highcharter)
library(tidyverse)

# proporción padrón nacional vs exterior

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = c("Nacional", "Exterior")) %>% 
  hc_add_series(data = c(6974363,  341001),
                name = "Padrón", 
                showInLegend = FALSE) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_exporting(
    enabled = TRUE
  ) %>% 
  hc_yAxis(title = list(text = "Inscritos habilitados para votar")) %>% 
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>%
  hc_title(text = "Padrón electoral 2019. 7.3 millones de personas. 99.3% votan en Bolivia y 4.7% en el exterior") %>% 
  htmlwidgets::saveWidget(here::here("img", "bolivia_ext_padron.html"))
  

# peso por departamentos
temp <- read_csv("input/horizontal_nacional.csv") %>% 
  group_by(departamento) %>% 
  summarise(inscritos = sum(inscritos_habilitados)) %>% 
  mutate(
    prop = prop.table(inscritos) * 100,
    prop = round(prop, 2)
  ) %>% 
  arrange(desc(inscritos))

highchart() %>% 
  hc_add_series(temp, type = "column",  name = "padron por departamento", showInLegend = FALSE,
                hcaes(x = as.factor(departamento), y = inscritos, color = departamento),
                tooltip = list(pointFormat = paste("<b>Departamento:<b> {point.departamento}<br>
                                                   <b>Inscritos:<b> {point.inscritos}<br>
                                                   <b>Porcentaje de inscritos respecto al padrón nacional:<b> {point.prop} %"), 
                               headerFormat = "")) %>% 
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_exporting(
    enabled = TRUE
  ) %>% 
  hc_yAxis(title = list(text = "Inscritos habilitados para votar")) %>% 
  hc_xAxis(categories = temp$departamento,
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_title(text = "Padrón electoral por departamento") %>%
  hc_plotOptions(column = list(
    pointWidth = 70,
    connectNulls = F,
    animation = list(
      duration = 3000 
    )
  )
) %>%  
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>% 
  htmlwidgets::saveWidget(here::here("img", "padron_dpto.html"))

# peso por municipios
temp <- read_csv("input/horizontal_nacional.csv") %>% 
  group_by(departamento, municipio, codigo) %>% 
  summarise(inscritos = sum(inscritos_habilitados)) %>% 
  ungroup() %>% 
  mutate(
    value = prop.table(inscritos) * 100,
    value = round(value, 2)
  ) %>% 
  arrange(desc(value)) %>% 
  rename(CODIGO = codigo)

# mapa
mapa <- jsonlite::fromJSON("input/municipios.339.geojson", simplifyVector = F)
colores <-  viridisLite::cividis(4, begin = 0, end = 0.82, direction = 1)
secuencia <- c(0, 1, 5, 10, 15.42)

highchart(type = "map") %>%
  hc_add_series(mapData = mapa, data = temp, value = "value", 
                joinBy = "CODIGO", borderColor = "gray",
                borderWidth = 0.1) %>%  
  hc_colorAxis(dataClasses = color_classes(secuencia, colores)) %>% 
  hc_legend(layout = "vertical", align = "right",
            verticalAlign = "bottom", floating = T,
            valueSuffix = " %", valueDecimals = 0) %>% 
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             pointFormat=paste("Municipio: <b>{point.municipio}</b><br>
                               Inscritos: <b>{point.inscritos} </b><br>
                               Porcentaje de inscritos respecto a padrón nacional: <b>{point.value:.3f} % </b>"),
             headerFormat = "") %>% 
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>% 
  hc_exporting(
    enabled = TRUE
  ) %>% 
  hc_title(text = "¿Cuál es el peso de cada municipio respecto al padrón nacional?") %>% 
  hc_subtitle(text = "Elecciones generales Bolivia 2019") %>% 
  hc_chart(style = list(fontFamily = "Roboto")) %>% 
  htmlwidgets::saveWidget(here::here("img", "mapa_municipios.html"))

# peso de los inscritos por países
temp <- read_csv("input/horizontal_exterior.csv") %>% 
  filter(!is.na(pais)) %>% 
  group_by(pais) %>% 
  summarise(inscritos = sum(inscritos_habilitados)) %>% 
  ungroup() %>% 
  mutate(
    value = prop.table(inscritos) * 100,
    value = round(value, 3),
    prop = inscritos/7315364 * 100,
    prop = round(prop, 3)
  ) %>% 
  arrange(desc(value))
  
highchart() %>% 
  hc_add_series(temp, type = "bar",  name = "padron por país", showInLegend = FALSE,
                hcaes(x = as.factor(pais), y = inscritos, color = pais),
                tooltip = list(pointFormat = paste("<b>País:<b> {point.pais}<br>
                                                   <b>Inscritos:<b> {point.inscritos}<br>
                                                   <b>Porcentaje de inscritos respecto al padrón exterior:<b> {point.value} %<br>
                                                   <b>Porcentaje de inscritos respecto al padrón total:<b> {point.prop} %"), 
                               headerFormat = "")) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_exporting(
    enabled = TRUE
  ) %>% 
  hc_yAxis(title = list(text = "Inscritos habilitados para votar")) %>% 
  hc_xAxis(categories = temp$pais,
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_title(text = "Padrón electoral por país") %>%
  hc_subtitle(text = "6 de 33 países concentran el 97% del voto") %>%
  hc_plotOptions(bar = list(
    pointWidth = 20,
    connectNulls = F,
    animation = list(
      duration = 3000 
    )
  )
  ) %>%  
  hc_credits(enabled = TRUE, text = "Bolivia Electoral", href = "http://www.boliviaelectoral.com/") %>% 
  htmlwidgets::saveWidget(here::here("img", "paises_barras.html"))
  

codigos <- countrycode::codelist %>% 
  select(iso3c, un.name.es) %>% 
  filter(!is.na(iso3c)) %>% 
  filter(!is.na(un.name.es)) %>% 
  mutate_if(is.character, toupper) %>% 
  rename(
    pais = un.name.es
  )

temp$pais[!temp$pais %in% codigos$pais]

codigos$pais %<>% gsub("ESTADOS UNIDOS DE AMÉRICA", "ESTADOS UNIDOS", .) 
temp$pais %<>% gsub("BELGICA", "BÉLGICA", .) 
codigos$pais %<>% gsub("FEDERACIÓN DE RUSIA", "RUSIA", .) 
codigos$pais %<>% gsub("IRÁN \\(REPÚBLICA ISLÁMICA DEL\\)", "IRÁN", .) 
temp$pais %<>% gsub("PAISES BAJOS", "PAÍSES BAJOS", .) 
codigos$pais %<>% gsub("VENEZUELA \\(REPÚBLICA BOLIVARIANA DE\\)", "VENEZUELA", .) 
codigos$pais %<>% gsub("REPÚBLICA DE COREA", "REPÚBLICA DE COREA DEL SUR", .) 
codigos$pais %<>% gsub("REINO UNIDO DE GRAN BRETAÑA E IRLANDA DEL NORTE", "GRAN BRETAÑA", .) 

temp$pais[!temp$pais %in% codigos$pais]

temp %<>% merge(., codigos, all.x = T)


temp %>% 
  slice(1:6) %>% 
  summarise(sum(value))



