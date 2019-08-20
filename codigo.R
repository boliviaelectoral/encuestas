# sobre: limpieza base de encuestas 
library(tidyverse)
library(magrittr)
library(janitor)

df <- rio::import("input/Cuadro de intención de voto en el nivel nacional en porcentajes_1.xlsx") %>% 
  clean_names()

df_1 <- rio::import("input/Cuadro de intención de voto en el nivel nacional en porcentajes.xlsx") %>% 
  clean_names()

row_1 <- df %>% slice(1)
correction <- c(NA, NA, "marzo_2018", "mayo_2018", rep("julio_2018", 2), "agosto_2018", rep("septiembre_2018", 2), 
  rep("octubre_2018", 4), "noviembre_2018", rep("diciembre_2018", 2), rep("enero_2019", 2),
  rep("febrero_2019", 2), rep("marzo_2019", 3), rep("abril_2019", 2), rep("mayo_2019", 2), rep("julio_2019", 3))
names <- paste0(row_1, ":" , correction)
colnames(df) <- names
df %<>% 
  slice(-c(1:6)) %>% 
  gather(encuestadora, valor, -`Partido o alianza:NA`, -`Candidato a la presidencia:NA`) 

df$mes <- str_split(df$encuestadora, pattern = ":") %>% 
  map(., last) %>% 
  unlist

df %<>% 
  mutate(año = parse_number(mes)) 

df$mes %<>% gsub("_2018|_2019", "", .)

df %<>% clean_names() 
colnames(df) %<>% gsub("_na", "", .) 
df$valor %<>% as.numeric() 
df %<>% 
  filter(!partido_o_alianza %in% "Totales")

df$encuestadora <- str_split(df$encuestadora, pattern = ":") %>%
  map(., first) %>% 
  unlist()

df$encuestadora <-  str_replace_all(df$encuestadora, "[\r\n]" , "")
df$encuestadora %>% unique

df %<>% 
  mutate(encuestadora_1 = case_when(
    encuestadora == "Captura Consulting (Poder y Placer/PAT)" ~ "captura_consulting",
    encuestadora == "Mercados y Muestras SRL (Página Siete)" ~ "mercados_y_muestras",
    encuestadora == "Mercados yMuestras SRL (Página Siete)" ~ "mercados_y_muestras",
    encuestadora == "IPSOS (RTP)" ~ "ipsos",
    encuestadora == "Tal Cual (ATB)" ~ "tal_cual",
    encuestadora == "CIESMORI (ATB)" ~ "ciesmori",
    encuestadora == "Celag" ~ "celag",
    encuestadora == "Tal Cual (La Razón)" ~ "tal_cual",
    encuestadora == "Captura Consulting  (Poder y Placer/PAT)" ~ "captura_consulting"
  ),
  medio = case_when(
    encuestadora == "Captura Consulting (Poder y Placer/PAT)" ~ "pat",
    encuestadora == "Mercados y Muestras SRL (Página Siete)" ~ "pg7",
    encuestadora == "Mercados yMuestras SRL (Página Siete)" ~ "pg7",
    encuestadora == "IPSOS (RTP)" ~ "rtp",
    encuestadora == "Tal Cual (ATB)" ~ "atb",
    encuestadora == "CIESMORI (ATB)" ~ "atb",
    encuestadora == "Celag" ~ "NA",
    encuestadora == "Tal Cual (La Razón)" ~ "la_razon",
    encuestadora == "Captura Consulting  (Poder y Placer/PAT)" ~ "pat"
  ))   

df$encuestadora_1 %<>% gsub("NA", "", .)
df %<>% 
  select(-encuestadora) %>% 
  rename(encuestadora = encuestadora_1)

df %<>% 
  mutate(codigo_encuesta = paste0(encuestadora, "_", mes, "_", ano)) %>% 
  rename(año = ano)

df %>% 
  rio::export("output_para_procesar/encuestas.xlsx")

df %>% 
  write_csv("output_para_procesar/encuestas.csv")
