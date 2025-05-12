# TRABAJO PRACTICO FINAL ALGORITMOS Y ESTRUCTURA DE DATOS

####################################

# limpio entorno de trabajo y quito notacion cientifica

rm(list = ls())
options(scipen=999)

# Obtener directorio del script actual y modificar el working directory
library("rstudioapi")  

current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

# Observo en que directorio estoy trabajando
getwd()


# Llamo libreria que voy a utilizar
library("tidyverse")

# NOTA TP. No descomprimir archivos en el script. Ver enunciado TP.
# Extraer el archivo CSV del archivo zip
# Construir la ruta completa al archivo ZIP dentro de la carpeta "datos"
#zip_file_path <- file.path(current_working_dir, "datos", "snic-departamentos-mes-sexo.zip")
#unzip(zip_file_path, files = "snic-departamentos-mes-sexo.csv", exdir = current_working_dir)
# unzip("../datos/snic-departamentos-mes-sexo.zip", files = "snic-departamentos-mes-sexo.csv", exdir = "../datos")

# Cargar datos del Sistema Nacional de Información Criminal (SNIC) por provincia

snic_departamentos_mes_sexo <- read_delim("../datos/snic-departamentos-mes-sexo.csv",delim = ";")

# Cargar datos Información ciudad de buenos aires

caba_poblacion_comunas<- read_delim("../datos/caba_poblacion_comunas.csv",delim= ";")

# Observo nombre de variables
colnames(snic_departamentos_mes_sexo)
colnames(caba_poblacion_comunas)
###########################################################
#PUNTOA

# Filtrar el dataframe snic por años a trabajar y tipo de delito
snic_filtrado <- subset(snic_departamentos_mes_sexo, anio >= 2014 & anio <= 2022)
snic_filtrado$provincia_id <- NULL
snic_filtrado$departamento_id <- NULL
snic_filtrado$codigo_delito_snic_id <- NULL
snic_filtrado <- subset(snic_filtrado, codigo_delito_snic_nombre =="Homicidios dolosos")
snic_filtrado <- subset(snic_filtrado,provincia_nombre== "Ciudad Autónoma de Buenos Aires")


# Mostrar NA
colSums(is.na(snic_filtrado))

#agrupo por año

snic_filtrado <- snic_filtrado %>% group_by(anio)%>%
  summarise(total_cantidad_hechos = sum(cantidad_hechos),
            total_cantidad_victimas = sum(cantidad_victimas),total_masculinas=sum(cantidad_victimas_masc),total_femeninas=sum(cantidad_victimas_fem))

snic_filtrado <- select(snic_filtrado, -total_cantidad_hechos)


# Llamo libreria que voy a utilizar
library(dplyr)
library(plotly)


# veo datos de file de comunas
colnames(caba_poblacion_comunas)



#filtrar datos caba
caba_poblacion_comuna_filtrado <- caba_poblacion_comunas %>% group_by(anio)%>%
  summarise(total_generos = sum(total),
            total_masculino = sum(masc),
            total_femenino = sum(fem))

caba_poblacion_comuna_filtrado <- subset(caba_poblacion_comuna_filtrado, anio >= 2014 & anio <= 2022)

### crear variable nueva con la tasa  cada 1.000 habitantes
#datoscaba_merge$victimasporMil <- datoscaba_merge$cantidad_victimas * 1000 / datoscaba_merge$total

# combinar ambos dataframes
merge_data_total <- merge(snic_filtrado, caba_poblacion_comuna_filtrado)

# crear variable nueva con la tasa  cada 1.000 habitantes y porcentajes genero
merge_data_total <- merge_data_total%>%mutate(tasa_por_mil=(total_cantidad_victimas*1000/total_generos))

merge_data_total <- merge_data_total%>%mutate(total_masculinas/total_cantidad_victimas)
merge_data_total <- merge_data_total%>%mutate(total_femeninas/total_cantidad_victimas)
merge_data_total <- select(merge_data_total, -total_masculino, -total_femenino)


#renombrar columnas
colnames(merge_data_total)
merge_data_total <-merge_data_total %>%rename(cantidad_victimas=total_cantidad_victimas,cantidad_victimas_masc=total_masculinas,cantidad_victimas_fem=total_femeninas )
merge_data_total <-merge_data_total %>%mutate(porcentaje_masc = `total_masculinas/total_cantidad_victimas` ,porcentaje_fem = `total_femeninas/total_cantidad_victimas` )
merge_data_total <- select(merge_data_total, -`total_masculinas/total_cantidad_victimas`, -`total_femeninas/total_cantidad_victimas`)
merge_data_total <-merge_data_total %>%rename(tasa_por_100k=tasa_por_mil,poblacion_total=total_generos)

colnames(merge_data_total)
merge_data_total <- merge_data_total%>%mutate(tasa_por_100k*100)
merge_data_total <- select(merge_data_total, -tasa_por_100k)
merge_data_total <- merge_data_total %>%
  rename(tasa_por_100k = `tasa_por_100k * 100`)

#reordenar
merge_data_total <- merge_data_total %>%
  select(anio, cantidad_victimas_masc,cantidad_victimas_fem,cantidad_victimas,porcentaje_masc,porcentaje_fem,tasa_por_100k,poblacion_total)

# NOTA TP. Ruta del archivo. Ver enunciado
#exportar csv
# write.csv(merge_data_total,'../Ton_Vanerio_Nicolas_tpalgoritmos/homicidios_dolosos_agrupados.csv', row.names = FALSE)
write.csv(merge_data_total,'homicidios_dolosos_agrupados.csv', row.names = FALSE)
############################################################################
#PUNTO B
#Grafico
library(ggplot2)

library(ggplot2)
library(plotly)

# Asume que merge_data_total es tu data frame con las columnas `anio`, `cantidad_victimas` y `tasa_por_100k`.

factor_escalado <- max(merge_data_total$cantidad_victimas) / max(merge_data_total$tasa_por_100k) * 1.2

gg <- ggplot(merge_data_total, aes(x = as.factor(anio))) +
  geom_bar(aes(y = cantidad_victimas), stat = "identity", fill = "#BED5B4", width = 0.6) +
  
  geom_smooth(aes(y = tasa_por_100k * factor_escalado, group = 1),
              method = "loess", se = FALSE, color = "#62993E", size = 1.5) +
  
  geom_point(aes(y = tasa_por_100k * factor_escalado), color = "#62993E", size = 3) +
  geom_text(aes(y = cantidad_victimas, label = cantidad_victimas),
            vjust = -0.5, hjust = 0.5, color = "black", size = 4, position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = tasa_por_100k * factor_escalado, label = paste0(round(tasa_por_100k, 1), "%")),
            vjust = -1.5, color = "#62993E", size = 4, nudge_y = 0.5 * factor_escalado)  +
  scale_y_continuous(
    name = "Número de víctimas",
    sec.axis = sec_axis(~ . / factor_escalado, name = "Tasa por 100.000 habitantes")
  ) +
  labs(title = "Víctimas de homicidios dolosos por año y tasas cada 100.000 habitantes",
       x = "Año") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())  

# Convertimos el gráfico a un objeto plotly
grafico <- ggplotly(gg)

print(grafico)

# NOTA TP: posibles mejoras al gráfico. Hacer que el "spline" (smooth) pase por los puntos.
gg2 <- ggplot(merge_data_total, aes(x = anio, y = cantidad_victimas)) +
  geom_segment(linewidth = 16, color="#BED5B4", aes(xend = anio, yend=0)) +
  geom_point(size=3, color="#62993E", position = position_stack(vjust = 1.15)) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#62993E", position = position_stack(vjust = 1.15), span = 0.5) +
  ggtitle("Víctimas de homicidios dolosos por año. Ciudad Autonoma de Buenos Aires. Años 2014-2022") +
  theme(plot.title = element_text(lineheight = 1, face ='bold'))   +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = unique(merge_data_total$anio)) +
  labs(caption = "\nFuente: Sistema Nacional de Información Criminal -Sistema Alerta Temprana (SNIC -SAT), Ministerio de Seguridad de la Nación e INDEC") +
  theme_minimal() +
  geom_text(aes(label = cantidad_victimas), size = 3.6, position = position_stack(vjust = .5), fontface = "bold") +
  geom_text(aes(label = round(tasa_por_100k, 1)), size = 3.6, position = position_stack(vjust = 1.23), color="#62993E", fontface = "bold") + 
  annotate("text", x = 2021, y = max(merge_data_total$cantidad_victimas) * 1.2, label = "Víctimas", color = "#BED5B4", fontface = "bold", size = 5) +
  annotate("text", x = 2021, y = max(merge_data_total$cantidad_victimas) * 1.1, label = "Tasa", color = "#62993E", fontface = "bold", size = 5)

grafico <- ggplotly(gg2)
print(grafico)


##############################################################################
#PUNTO C
#generar tabla generos

tabla_generos<- subset(snic_departamentos_mes_sexo, anio >= 2017 & anio <= 2022)
tabla_generos$provincia_id <- NULL
tabla_generos$departamento_id <- NULL
tabla_generos$codigo_delito_snic_id <- NULL
tabla_generos <- subset(tabla_generos, codigo_delito_snic_nombre =="Homicidios dolosos")
tabla_generos <- subset(tabla_generos,provincia_nombre== "Ciudad Autónoma de Buenos Aires")
tabla_generos <- tabla_generos %>% group_by(anio)%>%
  summarise(Masculino = sum(cantidad_victimas_masc),
            Femenino = sum(cantidad_victimas_fem),)
tabla_generos <-tabla_generos %>%rename(Años= anio)
tabla_generos <- tabla_generos%>%mutate(Total=(Masculino+Femenino))
tabla_generos <- tabla_generos%>%mutate("Masculino Porcentaje"=(Masculino/Total*100),"Femenino Porcentaje"=(Femenino/Total*100))
tabla_generos <- tabla_generos %>%
  select(Años, Masculino, `Masculino Porcentaje`,Femenino,`Femenino Porcentaje`,Total)
tabla_generos <-tabla_generos %>%rename(`Masculino cantidad`= Masculino,`Femenino cantidad`= Femenino)

print(tabla_generos)

##################################################################################

#2-Generar archivo csv


comunas_filtrado <- subset(caba_poblacion_comunas, anio >= 2022)
comunas_filtrado$masc <- NULL
comunas_filtrado$fem <- NULL
comunas_filtrado$superficie <- NULL
comunas_filtrado <-comunas_filtrado %>%rename(poblacion = total)

comunas_snic<- subset(snic_departamentos_mes_sexo, anio >= 2022)
comunas_snic <- subset(comunas_snic, codigo_delito_snic_nombre =="Homicidios dolosos")
comunas_snic <- subset(comunas_snic,provincia_nombre== "Ciudad Autónoma de Buenos Aires")
comunas_snic<- comunas_snic %>% filter(departamento_nombre != "Departamento sin determinar")
comunas_snic <- select(comunas_snic, -provincia_id, -provincia_nombre,-departamento_id,-mes,-codigo_delito_snic_id)
comunas_snic <- select(comunas_snic, -cantidad_victimas_masc, -cantidad_victimas_fem,-cantidad_victimas_sd)
comunas_snic <- comunas_snic %>% group_by(departamento_nombre, anio)%>%
  summarise(total_homicidios = sum(cantidad_hechos),
            total_victimas = sum(cantidad_victimas),)
comunas_snic <-comunas_snic %>%rename(comuna=departamento_nombre )

library(stringr)
comunas_snic$comuna <- str_replace_all(comunas_snic$comuna, "\\bComuna\\b", "")
comunas_snic$comuna <- as.numeric(comunas_snic$comuna)
merge_comunas <- merge(comunas_snic, comunas_filtrado, by.x = "comuna", by.y = "comuna")
comunas_snic$comuna <- as.numeric(comunas_snic$comuna)
class(comunas_snic$comuna)
merge_comunas$comuna <- as.numeric(merge_comunas$comuna)
class(comunas_snic$comuna)
merge_comunas <- select(merge_comunas, -anio.y)
merge_comunas <-merge_comunas %>%rename(anio=anio.x )
merge_comunas <- merge_comunas %>%
  select(anio, comuna,total_homicidios,total_victimas,poblacion)
merge_comunas <- merge_comunas%>%mutate(tasa_victimas_100k=(total_victimas*1000/poblacion)*100)

# NOTA TP. Ruta del archivo. Ver enunciado
#exportar csv
# write.csv(merge_comunas,'../Ton_Vanerio_Nicolas_tpalgoritmos/caba_2022_agrupado.csv', row.names = FALSE)
write.csv(merge_comunas, 'caba_2022_agrupado.csv', row.names = FALSE)
############################################################################

#PUNTO B

library(geojsonio)
library(sf)
library(leaflet)
library(dplyr)

# Leer el archivo geojson de comunas
comunas_geo <- st_read("../datos/comunas.geojson")

# Corregir posibles errores de geometría
comunas_geo <- st_make_valid(comunas_geo)

# data frame con las columnas `comuna` y `total_victimas`
map_data <- left_join(comunas_geo, merge_comunas, by = c("COMUNAS" = "comuna"))

# Crear la paleta de colores
pal <- colorNumeric(palette = "Greens", domain = map_data$total_victimas)

# Crear el mapa
map <- leaflet(data = map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(total_victimas),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    label = ~paste(COMUNAS, "-", "Víctimas Homicidios: valores absolutos", round(total_victimas, 2)),
    group = "Comunas"
  ) %>%
  addLabelOnlyMarkers(
    lng = ~st_coordinates(st_centroid(map_data))[,1],
    lat = ~st_coordinates(st_centroid(map_data))[,2],
    label = ~as.character(total_victimas),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, style = list("color" = "black", "font-weight" = "bold"))
  ) %>%
  addLegend(pal = pal, values = ~total_victimas, opacity = 0.7, title = "Víctimas Homicidios") %>%
  addLayersControl(
    overlayGroups = c("Comunas"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(map)

###PUNTO C

# Crear la paleta de colores para la nueva variable
pal2 <- colorNumeric(palette = "Greens", domain = map_data$tasa_victimas_100k)

# Crear el mapa con la nueva variable
map_vict <- leaflet(data = map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal2(tasa_victimas_100k),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    label = ~paste(COMUNAS, "-", "Homicidios: tasa cada 100.000", round(tasa_victimas_100k, 2)),
    group = "Comunas"
  ) %>%
  addLegend(pal = pal2, values = ~tasa_victimas_100k, opacity = 0.7, title = "Víctimas por mil") %>%
  addLayersControl(
    overlayGroups = c("Comunas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLabelOnlyMarkers(
    lng = ~st_coordinates(st_centroid(map_data))[,1],
    lat = ~st_coordinates(st_centroid(map_data))[,2],
    label = ~paste(round(tasa_victimas_100k, 2)),
    labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, style = list("color" = "black", "font-weight" = "bold"))
  )

print(map_vict)

