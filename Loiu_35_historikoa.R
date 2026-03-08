library(patchwork)
library(climaemet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(showtext)
library(sysfonts)
library(purrr)


font_add_google(name = "DM Serif Display", family = "dmserif")
font_add_google(name = "DM Sans", "dmsans")
showtext_auto()

options(aemet.timeout = 60)

## Use this function to register your API Key temporarly or permanently
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJncmtvcm1hZXR4ZWFAZ21haWwuY29tIiwianRpIjoiZmZiYzE3M2QtYjkwZi00ODMzLTk0MGUtYjYxYmYxYWEyNDlhIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE3MTUwOTUxMjAsInVzZXJJZCI6ImZmYmMxNzNkLWI5MGYtNDgzMy05NDBlLWI2MWJmMWFhMjQ5YSIsInJvbGUiOiIifQ._zJ7mk3ONt9XeV54jamhIPWamupEMmjwhbiIr1b8xWA")

stations <- aemet_stations()
id_station <- "1082"

# Fechas de inicio y fin
start_all <- as.Date("1950-01-01")
end_all   <- as.Date("2024-12-31")

# Crear secuencia de inicios cada 6 meses
start_dates <- seq(start_all, end_all, by = "6 months")
end_dates   <- c(start_dates[-1] - 1, end_all)

# Función con reintentos
safe_call <- function(id, start, end, tries = 3) {
  for (i in 1:tries) {
    out <- try(aemet_daily_clim(station = id, start = start, end = end), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    message(paste("Fallo en", start, "→ reintentando..."))
    Sys.sleep(3)
  }
  return(NULL)
}

# Descargar en bloques
years <- 1950:2024
data_list <- map(years, function(y) {
  safe_call(id_station, 
            start = as.Date(paste0(y, "-06-01")), 
            end   = as.Date(paste0(y, "-08-31")))
})

# Extraer solo las columnas que nos interesan y unir todo
bilbo_uda <- lapply(data_list, function(df) {df %>%
    select(fecha, tmax, tmin)}) %>%
    bind_rows() %>%
    filter(format(fecha, "%m") %in% c("06", "07", "08"))  # solo junio, julio, agosto


# Añadimos la columna 'año' para agrupar
bilbo_uda <- bilbo_uda %>%
  mutate(año = year(fecha))


# Contamos los días con tmax > 35 y tmin > 20 por año
laburpena <- bilbo_uda %>%
  group_by(año) %>%
  summarise(
    dias_tmax_mayor_35 = sum(tmax > 35, na.rm = TRUE),
    dias_tmax_mayor_30 = sum(tmax > 30, na.rm = TRUE),
    dias_tmin_mayor_20 = sum(tmin > 20, na.rm = TRUE)
  )


plot35 <- ggplot(laburpena, aes(x = año, y = dias_tmax_mayor_35)) +
  geom_col(fill = "tomato") +
  labs(
    title = "35 °C-tik gorako uda-egunak Bilboko aeroportuko estazioan",
    x = "Urtea",
    y = "> 35°C egunak"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot35

plot30 <- ggplot(laburpena, aes(x = año, y = dias_tmax_mayor_30)) +
  geom_col(fill = "tomato") +
  labs(
    title = "30 °C-tik gorako uda-egunak Bilboko aeroportuko estazioan",
    x = "Urtea",
    y = "> 30°C egunak"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot30

plot20 <- ggplot(laburpena, aes(x = año, y = dias_tmin_mayor_20)) +
  geom_col(fill = "tomato") +
  labs(
    title = "20 °C-tik gorako udako gauak Bilboko aeroportuko estazioan",
    x = "Urtea",
    y = "> 20°C gauak"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot20

plot <- plot35 / plot30 / plot20  
plot


# Orain hamarkadaka

df_decada <- laburpena %>%
  mutate(decada = (año %/% 10) * 10)  # Ej: 1995 -> 1990

df_decada <- df_decada %>%
  group_by(decada) %>%
  summarise(
    dias_tmax35 = sum(dias_tmax_mayor_35, na.rm = TRUE),
    dias_tmax30 = sum(dias_tmax_mayor_30, na.rm = TRUE),
    dias_tmin20 = sum(dias_tmin_mayor_20, na.rm = TRUE)
  )

# Grafika hamarkadaka

p1 <- ggplot(df_decada, aes(x = factor(decada), y = dias_tmax35)) +
  geom_col(fill = "tomato") +
  labs(x = "Década", y = "Días tmax>35", title = "Días con tmax > 35ºC") +
  theme_minimal()
 
p2 <- ggplot(df_decada, aes(x = factor(decada), y = dias_tmax30)) +
  geom_col(fill = "tomato") +
  labs(x = "Década", y = "Días tmax>30", title = "Días con tmax > 30ºC") +
  theme_minimal()

p3 <- ggplot(df_decada, aes(x = factor(decada), y = dias_tmin20)) +
  geom_col(fill = "steelblue") +
  labs(x = "Década", y = "Días tmin>20", title = "Días con tmin > 20ºC") +
  theme_minimal()


p <- p1 / p2 / p3
p
