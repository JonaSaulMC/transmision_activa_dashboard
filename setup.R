# Librerías
library(data.table)
library(lubridate)
library(stringr)
library(sf)
library(tidyr)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(mapview)
library(dplyr)

# Semana epidemiológica
semana_analizada <- isoweek(Sys.Date()) - 1

# Rutas
path_vect <- "C:/Users/JonaSMC/Documents/R-2025/16_Mich"
path_coord <- paste(path_vect, "DescargaOvitrampasMesFco.txt", sep = "/")
path_sinave <- "C:/Users/JonaSMC/Documents/R-2025/Descargas_SINAVE/DENGUE2_10_11_2025.txt"

# Datos
x_raw <- fread(path_sinave, encoding = "Latin-1", quote = "", fill = TRUE)
z <- get(load("C:/Users/JonaSMC/Documents/geocodificacion_dengue_sinave_edo16/3.geocoded_data/dengue_edo16_2025.RData"))

# Paleta
palette <- viridis::viridis

message("setup.R ejecutado sin errores.")
print(paste("Semana analizada:", semana_analizada))

# Carpetas
output_dir <- "figs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Limpiar _site/figs
if (dir.exists("_site/figs")) unlink("_site/figs", recursive = TRUE)
dir.create("_site/figs", recursive = TRUE)

# Función para sanitizar nombres
limpiar_nombre <- function(nombre) {
    nombre <- iconv(nombre, from = "UTF-8", to = "ASCII//TRANSLIT")
    nombre <- gsub("[^[:alnum:]_]", "_", nombre)
    return(nombre)
}

# Localidades
localidades <- c(
    "MORELIA", "ZAMORA DE HIDALGO", "HUETAMO DE NÚÑEZ", "TACÁMBARO DE CODALLOS",
    "URUAPAN", "LA PIEDAD DE CABADAS", "APATZINGÁN DE LA CONSTITUCIÓN", "LÁZARO CÁRDENAS"
)

# Mapa de calor estatal
z_filtrado <- z[!is.na(z$long) & !is.na(z$lat), ]
if (nrow(z_filtrado) > 0) {
    p_estado <- tryCatch({
        leaflet(z_filtrado) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addHeatmap(lng = ~long, lat = ~lat, blur = 20, max = 0.05, radius = 15)
    }, error = function(e) {
        message("❌ Error al generar el mapa estatal: ", e$message)
        NULL
    })
    
    if (!is.null(p_estado) && inherits(p_estado, "leaflet")) {
        p_estado <- p_estado %>%
            addControl(html = paste0("<strong>Semana ", semana_analizada, "</strong>"), position = "topright")
        
        saveWidget(p_estado, file = file.path(output_dir, "mapa_calor_estado.html"), selfcontained = TRUE)
        message("✅ Mapa de calor estatal generado correctamente")
    } else {
        message("⚠️ No se pudo generar el mapa de calor estatal")
    }
} else {
    message("⚠️ No hay datos georreferenciados para generar el mapa estatal")
}

# Cadenas de transmisión
for (loc in localidades) {
    nombre_seguro <- limpiar_nombre(loc)
    
    p <- tryCatch({
        denhotspots::transmission_chains_map(
            geocoded_dataset = z,
            cve_edo = "16",
            locality = loc,
            dengue_cases = "Probable"
        )
    }, error = function(e) {
        message("Error en cadena para ", loc, ": ", e$message)
        NULL
    })
    
    if (!is.null(p) && inherits(p, "mapview")) {
        leaflet_obj <- tryCatch({ p@map }, error = function(e) NULL)
        
        if (!is.null(leaflet_obj)) {
            leaflet_obj <- leaflet_obj %>%
                addControl(html = paste0("<strong>Semana ", semana_analizada, "</strong>"), position = "topright")
            
            saveWidget(leaflet_obj, file = file.path(output_dir, paste0("cadena_", nombre_seguro, ".html")), selfcontained = TRUE)
            message("✅ Cadena guardada para ", loc)
        } else {
            message("⚠️ No se pudo extraer el mapa de cadena para ", loc)
        }
    }
}

# Mapas de riesgo entomológico
localidades_riesgo <- c(
    "Morelia", "Zamora De Hidalgo", "Huetamo De Núñez", "Tacámbaro De Codallos",
    "Uruapan", "La Piedad De Cabadas", "Apatzingán De La Constitución", "Ciudad Lázaro Cárdenas"
)

for (loc in localidades_riesgo) {
    nombre_seguro <- limpiar_nombre(loc)
    
    m <- tryCatch({
        deneggs::eggs_risk(
            path_vect = path_vect,
            path_coord = path_coord,
            weeks = semana_analizada,
            locality = loc,
            risk = FALSE
        )
    }, error = function(e) {
        message("❌ Error en riesgo para ", loc, ": ", conditionMessage(e))
        NULL
    })
    
    if (!is.null(m)) {
        leaflet_obj <- tryCatch({
            if (inherits(m, "leaflet")) {
                m
            } else if (inherits(m, "mapview")) {
                m@map
            } else {
                NULL
            }
        }, error = function(e) {
            message("⚠️ No se pudo extraer el mapa para ", loc, ": ", conditionMessage(e))
            NULL
        })
        
        if (!is.null(leaflet_obj)) {
            leaflet_obj <- leaflet_obj %>%
                addControl(html = paste0("<strong>Semana ", semana_analizada, "</strong>"), position = "topright")
            
            saveWidget(leaflet_obj, file = file.path(output_dir, paste0("riesgo_", nombre_seguro, ".html")), selfcontained = TRUE)
            message("✅ Mapa de riesgo guardado para ", loc)
        }
    }
}

# Mapa de serotipos
palette <- viridisLite::viridis(3)
y <- rgeomex::AGEM_inegi19_mx |> filter(CVE_ENT == "16")

x_serotipo <- x_raw |>
    filter(
        ESTATUS_CASO == 2,
        DENGUE_SER_TRIPLEX %in% 1:4,
        !is.na(DENGUE_SER_TRIPLEX)
    ) |>
    mutate(
        CVE_EDO_REP = str_pad(CVE_EDO_REP, 2, "left", "0"),
        CVE_MPO_REP = str_pad(CVE_MPO_REP, 3, "left", "0")
    ) |>
    count(CVE_EDO_REP, CVE_MPO_REP, DENGUE_SER_TRIPLEX) |>
    pivot_wider(
        names_from = DENGUE_SER_TRIPLEX,
        values_from = n,
        names_prefix = "D",
        values_fill = 0
    ) |>
    mutate(
        D1_binary = ifelse(D1 > 0, 1, 0),
        D2_binary = ifelse(D2 > 0, 1, 0),
        D3_binary = ifelse(D3 > 0, 1, 0),
        n_serotipo = D1_binary + D2_binary + D3_binary,
        D1_text = ifelse(D1 > 0, "D1", ""),
        D2_text = ifelse(D2 > 0, "D2", ""),
        D3_text = ifelse(D3 > 0, "D3", ""),
        serotype_combination = paste(D1_text, D2_text, D3_text, sep = "")
    ) |>
    select(CVE_EDO_REP, CVE_MPO_REP, D1, D2, D3, n_serotipo, serotype_combination) |>
    filter(CVE_EDO_REP == "16")

xy_serotipo <- left_join(
    y,
    x_serotipo,
    by = c("CVE_ENT" = "CVE_EDO_REP", "CVE_MUN" = "CVE_MPO_REP")
) |> filter(!is.na(D1))

colores_serotipos <- c(
    "D1"       = "#0072B2",
    "D2"       = "#D55E00",
    "D3"       = "#009E73",
    "D1D2"     = "#CC79A7",
    "D1D3"     = "#F0E442",
    "D2D3"     = "#56B4E9",
    "D1D2D3"   = "#E69F00"
)

niveles_serotipos <- names(colores_serotipos)

xy_serotipo$serotype_combination <- factor(
    xy_serotipo$serotype_combination,
    levels = niveles_serotipos
)

# Validar combinaciones no previstas
combinaciones_detectadas <- unique(xy_serotipo$serotype_combination)
combinaciones_no_definidas <- setdiff(combinaciones_detectadas, niveles_serotipos)

if (length(combinaciones_no_definidas) > 0) {
    message("⚠️ Combinaciones de serotipos no definidas en la paleta:")
    print(combinaciones_no_definidas)
}

# Generar mapa
mapa_serotipos <- mapview(
    xy_serotipo,
    zcol = "serotype_combination",
    col.regions = colores_serotipos,
    layer.name = "Serotipos por Municipio"
) + mapview(y, col.regions = "gray90", layer.name = "Municipios")

# Agregar semana
mapa_serotipos@map <- mapa_serotipos@map %>%
    addControl(html = paste0("<strong>Semana ", semana_analizada, "</strong>"), position = "topright")

# Guardar mapa y datos
saveWidget(mapa_serotipos@map, file = "figs/mapa_serotipos.html", selfcontained = TRUE)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
save(x_raw, file = "data/x_raw.RData")
save(x_serotipo, file = "data/x_serotipo.RData")
message("✅ x_raw.RData y x_serotipo.RData guardados correctamente.")

# Copiar todos los .html a _site/figs para Netlify
site_figs <- "_site/figs"
if (!dir.exists(site_figs)) dir.create(site_figs, recursive = TRUE)

html_files <- list.files("figs", pattern = "\\.html$", full.names = TRUE)

copiados <- mapply(function(src, dst) {
    file.copy(src, dst, overwrite = TRUE)
}, html_files, file.path(site_figs, basename(html_files)))

if (any(!copiados)) {
    message("⚠️ Algunos archivos .html no se copiaron correctamente:")
    print(basename(html_files[!copiados]))
} else {
    message("✅ Todos los archivos .html fueron copiados correctamente a _site/figs.")
}

message("✅ Archivos .html copiados a _site/figs para Netlify.")

file.copy("figs/banner_inicio.png", "_site/figs/banner_inicio.png", overwrite = TRUE)

