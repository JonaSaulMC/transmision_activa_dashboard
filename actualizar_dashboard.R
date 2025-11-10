# actualizar_dashboard.R

message("🚀 Iniciando actualización del dashboard...")

# Semana epidemiológica actual
library(stringr)
library(lubridate)

semana_analizada <- isoweek(Sys.Date()) - 1

# Función para sanitizar nombres
limpiar_nombre <- function(nombre) {
    nombre <- iconv(nombre, from = "UTF-8", to = "ASCII//TRANSLIT")
    nombre <- tolower(nombre)
    nombre <- gsub("[^a-z0-9]+", "_", nombre)
    nombre <- gsub("_+", "_", nombre)
    nombre <- gsub("^_|_$", "", nombre)
    return(nombre)
}

# Paso 1: Ejecutar setup.R
message("🔧 Ejecutando setup.R...")
tryCatch({
    source("setup.R", encoding = "UTF-8")
    message("✅ setup.R ejecutado correctamente.")
}, error = function(e) {
    stop("❌ Error al ejecutar setup.R: ", e$message)
})

# Paso 2: Renderizar documentos Quarto
message("🧵 Renderizando documentos Quarto...")
render_result <- tryCatch({
    system("quarto render", intern = TRUE)
}, error = function(e) {
    stop("❌ Error al renderizar documentos Quarto: ", e$message)
})
message("✅ Renderización completada.")

# Paso 3: Validar archivos HTML generados
message("🔍 Validando archivos HTML en figs/...")

html_files <- list.files("figs", pattern = "\\.html$", full.names = TRUE)

# Validar que contengan la semana actual
validos <- sapply(html_files, function(path) {
    contenido <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
    if (is.null(contenido)) return(FALSE)
    any(grepl(paste0("Semana ", semana_analizada), contenido))
})

archivos_validos <- basename(html_files[validos])
archivos_invalidos <- basename(html_files[!validos])

if (length(archivos_invalidos) > 0) {
    message("⚠️ Archivos generados pero con semana incorrecta (no se desplegarán):")
    print(archivos_invalidos)
}

if (length(archivos_validos) > 0) {
    message("✅ Archivos válidos con semana ", semana_analizada, ":")
    print(archivos_validos)
} else {
    stop("❌ Ningún archivo válido contiene la semana actual. Revisa setup.R.")
}

# Paso 4: Verificar que estén en _site/figs
site_figs <- "_site/figs"
copiados <- archivos_validos %in% list.files(site_figs)

if (any(!copiados)) {
    message("⚠️ Archivos válidos que no están en _site/figs:")
    print(archivos_validos[!copiados])
} else {
    message("✅ Todos los archivos válidos están presentes en _site/figs.")
}

# Paso 5: Desplegar en Netlify
message("🌐 Desplegando en Netlify...")
deploy_result <- tryCatch({
    system("netlify deploy --prod --dir=_site", intern = TRUE)
}, error = function(e) {
    stop("❌ Error al desplegar en Netlify: ", e$message)
})
message("✅ Despliegue completado.")

message("🎉 Dashboard actualizado y desplegado correctamente.")