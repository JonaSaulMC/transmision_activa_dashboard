# actualizar_dashboard.R

message("🚀 Iniciando actualización del dashboard...")

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
expected_files <- c(
    "mapa_calor_estado.html",
    paste0("cadena_", c(
        "Morelia", "Zamora_de_Hidalgo", "Huetamo_de_Núñez", "Tacámbaro_de_Codallos",
        "Uruapan", "La_Piedad_de_Cabadas", "Apatzingán_de_la_Constitución", "Lázaro_Cárdenas"
    ), ".html"),
    paste0("riesgo_", c(
        "Morelia", "Zamora_de_Hidalgo", "Huetamo_de_Nunez", "Tacambaro_de_Codallos",
        "Uruapan", "La_Piedad_de_Cabadas", "Apatzingan_de_la_Constitucion", "Ciudad_Lazaro_Cardenas"
    ), ".html")
)

existing_files <- list.files("figs", pattern = "\\.html$")
missing_files <- setdiff(expected_files, existing_files)

if (length(missing_files) > 0) {
    message("⚠️ Archivos faltantes en figs/:")
    print(missing_files)
} else {
    message("✅ Todos los archivos HTML esperados están presentes.")
}

# Paso 4: Desplegar en Netlify
message("🌐 Desplegando en Netlify...")
deploy_result <- tryCatch({
    system("netlify deploy --prod --dir=_site", intern = TRUE)
}, error = function(e) {
    stop("❌ Error al desplegar en Netlify: ", e$message)
})
message("✅ Despliegue completado.")

message("🎉 Dashboard actualizado y desplegado correctamente.")
