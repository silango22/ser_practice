idw_para_munis <- function(df, munis, power, max_radius, max_vecinos) {
  # 1. Preparar los datos espaciales
  puntos <- df %>%
    st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%
    st_transform(32719)  # UTM 19S para cálculos en metros
  
  # Convertir munis a sf (asumiendo que tienes columnas lat/lon)
  puntos_munis <- munis %>%
    separate(municipalidad, into = c("lat", "lon"), sep = ",\\s*", convert = TRUE) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(32719)
  
  # Identificar columnas de fecha
  fechas <- names(df) %>%
    str_subset("^\\d{4}-\\d{2}-\\d{2}$")
  
  # Función para procesar una fecha
  interpolar_fecha <- function(fecha) {
    # Filtrar puntos con datos para esta fecha
    datos_fecha <- puntos %>%
      filter(!is.na(.data[[fecha]])) %>%
      mutate(valor = .data[[fecha]])
    
    if (nrow(datos_fecha) == 0) return(rep(NA, nrow(puntos_munis)))
    
    # Crear modelo IDW
    modelo <- gstat(
      formula = valor ~ 1,
      locations = datos_fecha,
      nmax = max_vecinos,        # Número máximo de vecinos
      maxdist = max_radius,      # Radio máximo de búsqueda (metros)
      set = list(idp = power)    # Parámetro de potencia
    )
    
    # Predecir en los puntos de municipalidades
    predict(modelo, puntos_munis)$var1.pred
  }
  
  # Aplicar a todas las fechas (en paralelo para mayor velocidad)
  resultados <- map_dfc(fechas, ~tibble(!!.x := interpolar_fecha(.x)))
  
  # Combinar con los datos originales de municipalidades
  bind_cols(munis, resultados)
}
