#Eficiencia en Microfinancieras

################## Librerias
pacman::p_load(readxl, forecast, tseries, TSA, urca, ggplot2,
               dplyr, stats, seasonal, janitor, lubridate, timetk,
               astsa, zoo, tidyr, deaR, Benchmarking)

######## -- Procesamiento de la Informacion ------

##### ----------- Variable 1: Activos
#Variable de stock mensual
df_activos <- read_excel("base.xlsx", sheet = "activos", col_names = TRUE) %>% 
  clean_names() %>% 
  mutate(
    fecha = ym(paste(anio, mes, sep = "-"))
  )

#seleccinando columnas de firmas
firm_cols_activos <- setdiff(names(df_activos), c("anio", "mes", "trimestre",
                                                  "anio_mes", "fecha"))

#pivotear a formato largo (firma, activos)
df_largo_activos <- df_activos %>% 
  pivot_longer(
    cols = all_of(firm_cols_activos),
    names_to = "firma",
    values_to = "activos"
  ) %>% 
  mutate(
    activos = ifelse(is.na(activos), 0, activos)
  ) %>% 
  select(fecha, firma, activos) %>% 
  arrange(fecha, firma)

##### ----------- Variable 2: Cartera de Créditos Neta
#Variable de stock mensual

df_cartera <- read_excel("base.xlsx", sheet = "cartera", col_names = TRUE) %>% 
  clean_names() %>% 
  mutate(
    fecha = ym(paste(anio, mes, sep = "-"))
  )

#seleccinando columnas de firmas
firm_cols_cartera <- setdiff(names(df_cartera), c("anio", "mes",
                                  "trimestre", "anio_mes", "fecha"))

#pivotear a formato largo (firma, cartera)
df_largo_cartera <- df_cartera %>% 
  pivot_longer(
    cols = all_of(firm_cols_cartera),
    names_to = "firma",
    values_to = "cartera"
  ) %>% 
  mutate(
    cartera = ifelse(is.na(cartera), 0, cartera)
  ) %>% 
  select(fecha, firma, cartera) %>% 
  arrange(fecha, firma)

##### ----------- Variable 3: Ingresos Financieros
#Variable de flujo mensual
df_ingresos <- read_excel("base.xlsx", sheet = "ingresos", col_names = TRUE) %>% 
  clean_names() %>% 
  mutate(
    fecha = ym(paste(anio, mes, sep = "-"))
  )

#seleccinando columnas de firmas
firm_cols_ingresos <- setdiff(names(df_ingresos), c("anio", "mes",
                                                  "trimestre", "anio_mes", "fecha"))

#pivotear a formato largo (firma, cartera)
df_largo_ingresos <- df_ingresos %>% 
  pivot_longer(
    cols = all_of(firm_cols_ingresos),
    names_to = "firma",
    values_to = "ingresos"
  ) %>% 
  mutate(
    ingresos = ifelse(is.na(ingresos), 0, ingresos)
  ) %>% 
  select(fecha, firma, ingresos) %>% 
  arrange(fecha, firma)


##### ----------- Variable 4: Gastos Financieros 
#Variable de flujo mensual
df_gastos_f <- read_excel("base.xlsx", sheet = "gastos_f", col_names = TRUE) %>% 
  clean_names() %>% 
  mutate(
    fecha = ym(paste(anio, mes, sep = "-"))
  )

#seleccinando columnas de firmas
firm_cols_gastos_f <- setdiff(names(df_gastos_f), c("anio", "mes",
                                                    "trimestre", "anio_mes", "fecha"))

#pivotear a formato largo (firma, cartera)
df_largo_gastos_f <- df_gastos_f %>% 
  pivot_longer(
    cols = all_of(firm_cols_gastos_f),
    names_to = "firma",
    values_to = "gastos_f"
  ) %>% 
  mutate(
    gastos_f = ifelse(is.na(gastos_f), 0, gastos_f)
  ) %>% 
  select(fecha, firma, gastos_f) %>% 
  arrange(fecha, firma)


##### ----------- Varible 5: Gastos Administrativos
#Variable de flujo mensual

df_gastos_ad <- read_excel("base.xlsx", sheet = "gastos_ad", col_names = TRUE) %>% 
  clean_names() %>% 
  mutate(
    fecha = ym(paste(anio, mes, sep = "-"))
  )

#seleccinando columnas de firmas
firm_cols_gastos_ad <- setdiff(names(df_gastos_ad), c("anio", "mes",
                                                    "trimestre", "anio_mes", "fecha"))

#pivotear a formato largo (firma, gastos_administrativos)
df_largo_gastos_ad <- df_gastos_ad %>% 
  pivot_longer(
    cols = all_of(firm_cols_gastos_ad),
    names_to = "firma",
    values_to = "gastos_ad"
  ) %>% 
  mutate(
    gastos_ad = ifelse(is.na(gastos_ad), 0, gastos_ad)
  ) %>% 
  select(fecha, firma, gastos_ad) %>% 
  arrange(fecha, firma)

##### ----------- Variable 6: Gastos Operativos
#Variable de flujo mensual

df_gastos_op <- read_excel("base.xlsx", sheet = "gastos_op", col_names = TRUE) %>% 
  clean_names() %>% 
  mutate(
    fecha = ym(paste(anio, mes, sep = "-"))
  )

#seleccinando columnas de firmas
firm_cols_gastos_op <- setdiff(names(df_gastos_op), c("anio", "mes",
                                                      "trimestre", "anio_mes", "fecha"))

#pivotear a formato largo (firma, gastos_administrativos)
df_largo_gastos_op <- df_gastos_op %>% 
  pivot_longer(
    cols = all_of(firm_cols_gastos_op),
    names_to = "firma",
    values_to = "gastos_op"
  ) %>% 
  mutate(
    gastos_op = ifelse(is.na(gastos_op), 0, gastos_op)
  ) %>% 
  select(fecha, firma, gastos_op) %>% 
  arrange(fecha, firma)

######creando solo una base de datos
df_final <- full_join(df_largo_activos, df_largo_cartera,
                      by = c("fecha", "firma"))

df_master <- df_final %>% 
  mutate(
    activos = replace_na(activos, 0),
    cartera = replace_na(cartera, 0)
  )

keys <- df_master %>% distinct(fecha, firma)

#Filtra cada base al universo de llaves (fecha, firma) de la maestra
df_largo_ingresos_f  <- df_largo_ingresos  %>% semi_join(keys, by = c("fecha","firma"))
df_largo_gastos_f_f  <- df_largo_gastos_f  %>% semi_join(keys, by = c("fecha","firma"))
df_largo_gastos_ad_f <- df_largo_gastos_ad %>% semi_join(keys, by = c("fecha","firma"))
df_largo_gastos_op_f <- df_largo_gastos_op %>% semi_join(keys, by = c("fecha","firma"))

#Creando base de datos final
df_final <- df_master %>%
  left_join(df_largo_ingresos_f,  by = c("fecha","firma")) %>%
  left_join(df_largo_gastos_f_f,  by = c("fecha","firma")) %>%
  left_join(df_largo_gastos_ad_f, by = c("fecha","firma")) %>%
  left_join(df_largo_gastos_op_f, by = c("fecha","firma")) %>%
  # Reemplaza NAs de las nuevas métricas por 0
  mutate(
    across(-c(fecha, firma, activos, cartera), ~ replace_na(.x, 0))
  ) %>%
  arrange(fecha, firma)

# Vista rápida
glimpse(df_final)
head(df_final)

#Vista rapida de relacion ingresos/activos y cartera/activos
ggplot(df_final, aes(ingresos/activos, cartera/activos, color = firma)) +
  geom_point()+
  geom_smooth(method = "lm", se = F)


######## -- Estructura de codigo para eficiencia ------
#1.Punto de partida, df_final:variables mensuales, stock y flujos acumulados
df <- df_final %>%
  mutate(
    fecha = as.Date(fecha, format = "%d/%m/%Y")
  ) %>%
  arrange(firma, fecha)

# Completar calendario mensual por firma
rango <- tibble(fecha = seq(min(df$fecha, na.rm = TRUE),
                            max(df$fecha, na.rm = TRUE), by = "month"))

df <- df %>%
  complete(firma, fecha = rango$fecha) %>%
  arrange(firma, fecha)

#2.Variable de cierre para detectar filas sin reporte
#si cierre es igual a cero, se interpreta "no hubo reporte"
#si cierre es mayor a cero, dejamos los ceros como ceros reales

value_cols <- c("activos","cartera","ingresos","gastos_f","gastos_ad","gastos_op")

df <- df %>%
  mutate(
    cierre = rowSums(across(all_of(value_cols), ~ replace_na(., 0)), na.rm = TRUE),
    across(all_of(value_cols),
           ~ if_else(cierre == 0, NA_real_, .x)) # filas sin reporte -> NA total
  )

#Identificadores de tiempo
df <- df %>%
  mutate(
    anio = year(fecha),
    mes  = month(fecha)
  )

#3.Flujos acumuladso pasan a flujos mensuales, por firma-año
#maneja "resets" (cuando baja el acumulado dentro del año)

to_monthly <- function(x) {
  lx <- dplyr::lag(x, default = 0)
  d  <- x - lx
  # si el acumulado baja (reset/ajuste), el flujo del mes es el valor actual
  d  <- ifelse(!is.na(x) & !is.na(lx) & x < lx, x, d)
  # si no hay dato (NA) mantenemos NA (no inventamos flujo)
  d[is.na(x)] <- NA_real_
  d
}

flow_acum <- c("ingresos","gastos_f","gastos_ad","gastos_op")

df <- df %>%
  group_by(firma, anio) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(across(all_of(flow_acum), to_monthly, .names = "{.col}_m")) %>%
  ungroup()

#4.stocks-trtamiento de faltantes
#no usar 0 (por la carencia de sentido en variables de stock)
#usamos LOCF + backfill dentro de cada firma para continuidad

df <- df %>%
  group_by(firma) %>%
  arrange(fecha, .by_group = TRUE) %>%
  mutate(
    activos = na.locf(activos, na.rm = FALSE),
    cartera = na.locf(cartera, na.rm = FALSE),
    activos = na.locf(activos, fromLast = TRUE, na.rm = FALSE),
    cartera = na.locf(cartera, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

#5.construccion de gastos (operativos) como gastos-ad + gastos_op (mensual)

df <- df %>%
  mutate(
    gastos_oper_m = gastos_ad_m + gastos_op_m   # mensual ya convertido
  )

#6.Agregacion trimestral
#stocks seran en promedio trimestral
#flujos mensuales seran en suma trimestral
df_q <- df %>%
  mutate(trimestre = quarter(fecha)) %>%
  group_by(firma, anio, trimestre) %>%
  summarise(
    # STOCKS (promedio)
    activos_qavg = mean(activos, na.rm = TRUE),
    cartera_qavg = mean(cartera, na.rm = TRUE),
    
    # FLUJOS (suma de *_m)
    ingresos_qsum      = sum(ingresos_m,   na.rm = TRUE),
    gastos_f_qsum      = sum(gastos_f_m,   na.rm = TRUE),
    gastos_ad_qsum     = sum(gastos_ad_m,  na.rm = TRUE),
    gastos_op_qsum     = sum(gastos_op_m,  na.rm = TRUE),
    gasto_operativo_qsum = sum(gastos_oper_m, na.rm = TRUE),
    
    # Métrica útil
    n_meses = n(),
    .groups = "drop"
  )

# Marcador de trimestre “vacío”: sin stocks y sin flujos
df_q <- df_q %>%
  mutate(
    trimestre_vacio = if_else(
      is.na(activos_qavg) & is.na(cartera_qavg) &
        ingresos_qsum == 0 & gastos_f_qsum == 0 &
        gastos_ad_qsum == 0 & gastos_op_qsum == 0, TRUE, FALSE
    )
  )

#base de datos excluyendo los trimestres vacios por firma
df_q_valid <- df_q %>% filter(!trimestre_vacio)

#7.Base para analisis DEA (desempeño financiero con orientacion a inputs)
#inputs: gasto_operativo_qsum, gastos_f_qsum, activos_qavg
#outputs: cartera_qavg, ingresos_qsum
df_dea_fin <- df_q_valid %>%
  select(
    firma, anio, trimestre,
    gasto_operativo_qsum, gastos_f_qsum, activos_qavg,
    cartera_qavg, ingresos_qsum
  )

df_dea_fin %>%
  filter(cartera_qavg < 0) %>% #la firma tiene valores abismales
  ggplot(aes(cartera_qavg, gastos_f_qsum, color = firma))+
  geom_point()
  #geom_smooth(method = "lm", se = F)

######## -- Eficiencia tecnica ------

#### Modelo BCC (VRS) orientado a inputs

#1.preparacion de datos
#filtramos las firla sin datos relevantes
df_ready <- df_dea_fin %>% 
  filter(
    !is.na(gasto_operativo_qsum),
    !is.na(gastos_f_qsum),
    !is.na(activos_qavg),
    !is.na(cartera_qavg),
    !is.na(ingresos_qsum)
  )

#Calculo del Modelo
# 1) Filtra y estabiliza (evita explosiones por outputs ~0)
eps <- 1e-9
dea_base <- df_ready %>%
  mutate(
    sum_inputs  = gasto_operativo_qsum + gastos_f_qsum + activos_qavg,
    sum_outputs = cartera_qavg + ingresos_qsum
  ) %>%
  filter(
    is.finite(gasto_operativo_qsum), is.finite(gastos_f_qsum), is.finite(activos_qavg),
    is.finite(cartera_qavg), is.finite(ingresos_qsum),
    sum_inputs > 0
  ) %>%
  mutate(
    cartera_qavg  = ifelse(cartera_qavg  < eps, eps, cartera_qavg),
    ingresos_qsum = ifelse(ingresos_qsum < eps, eps, ingresos_qsum)
  )

# 2) Matrices X (inputs) e Y (outputs)
X <- as.matrix(dea_base %>% select(gasto_operativo_qsum, gastos_f_qsum, activos_qavg))
Y <- as.matrix(dea_base %>% select(cartera_qavg, ingresos_qsum))

# 3) DEA VRS orientado a outputs
# Nota: en Benchmarking, 'eff' aquí es φ (factor de expansión de outputs), con φ >= 1.
dea_fit <- dea(X = X, Y = Y, RTS = "vrs", ORIENTATION = "out")
phi <- dea_fit$eff

# 4) Llevar a escala [0,1]
eficiencia_01 <- 1 / pmax(phi, 1)  # 1 = eficiente; 0<…<1 ineficiente
eficiencia_01[!is.finite(eficiencia_01)] <- 0

# 5) Resultado final unido a df_ready
res_dea <- dea_base %>%
  mutate(
    eficiencia_phi = phi,
    eficiencia_01  = round(eficiencia_01, 6)
  ) %>%
  select(firma, anio, trimestre, eficiencia_01, eficiencia_phi)

df_ready <- df_ready %>%
  left_join(res_dea, by = c("firma","anio","trimestre"))

attach(df_ready)

head(df_ready)


df_ready %>% 
  group_by(firma) %>% 
  summarize(eficiencia_promedio = mean(eficiencia_01),
            activos_promedio = mean(activos_qavg),
            cartera_promedio = mean(cartera_qavg))
