################################################################################
# Índice Salarial Real por Hora Trabajada — Argentina 2016–2026
# ==============================================================
# Construye un índice salarial real por hora a partir de microdatos de la EPH
# (INDEC) y lo contrasta con los índices oficiales del INDEC.
#
# Hipótesis de trabajo:
#   El empleo asalariado no registrado (informal) tradicional muestra salarios
#   por hora estancados o en baja real. Sin embargo, el arribo de plataformas
#   tipo Uber, Rappi, PedidosYa, etc. incorpora al "empleo informal" a personas
#   que antes no estaban ocupadas (o lo estaban en otro sector). Como los
#   ingresos de estas plataformas suelen ser mayores que los del cuentapropismo
#   tradicional de baja calificación, el promedio del salario informal se
#   "infla" estadísticamente — no por una mejora genuina del trabajo informal
#   tradicional, sino por un efecto composición (cambio en la mezcla de
#   quiénes integran el sector informal).
#
#   Adicionalmente, muchas de estas personas son pluriempleadas (tienen un
#   trabajo formal + changas en apps), lo que también distorsiona el indicador
#   al sumar ingresos de distintas fuentes.
#
# Fuentes:
#   - EPH (INDEC) vía paquete {eph}     (https://github.com/ropensci/eph)
#   - IPC Nacional Nivel General  vía API Series de Tiempo (datos.gob.ar)
#   - Índice de Salarios (INDEC)  vía API Series de Tiempo
#   - SIPA promedio privado        vía API Series de Tiempo (id 153.1_*)
#
# Requisitos: R >= 4.1, paquetes: eph, tidyverse, lubridate, scales
# Uso: Rscript index_salarial_hora.R
#   Primera ejecución: descarga ~44 trimestres EPH (~10–15 min).
#   Ejecuciones posteriores usan caché (eph_cache_full.rds).
################################################################################

# ---- 0. Librerías ----
required <- c("eph", "tidyverse", "lubridate", "scales")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(eph)
library(tidyverse)
library(lubridate)
library(scales)

# ---- 1. Parámetros ----
first_year   <- 2016
last_year    <- 2026
ipc_mensual_2016 <- 1.018   # variación mensual estimada para ene-mar 2016

# Año de referencia para los coeficientes fijos de subdeclaración (Albina et al.)
ref_year <- 2022
# Coeficientes de corrección en el año de referencia (fuente: Equilibra DT-8)
# Representan: ingreso_real = ingreso_declarado * coeficiente
corr_ref <- c(asal_reg    = 1.05,
              asal_noreg  = 1.35,
              cuenta_prop = 1.50)

output_rds <- "datos_procesados.rds"
dir.create("resultados", showWarnings = FALSE)

# ---- 2. Funciones auxiliares ----
fetch_ts_api <- function(ids, names_vec) {
  url <- sprintf(
    "https://apis.datos.gob.ar/series/api/series/?ids=%s&limit=500&format=csv",
    paste(ids, collapse = ",")
  )
  df <- tryCatch(read.csv(url), error = function(e) {
    stop("Fallo al descargar ", url, "\n", e$message)
  })
  names(df)[-1] <- names_vec
  df$indice_tiempo <- as.Date(df$indice_tiempo)
  df
}

quarterly_avg <- function(df) {
  df %>%
    mutate(
      year    = year(indice_tiempo),
      quarter = quarter(indice_tiempo)
    ) %>%
    group_by(year, quarter) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}

# ---- 3. IPC (Índice de Precios al Consumidor) ----
cat("Descargando IPC ...\n")

# IPC Nacional Nivel General (base dic-2016 = 100) — mensual desde dic-2016
ipc_nac <- fetch_ts_api("148.3_INIVELNAL_DICI_M_26", "ipc_nac")

# IPC-GBA Nivel General (base dic-2016 = 100) — mensual desde abr-2016
ipc_gba <- fetch_ts_api("103.1_I2N_2016_M_19", "ipc_gba")

ipc_gba_abr <- ipc_gba$ipc_gba[ipc_gba$indice_tiempo == as.Date("2016-04-01")]

meses_2016 <- tibble(
  indice_tiempo = seq(as.Date("2016-01-01"), as.Date("2016-12-01"), by = "month")
)
ipc <- meses_2016 %>%
  left_join(ipc_gba, by = "indice_tiempo") %>%
  left_join(ipc_nac, by = "indice_tiempo") %>%
  mutate(
    indice = coalesce(ipc_nac, ipc_gba),
    indice = if_else(is.na(indice),
                     ipc_gba_abr / (ipc_mensual_2016 ^ (4 - month(indice_tiempo))),
                     indice)
  ) %>%
  select(indice_tiempo, indice) %>%
  bind_rows(ipc_nac %>% filter(indice_tiempo > as.Date("2016-12-01")) %>% rename(indice = ipc_nac)) %>%
  arrange(indice_tiempo)

ipc_q <- ipc %>%
  mutate(year = year(indice_tiempo), quarter = quarter(indice_tiempo)) %>%
  group_by(year, quarter) %>%
  summarise(ipc = mean(indice), .groups = "drop")

cat(sprintf("  IPC último mes: %s = %.2f\n",
    format(max(ipc$indice_tiempo), "%Y-%m"), tail(ipc$indice, 1)))

# ---- 4. Índice de Salarios oficial (INDEC) ----
cat("Descargando Índice de Salarios oficial ...\n")

indec_sal <- fetch_ts_api(
  c("149.1_TL_INDIIOS_OCTU_0_21",
    "149.1_TL_REGIADO_OCTU_0_16",
    "149.1_SOR_PRIADO_OCTU_0_25",
    "149.1_SOR_PUBICO_OCTU_0_14",
    "149.1_SOR_PRIADO_OCTU_0_28"),
  c("total", "registrado", "reg_privado", "reg_publico", "no_reg_privado")
)

indec_sal_q <- quarterly_avg(indec_sal) %>%
  rename(is_total = total, is_reg = registrado,
         is_reg_priv = reg_privado, is_reg_pub = reg_publico,
         is_noreg_priv = no_reg_privado)

# ---- 5. SIPA: salario promedio registrado privado (Albina et al.) ----
cat("Descargando SIPA (remuneración promedio sector privado registrado) ...\n")

sipa <- fetch_ts_api(
  "153.1_RNERACIDIO_2009_M_21",
  "sipa_nominal"
)

sipa_q <- quarterly_avg(sipa) %>% rename(sipa_nominal = sipa_nominal)

cat(sprintf("  SIPA último mes: %s = $%.0f\n",
    format(max(sipa$indice_tiempo), "%Y-%m"), tail(sipa$sipa_nominal, 1)))

# ---- 6. EPH: descarga de microdatos ----
cat("Descargando EPH (2016–2026). Primera vez: ~10–15 min ...\n")

vars_eph <- c(
  "CODUSU", "NRO_HOGAR", "COMPONENTE",
  "ANO4", "TRIMESTRE", "REGION", "AGLOMERADO",
  "PONDERA",
  "CH04", "CH06",
  "ESTADO", "CAT_OCUP",
  "PP3E_TOT", "PP3F_TOT",
  "P21", "TOT_P12",
  "PP07H", "PP07I",
  "PP03D",
  "PP04A"       # 1=estatal/pública, 2=privada (no disponible en 2016)
)

eph_raw <- get_microdata(
  year     = first_year:last_year,
  period   = 1:4,
  type     = "individual",
  vars     = vars_eph,
  destfile = "eph_cache_full.rds"
)
if ("microdata" %in% names(eph_raw)) {
  eph_raw <- tidyr::unnest(eph_raw, microdata)
}

# ---- 7. Procesamiento base ----
cat("Procesando EPH ...\n")

eph <- eph_raw %>%
  filter(ESTADO == 1) %>%
  filter(CAT_OCUP %in% c(2, 3)) %>%
  filter(P21 > 0 & !is.na(P21)) %>%
  filter(PP3E_TOT > 0 & !is.na(PP3E_TOT))

eph <- eph %>%
  mutate(
    formal = case_when(
      CAT_OCUP == 3 & PP07H == 1 ~ TRUE,
      CAT_OCUP == 3 & PP07H == 2 ~ FALSE,
      CAT_OCUP == 2 & PP07I == 1 ~ TRUE,
      CAT_OCUP == 2 & PP07I == 2 ~ FALSE,
      TRUE ~ NA
    ),
    sector_lab = case_when(
      CAT_OCUP == 3 & formal == TRUE & coalesce(PP04A, 2) == 2 ~ "Privado Formal",
      CAT_OCUP == 3 & formal == TRUE & coalesce(PP04A, 2) == 1 ~ "Público Formal",
      CAT_OCUP == 3 & formal == FALSE                          ~ "Informal",
      CAT_OCUP == 2                                             ~ "Cuenta Propia",
      TRUE                                                       ~ NA_character_
    ),
    priv_reg = CAT_OCUP == 3 & formal == TRUE & coalesce(PP04A, 2) == 2,
    pluriempleo = coalesce(PP03D, 1) > 1,
    horas_sem   = PP3E_TOT + coalesce(PP3F_TOT, 0),
    ingreso_mensual = P21 + coalesce(TOT_P12, 0),
    salario_hora = ingreso_mensual / (horas_sem * 4.33),
    year   = ANO4,
    quarter = TRIMESTRE
  ) %>%
  filter(salario_hora > 0,
         salario_hora < quantile(salario_hora, 0.98, na.rm = TRUE))

# ---- 8. Corrección móvil por subdeclaración (SIPA/EPH) ----
cat("Estimando factores de corrección móviles (SIPA vs EPH) ...\n")

# 8a. Salario promedio EPH formal-privado por trimestre
eph_sipa <- eph %>%
  filter(priv_reg) %>%
  group_by(year, quarter) %>%
  summarise(
    eph_formal_priv = weighted.mean(ingreso_mensual, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  )

# 8b. Combinar con SIPA
sipa_corr <- sipa_q %>%
  left_join(eph_sipa, by = c("year", "quarter")) %>%
  filter(!is.na(eph_formal_priv)) %>%
  mutate(
    ratio_sipa_eph = sipa_nominal / eph_formal_priv
  )

# 8c. Ratio de referencia (promedio 2022)
ratio_ref <- sipa_corr %>%
  filter(year == ref_year) %>%
  summarise(ratio_ref = mean(ratio_sipa_eph, na.rm = TRUE)) %>%
  pull(ratio_ref)

cat(sprintf("  Ratio SIPA/EPH formal-priv en %d: %.4f\n", ref_year, ratio_ref))

# 8d. Construir tabla de factores de corrección por trimestre
#     corr_formal_t  = 1 + (corr_ref["asal_reg"] - 1) * (ratio_t / ratio_ref)
#     corr_informal_t = 1 + k_inf * (corr_formal_t - 1)
#     corr_cp_t       = 1 + k_cp  * (corr_formal_t - 1)
k_inf <- (corr_ref["asal_noreg"] - 1) / (corr_ref["asal_reg"] - 1)
k_cp  <- (corr_ref["cuenta_prop"] - 1) / (corr_ref["asal_reg"] - 1)

factores <- sipa_corr %>%
  mutate(
    corr_formal  = 1 + (corr_ref["asal_reg"] - 1) * (ratio_sipa_eph / ratio_ref),
    corr_informal = 1 + k_inf * (corr_formal - 1),
    corr_cp       = 1 + k_cp  * (corr_formal - 1)
  ) %>%
  select(year, quarter, corr_formal, corr_informal, corr_cp)

# Para trimestres sin dato SIPA (e.g., 2016 Q1-Q3), usar el primer disponible
factores <- factores %>%
  tidyr::fill(corr_formal, corr_informal, corr_cp, .direction = "downup")

cat(sprintf("  Factores (último): formal=%.3f, informal=%.3f, cp=%.3f\n",
    tail(factores$corr_formal, 1),
    tail(factores$corr_informal, 1),
    tail(factores$corr_cp, 1)))

# 8e. Aplicar factores a cada observación
eph <- eph %>%
  left_join(factores, by = c("year", "quarter")) %>%
  mutate(
    factor_subdecl = case_when(
      sector_lab == "Privado Formal"        ~ corr_formal,
      sector_lab == "Público Formal"        ~ corr_formal,
      sector_lab == "Informal"              ~ corr_informal,
      sector_lab == "Cuenta Propia"         ~ corr_cp,
      TRUE                                  ~ 1
    ),
    ingreso_corregido    = ingreso_mensual * factor_subdecl,
    salario_hora_corregido = ingreso_corregido / (horas_sem * 4.33)
  )

# ---- 9. Índices salariales por hora (EPH) ----
cat("Construyendo índices ...\n")

# 9a. Índice sin corregir — por sector y total
sectores <- c("Privado Formal", "Público Formal", "Informal", "Cuenta Propia")

eph_q <- eph %>%
  filter(!is.na(sector_lab)) %>%
  group_by(year, quarter, sector_lab) %>%
  summarise(
    n = n(),
    sal_hora_pond = weighted.mean(salario_hora, w = PONDERA, na.rm = TRUE),
    ing_mensual_pond = weighted.mean(ingreso_mensual, w = PONDERA, na.rm = TRUE),
    horas_prom = weighted.mean(horas_sem, w = PONDERA, na.rm = TRUE),
    tasa_pluriempleo = weighted.mean(pluriempleo, w = PONDERA, na.rm = TRUE) * 100,
    .groups = "drop"
  )

eph_long <- eph_q %>%
  mutate(sector_lab = case_when(
    sector_lab == "Privado Formal" ~ "Privado_Formal",
    sector_lab == "Público Formal" ~ "Publico_Formal",
    sector_lab == "Informal"       ~ "Informal",
    sector_lab == "Cuenta Propia"  ~ "Cuenta_Propia",
    TRUE ~ sector_lab
  ))

eph_wide <- eph_long %>%
  pivot_wider(
    id_cols = c(year, quarter),
    names_from = sector_lab,
    values_from = c(sal_hora_pond, ing_mensual_pond, horas_prom, tasa_pluriempleo)
  )

eph_total <- eph %>%
  group_by(year, quarter) %>%
  summarise(
    sal_hora_total   = weighted.mean(salario_hora, w = PONDERA, na.rm = TRUE),
    ing_mens_total   = weighted.mean(ingreso_mensual, w = PONDERA, na.rm = TRUE),
    horas_total      = weighted.mean(horas_sem, w = PONDERA, na.rm = TRUE),
    tasa_pluriempleo = weighted.mean(pluriempleo, w = PONDERA, na.rm = TRUE) * 100,
    pct_informal     = weighted.mean(sector_lab == "Informal" | sector_lab == "Cuenta Propia", w = PONDERA, na.rm = TRUE) * 100,
    .groups = "drop"
  )

eph_idx <- eph_wide %>%
  left_join(eph_total, by = c("year", "quarter")) %>%
  left_join(ipc_q, by = c("year", "quarter")) %>%
  left_join(indec_sal_q, by = c("year", "quarter"))

# 9b. Índice corregido
eph_q_corr <- eph %>%
  filter(!is.na(sector_lab)) %>%
  group_by(year, quarter, sector_lab) %>%
  summarise(
    sal_hora_corr_pond = weighted.mean(salario_hora_corregido, w = PONDERA, na.rm = TRUE),
    ing_mens_corr_pond = weighted.mean(ingreso_corregido, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  )

eph_corr_long <- eph_q_corr %>%
  mutate(sector_lab = case_when(
    sector_lab == "Privado Formal" ~ "Privado_Formal",
    sector_lab == "Público Formal" ~ "Publico_Formal",
    sector_lab == "Informal"       ~ "Informal",
    sector_lab == "Cuenta Propia"  ~ "Cuenta_Propia",
    TRUE ~ sector_lab
  ))

eph_corr_wide <- eph_corr_long %>%
  pivot_wider(
    id_cols = c(year, quarter),
    names_from = sector_lab,
    values_from = c(sal_hora_corr_pond, ing_mens_corr_pond)
  )

eph_total_corr <- eph %>%
  group_by(year, quarter) %>%
  summarise(
    sal_hora_corr_total = weighted.mean(salario_hora_corregido, w = PONDERA, na.rm = TRUE),
    ing_mens_corr_total = weighted.mean(ingreso_corregido, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  )

eph_idx <- eph_idx %>%
  left_join(eph_corr_wide, by = c("year", "quarter")) %>%
  left_join(eph_total_corr, by = c("year", "quarter"))

# ---- 10. Deflactar e indexar ----
cat("Deflactando e indexando ...\n")

# IPC del último trimestre disponible para constante
ultimo_ipc <- tail(ipc_q$ipc[!is.na(ipc_q$ipc)], 1)

eph_idx <- eph_idx %>%
  mutate(
    # Pesos constantes del último período disponible
    sal_hora_real_Privado_Formal   = sal_hora_pond_Privado_Formal   / ipc * ultimo_ipc,
    sal_hora_real_Publico_Formal   = sal_hora_pond_Publico_Formal   / ipc * ultimo_ipc,
    sal_hora_real_Informal         = sal_hora_pond_Informal         / ipc * ultimo_ipc,
    sal_hora_real_Cuenta_Propia    = sal_hora_pond_Cuenta_Propia    / ipc * ultimo_ipc,
    sal_hora_real_total            = sal_hora_total                 / ipc * ultimo_ipc,
    sal_hora_corr_real_Privado_Formal   = sal_hora_corr_pond_Privado_Formal  / ipc * ultimo_ipc,
    sal_hora_corr_real_Publico_Formal   = sal_hora_corr_pond_Publico_Formal  / ipc * ultimo_ipc,
    sal_hora_corr_real_Informal         = sal_hora_corr_pond_Informal        / ipc * ultimo_ipc,
    sal_hora_corr_real_Cuenta_Propia    = sal_hora_corr_pond_Cuenta_Propia   / ipc * ultimo_ipc,
    sal_hora_corr_real_total            = sal_hora_corr_total                 / ipc * ultimo_ipc
  )

base_year <- 2023
base_quarter <- 3

base_eph <- eph_idx %>% filter(year == base_year, quarter == base_quarter)

base_noreg <- indec_sal_q %>% filter(year == base_year, quarter == base_quarter)

eph_idx <- eph_idx %>%
  mutate(
    # Reales (deflactados IPC)
    idx_hora_Privado_Formal   = sal_hora_real_Privado_Formal   / base_eph$sal_hora_real_Privado_Formal   * 100,
    idx_hora_Publico_Formal   = sal_hora_real_Publico_Formal   / base_eph$sal_hora_real_Publico_Formal   * 100,
    idx_hora_Informal         = sal_hora_real_Informal         / base_eph$sal_hora_real_Informal         * 100,
    idx_hora_Cuenta_Propia    = sal_hora_real_Cuenta_Propia    / base_eph$sal_hora_real_Cuenta_Propia    * 100,
    idx_hora_total            = sal_hora_real_total            / base_eph$sal_hora_real_total            * 100,
    idx_hora_corr_Privado_Formal   = sal_hora_corr_real_Privado_Formal  / base_eph$sal_hora_corr_real_Privado_Formal   * 100,
    idx_hora_corr_Publico_Formal   = sal_hora_corr_real_Publico_Formal  / base_eph$sal_hora_corr_real_Publico_Formal   * 100,
    idx_hora_corr_Informal         = sal_hora_corr_real_Informal        / base_eph$sal_hora_corr_real_Informal         * 100,
    idx_hora_corr_Cuenta_Propia    = sal_hora_corr_real_Cuenta_Propia   / base_eph$sal_hora_corr_real_Cuenta_Propia    * 100,
    idx_hora_corr_total            = sal_hora_corr_real_total           / base_eph$sal_hora_corr_real_total            * 100,
    # Nominales (sin deflactar)
    idx_hora_nom_Privado_Formal   = sal_hora_pond_Privado_Formal  / base_eph$sal_hora_pond_Privado_Formal  * 100,
    idx_hora_nom_Publico_Formal   = sal_hora_pond_Publico_Formal  / base_eph$sal_hora_pond_Publico_Formal  * 100,
    idx_hora_nom_Informal         = sal_hora_pond_Informal        / base_eph$sal_hora_pond_Informal        * 100,
    idx_hora_nom_total            = sal_hora_total                / base_eph$sal_hora_total                * 100,
    # INDEC nominal indices → indexados a base común
    idx_is_reg_priv_nom = is_reg_priv / is_reg_priv[year == base_year & quarter == base_quarter] * 100,
    idx_is_reg_pub_nom  = is_reg_pub  / is_reg_pub[year == base_year & quarter == base_quarter] * 100,
    idx_is_noreg_priv_nom = is_noreg_priv / base_noreg$is_noreg_priv * 100
  )

# INDEC reales (deflactados por IPC)
# No Registrado: primero se desplaza el nominal (lead) para corregir el rezago
# de publicación, luego se deflacta con el IPC del período al que corresponde.
ultimo_ipc_base <- tail(eph_idx$ipc[!is.na(eph_idx$ipc)], 1)

temp_indec <- eph_idx %>%
  arrange(year, quarter) %>%
  mutate(
    is_noreg_priv_shifted = lead(is_noreg_priv, 2)
  )

eph_idx <- eph_idx %>%
  left_join(temp_indec %>% select(year, quarter, is_noreg_priv_shifted), by = c("year", "quarter")) %>%
  mutate(
    sal_indec_real_reg_priv      = is_reg_priv / ipc * ultimo_ipc_base,
    sal_indec_real_reg_pub       = is_reg_pub  / ipc * ultimo_ipc_base,
    sal_indec_real_noreg_priv    = is_noreg_priv_shifted / ipc * ultimo_ipc_base
  )

base_indec <- eph_idx %>%
  filter(year == base_year, quarter == base_quarter)

eph_idx <- eph_idx %>%
  mutate(
    idx_is_reg_priv = sal_indec_real_reg_priv / base_indec$sal_indec_real_reg_priv[1] * 100,
    idx_is_reg_pub  = sal_indec_real_reg_pub  / base_indec$sal_indec_real_reg_pub[1]   * 100,
    idx_is_noreg_priv = sal_indec_real_noreg_priv / base_indec$sal_indec_real_noreg_priv[1] * 100
  )

# ---- 11. Guardar ----
# Desplazar INDEC No Registrado nominal (ya se desplazó el real antes de deflactar)
eph_idx <- eph_idx %>%
  arrange(year, quarter) %>%
  mutate(
    idx_is_noreg_priv_nom = lead(idx_is_noreg_priv_nom, 2)
  )

# EPH No Registrado truncado hasta el último dato de INDEC No Registrado
ultimo_inde <- eph_idx %>%
  filter(!is.na(idx_is_noreg_priv)) %>%
  summarise(y = max(year), q = max(quarter[year == y])) %>%
  as.list()
eph_idx <- eph_idx %>%
  mutate(
    idx_hora_Informal_trunc = if_else(year > ultimo_inde$y | (year == ultimo_inde$y & quarter > ultimo_inde$q),
                                      NA_real_, idx_hora_Informal),
    idx_hora_nom_Informal_trunc = if_else(year > ultimo_inde$y | (year == ultimo_inde$y & quarter > ultimo_inde$q),
                                          NA_real_, idx_hora_nom_Informal)
  )

saveRDS(eph_idx, output_rds)
saveRDS(factores, "factores_subdeclaracion.rds")
cat(sprintf("Datos guardados en %s\n", output_rds))

# ---- 12. Gráficos ----
cat("Generando gráficos ...\n")

fecha_actualizacion <- Sys.Date()

nota_metodologica <- paste0(
  "EPH: sal/hora = ingreso total (P21+TOT_P12) / horas totales (PP3E_TOT+PP3F_TOT) × 4.33. ",
  "Winsorización sal/hora en p98 (se descarta 2% superior). ",
  "INDEC: Índice de Salarios (empleo principal registrado). ",
  "INDEC No Registrado desplazado 2 trim. por rezago. ",
  "Horas: media móvil 3 trim. (trailing). ",
  "Repositorio: github.com/rquiroga7/indice_salarial_hora") |>
  stringr::str_wrap(width = 120)

# Constantes para todos los gráficos base 100
base_breaks <- function(x) {
 下限 <- floor(min(x, na.rm = TRUE) / 10) * 10
 上限 <- ceiling(max(x, na.rm = TRUE) / 10) * 10
  seq(下限, 上限, by = 10)
}

last_val_text <- function(df, x_col, y_col, label_col, vjust = -0.5) {
  last <- df %>% filter(!is.na(.data[[y_col]])) %>% tail(1)
  if (nrow(last) == 0) return(geom_blank())
  x_pos <- last[[x_col]][1] + 0.15
  y_pos <- last[[y_col]][1]
  geom_text(aes(x = x_pos, y = y_pos, label = round(y_pos, 1)),
            color = label_col, size = 3, hjust = 0, vjust = vjust)
}

# Rectángulos por presidencia (colores partidarios)
gob_rects <- data.frame(
  start = c(2016, 2019.75, 2023.75),
  end   = c(2019.75, 2023.75, 2026),
  label = c("Macri", "Fernández", "Milei"),
  fill  = c("#FFD700", "#87CEEB", "#800080"),
  color = c("#B8860B", "#4682B4", "#800080")
)

make_gob <- function(y_top = 0) {
  list(
    annotate("rect",
      xmin = gob_rects$start, xmax = gob_rects$end,
      ymin = -Inf, ymax = y_top,
      fill = gob_rects$fill, alpha = 0.15),
    annotate("text",
      x = (gob_rects$start + gob_rects$end) / 2, y = y_top,
      label = gob_rects$label,
      color = gob_rects$color, size = 3.2, vjust = 1.5)
  )
}

gob_annotations <- make_gob()
y_expand_bottom <- 0.1

# 12a. Índice base 100 — EPH por sector (sin corregir)
pal_corr <- c("#1f77b4", "#2ca02c", "#ff7f0e", "#d62728", "#9467bd") |> setNames(
  c("Privado Formal (corr.)", "Público Formal (corr.)", "Informal (corr.)",
    "Cuenta Propia (corr.)", "Total (corr.)"))
pal3 <- c("Reg. Privado" = "#1f77b4", "Reg. Público" = "#2ca02c", "No Registrado" = "#ff7f0e")

pal_sectores5 <- c("Reg. Privado" = "#1f77b4", "Reg. Público" = "#2ca02c",
                    "No Registrado" = "#ff7f0e", "Cuenta Propia" = "#d62728",
                    "Total" = "#9467bd")

df_12a <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    `Reg. Privado`  = idx_hora_Privado_Formal,
    `Reg. Público`  = idx_hora_Publico_Formal,
    `No Registrado` = idx_hora_Informal,
    `Cuenta Propia` = idx_hora_Cuenta_Propia,
    `Total`         = idx_hora_total) %>%
  pivot_longer(-c(year, quarter), names_to = "sector", values_to = "indice") %>%
  mutate(x = year + (quarter - 1) / 4)

p1 <- df_12a %>%
  ggplot(aes(x = x, y = indice, color = sector)) +
  geom_line(linewidth = ifelse(df_12a$sector %in% c("Total", "Cuenta Propia"), 0.7, 1)) +
  scale_color_manual(values = pal_sectores5) +
  scale_y_continuous(breaks = base_breaks, labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Índice Salarial Real por Hora — EPH (Base 100 = 2023 Q3)",
       subtitle = "EPH sin corregir. Sectores: registrado privado, público, no registrado y cuenta propia",
       x = NULL, y = "Índice (base 100)", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")

for (s in names(pal_sectores5)) {
  d <- df_12a %>% filter(sector == s)
  l <- d %>% filter(!is.na(indice)) %>% tail(1)
  if (nrow(l) > 0) {
    p1 <- p1 + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), fill = "white", linewidth = 0.5, fontface = "bold",
                         color = pal_sectores5[s], size = 3, hjust = 0, vjust = -0.5)
  }
}
ggsave("resultados/indice_salarial_hora_sectores.png", p1, width = 10, height = 7.5, dpi = 150)

prepare_indec_data <- function(data, eph_prefix) {
  eph_priv <- paste0(eph_prefix, "Privado_Formal")
  eph_pub  <- paste0(eph_prefix, "Publico_Formal")
  eph_inf  <- paste0(eph_prefix, "Informal")
  data %>%
    filter(year <= last_year) %>%
    transmute(
      year, quarter,
      reg_priv_eph  = .data[[eph_priv]],
      reg_pub_eph   = .data[[eph_pub]],
      no_reg_eph    = .data[[eph_inf]],
      reg_priv_indec = idx_is_reg_priv,
      reg_pub_indec  = idx_is_reg_pub,
      no_reg_indec   = idx_is_noreg_priv
    )
}

format_indec_plot <- function(df, title, subtitle) {
  df %>%
    pivot_longer(-c(year, quarter), names_to = "serie", values_to = "indice") %>%
    mutate(
      categoria = case_when(
        grepl("reg_priv", serie) ~ "Reg. Privado",
        grepl("reg_pub", serie)  ~ "Reg. Público",
        grepl("no_reg", serie)   ~ "No Registrado"
      ),
      fuente = if_else(grepl("indec$", serie), "INDEC", "EPH")
    ) %>%
    ggplot(aes(x = year + (quarter - 1) / 4, y = indice,
               color = categoria, linetype = fuente, group = serie)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = c("Reg. Privado" = "#1f77b4",
                                   "Reg. Público" = "#2ca02c",
                                   "No Registrado" = "#ff7f0e")) +
    scale_linetype_manual(values = c("EPH" = "solid", "INDEC" = "22")) +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = "Índice (base 100)", color = NULL, linetype = "Fuente",
          caption = nota_metodologica) +
    guides(linetype = guide_legend(override.aes = list(linewidth = 1.2))) +
    theme_minimal() + theme(legend.position = "bottom")
}

# 12b. EPH solo (sin corregir) — tres sectores
df_eph <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    `Reg. Privado`  = idx_hora_Privado_Formal,
    `Reg. Público`  = idx_hora_Publico_Formal,
    `No Registrado` = idx_hora_Informal) %>%
  pivot_longer(-c(year, quarter), names_to = "sector", values_to = "indice") %>%
  mutate(x = year + (quarter - 1) / 4)

p_eph_only <- df_eph %>%
  ggplot(aes(x = x, y = indice, color = sector)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal3) +
  scale_y_continuous(breaks = base_breaks, labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Índice Salarial Real por Hora — EPH (sin corregir)",
       subtitle = "Base 100 = 2023 Q3",
       x = NULL, y = "Índice (base 100)", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")

for (s in names(pal3)) {
  l <- df_eph %>% filter(sector == s, !is.na(indice)) %>% tail(1)
  if (nrow(l) > 0) p_eph_only <- p_eph_only + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), color = pal3[s], fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5)
}
ggsave("resultados/indice_salarial_hora_eph.png", p_eph_only, width = 10, height = 7.5, dpi = 150)

# 12b-nom. EPH nominal (sin deflactar)
df_eph_nom <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    `Reg. Privado`  = idx_hora_nom_Privado_Formal,
    `Reg. Público`  = idx_hora_nom_Publico_Formal,
    `No Registrado` = idx_hora_nom_Informal) %>%
  pivot_longer(-c(year, quarter), names_to = "sector", values_to = "indice") %>%
  mutate(x = year + (quarter - 1) / 4)

p_eph_nom <- df_eph_nom %>%
  ggplot(aes(x = x, y = indice, color = sector)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal3) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Índice Salarial Nominal por Hora — EPH (sin corregir)",
       subtitle = "Sin deflactar. Base 100 = 2023 Q3",
       x = NULL, y = "Índice nominal (base 100)", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")

for (s in names(pal3)) {
  l <- df_eph_nom %>% filter(sector == s, !is.na(indice)) %>% tail(1)
  if (nrow(l) > 0) p_eph_nom <- p_eph_nom + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), color = pal3[s], fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5)
}
ggsave("resultados/indice_salarial_hora_eph_nominal.png", p_eph_nom, width = 10, height = 7.5, dpi = 150)

# 12c. INDEC solo — tres sectores (Índice de Salarios INDEC)
df_indec <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    `Reg. Privado`  = idx_is_reg_priv,
    `Reg. Público`  = idx_is_reg_pub,
    `No Registrado` = idx_is_noreg_priv) %>%
  pivot_longer(-c(year, quarter), names_to = "sector", values_to = "indice") %>%
  mutate(x = year + (quarter - 1) / 4)

p_indec_only <- df_indec %>%
  ggplot(aes(x = x, y = indice, color = sector)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal3) +
  scale_y_continuous(breaks = base_breaks, labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Índice Salarial Real (Mensual) — Índice de Salarios INDEC",
       subtitle = "Deflactado por IPC. Base 100 = 2023 Q3. Últimos dos trimestres censurados (rezago de publicación).",
       x = NULL, y = "Índice (base 100)", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")

for (s in names(pal3)) {
  l <- df_indec %>% filter(sector == s, !is.na(indice)) %>% tail(1)
  if (nrow(l) > 0) p_indec_only <- p_indec_only + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), color = pal3[s], fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5)
}
ggsave("resultados/indice_salarial_hora_indec.png", p_indec_only, width = 10, height = 7.5, dpi = 150)

# 12c-nom. INDEC nominal (sin deflactar)
df_indec_nom <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    `Reg. Privado`  = idx_is_reg_priv_nom,
    `Reg. Público`  = idx_is_reg_pub_nom,
    `No Registrado` = idx_is_noreg_priv_nom) %>%
  pivot_longer(-c(year, quarter), names_to = "sector", values_to = "indice") %>%
  mutate(x = year + (quarter - 1) / 4)

p_indec_nom <- df_indec_nom %>%
  ggplot(aes(x = x, y = indice, color = sector)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal3) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Índice Salarial Nominal (Mensual) — Índice de Salarios INDEC",
       subtitle = "Sin deflactar. Base 100 = 2023 Q3",
       x = NULL, y = "Índice nominal (base 100)", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")

for (s in names(pal3)) {
  l <- df_indec_nom %>% filter(sector == s, !is.na(indice)) %>% tail(1)
  if (nrow(l) > 0) p_indec_nom <- p_indec_nom + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), color = pal3[s], fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5)
}
ggsave("resultados/indice_salarial_hora_indec_nominal.png", p_indec_nom, width = 10, height = 7.5, dpi = 150)

# 12d. Comparación EPH vs INDEC por categoría — un plot por cada una
plot_cat_compare <- function(cat, eph_col, indec_col, col_eph = "#1f77b4", col_indec = "#ff7f0e") {
  df <- eph_idx %>%
    filter(year <= last_year) %>%
    select(year, quarter, eph = all_of(eph_col), indec = all_of(indec_col)) %>%
    pivot_longer(-c(year, quarter), names_to = "fuente", values_to = "indice") %>%
    mutate(fuente = if_else(fuente == "eph", "EPH (por hora)", "INDEC (mensual)"),
           x = year + (quarter - 1) / 4)

  p <- df %>%
    ggplot(aes(x = x, y = indice, color = fuente, linetype = fuente, group = fuente)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("EPH (por hora)" = col_eph, "INDEC (mensual)" = col_indec)) +
    scale_linetype_manual(values = c("EPH (por hora)" = "solid", "INDEC (mensual)" = "22")) +
    scale_y_continuous(breaks = base_breaks, labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
    gob_annotations +
    labs(title    = paste(cat, "— EPH (por hora) vs. Índice de Salarios INDEC (mensual)"),
         subtitle = "Ambos deflactados por IPC. Base 100 = 2023 Q3",
         x = NULL, y = "Índice (base 100)", color = NULL, linetype = NULL,
         caption = nota_metodologica) +
    guides(linetype = guide_legend(override.aes = list(linewidth = 1.2))) +
    theme_minimal() + theme(legend.position = "bottom")

  for (f in c("EPH (por hora)", "INDEC (mensual)")) {
    l <- df %>% filter(fuente == f, !is.na(indice)) %>% tail(1)
    c_use <- if (f == "EPH (por hora)") col_eph else col_indec
    if (nrow(l) > 0) p <- p + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), color = c_use, fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5, inherit.aes = FALSE)
  }
  p
}

p_reg_priv <- plot_cat_compare("Reg. Privado", "idx_hora_Privado_Formal", "idx_is_reg_priv", col_eph = "#1f77b4", col_indec = "#1f77b4")
ggsave("resultados/comparacion_reg_privado.png", p_reg_priv, width = 10, height = 7.5, dpi = 150)

p_reg_pub <- plot_cat_compare("Reg. Público", "idx_hora_Publico_Formal", "idx_is_reg_pub", col_eph = "#2ca02c", col_indec = "#2ca02c")
ggsave("resultados/comparacion_reg_publico.png", p_reg_pub, width = 10, height = 7.5, dpi = 150)

p_no_reg <- plot_cat_compare("No Registrado", "idx_hora_Informal_trunc", "idx_is_noreg_priv", col_eph = "#ff7f0e", col_indec = "#ff7f0e")
ggsave("resultados/comparacion_no_registrado.png", p_no_reg, width = 10, height = 7.5, dpi = 150)

# 12d-nom. Comparación nominal (sin deflactar)
plot_cat_compare_nom <- function(cat, eph_col, indec_col, col_eph = "#1f77b4", col_indec = "#ff7f0e") {
  df <- eph_idx %>%
    filter(year <= last_year) %>%
    select(year, quarter, eph = all_of(eph_col), indec = all_of(indec_col)) %>%
    pivot_longer(-c(year, quarter), names_to = "fuente", values_to = "indice") %>%
    mutate(fuente = if_else(fuente == "eph", "EPH (por hora, nominal)", "INDEC (mensual, nominal)"),
           x = year + (quarter - 1) / 4)

  p <- df %>%
    ggplot(aes(x = x, y = indice, color = fuente, linetype = fuente, group = fuente)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("EPH (por hora, nominal)" = col_eph, "INDEC (mensual, nominal)" = col_indec)) +
    scale_linetype_manual(values = c("EPH (por hora, nominal)" = "solid", "INDEC (mensual, nominal)" = "22")) +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
    gob_annotations +
    labs(title    = paste(cat, "— EPH vs. INDEC (nominal, sin deflactar)"),
         subtitle = "Base 100 = 2023 Q3",
         x = NULL, y = "Índice nominal (base 100)", color = NULL, linetype = NULL,
         caption = nota_metodologica) +
    guides(linetype = guide_legend(override.aes = list(linewidth = 1.2))) +
    theme_minimal() + theme(legend.position = "bottom")

  for (f in unique(df$fuente)) {
    l <- df %>% filter(fuente == f, !is.na(indice)) %>% tail(1)
    c_use <- if (grepl("EPH", f)) col_eph else col_indec
    if (nrow(l) > 0) p <- p + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), color = c_use, fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5, inherit.aes = FALSE)
  }
  p
}

p_reg_priv_nom <- plot_cat_compare_nom("Reg. Privado", "idx_hora_nom_Privado_Formal", "idx_is_reg_priv_nom", col_eph = "#1f77b4", col_indec = "#1f77b4")
ggsave("resultados/comparacion_reg_privado_nominal.png", p_reg_priv_nom, width = 10, height = 7.5, dpi = 150)

p_reg_pub_nom <- plot_cat_compare_nom("Reg. Público", "idx_hora_nom_Publico_Formal", "idx_is_reg_pub_nom", col_eph = "#2ca02c", col_indec = "#2ca02c")
ggsave("resultados/comparacion_reg_publico_nominal.png", p_reg_pub_nom, width = 10, height = 7.5, dpi = 150)

p_no_reg_nom <- plot_cat_compare_nom("No Registrado", "idx_hora_nom_Informal_trunc", "idx_is_noreg_priv_nom", col_eph = "#ff7f0e", col_indec = "#ff7f0e")
ggsave("resultados/comparacion_no_registrado_nominal.png", p_no_reg_nom, width = 10, height = 7.5, dpi = 150)

# 12d-int. Variación interanual — EPH e INDEC por sector
interanual <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    eph_priv  = idx_hora_Privado_Formal,
    eph_pub   = idx_hora_Publico_Formal,
    eph_noreg = idx_hora_Informal,
    indec_priv  = idx_is_reg_priv,
    indec_pub   = idx_is_reg_pub,
    indec_noreg = idx_is_noreg_priv) %>%
  pivot_longer(-c(year, quarter), names_to = "serie", values_to = "valor") %>%
  arrange(serie, year, quarter) %>%
  group_by(serie) %>%
  mutate(var_interanual = (valor / lag(valor, 4) - 1) * 100) %>%
  ungroup() %>%
  mutate(
    sector = case_when(
      grepl("priv", serie)  ~ "Reg. Privado",
      grepl("pub", serie)   ~ "Reg. Público",
      grepl("noreg", serie) ~ "No Registrado"
    ),
    fuente = if_else(grepl("^eph", serie), "EPH (por hora)", "INDEC (mensual)"),
    x = year + (quarter - 1) / 4
  ) %>%
  filter(!is.na(var_interanual))

make_interanual_plot <- function(cat, color_e = "#1f77b4", color_i = "#ff7f0e") {
  df <- interanual %>% filter(sector == cat)
  y_min <- min(df$var_interanual, na.rm = TRUE)
  y_top_gob <- if (y_min > 0) 0 else y_min - 5
  p <- df %>%
    ggplot(aes(x = x, y = var_interanual, color = fuente, linetype = fuente, group = serie)) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "gray50") +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = c("EPH (por hora)" = color_e, "INDEC (mensual)" = color_i)) +
    scale_linetype_manual(values = c("EPH (por hora)" = "solid", "INDEC (mensual)" = "22")) +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
    make_gob(y_top_gob) +
    labs(title    = paste("Variación Interanual —", cat),
         subtitle = "Mismo trimestre del año anterior. Var. % real (deflactado por IPC)",
         x = NULL, y = "Var. % interanual", color = NULL, linetype = NULL,
         caption = nota_metodologica) +
    guides(linetype = guide_legend(override.aes = list(linewidth = 1.2))) +
    theme_minimal() + theme(legend.position = "bottom")
  
  for (f in c("EPH (por hora)", "INDEC (mensual)")) {
    l <- df %>% filter(fuente == f, !is.na(var_interanual)) %>% tail(1)
    c_use <- if (f == "EPH (por hora)") color_e else color_i
    if (nrow(l) > 0) p <- p + geom_label(data = l, aes(x = x + 0.1, y = var_interanual, label = round(var_interanual, 1)), color = c_use, fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5, inherit.aes = FALSE)
  }
  p
}

p_int_priv <- make_interanual_plot("Reg. Privado", "#1f77b4", "#1f77b4")
ggsave("resultados/var_interanual_reg_privado.png", p_int_priv, width = 10, height = 7.5, dpi = 150)

p_int_pub <- make_interanual_plot("Reg. Público", "#2ca02c", "#2ca02c")
ggsave("resultados/var_interanual_reg_publico.png", p_int_pub, width = 10, height = 7.5, dpi = 150)

p_int_noreg <- make_interanual_plot("No Registrado", "#ff7f0e", "#ff7f0e")
ggsave("resultados/var_interanual_no_registrado.png", p_int_noreg, width = 10, height = 7.5, dpi = 150)

# 12d-int-nom. Variación interanual nominal (sin deflactar)
interanual_nom <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    eph_priv  = idx_hora_nom_Privado_Formal,
    eph_pub   = idx_hora_nom_Publico_Formal,
    eph_noreg = idx_hora_nom_Informal,
    indec_priv  = idx_is_reg_priv_nom,
    indec_pub   = idx_is_reg_pub_nom,
    indec_noreg = idx_is_noreg_priv_nom) %>%
  pivot_longer(-c(year, quarter), names_to = "serie", values_to = "valor") %>%
  arrange(serie, year, quarter) %>%
  group_by(serie) %>%
  mutate(var_interanual = (valor / lag(valor, 4) - 1) * 100) %>%
  ungroup() %>%
  mutate(
    sector = case_when(
      grepl("priv", serie)  ~ "Reg. Privado",
      grepl("pub", serie)   ~ "Reg. Público",
      grepl("noreg", serie) ~ "No Registrado"
    ),
    fuente = if_else(grepl("^eph", serie), "EPH (por hora)", "INDEC (mensual)"),
    x = year + (quarter - 1) / 4
  ) %>%
  filter(!is.na(var_interanual))

make_interanual_plot_nom <- function(cat, color_e = "#1f77b4", color_i = "#ff7f0e") {
  df <- interanual_nom %>% filter(sector == cat)
  y_min <- min(df$var_interanual, na.rm = TRUE)
  y_top_gob <- if (y_min > 0) 0 else y_min - 5
  p <- df %>%
    ggplot(aes(x = x, y = var_interanual, color = fuente, linetype = fuente, group = serie)) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "gray50") +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = c("EPH (por hora)" = color_e, "INDEC (mensual)" = color_i)) +
    scale_linetype_manual(values = c("EPH (por hora)" = "solid", "INDEC (mensual)" = "22")) +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
    make_gob(y_top_gob) +
    labs(title    = paste("Variación Interanual Nominal —", cat),
         subtitle = "Mismo trimestre del año anterior. Var. % nominal (sin deflactar)",
         x = NULL, y = "Var. % interanual nominal", color = NULL, linetype = NULL,
         caption = nota_metodologica) +
    guides(linetype = guide_legend(override.aes = list(linewidth = 1.2))) +
    theme_minimal() + theme(legend.position = "bottom")

  for (f in c("EPH (por hora)", "INDEC (mensual)")) {
    l <- df %>% filter(fuente == f, !is.na(var_interanual)) %>% tail(1)
    c_use <- if (f == "EPH (por hora)") color_e else color_i
    if (nrow(l) > 0) p <- p + geom_label(data = l, aes(x = x + 0.1, y = var_interanual, label = round(var_interanual, 1)), color = c_use, fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5, inherit.aes = FALSE)
  }
  p
}

p_int_nom_priv <- make_interanual_plot_nom("Reg. Privado", "#1f77b4", "#1f77b4")
ggsave("resultados/var_interanual_nominal_reg_privado.png", p_int_nom_priv, width = 10, height = 7.5, dpi = 150)

p_int_nom_pub <- make_interanual_plot_nom("Reg. Público", "#2ca02c", "#2ca02c")
ggsave("resultados/var_interanual_nominal_reg_publico.png", p_int_nom_pub, width = 10, height = 7.5, dpi = 150)

p_int_nom_noreg <- make_interanual_plot_nom("No Registrado", "#ff7f0e", "#ff7f0e")
ggsave("resultados/var_interanual_nominal_no_registrado.png", p_int_nom_noreg, width = 10, height = 7.5, dpi = 150)

# 12c. Corregido (SIPA/EPH móvil) — por sector
df_corr <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    `Reg. Privado`  = idx_hora_corr_Privado_Formal,
    `Reg. Público`  = idx_hora_corr_Publico_Formal,
    `No Registrado` = idx_hora_corr_Informal,
    `Cuenta Propia` = idx_hora_corr_Cuenta_Propia,
    `Total`         = idx_hora_corr_total) %>%
  pivot_longer(-c(year, quarter), names_to = "sector", values_to = "indice") %>%
  mutate(x = year + (quarter - 1) / 4)

p_corr <- df_corr %>%
  ggplot(aes(x = x, y = indice, color = sector)) +
  geom_line(linewidth = ifelse(df_corr$sector %in% c("Total", "Cuenta Propia"), 0.7, 1)) +
  scale_color_manual(values = pal_sectores5) +
  scale_y_continuous(breaks = base_breaks, labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Índice Salarial Real por Hora — Corregido (SIPA/EPH)",
       subtitle = "EPH corregido por subdeclaración (Albina et al.). Base 100 = 2023 Q3",
       x = NULL, y = "Índice (base 100)", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")

for (s in names(pal_sectores5)) {
  d <- df_corr %>% filter(sector == s)
  l <- d %>% filter(!is.na(indice)) %>% tail(1)
  if (nrow(l) > 0) p_corr <- p_corr + geom_label(data = l, aes(x = x + 0.1, y = indice, label = round(indice, 1)), color = pal_sectores5[s], fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5)
}
ggsave("resultados/indice_salarial_hora_corregido.png", p_corr, width = 10, height = 7.5, dpi = 150)

# 12d. Factores de corrección
p_fact <- factores %>%
  filter(year <= last_year) %>%
  pivot_longer(-c(year, quarter), names_to = "categoria", values_to = "factor") %>%
  mutate(categoria = recode(categoria,
    corr_formal = "Asal. Registrado", corr_informal = "Asal. No Registrado",
    corr_cp = "Cuenta Propia")) %>%
  ggplot(aes(x = year + (quarter - 1) / 4, y = factor, color = categoria)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  labs(title    = "Factores de Corrección por Subdeclaración (SIPA/EPH)",
       subtitle = "Evolución trimestral. Referencia: promedio 2022.",
       x = NULL, y = "Factor (ingreso real / declarado)", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("resultados/factores_subdeclaracion.png", p_fact, width = 10, height = 7.5, dpi = 150)

# 12e. Efecto corrección — comparación original vs corregido por sector
p_efecto <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter, matches("^idx_hora_|^idx_hora_corr_")) %>%
  select(-idx_hora_total, -idx_hora_corr_total) %>%
  pivot_longer(-c(year, quarter), names_to = "serie", values_to = "valor") %>%
  mutate(
    sector = case_when(
      grepl("Privado_Formal", serie) ~ "Privado Formal",
      grepl("Publico_Formal", serie) ~ "Público Formal",
      grepl("Informal", serie)        ~ "Informal",
      grepl("Cuenta_Propia", serie)   ~ "Cuenta Propia"
    ),
    tipo = if_else(grepl("corr", serie), "Corregido", "Original"),
    x = year + (quarter - 1) / 4
  ) %>%
  ggplot(aes(x = x, y = valor, color = sector, linetype = tipo)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(breaks = base_breaks, labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Efecto de la Corrección por Subdeclaración — EPH (por hora)",
       subtitle = "Original vs. corregido (SIPA/EPH móvil) por sector. Base 100 = 2023 Q3",
       x = NULL, y = "Índice (base 100)", color = NULL, linetype = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("resultados/efecto_correccion.png", p_efecto, width = 10, height = 7.5, dpi = 150)

# 12f. Pluriempleo por sector
p_pluri <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter, starts_with("tasa_pluriempleo")) %>%
  pivot_longer(-c(year, quarter), names_to = "grupo", values_to = "tasa") %>%
  mutate(grupo = recode(grupo,
    tasa_pluriempleo_Privado_Formal  = "Privado Formal",
    tasa_pluriempleo_Publico_Formal  = "Público Formal",
    tasa_pluriempleo_Informal        = "Informal",
    tasa_pluriempleo_Cuenta_Propia   = "Cuenta Propia",
    tasa_pluriempleo                 = "Total")) %>%
  mutate(x = year + (quarter - 1) / 4) %>%
  ggplot(aes(x = x, y = tasa, color = grupo)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Tasa de Pluriempleo por Sector",
       subtitle = "Ocupados con más de un trabajo",
       x = NULL, y = "%", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("resultados/pluriempleo.png", p_pluri, width = 10, height = 7.5, dpi = 150)

# 12g. Salario real por hora (nivel, $ constantes del último período)
pal_nivel <- c("Privado Formal" = "#1f77b4", "Público Formal" = "#2ca02c", "Informal" = "#ff7f0e")
df_nivel <- eph_idx %>%
  filter(year <= last_year) %>%
  select(year, quarter,
    `Privado Formal` = sal_hora_real_Privado_Formal,
    `Público Formal` = sal_hora_real_Publico_Formal,
    `Informal`       = sal_hora_real_Informal) %>%
  pivot_longer(-c(year, quarter), names_to = "sector", values_to = "salario") %>%
  mutate(x = year + (quarter - 1) / 4)

ultimo_mes <- format(as.Date(paste0(
  max(eph_idx$year[!is.na(eph_idx$ipc)]), "-",
  max(eph_idx$quarter[!is.na(eph_idx$ipc)], na.rm = TRUE) * 3 - 2, "-01")), "%Y-%m")

p_nivel <- df_nivel %>%
  ggplot(aes(x = x, y = salario, color = sector)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal_nivel) +
  scale_y_continuous(labels = dollar_format(suffix = "", prefix = "$"), expand = expansion(mult = c(y_expand_bottom, 0.05))) +
  gob_annotations +
  labs(title    = "Salario Real por Hora — EPH",
       subtitle = paste0("En pesos constantes de ", ultimo_mes),
       x = NULL, y = "$/hora", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")

for (s in names(pal_nivel)) {
  l <- df_nivel %>% filter(sector == s, !is.na(salario)) %>% tail(1)
  if (nrow(l) > 0) p_nivel <- p_nivel + geom_label(data = l, aes(x = x + 0.1, y = salario, label = round(salario)), color = pal_nivel[s], fill = "white", linewidth = 0.5, fontface = "bold", size = 3, hjust = 0, vjust = -0.5)
}
ggsave("resultados/salario_real_hora.png", p_nivel, width = 10, height = 7.5, dpi = 150)

# 12h. Horas semanales promedio por sector — ¿explica la brecha EPH vs INDEC?
horas_plot_data <- eph %>%
  filter(year <= last_year, !is.na(sector_lab)) %>%
  mutate(horas_principal = if_else(pluriempleo, PP3E_TOT, horas_sem)) %>%
  group_by(year, quarter, sector_lab) %>%
  summarise(
    horas_total   = weighted.mean(horas_sem, w = PONDERA, na.rm = TRUE),
    horas_ppal    = weighted.mean(horas_principal, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sector_lab = case_when(
    sector_lab == "Privado Formal" ~ "Reg. Privado",
    sector_lab == "Público Formal" ~ "Reg. Público",
    sector_lab == "Informal"       ~ "No Registrado",
    sector_lab == "Cuenta Propia"  ~ "Cuenta Propia"
  )) %>%
  filter(sector_lab != "Cuenta Propia") %>%
  group_by(sector_lab) %>%
  arrange(year, quarter) %>%
  mutate(
    horas_ppal_smooth   = (horas_ppal   + lag(horas_ppal, 1)   + lag(horas_ppal, 2))   / 3,
    horas_total_smooth  = (horas_total  + lag(horas_total, 1)  + lag(horas_total, 2))  / 3
  ) %>%
  ungroup()

pal_horas <- c("Reg. Privado" = "#1f77b4", "Reg. Público" = "#2ca02c",
               "No Registrado" = "#ff7f0e")

horas_plot_data <- horas_plot_data %>% mutate(x = year + (quarter - 1) / 4)

make_plot_horas <- function(y_col, smooth_col, title, subtitle, filename) {
  horas_plot_data %>%
    ggplot(aes(x = x, color = sector_lab)) +
    geom_line(aes(y = .data[[y_col]]), linewidth = 0.4, alpha = 0.3) +
    geom_line(aes(y = .data[[smooth_col]]), linewidth = 1) +
    scale_color_manual(values = pal_horas) +
    coord_cartesian(ylim = c(20, NA)) +
    scale_y_continuous(labels = comma_format(), expand = c(0, 0)) +
    make_gob(25) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = "Horas / semana", color = NULL,
         caption = nota_metodologica) +
    theme_minimal() + theme(legend.position = "bottom")
}

p_horas_ppal <- make_plot_horas("horas_ppal", "horas_ppal_smooth",
  "Horas Semanales — Solo Empleo Principal",
  "Línea sólida = media móvil 3 trim. Línea tenue = dato original. Excluye horas de pluriempleo.",
  "resultados/horas_semanales_ppal.png")
ggsave("resultados/horas_semanales_ppal.png", p_horas_ppal, width = 10, height = 7.5, dpi = 150)

p_horas_total <- make_plot_horas("horas_total", "horas_total_smooth",
  "Horas Semanales Trabajadas - Total (todas las ocupaciones)",
  "Línea sólida = media móvil 3 trim. Línea tenue = dato original. Incluye pluriempleo.",
  "resultados/horas_semanales_total.png")
ggsave("resultados/horas_semanales_total.png", p_horas_total, width = 10, height = 7.5, dpi = 150)

# 12i. Horas en empleo no principal (promedio entre todos los ocupados)
horas_sec <- eph %>%
  filter(year <= last_year, !is.na(sector_lab)) %>%
  mutate(horas_secundarias = coalesce(PP3F_TOT, 0)) %>%
  group_by(year, quarter, sector_lab) %>%
  summarise(
    horas_sec_prom    = weighted.mean(horas_secundarias, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sector_lab = case_when(
    sector_lab == "Privado Formal" ~ "Reg. Privado",
    sector_lab == "Público Formal" ~ "Reg. Público",
    sector_lab == "Informal"       ~ "No Registrado",
    sector_lab == "Cuenta Propia"  ~ "Cuenta Propia"
  )) %>%
  filter(sector_lab != "Cuenta Propia") %>%
  group_by(sector_lab) %>%
  arrange(year, quarter) %>%
  mutate(horas_sec_smooth = (horas_sec_prom + lag(horas_sec_prom, 1) + lag(horas_sec_prom, 2)) / 3) %>%
  ungroup() %>%
  mutate(x = year + (quarter - 1) / 4)

p_horas_sec <- horas_sec %>%
  ggplot(aes(x = x, color = sector_lab)) +
  geom_line(aes(y = horas_sec_prom), linewidth = 0.4, alpha = 0.3) +
  geom_line(aes(y = horas_sec_smooth), linewidth = 1) +
  scale_color_manual(values = pal_horas) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_y_continuous(labels = comma_format(), expand = c(0, 0)) +
  make_gob(-0.5) +
  labs(title    = "Horas en Empleo No Principal (promedio general)",
       subtitle = "Línea sólida = media móvil 3 trim. Línea tenue = dato original. Incluye ceros de quienes no tienen segundo empleo.",
       x = NULL, y = "Horas / semana", color = NULL,
       caption = nota_metodologica) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("resultados/horas_secundarias.png", p_horas_sec, width = 10, height = 7.5, dpi = 150)

# 12j. Horas totales agregadas (media ponderada + mediana)
horas_total_agg <- eph %>%
  filter(year <= last_year) %>%
  group_by(year, quarter) %>%
  summarise(
    horas_media = weighted.mean(horas_sem, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, quarter) %>%
  mutate(
    horas_smooth = (horas_media + lag(horas_media, 1) + lag(horas_media, 2)) / 3,
    x = year + (quarter - 1) / 4
  )

p_horas_total_agg <- horas_total_agg %>%
  ggplot(aes(x = x)) +
  geom_hline(yintercept = 40, linewidth = 0.5, linetype = "dotted", color = "gray50") +
  geom_line(aes(y = horas_media), linewidth = 0.4, alpha = 0.3, color = "#1f77b4") +
  geom_line(aes(y = horas_smooth), linewidth = 1, color = "#1f77b4") +
  coord_cartesian(ylim = c(30, 50)) +
  scale_y_continuous(breaks = seq(30, 50, by = 5), labels = comma_format(), expand = c(0, 0)) +
  make_gob(35) +
  labs(title    = "Horas Semanales Trabajadas - Total General",
       subtitle = "Media ponderada. Línea sólida = media móvil 3 trim. Línea punteada = mediana (=40).",
       x = NULL, y = "Horas / semana", color = NULL,
       caption = nota_metodologica) +
  theme_minimal()
ggsave("resultados/horas_semanales_total_agg.png", p_horas_total_agg, width = 10, height = 7.5, dpi = 150)

cat("\n--- Horas totales semanales (media ponderada) ---\n")
print(as.data.frame(horas_total_agg[, c("year", "quarter", "horas_media")]) |>
  transform(horas_media = round(horas_media, 1)), row.names = FALSE)

# 12k. Horas totales de pluriempleados
horas_pluri <- eph %>%
  filter(year <= last_year, pluriempleo) %>%
  group_by(year, quarter) %>%
  summarise(
    horas = weighted.mean(horas_sem, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(x = year + (quarter - 1) / 4) %>%
  arrange(year, quarter) %>%
  mutate(horas_smooth = (horas + lag(horas, 1) + lag(horas, 2)) / 3)

p_horas_pluri <- horas_pluri %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = horas), linewidth = 0.4, alpha = 0.3, color = "#d62728") +
  geom_line(aes(y = horas_smooth), linewidth = 1, color = "#d62728") +
  coord_cartesian(ylim = c(32.5, 65)) +
  scale_y_continuous(breaks = seq(32.5, 65, by = 5), labels = comma_format(), expand = c(0, 0)) +
  make_gob(35) +
  labs(title    = "Horas Semanales Trabajadas - Pluriempleados",
       subtitle = "Media ponderada. Línea sólida = media móvil 3 trim.",
       x = NULL, y = "Horas / semana",
       caption = nota_metodologica) +
  theme_minimal()
ggsave("resultados/horas_semanales_pluriempleo.png", p_horas_pluri, width = 10, height = 7.5, dpi = 150)

# 12l. Tasa de pluriempleo general (media móvil 3 trim)
tasa_pluri <- eph %>%
  filter(year <= last_year) %>%
  group_by(year, quarter) %>%
  summarise(
    tasa = weighted.mean(pluriempleo, w = PONDERA, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(x = year + (quarter - 1) / 4) %>%
  arrange(year, quarter) %>%
  mutate(tasa_smooth = (tasa + lag(tasa, 1) + lag(tasa, 2)) / 3)

p_tasa_pluri <- tasa_pluri %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = tasa), linewidth = 0.4, alpha = 0.3, color = "#1f77b4") +
  geom_line(aes(y = tasa_smooth), linewidth = 1, color = "#1f77b4") +
  coord_cartesian(ylim = c(0, 20)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5), labels = comma_format(), expand = c(0, 0)) +
  make_gob(1) +
  labs(title    = "Tasa de Pluriempleo General",
       subtitle = "% de ocupados con más de un trabajo. Media móvil 3 trim.",
       x = NULL, y = "%",
       caption = nota_metodologica) +
  theme_minimal()
ggsave("resultados/tasa_pluriempleo_general.png", p_tasa_pluri, width = 10, height = 7.5, dpi = 150)

# 12m. Tasa de pluriempleo solo registrados (asalariados formales)
tasa_pluri_reg <- eph %>%
  filter(year <= last_year, CAT_OCUP == 3, formal == TRUE) %>%
  group_by(year, quarter) %>%
  summarise(
    tasa = weighted.mean(pluriempleo, w = PONDERA, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(x = year + (quarter - 1) / 4) %>%
  arrange(year, quarter) %>%
  mutate(tasa_smooth = (tasa + lag(tasa, 1) + lag(tasa, 2)) / 3)

p_tasa_pluri_reg <- tasa_pluri_reg %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = tasa), linewidth = 0.4, alpha = 0.3, color = "#2ca02c") +
  geom_line(aes(y = tasa_smooth), linewidth = 1, color = "#2ca02c") +
  coord_cartesian(ylim = c(0, 20)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5), labels = comma_format(), expand = c(0, 0)) +
  make_gob(1) +
  labs(title    = "Tasa de Pluriempleo - Registrados",
       subtitle = "% de asalariados registrados con más de un trabajo. Media móvil 3 trim.",
       x = NULL, y = "%",
       caption = nota_metodologica) +
  theme_minimal()
ggsave("resultados/tasa_pluriempleo_registrados.png", p_tasa_pluri_reg, width = 10, height = 7.5, dpi = 150)

# ---- 13. Resumen ----
cat("\n=== RESUMEN ===\n")
cat(sprintf("Período: %d Q1 - %d Q%g\n",
    min(eph_idx$year), max(eph_idx$year), max(eph_idx$quarter)))
cat("  Var. real p/hora (sin corregir):\n")
cat(sprintf("    Privado Formal:  %.1f%%\n", last(eph_idx$idx_hora_Privado_Formal) - 100))
cat(sprintf("    Público Formal:  %.1f%%\n", last(eph_idx$idx_hora_Publico_Formal) - 100))
cat(sprintf("    Informal:        %.1f%%\n", last(eph_idx$idx_hora_Informal) - 100))
cat(sprintf("    Cuenta Propia:   %.1f%%\n", last(eph_idx$idx_hora_Cuenta_Propia) - 100))
cat(sprintf("    Total:           %.1f%%\n", last(eph_idx$idx_hora_total) - 100))
cat("  Var. real p/hora (corregido):\n")
cat(sprintf("    Privado Formal:  %.1f%%\n", last(eph_idx$idx_hora_corr_Privado_Formal) - 100))
cat(sprintf("    Público Formal:  %.1f%%\n", last(eph_idx$idx_hora_corr_Publico_Formal) - 100))
cat(sprintf("    Informal:        %.1f%%\n", last(eph_idx$idx_hora_corr_Informal) - 100))
cat(sprintf("    Cuenta Propia:   %.1f%%\n", last(eph_idx$idx_hora_corr_Cuenta_Propia) - 100))
cat(sprintf("    Total:           %.1f%%\n", last(eph_idx$idx_hora_corr_total) - 100))
cat(sprintf("Pluriempleo total (último):         %.1f%%\n",
    last(eph_idx$tasa_pluriempleo)))
cat(sprintf("Factor corrección formal (último):  %.3f\n",
    last(factores$corr_formal)))
cat(sprintf("Ratio SIPA/EPH formal-priv (%d):    %.3f\n",
    ref_year, ratio_ref))

cat("\nGráficos en 'resultados/'.\n")
cat("Datos en 'datos_procesados.rds'.\n")
cat("Factores de corrección en 'factores_subdeclaracion.rds'.\n")
