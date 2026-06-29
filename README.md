# Índice Salarial Real por Hora Trabajada — Argentina 2016–2026

Construcción de un índice salarial real por hora a partir de la **EPH** (Encuesta Permanente de Hogares, INDEC) para contrastar con los índices oficiales del INDEC.

## Hipótesis

El empleo **no registrado** (informal) tradicional muestra salarios por hora estancados o en baja real. Sin embargo, desde ~2018 el arribo de plataformas digitales (Uber, Rappi, PedidosYa, etc.) incorpora al "empleo informal" a personas que antes no estaban ocupadas en ese segmento (o tenían otra ocupación principal). Como los ingresos de estas plataformas son, en promedio, más altos que los del cuentapropismo tradicional de baja calificación, el **promedio del salario informal se infla estadísticamente** — no por una mejora genuina, sino por un **efecto composición** (cambio en la mezcla de quiénes integran el sector informal).

Además, muchos trabajadores de plataformas son **pluriempleados** (tienen un trabajo formal + changas en apps), lo que también distorsiona el indicador al sumar ingresos de fuentes muy distintas.

## Metodología

1. **Descarga de EPH** vía el paquete `{eph}` (2016–2026, trimestral, base individual).
2. **Clasificación formal/informal**:
   - Asalariados (`CAT_OCUP == 3`): formales si tienen descuento jubilatorio (`PP07H == 1`).
   - Cuentapropistas (`CAT_OCUP == 2`): formales si aportan a sistema jubilatorio (`PP07I == 1`).
3. **Salario por hora**: ingreso mensual total (ocupación principal + otras) / (horas semanales × 4.33).
4. **Ponderación**: promedios ponderados por el factor de expansión `PONDERA`.
5. **Deflactación**: IPC Nacional Nivel General (base dic-2016=100, fuente: API de Series de Tiempo del INDEC).
6. **Índice**: base 2016 Q1 = 100.
7. **Comparación**: con el Índice de Salarios oficial (INDEC), subíndices registrado y no registrado.

## Fuentes de datos

| Fuente | Descripción | Obtención |
|--------|-------------|-----------|
| EPH | Microdatos individuales | `eph::get_microdata()` |
| IPC Nacional | Nivel General, base dic-2016 | `apis.datos.gob.ar` (serie `148.3_INIVELNAL_DICI_M_26`) |
| IPC-GBA | Proxy para 2016 pre-dic | `apis.datos.gob.ar` (serie `103.1_I2N_2016_M_19`) |
| Índice de Salarios | Total, Registrado, No Registrado | `apis.datos.gob.ar` (series `149.1_*`) |

## Uso

```r
# Una sola vez: instalar dependencias
install.packages(c("eph", "tidyverse", "lubridate", "scales"))

# Ejecutar
Rscript index_salarial_hora.R
```

La primera ejecución descarga ~44 trimestres de EPH (~10–15 min). Las ejecuciones posteriores usan caché local (`eph_cache_full.rds`).

## Outputs

| Archivo | Descripción |
|---------|-------------|
| `resultados/indice_salarial_hora.png` | Índice real por hora: formal vs informal (EPH) vs oficial |
| `resultados/brecha_informal.png` | Diferencia entre índice EPH informal y oficial no registrado |
| `resultados/pluriempleo.png` | Evolución de la tasa de pluriempleo por formalidad |
| `resultados/salario_real_hora.png` | Nivel de salario real por hora ($ constantes 2016) |
| `datos_procesados.rds` | Data frame completo con todos los índices |

## Interpretación

Si la hipótesis es correcta, esperamos ver:

- El **índice EPH informal por hora** creciendo **por encima** del **índice oficial no registrado** — la brecha positiva refleja el efecto composición.
- La **tasa de pluriempleo** aumentando, sobre todo entre trabajadores formales que toman changas informales.
- El **salario real por hora del sector formal** manteniéndose relativamente estable o con leve tendencia positiva, mientras el **informal tradicional** (aproximado por el índice oficial no registrado) se estanca o cae.

## Limitaciones

- La EPH no captura perfectamente el ingreso de trabajadores de plataformas (pregunta P21, que puede subdeclararse).
- El IPC para ene-mar 2016 es estimado (extrapolación desde abr-2016 con tasa mensual fija).
- El índice oficial "no registrado" solo está disponible desde oct-2016.
- La EPH tiene rezago de publicación (~3 meses).

## Licencia

MIT
