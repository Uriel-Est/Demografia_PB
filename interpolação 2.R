# ───── 1. Pacotes ─────
library(readxl)
library(dplyr)
library(sf)
library(geobr)
library(stringr)
library(spatstat.geom)
library(spatstat.explore)
library(raster)
library(viridisLite)  # para usar inferno()

# ───── 2. Leitura e limpeza do SIDRA ─────
sexo_raw <- read_excel("sexo_22_municipios_2022.xlsx", skip = 5, col_names = FALSE)

sexo <- sexo_raw %>%
  rename(
    cod_full = ...1,
    idade    = ...2,
    homens   = ...4,
    mulheres = ...5
  ) %>%
  filter(!is.na(cod_full), cod_full != "Total") %>%
  mutate(
    cod6       = as.integer(substr(cod_full, 1, 6)),
    homens     = as.numeric(homens),
    mulheres   = as.numeric(mulheres),
    razao_sexo = 100 * homens / mulheres
  ) %>%
  filter(mulheres > 0, is.finite(razao_sexo))

# ───── 3. Geometria da Paraíba ─────
pb_sf <- read_municipality("PB", 2022, cache = FALSE) %>%
  st_transform(31983) %>%
  mutate(cod6 = as.integer(substr(code_muni, 1, 6)))

# ───── 4. Join com razão de sexo ─────
mapa <- pb_sf %>%
  left_join(sexo, by = "cod6") %>%
  filter(!is.na(razao_sexo))

# ───── 5. Centróides e objeto ppp ─────
pts <- st_centroid(mapa)
xy  <- st_coordinates(pts)

# Janela como bounding box retangular (não recortada ao estado)
bbox <- st_bbox(pb_sf)
win <- owin(
  xrange = c(bbox["xmin"], bbox["xmax"]),
  yrange = c(bbox["ymin"], bbox["ymax"])
)

pp <- ppp(xy[, 1], xy[, 2],
          marks = pts$razao_sexo,
          window = win)

# ───── 6. Interpolação IDW (sem máscara) ─────
idw_img <- idw(pp, power = 2, at = "pixels", eps = 4000)

# ───── 7. Visualização com escala tipo “inferno” ─────
plot(idw_img,
     col = inferno(100),
     main = "Interpolação IDW — Razão de sexo (PB, 2022)",
     ribbon = TRUE,
     ribargs = list(
       lab = "Homens p/100 Mulheres",
       line = 2,
       cex.axis = 0.8
     ))

# Adiciona contorno do estado
plot(st_geometry(pb_sf), add = TRUE, border = "black", lwd = 0.5)
