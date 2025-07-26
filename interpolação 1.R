# ───── 1. Pacotes ─────
library(geobr)
library(sf)
library(spatstat.geom)
library(spatstat.explore)
library(spatstat.model)
library(dplyr)

# ───── 2. Estado da Paraíba em UTM-23S ─────
uf <- geobr::read_state()
uf_pb <- subset(uf, code_state == 25) |>
  st_transform(31983)

# ───── 3. Centróides dos municípios com razão de sexo ─────
mapa_points <- mapa_razao_2010 |>
  filter(!is.na(razao_sexo)) |>
  st_centroid() |>
  st_transform(31983)

# ───── 4. Bounding box como janela ─────
bbox <- st_bbox(uf_pb)
obs_window <- owin(
  xrange = c(bbox["xmin"], bbox["xmax"]),
  yrange = c(bbox["ymin"], bbox["ymax"])
)

# ───── 5. Coordenadas e objeto ppp ─────
coords <- st_coordinates(mapa_points)
ppp_malaria <- ppp(
  x = coords[, 1],
  y = coords[, 2],
  marks = mapa_points$razao_sexo,
  window = obs_window
)

# ───── 6. Interpolação IDW (potência suave) ─────
idw_malaria <- idw(ppp_malaria, power = 0.05, at = "pixels")

# ───── 7. Plot com heat.colors clássica ─────
plot(idw_malaria,
     main = "Interpolação IDW — Razão de sexo (PB, 2010)",
     col = rev(heat.colors(100)),
     ribbon = TRUE,
     ribargs = list(
       lab = "Homens p/100 Mulheres",
       line = 2,
       cex.axis = 0.8
     ))

# ───── 8. Contorno do estado ─────
muni_pb <- read_municipality(code_muni = "PB", year = 2010) |> 
  st_transform(31983)

plot(st_geometry(muni_pb), add = TRUE, border = "black", lwd = 0.5)

