# Pacotes necessários para o script
library(sidrar)
library(PNADcIBGE)
library(survey)
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(gstat)
library(spatstat.geom)
library(sp)
library(stringi)
library(readxl)
library(viridis)
library(terra)

# Carregamento de dados iniciais ------------------------------------------

# Carregando mapa 
pb_map_10 <- read_municipality(
  code_muni = "PB",
  year = 2010,
  showProgress = F
)

pb_map <- read_municipality(
  code_muni = "PB",
  year = 2019,
  showProgress = F
)

  ggplot() +
    geom_sf(data = pb_map, fill="#2D3E50", color = "#FEBF57", size = .15, show.legend = F) +
    labs(subtitle = "Municípios Paraibanos, 2010", size = 8) +
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())


  

# Buscar tabelas~
info_sidra(200)
info_sidra(1209)
info_sidra(9514)
info_sidra(1378)
info_sidra(6579)
info_sidra(1419)

# Função auxiliar que carrega se existir ou baixa e salva
carregar_ou_baixar_sidra <- function(arquivo, ...) {
  if (file.exists(arquivo)) {
    readRDS(arquivo)
  } else {
    dados <- get_sidra(...)
    saveRDS(dados, arquivo)
    dados
  }
}

# 📦 Censo 2022 UF-25
sexo_22 <- carregar_ou_baixar_sidra(
  arquivo = "./sexo_22_2022.rds",
  x = 9514,
  period = "2022",
  geo = "State",
  geo.filter = list("State" = 25),
  classific = "all",
  category = "all"
)

# 🧼 Padroniza 2022
sexo_22 <- sexo_22 %>%
  select(sexo = `Sexo`, idade = `Idade`, valor = Valor)

# 📦 Censo 2010 UF-25
sexo_10 <- carregar_ou_baixar_sidra(
  arquivo = "./sexo_10_2010.rds",
  x = 200,
  period = "2010",
  geo = "State",
  geo.filter = list("State" = 25),
  classific = "all",
  category = "all"
)

# 🧼 Padroniza 2010
sexo_10 <- sexo_10 %>%
  select(sexo = `Sexo`, idade = `Grupo de idade`, valor = Valor)

# Razão de sexo -------------------------------------------------------

# Garante que 'valor' é numérico (só por segurança)
sexo_22 <- sexo_22 %>%
  mutate(valor = as.numeric(valor))

pop_razao_2022 <- sexo_22 %>%
  filter(sexo %in% c("Homens", "Mulheres"), idade != "Total") %>%
  group_by(sexo) %>%
  summarise(populacao = sum(valor), .groups = "drop") %>%
  pivot_wider(names_from = sexo, values_from = populacao) %>%
  mutate(razao_sexo = (Homens / Mulheres) * 100)

print(pop_razao_2022)

# Garante que 'valor' é numérico
sexo_10 <- sexo_10 %>%
  mutate(valor = as.numeric(valor))

# Calcula razão de sexo
pop_razao_2010 <- sexo_10 %>%
  filter(sexo %in% c("Homens", "Mulheres"), idade != "Total") %>%
  group_by(sexo) %>%
  summarise(populacao = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sexo, values_from = populacao) %>%
  mutate(razao_sexo = (Homens / Mulheres) * 100)

print(pop_razao_2010)

razoes_ano <- bind_rows(
  pop_razao_2010 %>% mutate(ano = 2010),
  pop_razao_2022 %>% mutate(ano = 2022)
) %>%
  select(ano, Homens, Mulheres, razao_sexo)

print(razoes_ano)

# Gráfico de Calor 🔥🗺️
# Os dados do SIDRAR para a tabela 1378 não estão funcionando
# Assim, eu prefiro
# Baixa dados por município da Paraíba (UF 25)
# Para a tabela 1378 :
# --- 2010 ---------------------------------------------------
sexo_mun_2010 <- read_excel("./sexo_mun_2010.xlsx",
                            skip      = 5,
                            col_names = FALSE) %>%
  select(
    code_muni = 1,
    municipio = 2,
    homens    = 5,
    mulheres  = 6
  ) %>%
  filter(!is.na(code_muni)) %>%
  mutate(
    code_muni  = as.integer(code_muni),
    municipio  = str_trim(municipio),
    homens     = as.numeric(homens),
    mulheres   = as.numeric(mulheres),
    razao_sexo = (homens / mulheres) * 100
  )

#Junta os dados da tabela aos dados espaciais usando o code_muni
pb_map_10 <- pb_map_10 %>%
  mutate(code_muni = as.integer(code_muni))
mapa_razao_2010 <- pb_map_10 %>%
  left_join(sexo_mun_2010, by = "code_muni")

# checar se agora há valores na razão:
summary(mapa_razao_2010$razao_sexo)

# Cartograma:
ggplot(data = mapa_razao_2010) +
  geom_sf(aes(fill = razao_sexo), color = NA) +
  scale_fill_gradientn(
    colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
    name   = "Razão de Sexo"
  ) +
  labs(
    title    = "Cartograma Lavado — Razão de Sexo por Município (PB, 2010)",
    subtitle = "Preenchimento térmico proporcional à razão entre homens e mulheres",
    caption  = "Fonte: Censo 2010 — IBGE / Tabela 1378"
  ) +
  theme_minimal() +
  theme(
    axis.title   = element_blank(),
    axis.text    = element_blank(),
    axis.ticks   = element_blank(),
    plot.title   = element_text(face = "bold", size = 14),
    plot.subtitle= element_text(size = 10),
    plot.caption = element_text(size = 8)
  )

# Cartograma de Interpolação IDW
# ───── 1. Estado da Paraíba em UTM-23S ─────
uf <- geobr::read_state()
uf_pb <- subset(uf, code_state == 25) |>
  st_transform(31983)

# ───── 2. Centróides dos municípios com razão de sexo ─────
mapa_points <- mapa_razao_2010 |>
  filter(!is.na(razao_sexo)) |>
  st_centroid() |>
  st_transform(31983)

# ───── 3. Bounding box como janela ─────
bbox <- st_bbox(uf_pb)
obs_window <- owin(
  xrange = c(bbox["xmin"], bbox["xmax"]),
  yrange = c(bbox["ymin"], bbox["ymax"])
)

# ───── 4. Coordenadas e objeto ppp ─────
coords <- st_coordinates(mapa_points)
ppp_malaria <- ppp(
  x = coords[, 1],
  y = coords[, 2],
  marks = mapa_points$razao_sexo,
  window = obs_window
)

# ───── 5. Interpolação IDW (potência suave) ─────
idw_malaria <- idw(ppp_malaria, power = 0.05, at = "pixels")

# ───── 6. Plot com heat.colors clássica ─────
plot(idw_malaria,
     main = "Interpolação IDW — Razão de sexo (PB, 2010)",
     col = rev(heat.colors(100)),
     ribbon = TRUE,
     ribargs = list(
       lab = "Homens p/100 Mulheres",
       line = 2,
       cex.axis = 0.8
     ))

# ───── 7. Contorno do estado ─────
muni_pb <- read_municipality(code_muni = "PB", year = 2010) |> 
  st_transform(31983)

plot(st_geometry(muni_pb), add = TRUE, border = "black", lwd = 0.5)


# Fazer novamente para o ano de 2022 agora
# ─── 1. Lê SIDRA 2022  ──────────────────────────────────────
  sexo_raw <- read_excel("sexo_22_municipios_2022.xlsx", skip = 5)

sexo <- sexo_raw %>% 
  rename(
    cod_full = 1,           # 7 dígitos, ex.: 2500106
    idade    = 2,
    homens   = 3,
    mulheres = 4
  ) %>% 
  mutate(
    cod_full   = as.integer(cod_full),
    cod6       = cod_full %/% 10,             # corta último dígito
    homens     = as.numeric(homens),
    mulheres   = as.numeric(mulheres),
    razao_sexo = 100 * homens / mulheres
  ) %>% 
  filter(mulheres > 0, is.finite(razao_sexo))

# ─── 2. Geometria PB 2022  ──────────────────────────────────
pb_sf <- read_municipality("PB", 2022, cache = FALSE) %>% 
  st_transform(31983) %>% 
  mutate(cod6 = as.integer(code_muni) %/% 10)          # mesmo corte

# ─── 3. Join por cod6  ──────────────────────────────────────
mapa <- pb_sf %>% 
  left_join(sexo, by = "cod6") %>% 
  filter(!is.na(razao_sexo))

cat("Linhas após join:", nrow(mapa), "\n")    # deve ser 223

# ─── 4. Centróides e ppp  ──────────────────────────────────
pts   <- st_centroid(mapa)
win   <- as.owin(st_union(pb_sf))
xy    <- st_coordinates(pts)

pp <- ppp(xy[,1], xy[,2], marks = pts$razao_sexo, window = win)
cat("Pontos no ppp:", pp$n, "\n")             # deverá ser 223

# ─── 5. IDW e plot  ─────────────────────────────────────────
idw_img <- idw(pp, power = 2, at = "pixels", eps = 4000)

plot(idw_img,
     main   = "Razão de Sexo — IDW (Paraíba, 2022)",
     col    = viridis(100),
     ribbon = TRUE)
plot(as.owin(pb_sf), add = TRUE, border = "black", lwd = .4)


# MYERS -------------------------------------------------------------------

# Para 2022
myers_base <- sexo_22 %>%
  filter(sexo == "Total") %>%
  mutate(
    idade_num = as.numeric(gsub("\\D", "", idade))  # extrai número ignorando texto
  ) %>%
  filter(idade_num >= 10, idade_num <= 99) %>%
  mutate(
    digito_final = idade_num %% 10,
    valor = as.numeric(valor)
  ) %>%
  group_by(digito_final) %>%
  summarise(total = sum(valor), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100,
         desvio = abs(percentual - 10))

indice_myers <- sum(myers_base$desvio) / 2

print(indice_myers)

# Para 2010


indice_myers_2010 <- sexo_10 %>%
  filter(sexo == "Total") %>%
  mutate(
    idade_num = as.numeric(gsub("\\D", "", idade)),  # Extrai número
    valor = as.numeric(valor)
  ) %>%
  filter(
    idade_num >= 10, idade_num <= 99,
    grepl("^\\d{2} anos$", idade)  # Garante que é do tipo "47 anos", "83 anos"
  ) %>%
  mutate(
    digito_final = idade_num %% 10
  ) %>%
  group_by(digito_final) %>%
  summarise(total = sum(valor), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    desvio = abs(percentual - 10)
  ) %>%
  summarise(indice_myers = sum(desvio) / 2) %>%
  pull(indice_myers)

print(indice_myers_2010)