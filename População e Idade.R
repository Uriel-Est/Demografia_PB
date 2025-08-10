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
library(grid)
library(gridExtra)
library(raster)
library(spData)
library(cowplot)
library(ggspatial)
library(spdep)
library(ggrepel)

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
info_sidra(1419) #não

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
    colors = c("#4575B4", "#91BFDB", "#FFFFBF", "#FC8D59", "#D73027"),
    name   = "Razão de Sexo"
  ) +
  labs(
    title    = "Choropeth — Razão de Sexo por Município (PB, 2010)",
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
# ─── 1. Lê tabela para 2022  ──────────────────────────────────────
  sexo_raw <- read_excel("sexo_22_municipios_2022.xlsx")

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


## Cartograma de Calor normal
# Cartograma Lavado — Censo 2022
ggplot(data = mapa) +
  geom_sf(aes(fill = razao_sexo), color = NA) +
  scale_fill_gradientn(
    colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
    name   = "Razão de Sexo"
  ) +
  labs(
    title    = "Cartograma Lavado — Razão de Sexo por Município (PB, 2022)",
    subtitle = "Preenchimento térmico proporcional à razão entre homens e mulheres",
    caption  = "Fonte: Censo 2022 — Instituto Brasileiro de Geografia e Estatística (IBGE). Cálculo próprio."
  ) +
  theme_minimal() +
  theme(
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank(),
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 8)
  )


## Global Moran's I para checar a correlação espacial xD
# ——————————————————————————————————————————————————————————————
# 1. Global Moran’s I para 2010
# ——————————————————————————————————————————————————————————————
# Preparação inicial~
pb_sf_2010 <- read_municipality(code_muni = "PB", year = 2010) %>%
  st_transform(31983)

mapa_razao_2010 <- pb_sf_2010 %>%
  left_join(sexo_mun_2010, by = "code_muni")

map_sp_2010 <- as(mapa_razao_2010, "Spatial")

# 1.1 Definir vizinhança por contiguidade (queen) 
nb_10 <- poly2nb(mapa_razao_2010, queen = TRUE)

# 1.2 Criar lista de pesos row-standardizada
lw_10 <- nb2listw(nb_10, style = "W", zero.policy = TRUE)

# 1.3 Calcular Moran’s I via Monte Carlo (999 permutações)
set.seed(123)  # para reprodutibilidade
moran_10 <- moran.mc(mapa_razao_2010$razao_sexo, listw = lw_10,
                     nsim = 999, zero.policy = TRUE)

# 1.4 Exibir resultados
print(moran_10)
#   → I observado, valor-p (simulado), e distribuição de referência

# 1.5 LISA - 2010
lisa_10 <- localmoran(mapa_razao_2010$razao_sexo, lw_10)

# 1.6 Adicionar Resultados ao mapa
mapa_razao_2010$lisa_I <- lisa_10[,1]         # valor do I local
mapa_razao_2010$p_value <- lisa_10[,5]        # p-valor
mapa_razao_2010$significativo <- mapa_razao_2010$p_value < 0.05

# 1.7 Classificar os clusters
media <- mean(mapa_razao_2010$razao_sexo)
mapa_razao_2010$tipo_cluster <- ifelse(
  mapa_razao_2010$significativo & 
    mapa_razao_2010$razao_sexo >= media & 
    lag.listw(lw_10, mapa_razao_2010$razao_sexo) >= media, "High-High",
  ifelse(
    mapa_razao_2010$significativo & 
      mapa_razao_2010$razao_sexo < media & 
      lag.listw(lw_10, mapa_razao_2010$razao_sexo) < media, "Low-Low", 
    "Não-Significativo"
  )
)

# 1.8 Representação visual dos resultados
ggplot(mapa_razao_2010) +
  geom_sf(aes(fill = tipo_cluster), color = NA) +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",      # vermelho intenso
      "Low-Low" = "#377EB8",        # azul marcante
      "Não-Significativo" = "#CCCCCC"  # cinza clarinho
    ),
    name = "Tipo de Cluster"
  ) +
  labs(
    title = "Clusters Espaciais - Moran Local (LISA)",
    subtitle = "Distribuição da razão de sexo por município — 2010",
    caption = "Fonte: IBGE. Elaboração própria com Moran Local."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    panel.grid = element_blank()
  )

subset(mapa_razao_2010, p_value < 0.05)

# ——————————————————————————————————————————————————————————————
# 2. Global Moran’s I para 2022
# ——————————————————————————————————————————————————————————————
# Passo 1: Pular linhas iniciais problemáticas
sexo_clean <- read_excel(
  "sexo_22_municipios_2022.xlsx",
  skip = 5,  # Ignora as 5 primeiras linhas
  col_names = c("codigo", "municipio", "declaracao_idade", "homens", "mulheres")
) 

# Passo 2: Filtrar apenas linhas com dados municipais
sexo_clean <- sexo_clean %>% 
  filter(
    !is.na(codigo),
    declaracao_idade == "Total"  # Mantém apenas totais, não desagregações
  ) %>% 
  mutate(
    municipio = str_remove(municipio, "\\s*\\(PB\\)"),  # Remove "(PB)"
    across(c(homens, mulheres), as.numeric)  # Converte para numérico
  )

# Passo 3: Criar variável de razão de sexo
sexo_clean <- sexo_clean %>% 
  mutate(
    razao_sexo = (homens / mulheres) * 100  # Razão padrão: homens por 100 mulheres
  )

# Resultado:
head(sexo_clean)

pb_sf_2022 <- read_municipality("PB", year = 2022) %>%
  st_transform(31983) %>%
  mutate(
    code_muni = str_pad(str_remove_all(as.character(code_muni), "\\D"),
                        width = 7, side = "left", pad = "0")
  )

# 2.2 Seu sexo_clean já preparado
#    — colunas: codigo (7 dígitos), municipio, declaracao_idade, homens, mulheres, razao_sexo
head(sexo_clean)  # já confere codigo e razao_sexo


# 2.3 Unir shapefile + sexo_clean
mapa_razao_sexo <- pb_sf_2022 %>%
  left_join(sexo_clean, by = c("code_muni" = "codigo")) %>%
  filter(!is.na(razao_sexo))

cat("Municípios com razão de sexo:", nrow(mapa_razao_sexo), "\n")  # deve dar 223

# 2.4 Converter para Spatial e gerar vizinhança Queen
map_sp_2022 <- as(mapa_razao_sexo, "Spatial")
nb_22 <- poly2nb(map_sp_2022, queen = TRUE, row.names = row.names(map_sp_2022))
lw_22 <- nb2listw(nb_22, style = "W", zero.policy = TRUE)

# 2.5 Moran I (Monte Carlo)
set.seed(123)
moran_22 <- moran.mc(map_sp_2022$razao_sexo, listw = lw_22,
                     nsim = 999, zero.policy = TRUE)


## Scatterplot Moran's I
# 1. padroniza a variável
z <- scale(map_sp_2022$razao_sexo)

# 2. calcula o spatial lag dos z-scores
lag_z <- spdep::lag.listw(lw_22, z)
lw <- lw_22

# recalcula vizinhança e pesos sob o nome lw
nb  <- poly2nb(map_sp_2022, queen = TRUE)
lw  <- nb2listw(nb, style = "W", zero.policy = TRUE)

# scatterplot
df <- data.frame(z = as.numeric(z), lag_z = as.numeric(lag_z))

ggplot(df, aes(x = z, y = lag_z)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Razão de Sexo (z-score)",
       y = "Spatial Lag (z-score)",
       title = "Moran Scatterplot",
       caption = "Fonte: IBGE Censo 2022 | Elaboração Própria")

# 2.6 LISA (Local Moran’s I) e anexar resultados
lisa_22 <- localmoran(map_sp_2022$razao_sexo, lw_22, zero.policy = TRUE)
map_sp_2022$lisa_I        <- lisa_22[,1]
map_sp_2022$p_value       <- lisa_22[,5]
map_sp_2022$significativo <- map_sp_2022$p_value < 0.05

# 2.7 Classificar clusters (High-High, Low-Low, Não-Significativo)
media_22 <- mean(map_sp_2022$razao_sexo, na.rm = TRUE)
map_sp_2022$tipo_cluster <- ifelse(
  map_sp_2022$significativo &
    map_sp_2022$razao_sexo >= media_22 &
    lag.listw(lw_22, map_sp_2022$razao_sexo) >= media_22, "High-High",
  ifelse(
    map_sp_2022$significativo &
      map_sp_2022$razao_sexo < media_22 &
      lag.listw(lw_22, map_sp_2022$razao_sexo) < media_22, "Low-Low",
    "Não-Significativo"
  )
)

# Adiciona os clusters ao objeto sf original (NÃO ao Spatial)
mapa_razao_sexo$tipo_cluster <- ifelse(
  map_sp_2022$significativo & 
    map_sp_2022$razao_sexo >= media_22 & 
    lag.listw(lw_22, map_sp_2022$razao_sexo) >= media_22, "High-High",
  ifelse(
    map_sp_2022$significativo & 
      map_sp_2022$razao_sexo < media_22 & 
      lag.listw(lw_22, map_sp_2022$razao_sexo) < media_22, "Low-Low",
    "Não-Significativo"
  )
)

# 2.9 Cartograma com ggplot2
ggplot(mapa_razao_sexo) +  # <<<--- OBJETO SF, NÃO SPATIAL!
  geom_sf(aes(fill = tipo_cluster), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "High-High"         = "#E41A1C",  # Vermelho (♂♂♂)
      "Low-Low"           = "#377EB8",  # Azul (♀♀♀)
      "Não-Significativo" = "#CCCCCC"   # Cinza claro
    ),
    name = "Padrão Espacial:"
  ) +
  labs(
    title    = "Clusters de Razão de Sexo na Paraíba (2022)",
    subtitle = "Análise LISA (Local Indicators of Spatial Association)",
    caption  = "Fonte: Censo Demográfico IBGE, 2022 | Elaboração própria"
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    plot.caption = element_text(color = "gray50", size = 9)
  )


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

## Gráfico de barras para Índice Myers
ggplot(myers_base, aes(x = factor(digito_final), y = percentual)) +
  geom_col(fill = "#0072B2") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  labs(
    title = paste0("Distribuição de dígitos finais (Myers = ", round(indice_myers, 2), ")"),
    x = "Dígito final da idade",
    y = "Percentual (%)"
  ) +
  theme_minimal()

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

## Gráfico comparativo entre 2010 e 2022
# Junta os dois anos em um único dataframe
myers_comparativo <- bind_rows(
  tibble(percentual = indice_myers_2010) %>% mutate(ano = "2010"),
  tibble(percentual = indice_myers)      %>% mutate(ano = "2022")
)

myers_comparativo <- tibble(
  ano = c("2010", "2022"),
  indice_myers = c(16.3, 6.03)
)

# Plot
# Gráfico
g <- ggplot(myers_comparativo, aes(x = ano, y = indice_myers, fill = ano)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("2010" = "#FF0000", "2022" = "#000000")) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = round(indice_myers, 2)), vjust = -0.5, color = "white", size = 5) +
  labs(
    title = "Figura 1 – Índice de Myers — Comparativo entre 2010 e 2022",
    x = "Ano",
    y = "Índice Myers (%)"
  ) +
  theme_minimal(base_size = 13)

# Fonte conforme ABNT
grid.arrange(g, bottom = textGrob("Fonte: IBGE — Cálculo próprio", 
                                  x = 0, hjust = 0, gp = gpar(fontsize = 10)))

# Gráfico Comparativo 2010-2022
# Processa dados de 2022
myers_2022 <- sexo_22 %>%
  filter(sexo == "Total") %>%
  mutate(
    idade_num = as.numeric(gsub("\\D", "", idade)),
    valor = as.numeric(valor)
  ) %>%
  filter(idade_num >= 10, idade_num <= 99) %>%
  mutate(digito_final = idade_num %% 10) %>%
  group_by(digito_final) %>%
  summarise(total = sum(valor), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    ano = "2022"
  )

# Processa dados de 2010
myers_2010 <- sexo_10 %>%
  filter(sexo == "Total") %>%
  mutate(
    idade_num = as.numeric(gsub("\\D", "", idade)),
    valor = as.numeric(valor)
  ) %>%
  filter(
    idade_num >= 10, idade_num <= 99,
    grepl("^\\d{2} anos$", idade)
  ) %>%
  mutate(digito_final = idade_num %% 10) %>%
  group_by(digito_final) %>%
  summarise(total = sum(valor), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    ano = "2010"
  )

# Junta os dois
myers_detalhado <- bind_rows(myers_2010, myers_2022)

# Gráfico comparativo
g <- ggplot(myers_detalhado, aes(x = factor(digito_final), y = percentual, fill = ano)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(percentual, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "white") +
  scale_fill_manual(values = c("2010" = "#005f73", "2022" = "#6c757d")) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray40") +
  labs(
    title = "Distribuição dos dígitos finais da idade — Censos 2010 e 2022",
    x = "Dígito final da idade",
    y = "Percentual (%)",
    fill = "Ano"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

bottom = textGrob("Fonte: IBGE. Censos Demográficos 2010 e 2022. Cálculo próprio.", 
                  x = 0, hjust = 0, gp = gpar(fontsize = 10, fontface = "italic"))



# Pirâmide Etária por Sexo ------------------------------------------------

# 1. Carregar dados
IDADE_SEXO_22 <- get_sidra(
  x = 9514,
  period = "2022",
  geo = "State",
  geo.filter = list("State" = 25),
  classific = "all",
  category = "all"
)

# 2. Processamento dos dados
dados22 <- IDADE_SEXO_22 %>%
  rename(sexo = "Sexo", idade_str = "Idade", populacao = "Valor") %>%
  filter(sexo %in% c("Homens", "Mulheres")) %>%
  mutate(
    idade = case_when(
      idade_str == "Menos de 1 ano" ~ 0,
      idade_str == "100 anos ou mais" ~ 100,
      str_detect(idade_str, "meses") ~ 0,
      TRUE ~ as.numeric(str_extract(idade_str, "\\d+"))
    ),
    faixa_etaria = cut(
      idade,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, Inf),
      labels = c("0–4", "5–9", "10–14", "15–19", "20–24", "25–29", "30–34", "35–39", "40–44", 
                 "45–49", "50–54", "55–59", "60–64", "65–69", "70–74", "75–79", "80–84", 
                 "85–89", "90–94", "95–99", "100+"),
      right = FALSE
    ),
    sexo = factor(sexo, levels = c("Homens", "Mulheres"))
  ) %>%
  filter(!is.na(faixa_etaria))

# 3. Preparar dados para pirâmide
piramide22 <- dados22 %>%
  group_by(faixa_etaria, sexo) %>%
  summarise(pop = sum(populacao, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    pop_abs = abs(pop),  # Valor absoluto para rótulos
    pop_display = ifelse(sexo == "Homens", -pop, pop)  # Para posicionamento no gráfico
  )

# 4. Visualização com rótulos
ggplot(piramide22, aes(x = faixa_etaria, y = pop_display, fill = sexo)) +
  geom_bar(stat = "identity", width = 0.95) +
  # Adicionar rótulos com valores
  geom_text(
    aes(
      label = format(pop_abs, big.mark = ".", decimal.mark = ",", scientific = FALSE),
      y = ifelse(piramide22$pop_display < 0, 
                 piramide22$pop_display - max(piramide22$pop_abs)*0.03, 
                 piramide22$pop_display + max(piramide22$pop_abs)*0.03),
      hjust = ifelse(piramide22$pop_display < 0, 1, 0)
    ),
    size = 3.5,
    color = "black"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(abs(x), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    limits = max(piramide22$pop_abs) * c(-1.2, 1.2)
  ) +
  scale_fill_manual(
    values = c("Homens" = "#E41A1C", "Mulheres" = "#377EB8"),
    labels = c("Masculino", "Feminino")
  ) +
  labs(
    title = "Pirâmide Etária — Paraíba, Censo 2022",
    x = "Faixa Etária",
    y = "População",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

# Pirâmide Etária 2010

# 1) Leitura dos dados, pulando as 3 primeiras linhas
IDADE_SEXO_10 <- read_excel(
  "IDADE_SEXO_10.xlsx",
  skip = 3,
  col_names = c("Cod_UF", "UF", "Faixa_Etaria", "Homens", "Mulheres")
)

# 2) Limpeza e pré‑processamento
dados10 <- IDADE_SEXO_10 %>%
  filter(
    !is.na(Faixa_Etaria),
    !Faixa_Etaria %in% c("Total", "Código", "Variável", "...")  # remove totais e rótulos
  ) %>%
  mutate(
    Homens   = as.numeric(Homens),
    Mulheres = as.numeric(Mulheres)
  ) %>%
  # 3) Agrupar em faixas etárias de 5 em 5 anos
  mutate(
    faixa_etaria = case_when(
      Faixa_Etaria %in% c("Menos de 1 ano", "0 ano", "1 ano", "2 anos", "3 anos", "4 anos", "0 a 4 anos") ~ "0–4",
      str_detect(Faixa_Etaria, "^[5-9] anos")    | Faixa_Etaria == "5 a 9 anos"   ~ "5–9",
      str_detect(Faixa_Etaria, "^1[0-4] anos")   | Faixa_Etaria == "10 a 14 anos"  ~ "10–14",
      str_detect(Faixa_Etaria, "^1[5-9] anos")   | Faixa_Etaria == "15 a 19 anos"  ~ "15–19",
      str_detect(Faixa_Etaria, "^2[0-4] anos")   | Faixa_Etaria == "20 a 24 anos"  ~ "20–24",
      str_detect(Faixa_Etaria, "^2[5-9] anos")   | Faixa_Etaria == "25 a 29 anos"  ~ "25–29",
      str_detect(Faixa_Etaria, "^3[0-4] anos")   | Faixa_Etaria == "30 a 34 anos"  ~ "30–34",
      str_detect(Faixa_Etaria, "^3[5-9] anos")   | Faixa_Etaria == "35 a 39 anos"  ~ "35–39",
      str_detect(Faixa_Etaria, "^4[0-4] anos")   | Faixa_Etaria == "40 a 44 anos"  ~ "40–44",
      str_detect(Faixa_Etaria, "^4[5-9] anos")   | Faixa_Etaria == "45 a 49 anos"  ~ "45–49",
      str_detect(Faixa_Etaria, "^5[0-4] anos")   | Faixa_Etaria == "50 a 54 anos"  ~ "50–54",
      str_detect(Faixa_Etaria, "^5[5-9] anos")   | Faixa_Etaria == "55 a 59 anos"  ~ "55–59",
      str_detect(Faixa_Etaria, "^6[0-4] anos")   | Faixa_Etaria == "60 a 64 anos"  ~ "60–64",
      str_detect(Faixa_Etaria, "^6[5-9] anos")   | Faixa_Etaria == "65 a 69 anos"  ~ "65–69",
      str_detect(Faixa_Etaria, "^7[0-4] anos")   | Faixa_Etaria == "70 a 74 anos"  ~ "70–74",
      str_detect(Faixa_Etaria, "^7[5-9] anos")   | Faixa_Etaria == "75 a 79 anos"  ~ "75–79",
      str_detect(Faixa_Etaria, "^8[0-4] anos")   | Faixa_Etaria == "80 a 84 anos"  ~ "80–84",
      str_detect(Faixa_Etaria, "^8[5-9] anos")   | Faixa_Etaria == "85 a 89 anos"  ~ "85–89",
      str_detect(Faixa_Etaria, "^9[0-4] anos")   | Faixa_Etaria == "90 a 94 anos"  ~ "90–94",
      str_detect(Faixa_Etaria, "^9[5-9] anos")   | Faixa_Etaria == "95 a 99 anos"  ~ "95–99",
      Faixa_Etaria == "100 anos ou mais"                                ~ "100+",
      TRUE                                                              ~ NA_character_
    )
  ) %>%
  filter(!is.na(faixa_etaria)) %>%
  # 4) “Wide” → “Long” para Homens/Mulheres
  pivot_longer(
    cols      = c(Homens, Mulheres),
    names_to  = "Sexo",
    values_to = "Populacao"
  ) %>%
  group_by(faixa_etaria, Sexo) %>%
  summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
  # 5) Garantir ordem correta das faixas
  mutate(
    faixa_etaria = factor(faixa_etaria, levels = c(
      "0–4","5–9","10–14","15–19","20–24","25–29","30–34",
      "35–39","40–44","45–49","50–54","55–59","60–64",
      "65–69","70–74","75–79","80–84","85–89","90–94",
      "95–99","100+"
    )),
    # valor negativo para homens (lado esquerdo) e positivo para mulheres
    valor = if_else(Sexo == "Homens", -Populacao, Populacao)
  )

# 6) Plot
ggplot(dados10, aes(x = faixa_etaria, y = valor, fill = Sexo)) +
  geom_col(width = 0.8) +
  coord_flip() +
  # mostra rótulos absolutos do lado de fora de cada barra
  geom_text(
    aes(label = comma(abs(Populacao))),
    hjust = if_else(dados10$Sexo == "Homens", 1.1, -0.1),
    size = 3,
    color = "black"
  ) +
  scale_y_continuous(
    labels = abs,
    breaks = pretty_breaks(n = 6)
  ) +
  scale_fill_manual(
    values = c("Homens" = "#1f78b4", "Mulheres" = "#e31a1c")
  ) +
  labs(
    x = "Faixa Etária",
    y = "População",
    title = "Pirâmide Etária — Paraíba 2010",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )



# Idade Média/Sexo - NE ---------------------------------------------------

# 1) Códigos dos 9 estados do Nordeste
uf_ne <- c(21,22,23,24,25,26,27,28,29)

# 2) Puxar população por idade simples (0,1,2,…,100+ anos) e sexo
pop_ne <- get_sidra(
  x           = 9514,
  period      = "2022",
  geo         = "State",
  geo.filter  = list("State" = uf_ne),
  classific   = "all",
  category    = "all"
) %>%
  filter(Sexo %in% c("Homens","Mulheres")) %>%
  rename(
    UF        = `Unidade da Federação`,
    IdadeRaw  = Idade,
    Sexo      = Sexo,
    Populacao = Valor
  ) %>%
  # 3) Converter Populacao e extrair número de anos em IdadeRaw
  mutate(
    Populacao = as.numeric(Populacao),
    IdadeNum = case_when(
      str_detect(IdadeRaw, "Menos de")       ~ 0,
      str_detect(IdadeRaw, "meses")          ~ 0,
      str_detect(IdadeRaw, "100 anos ou mais") ~ 100,
      str_detect(IdadeRaw, "\\d+")           ~ as.numeric(str_extract(IdadeRaw, "\\d+")),
      TRUE                                   ~ NA_real_
    )
  ) %>%
  filter(!is.na(IdadeNum))  # retira linhas como "Total", etc.

# 4) Calcular a idade média por UF
idade_media_ne <- pop_ne %>%
  group_by(UF, IdadeNum) %>%
  summarise(
    PopIdade = sum(Populacao, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(UF) %>%
  summarise(
    idade_media = sum(IdadeNum * PopIdade) / sum(PopIdade),
    .groups = "drop"
  ) %>%
  arrange(UF)

print(idade_media_ne)


idade_media_ne_sexo <- pop_ne %>%
  # 1) totaliza a população por estado, sexo e idade
  group_by(UF, Sexo, IdadeNum) %>%
  summarise(
    PopIdade = sum(Populacao, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  # 2) calcula a idade média ponderada dentro de cada UF e Sexo
  group_by(UF, Sexo) %>%
  summarise(
    idade_media = sum(IdadeNum * PopIdade) / sum(PopIdade),
    .groups     = "drop"
  ) %>%
  arrange(UF, Sexo)

print(idade_media_ne_sexo)

# Índice de Envelhecimento ------------------------------------------------

# 1. Baixar dados separadamente para evitar limites da API
# População 0-14 anos
dados_jovens <- get_sidra(
  x = 9514,
  variable = 93,
  period = "2022",
  geo = "City",
  geo.filter = list("State" = 25),
  classific = "c287",
  category = list("c287" = c(93070, 93084, 93085)), # 0-14 anos
  format = 3,
  header = TRUE
) %>% 
  select(cod_municipio = `Município (Código)`, municipio = Município, pop_0a14 = Valor)

# População 65+ anos
dados_idosos <- get_sidra(
  x = 9514,
  variable = 93,
  period = "2022",
  geo = "City",
  geo.filter = list("State" = 25),
  classific = "c287",
  category = list("c287" = c(93096, 93097, 93098, 49108, 49109, 60040, 60041, 6653)), # 65+ anos
  format = 3,
  header = TRUE
) %>% 
  select(cod_municipio = `Município (Código)`, municipio = Município, pop_65mais = Valor)

# 2. Combinar e calcular indicadores
dados_finais <- dados_jovens %>%
  inner_join(dados_idosos, by = c("cod_municipio", "municipio")) %>%
  mutate(
    # Calcular índice principal
    indice_envelhecimento = (pop_65mais / pop_0a14) * 100,
    
    # Calcular indicadores complementares
    percentual_idosos = (pop_65mais / (pop_0a14 + pop_65mais)) * 100,
    razao_dependencia = (pop_0a14 + pop_65mais) / (pop_0a14 + pop_65mais) * 100,
    
    # Classificação estratégica
    classificacao = case_when(
      indice_envelhecimento > 100 ~ "Crítico",
      indice_envelhecimento > 70 ~ "Alerta",
      indice_envelhecimento > 40 ~ "Moderado",
      TRUE ~ "Favorável"
    )
  ) %>%
  arrange(desc(indice_envelhecimento))

# 3. Análise de sensibilidade (comparativo 60+ vs 65+)
# População 60+ anos para comparação
dados_60mais <- get_sidra(
  x = 9514,
  variable = 93,
  period = "2022",
  geo = "City",
  geo.filter = list("State" = 25),
  classific = "c287",
  category = list("c287" = c(93095, 93096, 93097, 93098, 49108, 49109, 60040, 60041, 6653)), # 60+ anos
  format = 3,
  header = TRUE
) %>% 
  group_by(`Município (Código)`) %>%
  summarise(pop_60mais = sum(Valor), .groups = "drop") %>%
  select(cod_municipio = `Município (Código)`, pop_60mais)

# Adicionar ao dataframe principal
dados_finais <- dados_finais %>%
  left_join(dados_60mais, by = "cod_municipio") %>%
  mutate(
    indice_60mais = (pop_60mais / pop_0a14) * 100,
    diferenca_abs = indice_60mais - indice_envelhecimento,
    diferenca_rel = (diferenca_abs / indice_envelhecimento) * 100
  )

# 4. Salvar resultados detalhados
write_csv(dados_finais, "analise_envelhecimento_pb_completa.csv")

# 5. Visualizar top 15 municípios críticos
top_criticos <- dados_finais %>%
  select(
    municipio,
    "Índice (65+)" = indice_envelhecimento,
    "Índice (60+)" = indice_60mais,
    "Diferença (%)" = diferenca_rel,
    classificação = classificacao
  ) %>%
  arrange(desc(`Índice (65+)`)) %>%
  head(15)

print(top_criticos)

# Recriar dados_60mais com tratamento de agregação
dados_60mais <- get_sidra(
  x = 9514,
  variable = 93,
  period = "2022",
  geo = "City",
  geo.filter = list("State" = 25),
  classific = "c287",
  category = list("c287" = c(93095, 93096, 93097, 93098, 49108, 49109, 60040, 60041, 6653)),
  format = 3,
  header = TRUE
) %>% 
  group_by(`Município (Código)`, Município) %>%
  summarise(pop_60mais = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
  select(cod_municipio = `Município (Código)`, pop_60mais)

# Atualizar dados_finais com left_join seguro
dados_finais <- dados_finais %>%
  select(-matches("pop_60mais|indice_60mais")) %>%
  left_join(dados_60mais, by = "cod_municipio") %>%
  mutate(
    indice_60mais = (pop_60mais / pop_0a14) * 100,
    diferenca_abs = indice_60mais - indice_envelhecimento,
    diferenca_rel = (diferenca_abs / indice_envelhecimento) * 100
  ) %>%
  replace_na(list(indice_60mais = 0, diferenca_abs = 0, diferenca_rel = 0))

# Recriar top_criticos com dados completos
top_criticos <- dados_finais %>%
  arrange(desc(indice_envelhecimento)) %>%
  head(15) %>%
  select(
    municipio,
    "Índice (65+)" = indice_envelhecimento,
    "Índice (60+)" = indice_60mais,
    "Diferença (%)" = diferenca_rel,
    classificação = classificacao
  )

print(top_criticos)

# 6. Gráfico comparativo (versão corrigida)
ggplot(dados_finais, aes(x = indice_envelhecimento, y = indice_60mais)) +
  geom_point(aes(color = classificacao), size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(
    data = top_criticos,
    aes(x = `Índice (65+)`, 
        y = `Índice (60+)`, 
        label = municipio), 
    size = 3, 
    hjust = -0.1,
    vjust = 0.5,
    check_overlap = TRUE
  ) +
  scale_color_manual(values = c("Favorável" = "#1a9641", "Moderado" = "#a6d96a", 
                                "Alerta" = "#fdae61", "Crítico" = "#d7191c")) +
  labs(
    title = "Comparação de Índices de Envelhecimento na Paraíba",
    subtitle = "Censo Demográfico 2022 - Diferença entre critérios 60+ e 65+ anos",
    x = "Índice de Envelhecimento (65+ anos)",
    y = "Índice de Envelhecimento (60+ anos)",
    caption = "Fonte: IBGE SIDRA (Tabela 9514)",
    color = "Classificação:"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, max(dados_finais$indice_envelhecimento, na.rm = TRUE) * 1.1),
                  ylim = c(0, max(dados_finais$indice_60mais, na.rm = TRUE) * 1.1))


# Para realizar as coreções finais dos dados:
# Passo 1: Identificar e remover duplicatas
dados_finais <- dados_finais %>%
  distinct(cod_municipio, .keep_all = TRUE)

# Passo 2: Recalcular classificações
dados_finais <- dados_finais %>%
  mutate(
    indice_envelhecimento = (pop_65mais / pop_0a14) * 100,
    classificacao = case_when(
      indice_envelhecimento > 100 ~ "Crítico",
      indice_envelhecimento > 70 ~ "Alerta",
      indice_envelhecimento > 40 ~ "Moderado",
      TRUE ~ "Favorável"
    )
  ) %>%
  arrange(desc(indice_envelhecimento))

# Passo 3: Verificar correção
cat("Municípios únicos:", nrow(dados_finais), "\n")
print(table(dados_finais$classificacao))

# Recria top_criticos com os dados corrigidos
top_criticos <- dados_finais %>%
  arrange(desc(indice_envelhecimento)) %>%
  head(15) %>%
  select(
    municipio,
    "Índice (65+)" = indice_envelhecimento,
    "Índice (60+)" = indice_60mais,
    "Diferença (%)" = diferenca_rel,
    classificacao
  )

# Passo 1: Converter 'classificacao' em fator ordenado
dados_finais$classificacao <- factor(
  dados_finais$classificacao,
  levels = c("Favorável", "Moderado", "Alerta", "Crítico") # Ordem CORRETA
)

# Passo 2: Atualizar o top_criticos
top_criticos$classificacao <- factor(
  top_criticos$classificacao,
  levels = c("Favorável", "Moderado", "Alerta", "Crítico")
)

# Passo 3: Rodar o gráfico NOVAMENTE

ggplot(dados_finais, aes(x = indice_envelhecimento, y = indice_60mais)) +
  geom_point(aes(color = classificacao), size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(
    data = top_criticos,
    aes(x = `Índice (65+)`, 
        y = `Índice (60+)`, 
        label = municipio), 
    size = 3, 
    hjust = -0.1,
    vjust = 0.5,
    check_overlap = TRUE
  ) +
  scale_color_manual(values = c("Favorável" = "#1a9641", "Moderado" = "#a6d96a", 
                                "Alerta" = "#fdae61", "Crítico" = "#d7191c")) +
  labs(
    title = "Comparação de Índices de Envelhecimento na Paraíba",
    subtitle = "Censo 2022 - Dados Corrigidos",
    x = "Índice de Envelhecimento (65+ anos)",
    y = "Índice de Envelhecimento (60+ anos)",
    caption = "Fonte: IBGE | Elaboração Própria",
    color = "Classificação:"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(
    xlim = c(0, max(dados_finais$indice_envelhecimento, na.rm = TRUE) * 1.2),
    ylim = c(0, max(dados_finais$indice_60mais, na.rm = TRUE) * 1.2)
  )


# 1. Baixar shapefile da Paraíba (2020)
pb_shp <- read_municipality(
  code_muni = "PB",
  year = 2020,
  showProgress = FALSE
) %>%
  mutate(code_muni = substr(code_muni, 1, 6) %>% as.numeric())

# 2. Juntar com nossos dados
dados_mapa <- pb_shp %>%
  left_join(
    dados_finais %>% 
      mutate(cod_municipio = as.numeric(substr(cod_municipio, 1, 6))),
    by = c("code_muni" = "cod_municipio")
  )

# Solução definitiva para os triângulos amarelos - usando geometria de pontos
centroides_criticos <- dados_mapa %>%
  filter(classificacao == "Crítico") %>%
  st_centroid()

# 1. Criar o mapa corretamente com marcadores visíveis
mapa <- ggplot(dados_mapa) +
  # Camada principal de preenchimento
  geom_sf(aes(fill = classificacao), color = "white", size = 0.2, alpha = 0.9) +
  
  # Camada de marcadores VISÍVEIS - usando pontos e centroides
  geom_sf(
    data = centroides_criticos,
    aes(geometry = geom),
    color = "black",
    shape = 24,  # Triângulo
    fill = "yellow",
    size = 5,    # Tamanho aumentado
    stroke = 1.5 # Contorno reforçado
  ) +
  
  scale_fill_manual(
    values = c(
      "Crítico" = "#d7191c", 
      "Alerta" = "#fdae61",
      "Moderado" = "#a6d96a",
      "Favorável" = "#1a9641"
    ),
    name = "Classificação:"
  ) +
  
  # Elementos de mapa
  annotation_scale(
    location = "bl",  # Bottom-left
    width_hint = 0.3,
    pad_x = unit(1.5, "cm"),  # Mais afastado
    pad_y = unit(1.5, "cm")
  ) +
  annotation_north_arrow(
    location = "tr",  # Top-right
    height = unit(1.8, "cm"),
    width = unit(1.8, "cm"),
    pad_x = unit(1.5, "cm"),  # Mais afastado
    pad_y = unit(1.5, "cm")
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  )

# 2. Criar tabela formatada corretamente
tabela_criticos <- dados_finais %>%
  filter(classificacao %in% c("Crítico", "Alerta")) %>%
  arrange(desc(indice_envelhecimento)) %>%
  mutate(
    municipio = str_remove(municipio, " - PB"),
    indice = round(indice_envelhecimento, 1)
  ) %>%
  select(Município = municipio, `Índice (%)` = indice, Classificação = classificacao) %>%
  head(8)

# Formatar tabela com cores
cores_fundo <- ifelse(
  tabela_criticos$Classificação == "Crítico", "#fee090", "#fdae61"
)

tabela_grob <- tableGrob(
  tabela_criticos,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 12,
    padding = unit(c(6, 6), "mm"),
    core = list(
      bg_params = list(fill = cores_fundo, alpha = 0.8)
    ),
    colhead = list(
      bg_params = list(fill = "#2c3e50", alpha = 0.9),
      fg_params = list(col = "white", fontface = "bold")
    )
  )
)

# 3. Layout final garantido
plot_final <- ggdraw() +
  # Título
  draw_label(
    "VULNERABILIDADE PREVIDENCIÁRIA NA PARAÍBA",
    x = 0.5, y = 0.97, size = 24, fontface = "bold", color = "#2c3e50"
  ) +
  
  # Subtítulo
  draw_label(
    "Índice de Envelhecimento = (População ≥65 anos / População 0-14 anos) × 100",
    x = 0.5, y = 0.94, size = 16, color = "#34495e"
  ) +
  
  # Mapa (70% da altura)
  draw_plot(mapa, 0, 0.15, 1, 0.8) +
  
  # Tabela de municípios (direita inferior)
  draw_plot(
    ggplot() + 
      annotation_custom(tabela_grob) + 
      theme_void(),
    x = 0.6, y = 0.02, width = 0.35, height = 0.25
  ) +
  
  # Legenda dos ícones
  draw_label("▲ Municípios Críticos", x = 0.1, y = 0.1, size = 12, fontface = "bold") +
  
  # Legenda de cores (esquerda inferior)
  draw_label("Classificação:", x = 0.1, y = 0.07, size = 12, fontface = "bold", hjust = 0) +
  draw_label("Favorável", x = 0.1, y = 0.05, size = 11, color = "#1a9641", hjust = 0) +
  draw_label("Moderado", x = 0.2, y = 0.05, size = 11, color = "#a6d96a", hjust = 0) +
  draw_label("Alerta", x = 0.3, y = 0.05, size = 11, color = "#fdae61", hjust = 0) +
  draw_label("Crítico", x = 0.4, y = 0.05, size = 11, color = "#d7191c", hjust = 0) +
  
  # Fonte
  draw_label(
    "Fonte: IBGE Censo 2022 (Tabela 9514) | Elaboração Própria",
    x = 0.95, y = 0.03, size = 10, hjust = 1, color = "#7f8c8d"
  )

# Salvar com alta resolução
ggsave("índice de envelhecimento b.png", plot_final, width = 16, height = 14, dpi = 300, bg = "white")


# Razão de Dependência simples -------------------------------------------

# 1. Baixar e processar dados de forma consolidada
get_population_data <- function(categories, pop_name) {
  get_sidra(
    x = 9514,
    variable = 93,
    period = "2022",
    geo = "City",
    geo.filter = list("State" = 25),
    classific = "c287",
    category = list("c287" = categories),
    format = 3,
    header = TRUE
  ) %>% 
    group_by(`Município (Código)`, Município) %>%
    summarise(!!pop_name := sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    select(cod_municipio = `Município (Código)`, municipio = Município, !!pop_name) %>%
    mutate(cod_municipio = as.numeric(substr(cod_municipio, 1, 6)))
}

# Definir categorias por faixa etária
categorias_jovens <- c(93070, 93084, 93085)        # 0-14 anos
categorias_idosos <- c(93096, 93097, 93098, 49108, 49109, 60040, 60041, 6653) # 65+ anos
categorias_ativa <- c(93086:93095)                  # 15-64 anos (todas as subfaixas)

# Baixar dados de forma única e consolidada
dados_jovens <- get_population_data(categorias_jovens, "pop_0a14")
dados_idosos <- get_population_data(categorias_idosos, "pop_65mais")
dados_ativa <- get_population_data(categorias_ativa, "pop_15_64")

# 2. Combinar todos os dados sem duplicações
dados_completos <- dados_jovens %>%
  inner_join(dados_idosos, by = c("cod_municipio", "municipio")) %>%
  inner_join(dados_ativa, by = c("cod_municipio", "municipio")) %>%
  distinct(cod_municipio, .keep_all = TRUE) %>%  # Garantir unicidade
  mutate(
    # Calcular razão de dependência
    razao_dependencia = (pop_0a14 + pop_65mais) / pop_15_64 * 100,
    
    # Calcular índice de envelhecimento
    indice_envelhecimento = (pop_65mais / pop_0a14) * 100,
    
    # Classificação da razão de dependência
    classificacao_rd = case_when(
      razao_dependencia < 45 ~ "Baixa dependência",
      razao_dependencia < 60 ~ "Dependência moderada",
      TRUE ~ "Alta dependência"
    ),
    
    # Classificação do envelhecimento
    classificacao_ie = case_when(
      indice_envelhecimento > 100 ~ "Crítico",
      indice_envelhecimento > 70 ~ "Alerta",
      indice_envelhecimento > 40 ~ "Moderado",
      TRUE ~ "Favorável"
    )
  )

# Verificar número de municípios (deve ser 223)
cat("Número de municípios:", nrow(dados_completos), "\n")

# 3. Mapa temático da razão de dependência (corrigido)
pb_shp <- read_municipality("PB", year = 2020) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

dados_mapa_rd <- pb_shp %>%
  inner_join(dados_completos, by = c("code_muni" = "cod_municipio"))

ggplot(dados_mapa_rd) +
  geom_sf(aes(fill = razao_dependencia), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Razão de Dependência (%)",
    direction = -1,
    breaks = seq(30, 100, by = 10)
  ) +
  labs(
    title = "RAZÃO DE DEPENDÊNCIA DEMOGRÁFICA NA PARAÍBA",
    subtitle = "(População 0-14 + 65+ anos) / População 15-64 anos × 100",
    caption = "Fonte: IBGE Censo 2022"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.key.width = unit(2, "cm")
  )

# 4. Análise dos municípios mais críticos (top 10)
top_dependencia <- dados_completos %>%
  arrange(desc(razao_dependencia)) %>%
  select(
    municipio,
    "Pop 0-14" = pop_0a14,
    "Pop 65+" = pop_65mais,
    "Pop 15-64" = pop_15_64,
    "Razão (%)" = razao_dependencia,
    "Classificação" = classificacao_rd
  ) %>%
  head(10)

print(top_dependencia)

# 5. Gráfico de correlação entre indicadores
ggplot(dados_completos, aes(x = indice_envelhecimento, y = razao_dependencia)) +
  geom_point(aes(color = classificacao_ie), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  scale_color_manual(
    values = c("Favorável" = "#1a9641", "Moderado" = "#a6d96a",
               "Alerta" = "#fd8d3c", "Crítico" = "#d7191c"),
    name = "Nível de Envelhecimento"
  ) +
  labs(
    x = "Índice de Envelhecimento (%)",
    y = "Razão de Dependência (%)",
    title = "Relação entre Envelhecimento e Dependência Demográfica",
    subtitle = "Municípios da Paraíba - Censo 2022"
  ) +
  theme_minimal()

# 6. Análise estatística
cat("\n=== ANÁLISE ESTATÍSTICA ===\n")
cat("Média da Razão de Dependência:", round(mean(dados_completos$razao_dependencia), 1), "%\n")
cat("Mínimo:", round(min(dados_completos$razao_dependencia), 1), "%\n")
cat("Máximo:", round(max(dados_completos$razao_dependencia), 1), "%\n")
cat("Município com maior dependência:", top_dependencia$municipio[1], "=", round(top_dependencia$`Razão (%)`[1], 1), "%\n")
cat("Correlação entre indicadores:", round(cor(dados_completos$indice_envelhecimento, dados_completos$razao_dependencia), 2), "\n")


## Cartograma

# Verificar classificação
cat("\n=== VERIFICAÇÃO DE CLASSIFICAÇÃO ===\n")
dados_mapa %>%
  as.data.frame() %>%
  select(municipio, razao_dependencia, classificacao_rd) %>%
  arrange(desc(razao_dependencia)) %>%
  head(10) %>%
  print()

# Verificar cores da tabela
cat("\n=== CORES DA TABELA ===\n")
tabela_dados %>% 
  mutate(cor = cores_fundo) %>%
  print()


## Cartograma
# 1. Carregar shapefile e dados
pb_shp <- read_municipality("PB", year = 2020) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

dados_mapa <- pb_shp %>%
  left_join(dados_completos, by = c("code_muni" = "cod_municipio"))

# 2. Aplicar classificação de dependência COM FAIXAS AJUSTADAS
dados_mapa <- dados_mapa %>%
  mutate(
    classificacao_dep = case_when(
      razao_dependencia > 60 ~ "Crítico",
      razao_dependencia > 50 ~ "Alerta",
      razao_dependencia > 40 ~ "Moderado",
      TRUE ~ "Favorável"
    )
  )

# 3. Selecionar top 10 dependência para tabela
top10_dependencia <- dados_mapa %>%
  st_drop_geometry() %>%
  arrange(desc(razao_dependencia)) %>%
  head(10)

# 4. Selecionar APENAS CRÍTICOS para marcadores (máximo 3)
municipios_criticos <- dados_mapa %>%
  filter(classificacao_dep == "Crítico") %>%
  arrange(desc(razao_dependencia)) %>%
  head(3)  # No máximo 3 municípios

centroides_criticos <- municipios_criticos %>%
  st_centroid()

# 5. Criar mapa
mapa <- ggplot(dados_mapa) +
  geom_sf(aes(fill = classificacao_dep), color = "white", size = 0.2, alpha = 0.9) +
  
  # Marcadores APENAS para municípios críticos (máximo 3)
  geom_sf(
    data = centroides_criticos,
    aes(geometry = geom),
    color = "black",
    shape = 24,
    fill = "yellow",
    size = 5,    # Aumentado para melhor visibilidade
    stroke = 1.5 # Contorno reforçado
  ) +
  
  scale_fill_manual(
    values = c(
      "Crítico" = "#d7191c",
      "Alerta" = "#fdae61",
      "Moderado" = "#a6d96a",
      "Favorável" = "#1a9641"
    ),
    name = "Classificação de Dependência:"
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    pad_x = unit(1.5, "cm"),
    pad_y = unit(1.5, "cm")
  ) +
  annotation_north_arrow(
    location = "tr",
    height = unit(1.8, "cm"),
    width = unit(1.8, "cm"),
    pad_x = unit(1.5, "cm"),
    pad_y = unit(1.5, "cm")
  ) +
  theme_void() +
  theme(legend.position = "none")

# 6. Criar tabela com top 10
tabela_dados <- top10_dependencia %>%
  mutate(
    municipio = str_remove(municipio, " - PB"),
    razao = round(razao_dependencia, 1)
  ) %>%
  select(Município = municipio, `Razão (%)` = razao, Classificação = classificacao_dep)

# Definir cores com transparência reduzida
cores_fundo <- case_when(
  tabela_dados$Classificação == "Crítico" ~ "#d7191c80",  # Vermelho com 50% transparência
  tabela_dados$Classificação == "Alerta" ~ "#fdae6180",    # Laranja com 50% transparência
  tabela_dados$Classificação == "Moderado" ~ "#a6d96a80",  # Verde claro com 50% transparência
  tabela_dados$Classificação == "Favorável" ~ "#1a964180"  # Verde com 50% transparência
)

# Tabela minimalista
tabela_grob <- tableGrob(
  tabela_dados,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 12,
    padding = unit(c(6, 6), "mm"),
    core = list(
      bg_params = list(fill = cores_fundo),
      fg_params = list(col = "black")  # Texto preto para contraste
    ),
    colhead = list(
      bg_params = list(fill = "#2c3e50"),
      fg_params = list(col = "white", fontface = "bold")
    )
  )
)

# 7. Layout final
plot_final <- ggdraw() +
  draw_label(
    "VULNERABILIDADE PREVIDENCIÁRIA NA PARAÍBA",
    x = 0.5, y = 0.97, size = 24, fontface = "bold", color = "#2c3e50"
  ) +
  
  draw_label(
    "Razão de Dependência = (População 0-14 + 65+ anos) / População 15-64 anos × 100",
    x = 0.5, y = 0.94, size = 16, color = "#34495e"
  ) +
  
  draw_plot(mapa, 0, 0.15, 1, 0.8) +
  
  draw_plot(
    ggplot() + annotation_custom(tabela_grob) + theme_void(),
    x = 0.6, y = 0.02, width = 0.35, height = 0.25
  ) +
  
  draw_label("▲ Municípios Críticos de Dependência", 
             x = 0.1, y = 0.1, size = 12, fontface = "bold") +
  
  draw_label("Classificação de Dependência:", 
             x = 0.1, y = 0.07, size = 12, fontface = "bold", hjust = 0) +
  draw_label("Favorável (<40%)", x = 0.1, y = 0.05, size = 11, color = "#1a9641", hjust = 0) +
  draw_label("Moderado (40-50%)", x = 0.25, y = 0.05, size = 11, color = "#a6d96a", hjust = 0) +
  draw_label("Alerta (50-60%)", x = 0.4, y = 0.05, size = 11, color = "#fdae61", hjust = 0) +
  draw_label("Crítico (>60%)", x = 0.55, y = 0.05, size = 11, color = "#d7191c", hjust = 0) +
  
  draw_label(
    "Fonte: IBGE Censo 2022 (Tabela 9514) | Elaboração Própria",
    x = 0.95, y = 0.03, size = 10, hjust = 1, color = "#7f8c8d"
  )

# 8. Salvar
ggsave("cartograma_dependencia_pb_final.png", plot_final, 
       width = 16, height = 14, dpi = 300, bg = "white")


## Scatterplot 2²
# 0. Criar a classificação de dependência (se ainda não existir)
dados_completos <- dados_completos %>%
  mutate(
    classificacao_dep = case_when(
      razao_dependencia > 60 ~ "Crítico",
      razao_dependencia > 50 ~ "Alerta",
      razao_dependencia > 40 ~ "Moderado",
      TRUE ~ "Favorável"
    )
  ) %>%
  # Converter para fator ordenado diretamente
  mutate(classificacao_dep = factor(
    classificacao_dep,
    levels = c("Favorável", "Moderado", "Alerta", "Crítico")
  ))

# 1. Preparar os 10 municípios com maior razão de dependência
top10_dependencia <- dados_completos %>%
  arrange(desc(razao_dependencia)) %>%
  head(10) %>%
  mutate(municipio_simples = str_remove(municipio, " - PB"))

# 2. Criar o scatterplot
ggplot(dados_completos, aes(x = indice_envelhecimento, y = razao_dependencia)) +
  # Linhas de referência
  geom_hline(yintercept = mean(dados_completos$razao_dependencia, na.rm = TRUE),
             linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = mean(dados_completos$indice_envelhecimento, na.rm = TRUE),
             linetype = "dashed", color = "gray50") +
  
  # Todos os pontos
  geom_point(aes(color = classificacao_dep), size = 3, alpha = 0.7) +
  
  # Destacar os top 10 dependência
  geom_point(data = top10_dependencia, 
             shape = 1, size = 4, color = "black", stroke = 1.2) +
  
  # Rótulos para os top 10
  geom_text_repel(
    data = top10_dependencia,
    aes(label = municipio_simples),
    size = 3.5,
    point.padding = 0.3,
    min.segment.length = 0.2,
    segment.color = "grey40"
  ) +
  
  # Linha de tendência
  geom_smooth(method = "lm", se = FALSE, color = "#3498db", size = 1) +
  
  # Escala de cores
  scale_color_manual(
    values = c("Favorável" = "#1a9641", "Moderado" = "#a6d96a", 
               "Alerta" = "#fdae61", "Crítico" = "#d7191c"),
    name = "Nível de Dependência:"
  ) +
  
  labs(
    title = "RELAÇÃO ENTRE ENVELHECIMENTO E DEPENDÊNCIA NA PARAÍBA",
    subtitle = "Municípios com maior razão de dependência destacados (top 10)",
    x = "Índice de Envelhecimento (%)",
    y = "Razão de Dependência (%)",
    caption = "Fonte: IBGE Censo 2022 | Elaboração própria"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "grey90")
  ) +
  
  coord_cartesian(
    xlim = c(0, max(dados_completos$indice_envelhecimento, na.rm = TRUE) * 1.1),
    ylim = c(0, max(dados_completos$razao_dependencia, na.rm = TRUE) * 1.1)
  )

# 1. Converter classificações para fator
dados_completos <- dados_completos %>%
  mutate(
    classificacao_rd = factor(
      classificacao_rd,
      levels = c("Favorável", "Moderado", "Alerta", "Crítico")
    ),
    classificacao_ie = factor(
      classificacao_ie,
      levels = c("Favorável", "Moderado", "Alerta", "Crítico")
    )
  )

# 2. Preparar os 10 municípios com maior razão de dependência
top10_dependencia <- dados_completos %>%
  arrange(desc(razao_dependencia)) %>%
  head(10) %>%
  mutate(municipio_simples = str_remove(municipio, " - PB"))

# 3. Criar o scatterplot
ggplot(dados_completos, aes(x = indice_envelhecimento, y = razao_dependencia)) +
  # Linhas de referência
  geom_hline(yintercept = mean(dados_completos$razao_dependencia, na.rm = TRUE),
             linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = mean(dados_completos$indice_envelhecimento, na.rm = TRUE),
             linetype = "dashed", color = "gray50") +
  
  # Todos os pontos coloridos pela classificação de DEPENDÊNCIA
  geom_point(aes(color = classificacao_rd), size = 3, alpha = 0.7) +
  
  # Destacar os top 10 dependência
  geom_point(data = top10_dependencia, 
             shape = 1, size = 4, color = "black", stroke = 1.2) +
  
  # Rótulos para os top 10
  geom_text_repel(
    data = top10_dependencia,
    aes(label = municipio_simples),
    size = 3.5,
    point.padding = 0.3,
    min.segment.length = 0.2,
    segment.color = "grey40"
  ) +
  
  # Linha de tendência
  geom_smooth(method = "lm", se = FALSE, color = "#3498db", size = 1) +
  
  # Escala de cores - usando a mesma paleta
  scale_color_manual(
    values = c("Favorável" = "#1a9641", "Moderado" = "#a6d96a", 
               "Alerta" = "#fdae61", "Crítico" = "#d7191c"),
    name = "Nível de Dependência:"
  ) +
  
  labs(
    title = "RELAÇÃO ENTRE ENVELHECIMENTO E DEPENDÊNCIA NA PARAÍBA",
    subtitle = "Municípios com maior razão de dependência destacados (top 10)",
    x = "Índice de Envelhecimento (%)",
    y = "Razão de Dependência (%)",
    caption = "Fonte: IBGE Censo 2022 | Elaboração própria"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "grey90")
  ) +
  
  coord_cartesian(
    xlim = c(0, max(dados_completos$indice_envelhecimento, na.rm = TRUE) * 1.1),
    ylim = c(0, max(dados_completos$razao_dependencia, na.rm = TRUE) * 1.1)
  )


write.csv(top10_dependencia,
          file = "top10_dependencia.csv")


# Malha Estadual ----------------------------------------------------------


# Carregar pacotes necessários
library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(knitr)  # Para impressão elegante de tabelas

# Baixar dados dos municípios da Paraíba
muni_pb <- read_municipality(code_muni = "PB", year = 2020)

# Converter para CRS projetado
muni_pb <- muni_pb %>% 
  st_transform(crs = 5880)  # SIRGAS 2000 / Brazil Polyconic

# Adicionar coluna numérica sequencial (1 a 223) em ordem alfabética
muni_pb <- muni_pb %>% 
  arrange(name_muni) %>% 
  mutate(codigo = 1:n())

# Calcular centroides
centroides <- muni_pb %>% 
  st_centroid()

# Criar mapa com números municipais
ggplot() +
  geom_sf(data = muni_pb, fill = "white", color = "gray60", linewidth = 0.2) +
  geom_sf_text(data = centroides, aes(label = codigo), size = 2.0, color = "black") +
  labs(title = "Municípios da Paraíba") +
  theme_void()

# Salvar mapa
ggsave("mapa_paraiba.png", width = 10, height = 10, dpi = 300)

# Criar tabela de referência
tabela_referencia <- muni_pb %>%
  st_drop_geometry() %>%
  select(codigo, name_muni) %>%
  arrange(codigo) %>%
  rename(Código = codigo, Município = name_muni)

# SOLUÇÃO 1: Salvar tabela em HTML (mais elegante)
library(kableExtra)

tabela_referencia %>%
  kable(format = "html", align = "c") %>%
  kable_styling("striped", full_width = FALSE) %>%
  save_kable("tabela_municipios.html")

# SOLUÇÃO 2: Salvar tabela em PDF
# install.packages("tinytex") # Se necessário
# tinytex::install_tinytex()  # Para criar PDFs

tabela_referencia %>%
  kable(format = "latex", booktabs = TRUE, align = "c") %>%
  save_kable("tabela_municipios.pdf")

# SOLUÇÃO 3: Visualizar no Viewer do RStudio
View(tabela_referencia)

# SOLUÇÃO 4: Imprimir em partes no console
cat("=== Tabela de Municípios da Paraíba (1-50) ===\n")
print(tabela_referencia[1:50, ], row.names = FALSE)

cat("\n\n=== Tabela de Municípios da Paraíba (51-100) ===\n")
print(tabela_referencia[51:100, ], row.names = FALSE)

cat("\n\n=== Tabela de Municípios da Paraíba (101-150) ===\n")
print(tabela_referencia[101:150, ], row.names = FALSE)

cat("\n\n=== Tabela de Municípios da Paraíba (151-200) ===\n")
print(tabela_referencia[151:200, ], row.names = FALSE)

cat("\n\n=== Tabela de Municípios da Paraíba (201-223) ===\n")
print(tabela_referencia[201:223, ], row.names = FALSE)

# Salvar tabela em CSV
write.csv(tabela_referencia, "referencia_municipios.csv", row.names = FALSE)


# Transição Demográfica Paraibana -----------------------------------------


# 1. 2010
get_2010_data <- function() {
  # Obter dados brutos
  dados <- get_sidra(
    x = 1378,
    variable = 93,
    period = "2010",
    geo = "City",
    geo.filter = list("State" = 25),
    classific = "c287",  # Classificação por idade
    category = "all",    # Todas as categorias de idade
    format = 4
  )
  
  # Verificar e processar
  if (nrow(dados) == 0) stop("Nenhum dado retornado para 2010")
  
  # Criar faixas etárias
  dados_limpos <- dados %>%
    rename(
      cod_municipio = `Município (Código)`,
      municipio = `Município`,
      idade_grupo = `Idade`,
      populacao = Valor
    ) %>% 
    mutate(
      faixa = case_when(
        idade_grupo %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos") ~ "pop_0a14",
        idade_grupo %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                           "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
                           "55 a 59 anos", "60 a 64 anos") ~ "pop_15_64",
        idade_grupo %in% c("65 a 69 anos", "70 a 74 anos", "75 a 79 anos", "80 anos ou mais") ~ "pop_65mais",
        TRUE ~ "outros"
      )
    ) %>% 
    filter(faixa != "outros") %>% 
    group_by(cod_municipio, municipio, faixa) %>% 
    summarise(populacao = sum(populacao), .groups = "drop") %>% 
    pivot_wider(names_from = faixa, values_from = populacao) %>% 
    mutate(
      cod_municipio = as.numeric(substr(cod_municipio, 1, 6)),
      ano = 2010
    )
  
  return(dados_limpos)
}

# Calcular OADR e YDR para 2010
dados_2010_completo <- dados_2010 %>%
  mutate(
    OADR = (pop_65mais / pop_15_64) * 100,
    YDR = (pop_0a14 / pop_15_64) * 100,
    ano = 2010
  ) %>%
  select(cod_municipio, municipio, ano, pop_0a14, pop_15_64, pop_65mais, OADR, YDR)

# Selecionar colunas equivalentes para 2022
dados_2022_completo <- dados_completos %>%
  mutate(ano = 2022) %>%
  select(cod_municipio, municipio, ano, pop_0a14, pop_15_64, pop_65mais, OADR, YDR)

# Combinar ambos os anos
dados_anos <- bind_rows(dados_2010_completo, dados_2022_completo)

# Calcular mudanças durante os 12 anos
mudancas <- dados_anos %>%
  select(cod_municipio, municipio, ano, OADR, YDR) %>%
  pivot_wider(
    names_from = ano,
    values_from = c(OADR, YDR),
    names_glue = "{.value}_{ano}"
  ) %>%
  mutate(
    mudanca_OADR = OADR_2022 - OADR_2010,
    mudanca_YDR = YDR_2022 - YDR_2010,
    tendencia = case_when(
      mudanca_OADR > 5 & mudanca_YDR < -5 ~ "Envelhecimento Acelerado",
      mudanca_OADR > 2 & mudanca_YDR < -2 ~ "Envelhecimento Moderado",
      mudanca_YDR > 2 ~ "Rejuvenescimento",
      TRUE ~ "Estável"
    )
  )

# Usando o mesmo mapa que você já tem para 2022
dados_mapa_mudancas <- pb_municipios %>%
  left_join(mudancas, by = c("code_muni" = "cod_municipio"))

mapa_transicao <- ggplot(dados_mapa_mudancas) +
  geom_sf(aes(fill = tendencia), color = "white", size = 0.1) +
  scale_fill_manual(
    name = "Tendência Demográfica",
    values = c(
      "Envelhecimento Acelerado" = "#e41a1c",  # Vermelho
      "Envelhecimento Moderado" = "#ff7f00",    # Laranja
      "Rejuvenescimento" = "#377eb8",           # Azul
      "Estável" = "#4daf4a"                     # Verde
    )
  ) +
  labs(title = "Transição Demográfica na Paraíba (2010-2022)",
       subtitle = "Mudanças nas Razões de Dependência de Idosos (OADR) e Jovens (YDR)") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

ggsave("transicao_demografica_pb.png", mapa_transicao, width = 10, height = 8, dpi = 300)

top_mudancas <- mudancas %>%
  arrange(desc(abs(mudanca_OADR))) %>%
  slice_head(n = 10) %>%
  select(municipio, 
         OADR_2010, OADR_2022, mudanca_OADR,
         YDR_2010, YDR_2022, mudanca_YDR,
         tendencia)

print(top_mudancas)



# Pirâmide Etária 10 vs 22 PB ---------------------------------------------


# ============================================
# Pirâmides etárias PB — 2010 vs 2022 (SIDRA 1378 e 9514)
# - mesma escala
# - 0–4 na base, 100+ no topo (sem inversão)
# - overlay e pirâmide da diferença
# ============================================

# pacotes
library(sidrar)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
options(stringsAsFactors = FALSE)

dir.create("Figuras", showWarnings = FALSE)

# --------------------------------------------
# helpers
# --------------------------------------------

# Detecta a coluna "Idade" (ignora "(Código)" e "Forma de declaração")
find_idade_col <- function(df){
  cands <- names(df)[grepl("(?i)\\bidade\\b", names(df))]
  cands <- setdiff(cands, "Forma de declaração da idade")
  cands <- cands[!grepl("(?i)c[oó]digo", cands)]
  if (length(cands) < 1) {
    stop("Não encontrei coluna de 'Idade'. Nomes: ", paste(names(df), collapse = ", "))
  }
  cands[1]
}

# Extrai idade base: "Menos de 1"->0, "100 ou mais"->100, "45 a 49"->45, "37 anos"->37
parse_idade <- function(x){
  x <- trimws(x)
  out <- ifelse(grepl("Menos de 1", x, ignore.case = TRUE), 0,
                ifelse(grepl("ou mais", x, ignore.case = TRUE), 100,
                       ifelse(grepl(" a ", x), as.numeric(sub(" .*", "", x)),
                              as.numeric(str_extract(x, "\\d+")))))
  out
}

# Rotula grupos quinquenais até 100+
cut_5y <- function(a){
  a <- pmin(pmax(a,0), 120)
  brk <- c(seq(0,100,5), Inf)
  lab <- c(paste(seq(0,95,5), "a", seq(4,99,5)), "100+")
  cut(a, breaks = brk, labels = lab, right = TRUE, include.lowest = TRUE)
}

# Leitura robusta para 1378 (2010) e 9514 (2022) — PB (UF=25)
fetch_pb <- function(table_id, year){
  df <- sidrar::get_sidra(
    x          = table_id,
    variable   = 93,                # População residente
    period     = as.character(year),
    geo        = "State",
    geo.filter = list(State = 25)   # 25 = PB
  )
  
  # 9514 tem esta classificação; 1378 não
  if ("Forma de declaração da idade" %in% names(df)){
    df <- df %>% dplyr::filter(`Forma de declaração da idade` == "Total")
  }
  
  idade_col <- find_idade_col(df)
  
  df %>%
    dplyr::filter(Sexo %in% c("Homens","Mulheres")) %>%
    dplyr::filter(!grepl("^Total$", .data[[idade_col]], ignore.case = TRUE)) %>%
    dplyr::filter(!grepl("ignorada|não declarad", .data[[idade_col]], ignore.case = TRUE)) %>%
    dplyr::transmute(
      year      = year,
      sex       = Sexo,
      idade_lab = .data[[idade_col]],
      idade_num = parse_idade(.data[[idade_col]]),
      pop       = as.numeric(Valor)
    ) %>%
    dplyr::mutate(age_group = cut_5y(idade_num)) %>%
    dplyr::group_by(year, sex, age_group) %>%
    dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
}

# --------------------------------------------
# baixa / organiza
# --------------------------------------------

pb2010 <- fetch_pb(1378, 2010)
pb2022 <- fetch_pb(9514, 2022)

pb <- bind_rows(pb2010, pb2022) %>%
  group_by(year, sex) %>%
  mutate(share = pop / sum(pop)) %>%
  ungroup()

# ordem natural (0–4 na base, 100+ no topo)
age_levels <- levels(pb$age_group)

# valor com sinal p/ pirâmide (homens à esquerda)
pyr <- pb %>%
  mutate(
    val       = ifelse(sex == "Homens", -share, share),
    age_group = factor(age_group, levels = age_levels)  # mantém ordem natural (sem fct_rev)
  )

MAX <- max(abs(pyr$val), na.rm = TRUE)

# --------------------------------------------
# (A) Pirâmides lado a lado (mesma escala)
# --------------------------------------------
p_facets <- ggplot(pyr, aes(x = val, y = age_group, fill = sex)) +
  geom_col(width = .9, alpha = .95) +
  facet_wrap(~year, ncol = 2, scales = "fixed") +
  scale_y_discrete(limits = age_levels) +  # garante a ordem 0–4 ... 100+
  scale_x_continuous(limits = c(-MAX, MAX),
                     labels = function(x) percent(abs(x), accuracy = 1)) +
  scale_fill_manual(values = c("Homens"="#6CC3B2","Mulheres"="#1F6FB2")) +
  labs(x = NULL, y = NULL,
       title = "Pirâmides etárias – Paraíba (2010 vs 2022)",
       subtitle = "Escala fixa e grupos quinquenais idênticos",
       fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_line(color = "#eeeeee"),
        legend.position = "bottom")
ggsave("Figuras/piramides_pb_facets.png", p_facets, width = 10, height = 5, dpi = 300)

# --------------------------------------------
# (B) Overlay: 2010 preenchido × 2022 contorno
# --------------------------------------------
p_overlay <- ggplot() +
  geom_col(data = dplyr::filter(pyr, year == 2010),
           aes(x = val, y = age_group, fill = sex),
           alpha = .28, width = .9, position = "identity") +
  geom_col(data = dplyr::filter(pyr, year == 2022),
           aes(x = val, y = age_group, color = sex),
           fill = NA, width = .9, linewidth = 1, position = "identity") +
  scale_y_discrete(limits = age_levels) +
  scale_x_continuous(limits = c(-MAX, MAX),
                     labels = function(x) percent(abs(x))) +
  scale_fill_manual(values = c("Homens"="#6CC3B2","Mulheres"="#1F6FB2")) +
  scale_color_manual(values = c("Homens"="#2C7F73","Mulheres"="#0F4C81")) +
  labs(x=NULL, y=NULL,
       title = "Overlay 2010×2022 – Paraíba",
       subtitle = "2010 (preenchido) vs 2022 (contorno)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_line(color = "#eeeeee"),
        legend.position = "bottom")
ggsave("Figuras/piramides_pb_overlay.png", p_overlay, width = 8, height = 6, dpi = 300)

# --------------------------------------------
# (C) Pirâmide da diferença (Δ participação 2022–2010)
# --------------------------------------------
# ====== RÓTULOS (adicione depois de criar p_facets, p_overlay e p_diff) ======

# helper: formata pontos percentuais com sinal
fmt_pp <- function(x){
  sinal <- ifelse(x >= 0, "+", "−")
  paste0(sinal, scales::number(abs(x)*100, accuracy = 0.1, decimal.mark = ","), " p.p.")
}

# ---------- (C) rótulos na PIRÂMIDE DA DIFERENÇA ----------
label_min <- 0.003  # só rotula |Δ| >= 0,3 p.p.

labs_diff <- delta %>%
  dplyr::filter(abs(diff) >= label_min) %>%
  dplyr::mutate(
    lab  = fmt_pp(diff),
    xpos = ifelse(val > 0, val + MAX*0.01, val - MAX*0.01), # afasta um tiquinho
    hj   = ifelse(val > 0, 0, 1)
  )

p_diff <- p_diff +
  geom_text(data = labs_diff,
            aes(x = xpos, y = age_group, label = lab, hjust = hj),
            size = 3, check_overlap = TRUE)

ggsave("Figuras/piramide_pb_diferenca.png", p_diff, width = 7, height = 6.5, dpi = 300)


# ---------- (A) rótulos discretos nas PIRÂMIDES LADO A LADO ----------
label_min_share <- 0.02  # rotula faixas com ≥ 2% ou os extremos

labs_facets <- pyr %>%
  dplyr::filter(share >= label_min_share | age_group %in% c("0 a 4","100+")) %>%
  dplyr::mutate(
    lab  = scales::percent(abs(share), accuracy = 0.1, decimal.mark = ","),
    xpos = ifelse(val > 0, val + MAX*0.008, val - MAX*0.008),
    hj   = ifelse(val > 0, 0, 1)
  )

p_facets <- p_facets +
  geom_text(data = labs_facets,
            aes(x = xpos, y = age_group, label = lab, hjust = hj),
            size = 3, check_overlap = TRUE)

ggsave("Figuras/piramides_pb_facets.png", p_facets, width = 10, height = 5, dpi = 300)


# ---------- (B) (opcional) rótulos mínimos no OVERLAY ----------
# Comentado para não poluir. Descomente se quiser.
# labs_overlay <- pyr %>%
#   dplyr::filter(year == 2022, age_group %in% c("0 a 4","60 a 64","80 a 84")) %>%
#   dplyr::mutate(
#     lab  = scales::percent(abs(share), accuracy = 0.1, decimal.mark = ","),
#     xpos = ifelse(val > 0, val + MAX*0.01, val - MAX*0.01),
#     hj   = ifelse(val > 0, 0, 1)
#   )
#
# p_overlay <- p_overlay +
#   geom_text(data = labs_overlay,
#             aes(x = xpos, y = age_group, label = lab, hjust = hj),
#             size = 3, check_overlap = TRUE)
#
# ggsave("Figuras/piramides_pb_overlay.png", p_overlay, width = 8, height = 6, dpi = 300)

# fim

# soma sexos e calcula participação sobre o total do ano
pyr_tot <- bind_rows(pb2010, pb2022) |>
  dplyr::group_by(year, age_group) |>
  dplyr::summarise(pop = sum(pop), .groups="drop") |>
  dplyr::group_by(year) |>
  dplyr::mutate(share = pop / sum(pop)) |>
  dplyr::ungroup()

delta_tot <- pyr_tot |>
  tidyr::pivot_wider(names_from = year, values_from = share) |>
  dplyr::mutate(diff_pp = 100*(`2022` - `2010`))  # pontos percentuais

lim <- max(abs(delta_tot$diff_pp))

ggplot(delta_tot, aes(y = age_group, x = diff_pp, fill = diff_pp)) +
  geom_col(width = .9) +
  scale_x_continuous(limits = c(-lim, lim),
                     labels = function(x) paste0(abs(x), " p.p.")) +
  scale_fill_gradient2(low = "#C0392B", mid = "#F4F4F4", high = "#2E86C1",
                       midpoint = 0, guide = "none") +
  labs(x = NULL, y = NULL,
       title = "Mudança na estrutura etária – Paraíba (2022–2010)",
       subtitle = "Direita: ganhou participação; Esquerda: perdeu") +
  theme_minimal(base_size = 11)


# Nova Impressão -----------------------------------------------------------------------

totais <- bind_rows(pb2010, pb2022) |>
  dplyr::group_by(year) |>
  dplyr::summarise(total_ano = sum(pop), .groups="drop")

pyr_sex <- bind_rows(pb2010, pb2022) |>
  dplyr::left_join(totais, by="year") |>
  dplyr::group_by(year, sex, age_group) |>
  dplyr::summarise(pop = sum(pop), total_ano = dplyr::first(total_ano), .groups="drop") |>
  dplyr::mutate(share = pop / total_ano)

delta_sex <- pyr_sex |>
  dplyr::select(year, sex, age_group, share) |>
  tidyr::pivot_wider(names_from = year, values_from = share) |>
  dplyr::mutate(diff_pp = 100*(`2022` - `2010`),
                val = ifelse(sex=="Homens", -diff_pp, diff_pp))

lim <- max(abs(delta_sex$diff_pp))

ggplot(delta_sex, aes(y = age_group, x = val, fill = diff_pp)) +
  geom_col(width = .9) +
  scale_x_continuous(limits = c(-lim, lim),
                     labels = function(x) paste0(abs(x), " p.p.")) +
  scale_fill_gradient2(low = "#C0392B", mid = "#F4F4F4", high = "#2E86C1",
                       midpoint = 0, guide = "none") +
  labs(x=NULL, y=NULL,
       title = "Mudança por sexo – Paraíba (2022–2010)",
       subtitle = "Esq.: homens (p.p. negativos), Dir.: mulheres (p.p. positivos)") +
  theme_minimal(base_size = 11)

ggsave("Figuras/piramides_pb_diff.png", p_facets, width = 10, height = 5, dpi = 300)

# ver shares 95–99 e 100+ por sexo/ano
pyr |>
  filter(age_group %in% c("95 a 99","100+")) |>
  select(year, sex, age_group, share) |>
  tidyr::pivot_wider(names_from = year, values_from = share) |>
  mutate(diff_pp = (`2022` - `2010`)*100) |>
  arrange(sex, age_group)

# checar também 95+ combinado (se sobe, ok; só trocou de 95–99 p/ 100+)
pyr |>
  mutate(top = ifelse(age_group %in% c("95 a 99","100+"), "95+", as.character(age_group))) |>
  group_by(year, sex, top) |>
  summarise(share = sum(share), .groups = "drop") |>
  filter(top=="95+") |>
  tidyr::pivot_wider(names_from = year, values_from = share) |>
  mutate(diff_pp = (`2022` - `2010`)*100)



# Novo Scatter IE vs RDD --------------------------------------------------

# pacotes
library(sidrar)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(geobr)
library(sf)
library(patchwork)
library(scales)
library(purrr)

options(stringsAsFactors = FALSE)
sf::sf_use_s2(FALSE)

# -----------------------------
# Helpers
# -----------------------------
find_idade_col <- function(df){
  nm <- names(df)
  cand <- nm[grepl("^Idade", nm, ignore.case = TRUE)]
  cand <- cand[!grepl("c[oó]digo|code", cand, ignore.case = TRUE)]
  if (length(cand) == 0) stop("Não encontrei coluna de idade.")
  if (length(cand) > 1) {
    uc <- sapply(cand, function(x) length(unique(na.omit(df[[x]]))))
    cand <- cand[which.max(uc)]
  }
  cand
}

parse_idade <- function(x){
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[grepl("menos de 1", x, TRUE)]         <- 0
  out[grepl("\\b100\\b.*mais", x, TRUE)]    <- 100
  int <- is.na(out) & grepl("\\d+\\s*a\\s*\\d+", x)
  out[int] <- as.numeric(sub(" .*", "", x[int]))
  num <- is.na(out) & grepl("\\d+", x)
  out[num] <- as.numeric(str_extract(x[num], "\\d+"))
  out
}

collapse_buckets <- function(df){
  df %>%
    mutate(faixa = case_when(
      idade_num >= 0  & idade_num <= 14 ~ "pop_0a14",
      idade_num >= 15 & idade_num <= 64 ~ "pop_15_64",
      idade_num >= 65                   ~ "pop_65mais",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(faixa)) %>%
    group_by(cod_municipio, municipio, faixa) %>%
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = faixa, values_from = pop, values_fill = 0)
}

fetch_one_city <- function(table_id, year, cod_city){
  df <- sidrar::get_sidra(
    x          = table_id,
    variable   = 93,
    period     = as.character(year),
    geo        = "City",
    geo.filter = list(City = cod_city),
    format     = 3,
    header     = TRUE
  )
  
  # 9514 tem essa classificação
  if ("Forma de declaração da idade" %in% names(df)) {
    df <- dplyr::filter(df, `Forma de declaração da idade` == "Total")
  }
  # deixa só Sexo = Total, se existir
  if ("Sexo" %in% names(df)) {
    df <- dplyr::filter(df, Sexo == "Total")
  }
  
  # padroniza nome do município para 'municipio'
  mun_col <- intersect(names(df), c("Município","Municipio","Nome do Município"))
  if (length(mun_col) == 0) {
    mun_col <- names(df)[stringr::str_detect(names(df), "Munic")]
  }
  if (length(mun_col) == 0) stop("Coluna de município não encontrada.")
  df <- dplyr::mutate(df, municipio = .data[[ mun_col[1] ]])
  
  # acha a coluna de idade textual
  idade_col <- find_idade_col(df)
  
  df %>%
    dplyr::filter(
      !grepl("^Total$", .data[[idade_col]], ignore.case = TRUE),
      !grepl("ignorada|não declarad", .data[[idade_col]], ignore.case = TRUE)
    ) %>%
    dplyr::transmute(
      cod_municipio = as.integer(substr(as.character(`Município (Código)`), 1, 7)),
      municipio     = municipio,
      idade_num     = parse_idade(.data[[idade_col]]),
      pop           = as.numeric(Valor)
    ) %>%
    collapse_buckets() %>%
    dplyr::mutate(ano = year)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

prep_indic <- function(df){
  df %>%
    mutate(
      den  = pmax(pop_15_64, 1),
      OADR = 100 * pop_65mais / den,
      YDR  = 100 * pop_0a14  / den,
      TDR  = 100 * (pop_0a14 + pop_65mais) / den
    ) %>%
    select(cod_municipio, municipio, ano, OADR, YDR, TDR)
}

make_map <- function(gdf, var, title, limits, highlight = NULL){
  ggplot() +
    geom_sf(data = gdf, aes(fill = .data[[var]]), color = "white", size = 0.15) +
    { if (!is.null(highlight) && nrow(highlight) > 0)
      geom_sf(data = highlight, shape = 24, fill = "yellow", color = "black",
              size = 2.5, stroke = 0.3) else NULL } +
    scale_fill_viridis_c(option = "C", name = paste0(title, " (%)"),
                         limits = limits, labels = label_number(accuracy = 0.1)) +
    labs(title = title, subtitle = "Paraíba — municípios") +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = .5, face = "bold"),
      plot.subtitle = element_text(hjust = .5)
    )
}

# -----------------------------
# 1) Lista de municípios da PB
# -----------------------------
mun_pb <- geobr::read_municipality(code_muni = "PB", year = 2020, simplified = TRUE) %>%
  mutate(code_muni = as.integer(code_muni)) %>%
  select(code_muni, name_muni, geom)

pb_codes <- mun_pb$code_muni

# -----------------------------
# 2) Baixar 2010 e 2022 (em loop)
# -----------------------------
message("Baixando 2010 (1378), isso pode levar alguns minutos…")
pb2010 <- map_dfr(pb_codes, function(cod){
  # pequeno descanso para não sobrecarregar a API
  Sys.sleep(0.15)
  fetch_one_city(1378, 2010, cod)
})

message("Baixando 2022 (9514)…")
pb2022 <- map_dfr(pb_codes, function(cod){
  Sys.sleep(0.08)
  fetch_one_city(9514, 2022, cod)
})

# -----------------------------
# 3) Indicadores por ano e deltas
# -----------------------------
ind2010 <- prep_indic(pb2010)
ind2022 <- prep_indic(pb2022)

deltas <- ind2010 %>%
  select(cod_municipio, municipio, OADR_2010 = OADR, YDR_2010 = YDR) %>%
  inner_join(ind2022 %>% select(cod_municipio, OADR_2022 = OADR, YDR_2022 = YDR),
             by = "cod_municipio") %>%
  mutate(
    dOADR = OADR_2022 - OADR_2010,
    dYDR  = YDR_2022 - YDR_2010
  )

top10_codes <- deltas %>%
  arrange(desc(dOADR)) %>%
  slice_head(n = 10) %>%
  pull(cod_municipio)

# -----------------------------
# 4) Geometrias + centróides para destaque
# -----------------------------
centros_top10 <- mun_pb %>%
  filter(code_muni %in% top10_codes) %>%
  st_point_on_surface() %>%
  select(code_muni, geom)

map2010 <- mun_pb %>%
  left_join(ind2010, by = c("code_muni" = "cod_municipio"))

map2022 <- mun_pb %>%
  left_join(ind2022, by = c("code_muni" = "cod_municipio"))

lim_OADR <- range(c(map2010$OADR, map2022$OADR), na.rm = TRUE)
lim_YDR  <- range(c(map2010$YDR,  map2022$YDR),  na.rm = TRUE)

# -----------------------------
# 5) Mapas
# -----------------------------
m_OADR_2010 <- make_map(map2010, "OADR", "OADR 2010", lim_OADR, centros_top10)
m_OADR_2022 <- make_map(map2022, "OADR", "OADR 2022", lim_OADR, centros_top10)
m_YDR_2010  <- make_map(map2010, "YDR",  "YDR 2010",  lim_YDR,  centros_top10)
m_YDR_2022  <- make_map(map2022, "YDR",  "YDR 2022",  lim_YDR,  centros_top10)

painel <- (m_OADR_2010 + m_OADR_2022) / (m_YDR_2010 + m_YDR_2022) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Paraíba — Razões de Dependência (OADR/YDR), 2010 vs 2022",
    subtitle = "Triângulos amarelos: 10 municípios com maior aumento de OADR (2010→2022)",
    theme = theme(
      plot.title = element_text(hjust = .5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = .5, size = 11),
      legend.position = "bottom"
    )
  )

ggsave("mapas_dependencia_pb_2010_2022.png", painel, width = 14, height = 10, dpi = 300)

# (opcional) salva também os dados de variação
readr::write_csv(deltas, "variacao_OADR_YDR_2010_2022.csv")


# Inferência --------------------------------------------------------------


# pacotes
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

# ==== 1) Base 2022 com IE e RDT (assume que 'dados_completos' já existe) ====
df22 <- dados_completos %>%
  mutate(
    IE  = 100 * pop_65mais / pmax(pop_0a14, 1),                      # (65+) / (0–14)
    RDT = 100 * (pop_0a14 + pop_65mais) / pmax(pop_15_64, 1)         # (0–14 + 65+) / (15–64)
  ) %>%
  select(cod_municipio, municipio, IE, RDT, pop_0a14, pop_65mais, pop_15_64)

# ==== 2) Médias/razões estaduais (ponderadas pelos totais) ====
IE_PB  <- 100 * sum(df22$pop_65mais) / sum(df22$pop_0a14)
RDT_PB <- 100 * (sum(df22$pop_0a14) + sum(df22$pop_65mais)) / sum(df22$pop_15_64)

# ==== 3) Correlações ====
pear  <- cor.test(df22$IE, df22$RDT, method = "pearson")
spear <- cor.test(df22$IE, df22$RDT, method = "spearman")

cat(
  sprintf("r (Pearson)   = %.3f (p=%.4f)\n",  pear$estimate,  pear$p.value),
  sprintf("rho (Spearman)= %.3f (p=%.4f)\n", spear$estimate, spear$p.value)
)

# ==== 4) Quem rotular? (usa seu top10 se existir; senão calcula) ====
if (exists("top10_dependencia")) {
  rotulos <- df22 %>% semi_join(top10_dependencia, by = c("cod_municipio" = "cod_municipio"))
} else {
  rotulos <- df22 %>% slice_max(RDT, n = 10)
}

# (cores por nível de RDT — mesmo esquema dos seus mapas)
df22 <- df22 %>%
  mutate(
    classe_rdt = cut(
      RDT,
      breaks = c(-Inf, 40, 50, 60, Inf),
      labels = c("Favorável (<40)", "Moderado (40–50)", "Alerta (50–60)", "Crítico (≥60)")
    )
  )

pal <- c(
  "Favorável (<40)" = "#6CC3B2",
  "Moderado (40–50)"= "#A8E6A3",
  "Alerta (50–60)"  = "#F39C12",
  "Crítico (≥60)"   = "#E74C3C"
)

# ==== 5) Scatter ====
p_scatter <- ggplot(df22, aes(x = IE, y = RDT)) +
  # quadrantes (opcional: leve sombreado)
  annotate("rect", xmin = -Inf, xmax = IE_PB, ymin = -Inf, ymax = RDT_PB, alpha=.03, fill="grey50") +
  annotate("rect", xmin = IE_PB, xmax =  Inf, ymin = -Inf, ymax = RDT_PB, alpha=.03, fill="grey50") +
  annotate("rect", xmin = -Inf, xmax = IE_PB, ymin = RDT_PB, ymax =  Inf, alpha=.03, fill="grey50") +
  annotate("rect", xmin = IE_PB, xmax =  Inf, ymin = RDT_PB, ymax =  Inf, alpha=.03, fill="grey50") +
  geom_point(aes(color = classe_rdt), alpha = .85, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = .6) +
  geom_vline(xintercept = IE_PB, linetype = "dashed", linewidth = .4, color = "grey30") +
  geom_hline(yintercept = RDT_PB, linetype = "dashed", linewidth = .4, color = "grey30") +
  geom_label_repel(
    data = rotulos,
    aes(label = municipio),
    size = 2.8, label.size = 0.15, max.overlaps = Inf, seed = 123,
    box.padding = .35, point.padding = .3
  ) +
  scale_color_manual(values = pal, name = "Nível de Dependência (RDT)") +
  scale_x_continuous(name = "Índice de Envelhecimento (IE) — 65+/0–14 × 100",
                     labels = label_number(accuracy = 1)) +
  scale_y_continuous(name = "Razão de Dependência Total (RDT) — (0–14 + 65+)/15–64 × 100",
                     labels = label_number(accuracy = 1)) +
  labs(
    title = "Relação entre Envelhecimento (IE) e Dependência (RDT) — Municípios da Paraíba (2022)",
    subtitle = paste0(
      "Correlação fraca: r(Pearson) = ", number(pear$estimate, accuracy = 0.01),
      " | ρ(Spearman) = ", number(spear$estimate, accuracy = 0.01),
      "  — linhas tracejadas: valores estaduais"
    ),
    caption = "Fonte: IBGE/SIDRA 9514 (2022). Elaboração própria."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

p_scatter
ggsave("Figuras/scatter_IE_vs_RDT_2022.png", p_scatter, width = 11, height = 7, dpi = 300)


# RGI PB ------------------------------------------------------------------


# ------------------------------------------------------------
# Dependência demográfica (2022) — PB
# OADR, YDR, mapas lado a lado e Top 10
# ------------------------------------------------------------

library(sidrar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geobr)
library(sf)
library(patchwork)

# 1) Função para baixar e consolidar por faixas (SIDRA 9514, UF=25 PB)
get_population_data <- function(categories, pop_name) {
  get_sidra(
    x = 9514,
    variable = 93,
    period = "2022",
    geo = "City",
    geo.filter = list(State = 25),
    classific = "c287",
    category = list(c287 = categories),
    format = 3,
    header = TRUE
  ) |>
    group_by(`Município (Código)`, Município) |>
    summarise("{pop_name}" := sum(Valor, na.rm = TRUE), .groups = "drop") |>
    transmute(
      cod_municipio = as.integer(substr(as.character(`Município (Código)`), 1, 7)), # 7 dígitos
      municipio     = Município,
      "{pop_name}"  := .data[[pop_name]]
    )
}

# 2) Categorias das idades (c287) que você já usa
categorias_jovens <- c(93070, 93084, 93085)                             # 0–14
categorias_idosos <- c(93096, 93097, 93098, 49108, 49109, 60040, 60041, 6653) # 65+
categorias_ativa  <- c(93086:93095)                                      # 15–64

# 3) Baixar blocos
dados_jovens <- get_population_data(categorias_jovens, "pop_0a14")
dados_idosos <- get_population_data(categorias_idosos, "pop_65mais")
dados_ativa  <- get_population_data(categorias_ativa,  "pop_15_64")

# 4) Consolidar e calcular indicadores  — JUNTA SÓ POR COD_MUNICIPIO
dados_completos <- dados_jovens |>
  select(cod_municipio, municipio, pop_0a14) |>
  inner_join(select(dados_idosos, cod_municipio, pop_65mais),
             by = "cod_municipio") |>
  inner_join(select(dados_ativa,  cod_municipio, pop_15_64),
             by = "cod_municipio") |>
  # se vierem dois nomes de município, mantém o do 1º dataset
  relocate(municipio, .after = cod_municipio) |>
  distinct(cod_municipio, .keep_all = TRUE) |>
  mutate(
    den  = pmax(pop_15_64, 1),          # blindagem contra divisão por zero
    OADR = 100 * pop_65mais / den,      # 65+ / 15–64
    YDR  = 100 * pop_0a14  / den,       # 0–14 / 15–64
    TDR  = 100 * (pop_0a14 + pop_65mais) / den,
    class_OADR = cut(
      OADR, breaks = c(0, 15, 25, 35, Inf),
      labels = c("Muito Baixa (<15)", "Baixa (15-25)", "Moderada (25-35)", "Alta (>35)")
    ),
    class_YDR = cut(
      YDR, breaks = c(0, 30, 45, 60, Inf),
      labels = c("Muito Baixa (<30)", "Baixa (30-45)", "Moderada (45-60)", "Alta (>60)")
    )
  ) |>
  select(-den)

# 5) Malha municipal (7 dígitos) e join
pb_municipios <- read_municipality(code_muni = "PB", year = 2022) |>
  mutate(cod_municipio = as.integer(code_muni)) |>
  st_simplify(dTolerance = 0.01)

dados_mapa <- pb_municipios |>
  left_join(dados_completos, by = "cod_municipio")

# (diagnóstico rápido — opcional)
# anti_join(dados_completos, pb_municipios, by = "cod_municipio") |> nrow()
# anti_join(pb_municipios, dados_completos, by = "cod_municipio") |> nrow()

# 6) Mapas
mapa_OADR <- ggplot() +
  geom_sf(data = dados_mapa, aes(fill = class_OADR), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "Reds", name = "OADR (%)") +
  labs(title = "Razão de Dependência de Idosos (OADR)",
       subtitle = "População 65+ / População 15–64 anos") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

mapa_YDR <- ggplot() +
  geom_sf(data = dados_mapa, aes(fill = class_YDR), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "Blues", name = "YDR (%)") +
  labs(title = "Razão de Dependência de Jovens (YDR)",
       subtitle = "População 0–14 / População 15–64 anos") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

mapas_combinados <- mapa_OADR + mapa_YDR +
  plot_annotation(
    title = "Indicadores de Dependência Demográfica — Paraíba (2022)",
    subtitle = "Comparação entre Dependência de Idosos (OADR) e Jovens (YDR)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 12))
  )

ggsave("mapas_dependencia_pb_2022.png", mapas_combinados, width = 16, height = 9, dpi = 300)
print(mapas_combinados)

# 7) Top 10 (tabelas)
top_OADR <- dados_completos |>
  arrange(desc(OADR)) |>
  slice_head(n = 10) |>
  select(municipio, OADR, class_OADR)

top_YDR <- dados_completos |>
  arrange(desc(YDR)) |>
  slice_head(n = 10) |>
  select(municipio, YDR, class_YDR)

print(list(OADR_top = top_OADR, YDR_top = top_YDR))

# (opcional) exporta CSVs para conferência
# write.csv(dados_completos, "dependencia_pb_2022.csv", row.names = FALSE)
# write.csv(top_OADR, "top10_OADR_2022.csv", row.names = FALSE)
# write.csv(top_YDR, "top10_YDR_2022.csv", row.names = FALSE)


# Myers / Municípios ------------------------------------------------------

# instalar/usar geobr e sf
# install.packages("geobr")
library(sidrar)
library(dplyr)
library(purrr)
library(stringr)
library(geobr)
library(sf)
library(lwgeom)
library(purrr)
library(ggplot2)
library(DescTools)

# Divisão Regional PNG ----------------------------------------------------

# 1. Baixar as regiões intermediárias da Paraíba
reg_int <- geobr::read_intermediate_region(
  code_intermediate = "PB",
  year              = 2019,
  simplified        = TRUE,
  cache             = TRUE
)

# 2. Criar o mapa: só linhas, sem preenchimento
mapa_pb <- ggplot(reg_int) +
  geom_sf(color = "black", fill = NA, size = 0.6) +
  theme_void() +
  ggtitle("Divisão Regional da Paraíba") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# 3. Exportar como PNG
ggsave("paraiba_divisao_regional.png", plot = mapa_pb, width = 6, height = 6, dpi = 300)


# Filtrar Regiões~Municípios  ---------------------------------------------

# 1) Baixa todos os municípios do Brasil em 2010
all_mun <- read_municipality(
  code_muni      = "all",
  year           = 2010,
  simplified     = TRUE,
  cache          = TRUE,
  showProgress   = FALSE,
  keep_areas_operacionais = FALSE
)

# 2) Filtra só Paraíba (código IBGE começa em "25") e solta a geometria
mun_pb_sf <- all_mun[substr(all_mun$code_muni, 1, 2) == "25", ]
mun_pb_df <- st_drop_geometry(mun_pb_sf)

# 3) Seleciona e renomeia colunas com base R (sem dplyr::select)
municipios_pb <- mun_pb_df[
  ,
  c("code_muni", "name_muni"),
  drop = FALSE
]
names(municipios_pb) <- c("cod_mun", "nome_mun")

# 4) Ordena pelo nome do município
municipios_pb <- municipios_pb[order(municipios_pb$nome_mun), ]

# Verifica
nrow(municipios_pb)     # 223
head(municipios_pb, 10) # primeiros 10 municípios


# rodar sidrar ------------------------------------------------------------

# 1. Parâmetros
table_2022  <- 9514
vars_2022   <- 93
period_2022 <- "2022"

# 2. Função para baixar e filtrar pra um município
baixar_mun_2022 <- function(cod_mun) {
  get_sidra(
    x          = table_2022,
    variable   = vars_2022,
    period     = period_2022,
    geo        = "City",
    geo.filter = list(City = cod_mun)
    # aqui NÃO declaramos classific
  ) %>%
    filter(
      Sexo                          == "Total",
      `Forma de declaração da idade` == "Total"
    ) %>%
    transmute(
      cod_mun    = `Município (Código)`,
      idade      = as.numeric(str_extract(Idade, "^[0-9]+")),
      populacao  = as.numeric(Valor)
    )
}

# 3. Loop em todos os 223 municípios
dados_2022 <- municipios_pb$cod_mun %>%
  map_dfr(baixar_mun_2022)

# Transformar as colunas em mesmo tipo
dados_2022 <- dados_2022 %>%
  mutate(cod_mun = as.numeric(cod_mun))

# 4. Juntar nome e ordenar
final_2022 <- dados_2022 %>%
  dplyr::left_join(municipios_pb, by = "cod_mun") %>%
  dplyr::select(nome_mun, idade, populacao) %>%
  dplyr::arrange(nome_mun, idade)

dim(final_2022)  # ~22 300 linhas
head(final_2022, 10)

# Agregando os dados
dados_brutos <- municipios_pb$cod_mun %>%
  map_dfr(baixar_mun_2022)

dados_unicos <- dados_brutos %>%
  group_by(cod_mun, idade) %>%
  summarise(
    populacao = sum(populacao, na.rm = T),
    .groups = "drop"
  )


# RIs ------------------------------------------------------

# 0) Carregar pacotes
#geobr      read_intermediate_region, read_municipality
#sf         st_transform, st_intersection, st_area, st_buffer
#dplyr      select, mutate, group_by, filter, slice, rename, case_when
#lwgeom     st_make_valid

# 0) Opcional: desabilita o uso de S2 (usa GEOS, mais tolerante a geometrias imperfeitas)
sf::sf_use_s2(FALSE)

# 1) Baixar os shapefiles sem simplificação e validar geometrias
muni_pb <- geobr::read_municipality(
  code_muni  = "PB", 
  year       = 2019, 
  simplified = FALSE, 
  cache      = TRUE
) %>% 
  sf::st_make_valid()

reg_int <- geobr::read_intermediate_region(
  code_intermediate = "PB", 
  year              = 2019, 
  simplified        = FALSE, 
  cache             = TRUE
) %>% 
  sf::st_make_valid()

# 2) Harmonizar CRS de ambos
reg_int <- sf::st_transform(reg_int, sf::st_crs(muni_pb))

# 3) Preparar objetos só com código + geometria
#    usando colchetes para manter a coluna geom implícita
muni_codesf <- muni_pb["code_muni"]
reg_codesf  <- reg_int["code_intermediate"]

# 4) Calcular interseções geométricas
ints <- sf::st_intersection(muni_codesf, reg_codesf)

# 5) Calcular área de cada pedaço de interseção
ints$area <- sf::st_area(ints)

# 6) Para cada município, escolher a interseção de maior área
main_reg <- ints %>%
  dplyr::group_by(code_muni) %>%
  dplyr::slice_max(area, n = 1) %>%   # pega o polígono com maior área
  dplyr::ungroup()

# 7) Recodificar e montar o data.frame final
df <- main_reg %>%
  dplyr::rename(cod_mun = code_muni) %>%
  dplyr::mutate(
    regiao = dplyr::case_when(
      code_intermediate == 2501 ~ 1,
      code_intermediate == 2502 ~ 2,
      code_intermediate == 2503 ~ 3,
      code_intermediate == 2504 ~ 4,
      TRUE                     ~ NA_integer_
    )
  ) %>%
  dplyr::select(cod_mun, regiao)

# 8) Conferir
print(dim(df))      # deve ser 223  2
print(head(df, 10))

# Remover Geometrias
muni_pb_region <- muni_pb %>%
  rename(cod_mun = code_muni) %>%
  left_join(
    df %>% st_drop_geometry(),   # tira a coluna geom de df
    by = "cod_mun"
  )


lookup <- df %>%
  st_drop_geometry()

# 9) Adicionar Região de Volta a Municípios
muni_pb_region <- muni_pb %>%
  dplyr::rename(cod_mun = code_muni) %>%
  dplyr::left_join(lookup, by = "cod_mun")

# Conferindo
dim(muni_pb_region)    # 223 municípios + colunas extras
names(muni_pb_region)  # veja cod_mun, nome, regiao, etc.

# 10) Agregar lookup em dados_unicos
# Junta ao seu dados_unicos
dados_regiao <- dados_unicos %>%
  # garante mesmo tipo de cod_mun
  mutate(cod_mun = as.numeric(cod_mun)) %>%
  left_join(lookup, by = "cod_mun") %>%
  # opcional: eliminar eventuais municípios sem região
  filter(!is.na(regiao))

# Verifique
dim(dados_regiao)       # deve ter mesmo número de linhas que dados_unicos (menos os NAs)
head(dados_regiao)

# Myers 2022 --------------------------------------------------------------


# 1) Função para calcular o Índice de Myers
compute_myers <- function(idade, pop, min_age = 23, max_age = 62) {
  # Constrói um data.frame temporário e filtra as idades de interesse
  df <- data.frame(idade = idade, pop = pop)
  df <- df[df$idade >= min_age & df$idade <= max_age, ]
  # Extrai o dígito terminal de cada idade
  df$digito <- df$idade %% 10
  # Soma a população por dígito
  pops <- tapply(df$pop, df$digito, sum, default = 0)
  # Garante que tenhamos todos os dígitos 0:9
  pops <- pops[as.character(0:9)]
  pops[is.na(pops)] <- 0
  total <- sum(pops)
  if(total == 0) return(NA_real_)
  # Proporção de cada dígito
  p <- pops / total
  # Desvios em relação a 0.10
  desv <- abs(p - 0.1)
  # Índice de Myers (metade da soma dos desvios, em %)
  myers <- sum(desv) / 2 * 100
  return(myers)
}

# 2) Aplica a todos os 223 municípios
library(dplyr)

myers_por_mun <- dados_unicos %>%
  group_by(cod_mun) %>%
  summarise(
    Myers = compute_myers(idade, populacao),
    .groups = "drop"
  ) %>%
  mutate(cod_mun = as.numeric(cod_mun)) %>%
  left_join(municipios_pb, by = "cod_mun") %>%
  select(cod_mun, nome_mun, Myers)

myers_por_mun <- dados_unicos %>%
  group_by(cod_mun) %>%
  summarise(
    Myers = compute_myers(idade, populacao),
    .groups = "drop"
  ) %>%
  # opcional: junta o nome do município
  left_join(municipios_pb, by = c("cod_mun")) %>%
  select(cod_mun, nome_mun, Myers)

# 3) Veja o resultado
print(myers_por_mun)

# 4) (Opcional) Exportar
# write_csv(myers_por_mun, "myers_index_por_municipio.csv")

