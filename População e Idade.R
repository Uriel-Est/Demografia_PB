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


# Índice de Envelhecimento ------------------------------------------------


library(sidrar)
library(tidyverse)
library(cowplot)

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
    caption = "Fonte: IBGE | Elaboração: Análise Demográfica",
    color = "Classificação:"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_cartesian(
    xlim = c(0, max(dados_finais$indice_envelhecimento, na.rm = TRUE) * 1.2),
    ylim = c(0, max(dados_finais$indice_60mais, na.rm = TRUE) * 1.2)
  )

library(geobr)
library(ggplot2)
library(gghighlight)
library(ggspatial)
library(sf)

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

library(ggspatial)
library(sf)
library(cowplot)
library(gridExtra)
library(grid)
library(dplyr)

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
    "Fonte: IBGE Censo 2022 (Tabela 9514) | Elaboração: Análise Demográfica",
    x = 0.95, y = 0.03, size = 10, hjust = 1, color = "#7f8c8d"
  )

# Salvar com alta resolução
ggsave("cartograma_final_pb.png", plot_final, width = 16, height = 14, dpi = 300, bg = "white")


# Índice de Dependência simples -------------------------------------------


