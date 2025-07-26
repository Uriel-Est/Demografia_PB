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