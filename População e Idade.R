library(sidrar)
library(PNADcIBGE)
library(survey)
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(tidyverse)

pb_map <- read_municipality(
  code_muni = "PB",
  year = 2010,
  showProgress = F
)

ggplot() +
  geom_sf(data = pb_map, fill="#2D3E50", color = "#FEBF57", size = .15, show.legend = F) +
  labs(subtitle = "Municípios Paraibanos, 2010", size = 8) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
  

info_sidra(200)
info_sidra(1209)
info_sidra(9514)

# Censo 2022 UF-25
sexo_22 <- get_sidra(
  x = 9514,
  period = "2022",
  geo = "State",
  geo.filter = list("State" = 25),
  classific = "all",
  category = "all"
)

sexo_22 <- sexo_22 %>%
  select(sexo = `Sexo`, idade = `Idade`, valor = Valor) %>%
  filter(!is.na(sexo), !is.na(idade))

pop_razao <- sexo_22 %>%
  filter(sexo %in% c("Homens", "Mulheres"), idade != "Total") %>%
  group_by(sexo) %>%
  summarise(populacao = sum(valor)) %>%
  pivot_wider(names_from = sexo, values_from = populacao) %>%
  mutate(razao_sexo = (Homens / Mulheres) * 100)

print(pop_razao)

sexo_10 <- get_sidra(
  x = 200,
  period = "2010",
  geo = "State",
  geo.filter = list("State" = 25),
  classific = "all",
  category = "all"
)

sexo_10 <- sexo_10 %>%
  select(sexo = `Sexo`, idade = `Grupo de idade`, valor = Valor) %>%
  filter(!is.na(sexo), !is.na(idade))

sexo_10 <- sexo_10 %>%
  mutate(valor = as.numeric(valor))

pop_razao10 <- sexo_10 %>%
  filter(sexo %in% c("Homens", "Mulheres"), idade != "Total") %>%
  group_by(sexo) %>%
  summarise(populacao = sum(valor), .groups = "drop") %>%
  pivot_wider(names_from = sexo, values_from = populacao) %>%
  mutate(razao_sexo = (Homens / Mulheres) * 100)

print(pop_razao10)