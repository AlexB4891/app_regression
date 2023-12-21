
# ------------------------------------------------------------------------- #
#   Indirect and direct participation concentration in foreign residences   #
# ------------------------------------------------------------------------- #

library(tidyverse)
library(lubridate)
library(readxl)
library(furrr)


participacion <- read_rds("01_DATA/APS/aps_unicos_lista.rds")


grandes <- participacion %>% 
  mutate(filas = map_dbl(data,nrow)) %>% 
  filter(filas > 20)

grandes <- grandes %>% 
  mutate(data = map(data,
                    ~.x %>% mutate(codigo_pais_accionista = as.character(codigo_pais_accionista)))) %>% 
  unnest(data)


accionistas_pff <- grandes %>% 
  distinct(id_accionista_anon,paraiso_fiscal_accionista)

sociedades_pff <- grandes %>% 
  distinct(id_sociedad_anon)

sociedades_class <- sociedades_pff %>% 
  left_join(accionistas_pff, 
            by = c("id_sociedad_anon" = "id_accionista_anon"))


cambiantes_nivel <- sociedades_class %>% 
  group_by(id_sociedad_anon) %>% 
  summarise(n = n_distinct(paraiso_fiscal_accionista)) %>% 
  filter(n > 1)







