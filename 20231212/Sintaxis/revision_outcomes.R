
library(tidyverse)
library(readxl)
library(janitor)
library(here)


firm_table <- read_rds(here("01_DATA/APS/core_sample_assigned.rds"))

# 27112023: Revisiòn de la calidad de las variables de relaciòn con el MID ----------


firm_table %>% 
  filter(anio_fiscal == 2014) %>% 
  group_by(group_assign) %>% 
  summarise(
    across(matches("pff_[0-9]+|ext_[0-9]+"),
    list(mean = ~mean(.x,na.rm = T),
         sd = ~sd(.x,na.rm = T),
         median = ~median(.x,na.rm = T),
         q25 = ~quantile(.x,0.25,na.rm = T),
         q75 = ~quantile(.x,0.25,na.rm = T),
         ceros = ~sum(.x == 0,na.rm = T),
         posi = ~sum(.x>0,na.rm = T)),
    .names = "{.fn}__{.col}")
    
  ) %>% 
  pivot_longer(-group_assign) %>% 
  separate(name,sep = "__",into = c("stat","var")) %>% 
  pivot_wider(names_from = stat,values_from = value) %>% View


core_sample <- particip_df %>% distinct(anio_fiscal,identificacion_informante_anon)

mid_invest %>% 
  map("result") %>% 
  map(inner_join,core_sample) %>% 
  map(ungroup) %>% 
  map(group_by, motivo_factor,identificacion_informante_anon,anio_fiscal ) %>% 
  map(summarise, monto = sum(monto,na.rm = T)) %>% 
  map(~{
    
    ass <- firm_table %>% 
      select(identificacion_informante_anon,anio_fiscal,group_assign)
    
    inner_join(.x,ass) %>% 
      ungroup %>% 
      summarise(n_distinct(identificacion_informante_anon))
  })


foreign_relationship <- c(
  "ext_650", 
  "pff_650",
  "pff_645",
  "ext_645",
  "ext_635",
  "pff_635",
  "ext_630",
  "pff_630",
  "ext_625",
  "pff_625",
  "ext_615",
  "pff_615",
  "pff_640",
  "ext_640",
  "pff_405",
  "ext_405",
  "ext_415",
  "pff_415",
  "pff_720",
  "ext_720",
  "ext_710",
  "pff_710",
  "ext_410",
  "pff_410"
) 

dicci <- read_excel("database_dictionary.xlsx", skip = 8) %>% 
  clean_names() %>% 
  filter(name_in_the_database %in% str_remove(foreign_relationship,"_")) %>% 
  select(name_in_the_database,label)


check_empty <- function(vari){

  
  firm_table %>% 
    filter(group_assign %in% c("T-Min","T-Maj","C-Min","C-Maj")) %>% 
    mutate(across(one_of(vari),
                  list(status = ~ case_when(
                    .x  > 0 ~ "Positive",
                    .x == 0 ~ "Zero",
                    is.na(.x) ~ "Empty"
                  )), .names = "{.fn}")) %>% 
    count(across(c(anio_fiscal,group_assign,status))) %>% 
    mutate(name_in_the_database = str_remove(vari,"_")) %>% 
    left_join(dicci)
  

}



check_population <- map_dfr(foreign_relationship,check_empty)

check_population <- check_population %>% 
  mutate(label = str_c(label," (",str_extract(name_in_the_database,"[:digit:]+"),")"))

check_population %>% 
  ggplot() + 
  geom_col(aes(x = label, fill = status, y = n), position = "dodge") +
  facet_wrap(.~group_assign, scales = "free") +
  coord_flip() 



# 01122023: Revisiòn de las tasas de impuesto a la renta ------------------


# Con esta especificacion, tenemo los mismos resultados anteriores
# y se ve mejor los pretrends:

cit_summ <- firm_table %>% 
  filter(tasa_ir >= 0.22) %>%
  filter(net_profits > 0) %>% 
  group_by(anio_fiscal,group_assign) %>% 
  summarise(mean = mean(tasa_ir,na.rm = T),
            median = median(tasa_ir,na.rm = T)) %>% 
  pivot_longer(cols = c(mean, median),
               names_to = "stat",
               values_to = "value")


firm_table %>% 
  filter(group_assign %in% c("T-Min","C-Min","T-Maj","C-Maj")) %>% 
  group_by(anio_fiscal,group_assign) %>% 
  summarise(mean = mean(tasa_ir,na.rm = T),
            median = median(tasa_ir,na.rm = T),
            suma = sum(tasa_ir >= 0.22,na.rm = T),
            n = n()) %>% 
  mutate(proportion = suma/n) %>% 
  select(anio_fiscal,group_assign,proportion) %>% 
  pivot_wider(names_from = anio_fiscal,values_from = proportion)
  
  
cit_summ %>% 
  filter(group_assign %in% c("T-Min","C-Min","T-Maj","C-Maj")) %>% 
  ggplot(aes(x = anio_fiscal,
             color = group_assign,
             y = value)) +
  geom_line() +
  geom_point() +
  facet_grid(group_assign~stat, scales = "free")



  

