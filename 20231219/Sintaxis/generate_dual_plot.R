
# Assets and profits plots comparision ------------------------------------

generate_plot <- function(table,
                          filter){
  
  df_a <- table %>% 
    filter(group_assign %in% c("T-Maj","T-Min","C-Maj","C-Min")) %>% 
    mutate(population = "Core Sample")
  
  if(filter == "a"){
    df_b <- table %>% 
      filter(!is.na(treatment_a)) %>% 
      mutate(population = "Alternative A")
    
  }else if(filter == "b"){
    df_b <- table %>% 
      filter(!is.na(treatment_b)) %>% 
      mutate(population = "Alternative B")
    
  }else if(filter == "c"){
    df_b <- table %>% 
      filter(!is.na(treatment_c)) %>% 
      mutate(population = "Alternative C")
    
  }
  
  df_plot <- list(
    df_a,
    df_b
  ) %>% 
    map(select,
        identificacion_informante_anon,
        group_assign,
        `Total assets` = total_assets,
        `Net profits` = net_profits, 
        population
    ) %>% 
    reduce(bind_rows) %>% 
    pivot_longer(cols = c(`Total assets` ,
                          `Net profits`),
                 names_to = "variable",
                 values_to = "value") %>% 
    mutate(value = log(value))
  
  df_plot %>% 
    ggplot() +
    geom_density(aes(x = value, fill = population), position = "identity", alpha = 0.6) +
    facet_grid(group_assign ~ variable) + 
    theme_light() + 
    theme(legend.position = "bottom") +
    labs(fill = "Population",
         x = "",
         y = "Density")
}

# generate_plot(table = firm_table,"c")

cit_plot <- function(table, filter){

  
  df_a <- table %>% 
  filter(group_assign %in% c("T-Maj","T-Min","C-Maj","C-Min")) %>% 
  mutate(population = "Core Sample")

if(filter == "a"){
  df_b <- table %>% 
    filter(!is.na(treatment_a)) %>% 
    mutate(population = "Alternative A")
  
}else if(filter == "b"){
  df_b <- table %>% 
    filter(!is.na(treatment_b)) %>% 
    mutate(population = "Alternative B")
  
}else if(filter == "c"){
  df_b <- table %>% 
    filter(!is.na(treatment_c)) %>% 
    mutate(population = "Alternative C")
  
}
  
  df_plot <- list(
    df_a,
    df_b
  ) %>% 
    map(select,
        anio_fiscal,
        identificacion_informante_anon,
        group_assign,
        tasa_ir,
        population
    ) %>% 
    reduce(bind_rows) 
  
  df_plot_long <- df_plot %>% 
    mutate(status = "All firms") %>% 
    bind_rows(df_plot %>% 
                filter(tasa_ir >= 0.22) %>% 
                mutate(status = "CIT > 0") )
    
  
cit_summ <- df_plot_long %>% 
  group_by(group_assign,population,anio_fiscal,status) %>% 
  summarise(mean = mean(tasa_ir,na.rm = T),
            median = median(tasa_ir,na.rm = T)) %>% 
  pivot_longer(cols = c(mean, median),
               names_to = "stat",
               values_to = "value")

cit_summ %>% 
  ggplot(aes(x = anio_fiscal,
             color = population,
             linetype = stat,
             y = value,
             shape = stat)) +
  geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = 2014.5), linetype = "dashed") +
  facet_grid(group_assign ~ status, scales = "free") + 
  theme_light() + 
  theme(legend.position = "bottom") +
  labs(color = "Population",
       x = "Fiscal_year",
       y = "Density")
}

# cit_plot(table = firm_table,"a")
