library(tidyverse)
library(fixest)
library(plm)
library(broom)
library(scales)
library(here)
library(lmtest)
library(sandwich)


# Functions ---------------------------------------------------------------

coef_plot_plus <- function(tidy_tems,
                           title_plot,
                           subtitle_plot){
  
  tidy_tems %>% 
    mutate(term = str_remove_all(term,"^treatment|^post"),
           term = str_replace(term,":post",":")) %>% 
    ggplot() +
    geom_point(aes(x = term, y = estimate)) +
    geom_text(aes(x = term, 
                  y = estimate,
                  label = round(estimate, 3)),
              vjust = -0.5,
              color = "#841CD4") +
    geom_linerange(aes(x = term,
                       ymin = conf.low, 
                       ymax = conf.high ),
                   color = "#841CD4") +
    # coord_flip() +
    theme_minimal() +
    theme(axis.title= element_blank()) +
    labs(title = title_plot,
         subtitle = subtitle_plot)

}

# Data --------------------------------------------------------------------

firm_table <- read_rds(here("01_DATA/APS/core_sample_assigned.rds"))

firm_table <- firm_table %>% 
  select(anio_fiscal,
         identificacion_informante_anon,
         group_assign,
         porcentaje_aps_pff,
         porcentaje_aps_ext,
         porcentaje_aps_nac,
         total_assets,
         tax_liability,
         net_profits,
         taxable_profits) %>% 
  mutate(across(c(porcentaje_aps_pff,porcentaje_aps_ext,porcentaje_aps_nac),
                ~.x/100)) 

reg_table <- firm_table %>% 
  filter(group_assign %in% c("C-Min","C-Maj","T-Min","T-Maj")) %>% 
  mutate(
    post = as.numeric(anio_fiscal >= 2015),
    post = factor(post,levels = c(0,1), labels = c("Pre","Post")),
    treatment = as_factor(group_assign),
    treatment = fct_collapse(treatment,
                             Control = c("C-Min","C-Maj"),
                             Treatment = c("T-Min","T-Maj")),
    treatment = fct_relevel(treatment,c("Control","Treatment")),
    prominent = if_else(
      treatment == "Control", porcentaje_aps_ext,porcentaje_aps_pff
    )
  ) %>% 
  select(
    pff_p = porcentaje_aps_pff,
    ext_p = porcentaje_aps_ext,
    nac_p = porcentaje_aps_nac,
    post,
    treatment,
    anio_fiscal,
    firm_id = identificacion_informante_anon,
    prominent,
    group_assign,
    total_assets,
    tax_liability,
    net_profits,
    taxable_profits
  )


# AQUI --------------------------------------------------------------------


# Logs de variables

reg_table <- reg_table %>% 
  mutate(
    log_assets_attr_pff = log(total_assets*pff_p),
    log_assets_attr_ext = log(total_assets*ext_p),
    log_utility = log(net_profits),
    log_cit_liability = log(tax_liability),
    log_taxable_profits = log(taxable_profits),
    log_assets_prominent = log(prominent*total_assets)
  )


# Relative change  

reg_2014 <- reg_table %>% 
  filter(anio_fiscal == 2014) %>% 
  select(firm_id,
         pff_2014 = pff_p,
         prom_2014 = prominent)

reg_table <-reg_table %>% 
  left_join(reg_2014) %>% 
  mutate(
    pff_rel = if_else(pff_2014 == 0 | is.na(pff_2014),
                      true = NA_real_, 
                      false = pff_p/pff_2014),
    prom_rel = if_else(prom_2014 == 0 | is.na(prom_2014),
                      true = NA_real_, 
                      false = prominent/prom_2014)
    )


perform_model <- function(model,
                           label,
                           rtable,
                          dependent){
  
  # browser()
  
  if(label %in% c( "Linear model with fixed effect",
                   "Firm clustered model with time",
                   "Event study saturated with fixed effects")){
    
    vcov_cl <- vcovHC(x = model,
                      type = "HC1", 
                      cluster = "group")
    
  }else{
    vcov_cl <- vcovCL(x = model,
                      cluster = rtable$firm_id)
  }
  

  
  model_corrected <- coeftest(vcov. = vcov_cl,
                              x = model) 
  
  conf_int <- tidy(model_corrected,
                   conf.int = TRUE)
  
  perfor <- glance(model)
  
  output <- list(
    model = model_corrected,
    params = conf_int,
    performance = perfor
  ) %>% 
    map_at(.at = c("params","performance"),mutate,model = label)
  
  return(output)
}




# browser()



diff_diff_model <- function(rtable,dependent){
  
  # Prepare data:
  
  # 1. Linear model ---------------------------------------------------------
  
  fml <- glue::glue('{dependent} ~ treatment + post + treatment:post')
  
  
  model <- lm(data = rtable,
              formula = fml)


# 2. With fixed effects ---------------------------------------------------
  
  model_fe <- plm(data = rtable,
               formula = fml,
               index = c("firm_id","anio_fiscal"),
               model = "within")
  

  rtable_time <- rtable %>%
    mutate(anio_fiscal = factor(anio_fiscal),
           anio_refact = relevel(anio_fiscal,
                                 ref = "2014"),
           treatment_num = as.numeric(treatment) - 1)
  
  # 3. Paneled with time ---------------------------------------------------
  
  fml_time <- glue::glue('{dependent} ~ treatment + anio_refact + i(anio_fiscal,treatment_num,ref = 2014)') %>% 
    formula(.)
  # browser()
  
  model_time <- feols(data = rtable_time,fml = fml_time)
  

# 4. Linear with time --------------------------------------------------------
  
  fml_time_lm <- glue::glue('{dependent} ~ treatment + anio_refact +treatment:anio_refact') %>% 
    formula(.)
  # browser()
  
  model_time_lm <- lm(data = rtable_time,formula = fml_time_lm)
  

# 5. Paneled with time and fixed effects ----------------------------------

  
  fml_time_plm <- glue::glue('{dependent} ~ treatment + anio_refact +treatment:anio_refact') %>% 
    formula(.)
  # browser()
  
  model_time_plm <- plm(data = rtable_time,formula = fml_time_plm,
                       index = c("firm_id","anio_fiscal"), model = "within")
  

  # 6. Fixed effects time ----------------------------------
  
  
  fml_fe_time <- glue::glue('{dependent} ~ i(anio_fiscal,treatment_num,ref = 2014) | treatment + anio_fiscal') %>% 
    formula(.)
  # browser()
  
  model_fe_time <- feols(data = rtable_time,fml = fml_fe_time,
                        cluster = rtable_time$firm_id)
  

# 7. Event study -------------------------------------------------------------

  
  es_fml <- glue::glue('{dependent} ~ anio_refact') %>% 
    formula(.)
  # browser()
  
  model_es <- lm(data = rtable_time,
                      formula = es_fml)
  

# 8. Event study with fixed effects ------------------------------------------
  

  # browser()
  
  model_es_fe <- plm(data = rtable_time,
                     formula = es_fml,
                     index = c("firm_id","anio_fiscal"), 
                     model = "within")
  
  
  output <- list(
      "Linear model" =   model,
      "Linear model with fixed effect" = model_fe,
      "Linear model with time and fixed effects" = model_time ,
      "Linear model with time" = model_time_lm,
      "Firm clustered model with time" = model_time_plm,
      "Firm clustered model with time and fixed effects" = model_fe_time,
      "Event study saturated" = model_es,
      "Event study saturated with fixed effects" = model_es_fe
    ) 
    
    
  
  return(output)
  
}




c(
  "pff_p",
  "ext_p",
  "log_assets_attr_pff",
  "log_assets_attr_ext",
  "log_utility",
  "log_cit_liability",
  "log_taxable_profits",
  "prominent",
  "log_assets_prominent"
  # ,
  # "any_change"
  ) %>% 
  map(~{
    
    vari <- .x
    
    if(vari == "any_change"){
      expanded <- expand(reg_table,anio_fiscal,firm_id)
      
      reg_table_0 <- expanded %>% 
        left_join(reg_table) %>% 
        group_by(firm_id) %>% 
        arrange(firm_id,anio_fiscal) %>% 
        fill(everything(),.direction = "down") %>% 
        mutate(
          cambio_pff = as.numeric(abs(pff_p - lag(pff_p,1)) > 0.05),
          cambio_ext = as.numeric(abs(ext_p - lag(ext_p,1)) > 0.05),
          cambio_nac = as.numeric(abs(nac_p - lag(nac_p,1)) > 0.05),
          indicador_cambio = cambio_pff + cambio_ext + cambio_nac,
          any_change = as.numeric(indicador_cambio > 0))  
    }else{
      reg_table_0 <- reg_table
    }
    
    reg_table_0 <- reg_table_0 %>% 
      filter(if_any(.cols = one_of(vari),~!is.na(.x)^!is.nan(.x)^!is.infinite(.x)))
    
    results <- diff_diff_model(rtable = reg_table_0,dependent = vari) %>% 
      imap(~perform_model(.x,rtable = reg_table_0,label = .y,dependent = vari)) %>% 
      transpose()
    
    results <- results[c("params","performance")] %>% 
      map(reduce,bind_rows)
    
    archivos <- names(results) %>% 
      str_c("03_RESULTADOS/WB_PB_INVERSIONES/20231024/Tablas/",.,"/dep_var_",.x,"_joint.txt")
    
    map2(
      .x = results,
      .y = archivos,
      ~ write_tsv(file = .y,x = .x)
    )
  })


c("pff_p",
  "ext_p",
  "log_assets_attr_pff",
  "log_assets_attr_ext",
  "log_utility",
  "log_cit_liability",
  "log_taxable_profits",
  "prominent",
  "log_assets_prominent"
  # ,
  # "any_change"
  ) %>% 
  map(~{
    
    vari <- .x
    
    if(vari == "any_change"){
      expanded <- expand(reg_table,anio_fiscal,firm_id)
      
      reg_table <- expanded %>% 
        left_join(reg_table) %>% 
        group_by(firm_id) %>% 
        arrange(firm_id,anio_fiscal) %>% 
        fill(everything(),.direction = "down") %>% 
        mutate(
          cambio_pff = as.numeric(abs(pff_p - lag(pff_p,1)) > 0.05),
          cambio_ext = as.numeric(abs(ext_p - lag(ext_p,1)) > 0.05),
          cambio_nac = as.numeric(abs(nac_p - lag(nac_p,1)) > 0.05),
          indicador_cambio = cambio_pff + cambio_ext + cambio_nac,
          any_change = as.numeric(indicador_cambio > 0))  
  }
    
    reg_table <- reg_table %>% 
      filter(if_any(.cols = one_of(vari),~!is.na(.x)^!is.nan(.x)^!is.infinite(.x)))
    
    results <- diff_diff_model(rtable = reg_table %>% 
                                 filter(group_assign %in% c("T-Maj","C-Maj")),dependent = vari) %>% 
      imap(~perform_model(.x,rtable = reg_table %>% 
                            filter(group_assign %in% c("T-Maj","C-Maj")),label = .y,dependent = vari)) %>% 
      transpose()
    
    results <- results[c("params","performance")] %>% 
      map(reduce,bind_rows)
    
    
    archivos <- names(results) %>% 
      str_c("03_RESULTADOS/WB_PB_INVERSIONES/20231024/Tablas/",.,"/dep_var_",.x,"_majors.txt")
    
    map2(
      .x = results,
      .y = archivos,
      ~ write_tsv(file = .y,x = .x)
    )
  })

c("pff_p",
  "ext_p",
  "log_assets_attr_pff",
  "log_assets_attr_ext",
  "log_utility",
  "log_cit_liability",
  "log_taxable_profits",
  "prominent",
  "log_assets_prominent"
  # ,
  # "any_change"
  ) %>% 
  map(~{
    
    vari <- .x
    
    if(vari == "any_change"){
      expanded <- expand(reg_table,anio_fiscal,firm_id)
      
      reg_table <- expanded %>% 
        left_join(reg_table) %>% 
        group_by(firm_id) %>% 
        arrange(firm_id,anio_fiscal) %>% 
        fill(everything(),.direction = "down") %>% 
        mutate(
          cambio_pff = as.numeric(abs(pff_p - lag(pff_p,1)) > 0.05),
          cambio_ext = as.numeric(abs(ext_p - lag(ext_p,1)) > 0.05),
          cambio_nac = as.numeric(abs(nac_p - lag(nac_p,1)) > 0.05),
          indicador_cambio = cambio_pff + cambio_ext + cambio_nac,
          any_change = as.numeric(indicador_cambio > 0))  
    }
    
    reg_table <- reg_table %>% 
      filter(if_any(.cols = one_of(vari),~!is.na(.x)^!is.nan(.x)^!is.infinite(.x)))
    
    results <- diff_diff_model(rtable = reg_table %>% 
                                 filter(!group_assign %in% c("T-Maj","C-Maj")),dependent = vari) %>% 
      imap(~perform_model(.x,rtable = reg_table %>% 
                            filter(!group_assign %in% c("T-Maj","C-Maj")),label = .y,dependent = vari)) %>% 
      transpose()
    
    results <- results[c("params","performance")] %>% 
      map(reduce,bind_rows)
    
    archivos <- names(results) %>% 
      str_c("03_RESULTADOS/WB_PB_INVERSIONES/20231024/Tablas/",.,"/dep_var_",.x,"_minors.txt")
    
    
    map2(
      .x = results,
      .y = archivos,
      ~ write_tsv(file = .y,x = .x)
    )
  })





