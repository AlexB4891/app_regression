library(tidyverse)
library(fixest)
library(plm)
library(broom)
library(scales)
library(here)
library(lmtest)
library(sandwich)
library(furrr)


# Functions ---------------------------------------------------------------



coef_plot_plus_se <- function(tidy_tems,
                              title_plot,
                              subtitle_plot){
  
  # browser()
  
  tidy_tems %>% 
    filter(str_detect(term,"treatment")) %>% 
    mutate(
      term = str_remove_all(term,"i\\(.*\\)"),
      term = str_remove_all(term,"anio_fiscal::"),
      term = str_remove_all(term,":treatment"),
      term = as.numeric(term)) %>% 
    filter(!is.na(term)) %>% 
    bind_rows(tibble(term = 2014,estimate = 0, conf.low = 0, conf.high = 0)) %>% 
    ggplot() +
    geom_point(aes(x = term, y = estimate)) +
    geom_point(aes(x = term, y = estimate)) +
    geom_text(aes(x = term, 
                  y = estimate,
                  label = round(estimate, 3)),
              vjust = -0.5,
              color = "#841CD4") +
    geom_vline(aes(xintercept = 2014.5), linetype = "dashed") +
    geom_hline(aes(yintercept = 0)) +
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

firm_table <- read_rds(here("01_DATA/APS/core_sample_assigned_alternatives.rds"))

firm_table <- firm_table %>% 
  select(anio_fiscal,
         en_f101,
         identificacion_informante_anon,
         group_assign,
         porcentaje_aps_pff,
         porcentaje_aps_ext,
         porcentaje_aps_nac,
         total_assets,
         tax_liability,
         net_profits,
         taxable_profits,
         tasa_ir,
         treatment_a,
         treatment_b,
         treatment_c) %>% 
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
    ),
    inverse_prom  = if_else(
      treatment == "Control", porcentaje_aps_pff,porcentaje_aps_ext
    )
  ) %>% 
  select(
    en_f101,
    tasa_ir,
    pff_p = porcentaje_aps_pff,
    ext_p = porcentaje_aps_ext,
    nac_p = porcentaje_aps_nac,
    post,
    treatment,
    anio_fiscal,
    firm_id = identificacion_informante_anon,
    prominent,
    inverse_prom,
    group_assign,
    total_assets,
    tax_liability,
    net_profits,
    taxable_profits,
    treatment_a,
    treatment_b,
    treatment_c
  ) %>% 
  mutate(across(c(total_assets,
                  tax_liability,
                  net_profits,
                  taxable_profits),
                ~datawizard::winsorize(.x,method = "percentile",treshold = 0.01)))


# AQUI --------------------------------------------------------------------


# Logs de variables

reg_table <- reg_table %>% 
  mutate(
    level_assets_attr_inv = inverse_prom*total_assets,
    log_assets_inv = log(level_assets_attr_inv),
    level_assets_attr_main = prominent*total_assets,
    log_assets_prominent = log(level_assets_attr_main),
    levels_assets_attr_haven = total_assets*pff_p,
    log_assets_attr_pff = log(levels_assets_attr_haven),
    levels_assets_attr_nonhaven = total_assets*ext_p,
    log_assets_attr_ext = log(levels_assets_attr_nonhaven),
    levels_assets_attr_domestic = total_assets*nac_p,
    log_assets_attr_dom = log(levels_assets_attr_domestic),
    log_utility = log(net_profits),
    log_cit_liability = log(tax_liability),
    log_taxable_profits = log(taxable_profits),
    across(c(
      log_assets_prominent,
      log_assets_attr_pff,
      log_assets_attr_ext,
      log_assets_attr_dom,
      log_utility,
      log_cit_liability,
      log_taxable_profits
    ), ~if_else(is.infinite(.x),NA_real_,.x))
  )


# Relative change  

reg_2014 <- reg_table %>% 
  filter(anio_fiscal == 2014) %>% 
  select(firm_id,
         pff_2014 = pff_p,
         prom_2014 = prominent,
         assets_2014 = total_assets) %>% 
  mutate(assets_2014 = replace_na(assets_2014, 0))

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


reg_table <- reg_table %>%
  mutate(anio_fiscal_f = factor(anio_fiscal),
         anio_refact = relevel(anio_fiscal_f,
                               ref = "2014"),
         treatment_num = as.numeric(treatment) - 1,
         positive_profits = as.numeric(net_profits > 0))


reg_table <- reg_table %>% 
  mutate(treatment = if_else(treatment == "Treatment",1,0))

expanded <- expand(reg_table ,anio_fiscal,firm_id)

reg_table_exp <- expanded %>% 
  left_join(reg_table) %>% 
  group_by(firm_id) %>% 
  arrange(firm_id,anio_fiscal) %>% 
  fill(everything(),
       .direction = "down")  %>% 
  select(
    firm_id,
    anio_fiscal,
    pff_p,
    ext_p,
    nac_p,
    anio_fiscal,
    treatment, 
    assets_2014,
    group_assign,
    treatment_a,
    treatment_b,
    treatment_c,
    tasa_ir) %>% 
  mutate(
    cambio_ext = as.numeric(abs(ext_p - dplyr::lag(ext_p,1)) > 0.005),
    cambio_pff = as.numeric(abs(pff_p - dplyr::lag(pff_p,1)) > 0.005),
    cambio_nac = as.numeric(abs(nac_p - dplyr::lag(nac_p,1)) > 0.005))  


reg_table_exp <- reg_table_exp %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(any_change = sum(c_across(cols = c(cambio_ext,
                                            cambio_pff,
                                            cambio_nac)),na.rm = T))

reg_table_exp <-  reg_table_exp %>%
  mutate(post = if_else(anio_fiscal<=2014,1,0))



# Funciones de modelo -----------------------------------------------------


diff_diff_model <- function(rtable,dependent){
  
  # 1. Diff in diff model --------------------------------------------------
  
  fml <- glue::glue('{dependent} ~ treatment + post + treatment:post')
  
  fml_fe <- glue::glue('{dependent} ~  treatment + post + treatment:post | firm_id + anio_fiscal') %>%
    formula(.)
  
  
  did_model <- lm(data = rtable,
                  formula = fml)
  
  
  # 2. Diff in diff model with fixed effects ----------------------------------
  
  did_model_fe <- plm(data = rtable,
                      formula = fml,
                      index = c("firm_id","anio_fiscal"),
                      model = "within")
  
  # 3. Diff in diff model assets weighted -------------------------------------
  
  did_model_aw <- lm(data = rtable,
                     formula = fml,
                     weights = assets_2014)
  
  # 4. Diff in diff model with fixed effects  assets weighted -----------------
  
  did_model_fe_aw <- feols(data = rtable,
                           fml = fml_fe,
                           weights = ~assets_2014,
                           cluster = ~firm_id)
  # 
  # # 5. Event study design -----------------------------------------------------
  # 
  #   
  fml_event_study <- glue::glue('{dependent} ~  anio_fiscal + treatment + i(anio_fiscal,treatment, ref = 2014)') %>%
    formula(.)
  
  fml_event_study_fe <- glue::glue('{dependent} ~  i(anio_fiscal,treatment, ref = 2014) | firm_id + anio_fiscal') %>%
    formula(.)
  
  es_model <- lm(data = rtable,
                 formula = fml_event_study)
  
  
  # 6. Event study design with fixed effects ----------------------------------
  
  
  es_model_fe <- plm(data = rtable,
                     formula = fml_event_study,
                     index = c("firm_id","anio_fiscal"),
                     model = "within")
  
  # 7. Event study design assets weighted -------------------------------------
  
  es_model_aw <- feols(data = rtable,
                       fml = fml_event_study,
                       weights = ~assets_2014,
                       cluster = ~firm_id)
  
  
  # 8. Event study design with fixed effects assets weighted -------------------
  
  
  es_model_aw_fe <- feols(data = rtable,
                          fml = fml_event_study_fe,
                          weights = ~assets_2014,
                          cluster = ~firm_id)
  
  output <- list(
    "Linear model" =   did_model,
    "Linear model with fixed effect" = did_model_fe,
    "Linear model assets weighted" = did_model_aw ,
    "Linear model assets weighted with fixed effect" = did_model_fe_aw,
    "Event study" = es_model,
    "Event study with fixed effects" = es_model_fe,
    "Event study assets weighted" = es_model_aw,
    "Event study assets weighted with fixed effects" = es_model_aw_fe
  )
  
  return(output)
  
}

perform_model_temp <- function(model,
                               label,
                               rtable,
                               dependent){
  
  
  
  if(label %in% c("Linear model assets weighted")){
    rtable <- rtable %>% 
      filter(if_any(.cols = one_of(dependent),~!is.na(.x)))
  }
  
  if(label %in% c(
    "Linear model assets weighted with fixed effect",
    "Event study assets weighted",
    "Event study assets weighted with fixed effects")){
    rtable <- rtable %>% 
      filter(assets_2014 > 0,
             if_any(.cols = one_of(dependent),~!is.na(.x)))
    
  }
  
  
  if(label %in% c("Event study with fixed effects",
                  "Linear model with fixed effect")){
    
    vcov <- vcovHC(x = model,
                   type = "HC1",
                   cluster = "group")
    
  }else{
    vcov <- sandwich::vcovCL(x = model,
                             cluster = rtable$firm_id)
  }
  
  
  model_corrected <- coeftest(vcov. = vcov,
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

# vari <- "log_assets_attr_pff"

# level_assets_attr_main = prominent*total_assets,
# log_assets_prominent = log(level_assets_attr_main),
# vari <- "levels_assets_attr_haven"

# function(var,filtro = "level")

# test_table <- reg_table %>% 
#   filter(log_assets_attr_ext)
# 
# vari <- "log_assets_attr_ext"
# # 
# modelo <- diff_diff_model(rtable =  test_table ,dependent = vari)
# 
# tests <- modelo %>%
#   imap(~perform_model_temp(.x,rtable = test_table,label = .y,dependent = vari))
# # 
# # 
# tests$`Event study`$params %>%
#   coef_plot_plus_se(title_plot = "Log assets attributables to non-tax haven",
#                  subtitle_plot = "Event study design, no restriction")
# # lm(any_change ~ treatment + post + treatment:post,data = reg_table_exp_2 )



# Ejecución ---------------------------------------------------------------

# Función que ejecuta:

execute_model <- function(grupo){
  
  c(
    "log_cit_liability",
    "log_taxable_profits",
    "prominent",
    "level_assets_attr_main",
    "level_assets_attr_main_positive",
    "log_assets_prominent"
  ) %>% 
    map(~{
      
      vari <- .x
      
      
      
      
      
      if(vari %in% c("any_change")){
        reg_table_0 <-  reg_table_exp
      }else{
        reg_table_0 <-  reg_table
      }
      
      reg_tables <- list(
        all = reg_table_0,
        treat_a = reg_table_0 %>% 
          filter(!is.na(treatment_a)),
        treat_b = reg_table_0 %>% 
          filter(!is.na(treatment_b)),
        treat_c = reg_table_0 %>% 
          filter(!is.na(treatment_c)),
        cit_m22 = reg_table_0 %>% 
          filter(tasa_ir >= 0.22)
      )
      
      
      if(grupo == "_majors"){
        
        reg_tables <-  reg_tables %>% 
          map(~.x %>% 
                filter(group_assign %in% c("T-Maj","C-Maj")))
        
        
      }else if(grupo == "_minors"){
        
        reg_tables <-  reg_tables %>% 
          map(~.x %>%  
                filter(group_assign %in% c("T-Min","C-Min")))
      }
      
      if(vari %in% c("level_assets_attr_main_positive",
                     "levels_assets_attr_haven_positive",
                     "levels_assets_attr_nonhaven_positive",
                     "level_assets_attr_inv",
                     "levels_assets_attr_domestic")){
        
        vari_2 <- str_remove(vari,"_positive")
        
        reg_tables <-  reg_tables %>% 
          map(~.x %>%   
                filter(if_any(.cols = one_of(vari_2),~.x > 0)) %>% 
                rename_with(.cols = vari_2,~vari))
      }
      
      reg_tables <-  reg_tables %>% 
        map(~.x %>%  
              filter(if_any(.cols = one_of(vari),~!is.na(.x)^!is.nan(.x)^!is.infinite(.x))))
      
      results <- reg_tables %>% 
        imap(~{ 
          
          pop <- .y
          
          tabla <- .x
          
          results <-  diff_diff_model(rtable = tabla,
                                      dependent = vari) %>%
            imap(~perform_model_temp(.x,rtable = tabla,
                                     label = .y,
                                     dependent = vari)) %>%
            transpose()
          
          results <- results[c("params","performance")] %>%
            map(reduce,bind_rows) %>%
            map(mutate,population = pop)
        })
      
      results <- list(
        params = results %>% 
          map("params") %>% 
          reduce(bind_rows),
        
        performance = results %>% 
          map("performance") %>% 
          reduce(bind_rows)
      )
      
      # browser()
      
      archivos <- names(results) %>% 
        str_c("03_RESULTADOS/WB_PB_INVERSIONES/20231212/Tablas/",.,"/dep_var_",.x,grupo,".txt")
      
      map2(
        .x = results,
        .y = archivos,
        ~ write_tsv(file = .y,x = .x)
      )
    })
}

# Expresión que ejecuta ----------------------------------

plan(multisession)

c("_joint","_majors","_minors") %>% 
  map(execute_model)