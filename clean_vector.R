texto_tabla <- ' # Compliance --------------------------------
  # Compliance
  "en_aps",
  "en_mid",
  # Actionary participation ------------------
  # Actionary participation
  "pff_p",
  "ext_p",
  "nac_p",
  "prominent",
  "inverse_prom",
  # Levels ---------------------------------------------------      
  # Tax results
  "total_assets",
  "tax_liability",
  "net_profits",
  "taxable_profits",
  "tasa_ir",
  # Assets attributables
  "level_assets_attr_inv",
  "level_assets_attr_main",
  "levels_assets_attr_haven",
  "levels_assets_attr_nonhaven",
  "levels_assets_attr_domestic",
  # Exits
  "mid_firm_total_salida_pff",
  "mid_firm_total_salida_ext",
  "mid_total_salida_pff",
  "mid_total_salida_ext",
  "mid_total_salida_all",
  # Entrances
  "mid_firm_total_entrada_pff",
  "mid_firm_total_entrada_ext",
  "mid_total_entrada_pff",
  "mid_total_entrada_ext",
  "mid_total_entrada_all",
  # Logs ---------------------------------------------------
  # Tax results
  "log_total_assets",
  "log_utility",
  "log_cit_liability",
  "log_taxable_profits",
  # Assets atribuibles:
  "log_assets_inv",
  "log_assets_prominent",
  "log_assets_attr_pff",
  "log_assets_attr_ext",
  "log_assets_attr_dom",
  # Exits
  "log_mid_firm_total_salida_pff",
  "log_mid_firm_total_salida_ext",
  "log_mid_total_salida_pff",
  "log_mid_total_salida_ext",
  "log_mid_total_salida_all",
  # Entrances
  "log_mid_firm_total_entrada_pff",
  "log_mid_firm_total_entrada_ext",
  "log_mid_total_entrada_pff",
  "log_mid_total_entrada_ext",
  "log_mid_total_entrada_all",
  # Binarias si el valor es positivo  --------------
  # Tax results
  "bin_total_assets",
  "bin_net_profits",
  "bin_tax_liability",
  "bin_taxable_profits",
  # Assets attributables
  "bin_level_assets_attr_inv",
  "bin_level_assets_attr_main",
  "bin_levels_assets_attr_haven",
  "bin_levels_assets_attr_nonhaven",
  "bin_levels_assets_attr_domestic",
  # Exits
  "bin_mid_firm_total_salida_pff",
  "bin_mid_firm_total_salida_ext",
  "bin_mid_total_salida_pff",
  "bin_mid_total_salida_ext",
  "bin_mid_total_salida_all",
  # Entrances
  "bin_mid_firm_total_entrada_pff",
  "bin_mid_firm_total_entrada_ext",
  "bin_mid_total_entrada_pff",
  "bin_mid_total_entrada_ext",
  "bin_mid_total_entrada_all",
  # Binarias de cambio con t-1  --------------
  # Tax results
  "change_total_assets",
  "change_net_profits",
  "change_tax_liability",
  "change_taxable_profits",
  # Assets attributables
  "change_level_assets_attr_inv",
  "change_level_assets_attr_main",
  "change_levels_assets_attr_haven",
  "change_levels_assets_attr_nonhaven",
  "change_levels_assets_attr_domestic",
  # Exits
  "change_mid_firm_total_salida_pff",
  "change_mid_firm_total_salida_ext",
  "change_mid_total_salida_pff",
  "change_mid_total_salida_ext",
  "change_mid_total_salida_all",
  # Entrances
  "change_mid_firm_total_entrada_pff",
  "change_mid_firm_total_entrada_ext",
  "change_mid_total_entrada_pff",
  "change_mid_total_entrada_ext",
  "change_mid_total_entrada_all"'


dicctionary_from_text <- str_split(pattern = "\n",string =  texto_tabla)  %>% 
  unlist() %>% 
  str_remove_all(pattern = '\\"|,') %>% 
  str_trim() %>% 
  tibble(raw = .) %>%
  mutate(
    section = if_else(str_detect(raw,"#.*-+"),raw,NA_character_),
    subsection = if_else(str_detect(raw,"#.*") & is.na(section),raw,NA_character_),
    variable = if_else(is.na(section) & is.na(subsection),raw,NA_character_)) %>% 
  fill(section,subsection) %>% 
  filter(!is.na(variable)) 

  
  
dicctionary_from_text %>% 
  write_tsv(file = "data/diccionario_script_20240213.txt")




choices_variable <- c("Tax haven participation" = "pff_p",
                      "Foreign participation" = "ext_p",
                      # "Domestic participation" = "nac_p",
                      "Prominent participation in main group" = "prominent",
                      "Inverse prominent part. in main group" = "inverse_prom",
                      "Levels assets attributable to tax havens" = "levels_assets_attr_haven",
                      "Levels assets attributable to tax havens (positive)" = "levels_assets_attr_haven_positive",
                      "Log assets attributable to tax havens (positive)" = "log_assets_attr_pff",
                      "Levels assets attributable to non tax havens" = "levels_assets_attr_nonhaven",
                      "Levels assets attributable to non tax havens (positive)" = "levels_assets_attr_nonhaven_positive",
                      "Log assets attributable to non tax havens (positive)" = "log_assets_attr_ext",
                      "Level assets attributable to main group" = "level_assets_attr_main",
                      "Level assets attributable to main group (positive)" = "level_assets_attr_main_positive",
                      "Log assets attributable to main group" = "log_assets_prominent",
                      "Level assets attributable domestic" = "levels_assets_attr_domestic",
                      "Log assets attributable domestic" = "log_assets_attr_dom",
                      "Level assets attributable to inverse main group" = "level_assets_attr_inv",
                      "Log assets attributable to inverse main group" = "log_assets_inv",
                      "Income tax rate" = "tasa_ir",
                      "Log(CIT liability)" = "log_cit_liability",
                      "Log(Taxable profits)" = "log_taxable_profits",
                      "Log(Profits)" = "log_utility",
                      "Positive profits" = "positive_profits",
                      "APS compliance (%)" = "en_aps",
                      "MID compliance (%)" = "en_mid",
                      "Any change" = "any_change",
                      "Log financial activity (MID Codes 4-7, Non tax havens)" =  "log_mid_finance_ext",
                      "Log financial activity (MID Codes 4-7, Tax haven)" =  "log_mid_finance_pff",
                      "Log international activity (All MID codes, Tax haven)" = "log_mid_total_pff",
                      "Commerce and financial activity (Non tax havens)" = "mid_firm_percent_ext",
                      "Commerce and financial activity (Tax haven)" = "mid_firm_percent_pff",
                      "Percent of international activity (Non tax havens)" = "mid_percent_ext",    
                      "Percent of international  activity (Tax haven)" = "mid_percent_pff")



choices_variable <- enframe(choices_variable,name = "label",value = "sri_out")

clean_dicc_sri %>% 
  left_join(choices_variable,by = c("var" = "sri_out")) %>% 
  write_tsv("data/diccionario_sri_20240213.txt")



presentes <- list.files("20240130/params/",full.names = F) %>% 
  str_remove_all("dep_var_|_joint|_majors|_minors|\\.txt") %>% 
  unique()


ejecuciones <- read_tsv("data/diccionario_script_20240213.txt")

ejecuciones %>% 
  left_join(choices_variable,by = c("variable" = "sri_out"))   %>% 
  write_tsv("data/diccionario_sri_20240213.txt")