library(tidyverse)


# list.files("20231120/params")  %>% 
#     str_remove("dep_var_") %>% 
#     str_remove(".txt")  %>% 
#     str_remove("_joint|_majors|_minors")  %>% 
#     unique()
# # 

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
   

presentes <- list.files("20240130/params/",full.names = F) %>% str_remove_all("dep_var_|_joint|_majors|_minors|\\.txt") %>% unique()

for (i in 28:length(choices_variable)){ 
        
        label <- choices_variable[i]
        
        if(label %in% presentes){
          
          var <- names(choices_variable)[i]
          # browser()
          dir_name <- str_c("quarto/regression_transparency_",label)
          
          dir.create(dir_name)
          
          # Cambia el directorio de trabajo a este nuevo directorio
          setwd(dir_name)
          
          file.copy(from = "../../regression_transparency.qmd",
                    to = "./")
          
          file.copy(from = "../../global.R",
                    to = "./")
          
          quarto::quarto_render(input = "regression_transparency.qmd",
                                output_file = str_c("regression_transparency_",label,".html"),
                                execute_params =  list(var_dep = label,
                                                       var_lab = var))
          
          # Cambia el directorio de trabajo de nuevo al directorio original después de ejecutar el documento
          setwd("../..")
          
          
        }
        
       
      }

        
choices_variable <- enframe(choices_variable,name = "label",value = "sri_out")

salidas <- tibble(sri_out = presentes) %>% 
  mutate(quarto_dir = str_c("quarto/regression_transparency_",sri_out))
  

carpetas <- tibble(quarto_dir = list.files("quarto/",
                                           full.names = T,
                                           recursive = F))

salidas <- salidas %>% 
  right_join(carpetas) %>% 
  mutate(archivo = map(quarto_dir,list.files,pattern = "\\.html")) %>% 
  unnest(archivo) %>% 
  left_join(choices_variable)


salidas %>% 
  transmute(
    ruta = str_c(quarto_dir,archivo,sep ="/"),
    tag = glue::glue(' <option value= "{ruta}">{label}</option>')
  ) %>% 
  pull(tag) %>% 
  str_c(collapse = "\n") %>% 
  cat()



