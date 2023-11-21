library(tidyverse)


# list.files("20231120/params")  %>% 
#     str_remove("dep_var_") %>% 
#     str_remove(".txt")  %>% 
#     str_remove("_joint|_majors|_minors")  %>% 
#     unique()
# # 

choices_variable_new <- c("Tax haven participation" = "pff_p",
                          "Foreign participation" = "ext_p",
                          "Prominent participation in group" = "prominent",
                          "Levels assets attributable to tax havens" = "levels_assets_attr_haven",
                          "Levels assets attributable to tax havens (positive)" = "levels_assets_attr_haven_positive",
                          "Log assets attributable to tax havens (positive)" = "log_assets_attr_pff",
                          "Levels assets attributable to non tax havens" = "levels_assets_attr_nonhaven",
                          "Levels assets attributable to non tax havens (positive)" = "levels_assets_attr_nonhaven_positive",
                          "Log assets attributable to non tax havens (positive)" = "log_assets_attr_ext",
                          "Level assets attributable to main group" = "level_assets_attr_main",
                          "Level assets attributable to main group (positive)" = "level_assets_attr_main_positive",
                          "Log assets attributable to main group" = "log_assets_prominent",
                          "Income tax rate" = "tasa_ir",
                          "Log(CIT liability)" = "log_cit_liability",
                          "Log(Taxable profits)" = "log_taxable_profits",
                          "Log(Profits)" = "log_utility",
                          "Positive profits" = "positive_profits",
                          "Any change" = "any_change",
                          "CIT declared dummy" = "en_f101"
                          )
   

for (i in 1:length(choices_variable)){ 
        
        label <- choices_variable[i]
        var <- names(choices_variable)[i]
        # browser()
        dir_name <- str_c("regression_transparency_",label)
        dir.create(dir_name)
        
        # Cambia el directorio de trabajo a este nuevo directorio
        setwd(dir_name)
        
        file.copy(from = "../regression_transparency.qmd",
                  to = "./")
        
        file.copy(from = "../global.R",
                  to = "./")
        
        quarto::quarto_render(input = "regression_transparency.qmd",
                              output_file = str_c("regression_transparency_",var,".html"),
                              execute_params =  list(var_dep = label,
                                                     var_lab = var))
        
        # Cambia el directorio de trabajo de nuevo al directorio original después de ejecutar el documento
        setwd("..")
        
      }

        