
iwalk(.x = choices_variable <- c("Tax haven participation" = "pff_p",
                                 "Foreign participation" = "ext_p",
                                 "Log amount of assets attributable to TH" = "log_assets_attr_pff",
                                 "Log amount of assets attributable to non TH" = "log_assets_attr_ext",
                                 "Log(CIT liability)" = "log_cit_liability",
                                 "Log(Profits)"  = "log_utility",
                                 "Log(Taxable profits)" = "log_taxable_profits",
                                 "Prominent participation in group" = "prominent",
                                 "Amount of assets atributables in dominant group"  = "log_assets_prominent",
                                 "Any change in participation declared" = "any_change",
                                 "CIT declared dummy" = "en_f101",
                                 "Positive profits" = "positive_profits"),
      .f = ~{
        
        label <- .y
        var <- .x
        
        dir.create(str_c("regression_transparency_",var))
        
        quarto::quarto_render(input = "regression_transparency.qmd",
                              output_file = str_c("regression_transparency_",var,".html"),
                              execute_params =  list(var_dep = var,
                                            var_lab = label))
        
      })

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

        