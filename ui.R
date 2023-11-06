my_grid_template <- grid_template(
  default = list(
    areas = 
      rbind(
        c("sidebar","title","title","title","title","title"),
        c("sidebar","control","blank1" ,"modelo","blank2" ,"table" ),
        c("sidebar","main" ,"main" ,"main","main","table" ),
        c("sidebar","footer","footer" ,"footer" ,"footer" ,"footer" )),
    cols_width = c("22.5%","15%","5%","15%" ,"17.5%","25%"),
    rows_height = c("10%","10%", "70", "10%")
  ))


choices_grupo <- c("T-Maj + T-Min vs C-Maj + C-Min" = "joint",
                   "T-Maj vs C-Maj" = "majors",
                   "T-Min vs C-Min" = "minors")

choices_variable <- c("Tax haven participation" = "pff_p",
                      "Foreign participation" = "ext_p",
                      "Log amount of assets attributable to TH" = "log_assets_attr_pff",
                      "Log amount of assets attributable to non TH" = "log_assets_attr_ext",
                      "Log(CIT liability)" = "log_cit_liability",
                      "Log(Profits)"  = "log_utility",
                      "Log(Taxable profits)" = "log_taxable_profits",
                      "Prominent participation in group" = "prominent",
                      "Amount of assets atributables in dominant group"  = "log_assets_prominent")

choices_model <- c("Saturarated model" = "satu",
                   "Fixed effect model" = "fe")

choices_design <- c("Diff-in-diff design" = "lm",
                    "Event study design"  = "es")

ui <- semanticPage(
  grid(my_grid_template,
       sidebar = tagList(
         h2("Retaining yout tax base"),
         p("Este es un ejemplo")
       ),
       title  = h2("Aplicación para revisar los resultados de los modelos D-D"),
       blank1  = div(),
       blank2  = div(),
       blank3  = div(),
       blank4  = div(),
       control = tagList(
         selectInput(inputId = "grupo",
                     label = "Grupo",
                     choices = choices_grupo,
                     selected = "joint"),
         selectInput(inputId = "variable",
                     label = "Variable de respuesta",
                     choices = choices_variable,
                     selected = "pff_p")  
       ),
       modelo = tagList(
         multiple_radio(input_id =  "modelo_tipo",
                        label = "Tipo de modelo",
                        choices = names(choices_model),
                        choices_value = choices_model,
                        selected = "satu"),
         multiple_radio(input_id =  "modelo_design",
                        label = "Tipo de diseño",
                        choices = names(choices_design),
                        choices_value = choices_design,
                        selected = "lm")
       ),
       main = plotOutput("params"),
       table = verbatimTextOutput("perfor"),
       footer = textOutput(outputId = "texto")
  )
)