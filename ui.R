
semanticPage(
  grid(my_grid_template,
       sidebar = tagList(
         h2("Retaining yout tax base"),
         uiOutput("formula"),
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
       modelo_tipo = 
         multiple_radio(input_id =  "modelo_tipo",
                        label = "Tipo de modelo",
                        choices = names(choices_model),
                        choices_value = choices_model,
                        selected = "satu"),
        modelo_design = multiple_radio(input_id =  "modelo_design",
                        label = "Tipo de diseño",
                        choices = names(choices_design),
                        choices_value = choices_design,
                        selected = "lm"),
       main = plotOutput("params"),
       table = tagList(
         h3("Regression output"),
         verbatimTextOutput("perfor")),
       footer = textOutput(outputId = "texto")
  )
)