

server <- function(input, output, session) {
  
  archivos <- reactiveValues(
    params = "20231024/params/dep_var_pff_p_joint.txt",
    perfor = "20231024/performance/dep_var_pff_p_joint.txt"
  )
  
  cambios <- reactive(
    list(
      input$variable,
      input$grupo,
      input$modelo_tipo,
      input$modelo_design
    )
  )
  
  observeEvent(cambios(),{
    
    archivos$params <- str_c("20231024/params/dep_var_",input$variable,"_",input$grupo,".txt")
    archivos$perfor <- str_c("20231024/performance/dep_var_",input$variable,"_",input$grupo,".txt")
    
  })
  
  reg_param <- reactive({
    
    read_tsv(archivos$params)
    
  })
  
  reg_perfor <- reactive({
    
    read_tsv(archivos$perfor)
    
  })
  
  filtro <- reactive({
    
    # browser()
    
    x <- if(input$modelo_tipo == "satu" & input$modelo_design == "lm"){
      "Linear model"  
      
    }else if(input$modelo_tipo == "fe" & input$modelo_design == "lm"){
      "Linear model with fixed effect"
      
    }else if(input$modelo_tipo == "satu" & input$modelo_design == "es"){
      "Event study saturated"
      
    }else if(input$modelo_tipo == "fe" & input$modelo_design == "es"){
      "Event study saturated with fixed effects" 
      
    }
    
    return(x)
  })
  
  
  labels_plot <- reactiveValues(
    title = "Diff-in-Diff Design: Tax haven participation",
    subtitle = "Saturated model with firm clustered standard errors"
  )
  
  observeEvent(cambios(),{
    
    lab_mod <- names(choices_model)[choices_model == input$modelo_tipo]
    
    lab_des <- names(choices_design)[choices_design == input$modelo_design]
    
    lab_var <- names(choices_variable)[choices_variable == input$variable]
    
    labels_plot$title <- str_c(lab_des,lab_var,sep = ": ")
    
    labels_plot$subtitle <- str_c(lab_mod,"with firm clustered standard errors",sep = " ")
    
  })
  
  
  
  reactive_plot <- reactive({
    
    ff <- filtro()
    
    reg_param() %>% 
      filter(model == ff) %>% 
      coef_plot_plus(title_plot = input$variable,
                     subtitle_plot = input$grupo)
  })
  
  
  labaled <- reactive({
    
    reactive_plot() +
      labs(
        title = labels_plot$title,
        subtitle = labels_plot$subtitle
      )
  })
  
  output$params <- renderPlot({
    
    labaled()  
    
    
    
  })
  
  output$perfor <- renderText({
    
    # browser()
    
    ff <- filtro()
    
    reg <- reg_param() %>% 
      filter(model == ff)
    
    perf <-   reg_perfor() %>% 
      filter(model == ff)
    
    texreg_model <- extract_broom(reg, perf)
    
    screenreg(list(texreg_model))
    
  })
  
  
}