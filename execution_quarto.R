library(tidyverse)
library(readxl)
library(here)
library(furrr)



last_date <- "20240209"

# Primero leemos el diccionario extraido del SRI

dicc_sri <- read_tsv(here(last_date,"Tablas","salidas.txt"))

variables_sri <- dicc_sri %>% 
  distinct(var) 

# Contar las ocurrencias en gr agrupando por va, 
# la condición es que res sea distinto de NA, 
# si res es NA y el conteo de gr es 3 entonces la variable
# tiene estado OK, pero si algun res 
# es texto quiere decir que el reporte no está completo

clean_dicc_sri <- dicc_sri %>% 
  fill(var,gr,.direction = "down") %>%
  mutate(res = if_else(is.na(res),"",res)) %>%
  group_by(var,gr) %>% 
  summarise(res = str_c(res,collapse = " ")) %>% 
  ungroup() 


# Escritura del diccionario limpio:
clean_dicc_sri %>% 
  write_tsv("data/diccionario_sri_20240213.txt")


dicc_sri_incomplete <- clean_dicc_sri %>% 
  group_by(var) %>%
  summarise(n_distinct_res = sum(res == "")) %>%
  filter(n_distinct_res != 3) %>% 
  arrange(n_distinct_res) 


dicc_label <- read_excel("data/diccionario_con_labels.xlsx") 


dicc_label_completes <- dicc_label %>% 
  anti_join(dicc_sri_incomplete,by = c("variable" = "var"))

choices_variable <- set_names(dicc_label_completes$variable,dicc_label_completes$label)

presentes <- list.files(here(last_date,"params/"),full.names = F) %>% 
  str_remove_all("dep_var_|_joint|_majors|_minors|\\.txt") %>% 
  unique()


plan(multisession, workers = 20)


future_walk(
  .x = 1:length(choices_variable),
  .f = ~{
    
    require(tidyverse)
    require(quarto)
    
    label <- choices_variable[.x]
    
    if(label %in% presentes){
      
      var <- names(choices_variable)[.x]
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
                                                   var_lab = var,
                                                   date = last_date))
      
      # Cambia el directorio de trabajo de nuevo al directorio original después de ejecutar el documento
      setwd("../..")
      
      
    }
    
    
  }
)


        
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



