####################
# Global functions #
####################
# Author: Alex Bajaña

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shiny.semantic)
library(texreg)
library(gt)


# Graficos para los coeficientes ------------------------------------------


coef_plot_plus <- function(tidy_tems,
                           title_plot,
                           subtitle_plot,
                           color = "",
                           type = "lm"){
  
  if(type == "lm"){
    plot_table <- tidy_tems %>% 
      mutate(term = str_remove_all(term,"^treatment|^post"),
             term = str_replace(term,":post",":"))
  }else{
    tidy_tems %>% 
      mutate(term = str_remove_all(term,"^treatment|^post"),
             term = str_replace(term,":post",":"))
  }
  
  plot_table %>% 
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
