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
                           type = "lm"){
  
  if(type == "lm"){
    plot_table <- tidy_tems %>% 
      mutate(term = str_remove_all(term,"^treatment|^post"),
             term = str_replace(term,":post",":"))
  }else{
    plot_table <- tidy_tems %>% 
      mutate(term = str_remove_all(term,"^anio_refact|^post"),
             term = as.numeric(term),
             term = replace_na(term,2011))
  }
  
  plot_out <- plot_table %>% 
    ggplot() +
    geom_point(aes(x = term, y = estimate)) +
    geom_text(aes(x = term, 
                  y = estimate,
                  label = round(estimate, 3)),
              vjust = -0.5,
              color = "darkblue") +
    geom_linerange(aes(x = term,
                       ymin = conf.low, 
                       ymax = conf.high ),
                   color = "darkblue") +
    # coord_flip() +
    theme_light() +
    theme(axis.title= element_blank(),text = element_text(size = 14)) +
    labs(title = title_plot,
         subtitle = subtitle_plot)
  
  if(type == "es"){
    plot_out <- plot_out +
      geom_line(aes(x = term, y = estimate),
                color = "darkblue") +
      geom_vline(aes(xintercept = 2014.5),linetype = "dashed") +
      scale_x_continuous(breaks = 2011:2017,
                         labels = c("(Intercept)",2012:2017))
  }
  
  return(plot_out)
  
}


