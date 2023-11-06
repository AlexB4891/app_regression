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


# Función para poner los modelos en modo tabla ----------------------------

extract_broom <- function(tidy_model, glance_model){
  
  # browser()
  
  # get estimates/standard errors from tidy
  
  coef <- tidy_model$estimate
  
  ci_low <- tidy_model$conf.low
  ci_up <- tidy_model$conf.high
  
  coef.names <- as.character(tidy_model$term)
  
  se <- tidy_model$std.error
  
  pvalues <- tidy_model$p.value
  
  # get goodness-of-fit statistics from glance
  
  glance_transposed <- glance_model %>% 
    select(-model) %>% 
    pivot_longer(everything()) %>% 
    filter(!is.na(value))
  
  # as_tibble(cbind(name = names(glance_model), value = t(glance_model)))
  
  gof.names <- as.character(glance_transposed$name)
  
  gof <- as.double(glance_transposed$value)
  
  gof.decimal <- gof %% 1 > 0
  
  # browser()
  
  tr_object <- texreg::createTexreg(coef.names = coef.names,
                                    coef = coef,
                                    se = se,
                                    pvalues = pvalues,
                                    gof.names = gof.names,
                                    gof = gof,
                                    gof.decimal = gof.decimal,
                                    ci.low = ci_low,
                                    ci.up = ci_up)
  return(tr_object)
}

# extract_broom(tidy_model = dep_var_pff_p_joint_p %>% 
#                 filter(model == "Event study saturated with fixed effects"),
#               glance_model = dep_var_pff_p_joint %>% 
#                 filter(model == "Event study saturated with fixed effects")) %>% 
#   screenreg()
