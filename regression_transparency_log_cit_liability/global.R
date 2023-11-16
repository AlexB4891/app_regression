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
      mutate(term = str_extract(term,"[:digit:]{4}.*[:digit:]{4}"),
             term = str_extract(term,"[:digit:]{4}"),
             term = as.numeric(term)) %>% 
      filter(!is.na(term)) %>% 
      bind_rows(tibble(term = 2014,
                       estimate = 0,
                       conf.low = 0,
                       conf.high = 0))
  }
  
  plot_out <- plot_table %>% 
    ggplot() +
    geom_point(aes(x = term, y = estimate)) +
    geom_label(aes(x = term, 
                  y = estimate,
                  label = round(estimate, 3)),
              vjust = 1,
              alpha = 0.8,
              color = "darkblue") +
    geom_linerange(aes(x = term,
                       ymin = conf.low, 
                       ymax = conf.high ),
                   color = "darkblue") +
    geom_line(aes(x = term,
                  y = estimate),
              color = "darkblue",
              alpha = 0.7) +
    geom_hline(aes(yintercept = 0),linetype = "dashed") +
    theme_light() +
    theme(axis.title= element_blank(),text = element_text(size = 13),
          plot.margin = margin(t = 0,r = 0,b = 0.1,l = 0, unit = "cm")) +
    labs(title = title_plot,
         subtitle = subtitle_plot)
  
  if(type == "es"){
    
    # browser()
    
    plot_out <- plot_out +
      geom_line(aes(x = term, 
                    y = estimate),
                color = "darkblue",
                alpha = 0.7) +
      geom_vline(aes(xintercept = 2014.5),linetype = "dashed") +
      geom_hline(aes(yintercept = 0),linetype = "dashed") +
      scale_x_continuous(breaks = 2012:2017,
                         labels = c(2012:2017))
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
                                    gof.decimal = gof.decimal)
  return(tr_object)
}


# Objetos extra -----------------------------------------------------------

my_grid_template <- grid_template(
  default = list(
    areas = 
      rbind(
        c("sidebar","title","title","title","title","title"),
        c("sidebar","control","blank1" ,"modelo_tipo","modelo_design" ,"blank2" ),
        c("sidebar","main" ,"main" ,"main","main","table" ),
        c("sidebar","footer","footer" ,"footer" ,"footer" ,"footer" )),
    cols_width = c("22.5%","15%","5%","15%" ,"17.5%","25%"),
    rows_height = c("5%","5%", "80", "10%")
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
