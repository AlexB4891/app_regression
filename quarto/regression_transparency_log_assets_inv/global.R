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
library(ggrepel)
# 
# datos <- read_tsv("20231212/Tablas/params/dep_var_pff_p_joint.txt")
# gdatos <- read_tsv("20231212/Tablas/performance/dep_var_pff_p_joint.txt")


# Graficos para los coeficientes ------------------------------------------


coef_plot_plus <- function(tidy_tems,
                           title_plot,
                           subtitle_plot,
                           type = "lm"){
  

  
  
  
  if(type == "lm"){
    # browser()
    plot_table <- tidy_tems %>% 
      mutate(term = str_replace(term,"(:?)Post",":"),
             term = str_to_sentence(term)
             )
  }else{
    
    
    
    plot_table <- tidy_tems %>% 
      filter(str_detect(term,"treatment")) %>% 
      mutate(
        term = str_remove_all(term,"i\\(.*\\)"),
        term = str_remove_all(term,"anio_fiscal::"),
        term = str_remove_all(term,":treatment"),
        term = as.numeric(term)) %>% 
      filter(!is.na(term)) %>% 
      filter(!is.na(term)) %>% 
      bind_rows(tibble(term = rep(2014,6),
                       estimate = rep(0,6),
                       conf.low = rep(0,6),
                       conf.high = rep(0,6),
                       population  = c("all","cit_more_stat",
                                       "cit_less_stat","treat_a",
                                       "treat_b","treat_c")))
  }
  
  
  
  plot_table <- plot_table %>% 
    mutate(population = case_when(
      population == "all" ~ "All firms",
      population == "cit_more_stat" ~ "CIT >= 22%",
      population == "cit_m22" ~ "CIT >= 22%",
      population == "cit_less_stat" ~ "0 < CIT < 22%",
      population == "treat_a" ~ "Treatment A",
      population == "treat_b" ~ "Treatment B",
      population == "treat_c" ~ "Treatment C"
    ),
    population = factor(population,
                        levels = c("All firms",
                                   "CIT >= 22%",
                                   "Treatment A",
                                   "Treatment B",
                                   "Treatment C"))
    )   %>%
    filter(!is.na(population))
  
  plot_out <- plot_table %>% 
    ggplot(aes(x = term, 
               y = estimate,
               color = population)) +
    geom_point(size = 3)  +
    geom_linerange(aes(x = term,
                       ymin = conf.low, 
                       ymax = conf.high )) +
    geom_line(aes(x = term,
                  y = estimate),
              alpha = 0.7) +
    geom_hline(aes(yintercept = 0),linetype = "dashed") +
    geom_vline(aes(xintercept = 2014.5),linetype = "dashed") +
    theme_light() +
    theme(axis.title= element_blank(),text = element_text(size = 9),
          plot.margin = margin(t = 0,r = 0,b = 0.1,l = 0, unit = "cm"),
          legend.position = "bottom",legend.text = element_text(size = 14), 
          title = element_text(size = 12)) +
    labs(title = title_plot,
         subtitle = subtitle_plot,
         color = "Population") + 
    guides(color = guide_legend(nrow = 2))
  
  # browser()
  
  if(type == "es"){
    
    # browser()
    
    plot_out <- plot_out +
      geom_line(aes(x = term, 
                    y = estimate,
                    color = population
                    ),
              
                alpha = 0.7) +
      geom_vline(aes(xintercept = 2014.5),linetype = "dashed") +
      geom_hline(aes(yintercept = 0),linetype = "dashed") +
      scale_x_continuous(breaks = 2010:2019,
                         labels = c(2010:2019))  +
      theme_light() +
      theme(axis.title= element_blank(),text = element_text(size = 13),
            plot.margin = margin(t = 0,r = 0,b = 0.1,l = 0, unit = "cm"),
            legend.position = "bottom",legend.text = element_text(size = 12), 
            title = element_text(size = 15)) +
      labs(title = title_plot,
           subtitle = subtitle_plot,
           color = "Population") + 
      guides(color = guide_legend(nrow = 2))
  }
  
  return(plot_out)
  
}

# coef_plot_plus(datos %>% filter(model == "Event study assets weighted with fixed effects"),
#                "Tax haven participation",
#                "T-Maj + T-Min vs C-Maj + C-Min",
#                type = "es")


# Función para poner los modelos en modo tabla ----------------------------

extract_broom <- function(tidy_model, glance_model, type = "lm"){
  
  # browser()
  
  
  if(type == "lm"){
    # browser()
    plot_table <- tidy_model %>% 
      mutate(term = str_replace(term,"(:?)Post",":"),
             term = str_to_sentence(term)
      )
  }
  
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


# extract_broom (datos %>% 
#                  filter(model == "Event study assets weighted",
#                         population == "all") %>% 
#                  select(-population),
#                gdatos %>% 
#                  filter(model == "Event study assets weighted",
#                         population == "all") %>% 
#                  select(-population),type = "lm")

make_multi_model <- function(model_df_0,
                             model_df_0_p,
                             modelo,
                             tipo){
  
  
  # browser()
  
  model_df <- model_df_0 %>% 
    filter(model == modelo) 
  
  model_df_p <- model_df_0_p %>% 
    filter(model == modelo)
  
  
  elemnts <- list(model_df,
                  model_df_p) %>% 
    map(~.x %>% 
          mutate(population = case_when(
            population == "all" ~ "All firms",
            population == "cit_m22" ~ "CIT >= 22%",
            population == "cit_more_stat" ~ "CIT >= 22%",
            population == "cit_less_stat" ~ "0 < CIT < 22%",
            population == "treat_a" ~ "Treatment A",
            population == "treat_b" ~ "Treatment B",
            population == "treat_c" ~ "Treatment C"
          ),
          population = factor(population,
                              levels = c("All firms",
                                         "CIT >= 22%",
                                         "Treatment A",
                                         "Treatment B",
                                         "Treatment C"))
          ) ) %>% 
    map(~ .x %>% split(.$population)) %>% 
    transpose()
  
  lista <- map(.x = elemnts,
               ~{ 
                 
                 tabla <- map(.x,select,-population)
                 
                 extract_broom(tidy_model = tabla[[1]],
                               glance_model = tabla[[2]],
                               type = tipo)
               })
  
  screenreg(lista)

}

# make_multi_model(datos,
#                  gdatos,
#                  "Event study assets weighted",
#                  "lm")

# make_multi_model(model_df_0,
#                  model_df_0_p,
#                  "Event study",
#                  "es")

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


# coef_plot_plus(model_df,
#                title_plot  = str_c("Event Study Design: ",params$var_lab),
#                subtitle_plot  = "Unsaturated model with firm clustered standard errors",
#                type = "es")
