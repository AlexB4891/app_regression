# Ecuador Transparency Regressions Report

Este proyecto es una recopilación de resultados de regresiones realizadas en datos de transparencia de Ecuador. Se han realizado varios tipos de regresiones, incluyendo diferencias en diferencias, diferencias en diferencias con efectos fijos, event study, y event study con efectos fijos.

## Grupos de Estudio

Los resultados se presentan para tres grupos:

- Joint groups (T-Maj + T-Min vs C-Maj + C-Min)
- Majors group (T-Maj vs C-Maj)
- Minors group  (T-Min vs C-Min)

Cada grupo ha sido sometido a los cuatro tipos de regresiones:

- Diff-in-diff design (unsaturated model)

$$Y_{it} = \beta_0 \times Treatment_{it} + \beta_1 \times Post_{it} + \beta_2 \times Treatment_{it} \times Post_{it} + \epsilon$$ 

- Diff-in-diff design (with fixed effects)

$$Y_{it} = \beta_0 + \beta_1 \times Treatment_{it} + \beta_2 \times Post_{it} + \beta_3 \times Treatment_{it} \times Post_{it} + \alpha_i + \gamma_t + \epsilon_{it}$$

- Event Study design (unsaturated model)

$$Y_{it} = \beta_0 + \sum_{i \neq 2014} \beta_i \times Time_{i} + \sum_{i \neq 2014} \beta_i \times Time_{i} \times Treatment_{it} + \epsilon_{it}$$

- Event Study design (fixed effects model)

$$Y_{it} = \beta_0 + \sum_{i \neq 2014} \beta_i \times Time_{i} + \sum_{i \neq 2014} \beta_i \times Time_{i} \times Treatment_{it} + \alpha_i + \epsilon_{it}$$



## Variables de Estudio


| Label                                             | BDD variable          |
|---------------------------------------------------|-----------------------|
| Tax haven participation                           | pff_p                 |
| Foreign participation                             | ext_p                 |
| Log amount of assets attributable to TH           | log_assets_attr_pff   |
| Log amount of assets attributable to non TH       | log_assets_attr_ext   |
| Log(CIT liability)                                | log_cit_liability     |
| Log(Profits)                                      | log_utility           |
| Log(Taxable profits)                              | log_taxable_profits   |
| Prominent participation in group                  | prominent             |
| Amount of assets atributables in dominant group   | log_assets_prominent  |

## Uso

Utiliza el menú desplegable para seleccionar la variable que deseas explorar.

## Despliegue

Este proyecto se ha desplegado en [https://alexb4891.github.io/app_regression/] para facilitar su visualización y uso.

## Contribuciones

Las contribuciones son bienvenidas. Por favor, abre un issue para discutir lo que te gustaría cambiar o añadir.

## Licencia

[MIT](https://choosealicense.com/licenses/mit/)