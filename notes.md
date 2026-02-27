Some difficulty getting it to run out of the box 

# packages 

# install.packages("devtools")
```r
# two packages not on cran 
devtools::install_github("anguswg-ucsb/cdssr")
devtools::install_github("rossyndicate/ross.wq.tools")
# Load (and install if missing) all required packages
pacman::p_load(
  ross.wq.tools,
  zoo, padr,
  janitor, broom, here, glue,
  stats, RcppRoll, trend, xgboost,
  sf, leaflet,
  ggpubr, ggthemes, scales, plotly, ggpmisc,
  rvest, httr, httr2, cdssr, yaml,
  devtools,
  shiny, shinymanager, shinycssloaders, shinyTime, 
  bslib, shinyWidgets, shinydashboard, htmltools,
  tidyverse, readr, DT, purrr, data.table, arrow
)

# Also needed  
install.packages("ggridges")
```


