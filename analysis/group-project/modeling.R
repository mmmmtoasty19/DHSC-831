rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(tsibble)
library(fable)

# ---- load-sources ------------------------------------------------------------

# ---- declare-globals ---------------------------------------------------------

# custom function for HTML tables
neat <- function(x, output_format = "html"){
  # knitr.table.format = output_format
  if(output_format == "pandoc"){
    x_t <- knitr::kable(x, format = "pandoc")
  }else{
    x_t <- x %>%
      # x %>%
      # neat() %>%
      knitr::kable(format=output_format) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed","responsive"),
        # bootstrap_options = c( "condensed"),
        full_width = F,
        position = "left"
      )
  }
  return(x_t)
}
# Note: when printing to Word or PDF use `neat(output_format =  "pandoc")`


prints_folder <- "./analysis/group-project/prints/"
if(!dir.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}



ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
    )
)
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".jpg"),
    plot     = g,
    device   = "jpg",
    path     = prints_folder,
    # width    = width,
    # height   = height,
    # units = "cm",
    dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}
# ---- load-data ---------------------------------------------------------------

ds0 <- readr::read_csv("./data-public/derived/nc-505-housing-counts.csv")

# ---- tweak-data --------------------------------------------------------------

ds1 <- ds0 %>% 
  as_tsibble(index = year) %>% 
  select(-CoC) %>% 
  janitor::clean_names()

# ---- modeling ----------------------------------------------------------------

fit_trends <- ds1 %>% 
  model(
    # linear = TSLM(total_beds_for_households_with_children ~ trend()),
    # exponential = TSLM(log(total_beds_for_households_with_children) ~ trend()),
    piecewise = TSLM(total_beds_for_households_with_children ~ trend(knots = c(2010, 2012, 2017)))
  )

fc_trends <- fit_trends %>% forecast(h = 5)



ds1 %>% 
  autoplot(total_beds_for_households_with_children) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model), show.legend = FALSE) +
  autolayer(fc_trends, alpha = 0.5, level = 50, color = "#1B9E77", data = ds1, show_gap = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(2005, 2025, 2)) +
  theme(
    legend.position = "none"
  ) + 
  labs(
    x = NULL
    ,y = NULL
  )

