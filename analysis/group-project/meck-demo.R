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
if(!file.exists(prints_folder)){
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

ds_race_raw <- readr::read_csv("data-public/raw/Race and Ethnicity.csv")

# ---- tweak-data --------------------------------------------------------------

race_factors <- c(
  "White"  = "White Alone"                                                               
  ,"Black" = "Black or African American Alone"                 
  ,"Other" = "American Indian & Alaska Native Alone"           
  ,"Other" = "Asian Alone"                                     
  ,"Other" = "Native Hawaiian & Other Pacific Islander Alone"  
  ,"Other" = "Some Other Race Alone"                           
  ,"Other" = "Two or More Races"                              
)

ethnicity_factor <- c(
  "Non-Hispanic" =  "Not Hispanic or Latino"
  ,"Hispanic"    =  "Hispanic or Latino" 
)


ds_race <- ds_race_raw %>% 
  mutate(across(c("Race","Ethnicity"), as.factor)
         ,across(Race, ~fct_recode(.,!!!race_factors))
         ,across(Ethnicity, ~fct_recode(., !!!ethnicity_factor))) %>% 
  select(Race, Ethnicity, Year, Population) %>% 
  group_by(Year, Race, 
           Ethnicity
           ) %>% 
  summarise(
    across(Population, sum)
    ,.groups = "keep"
  ) %>% ungroup()
  # unite("race_ethnicity", Race:Ethnicity, sep = "_", remove = FALSE) 
  
ds_hispanic <- ds_race %>%   
  filter(Ethnicity == "Hispanic") %>% 
  group_by(Year, Ethnicity) %>% 
  summarise(
    across(Population, sum)
    ,.groups = "keep"
  ) %>% 
  mutate(
    Race = "Hispanic"
  ) %>%  ungroup()

ds_total_pop <- ds_race %>% 
  group_by(Year) %>% 
  summarise(
    total_pop = sum(Population)
  ) %>% ungroup()

ds_race_total <- ds_race %>% 
  filter(Ethnicity == "Non-Hispanic") %>% 
  bind_rows(ds_hispanic) %>% 
  left_join(ds_total_pop) %>%
  mutate(
    pop_pct = round((Population/total_pop) * 100,2)
  )


# ---- graph-1 -----------------------------------------------------------------

g1 <- ds_race_total %>% 
  mutate(
    across(Race, ~factor(., levels = c("White", "Black", "Other", "Hispanic")))
  ) %>% 
  ggplot(aes(x = Year, y = pop_pct, group = Race,fill = Race )) +
  geom_col(color = "black") +
  geom_text(aes(label = pop_pct),color = "black",  position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")

g1
  





