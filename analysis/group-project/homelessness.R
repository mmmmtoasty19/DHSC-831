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

data_state_path <- "data-public/raw/2007-2019-Point-in-Time-Estimates-by-state.xlsx"
years <- as.character(2007:2019)

import_state_data <- function(path){
  
  output_list <- list()
  for(i in years){
    ds <- readxl::read_xlsx(data_state_path, sheet = i) %>% 
      filter(State == "NC") %>% 
      select(State, contains("Overall Homeless,")) %>% 
      rename_with(~str_remove(.,"Overall Homeless,")) %>% 
      pivot_longer(-State, names_to = "year")
    
    output_list[[i]] <- ds
    
  }
  return(output_list)
} 


ds0 <- import_state_data(data_state_path)

ds_nc <- bind_rows(ds0)


data_coc_path <- "data-public/raw/2007-2019-Point-in-Time-Estimates-by-CoC.xlsx"


col_select <- c(
  "Overall Homeless - Hispanic/Latino"
  ,"Overall Homeless - White"
  ,"Overall Homeless - Black or African American"
  ,"Overall Homeless - Asian"
  ,"Overall Homeless - American Indian or Alaska Native"
  ,"Overall Homeless - Native Hawaiian or Other Pacific Islander"
)


import_coc_data <- function(path){
  output_list <- list()
  for(i in years){
    ds <- readxl::read_xlsx(path, sheet = i) %>% 
      filter(`CoC Number` == "NC-505") %>% 
      select(`CoC Number`, contains("Overall Homeless,"), matches(col_select)) %>% 
      rename_with(~str_remove(.,"Overall Homeless,")) %>% 
      pivot_longer(-`CoC Number`, names_to = "year")
    
    output_list[[i]] <- ds
    
  }
  return(output_list)
  
}

ds0_coc <- import_coc_data(data_coc_path)

ds_coc_raw <- bind_rows(ds0_coc)




col_select_sex <- c(
  "Sheltered Total Homeless - Female"
  ,"Sheltered Total Homeless - Male"
  ,"Unsheltered Homeless - Female"
  ,"Unsheltered Homeless - Male"
)


years_abv <- as.character(2015:2019)

import_coc_sexes_data <- function(path){
  output_list <- list()
  for(i in years_abv){
    ds <- readxl::read_xlsx(path, sheet = i) %>% 
      filter(`CoC Number` == "NC-505") %>% 
      select(`CoC Number`, matches(col_select_sex)) %>% 
      # rename_with(~str_remove(.,"Overall Homeless,")) %>% 
      pivot_longer(-`CoC Number`, names_to = "year")
    
    output_list[[i]] <- ds
    
  }
  return(output_list)
}


ds0_sex <- import_coc_sexes_data(data_coc_path)

ds_coc_sex_raw <- bind_rows(ds0_sex)


# ---- tweak-data --------------------------------------------------------------

ds_coc <- ds_coc_raw %>% 
  mutate(across(year, ~str_remove(.,"Overall Homeless - "))) %>% 
  separate(year, into = c("race", "year"), sep = ",", fill = "left") %>% 
  replace_na(list(race = "Total")) 


ds_sex <- ds_coc_sex_raw %>% 
  separate(year, into = c("type", "sex"), sep = "-") %>% 
  separate(sex , into = c("sex", "year"), sep = ",")

# ---- table-1 -----------------------------------------------------------------

# ---- graph-1 -----------------------------------------------------------------

g1 <- ds_nc %>% 
  mutate(across(year, as.numeric)
         ,label = ifelse(year == max(year), value, "")) %>% 
  ggplot(aes(x = year, y = value, group = State)) +
  geom_point(size = 2,  color = "#D95F02") +
  geom_line( color = "#D95F02") +
  ggrepel::geom_text_repel(
    aes(
      label = label
      )
    ,vjust              = -1
    ,min.segment.length = 5
    ) +
  scale_x_continuous(breaks = seq(2007,2019,3)) +
  labs(
    title     = "Total People Experiencing Homelessness in North Carolina"
    ,subtitle = "2007 - 2019"
    ,x        = NULL
    ,y        = NULL
    ,caption  = "Data from United States Department of Housing and Urban Development. \n Retrieved from https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/"
  ) +
  theme(
    plot.title     = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
  )

g1 %>% quick_save("nc_total_homeless"
                  # , height = 5.5, width = 7.1
                  )
 

# ---- graph-2 -----------------------------------------------------------------

g2 <- ds_coc %>% 
  mutate(across(year, as.numeric)
         ,label = ifelse(year == max(year), value, "")) %>% 
  filter(race == "Total") %>% 
  ggplot(aes(x = year, y = value, group = race)) +
  geom_point(size = 2,  color = "#D95F02") +
  geom_line( color = "#D95F02") +
  ggrepel::geom_text_repel(
    aes(
      label = label
    )
    ,vjust              = -1
    ,min.segment.length = 5
  ) +
  scale_x_continuous(breaks = seq(2007,2019,3)) +
  labs(
    title     = "Total People Experiencing Homelessness in Charlotte - Mecklenburg"
    ,subtitle = "2007 - 2019"
    ,x        = NULL
    ,y        = NULL
    ,caption  = "Data from United States Department of Housing and Urban Development. \n Retrieved from https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/"
  ) +
  theme(
    plot.title     = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
  )


g2 %>% quick_save("meck_total_homeless")
  
  
# ---- graph-3 -----------------------------------------------------------------

fct_levels <- c(
  "Black"   = "Black or African American"
  ,"Other"  = "Asian"
  ,"Other"  = "American Indian or Alaska Native"
  ,"Other"  = "Native Hawaiian or Other Pacific Islander"
)


g3 <- ds_coc %>% 
  mutate(across(year, as.numeric)
         ,across(race, as.factor)
         ,across(race ,~fct_recode(., !!!fct_levels))
         ,across(race, ~fct_relevel(., "Other", after = Inf))
         ,label = ifelse(
           year == max(year) & race %in% c("White", "Black"), value, "")
         ) %>% 
  filter(race != "Total") %>% 
  ggplot(aes(x = year, y = value, group = race, color = race)) +
  geom_point(size = 2) +
  geom_line(aes(linetype = race)) +
  ggrepel::geom_text_repel(
    aes(
      label = label
    )
    ,vjust              = -1
    ,min.segment.length = 5
    ,show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(2015,2019,2)) +
  labs(
    title     = "People Experiencing Homelessness in Charlotte - Mecklenburg by Race"
    ,subtitle = "2015 - 2019"
    ,x        = NULL
    ,y        = NULL
    ,color    = NULL
    ,linetype = NULL
    ,caption  = "Data from United States Department of Housing and Urban Development. \n Retrieved from https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title     = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
    ,legend.position = "bottom"
  ) 


g3
g3 %>% quick_save("coc_by_race")


# ---- graph-4 -----------------------------------------------------------------

g4 <- ds_sex %>% 
  mutate(across(year, as.numeric)) %>% 
  ggplot(aes(x = year, y = value, group = interaction(type,sex), color = sex)) +
  geom_point(size = 2) +
  geom_line(aes(linetype = type)) +
  scale_x_continuous(breaks = seq(2015,2019,2)) +
  labs(
    title     = "People Experiencing Homelessness in Charlotte - Mecklenburg by Gender"
    ,subtitle = "2015 - 2019"
    ,x        = NULL
    ,y        = NULL
    ,color    = NULL
    ,linetype = NULL
    ,caption  = "Data from United States Department of Housing and Urban Development. \n Retrieved from https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title     = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
    ,legend.position = "bottom"
  ) 

g4 %>% quick_save("coc_by_gender")





# ----- publish ----------------------------------------------------------------
path <- "./analysis/report/report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
