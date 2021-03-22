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

import_data <- function(path){
  
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


ds0 <- import_data(data_state_path)

ds_nc <- bind_rows(ds0)







# ---- tweak-data --------------------------------------------------------------


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
  ) +
  theme(
    plot.title     = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
  )

g1


# ---- graph-2 -----------------------------------------------------------------


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
