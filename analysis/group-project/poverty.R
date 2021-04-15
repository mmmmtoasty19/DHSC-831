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

ds0 <- readr::read_csv("./data-public/raw/Poverty by Age and Gender.csv")


# ---- tweak-data --------------------------------------------------------------

ds <- ds0 %>% 
  janitor::clean_names() %>% 
  select(age, gender, year, share)

# ---- graph-1 -----------------------------------------------------------------

select_age <- c(
  "18 to 24 Years"  
  ,"25 to 34 Years" 
  ,"35 to 44 Years" 
  ,"45 to 54 Years"  
  ,"55 to 64 Years" 
  ,"65 to 74 Years" 
  ,"75 Years & Over"
)


g1 <- ds %>% 
  filter(age %in% select_age) %>% 
  ggplot(aes(x = year, y = share, group = age, color = age)) +
  geom_line() +
  geom_point() +
  facet_wrap(gender ~ . ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    legend.position = "bottom"
    ,plot.title     = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title     = "Poverty by Age and Gender in Charlotte - Mecklenburg"
    ,subtitle = "2013 - 2018"
    ,x        = NULL
    ,y        = NULL
    ,color    = NULL
  )


g1 %>% quick_save("poverty_by_age_gender")
