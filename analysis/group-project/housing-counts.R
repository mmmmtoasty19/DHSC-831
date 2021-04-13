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

housing_count_path <- "data-public/raw/2007-2019-Housing-Inventory-Count-by-CoC.xlsx"



col_select <- c(
  "Total Year-Round Beds (ES, TH, SH)"
  ,"Total Units for Households with Children (ES, TH, SH)"
  ,"Total Beds for Households with Children (ES, TH, SH)"
  ,"Total Units for Households with Children (ES,TH)"
  ,"Total Beds for Households with Children (ES,TH)"
  ,"Total Year-Round Beds (ES,TH)"
  ,"Total Year-Round Beds (ES,TH,RRH,SH)"
  ,"Total Units for Households with Children (ES,TH,RRH)"
  ,"Total Beds for Households with Children (ES,TH,RRH)"
  ,"Total Year-Round Beds (ES,TH,SH)"
)


years <- as.character(2007:2019)


output_list <- list()
for(i in years){
  ds <- readxl::read_xlsx(housing_count_path, sheet = i, skip = 1 ) %>% 
    rename_with(~str_remove(.," Number")) %>% 
    filter(CoC == "NC-505") %>% 
    select(CoC, contains(col_select)) %>% 
    mutate(
      year = i
    ) %>% 
    rename_with(~str_remove(.
              ," \\(ES, TH, SH\\)| \\(ES,TH\\)| \\(ES,TH,RRH,SH\\)| \\(ES,TH,RRH\\)| \\(ES,TH,SH\\)"))
    
  
  output_list[[i]] <- ds
  
}


ds0 <- bind_rows(output_list)






# ---- tweak-data --------------------------------------------------------------

ds <- ds0 %>% 
  select(-CoC) %>% 
  janitor::clean_names() %>% 
  relocate(year) %>% 
  mutate(
    pct_family_beds = round(
      (`total_beds_for_households_with_children`/`total_year_round_beds`) *100
      ,2
      )
    ,pct_total_change = round(
      (total_year_round_beds - lag(total_year_round_beds))/total_year_round_beds *100
      ,2 
    )
    ,pct_family_change = round(
      (total_beds_for_households_with_children - lag(
        total_beds_for_households_with_children))/
        total_beds_for_households_with_children * 100
       ,2
    )
    ,across(year, as.numeric)
  )



ds_long <- ds %>% 
  pivot_longer(-year)


# ---- graph-1 -----------------------------------------------------------------


g1 <- ds_long %>% 
  filter(
    name %in% c("total_beds_for_households_with_children", "total_year_round_beds")
    ) %>% 
  mutate(
    across(
      name, ~str_replace_all(., c(
        "total_beds_for_households_with_children" = "Family Beds"
        ,"total_year_round_beds" = "Total Beds"
      ))
    )
  ) %>% 
  ggplot(aes(x = year, y = value, group = name, color = name)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = seq(2007,2019,3)) +
  labs(
    title     = "Total Beds Compared to Family Beds in Charlotte - Mecklenburg"
    ,subtitle = "2007 - 2019"
    ,x        = NULL
    ,y        = NULL
    ,color    = NULL
    ,caption  = "Data from United States Department of Housing and Urban Development. \n Retrieved from https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title     = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
    ,legend.position = "bottom"
  ) 



g1






# ---- write-data --------------------------------------------------------------


ds %>% readr::write_csv("./data-public/derived/nc-505-housing-counts.csv")





