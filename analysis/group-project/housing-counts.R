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

# Manually Add Row



ds1 <- ds0 %>% 
  add_row(
   CoC                         = "NC-505"              
   ,`Total Units for Households with Children` = 59
   ,`Total Beds for Households with Children`  = 539
   ,`Total Year-Round Beds`                    = 1323
   ,year                                       = "2020"
  )






# ---- tweak-data --------------------------------------------------------------

ds <- ds1 %>% 
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
  scale_x_continuous(breaks = seq(2007,2020,2)) +
  labs(
    title     = "Total Beds Compared to Family Beds in Charlotte - Mecklenburg"
    ,subtitle = "2007 - 2020"
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



# ---- modeling ----------------------------------------------------------------


ds_modeling <- ds %>% 
  mutate(
    # let us center the variable year, for easier estimation
    # now `year` will assume meaning `year since 2006`, but I still call it `year`
    # to to have a more succinct variable name
    year   = year - 2007 # for easier interpretation, assume meaning of `number of years since 2007`
    ,year2  = year^2 # quadratic term, helps model trajectories with one inflection
    , year3 = year^3 # cubic term, helps model trajectories with two inflections
  ) %>% 
  select(total_beds_for_households_with_children,total_year_round_beds, year, year2, year3)

model1 <- lm(total_beds_for_households_with_children ~ year + year2 + year3, data = ds_modeling)
summary(model1) # standard model summary, notice that quadratic and cubic terms an not significant
# however, removing them from the model is detrimental
lm(total_beds_for_households_with_children ~ year, data = ds_modeling) %>% broom::glance()
lm(total_beds_for_households_with_children ~ year + year2, data = ds_modeling) %>% broom::glance()
lm(total_beds_for_households_with_children ~ year + year2 + year3, data = ds_modeling) %>% broom::glance()
# notice that R square increases in each next, but DESCREASES in the third (penalized for model complexity)
# basically, it means that adding one more predictor to the model (cubic term) did not deliver
# increase in fit expected from this increase in model complexity (i.e. number of parameters)
anova(model1, test="Chisq") # tests significance of each sequentially added predictor
# notice that adding quadratic terms improves the model drastically, but cubic does not
# However, I still think you need to keep the quadratic term to account for the
# recovery inflection (2017 - 2019), it's just too small to be picked up mathematically

broom::augment(model1) # gets predicted values (+ more), but not extrapolated values

# build the data set with ranges of predictors you want to extrapolate to
d_predictors <- tibble::tibble(
  "year" = 0:16 # now we include 2020, which in not in the original data
  ,year2 = year^2
  ,year3 = year^3
)
# let's generate predictions from the model, including the extrapolated value (year == 13)
d_predictors$predicted <- predict(model1, newdata = d_predictors)
d_model1_results <- d_predictors %>% left_join(
  ds_modeling %>% select(year, total_beds_for_households_with_children)
  ,by = "year"
)
d_model1_results # notice we don't have observed data for year == 13

d_model1_results %>% 
  mutate(
    year = year + 2007 # re-center back for graphing
  ) %>% 
  { # very handy indeed!
    ggplot(.,aes(x=year, y = total_beds_for_households_with_children))+
      geom_point()+
      geom_line()+
      geom_point(aes(y=predicted), color = 'red')+
      geom_line(aes(y=predicted), color = 'red')+
      geom_point(aes(y=predicted), shape = 21, size =4, color = "blue", data = . %>% filter(year==2021))+
      geom_text(aes(y=predicted,label=round(predicted,2)),vjust=-1.5, data = . %>% filter(year==2021))+
      scale_x_continuous(breaks = seq(2007,2020,2)) 
  }
# We added the predicted value, but it is only as good as the model
# It appears that this recovery that seemingly started in 2017 is too small
# to register with the model yet
# This is the predicton of the "best fitted line"


# ---- write-data --------------------------------------------------------------


ds %>% readr::write_csv("./data-public/derived/nc-505-housing-counts.csv")





