#library(MASS)
library(tidyverse)
library(magrittr)
library(stringi)

save_datasets = function(...){
  datasets_list <- lapply(eval(substitute(alist(...))),deparse)
  files = lapply(datasets_list,function(x) paste0("/home/andrew/Dropbox/R_packages/gapminder2/data/",x,".rda"))
  mapply(save, list = datasets_list, file = files)
  invisible(datasets_list)
}

remove_kM <- function(x){
  case_when(grepl("k",x) ~ suppressWarnings(as.character(1000*as.numeric(stri_replace_all_regex(x,"k","")))),
        grepl("M",x) ~ suppressWarnings(as.character(1000000*as.numeric(stri_replace_all_regex(x,"M","")))),
        grepl("B",x) ~ suppressWarnings(as.character(1000000000*as.numeric(stri_replace_all_regex(x,"B","")))),
        TRUE ~ x)
}

#child mortality
child_mortality <- read_csv("child_mortality_0_5.csv")
child_mortality %<>% pivot_longer(-country,names_to="year",values_to="chmort")
child_mortality %<>% mutate(year = as.integer(year))

#gdp and growth
gdp <- read_csv("gdp_pcap.csv")
gdp %<>% mutate(across(c(where(is.character),-country),~remove_kM(.)))
gdp %<>% mutate(across(c(where(is.character),-country),~as.numeric(.)))
gdp %<>% pivot_longer(-country,names_to="year",values_to="gdpPercap")
gdp %<>% mutate(year = as.integer(year))
gdp %<>% group_by(country) %>% mutate(growth = (gdpPercap - lag(gdpPercap))/lag(gdpPercap))
gdp %<>% ungroup

#population
pop <- read_csv("pop.csv")
pop %<>% mutate(across(c(where(is.character),-country),~remove_kM(.)))
pop %<>% mutate(across(c(where(is.character),-country),~as.numeric(.)))
pop %<>% pivot_longer(-country,names_to="year",values_to="pop")
pop %<>% mutate(year = as.integer(year))

#life expectancy
lex <- read_csv("lex.csv")
lex %<>% pivot_longer(-country,names_to="year",values_to="lifeExp")
lex %<>% mutate(year = as.integer(year))

# gapminder %<>% mutate(country = if_else(country=="Korea, Dem. Rep.","North Korea",country))
# gapminder %<>% mutate(country = if_else(country=="Korea, Rep.","South Korea",country))

# #need to add continents
# continents = distinct(select(gapminder,continent,country))
# continents %<>% mutate(across(everything(),as.character))

gapminder2 <- child_mortality %>%
              full_join(gdp, by = join_by(country, year)) %>%
              full_join(pop, by = join_by(country, year)) %>%
              full_join(lex, by = join_by(country, year))

gapminder2 %<>% select(country,year,gdpPercap,pop,gdp,growth,lifeExp,chmort)

save_datasets(gapminder2)




