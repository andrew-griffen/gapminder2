library(pacman)
p_load(scales,tidyverse,griffen,gvlma,summarytools,jtools,janitor,hrbrthemes,readxl,stringi,ggrepel)
theme_set(theme_ipsum())

remove_kM <- function(x){
	case_when(grepl("k",x) ~ suppressWarnings(as.character(1000*as.numeric(stri_replace_all_regex(x,"k","")))),
			  grepl("M",x) ~ suppressWarnings(as.character(1000000*as.numeric(stri_replace_all_regex(x,"M","")))),
			  grepl("B",x) ~ suppressWarnings(as.character(1000000000*as.numeric(stri_replace_all_regex(x,"B","")))),
			  TRUE ~ x)
}

#child mortality
child_mortality <- read_csv("child_mortality_0_5.csv")
child_mortality %<>% pivot_longer(-country,names_to="year",values_to="mortality")
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

gapminder %<>% mutate(country = if_else(country=="Korea, Dem. Rep.","North Korea",country))
gapminder %<>% mutate(country = if_else(country=="Korea, Rep.","South Korea",country))

gapminder2 <- child_mortality %>% full_join(gdp, by = join_by(country, year)) %>% full_join(pop, by = join_by(country, year)) %>% full_join(lex, by = join_by(country, year))


gapminder2 %<>% mutate(gdp = gdpPercap*pop)
gapminder2 %<>% mutate(country = if_else(country=="USA","United States",country))
gapminder2 %<>% mutate(country = if_else(country=="UK","United Kingdom",country))

#need to add continents
continents = distinct(select(gapminder,continent,country))
continents %<>% mutate(across(everything(),as.character))


# continents %>% anti_join(distinct(select(gapminder2,country)))
# distinct(select(gapminder2,country)) %>% anti_join(continents)
# unmatched = distinct(select(gapminder2,country)) %>% anti_join(continents) %>% pull
# gapminder2 %<>% filter(!country %in% unmatched)
# gapminder2 %<>% left_join(continents)
gapminder2 <- filter(gapminder2,year>=1923 & year<=2023)

# gapminder2 %<>% mutate(country = factor(country))
# gapminder2 %<>% mutate(country = fct_reorder(country,))

country_levels <- gapminder2 %>% filter(year==max(year)) %>% arrange(-gdpPercap) %>% pull(country)

gapminder2 %<>% mutate(country = factor(country,levels = country_levels))

stop("here")

# ggplot(filter(gapminder2,year==1923),
# aes(x = log(gdpPercap) , y = lifeExp, color=country, size=pop)) +
# geom_point(show.legend = FALSE) +
# scale_colour_manual(values = country_colors) +
# labs(x = "log GDP per capita", y = "Life expectancy") +
# facet_wrap(. ~ continent)















