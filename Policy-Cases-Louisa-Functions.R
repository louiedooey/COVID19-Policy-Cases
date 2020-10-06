library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(directlabels)
library(RColorBrewer)

policy <- read.csv("acaps_policy.csv")
covid <- read.csv(url("https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"))


# change variable names
covid_2 <- covid %>% 
  rename("COUNTRY" = "Country.Region") %>% 
  select(COUNTRY, Date, Confirmed, Recovered, Deaths)

# format dates
covid_2$Date <- as.Date(covid_2$Date)
policy$DATE_IMPLEMENTED <- as.Date(policy$DATE_IMPLEMENTED, "%d/%m/%Y")



# ------------------- POLICY DATASET FUNCTION ------------------ # 

policy_dataset <- function(policy_df, pol_cat, pol_type, color_cntry) {
  # policy dataframe, column: category or measure, policy type 
  
  policy_df %>%
    select(COUNTRY, CATEGORY, MEASURE, DATE_IMPLEMENTED) %>%
    filter(pol_cat == pol_type) %>%  # filter policy category or measure 
    left_join(covid_2, by = c("COUNTRY", "DATE_IMPLEMENTED" = "Date")) # joining policy and confirmed
  
}


# ------------  COUNTRIES TO LKDOWN INT & COVID_INT FUNCTION ----------- #  

add_cty <- function(pol_choice, cntry, color_cntry) {
  
  # countries are added separately as they are plotted individually 
  
  pol_cty <- function (pol_choice, cntry) { 
    # policy choice, country, column label for Confirmed_Country
    
    pol_choice <- pol_choice %>% 
      filter(COUNTRY == cntry) %>%  # country chosen 
      drop_na %>% 
      rename(!!sym(paste("Con_", cntry, sep = "")) := Confirmed) %>% # assignment by reference := 
      rename(!!sym(paste("Imp_", cntry, sep = "")) := DATE_IMPLEMENTED) %>%
      rename(!!sym(paste("Msu_", cntry, sep = "")) := MEASURE) %>%
      rename(!!sym(paste("Rec_", cntry, sep = "")) := Recovered) %>%
      rename(!!sym(paste("Dea_", cntry, sep = "")) := Deaths)
    
  }
  covid_cty <- function(cntry) {
    
    covid_2 <- covid_2 %>% 
      filter(COUNTRY == cntry) %>% # country chosen 
      rename(!!sym(paste("Dat_", cntry, sep = "")) := Date) %>%
      rename(!!sym(paste("Con_cov_", cntry, sep = "")) := Confirmed) %>%
      rename(!!sym(paste("Rec_cov_", cntry, sep = "")) := Recovered) %>%
      rename(!!sym(paste("Dea_cov_", cntry, sep = "")) := Deaths)
    
  }
  
  # creating a policy dataset for the country
  pol_cntry <- pol_cty(pol_choice, cntry)
  
  # creating a covid dataset for the country  
  covid_cntry <- covid_cty(cntry)
  
  
  # adding selected country to the pol_int dataset 
  pol_int_add <- function(pol_cntry) {
    pol_int %>% left_join(pol_cntry, by = "COUNTRY")
  }
  pol_int <- pol_int_add(pol_cntry)
  
  # adding selected country to the covid_int dataset 
  covid_int_add <- function(covid_cntry) {
    covid_int %>% left_join(covid_cntry, by = "COUNTRY")
  }
  covid_int <- covid_int_add(covid_cntry)
  

  # ------------------ GRAPHING -------------------- #
  # plotting country's curve and policies
  g + geom_point(data = pol_int,
                 aes(x = !!sym(paste("Imp_", cntry, sep = "")),
                     y = !!sym(paste("Con_", cntry, sep = "")),
                     shape = !!sym(paste("Msu_", cntry, sep = ""))),
                     size = 4, 
                     na.rm = TRUE, 
                     color = color_cntry, 
                     alpha = 0.5) +
    geom_smooth(data = covid_int,
                aes(x = !!sym(paste("Dat_", cntry, sep = "")),
                    y = !!sym(paste("Con_cov_", cntry, sep = ""))),
                color = color_cntry)
  
}

####### ------------------------------------------------------ ########
# --------------- HERE IS WHERE YOU CHANGE STUFF -------------------- #
####### ------------------------------------------------------ ########

# ------------------ choosing a policy -------------------------- #

# 1. Either
table(policy$CATEGORY) # run this to see options 
# or 
table(policy$MEASURE) # run this to see options 

# 2. choose one or more of these options from there as "____" 


# POL_CHOICE : 
# Choosing Lockdown 
pol_lkdown <- policy_dataset(policy, policy$CATEGORY, "Lockdown")

# Choosing Social Distancing
# pol_sd <- policy_dataset(policy, policy$CATEGORY, "Social distancing")

# ------------------ choosing countries policy -------------------------- #

# run before using function 
pol_int <- pol_lkdown
# pol_int <- pol_sd

covid_int <- covid_2 

# ---------------- Graph Frame ------------------------- # 

# Also A Reset 
g <- ggplot() + 
  theme_economist_white() +
  xlab("") +
  ylab("Number of Cases\n\n") +
  labs(title = "Fig 1) COVID Lockdown Policies and Cases Worldwide", 
       subtitle = "\nSource: Johns Hopkins University (CSSE), ACAPS Govt Measures") + 
  theme(legend.position = "right")


# ------------- Plot graph function -------------------- # 

# See what Countries 
table(covid$Country.Region)

g <- add_cty(pol_lkdown, "Italy", "mediumturquoise")

g <- add_cty(pol_lkdown, "Spain", "gold")

g <- add_cty(pol_lkdown, "Brazil", "darkgreen")

g <- add_cty(pol_lkdown, "Poland", "deeppink3")

g <- add_cty(pol_lkdown, "Singapore", "tomato3")

g <- add_cty(pol_lkdown, "Taiwan*", "hotpink3")

g <- add_cty(pol_lkdown, "Sweden", "dodgerblue4") # no policies

g <- add_cty(pol_lkdown, "Qatar", "red4")

g <- add_cty(pol_lkdown, "Colombia", "salmon")

g <- add_cty(pol_lkdown, "China", "hotpink3")

g <- add_cty(pol_lkdown, "France", "royalblue3")

g <- add_cty(pol_lkdown, "India", "darkgreen")

g <- add_cty(pol_lkdown, "US", "navyblue") # No policies 

# g <- add_cty(pol_lkdown, "United Kingdom", "lightcyan4") # UK has issues

g 

# ---------

library(plotly)
ggplotly(g) # interactive 
