##################################################
# File to create rich poor mortality tables from
# data used in previous paper
#
# Author: Miqdad Asaria
# Date: August 2017
##################################################

library(tidyverse)

load("lifetable_and_cost_table.RData")

#Delete quintiles 2 3 4
lifetable_and_cost_table$lifetable<-subset(lifetable_and_cost_table$lifetable,IMD_QUINTILE>4|IMD_QUINTILE<2)
#Run assuming rich are top quintile, poor are bottom quintile
rich_poor_mortality_probs = lifetable_and_cost_table$lifetable %>%
  group_by(MIN_AGE, MAX_AGE, SEX, RICH=IMD_QUINTILE>4) %>% 
  dplyr::summarise(death_total=sum(DEATHS), pop_total=sum(POPULATION)) %>% 
  mutate(prob_mort = death_total/pop_total, MALE = SEX=="M") %>%
  ungroup()


rich_poor_mortality_probs[rich_poor_mortality_probs$MAX_AGE==85,"MAX_AGE"]=200

probs = rich_poor_mortality_probs

while(TRUE){
  probs = probs %>% filter(MAX_AGE>MIN_AGE) %>% mutate(MIN_AGE=MIN_AGE+1)
  if(nrow(probs)>0){
    rich_poor_mortality_probs = bind_rows(rich_poor_mortality_probs,probs)
  } else {
    break
  }
}

mortality_probs = rich_poor_mortality_probs %>% select(AGE=MIN_AGE,MALE,RICH,PROB_MORT=prob_mort) %>% arrange(AGE,MALE,RICH)

get_survival_probability = function(prob_mort){
  survival = vector("numeric",length(prob_mort))
  survival[1] = 1
  for (i in 1:(length(prob_mort)-1)) {
    survival[i+1] = survival[i]*(1-prob_mort[i])
  }
  return(survival)
}

monte_carlo_integral = function(curve, a, b, n=10){
  set.seed(123)
  return(round(mean(curve[round(runif(n, a+1, b+1))])*(b-a),1))
}

life_expectancy = tibble(le=monte_carlo_integral(get_survival_probability((filter(mortality_probs, RICH==TRUE & MALE==TRUE)%>%select(PROB_MORT))$PROB_MORT),
                     0,200,10000000),MALE=TRUE,RICH=TRUE)
life_expectancy = life_expectancy %>% add_row(le=monte_carlo_integral(get_survival_probability((filter(mortality_probs, RICH==FALSE & MALE==TRUE)%>%select(PROB_MORT))$PROB_MORT),
                     0,200,10000000),MALE=TRUE,RICH=FALSE)
life_expectancy = life_expectancy %>% add_row(le=monte_carlo_integral(get_survival_probability((filter(mortality_probs, RICH==TRUE & MALE==FALSE)%>%select(PROB_MORT))$PROB_MORT),
                     0,200,10000000),MALE=FALSE,RICH=TRUE)
life_expectancy = life_expectancy %>% add_row(le=monte_carlo_integral(get_survival_probability((filter(mortality_probs, RICH==FALSE & MALE==FALSE)%>%select(PROB_MORT))$PROB_MORT),
                     0,200,10000000),MALE=FALSE,RICH=FALSE)

save(mortality_probs,life_expectancy,file="mortality_data.RData")
