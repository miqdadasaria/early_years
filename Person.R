library(R6)
library(tidyverse)

Person = R6Class("Person", 
                 public = list(
                   initialize = function(male, rich, mortality_probs, life_expectancy) {
                     private$male = male
                     private$rich = rich
                     private$mortality_probs = mortality_probs
                     private$life_expectancy = life_expectancy
                     
                     private$life_history = tibble(age=0,
                                           life_stage = private$get_life_stage(0),
                                           cognitive_capital = if(rich) rbeta(1,6,3) else rbeta(1,6,6),
                                           social_emotional_capital = if(rich) rbeta(1,6,3) else rbeta(1,6,6),
                                           health_capital = if(rich) rbeta(1,6,3) else rbeta(1,6,6),
                                           wealth = if(rich) rbeta(1,60,20) else rbeta(1,30,40),
                                           good_state = private$get_state(TRUE, cognitive_capital, social_emotional_capital, health_capital, wealth),
                                           income = private$get_income(good_state,life_stage),
                                           health = private$get_health(health_capital, good_state, age),
                                           consumption = private$get_consumption(income, wealth, age, life_stage))
                     
                     return(invisible(self))
                   },
                   print = function(...) {
                     cat("Person: \n")
                     if(private$male){
                       cat(" Sex: Male\n")  
                     } else {
                       cat(" Sex: Female\n")
                     }
                     if(private$rich){
                       cat(" SES: Rich\n")
                     } else {
                       cat(" SES: Poor\n")
                     }
                     cat(" Current Status:\n")
                     glimpse(private$life_history)
                     return(invisible(self))
                   },
                   get_life_history = function(){
                     return(private$life_history)
                   },
                   live_life = function(){
                     previous_year = tail(private$life_history,1)
                     current_year = previous_year
                     while(current_year$life_stage != "dead" & current_year$age < 120){
                       current_year$age = previous_year$age + 1
                       if(runif(1)<filter(private$mortality_probs,AGE==current_year$age)$PROB_MORT){
                         current_year$life_stage = "dead"
                       } else {
                         current_year$life_stage = private$get_life_stage(current_year$age)
                         current_year$wealth = private$get_wealth(previous_year$wealth, previous_year$income, previous_year$consumption)
                         current_year$cognitive_capital = private$get_cognitive_capital(previous_year$good_state, previous_year$cognitive_capital, previous_year$social_emotional_capital, previous_year$health_capital, current_year$life_stage)
                         current_year$social_emotional_capital = private$get_social_emotional_capital(previous_year$good_state, previous_year$cognitive_capital, previous_year$social_emotional_capital, previous_year$health_capital, current_year$life_stage)
                         current_year$health_capital = private$get_health_capital(previous_year$good_state, previous_year$cognitive_capital, previous_year$social_emotional_capital, previous_year$health_capital)
                         current_year$good_state = private$get_state(previous_year$good_state, current_year$social_emotional_capital, current_year$cognitive_capital, current_year$health_capital, current_year$wealth)
                         current_year$income = private$get_income(current_year$good_state, current_year$life_stage)
                         current_year$health = private$get_health(current_year$health_capital, current_year$good_state, current_year$age)
                         current_year$consumption = private$get_consumption(current_year$income, current_year$wealth, current_year$age, current_year$life_stage)
                       }
                       private$life_history = private$life_history %>% bind_rows(current_year)
                       previous_year = current_year
                     }
                     return(invisible(self))
                   }
                 ),
                 private = list(
                   male = NA,
                   rich = NA,
                   mortality_probs = NULL,
                   life_expectancy = NA,
                   current_annual_consumption = NA,
                   life_history = NULL,
                   get_life_stage = function(age){
                     return(case_when(
                       age < 18 ~ "early_years",
                       age >= 18 & age <= 60 ~ "productive_years",
                       age > 60 ~ "retirement"
                     ))
                   },
                   get_income = function(is_good_state, life_stage){
                     if(life_stage != "productive_years"){
                       return(0)
                     } else {
                       if(is_good_state){
                         return(1/12)
                       } else {
                         return(1/36)
                       }
                     }
                   },
                   get_consumption = function(income, wealth, age, life_stage){
                     if(age==0){
                       private$current_annual_consumption = wealth/18
                     }else if(age==61){
                       private$current_annual_consumption = wealth/(private$life_expectancy - 61)
                     }
                     
                     if(life_stage=="productive_years"){
                       return(0.7*income)
                     }else{
                       return(private$current_annual_consumption)
                     }
                   },
                   get_wealth = function(wealth, income, consumption){
                     new_wealth = wealth + income - consumption
                     return(new_wealth)
                   },
                   get_health = function(health_capital, good_state, age){
                     if(good_state){
                       state_weight = 1
                     }else{
                       state_weight = 0.9
                     }
                     new_health = state_weight * ((3+health_capital)/4 - age/200)
                     return(new_health)
                   },
                   get_state = function(previous_good_state, social_emotional_capital, cognitive_capital, health_capital, wealth){
                     if(previous_good_state){
                       state_weight = 1
                     }else{
                       state_weight = 0.9
                     } 
                     good_state_probability = state_weight * mean(c(cognitive_capital,social_emotional_capital,health_capital,wealth))
                     good_state = runif(1) < good_state_probability
                     return(good_state)
                   },
                   get_cognitive_capital = function(previous_good_state, previous_cognitive_capital, previous_social_emotional_capital, previous_health_capital, life_stage){
                     if(life_stage != "early_years"){
                       return(previous_cognitive_capital)
                     }else{
                       if(previous_good_state){
                         cognitive_capital = min(1, previous_cognitive_capital * (previous_social_emotional_capital + previous_health_capital  + 4)/5)
                       }else{
                         cognitive_capital = min(1, previous_cognitive_capital * (previous_social_emotional_capital + previous_health_capital  + 3)/5)
                       }
                       return(cognitive_capital)
                     } 
                   },                   
                   get_social_emotional_capital = function(previous_good_state, previous_cognitive_capital, previous_social_emotional_capital, previous_health_capital, life_stage){
                     if(life_stage != "early_years"){
                       return(previous_social_emotional_capital)
                     }else{
                       if(previous_good_state){
                         social_emotional_capital = min(1, previous_social_emotional_capital * (previous_cognitive_capital + previous_health_capital + 4)/5)
                       }else{
                         social_emotional_capital = min(1, previous_social_emotional_capital * (previous_cognitive_capital + previous_health_capital + 3)/5)
                       }
                       return(social_emotional_capital)
                     } 
                   },                   
                   get_health_capital = function(previous_good_state, previous_cognitive_capital, previous_social_emotional_capital, previous_health_capital){
                       if(previous_good_state){
                         health_capital = min(1, previous_health_capital * (previous_cognitive_capital + previous_social_emotional_capital + 4)/5)
                       }else{
                         health_capital = min(1,previous_health_capital * (previous_cognitive_capital + previous_social_emotional_capital + 3)/5)
                       }
                       return(health_capital)
                   }
                 )
)

