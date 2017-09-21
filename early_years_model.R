source("Person.R")

library(ggplot2)

load("mortality_data.RData")

run_model = function(male, rich, mortality_data, life_expectancy, number_of_patients){
  mortality = filter(mortality_data, RICH==rich & MALE==male)%>%select(AGE,PROB_MORT)
  le = filter(life_expectancy, RICH==rich & MALE==male)$le
  results = list()
  
  for(i in 1:number_of_patients){
    results[[i]] = Person$new(male, rich, mortality, le)$live_life()$get_life_history() %>% 
      mutate(cum_health = cumsum(health), cum_consumption=cumsum(consumption))
  }
  
  return(results)
}

set.seed(1234)
n=10
rich_men = run_model(male=TRUE, rich=TRUE, mortality_data=mortality_probs, life_expectancy=life_expectancy, number_of_patients=n)
poor_men = run_model(male=TRUE, rich=FALSE, mortality_data=mortality_probs, life_expectancy=life_expectancy, number_of_patients=n)
rich_women = run_model(male=FALSE, rich=TRUE, mortality_data=mortality_probs, life_expectancy=life_expectancy, number_of_patients=n)
poor_women = run_model(male=FALSE, rich=FALSE, mortality_data=mortality_probs, life_expectancy=life_expectancy, number_of_patients=n)

sample_person = poor_men[[1]]
qplot(age,wealth,data=sample_person)
qplot(age,income,data=sample_person)
qplot(age,consumption,data=sample_person)

qplot(age,social_emotional_capital,data=sample_person,ylim=c(0,1))
write.csv(sample_person,file="sample_person.csv")
# 
# miqdad_male = TRUE
# miqdad_rich = FALSE
# miqdad = Person$new(male=miqdad_male, 
#                     rich=miqdad_rich, 
#                     mortality_probs=filter(mortality_probs, RICH==miqdad_rich & MALE==miqdad_male)%>%select(AGE,PROB_MORT),
#                     life_expectancy=filter(life_expectancy, RICH==miqdad_rich & MALE==miqdad_male)$le)
# 
# miqdad$live_life()
# 
# miqdad_life_history = mutate(miqdad$get_life_history(), cum_health = cumsum(health), cum_consumption=cumsum(consumption))
#
#View(miqdad_life_history)
#
#qplot(age,cognitive_capital,data=miqdad_life_history,ylim=c(0,1))
