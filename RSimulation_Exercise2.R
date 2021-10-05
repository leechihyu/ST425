library(tidyverse)

#Define a function to generate n times tossing
tossing <- function(n){
  return(ceiling(runif(n, 0, 6)))
}


Prob_A_B_AB <- function(n, A, B){
  #assign tossing result to a local variable
  n_times_tossing <- tossing(n)
  
  Prob_A <- sum(n_times_tossing %in% A)/n
  
  Prob_B <- sum(n_times_tossing %in% B)/n
  
  Prob_AB <- sum((n_times_tossing %in% intersect(A, B)))/n
  
  return(c(Prob_A, Prob_B, Prob_AB))
}

Diff_Between_A_B_AB <- function(n, A = c(2,4,6), B = c(1,2,3,4)){
  Prob <- Prob_A_B_AB(n, A, B)
  ProbA <- Prob[1]
  ProbB <- Prob[2]
  ProbAB <- Prob[3]
  Diff <- ProbA * ProbB - ProbAB
  return(Diff)
}

#Default setting is A = {2,4,6}, B = {1,2,3,4}
diff1 <- replicate(10000, Diff_Between_A_B_AB(1000))
hist(diff1)

#Let A > 2, that A = {3,4,5,6}, and B > 4, that B = {5,6}.
#A and B are not independent.
A = c(3,4,5,6)
B = c(5,6)
diff2 <- replicate(10000, Diff_Between_A_B_AB(1000, A, B))
hist(diff2)

#Let A is odd, that A = {1,3,5}, and B is even, that B = {2,4,6}.
#A and B are not independent.
A = c(1,3,5)
B = c(2,4,6)
diff3 <- replicate(10000, Diff_Between_A_B_AB(1000, A, B))
hist(diff3)

c(
  Independent_Events = diff1 %>% mean(),
  Dependent_Events_I = diff2 %>% mean(),
  Dependent_Events_II = diff3 %>% mean()
)
