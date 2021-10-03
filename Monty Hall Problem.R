'''
title: "Monty Hall Problem"
author: "Zhiyu Li"
date: "2021/10/3"
'''

#Step 1
#Create three doors behind one of which there is an award.
doors <- function(){
  door_value <- sample(c(T,F,F), 3)
  door <- c(d1 = door_value[1],
            d2 = door_value[2],
            d3 = door_value[3])
  return(door)
}

# Step 2
"Simulate the choosing process and only keep those 
situations satisfying the description."
switch_or_stay <- function(){
  first_round <- sample(c(2,3), 1)
  doors <- doors()
  if (doors()[first_round] == F){
    return(doors[1])
  } 
  else{return(NA)}
}

# Step3
"Define the function for calculating the probability
of winning by staying."
probability_of_winning_by_staying <- function(n_times_replication){
  valid_experiment <- 
    sum(!is.na(replicate(n_times_replication, switch_or_stay())))
  win_by_staying <-
    sum(replicate(n_times_replication, switch_or_stay()) == TRUE, na.rm = T)
  return(win_by_staying/valid_experiment)
}

# Step 4
"Replication the calculating function. 
According to Large Number Law, with a large
number of replicating time, the mean would 
proximate to the real number, which is the 
probability in this case."
prob <- mean(
  replicate(1000, probability_of_winning_by_staying(100)))
cat('Prob of winning by staying at door1:', prob,'.')