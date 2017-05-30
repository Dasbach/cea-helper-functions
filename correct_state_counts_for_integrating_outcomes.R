
# correct_state_counts_simpsons_rule adjusts the health state counts using Simpson's rule
#
# the function expects a data frame of raw health state counts by cycle
# the function returns a data frame of adjusted health state counts by cycle 
# the returned data frame health state counts can be summed across cycles to integrate an outcome
#
# the method is based on the publication 
#   Elbasha EH, Chhatwal J.  Theoretical Foundations and Practical Applications of Within-Cycle Correction Methods. 
#     Medical Decision Making 2016 Jan;36(1):115-31

correct_state_counts_simpsons_rule <- function(state_counts) {
  
  first_cycle <- 1
  second_cycle <- 2
  third_cycle <- 3
  last_cycle <-length(state_counts)
  
  state_corrected_counts <- state_counts
  
  first_cycle_corrected_count <- state_counts[first_cycle]/3.0
  
  last_cycle_corrected_count <- state_counts[last_cycle]/3.0
  
  even_cycle_sequence <-seq(second_cycle,last_cycle-1,2)
  
  odd_cycle_sequence <-seq(third_cycle,last_cycle-1,2)
  
  state_corrected_counts[even_cycle_sequence] <- state_counts[even_cycle_sequence]*4.0/3.0
  
  state_corrected_counts[odd_cycle_sequence] <- state_counts[odd_cycle_sequence]*2.0/3.0
  
  state_corrected_counts[first_cycle] <- first_cycle_corrected_count
  
  state_corrected_counts[last_cycle] <- last_cycle_corrected_count
  
  state_corrected_counts
}
