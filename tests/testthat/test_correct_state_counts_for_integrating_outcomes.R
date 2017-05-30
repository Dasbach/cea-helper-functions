context("test counts adjusted with Simpson's Rule")

state_counts_df <- data.frame(Alive_State_Counts=c(100,100,100,100,100,100))
corrected_state_counts_df <- as.data.frame(lapply(state_counts_df,correct_state_counts_simpsons_rule))

test_that("dataframe values compute correctly for a single state with 6 cycles", {
  expect_equal(corrected_state_counts_df[1,1], 100/3.0)
  expect_equal(corrected_state_counts_df[[2,1]], 100*(4.0/3.0))
  expect_equal(corrected_state_counts_df[[3,1]], 100*(2.0/3.0))
  expect_equal(corrected_state_counts_df[[4,1]], 100*(4.0/3.0))
  expect_equal(corrected_state_counts_df[[5,1]], 100*(2.0/3.0))
  expect_equal(corrected_state_counts_df[[6,1]], 100/3.0)
  
})

state_counts_df <- data.frame(A=c(100,100,100,100,100,100), B=c(100,100,100,100,100,100), C=c(100,100,100,100,100,100))
corrected_state_counts_df <- as.data.frame(lapply(state_counts_df,correct_state_counts_simpsons_rule))

test_that("dataframe values compute correctly for multiple states with 6 cycles", {
  expect_equal(corrected_state_counts_df[1,1], 100/3.0)
  expect_equal(corrected_state_counts_df[[2,2]], 100*(4.0/3.0))
  expect_equal(corrected_state_counts_df[[3,3]], 100*(2.0/3.0))
  expect_equal(corrected_state_counts_df[[4,3]], 100*(4.0/3.0))
  expect_equal(corrected_state_counts_df[[5,2]], 100*(2.0/3.0))
  expect_equal(corrected_state_counts_df[[6,2]], 100/3.0)
  
})

state_counts_df <- data.frame(Alive_State_Counts=c(100,100,100,100,100,100,100,100))
corrected_state_counts_df <- as.data.frame(lapply(state_counts_df,correct_state_counts_simpsons_rule))

test_that("dataframe values computed correctly for a single state with 8 cycles", {
  expect_equal(corrected_state_counts_df[[6,1]], 100*(4.0/3.0))
  expect_equal(corrected_state_counts_df[[7,1]], 100*(2.0/3.0))
  expect_equal(corrected_state_counts_df[[8,1]], 100/3.0)
  
})
