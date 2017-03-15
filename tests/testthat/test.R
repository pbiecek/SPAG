test_that("Checking data quality for Coverage Index", {
  df <- data.frame(emp = c(5,8,2,4,14,11,3,6,9,6,2,14,5), category=c("A","A","B","B","B","C","C","C","C","A","A","A","A"))
  expect_equal(calcCoverageIndex(df,unique(df[,2])), c(40/89,20/89,29/89, 1))
  df <- data.frame(emp = c(2,4,14,11,3,6,9,6,2,14,5,5,8), category=c("B","B","B","C","C","C","C","A","A","A","A","A","A"))
  expect_equal(calcCoverageIndex(df,unique(df[,2])), c(20/89,29/89,40/89, 1))
})