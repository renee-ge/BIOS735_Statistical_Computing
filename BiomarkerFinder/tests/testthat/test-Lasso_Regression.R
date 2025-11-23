test_that("Lasso inference test", {
  data(mtcars)
  X = mtcars[,2:11]
  y = mtcars[,1]
  expect_no_error(lasso_bootstrap_inference(X,y,2))
})

