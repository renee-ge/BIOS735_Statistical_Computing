test_that("test the RF function", {
  data(mtcars)
  expect_no_error(do_rf_v2(data=data.table(mtcars),features = colnames(mtcars)[2:11],outcome = "mpg"))
})

test_that("test the GBM function", {
  data(mtcars)
  gbm_tg <- expand.grid(n.trees=c(70),
                        interaction.depth=c(5,6,7),
                        shrinkage=c(0.04,0.05),
                        n.minobsinnode=c(3, 5))
  expect_no_error(do_gbm_v2(data=data.table(mtcars),features = colnames(mtcars)[2:11],outcome = "mpg",tg=gbm_tg))
})

