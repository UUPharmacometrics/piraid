context("irt_model")

test_that("Constructor for simple scale", {
    df <- data.frame(ITEM=c(1, 1, 5, 5), DV=c(0, 1, 0, 1))
    scale <- scale_from_df(df)
    model <- irt_model(scale)
    expect_equal(model$scale, scale) # scale object is integrated in model 
})