context("Dataset")

test_that("consolidate_data", {
    scale <- load_predefined_scale("hra-score")
    model <- irt_model(scale)
    model <- consolidate_levels(model, 2, c(4, 5))
    data <- data.frame(ITEM=c(1,1,2,2,1,1,1), DV=c(4,3,4,1,5,1,2))
    res <- consolidate_data(data, model)
    expect_equal(res, data.frame(ITEM=c(1,1,2,2,1,1,1), DV=c(4,3,3,1,5,1,2)))
    
    model <- consolidate_levels(model, 1, 5)
    data2 <- data.frame(ITEM=c(1,1,2,2,1,1,2,2), DV=c(4,5,1,4,1,2,5,1))
    res2 <- consolidate_data(data2, model)
    expect_equal(res2, data.frame(ITEM=c(1,1,2,2,1,1,2,2), DV=c(4,4,1,3,1,2,3,1)))
})
    