scale <- load_scale("data/hra-score.yaml")
test_that("conversion to mirt object",{
    m <- irt_model(scale)
    mirt_model <- as_mirt_model(m)
    mirt_prms <- mirt::coef(mirt_model)
    expect_named(mirt_prms, c(paste0("ITEM_", 1:7), "GroupPars"))
    expect_equal(mirt_prms$ITEM_1[1,], c(a1 = 1, d1=2, d2 = 1, d3= 0, d4 = -1, d5 = -2))
    expect_equal(mirt_prms$ITEM_7[1,], c(a1 = 1, d=-0.1, g = 0.01, u = 1))

    m <- set_initial_estimates(m,2,"DIS",2.5)
    m <- set_initial_estimates(m,2,"DIF1",-3)
    mirt_model <- as_mirt_model(m)
    mirt_prms <- mirt::coef(mirt_model)
    expect_equal(mirt_prms$ITEM_2[1,], c(a1 = 2.5, d1=7.5, d2 = 5, d3= 2.5, d4 = 0, d5 = -2.5))
    
})