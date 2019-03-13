context("lv_models")


test_that("get_number_of_thetas_and_etas", {
    a <- get_number_of_thetas_and_etas(c("PSI=THETA(1)+ETA(1)"))
    expect_equal(a, list(numetas=1, numthetas=1))
    
    a <- get_number_of_thetas_and_etas(c("BASE=THETA(1)+ETA(1)", "SLOP=THETA(2)+ETA(2)"))
    expect_equal(a, list(numetas=2, numthetas=2))
    
    a <- get_number_of_thetas_and_etas(c("BASE=THETA(1)+ETA(1)", "SLOP=THETA(1)+ETA(2)"))
    expect_equal(a, list(numetas=2, numthetas=1))
})