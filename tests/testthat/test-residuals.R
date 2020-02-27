context("residuals")


test_that("WRESLIKE", {
    df <- data.frame(ID=c(1,1,1,2,2), TIME=c(1,2,3,1,2), DV=c(3,2,4,1,3), P0=c(0, 0.010, 0, 0.12, 0), P1=c(0.07, 0.09, 0, 0.33, 0.004), P2=c(0.08, 0.13, 0.013, 0.1, 0.11), P3=c(0.23, 0, 0.22, 0, 0.11), P4=c(0, 0, 0.09, 0, 0))
    new <- withr::with_seed(12, piraid::calculate_wreslike(df))
    expect_equal(new$WRESLIKE, c(-0.97, -0.819, -0.474, -0.810, -1.11), tolerance=1e-2)
})