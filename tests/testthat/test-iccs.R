scale <- load_scale("data/hra-score.yaml")
m <- irt_model(scale)

test_that("calculation of ICCs works", {
    df <- calculate_iccs(m)
    expect_setequal(colnames(df), c("item", "psi", "category","probability"))
})