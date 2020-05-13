context("Plots")

test_that("mirror_plots", {
    scale <- load_predefined_scale("hra-score")
    model <- irt_model(scale) %>% 
        set_dataset(system.file("extdata", "hra-score-data.csv", package="piraid"))
    
    plot_list <- mirror_plots(model)
    expect_equal(length(plot_list), 1)
    expect_true(is(plot_list[[1]], "ggplot"))
})


test_that("icc_plots", {
    scale <- load_predefined_scale("hra-score")
    model <- irt_model(scale)
    tab <- read.table(system.file("extdata","irt_tab1", package = "piraid"), skip = 1, header = T)
    plot_list <- diagnose_icc_fit(model, tab, resample_psi = F)
    expect_equal(length(plot_list), 1)
    expect_true(is(plot_list[[1]], "ggplot"))
})