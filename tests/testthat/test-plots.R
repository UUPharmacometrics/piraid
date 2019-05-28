context("Plots")

test_that("mirror_plots", {
    scale <- load_predefined_scale("hra-score")
    model <- irt_model(scale)
    data <- read_dataset(system.file("extdata", "hra-score-data.csv", package="piraid"))

    plot_list <- mirror_plots(data, model)
    expect_equal(length(plot_list), 1)
    expect_true(is(plot_list[[1]], "ggplot"))
})
