context("NONMEM integration")

skip_if_not(FALSE, message="NONMEM integration not activated")
skip_if_not(Sys.which("psn") != "", message="PsN not available: NONMEM integration tests skipped")


test_that("hra-score",{
    scale <- load_predefined_scale("hra-score")
    model <- irt_model(scale)
    data_path <- system.file("extdata/hra-score-data.csv", package="piraid")
    model <- set_dataset(model, data_path)
    temp_path <- tempdir()
    model_path <- file.path(temp_path, "run1.mod")
    save_model_code(model, model_path)
    oldwd <- getwd()
    setwd(temp_path)
    teardown(setwd(oldwd))
    system("execute run1.mod")
    if (FALSE) {     # Enable to update the table in the package
        file.copy(file.path(temp_path, "irt_tab1"), system.file("extdata", package="piraid"))
    }
    expect_true("run1.lst" %in% list.files(temp_path))
})