context("Scale objects")

test_that("Scale constructor", {
    scale <- irt_scale()
    expect_equal(length(scale$items), 0)
})

test_that("List predifined scales", {
    a <- list_predefined_scales()
    print(a)
    expect_true(length(a) >= 1)
    expect_true("mds-updrs" %in% a)
})

test_that("Predefined scale", {
    scale <- predefined_scale("MDS-UPDRS")
    item <- get_item(scale, 14)
    expect_equal(item$number, 14)
    expect_equal(item$levels, c(0, 1, 2, 3, 4))
})

test_that("Load scale", {
    path <- system.file("extdata", "mds-updrs.yaml", package="nmIRT")
    scale <- load_scale(path)
    item <- get_item(scale, 14)
    expect_equal(item$levels, c(0, 1, 2, 3, 4))
})

test_that("Save scale", {
    mf <- mockery::mock(1)
    mockery::stub(save_scale, 'yaml::write_yaml', mf)
    scale <- predefined_scale("MDS-UPDRS")
    save_scale(scale, "myscale.yaml")
    mockery::expect_called(mf, 1)
})

test_that("Get item", {
    item <- irt_item(23, "myNAME", c(1, 2), "ordcat")
    scale <- irt_scale()
    scale <- add_item(scale, item)
    outitem <- get_item(scale, 23)
    expect_equal(outitem$number, 23)
    expect_equal(outitem$name, "myNAME")
    expect_equal(outitem$levels, c(1, 2))
    expect_equal(outitem$type, "ordcat")
})

test_that("Add item", {
    item <- irt_item(23, "myNAME", c(1, 2), "ordcat")
    scale <- irt_scale()
    scale <- add_item(scale, item)
    expect_equal(length(scale$items), 1)
    expect_equal(scale$items[[1]]$number, 23)
    item2 <- irt_item(28, "otherName", c(2), "ordcat")
    expect_warning(scale <- add_item(scale, item2))
    expect_equal(length(scale$items), 1)
    expect_warning(scale <- add_item(scale, item))
    expect_equal(length(scale$items), 1)
})