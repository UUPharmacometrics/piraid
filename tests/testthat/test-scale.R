context("Scale objects")

test_that("Get item", {
    item <- irt_item(23, c(1, 2), "ordcat")
    scale <- irt_scale()
    scale <- add_item(scale, item)
    outitem <- get_item(scale, 23)
    expect_equal(outitem$number, 23)
    expect_equal(outitem$levels, c(1, 2))
    expect_equal(outitem$type, "ordcat")
})

test_that("Add item", {
    item <- irt_item(23, c(1, 2), "ordcat")
    scale <- irt_scale()
    scale <- add_item(scale, item)
    expect_equal(length(scale$items), 1)
    expect_equal(scale$items[[1]]$number, 23)
    item2 <- irt_item(28, c(2), "ordcat")
    expect_warning(scale <- add_item(scale, item2))
    expect_equal(length(scale$items), 1)
    expect_warning(scale <- add_item(scale, item))
    expect_equal(length(scale$items), 1)
})

test_that("Predefined scale", {
    scale <- predefined_scale("MDS-UPDRS")
    item <- get_item(scale, 14)
    expect_equal(item$number, 14)
    expect_equal(item$levels, c(0, 1, 2, 3, 4))
})
