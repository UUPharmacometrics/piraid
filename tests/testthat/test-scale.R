context("Scale objects")
myscale <- predefined_scale("MDS-UPDRS")

test_that("Scale constructor", {
    scale <- irt_scale()
    expect_equal(length(scale$items), 0)
})

test_that("List predifined scales", {
    a <- list_predefined_scales()
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

test_that("Select categories", {
    scale <- predefined_scale("MDS-UPDRS")
    scale <- select_categories(scale, "motor")
    item <- get_item(scale, 14)
    expect_equal(item$number, 14)
    item <- get_item(scale, 13)
    expect_null(item)
    expect_equal(length(scale$items), 35)
})

test_that("Scale from dataset", {
    df <- data.frame(ITEM=c(1,2,2), DV=c(1,1,2))
    suppressWarnings(scale <- scale_from_dataset(df))
    item <- get_item(scale, 1)
    expect_null(item)
    item <- get_item(scale, 2)
    expect_equal(item$levels, c(1, 2))
    
    df <- data.frame(GROB=c(1,1,1,1,4,4), PROD=c(1,3,2,4,6,5))
    suppressWarnings(scale <- scale_from_dataset(df, item="GROB", dv="PROD"))
    item <- get_item(scale, 1)
    expect_equal(item$levels, c(1,2,3,4))
    item <- get_item(scale, 4)
    expect_equal(item$levels, c(5, 6))
    expect_equal(length(scale$items), 2)
})

test_that("Scale overview", {
    scale <- predefined_scale("MDS-UPDRS")
    df <- scale_overview(scale)
    expect_equal(nrow(df), 68)
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

test_that("Remove items", {
    scale <- predefined_scale("MDS-UPDRS")
    scale <- remove_items(scale, 1:67)
    expect_equal(length(scale$items), 1)
    item <- get_item(scale, 68)
    expect_equal(item$number, 68)
})

test_that("Get item index", {
      i <- get_item_index(myscale, 43)
      expect_equal(i, 43)
      item <- irt_item(23, "myNAME", c(1, 2), "ordcat")
      scale <- irt_scale()
      scale <- add_item(scale, item)
      i <- get_item_index(scale, 23)
      expect_equal(i, 1)
})

test_that("Consolidate levels", {
    scale <- consolidate_levels(myscale, 2, c(0,1,2,3))
    item <- get_item(scale, 2)
    #expect_equal(item$levels, c(0,1,2,3))
})