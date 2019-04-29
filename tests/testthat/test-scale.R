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

test_that("Scale from df",{
  df <- data.frame(ITEM=c(1,2,2), DV=c(1,1,2))
  suppressWarnings(scale <- create_scale_from_df(df))
  item <- get_item(scale, 1)
  expect_null(item)   # One level makes no item
  item <- get_item(scale, 2)
  expect_equal(item$levels, c(1, 2))
  
  df <- data.frame(GROB=c(1,1,1,1,4,4), PROD=c(1,3,2,4,6,5))
  suppressWarnings(scale <- create_scale_from_df(df, item="GROB", dv="PROD"))
  item <- get_item(scale, 1)
  expect_equal(item$levels, c(1,2,3,4))
  item <- get_item(scale, 4)
  expect_equal(item$levels, c(5, 6))
  expect_equal(length(scale$items), 2)
  
  df <- data.frame(ITEM=c(1, 1, 5, 5), DV=c(0, 1, 0, 1), name=c("myname1", "", "myname2", ""), stringsAsFactors=FALSE)
  scale <- create_scale_from_df(df, name="name")
  item <- get_item(scale, 1)
  expect_equal(item$name, "myname1")
  expect_equal(item$type, "binary")
  item <- get_item(scale, 5)
  expect_equal(item$name, "myname2")
  expect_equal(item$type, "binary")
  
  df <- data.frame(ITEM=c(1, 1, 5, 5), DV=c(0, 1, 0, 1), type=c("ordcat", "", "binary", ""), stringsAsFactors=FALSE)
  scale <- create_scale_from_df(df, type="type")
  item <- get_item(scale, 1)
  expect_equal(item$type, "ordcat")
  item <- get_item(scale, 5)
  expect_equal(item$type, "binary")
})

test_that("Scale from csv", {
  scale <- create_scale_from_csv(file = system.file("extdata","hra-score-data.csv", package = "nmIRT"))
  # item 100 should be ignored (MDV=1)
  expect_null(get_item(scale, 100))
  for(i in 1:7){ 
    # other items should have correct class, type and levels
    item <- get_item(scale, i)
    expect_s3_class(item, "irt_item")
    if(i %in% 1:2) {
      expect_equal(item$levels, 0:5)
      expect_equal(item$type, "ordcat")
    }else{
      expect_equal(item$levels, 0:1)
      expect_equal(item$type, "binary")
    }
  }
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
    scale <- consolidate_levels(myscale, 2, c(0, 1, 2, 3))
    item <- get_item(scale, 2)
    expect_equal(item$levels, c(3, 4))
    scale <- consolidate_levels(myscale, 3, c(3, 4))
    item <- get_item(scale, 3)
    expect_equal(item$levels, c(0, 1, 2, 3))
    expect_error(consolidate_levels(myscale, 3, c(0)))
    expect_error(consolidate_levels(myscale, 3, c(1, 2)))
})

test_that("Ordcat levels", {
    levels <- ordcat_levels(myscale)
    expect_equal(levels, c(5, 6))
})

test_that("Ordcat level arrays", {
    arrs <- ordcat_level_arrays(myscale)
    expect_equal(arrs, list(c(0, 1, 2, 3, 4), c(0, 1, 2, 3, 4, 5)))
})

test_that("irt_item constructor", {
    item <- irt_item(1, "name", c(1, 2, 3), "ordcat", c("cat1", "cat5"), inits=c(8, 9, 10))
    expect_equal(item$levels, c(1, 2, 3))
})

test_that("Item levels", {
    a <- item_levels('[2,3]')
    expect_equal(a, c(2, 3))
    a <- item_levels('(1, 2, 4)')
    expect_equal(a, c(1, 2, 4))
    a <- item_levels('(3, 1)')
    expect_equal(a, c(1, 3))
})

test_that("Levels as string", {
    a <- levels_as_string(c(1,2,3))
    expect_equal(a, "[1,3]")
    a <- levels_as_string(c(3,5))
    expect_equal(a, "(3,5)")
})

test_that("All items", {
    a <- all_items(myscale)
    expect_equal(a, 1:68)
})

test_that("Items by type", {
    a <- items_by_type(myscale, "binary")
    expect_equal(a, c(60, 61))
})

test_that("Item name list", {
    a <- item_name_list(myscale)
    expect_equal(names(a)[1], "1")
    expect_true(is.character(a))
})

test_that("Items in categories", {
    items <- items_in_categories(myscale, "non-motor")
    expect_equal(items, 1:13)
    items <- items_in_categories(myscale, c("motor", "tremor"))
    expect_equal(items, 14:65)
})