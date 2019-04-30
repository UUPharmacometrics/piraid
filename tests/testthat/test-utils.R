context("utility functions")

test_that("update_or_insert",{
    
    old <- tibble::tibble(item = as.integer(1), parameter = "DIF", value = -1, fix = T)
    new <- tibble::tibble(item = 1:2, parameter = "DIF", value = 2)
    expected <- tibble::tibble(item = 1:2, parameter = "DIF", value = 2, fix = c(T, NA))
    expect_message(updated <- update_or_insert(old, new, c("item", "parameter")))
    expect_equal(updated, expected)
})