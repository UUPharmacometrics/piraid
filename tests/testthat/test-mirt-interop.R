scale <- load_scale("data/hra-score.yaml")
m <- irt_model(scale)
mirt_model <- as_mirt_model(m)

test_that("conversion to mirt object",{
    mirt_prms <- mirt::coef(mirt_model)
    expect_named(mirt_prms, c(paste0("ITEM_", 1:7), "GroupPars"))
    expect_equal(mirt_prms$ITEM_1[1,], c(a1 = 1, d1=2, d2 = 1, d3= 0, d4 = -1, d5 = -2))
    expect_equal(mirt_prms$ITEM_7[1,], c(a1 = 1, d=-0.1, g = 0.01, u = 1))

    m <- set_initial_estimates(m,2,"DIS",2.5)
    m <- set_initial_estimates(m,2,"DIF1",-3)
    mirt_model <- as_mirt_model(m)
    mirt_prms <- mirt::coef(mirt_model)
    expect_equal(mirt_prms$ITEM_2[1,], c(a1 = 2.5, d1=7.5, d2 = 5, d3= 2.5, d4 = 0, d5 = -2.5))
    
})

test_that("conversion to mirt names",{
    expect_equal(get_mirt_names(m), paste0("ITEM_",1:7))  
})


test_that("conversion to mirt types", {
    expected <-
        c(
            ITEM_1 = "graded",
            ITEM_2 = "graded",
            ITEM_3 = "3PL",
            ITEM_4 = "3PL",
            ITEM_5 = "3PL",
            ITEM_6 = "3PL",
            ITEM_7 = "3PL"
        )
    expect_equal(get_mirt_types(m), expected)
})

test_that("creation of pseudo data", {
    expected <-
        data.frame(
            ITEM_1 = 0:5,
            ITEM_2 = 0:5,
            ITEM_3 = c(0L, 1L,  NA_integer_, NA_integer_, NA_integer_, NA_integer_),
            ITEM_4 = c(0L, 1L,  NA_integer_, NA_integer_, NA_integer_, NA_integer_),
            ITEM_5 = c(0L, 1L,  NA_integer_, NA_integer_, NA_integer_, NA_integer_),
            ITEM_6 = c(0L, 1L,  NA_integer_, NA_integer_, NA_integer_, NA_integer_),
            ITEM_7 = c(0L, 1L,  NA_integer_, NA_integer_, NA_integer_, NA_integer_)
        )
    expect_equal(create_mirt_pseudo_data(m), expected)
})