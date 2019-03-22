context("item_parameters")

scale <- predefined_scale("MDS-UPDRS")
model <- irt_model(scale)


test_that("Theta init", {
    a <- theta_init(0.5, -1 ,2)
    expect_equal(a, "(-1,0.5,2)")
    a <- theta_init(0, -1 ,2)
    expect_equal(a, "0")
    a <- theta_init(1, 0.2)
    expect_equal(a, "(0.2,1)")
})

test_that("Item inits", {
    a <- item_inits(model, get_item(model$scale, 17), 1)
    expect_equal(length(a), 5)
    expect_equal(a[1], "(0,1.06) ; I17DIS 1")
})

test_that("theta_string_label_part", {
    a <- theta_string_label_part(model, get_item(model$scale, 1), "DIF2", 23)
    expect_equal(a, "; I1DIF2 23")
})

test_that("fix_item_parameters", {
    # White box testing ahead
    mod <- fix_item_parameters(model, 1, "DIF1")
    expect_equal(mod$item_parameters, data.frame(item=1, parameter="DIF1", fix=TRUE, init=as.numeric(NA), stringsAsFactors=FALSE))

    mod2 <- fix_item_parameters(mod, c(2, 3), c("DIS", "DIF2"))
    expect_equal(mod2$item_parameters, data.frame(item=c(1, 2, 3, 2, 3), parameter=c("DIF1", "DIS", "DIS", "DIF2", "DIF2"), fix=c(TRUE, TRUE, TRUE, TRUE, TRUE), init=as.numeric(c(NA, NA, NA, NA, NA)), stringsAsFactors=FALSE))

    mod3 <- fix_item_parameters(model, c(1), c("DIS", "DIF1"))
    mod3$item_parameters$init = c(0.25, 3)
    mod4 <- fix_item_parameters(mod3, 1, "DIF1")
    expect_equal(mod4$item_parameters, data.frame(item=c(1, 1), parameter=c("DIS", "DIF1"), fix=c(T, T), init=c(0.25, 3), stringsAsFactors=F))
})

test_that("initial_estimates_item_parameters", {
    mod <- initial_estimates_item_parameters(model, 2, "DIF1", 0.1)
    expect_equal(mod$item_parameters, data.frame(item=2, parameter="DIF1", fix=as.logical(NA), init=0.1, stringsAsFactors=FALSE))
    
    mod2 <- initial_estimates_item_parameters(model, c(1, 2), c("DIF1", "DIF2"), 0.1)
    expect_equal(mod2$item_parameters, data.frame(item=c(1, 1, 2, 2), parameter=c("DIF1", "DIF2", "DIF1", "DIF2"), fix=as.logical(c(NA, NA, NA, NA)), init=c(0.1, 0.1, 0.1, 0.1), stringsAsFactors=F))
    
    mod3 <- initial_estimates_item_parameters(model, c(1, 2), c("DIF1", "DIF2"), c(0.1, 0.2))
    expect_equal(mod3$item_parameters, data.frame(item=c(1, 1, 2, 2), parameter=c("DIF1", "DIF2", "DIF1", "DIF2"), fix=as.logical(c(NA, NA, NA, NA)), init=c(0.1, 0.2, 0.1, 0.2), stringsAsFactors=F))
})

test_that("initial_estimates_overview", {
    df <- initial_estimates_overview(model)
    expect_equal(df$item[1], 1)
    expect_equal(df$item[6], 2)
    expect_equal(df$parameter[1], "DIS")
    expect_equal(df$parameter[3], "DIF2")
    expect_equal(df$fix[1], FALSE)
    expect_equal(df$init[9], 2.59)
})