context("item_parameters")

scale <- load_predefined_scale("MDS-UPDRS")
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

test_that("update parameter table", {
    new_values <- tibble::tibble(item = c(1,2), parameter = c("DIF1", "DIF2"), init = 0.1)
    mod1 <- update_parameter_table(model, new_values)
    newer_values <- tibble::tibble(item = c(1,2), parameter = c("DIF1", "DIF1"), init = -0.1, bla ="bla")
    mod2 <- update_parameter_table(mod1, newer_values)
})

test_that("fix_item_parameters", {
    # White box testing ahead
    mod <- fix_item_parameters(model, 1, "DIF1")
    expect_equal(mod$item_parameters, data.frame(item=1, parameter="DIF1", fix=TRUE, init=as.numeric(NA), ignore=as.logical(NA), stringsAsFactors=FALSE))

    mod2 <- fix_item_parameters(mod, c(2, 3), c("DIS", "DIF2"))
    expect_equal(mod2$item_parameters, data.frame(item=c(1, 2, 3, 2, 3), parameter=c("DIF1", "DIS", "DIS", "DIF2", "DIF2"), fix=c(TRUE, TRUE, TRUE, TRUE, TRUE), init=as.numeric(c(NA, NA, NA, NA, NA)), ignore=as.logical(c(NA, NA, NA, NA, NA)), stringsAsFactors=FALSE))

    mod3 <- fix_item_parameters(model, c(1), c("DIS", "DIF1"))
    mod3$item_parameters$init = c(0.25, 3)
    mod4 <- fix_item_parameters(mod3, 1, "DIF1")
    expect_equal(mod4$item_parameters, data.frame(item=c(1, 1), parameter=c("DIS", "DIF1"), fix=c(T, T), init=c(0.25, 3), ignore=as.logical(c(NA, NA)), stringsAsFactors=F))
})

test_that("setting initial estimates", {
    mod <- set_initial_estimates(model, 2, "DIF1", 0.1)
    expect_equal(mod$item_parameters, data.frame(item=2, parameter="DIF1", fix=as.logical(NA), init=0.1, ignore=as.logical(NA), stringsAsFactors=FALSE))
    
    mod2 <- set_initial_estimates(model, c(1, 2), c("DIF1", "DIF2"), 0.1)
    expect_equal(mod2$item_parameters, data.frame(item=c(1, 1, 2, 2), parameter=c("DIF1", "DIF2", "DIF1", "DIF2"), fix=as.logical(c(NA, NA, NA, NA)), init=c(0.1, 0.1, 0.1, 0.1), ignore=as.logical(c(NA, NA, NA, NA)), stringsAsFactors=F))
    
    mod3 <- set_initial_estimates(model, c(1, 2), c("DIF1", "DIF2"), c(0.1, 0.2))
    expect_equal(mod3$item_parameters, data.frame(item=c(1, 1, 2, 2), parameter=c("DIF1", "DIF2", "DIF1", "DIF2"), fix=as.logical(c(NA, NA, NA, NA)), init=c(0.1, 0.2, 0.1, 0.2), ignore=as.logical(c(NA, NA, NA, NA)), stringsAsFactors=F))
})


test_that("setting initial estimates table", {
    inits <- tibble::tibble(item = c(1,2), parameter = c("DIF1", "DIF2"), init = c(0.1, 0.2))
    mod <- set_initial_estimates_table(model, inits)
    
    dplyr::left_join(inits, mod$item_parameter, c("item", "parameter")) %>% 
        {expect_equal(.$init.x, .$init.y)}
    
    inits2 <- dplyr::mutate(inits, init = -1)
    mod2 <- set_initial_estimates_table(mod, inits2)

})

test_that("updating parameters using long format", {
    inits <- tibble::tibble(item = c(1,2), parameter = c("DIF1", "DIF2"), value = c(0.1, 0.2))
    mod <- update_parameters(model, inits)
    
    expect_equal(initial_estimate(mod, get_item(mod$scale, 1), "DIF1"), 0.1)
    expect_equal(initial_estimate(mod, get_item(mod$scale, 2), "DIF2"), 0.2)
})

test_that("updating parameters using wide format", {
    prms <- tibble::tibble(item = 1:2, DIF1 = c(0.1, NA), DIF2=c(NA, 0.2))
    mod <- update_parameters(model, prms)
    expect_equal(initial_estimate(mod, get_item(mod$scale, 1), "DIF1"), 0.1)
    expect_equal(initial_estimate(mod, get_item(mod$scale, 2), "DIF2"), 0.2)
})



test_that("list_initial_estimates", {
    df <- list_initial_estimates(model)
    expect_equal(df$item[1], 1)
    expect_equal(df$item[6], 2)
    expect_equal(df$parameter[1], "DIS")
    expect_equal(df$parameter[3], "DIF2")
    expect_equal(df$fix[1], FALSE)
    expect_equal(df$init[9], 2.59)
})

test_that("all_parameter_names", {
    parnames <- all_parameter_names(model)
    expect_equal(parnames, c("DIF", "DIF1", "DIF2", "DIF3", "DIF4", "DIF5", "DIS", "GUE"))
})