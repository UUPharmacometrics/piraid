#' Calculate mean and SD as a function of PSI
#'
#' @param model An irt_model object
#' @param psi_range A vector of lenght 2 specifying the lower and upper boundary for the latent variable
#'
#' @return A data.frame or a plot 
#' @export
calculate_e_score_vs_psi <- function(model, psi_range = c(-4,4)){
    mirt_model <- as_mirt_model(model)
    theta <- seq(psi_range[1], psi_range[2], length.out = 100)
    tibble::tibble(
        psi = theta,
        score = mirt::expected.test(mirt_model, matrix(theta))
    )
}

#' @export
#' @rdname calculate_e_score_vs_psi
plot_e_score_vs_psi <- function(model, ...){
    df <- calculate_e_score_vs_psi(model, ...)
    ggplot2::ggplot(df, aes(psi, score))+
        ggplot2::xlab("PSI")+
        ggplot2::ylab("Score")+
        ggplot2::geom_line()
}

#' @export
#' @rdname calculate_e_score_vs_psi
calculate_sd_score_vs_psi <- function(model, psi_range = c(-4,4)){
    mirt_model <- as_mirt_model(model)
    theta <- seq(psi_range[1], psi_range[2], length.out = 100)
    prob <- purrr::map(seq_along(get_mirt_names(model)), ~mirt::extract.item(mirt_model, .x)) %>%
        purrr::map(~mirt::probtrace(.x, theta))
    escore <- purrr::map(seq_along(get_mirt_names(model)), ~mirt::extract.item(mirt_model, .x)) %>%
        purrr::map(~mirt::expected.item(.x, theta))
    
    item_levels <- purrr::map(model$scale$items, "levels") %>% 
        purrr::map(~matrix(.x, nrow = length(theta), ncol = length(.x), byrow = TRUE))
    
    sd_score <- purrr::map2(item_levels, escore, ~.x-.y) %>% 
        purrr::map2(prob, ~rowSums(.y*.x^2)) %>% 
        purrr::reduce(`+`) %>% 
        sqrt

    tibble::tibble(
        psi = theta,
        sd_score = sd_score
    ) 
}

#' @export
#' @rdname calculate_e_score_vs_psi
plot_sd_score_vs_psi <- function(model, ...){
    df <- calculate_sd_score_vs_psi(model, ...)
    ggplot2::ggplot(df, aes(psi, sd_score))+
        ggplot2::xlab("PSI")+
        ggplot2::ylab("sd(Score)")+
        ggplot2::geom_line()
}

#' Determine IRT-based mean and SD link for CV model
#'
#' @param model 
#' @param psi_range 
#' @param score_range 
#' @param range_tol 
#' @param approx_tol_mean 
#' @param approx_tol_sd 
#'
#' @return
#' @export
calculate_cv_irt_link <- function(model, 
                                  psi_range = NULL, 
                                  score_range = c(-Inf, Inf),
                                  range_tol = 0.3,
                                  approx_tol_mean = 0.1,
                                  approx_tol_sd  = 0.01){
    mirt_model <- as_mirt_model(model)
    result <- list()
    # function calculatingirt_ts the mean score
    f_mean <- function(x) mirt::expected.test(mirt_model, matrix(x))
    # function calculating the sd score
    f_sd <- function(psi){
        prob <- purrr::map(seq_along(get_mirt_names(model)), ~mirt::extract.item(mirt_model, .x)) %>%
            purrr::map(~mirt::probtrace(.x, psi))
        escore <- purrr::map(seq_along(get_mirt_names(model)), ~mirt::extract.item(mirt_model, .x)) %>%
            purrr::map(~mirt::expected.item(.x, psi))
        
        item_levels <- purrr::map(model$scale$items, "levels") %>% 
            purrr::map(~matrix(.x, nrow = length(psi), ncol = length(.x), byrow = TRUE))
        
        sd_score <- purrr::map2(item_levels, escore, ~.x-.y) %>% 
            purrr::map2(prob, ~rowSums(.y*.x^2)) %>% 
            purrr::reduce(`+`) %>% 
            sqrt
        return(sd_score)
    }
    # determine psi range for approximation
    max_range <- total_score_range(model$scale)
    if(!is.finite(score_range[1])) score_range[1] <- max_range[1]
    if(!is.finite(score_range[2])) score_range[2] <- max_range[2]
    psi_interval <- c(-10, 10)
    # solve score_min+range_tol = f_mean(psi) using root finder
    sol_min <- uniroot(function(psi) f_mean(psi) - score_range[1] - range_tol, psi_interval)
    # solve score_max-range_tol = f_mean(psi) using root finder
    sol_max <- uniroot(function(psi) f_mean(psi) - score_range[2] + range_tol, psi_interval)
    psi_range <- c(sol_min$root, sol_max$root)
    # determine the polynomial degree necessary to approx mean fun with requested tol
    degree <- 0
    psi_grid <- seq(psi_range[1], psi_range[2], length.out = 100)
    f_true <- f_mean(psi_grid)
    result$range <- psi_range
    repeat{
        degree <- degree+1
        f_approx <- pracma::chebApprox(psi_grid, f_mean, psi_range[1], psi_range[2], degree)
        error <- max(abs(f_true-f_approx))  
        if(error < approx_tol_mean || degree>20 ) {
            break
        }
    }
    # determine Chebyshev polynomial coefficients
    coef_cheb <-  pracma::chebCoeff(f_mean, psi_range[1], psi_range[2], degree)
    poly_cheb <- pracma::chebPoly(degree)
    coef <- rev(drop(coef_cheb %*% poly_cheb))
    coef[1] <- coef[1] - coef_cheb[1]/2
    result$mean <- list(degree = degree, 
                        psi = psi_grid, 
                        true = f_true, 
                        approx = f_approx,
                        coefficients = coef)
    # determine the polynomial degree necessary to approx sd fun with requested tol
    degree <- 0
    f_true <- f_sd(psi_grid)
    repeat{
        degree <- degree+1
        f_approx <- pracma::chebApprox(psi_grid, f_sd, psi_range[1], psi_range[2], degree)
        error <- max(abs(f_true-f_approx))  
        if(error < approx_tol_sd || degree>20 ) {
            break
        }
    }
    # determine Chebyshev polynomial coefficients
    coef_cheb <-  pracma::chebCoeff(f_sd, psi_range[1], psi_range[2], degree)
    poly_cheb <- pracma::chebPoly(degree)
    coef <- rev(drop(coef_cheb %*% poly_cheb))
    coef[1] <- coef[1] - coef_cheb[1]/2
    result$sd <- list(degree = degree, 
                      psi = psi_grid, 
                      true = f_true, 
                      approx = f_approx, 
                      coefficients = coef)
    return(result)
}


#' @export
#' @rdname calculate_cv_irt_link
plot_cv_irt_link <- function(res){
    df <- res[c("mean","sd")] %>% 
        purrr::map(`[`, c("approx","true","psi")) %>% 
        purrr::map_dfr(tibble::as_tibble, 
                       .id = "stat") %>% 
        tidyr::pivot_longer(cols = c("true", "approx"))
    
    ggplot2::ggplot(df, aes(psi, value, color = name)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_discrete("") +
        ggplot2::facet_wrap(ggplot2::vars(stat), scales = "free_y") +
        ggplot2::theme(legend.position = "top")
            
}


get_nm_cv_irt_link <- function(res, digits = 6){
    range_transform <- glue::glue("TLV = (2*LV-({b+a}))/{b-a}", 
                                  a = round(res$range[1], digits), 
                                  b = round(res$range[2], digits))
    create_polynom <- . %>% 
        purrr::imap_chr(~glue::glue("{round(.x, digits)}*TLV**{.y-1}")) %>% 
        purrr::reduce(~paste(.x, .y, sep = ifelse(startsWith(.y,"-"),"","+"))) %>% 
        sub(x = ., pattern = "*TLV**0", replacement = "", fixed = TRUE) %>% 
        sub(x = ., pattern = "TLV**1", replacement = "TLV", fixed = TRUE)
    mean_poly <- res$mean$coefficients %>% create_polynom
    sd_poly <- res$sd$coefficients %>% create_polynom
    cg <- code_generator() %>% 
        add_line(range_transform) %>% 
        add_line("SCORE =", mean_poly) %>% 
        add_line("SDSCORE =", sd_poly) %>% 
        add_empty_line() %>% 
        add_line("Y = SCORE+SDSCORE*EPS(1)")
    return(cg)
}
