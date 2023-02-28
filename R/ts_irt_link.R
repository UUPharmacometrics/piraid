
# the function calculates the probability mass function for the total score given psi
pmf_ts <- function(model, psi){
    if(inherits(model, "SingleGroupClass")){
        mirt_model <- model
    }else{
        mirt_model <- as_mirt_model(model)
    }
    pgf <- purrr::map(mirt::extract.mirt(mirt_model, "itemnames"), ~mirt::extract.item(mirt_model, .x)) %>%
        purrr::map(~mirt::probtrace(.x, as.matrix(psi))) %>% 
        purrr::map(~as.list(as.data.frame(t(.x)))) %>% 
        purrr::transpose() %>% 
        purrr::map(~purrr::reduce(.x, function(x, y) pracma::polymul(y, x), .init = 1)) 
    
    do.call(rbind, args = pgf)
}

mean_score <- function(pmf){
    if(NCOL(pmf)==1) pmf <- matrix(pmf, nrow = 1)
    unname(t(matrix(seq(0, ncol(pmf)-1), nrow = 1) %*% t(pmf)))
}

var_score <- function(pmf){
    if(NCOL(pmf)==1) pmf <- matrix(pmf, nrow = 1)
    mu <- mean_score(pmf)
    scores <- matrix(seq(0, ncol(pmf)-1), nrow = nrow(pmf), ncol = ncol(pmf), byrow = T)
    unname(sweep(scores, 1, drop(mu))^2 %*% t(pmf))
}

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

#' Calculate SD(zscore) as a function of PSI
#'
#' @param model An irt_model object
#' @param psi_range A vector of lenght 2 specifying the lower and upper boundary for the latent variable
#'
#' @return A data.frame or a plot 
#' @export
calculate_sd_zscore_vs_psi <- function(model, psi_range = c(-4, 4)){
    psi_grid <- seq(psi_range[1], psi_range[2], length.out = 100)
    pmf <- pmf_ts(model, psi_grid) 
    scores <- matrix(seq(0, ncol(pmf)-1), nrow = 1)
    mx <- max(scores)
    pscores <- (scores+0.5)/(mx+1)
    zscores <- drop(qnorm(pscores))
    mu <- pmf %*% zscores
    
    var <- diag(t(vapply(mu, function(mu_i)  zscores-mu_i, FUN.VALUE = zscores))^2 %*% t(pmf))

    tibble::tibble(
        psi = psi_grid,
        sd_zscore = sqrt(var)
    )
}

#' @export
#' @rdname calculate_sd_zscore_vs_psi
plot_sd_zscore_vs_psi <- function(model, ...){
    df <- calculate_sd_zscore_vs_psi(model, ...)
    ggplot2::ggplot(df, aes(psi, sd_zscore))+
        ggplot2::xlab("PSI")+
        ggplot2::ylab("sd(zscore)")+
        ggplot2::geom_line()
}


#' Determine IRT-based links for CV or BI models
#'
#' @param model 
#' @param psi_range 
#' @param score_range 
#' @param lv_based
#' @param range_tol 
#' @param approx_tol_mean 
#' @param approx_tol_sd 
#'
#' @return
#' @export
calculate_cv_irt_link <- function(model, 
                                  psi_range = NULL, 
                                  score_range = c(-Inf, Inf),
                                  lv_based = TRUE, 
                                  range_tol = 0.3,
                                  approx_tol_mean = 0.1,
                                  approx_tol_sd  = 0.01, 
                                  max_degree = 100){
    mirt_model <- as_mirt_model(model)
    result <- list()
    result$type <- "score"
    result$idv <- ifelse(lv_based, "psi", "score")
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
    if(is.null(psi_range)){
        # determine psi range for approximation
        max_range <- total_score_range(model$scale)
        if(!is.finite(score_range[1])) score_range[1] <- max_range[1]
        if(!is.finite(score_range[2])) score_range[2] <- max_range[2]
        psi_interval <- c(-50, 50)
        # solve score_min+range_tol = f_mean(psi) using root finder
        sol_min <- uniroot(function(psi) f_mean(psi) - score_range[1] - range_tol, psi_interval)
        # solve score_max-range_tol = f_mean(psi) using root finder
        sol_max <- uniroot(function(psi) f_mean(psi) - score_range[2] + range_tol, psi_interval)
        psi_range <- c(sol_min$root, sol_max$root)
    }
    psi_grid <- seq(psi_range[1], psi_range[2], length.out = 100)
    if(!lv_based){
        f_psi <- approxfun(y = psi_grid, x = f_mean(psi_grid), rule = 2)
        f_sd_score <- function(score){
            f_sd(f_psi(score))
        }
        # determine the polynomial degree necessary to approx mean fun with requested tol
        degree <- 0
        score_range <- f_mean(psi_range)
        score_grid <- seq(f_mean(psi_range[1]), f_mean(psi_range[2]), length.out = 100)
        f_true <- f_sd_score(score_grid)
        result$range <- score_range
        repeat{
            degree <- degree+1
            f_approx <- pracma::chebApprox(score_grid, f_sd_score, score_range[1], score_range[2], degree)
            error <- max(abs(f_true-f_approx))  
            if(error < approx_tol_mean || degree>max_degree ) {
                break
            }
        }
        # determine Chebyshev polynomial coefficients
        coef_cheb <-  pracma::chebCoeff(f_sd_score, score_range[1], score_range[2], degree)
        poly_cheb <- pracma::chebPoly(degree)
        coef <- rev(drop(coef_cheb %*% poly_cheb))
        coef[1] <- coef[1] - coef_cheb[1]/2
        result$sd <- list(degree = degree, 
                            score = score_grid, 
                            true = f_true, 
                            approx = f_approx,
                            coefficients = coef)
        # determine approximation quality of the derivatives
        poly_sigma <- rev(result$sd$coefficients)
        poly_dsigma_dscore <- pracma::polyder(poly_sigma)
        score_t  <-  (2*score_grid-(score_range[2]+score_range[1]))/(score_range[2]-score_range[1])
        result$sd$deriv_approx <- pracma::polyval(poly_dsigma_dscore, score_t)*2/(score_range[2]-score_range[1])
        result$sd$deriv_true <- pracma::fderiv(f_sd_score, score_grid)
        return(result)
    }
    
    # determine the polynomial degree necessary to approx mean fun with requested tol
    degree <- 0
    f_true <- f_mean(psi_grid)
    result$range <- psi_range
    repeat{
        degree <- degree+1
        f_approx <- pracma::chebApprox(psi_grid, f_mean, psi_range[1], psi_range[2], degree)
        error <- max(abs(f_true-f_approx))  
        if(error < approx_tol_mean || degree>max_degree ) {
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
        if(error < approx_tol_sd || degree>max_degree ) {
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
    # determine approximation quality of the derivatives
    poly_mu <- rev(result$mean$coefficients)
    poly_sigma <- rev(result$sd$coefficients)
    poly_dmu_dpsi <- pracma::polyder(poly_mu)
    poly_dsigma_dpsi <- pracma::polyder(poly_sigma)
    psi_t  <-  (2*psi_grid-(psi_range[2]+psi_range[1]))/(psi_range[2]-psi_range[1])
    result$mean$deriv_approx <- pracma::polyval(poly_dmu_dpsi, psi_t)*2/(psi_range[2]-psi_range[1])
    result$mean$deriv_true <- pracma::fderiv(f_mean, psi_grid)
    result$sd$deriv_approx <- pracma::polyval(poly_dsigma_dpsi, psi_t)*2/(psi_range[2]-psi_range[1])
    result$sd$deriv_true <- pracma::fderiv(f_sd, psi_grid)
    return(result)
}


#' @rdname calculate_cv_irt_link
#' @export
calculate_bi_irt_link <- function(model, 
                                  psi_range = NULL, 
                                  score_range = c(-Inf, Inf),
                                  lv_based = FALSE,
                                  range_tol = 0.3,
                                  approx_tol_mean = 0.1,
                                  approx_tol_sd  = 0.01, 
                                  max_degree = 100){
    mirt_model <- as_mirt_model(model)
    result <- list()
    result$type <- "zscore"
    result$idv <- ifelse(lv_based, "psi", "zscore")
    # function calculatingirt_ts the mean score
    f_mean <- function(x) mirt::expected.test(mirt_model, matrix(x))
    f_mu <- function(psi){
        pmf <- pmf_ts(mirt_model, psi) 
        scores <- matrix(seq(0, ncol(pmf)-1), nrow = 1)
 #       pmf <- pmf[,-c(1, ncol(pmf))]
        mx <- max(scores)
        pscores <- (scores+0.5)/(mx+1)
#        pscores <- pscores[-c(1, length(pscores))]
        zscores <- drop(qnorm(pscores))
        drop(pmf %*% zscores)
    }
    # function calculating the sd score
    f_sd <- function(psi){
        pmf <- pmf_ts(mirt_model, psi) 
        scores <- matrix(seq(0, ncol(pmf)-1), nrow = 1)
        #pmf <- pmf[,-c(1, ncol(pmf))]
        mx <- max(scores)
        pscores <- (scores+0.5)/(mx+1)
        # pscores <- pscores[-c(1, length(pscores))]
        zscores <- drop(qnorm(pscores))
        mu <- pmf %*% zscores
        
        sqrt(diag(t(vapply(mu, function(mu_i)  zscores-mu_i, FUN.VALUE = zscores))^2 %*% t(pmf)))
    }
    if(is.null(psi_range)){
        # determine psi range for approximation
        max_range <- total_score_range(model$scale)
        if(!is.finite(score_range[1])) score_range[1] <- max_range[1]
        if(!is.finite(score_range[2])) score_range[2] <- max_range[2]
        psi_interval <- c(-50, 50)
        # solve score_min+range_tol = f_mean(psi) using root finder
        sol_min <- uniroot(function(psi) f_mean(psi) - score_range[1] - range_tol, psi_interval)
        # solve score_max-range_tol = f_mean(psi) using root finder
        sol_max <- uniroot(function(psi) f_mean(psi) - score_range[2] + range_tol, psi_interval)
        psi_range <- c(sol_min$root, sol_max$root)
    }
    result$range <- psi_range
    psi_grid <- seq(psi_range[1], psi_range[2], length.out = 100)
    if(!lv_based){
        f_psi <- approxfun(y = psi_grid, x = f_mu(psi_grid), rule = 2)
        f_sd_zscore <- function(score){
            f_sd(f_psi(score))
        }
        # determine the polynomial degree necessary to approx mean fun with requested tol
        degree <- 0
        zscore_range <- f_mu(psi_range)
        zscore_grid <- seq(f_mu(psi_range[1]), f_mu(psi_range[2]), length.out = 100)
        f_true <- f_sd_zscore(zscore_grid)
        result$range <- zscore_range
        repeat{
            degree <- degree+1
            f_approx <- pracma::chebApprox(zscore_grid, f_sd_zscore, zscore_range[1], zscore_range[2], degree)
            error <- max(abs(f_true-f_approx))  
            if(error < approx_tol_sd || degree>max_degree ) {
                break
            }
        }
        # determine Chebyshev polynomial coefficients
        coef_cheb <-  pracma::chebCoeff(f_sd_zscore, zscore_range[1], zscore_range[2], degree)
        poly_cheb <- pracma::chebPoly(degree)
        coef <- rev(drop(coef_cheb %*% poly_cheb))
        coef[1] <- coef[1] - coef_cheb[1]/2
        result$sd <- list(degree = degree, 
                          zscore = zscore_grid, 
                          true = f_true, 
                          approx = f_approx,
                          coefficients = coef)
        # determine approximation quality of the derivatives
        poly_sigma <- rev(result$sd$coefficients)
        poly_dsigma_dscore <- pracma::polyder(poly_sigma)
        score_t  <-  (2*zscore_grid-(zscore_range[2]+zscore_range[1]))/(zscore_range[2]-zscore_range[1])
        result$sd$deriv_approx <- pracma::polyval(poly_dsigma_dscore, score_t)*2/(zscore_range[2]-zscore_range[1])
        result$sd$deriv_true <- pracma::fderiv(f_sd_zscore, zscore_grid)
        return(result)
    }
    # determine the polynomial degree necessary to approx mean fun with requested tol
    degree <- 0
    f_true <- f_mu(psi_grid)
    repeat{
        degree <- degree+1
        f_approx <- pracma::chebApprox(psi_grid, f_mu, psi_range[1], psi_range[2], degree)
        error <- max(abs(f_true-f_approx))
        if(error < approx_tol_mean || degree>max_degree ) {
            break
        }
    }
    # determine Chebyshev polynomial coefficients
    coef_cheb <-  pracma::chebCoeff(f_mu, psi_range[1], psi_range[2], degree)
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
        if(error < approx_tol_sd || degree>max_degree ) {
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
    # determine approximation quality of the derivatives
    poly_mu <- rev(result$mean$coefficients)
    poly_sigma <- rev(result$sd$coefficients)
    poly_dmu_dpsi <- pracma::polyder(poly_mu)
    poly_dsigma_dpsi <- pracma::polyder(poly_sigma)
    psi_t  <-  (2*psi_grid-(psi_range[2]+psi_range[1]))/(psi_range[2]-psi_range[1])
    result$mean$deriv_approx <- pracma::polyval(poly_dmu_dpsi, psi_t)*2/(psi_range[2]-psi_range[1])
    result$mean$deriv_true <- pracma::fderiv(f_mu, psi_grid)
    result$sd$deriv_approx <- pracma::polyval(poly_dsigma_dpsi, psi_t)*2/(psi_range[2]-psi_range[1])
    result$sd$deriv_true <- pracma::fderiv(f_sd, psi_grid)
    return(result)
}



#' @export
#' @rdname calculate_cv_irt_link
plot_irt_link <- function(res, plot_derivatives=FALSE){
    df <- res[c("mean","sd")] %>% 
        purrr::map(`[`, c("approx","true",res$idv)) %>% 
        purrr::map_dfr(tibble::as_tibble, 
                       .id = "stat") %>% 
        tidyr::pivot_longer(cols = c("true", "approx"))
    
    if(plot_derivatives){
        df_deriv <- res[c("mean","sd")] %>% 
            purrr::map(`[`, c("deriv_approx","deriv_true",res$idv)) %>% 
            purrr::map_dfr(tibble::as_tibble, 
                           .id = "stat") %>% 
            dplyr::rename(approx = "deriv_approx", true = "deriv_true") %>% 
            tidyr::pivot_longer(cols = c("approx", "true")) 
        
        df <- dplyr::bind_rows(
            `function` = df,
            `derivative` = df_deriv,
            .id = "type"
        ) %>% 
            dplyr::mutate(
                type = factor(.data$type, levels = c("function", "derivative")),
                stat = factor(.data$stat, levels = c("mean", "sd"), labels = sprintf(c("mean(%s)", "sd(%s)"), res$type))
            )
    }
    df <- df %>% 
        dplyr::mutate(name = factor(.data$name, levels = c("true", "approx")),
                      )
    idv <- rlang::sym(res$idv)
    p <- ggplot2::ggplot(df, aes(!!idv, value, color = name, linetype = name)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::scale_linetype_manual("", values = c(true = "solid", approx = 'dashed'))+
        ggplot2::scale_color_discrete("") +
        ggplot2::theme(legend.position = "top")
    if(plot_derivatives){
        p <- p + 
            ggplot2::facet_grid(rows = ggplot2::vars(stat), cols = ggplot2::vars(type), scales = "free")
    }else{
        p <- p + 
            ggplot2::facet_wrap(ggplot2::vars(stat), scales = "free")
    }
    p
}

nm_polynom <- function(poly_coef, digits = 6){
    purrr::imap_chr(poly_coef, ~glue::glue("{round(.x, digits)}*TLV**{.y-1}")) %>% 
        purrr::reduce(~paste(.x, .y, sep = ifelse(startsWith(.y,"-"),"","+"))) %>% 
        sub(x = ., pattern = "*TLV**0", replacement = "", fixed = TRUE) %>% 
        sub(x = ., pattern = "TLV**1", replacement = "TLV", fixed = TRUE)
}

nm_range_transform <- function(res, variable = "LV", digits = 6){
    glue::glue("TLV = (2*{variable}-({b+a}))/{b-a}", 
               a = round(res$range[1], digits), 
               b = round(res$range[2], digits))
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

#' Calculate latent variable information for an IRT-informed CV model
#'
#' @param res Result object from an IRT link-analysis
#' @param psi_range Latent variable range
#'
#' @return A tibble
#' @export
calculate_cv_irt_information <- function(res, psi_range = c(-4, 4)){
    # polynomials for mean and SD
    poly_mu <- rev(res$mean$coefficients)
    poly_sigma <- rev(res$sd$coefficients)
    # polynomials for derivatives of mean and SD
    poly_dmu_dpsi <- pracma::polyder(poly_mu)
    poly_dsigma_dpsi <- pracma::polyder(poly_sigma)
    f_information <- function(psi) {
        psi_t <- (2*psi-(res$range[2]+res$range[1]))/(res$range[2]-res$range[1])
        sigma <- pracma::polyval(poly_sigma, psi_t)
        sigma2 <- sigma^2
        # derivative of mu w.r.t. ps
        d_mu_dpsi <- pracma::polyval(poly_dmu_dpsi, psi_t)*2/(res$range[2]-res$range[1])
        d_sigma_dpsi <- pracma::polyval(poly_dsigma_dpsi, psi_t)*2/(res$range[2]-res$range[1])
        d_sigma2_dpsi <- 2*sigma*d_sigma_dpsi
        d_mu_dpsi^2/sigma2+0.5*d_sigma2_dpsi^2/sigma2^2    
    }
    tibble::tibble(
        psi = seq(psi_range[1], psi_range[2], length.out = 100),
        information = f_information(psi)
    )
}


#' Calculate latent variable information for an IRT-informed BI model
#'
#' @param res Result object from an IRT link-analysis
#' @param psi_range Latent variable range
#'
#' @return A tibble
#' @export
calculate_bi_irt_information <- function(res, psi_range = c(-4,4)){
    # polynomials for mean and SD
    poly_mu <- rev(res$mean$coefficients)
    poly_sigma <- rev(res$sd$coefficients)
    # polynomials for derivatives of mean and SD
    poly_dmu_dpsi <- pracma::polyder(poly_mu)
    poly_dsigma_dpsi <- pracma::polyder(poly_sigma)
    
    prob_bi <- function(y, psi, ymax){
        q_lower <- qnorm(y/(ymax+1))
        q_upper <- qnorm((y+1)/(ymax+1))
        psi_t <- (2*psi-(res$range[2]+res$range[1]))/(res$range[2]-res$range[1])
        sigma <- pracma::polyval(poly_sigma, psi_t)
        mu <- pracma::polyval(poly_mu, psi_t)
        z_upper <- (q_upper - mu)/sigma
        z_lower <- (q_lower - mu)/sigma
        if(z_lower>0&&z_upper>0){
            tmp <- z_lower
            z_lower <- -z_upper
            z_upper <- -tmp
        }
        pnorm(z_upper)-pnorm(z_lower)
    }
    
    # d_loglike <- function(y, psi, ymax){
    #     z_lower <- qnorm(y/(ymax+1))
    #     z_upper <- qnorm((y+1)/(ymax+1))
    #     psi_t <- (2*psi-(res$range[2]+res$range[1]))/(res$range[2]-res$range[1])
    #     sigma <- pracma::polyval(poly_sigma, psi_t)
    #     mu <- pracma::polyval(poly_mu, psi_t)
    #     # derivative of mu w.r.t. psi
    #     d_mu_dpsi <- pracma::polyval(poly_dmu_dpsi, psi_t)*2/(res$range[2]-res$range[1])
    #     d_sigma_dpsi <- pracma::polyval(poly_dsigma_dpsi, psi_t)*2/(res$range[2]-res$range[1])
    #     
    #     1/prob_bi(y, psi, ymax)*(-dnorm(z_upper, mu, sigma)+dnorm(z_lower, mu, sigma))*((mu-psi)*d_sigma_dpsi-sigma*d_mu_dpsi)/sigma^2
    # }
    
   
    
    log_prob_bi <- function(y, psi, ymax){
        q_lower <- qnorm(y/(ymax+1))
        q_upper <- qnorm((y+1)/(ymax+1))
        psi_t <- (2*psi-(res$range[2]+res$range[1]))/(res$range[2]-res$range[1])
        sigma <- pracma::polyval(poly_sigma, psi_t)
        mu <- pracma::polyval(poly_mu, psi_t)
        z_upper <- (q_upper - mu)/sigma
        z_lower <- (q_lower - mu)/sigma
        if(z_lower>0 && z_upper>0){
            tmp <- z_lower
            z_lower <- -z_upper
            z_upper <- -tmp
        }
        lpz_lower <- pnorm(z_lower, lower.tail = TRUE, log.p = TRUE)
        lpz_upper <- pnorm(z_upper, lower.tail = TRUE, log.p = TRUE)
        ifelse(lpz_upper>lpz_lower, 
               lpz_upper+log1p(-exp(lpz_lower-lpz_upper)),
               lpz_lower+log1p(-exp(lpz_upper-lpz_lower)))
    }
    
    
    d_loglike <- function(y, psi, ymax) {
        pracma::fderiv(function(x) log_prob_bi(y, x, ymax), psi)
    }
    
    
    fi <- function(psi, ymax){
        sum(purrr::map_dbl(seq_len(ymax+1)-1, ~prob_bi(.x, psi, ymax)*d_loglike(.x, psi, ymax)^2))
    }
    tibble::tibble(
        psi = seq(psi_range[1], psi_range[2], length.out = 100),
        information = purrr::map_dbl(.data$psi, ~fi(., 70))
    )
}
