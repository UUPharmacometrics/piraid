combine_dist <- function(scores1, probs1, scores2, probs2) {
  sums <- outer(scores1, scores2, "+")
  probs <- outer(probs1, probs2, "*")
  out <- tapply(
    probs,
    INDEX = as.character(sums),
    FUN = sum)
  list(
    scores = as.numeric(names(out)),
    probs = as.numeric(out)
  )
}

# the function calculates the probability mass function for the total score 
# given psi and item_lables
# the output is matrix 
# rows - number of psi values; columns - number of unique total score values
pmf_ts_labels <- function(model, psi, item_labels){
  if(inherits(model, "SingleGroupClass")){
    mirt_model <- model
  }else{
    mirt_model <- as_mirt_model(model)
  }
  
  possible_scores = sort(unique(Reduce(
    function(x, y) unique(round(as.vector(outer(x, y, "+")), 6)),
    item_labels)))
  
  item_names <- extract.mirt(model, "itemnames")
  prob_trace <- lapply(item_names, function(nm) {
    probtrace(extract.item(model, nm), as.matrix(psi))})
  names(prob_trace) <- item_names
  
  n_psi <- length(psi)
  
  score_probs <- matrix(
    nrow = n_psi,
    ncol = length(possible_scores),
    dimnames = list(NULL, possible_scores)
  )
  
  for(th in seq_len(n_psi)) {
    dist <- list(
      scores = item_labels[[1]],
      probs = prob_trace[[1]][th, ]
    )
    for(i in 2:length(item_labels)) {
      dist <- combine_dist(dist$scores,
                           dist$probs,
                           item_labels[[i]],
                           prob_trace[[i]][th, ])
    }
    idx <- match(dist$scores, possible_scores)
    score_probs[th, idx] <- dist$probs
  }
  colnames(score_probs) <- paste0("V", seq_len(ncol(score_probs)))
  rownames(score_probs) <- paste0("V", seq_len(nrow(score_probs)))
  return(score_probs)
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
#' @param item_labels - list in the form list("ITEM_1"=c(...), "ITEM_2"=c(...),..., "ITEM_N"=c(...))
#'
#' @return
#' @export
calculate_cv_irt_link <- function(model, item_labels = NULL,
                                  psi_range = NULL, 
                                  score_range = c(-Inf, Inf),
                                  lv_based = TRUE, 
                                  range_tol = 0.3,
                                  approx_tol_mean = 0.1,
                                  approx_tol_sd  = 0.01, 
                                  max_degree = 100){
  
  if(is.null(item_labels)){
    # # The default attribution 
    # # list("ITEM_1"=c(0,1,2...,m1), "ITEM_2"=c(0,1,2,..,m2),..., "ITEM_N"=c(0,1,2,...,mn))
    item_labels <- setNames(
      lapply(model$scale$items, function(item) {
        0:(length(item$levels) - 1)
      }),paste0("ITEM_", seq_along(model$scale$items))
    )
    
  } else {
    
    # check if all ITEMs are supplied with labels
    if(length(model$scale$items)!=length(item_labels)){
      stop(paste0("Numbers of items in the model and in the list of labels do not match: ",
                  length(model$scale$items), " items in the model, but ",
                  length(item_labels), " items in the list of labels"),
           call. = FALSE)
    }
    
    # check if all levels of each item have new labels
    mismatch <- vapply(
      c(1:length(model$scale$items)),
      function(item) {length(model$scale$items[[item]]$levels) != length(item_labels[[paste0("ITEM_",item)]])},
      logical(1))
    if (any(mismatch)) {
      stop(paste0("Mismatch in the number of labels for: ",
                  paste("ITEM_", c(1:length(model$scale$items))[mismatch], collapse = ", ", sep="")),
           call. = FALSE)
    }
  }
  
  mirt_model <- as_mirt_model(model)
  result <- list()
  result$type <- "score"
  result$idv <- ifelse(lv_based, "psi", "score")
  
  # function calculating irt_ts the mean score
  f_mean <- function(x, item_labels_values){
    item_probs = mirt::expected.test(mirt_model, x,
                                     individual=TRUE, probs.only=TRUE)
    vec_labels = unlist(item_labels_values, use.names = FALSE)
    return(rowSums(item_probs * rep(vec_labels, 
                                    each = nrow(item_probs))))
  }
  
  # function calculating the sd score
  f_sd <- function(psi, item_labels_values){
    prob <- purrr::map(seq_along(get_mirt_names(model)), 
                       ~mirt::extract.item(mirt_model, .x)) %>%
      purrr::map(~mirt::probtrace(.x, psi))
    item_names <- get_mirt_names(model)
    escore <- purrr::map(seq_along(item_names), function(i) {
      item <- mirt::extract.item(mirt_model, i)
      labels <- item_labels_values[[item_names[i]]]
      probs <- mirt::probtrace(item, Theta = psi)
      as.vector(probs %*% labels)
    })
    item_levels <- purrr::map(item_labels_values, ~
                                matrix(.x, length(psi), length(.x), byrow = TRUE))
    sd_score <- purrr::map2(item_levels, escore, ~.x-.y) %>% 
      purrr::map2(prob, ~rowSums(.y*.x^2)) %>% 
      purrr::reduce(`+`) %>% 
      sqrt
    return(sd_score)
  }
  
  if(is.null(psi_range)){
    # determine psi range for approximation
    max_range <- c(sum(sapply(item_labels, min)), sum(sapply(item_labels, max)))
    if(!is.finite(score_range[1])) score_range[1] <- max_range[1]
    if(!is.finite(score_range[2])) score_range[2] <- max_range[2]
    psi_interval <- c(-50, 50)
    # solve score_min+range_tol = f_mean(psi) using root finder
    sol_min <- uniroot(function(psi) f_mean(psi, item_labels) - score_range[1] - range_tol, psi_interval)
    # solve score_max-range_tol = f_mean(psi) using root finder
    sol_max <- uniroot(function(psi) f_mean(psi, item_labels) - score_range[2] + range_tol, psi_interval)
    psi_range <- c(sol_min$root, sol_max$root)
  }
  
  psi_grid <- seq(psi_range[1], psi_range[2], length.out = 100)
  
  # determine the polynomial degree necessary to approx mean fun with requested tol
  degree <- 0
  # # f_true <- f_mean(psi_grid)
  f_true <- f_mean(psi_grid, item_labels)
  result$range <- psi_range
  repeat{
    degree <- degree+1
    # # f_approx <- pracma::chebApprox(psi_grid, f_mean, psi_range[1], psi_range[2], degree)
    f_approx <- pracma::chebApprox(psi_grid, 
                                   function(x) f_mean(x, item_labels), 
                                   psi_range[1], psi_range[2], degree)
    error <- max(abs(f_true-f_approx))  
    if(error < approx_tol_mean || degree>max_degree ) {
      break
    }
  }
  # determine Chebyshev polynomial coefficients
  # # coef_cheb <-  pracma::chebCoeff(f_mean, psi_range[1], psi_range[2], degree)
  coef_cheb <-  pracma::chebCoeff(function(x) f_mean(x, item_labels),  
                                  psi_range[1], psi_range[2], degree)
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
  # # f_true <- f_sd(psi_grid)
  f_true <- f_sd(psi_grid, item_labels)
  repeat{
    degree <- degree+1
    # # f_approx <- pracma::chebApprox(psi_grid, f_sd, psi_range[1], psi_range[2], degree)
    f_approx <- pracma::chebApprox(psi_grid, 
                                   function(x) f_sd(x, item_labels), 
                                   psi_range[1], psi_range[2], degree)
    error <- max(abs(f_true-f_approx))  
    if(error < approx_tol_sd || degree>max_degree ) {
      break
    }
  }
  # determine Chebyshev polynomial coefficients
  # # coef_cheb <-  pracma::chebCoeff(f_sd, psi_range[1], psi_range[2], degree)
  coef_cheb <-  pracma::chebCoeff(function(x) f_sd(x, item_labels), 
                                  psi_range[1], psi_range[2], degree)
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
  # # result$mean$deriv_true <- pracma::fderiv(f_mean, psi_grid)
  result$mean$deriv_true <- pracma::fderiv(function(x) f_mean(x, item_labels), 
                                           psi_grid)
  result$sd$deriv_approx <- pracma::polyval(poly_dsigma_dpsi, psi_t)*2/(psi_range[2]-psi_range[1])
  # # result$sd$deriv_true <- pracma::fderiv(f_sd, psi_grid)
  result$sd$deriv_true <- pracma::fderiv(function(x) f_sd(x, item_labels), 
                                         psi_grid)
  
  return(result)
}



#' @rdname calculate_bi_irt_link
#' @param item_labels - list in the form list("ITEM_1"=c(...), "ITEM_2"=c(...),..., "ITEM_N"=c(...)) 
#' @param corr_factor - correction factor for the total score computation 
#' @param sim_dafault - keep the simulation as default through the loop (TRUE), or define with IF statements (FALSE)
#' @export
calculate_bi_irt_link <- function(model, 
                                  psi_range = NULL, 
                                  score_range = c(-Inf, Inf),
                                  lv_based = FALSE,
                                  range_tol = 0.3,
                                  approx_tol_mean = 0.1,
                                  approx_tol_sd  = 0.01, 
                                  max_degree = 100,
                                  item_labels = NULL,
                                  corr_factor = 1,
                                  sim_default = TRUE){
    
  if(is.null(item_labels)){
    # # The default attribution 
    # # list("ITEM_1"=c(0,1,2...,m1), "ITEM_2"=c(0,1,2,..,m2),..., "ITEM_N"=c(0,1,2,...,mn))
    item_labels <- setNames(
      lapply(model$scale$items, function(item) {
        0:(length(item$levels) - 1)
      }),paste0("ITEM_", seq_along(model$scale$items))
    )
    
  } else {
    
    # check if all ITEMs are supplied with labels
    if(length(model$scale$items)!=length(item_labels)){
      stop(paste0("Numbers of items in the model and in the list of labels do not match: ",
                  length(model$scale$items), " items in the model, but ",
                  length(item_labels), " items in the list of labels"),
           call. = FALSE)
    }
    
    # check if all levels of each item have new labels
    mismatch <- vapply(
      c(1:length(model$scale$items)),
      function(item) {length(model$scale$items[[item]]$levels) != length(item_labels[[paste0("ITEM_",item)]])},
      logical(1))
    if (any(mismatch)) {
      stop(paste0("Mismatch in the number of labels for: ",
                  paste("ITEM_", c(1:length(model$scale$items))[mismatch], collapse = ", ", sep="")),
           call. = FALSE)
    }
  }
  
  mirt_model <- as_mirt_model(model)
    result <- list()
    result$type <- "zscore"
    result$idv <- ifelse(lv_based, "psi", "zscore")
    # function calculatingirt_ts the mean score
    f_mean <- function(x, item_labels_values){
      item_probs = mirt::expected.test(mirt_model, x,
                                       individual=TRUE, probs.only=TRUE)
      vec_labels = unlist(item_labels_values, use.names = FALSE)
      return(rowSums(item_probs * rep(vec_labels, 
                                      each = nrow(item_probs))))
    }
    # function calculating the mean z-score
    f_mu_labels <- function(model, psi, item_labels){
      pmf <- pmf_ts_labels(model, psi, item_labels) 
      scores <- matrix(seq(0, ncol(pmf)-1), nrow = 1)
      mx <- max(scores)
      pscores <- (scores+0.5)/(mx+1)
      zscores <- drop(qnorm(pscores))
      drop(pmf %*% zscores)
    }
    # function calculating the sd z-score
    f_sd_labels <- function(model, psi, item_labels){
      pmf <- pmf_ts_labels(model, psi, item_labels)
      scores <- matrix(seq(0, ncol(pmf)-1), nrow = 1)
      mx <- max(scores)
      pscores <- (scores+0.5)/(mx+1)
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
    
    # determine the polynomial degree necessary to approx mean fun with requested tol
    degree <- 0
    f_true <- f_mu_labels(mirt_model, psi_grid, item_labels)
    repeat{
      degree <- degree+1
      f_approx <- pracma::chebApprox(psi_grid, 
                                     function(psi) f_mu_labels(mirt_model, psi, item_labels), 
                                     psi_range[1], psi_range[2], degree)
      error <- max(abs(f_true-f_approx))
      if(error < approx_tol_mean || degree>max_degree ) {
        break
      }
    }
    # determine Chebyshev polynomial coefficients
    coef_cheb <-  pracma::chebCoeff(function(psi) f_mu_labels(mirt_model, psi, item_labels), 
                                    psi_range[1], psi_range[2], degree)
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
    f_true <- f_sd_labels(mirt_model, psi_grid, item_labels)
    repeat{
      degree <- degree+1
      f_approx <- pracma::chebApprox(psi_grid, 
                                     function(psi) f_sd_labels(mirt_model, psi, item_labels), 
                                     psi_range[1], psi_range[2], degree)
      error <- max(abs(f_true-f_approx))  
      if(error < approx_tol_sd || degree>max_degree ) {
        break
      }
    }
    # determine Chebyshev polynomial coefficients
    coef_cheb <-  pracma::chebCoeff(function(psi) f_sd_labels(mirt_model, psi, item_labels), 
                                    psi_range[1], psi_range[2], degree)
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
    result$mean$deriv_true <- pracma::fderiv(function(psi) f_mu_labels(mirt_model, psi, item_labels), 
                                             psi_grid)
    result$sd$deriv_approx <- pracma::polyval(poly_dsigma_dpsi, psi_t)*2/(psi_range[2]-psi_range[1])
    result$sd$deriv_true <- pracma::fderiv(function(psi) f_sd_labels(mirt_model, psi, item_labels), 
                                           psi_grid)
    
    result$item_labels = item_labels
    result$corr_factor = corr_factor
    result$sim_default = sim_default
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



