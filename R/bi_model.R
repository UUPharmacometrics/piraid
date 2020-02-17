#' Create a bounded integer model object
#' 
#' \code{bi_model} returns a newly created model object
#'
#' @param scale_or_levels A scale object or a total number of levels
#' @return A model object
#' @export
bi_model <- function(scale_or_levels) {
    if (is.numeric(scale_or_levels)) {
        scale_levels <- scale_or_levels
    } else {
        stopifnot(is.irt_scale(scale_or_levels))
        scale_levels <- number_of_levels(scale_or_levels)
    }
    model <- base_model(subclass="bi_model")
    model$levels <- scale_levels
    
    model
}

#' @export
model_code.bi_model <- function(model) {
    cg <- code_generator()
    cg <- add_code(cg, cutoffs(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, probability_functions(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, set_predictions(model))
    get_code(cg)
}

cutoffs <- function(bi_model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "BI cut-offs")
    n <- bi_model$levels
    cuts <- stats::qnorm(1:(n-1)/n)
    nums <- 1:(n-1)
    lines <- paste0("CO", nums, " = ", cuts)
    cg <- add_lines(cg, lines)
    cg
}

probability_functions <- function(bi_model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "Probability functions for all scores")
    cg <- add_line(cg, "P0 = PHI((CO1 - IPRED) / SD)")
    for (i in seq(1, bi_model$levels - 2)) {
        cg <- add_line(cg, paste0("P", i, " = PHI((CO", i + 1, " - IPRED) / SD) - PHI((CO", i, " - IPRED) / SD)"))
    }
    cg <- add_line(cg, paste0("P", bi_model$levels - 1, " = 1 - PHI((CO", bi_model$levels - 1, " - IPRED) / SD)"))
    cg
}

set_predictions <- function(bi_model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "Setting predictions")
    n <- bi_model$levels
    dvs <- 0:(n-1)
    lines <- paste0("IF(DV.EQ.", dvs, ") P = P", dvs)
    cg <- add_lines(cg, lines)
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "IF(P.LT.1E-16) P = 1E-16")
    cg <- add_line(cg, "Y = P")
    cg
}