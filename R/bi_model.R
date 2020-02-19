#' Create a bounded integer model object
#' 
#' \code{bi_model} returns a newly created model object
#'
#' @param scale_or_min A scale object or a minimum total score
#' @param max A maximum total score
#' @return A model object
#' @export
bi_model <- function(scale_or_min, max=NULL) {
    model <- base_model(subclass="bi_model")
    if (is.numeric(scale_or_min)) {
        model$min <- scale_or_min
        stopifnot(is.numeric(max))
        model$max <- max
    } else {
        stopifnot(is.irt_scale(scale_or_min))
        min_max <- total_score_range(scale_or_min)
        model$min <- min_max[1]
        model$max <- min_max[2]
    }
    model
}

#' @export
model_code.bi_model <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, data_and_input_code(model))
    cg <- add_code(cg, default_bi_model())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, cutoffs(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, probability_functions(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, set_predictions(model))
    cg <- add_empty_line(cg)
    cg <- add_line(cg, ppred_code(model$min:model$max))
    cg <- add_line(cg, sdpred_code(model$min:model$max))
    cg <- add_line(cg, pwres_code())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, bi_simulation_code(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_bi_parameters())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, bi_estimation(model))
    get_code(cg)
}

cutoffs <- function(bi_model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "BI cut-offs")
    n <- bi_model$max - bi_model$min + 1
    cuts <- stats::qnorm(1:(n-1)/n)
    nums <- 1:(n-1)
    lines <- paste0("CO", nums, " = ", cuts)
    cg <- add_lines(cg, lines)
    cg
}

probability_functions <- function(bi_model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "Probability functions for all scores")
    cg <- add_line(cg, paste0("P", bi_model$min, " = PHI((CO1 - IPRED) / SD)"))
    j <- 1
    for (i in seq(bi_model$min + 1, bi_model$max - 1)) {
        cg <- add_line(cg, paste0("P", i, " = PHI((CO", j + 1, " - IPRED) / SD) - PHI((CO", j, " - IPRED) / SD)"))
        j <- j + 1
    }
    cg <- add_line(cg, paste0("P", bi_model$max, " = 1 - PHI((CO", j, " - IPRED) / SD)"))
    cg
}

set_predictions <- function(bi_model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "Setting predictions")
    dvs <- bi_model$min:bi_model$max
    lines <- paste0("IF(DV.EQ.", dvs, ") P = P", dvs)
    cg <- add_lines(cg, lines)
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "IF(P.LT.1E-16) P = 1E-16")
    cg <- add_line(cg, "Y = P")
    cg
}

default_bi_model <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "BASE = THETA(1) + ETA(1)")
    cg <- add_line(cg, "SLOPE = THETA(2) + ETA(2)")
    cg <- add_line(cg, "IPRED = BASE + SLOPE*TIME")
    cg <- add_line(cg, "SD = THETA(3) * EXP(ETA(3))")
    cg
}

default_bi_parameters <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$THETA 0.01  ; TVBASE")
    cg <- add_line(cg, "$THETA 0.01  ; TVSLOPE")
    cg <- add_line(cg, "$THETA (0,0.01) ; TVSD")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVBASE")
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVSLOPE")
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVSD")
    cg
}

bi_simulation_code <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "Simulation code")
    cg <- add_line(cg, "IF(ICALL.EQ.4) THEN")
    cg <- increase_indent(cg)
    cg <- add_line(cg, paste0("PLE", model$min, " = P", model$min))
    ples <- (model$min + 1):(model$max - 1)
    cg <- add_lines(cg, paste0("PLE", ples, " = PLE", ples - 1, " + P", ples))
    cg <- add_line(cg, "CALL RANDOM(2, R)")
    cg <- add_line(cg, paste0("DV = ", model$max))
    for (i in seq(model$min, model$max - 1)) {
        cg <- add_line(cg, paste0("IF(R.LE.PLE", i, ") DV = ", i))
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}

bi_estimation <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("$ESTIMATION METHOD=COND LAPLACE LIKE MAXEVAL=999999 PRINT=1", " ", model$estimaton_options))
    cg
}