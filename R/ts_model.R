#' Create a total score model object
#' 
#' \code{ts_model} returns a newly created model object
#'
#' @param scale_or_min A scale object or a minimum total score
#' @param max A maximum total score
#' @return A model object
#' @export
ts_model <- function(scale_or_min, max=NULL) {
    model <- base_model(subclass="ts_model")
    if (is.numeric(scale_or_min)) {
        model$min <- scale_or_min
        stopifnot(is.numeric(max))
        model$max <- max
    } else {
        stopifnot(is.irt_scale(scale_or_min))
        model$scale <- scale_or_min
        min_max <- total_score_range(scale_or_min)
        model$min <- min_max[1]
        model$max <- min_max[2]
    }
    model
}

#' @export
model_code.ts_model <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, data_and_input_code(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_ts_model())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_ts_parameters(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, ts_estimation(model))
    get_code(cg)
}

default_ts_model <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "BASE = THETA(1) * EXP(ETA(1))")
    cg <- add_line(cg, "SLOPE = THETA(2) * EXP(ETA(2))")
    cg <- add_line(cg, "IPRED = BASE + SLOPE*TIME")
    cg <- add_line(cg, "Y = IPRED + EPS(1)")
    cg
}

default_ts_parameters <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("$THETA (", model$min, ",", (model$max + model$min) / 2, ",", model$max, ")  ; TVBASE"))
    cg <- add_line(cg, "$THETA 0.01  ; TVSLOPE")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVBASE")
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVSLOPE")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$SIGMA 1")
    cg
}

ts_estimation <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("$ESTIMATION METHOD=COND MAXEVAL=999999 PRINT=1", " ", model$estimaton_options))
    cg
}