#' Create a continuous variable model object
#' 
#' \code{cv_model} returns a newly created model object
#'
#' @param scale_or_min A scale object or a minimum total score
#' @param max A maximum total score
#' @param cv_irt_link A link object
#' @return A model object
#' @export
cv_model <- function(scale_or_min, max=NULL, cv_irt_link = NULL) {
    model <- base_model(subclass="cv_model")
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
    model$cv_irt_link <- cv_irt_link
    model
}

#' @export
set_dataset.cv_model <- function(model, path, use_path=TRUE, data_columns=NULL, mdv_column) {
    model <- NextMethod()
    if("ITEM" %in% model$data_columns && !is.null(model$scale)){
        message("Note: IGNORE statements were added to filter item-level entries in total score model.")
        stms <- purrr::map(all_items(model$scale), ~sprintf("IGNORE=(ITEM.EQN.%i)", .x))
        model$ignore_statements <- c(model$ignore_statements, stms) 
    }
    return(model)
}

#' @export
model_code.cv_model <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, data_and_input_code(model))
    cg <- add_empty_line(cg)
    if(!is.null(model$cv_irt_link)){
        cg <- add_code(cg, default_irt_based_cv_model(model))
    }else{
        cg <- add_code(cg, default_cv_model())
    }
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_cv_parameters(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, cv_estimation(model))
    get_code(cg)
}

default_irt_based_cv_model <- function(model){
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "BASE = THETA(1) * ETA(1)")
    cg <- add_line(cg, "SLOPE = THETA(2) * ETA(2)")
    cg <- add_empty_line(cg)
    cg <- add_code(cg, get_nm_cv_irt_link(model$cv_irt_link))
    cg
}

default_cv_model <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "BASE = THETA(1) * EXP(ETA(1))")
    cg <- add_line(cg, "SLOPE = THETA(2) * EXP(ETA(2))")
    cg <- add_line(cg, "IPRED = BASE + SLOPE*TIME")
    cg <- add_line(cg, "Y = IPRED + EPS(1)")
    cg
}

default_cv_parameters <- function(model) {
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

cv_estimation <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("$ESTIMATION METHOD=COND INTER MAXEVAL=999999 PRINT=1", " ", model$estimaton_options))
    cg
}