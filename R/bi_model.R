#' Create a bounded integer model object
#' 
#' \code{bi_model} returns a newly created model object
#'
#' @param scale_or_min A scale object or a minimum total score
#' @param max A maximum total score
#' @param irt_link Results of an IRT link analysis
#' @return A model object
#' @export
bi_model <- function(scale_or_min, max=NULL, irt_link = NULL) {
    model <- base_model(subclass="bi_model")
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
    model$irt_link <- irt_link
    model
}

#' @export
set_dataset.bi_model <- function(model, path, use_path=TRUE, data_columns=NULL, mdv_column) {
    model <- NextMethod()
    if("ITEM" %in% model$data_columns && !is.null(model$scale)){
        message("Note: IGNORE statements were added to filter item-level entries in total score model.")
        stms <- purrr::map(all_items(model$scale), ~sprintf("IGNORE=(ITEM.EQN.%i)", .x))
        model$ignore_statements <- c(model$ignore_statements, stms) 
    }
    return(model)
}

#' @export
model_code.bi_model <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "$SIZES DIMNEW=10000")
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, data_and_input_code(model))
    if(is.null(model$irt_link)){
        cg <- add_code(cg, default_bi_model())
    }else{
        if(model$irt_link$idv == "psi"){
            cg <- add_code(cg, default_irt_based_bi_model(model))
        }else{
            cg <- add_code(cg, default_irt_score_based_bi_model(model))
        }
    }
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
    if(is.null(model$irt_link)){
        cg <- add_code(cg, default_bi_parameters())
    }else{
        cg <- add_code(cg, default_irt_based_bi_parameters())
    }
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

default_irt_based_bi_model <- function(model){
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "MU_1 = THETA(1)")
    cg <- add_line(cg, "MU_2 = THETA(2)")
    cg <- add_line(cg, "BASE = THETA(1) + ETA(1)")
    cg <- add_line(cg, "SLOPE = THETA(2) + ETA(2)")
    cg <- add_line(cg, "LV = BASE + SLOPE*TIME")
    cg <- add_line(cg, nm_range_transform(model$irt_link))
    cg <- add_line(cg, "IPRED =", nm_polynom(model$irt_link$mean$coefficients))
    cg <- add_line(cg, "SD =", nm_polynom(model$irt_link$sd$coefficients))
    cg
}

default_irt_score_based_bi_model <- function(model){
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "MU_1 = THETA(1)")
    cg <- add_line(cg, "MU_2 = THETA(2)")
    cg <- add_line(cg, "BASE = THETA(1) + ETA(1)")
    cg <- add_line(cg, "SLOPE = THETA(2) + ETA(2)")
    cg <- add_line(cg, "IPRED = BASE + SLOPE*TIME")
    cg <- add_line(cg, nm_range_transform(model$irt_link, variable = "IPRED"))
    cg <- add_line(cg, "SD =", nm_polynom(model$irt_link$sd$coefficients))
    cg <- add_line(cg, "IF(TLV.LT.-1) SD = ", model$irt_link$sd$true[1])
    cg <- add_line(cg, "IF(TLV.GT.1) SD = ", model$irt_link$sd$true[length(model$irt_link$sd$true)])
    cg
}

default_bi_model <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "MU_1 = THETA(1)")
    cg <- add_line(cg, "MU_2 = THETA(2)")
    cg <- add_line(cg, "BASE = THETA(1) + ETA(1)")
    cg <- add_line(cg, "SLOPE = THETA(2) + ETA(2)")
    cg <- add_line(cg, "IPRED = BASE + SLOPE*TIME")
    cg <- add_line(cg, "SD = THETA(3)")
    cg
}

default_irt_based_bi_parameters <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$THETA 0.01  ; TVBASE")
    cg <- add_line(cg, "$THETA 0.01  ; TVSLOPE")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$OMEGA 1  ; IIVBASE")
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVSLOPE")
    cg
}



default_bi_parameters <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$THETA 0.01  ; TVBASE")
    cg <- add_line(cg, "$THETA 0.01  ; TVSLOPE")
    cg <- add_line(cg, "$THETA (0,1) ; SD")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVBASE")
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVSLOPE")
    cg
}



bi_simulation_code <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "Simulation code")
    cg <- add_line(cg, "IF (ICALL.EQ.4) THEN")
    cg <- increase_indent(cg)
    cg <- add_line(cg, paste0("PLE", model$min, " = P", model$min))
    ples <- (model$min + 1):(model$max - 1)
    cg <- add_lines(cg, paste0("PLE", ples, " = PLE", ples - 1, " + P", ples))
    cg <- add_line(cg, "CALL RANDOM(2, R)")
    cg <- add_line(cg, paste0("DV = ", model$max))
    for (i in rev(seq(model$min, model$max - 1))) {
        cg <- add_line(cg, paste0("IF(R.LE.PLE", i, ") DV = ", i))
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}


bi_estimation <- function(model) {
    cg <- code_generator() %>% 
        add_line(";Sim_start for VPC") %>% 
        add_line("$ESTIMATION METHOD=IMP LAPLACE LIKE AUTO=1 PRINT=1", " ", model$estimaton_options) %>% 
        add_line(";$SIMULATION (", sample.int(2147483647, 1),") (", sample.int(2147483647, 1)," UNI) ONLYSIM NOPRED") %>% 
        add_line(";Sim_end for VPC")
    cg
}