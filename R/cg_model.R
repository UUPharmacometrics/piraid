#' Create a coursened grid model object
#' 
#' \code{cg_model} returns a newly created model object
#'
#' @param scale_or_min A scale object or a minimum total score
#' @param max A maximum total score
#' @return A model object
#' @export
cg_model <- function(scale_or_min, max=NULL) {
    model <- base_model(subclass="cg_model")
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
model_code.cg_model <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, data_and_input_code(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_cg_model())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, cg_likelihood_model(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_cg_parameters(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, cg_estimation(model))
    get_code(cg)
}

default_cg_model <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "SIG = THETA(1)")
    cg <- add_line(cg, "BASE = THETA(2) * ETA(1)")
    cg <- add_line(cg, "ZPRED = BASE")
    cg
}

cg_likelihood_model <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("KMIN = ", model$min))
    cg <- add_line(cg, paste0("KMAX = ", model$max))
    cg <- add_line(cg, "K = DV - KMIN   ; Shift so min=0")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "AKL = (K - 0.5) / (KMAX - KMIN)")
    cg <- add_line(cg, "AKU = (K + 0.5) / (KMAX - KMIN)")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "ZKL = 0")
    cg <- add_line(cg, "ZKU = 0")
    cg <- add_line(cg, "IF (AKL.GT.0) ZKL = LOG(AKL/(1-AKL))")
    cg <- add_line(cg, "IF (AKU.LT.1) ZKU = LOG(AKU/(1-AKU))")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "IF (K.EQ.0) THEN")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "P = PHI((ZKU-ZPRED)/SIG)")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ELSE IF (K.EQ.(KMAX - KMIN)")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "P = 1 - PHI((ZKL-ZPRED)/SIG)")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ELSE")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "P = PHI((ZKU-ZPRED)/SIG) - PHI((ZKL-ZPRED)/SIG)")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "END IF")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "IF(P.LT.1E-16) P = 1E-16")
    cg <- add_line(cg, "Y = P")
    cg
}

default_cg_parameters <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "$THETA (0,1)  ; SIG")
    cg <- add_line(cg, "$THETA 1  ; TVBASE")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVBASE")
    cg
}

cg_estimation <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("$ESTIMATION METHOD=COND LAPLACE LIKE MAXEVAL=999999 PRINT=1", " ", model$estimaton_options))
    cg
}