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
    cg <- add_line(cg, "$ABBR DECLARE DOWHILE I")
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_cg_model())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, cg_likelihood_model(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, cg_simulation(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, default_cg_parameters(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, cg_estimation(model))
    get_code(cg)
}

default_cg_model <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "BASE = THETA(1) + ETA(1)")
    cg <- add_line(cg, "SLP = THETA(2) + ETA(2)")
    cg <- add_line(cg, "SIG = THETA(3)")
    cg <- add_line(cg, "ZPRED = BASE + SLP*TIME")
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
    cg <- add_line(cg, "P = 0")
    cg <- add_line(cg, "IF (K.EQ.0) P = PHI((ZKU - ZPRED) / SIG)")
    cg <- add_line(cg, "IF (K.EQ.(KMAX - KMIN)) P = 1 - PHI((ZKL-ZPRED)/SIG)")
    cg <- add_line(cg, "IF (K.GT.0.AND.K.LT.(KMAX-KMIN)) P = PHI((ZKU-ZPRED)/SIG) - PHI((ZKL-ZPRED)/SIG)")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "IF(P.LT.1E-16) P = 1E-16")
    cg <- add_line(cg, "Y = P")
    cg
}

cg_simulation <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "IF (ICALL.EQ.4) THEN")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "CALL RANDOM(2, R)")
    cg <- add_line(cg, "DONE = 0")
    cg <- add_line(cg, "I = 0")
    cg <- add_line(cg, "DOWHILE (I.LT.(KMAX - KMIN).AND.DONE.EQ.0)")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "AKU = (I + 0.5) / (KMAX - KMIN)")
    cg <- add_line(cg, "ZKU = LOG(AKU/(1-AKU))")
    cg <- add_line(cg, "P = PHI((ZKU-ZPRED)/SIG)")
    cg <- add_line(cg, "IF (R.LE.P) DONE = 1")
    cg <- add_line(cg, "I = I + 1")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDDO")
    cg <- add_line(cg, "IF (DONE.EQ.1) I =  I - 1")
    cg <- add_line(cg, "DV = I + KMIN")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "END IF")
    cg
}

default_cg_parameters <- function(model) {
    cg <- code_generator()
    cg <- add_line(cg, "$THETA 0.1  ; TVBASE")
    cg <- add_line(cg, "$THETA 0.1  ; TVSLP")
    cg <- add_line(cg, "$THETA (0,1)  ; SIG")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVBASE")
    cg <- add_line(cg, "$OMEGA 0.1  ; IIVSLP")
    cg
}

cg_estimation <- function(model) {
    cg <- code_generator() %>% 
        add_line(";Sim_start for VPC") %>% 
        add_line("$ESTIMATION METHOD=IMP LAPLACE LIKE AUTO=1 PRINT=1", " ", model$estimaton_options) %>% 
        add_line(";$SIMULATION (", sample.int(2147483647, 1),") (", sample.int(2147483647, 1)," UNI) ONLYSIM NOPRED") %>% 
        add_line(";Sim_end for VPC")
    cg
}