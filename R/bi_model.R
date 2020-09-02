#' Create a bounded integer model object
#' 
#' \code{bi_model} returns a newly created model object
#'
#' @param scale_or_min A scale object or a minimum total score
#' @param max A maximum total score
#' @param irt_link Results of an IRT link analysis
#' @param stable_log_like Should the stable log-likelihood implementation be used
#' @return A model object
#' @export
bi_model <- function(scale_or_min, 
                     max=NULL, 
                     irt_link = NULL, 
                     stable_log_like = FALSE) {
    model <- base_model(subclass = "bi_model")
    model$stable_log_like <- stable_log_like
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
    cg <- add_code(cg, data_and_input_code(model)) %>% 
        add_line("$ABBREVIATED DECLARE Z(", model$max - model$min + 1, ")") %>% 
        add_line("$ABBREVIATED DECLARE INTEGER I")
    
    if(is.null(model$irt_link)){
        cg <- add_code(cg, default_bi_model())
    }else{
        if(model$irt_link$idv == "psi"){
            cg <- add_code(cg, default_irt_based_bi_model(model))
        }else{
            cg <- add_code(cg, default_irt_score_based_bi_model(model))
        }
    }
    cg <- add_empty_line(cg) %>% 
        add_code(cutoffs(model)) %>% 
        add_empty_line() %>% 
        add_code(zscores(model)) %>% 
        add_empty_line() %>% 
        add_code(zscore_selection(model)) %>% 
        add_empty_line() 
    if (model$stable_log_like) {
        cg <- add_code(cg, bi_loglikelihood_improved(model))
    }else{
        cg <- add_code(cg, bi_loglikelihood(model)) 
    }    
    cg <- cg %>% 
        add_empty_line() %>% 
        add_code(bi_simulation_code(model)) %>% 
        add_empty_line()
    if (is.null(model$irt_link)) {
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

zscores <- function(bi_model) {
    n <- bi_model$max - bi_model$min + 1
    nums <- 1:(n - 1)
    lines <- paste0("Z(", nums, ") = ", "(CO", nums, "-IPRED)/SD")
    code_generator() %>% 
        banner_comment("BI z-scores") %>% 
        add_lines(lines)
}

zscore_selection <- function(bi_model) {
    n <- bi_model$max - bi_model$min + 1
    nums <- 1:(n - 1)
    lines_lower <- paste0("IF(DV.EQ.", nums, ") ZL = Z(", nums, ")")
    lines_upper <- paste0("IF(DV.EQ.", nums - 1, ") ZU = Z(", nums, ")")
    
    code_generator() %>% 
        banner_comment("z-score selection") %>% 
        add_lines(lines_lower) %>% 
        add_lines(lines_upper)
}

bi_loglikelihood <- function(bi_model) {
    code_generator() %>% 
        add_line("IF(DV.EQ.", bi_model$min, ") P = PHI(ZU)") %>% 
        add_line("IF(DV.GT.", bi_model$min, ".AND.DV.LT.", bi_model$max,") P = PHI(ZU) - PHI(ZL)") %>% 
        add_line("IF(DV.EQ.", bi_model$max, ") P = 1 - PHI(ZL)") %>% 
        add_empty_line() %>% 
        add_line("IF(P.LT.1E-16) P = 1E-16") %>% 
        add_line("Y = -2*LOG(P)")
}

bi_loglikelihood_improved <- function(bi_model) {
    code_generator() %>% 
        banner_comment("Numerically stable log-likelihood calculation") %>% 
        add_line("IF(DV.EQ.",bi_model$min,") ZL=ZU") %>% 
        add_line("IF(DV.EQ.",bi_model$max,") ZU=ZL") %>%
        add_empty_line() %>% 
        add_line("; Switch tail probabilities") %>% 
        add_line("IF(DV.EQ.",bi_model$min,") ZU=-ZU") %>% 
        add_line("IF(DV.GT.",bi_model$min,".AND.DV.LT.",bi_model$max,".AND.ZL.LT.0.AND.ZU.LT.0) THEN") %>% 
        increase_indent() %>% 
        add_line("TMP=ZL") %>% 
        add_line("ZL=-ZU") %>% 
        add_line("ZU=-TMP") %>% 
        decrease_indent() %>% 
        add_line("ENDIF") %>% 
        add_empty_line() %>% 
        add_line("; Calculate log(1-PHI)") %>% 
        add_line("LPHI_ZL=LOG(1/(ZL + SQRT(ZL**2 + 4/3.141593))/SQRT(3.141593/2))-ZL**2/2") %>% 
        add_line("IF(ZL.LT.6) LPHI_ZL=LOG(1-PHI(ZL))") %>% 
        add_line("LPHI_ZU=LOG(1/(ZU + SQRT(ZU**2 + 4/3.141593))/SQRT(3.141593/2))-ZU**2/2") %>% 
        add_line("IF(ZU.LT.6) LPHI_ZU=LOG(1-PHI(ZU))") %>% 
        add_empty_line() %>% 
        add_line("; Add log-probabilities") %>%
        add_line("IF(LPHI_ZU.LT.LPHI_ZL) A = LPHI_ZL") %>% 
        add_line("IF(LPHI_ZU.LT.LPHI_ZL) B = LPHI_ZU-LPHI_ZL") %>% 
        add_line("IF(LPHI_ZU.GT.LPHI_ZL) A = LPHI_ZU") %>% 
        add_line("IF(LPHI_ZU.GT.LPHI_ZL) B = LPHI_ZL-LPHI_ZU") %>% 
        add_line("LOGSUM = A+LOG(1-EXP(B))") %>% 
        add_line("IF(EXP(B).LT.1E-8) LOGSUM = A + EXP(B)*(1-0.5*EXP(B))") %>% 
        add_empty_line() %>% 
        add_line("; Return -2*LL") %>% 
        add_line("IF(DV.EQ.", bi_model$min, ") Y=-2*LPHI_ZU") %>% 
        add_line("IF(DV.GT.",bi_model$min,".AND.DV.LT.",bi_model$max,") Y=-2*LOGSUM") %>% 
        add_line("IF(DV.EQ.",bi_model$max,") Y=-2*LPHI_ZL")
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
    code_generator() %>% 
        banner_comment("Simulation code") %>% 
        add_line("IF (ICALL.EQ.4) THEN") %>%
        increase_indent() %>% 
        add_line("I = ", model$max) %>% 
        add_line("CALL RANDOM(2, R)") %>% 
        add_line("DOWHILE(I>", model$min,".AND.R<=PHI(Z(I)))") %>% 
        increase_indent() %>% 
        add_line("I = I - 1") %>% 
        decrease_indent() %>% 
        add_line("ENDDO") %>% 
        add_line("DV = I") %>% 
        decrease_indent() %>% 
        add_line("ENDIF")
}


bi_estimation <- function(model) {
    cg <- code_generator() %>% 
        add_line(";Sim_start for VPC") %>% 
        add_line("$ESTIMATION METHOD=IMP LAPLACE -2LL AUTO=1 PRINT=1", " ", model$estimaton_options) %>% 
        add_line(";$SIMULATION (", sample.int(2147483647, 1),") (", sample.int(2147483647, 1)," UNI) ONLYSIM NOPRED") %>% 
        add_line(";Sim_end for VPC")
    cg
}