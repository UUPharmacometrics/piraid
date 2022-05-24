#' Calculate the WRESLIKE residual
#'@param ptab Dataframe with the probability table from modelfit output
#'@return The input table with WRESLIKE added
#'@export
calculate_wreslike <- function(ptab) {
    upper <- function(row, colnames) {
        names(row) <- colnames
        sum(row[paste0("P", 0:row['DV'])])
    }
    
    lower <- function(row, colnames) {
        names(row) <- colnames
        i <- row['DV'] - 1
        if (i < 0) {
            0
        } else {
            sum(row[paste0("P", 0:(row['DV'] - 1))])
        }
    }

    # range of the uniform random variable
    ptab$lower <- apply(ptab, 1, lower, colnames(ptab)) 
    ptab$upper <- apply(ptab, 1, upper, colnames(ptab))
    
    ptab <- ptab %>% dplyr::rowwise() %>% dplyr::mutate(WRESLIKE=stats::qnorm(stats::runif(1, min=lower, max=upper))) %>% dplyr::ungroup()

    subset(ptab, select=-c(lower, upper))
}
