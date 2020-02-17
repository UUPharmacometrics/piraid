# Abstract base class for all types of Models

base_model <- function(subclass=NULL) {
    structure(list(simulation=FALSE, run_number=1, use_data_path=TRUE, simulation_options="", estimation_options=""), class=c(subclass, "base_model"))
}