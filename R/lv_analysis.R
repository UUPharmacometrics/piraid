#' Prepare latent variable analysis
#' 
#' The function accepts a dataset with a column containing latent variable estimates and uses it to prepare a direct latent variable analysis in NONMEM by
#' generating scaffold NONMEM model and dataset in the desired directory. 
#'
#' @param lv_dataset The dataset with the latent variable (stored in column PSI) as well as its associated standard error (in column SE_PSI)
#' @param output_directory The directory of where to store the NONMEM control stream and the dataset
#' @param model_filename The filename for the NONMEM control stream that will be created
#' @param data_filename The filename for the CSV data file that will be created 
#'
#' @export
prepare_lv_analysis <- function(lv_dataset, output_directory, model_filename = "lv_run1.mod", data_filename = "lv_data.csv"){
    if(!dir.exists(output_directory)) rlang::abort("The output directory does not exist.")
    utils::write.csv(lv_dataset, file = file.path(output_directory, data_filename), na = "0", quote = F, row.names = F)

    
    str <- str_lv_scaffold_model(lv_dataset, data_filename)
    fp <- file(file.path(output_directory, model_filename))
    writeLines(str, fp)
    close(fp)
}

str_lv_scaffold_model <- function(lv_dataset, data_filename){
    
    data_columns <- colnames(lv_dataset) %>% 
        purrr::map_if(~.=="PSI", ~"DV=PSI")
    
    code_generator() %>% 
        add_line("$PROBLEM") %>% 
        add_line("$INPUT", paste(data_columns, collapse=' ')) %>% 
        add_line("$DATA", data_filename, "IGNORE=@") %>% 
        add_line("$PRED") %>% 
        add_code(lv_model_code()) %>% 
        add_line("$ESTIMATION METHOD=COND MAXEVAL=9999") %>% 
        add_line("$THETA 0.1 \t;BASE") %>% 
        add_line("$THETA 0.1 \t;SLP") %>%
        add_line("$OMEGA 1 \t;IIV-SLP") %>%
        add_line("$OMEGA 0.1 \t;IIV-SLP") %>% 
        add_line("$SIGMA 1 FIX \t;RUV") %>% 
        get_code()
}

lv_model_code <- function(){
    code_generator() %>% 
        increase_indent() %>% 
        add_line("BASE=THETA(1)+ETA(1)") %>% 
        add_line("SLP=THETA(2)+ETA(2)") %>% 
        add_line("Y=BASE+SLP*TIME+SE_PSI*EPS(1)") %>% 
        decrease_indent()
}