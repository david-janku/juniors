#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param db_path
#' @param vedidk
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
transcribe_database <- function(path, authors_arrow, cep_investigators_arrow, cep_details_arrow, riv_details_arrow, riv_authors_arrow, text_arrow) {

    # authors_arrow <- as.data.frame(authors_arrow)
    # cep_investigators_arrow <- as.data.frame(cep_investigators_arrow) %>% 
    #     mutate(vedidk = case_when(vedidk == "9014701" ~ "2677741", 
    #                               TRUE~vedidk))
    # cep_details_arrow <- as.data.frame(cep_details_arrow)
    # riv_details_arrow <- as.data.frame(riv_details_arrow)
    # riv_authors_arrow <- as.data.frame(riv_authors_arrow)
    # text_arrow <- as.data.frame(text_arrow)
    # con <-  DBI::dbConnect(RSQLite::SQLite(), path)
    # dbWriteTable(con, "authors_by_pubs", authors_arrow, overwrite = TRUE)
    # dbWriteTable(con, "cep_investigators", cep_investigators_arrow, overwrite = TRUE)
    # dbWriteTable(con, "cep_details", cep_details_arrow, overwrite = TRUE)
    # dbWriteTable(con, "riv_disc", riv_details_arrow, overwrite = TRUE)
    # dbWriteTable(con, "riv_authors", riv_authors_arrow, overwrite = TRUE)
    # dbWriteTable(con, "riv_text", text_arrow, overwrite = TRUE)
    # on.exit(DBI::dbDisconnect(con))
    # 
    # 
    # path
    
    # Convert Arrow objects to data frames
    authors_arrow <- as.data.frame(authors_arrow)
    cep_investigators_arrow <- as.data.frame(cep_investigators_arrow) %>% 
        mutate(vedidk = case_when(vedidk == "9014701" ~ "2677741", 
                                  TRUE ~ vedidk))
    cep_details_arrow <- as.data.frame(cep_details_arrow)
    riv_details_arrow <- as.data.frame(riv_details_arrow)
    riv_authors_arrow <- as.data.frame(riv_authors_arrow)
    text_arrow <- as.data.frame(text_arrow)
    
    # Connect to the SQLite database
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    
    # Ensure the database connection is closed on exit
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    # Wrap database operations in a try-catch block
    tryCatch({
        # Write data frames to the database
        dbWriteTable(con, "authors_by_pubs", authors_arrow, overwrite = TRUE)
        dbWriteTable(con, "cep_investigators", cep_investigators_arrow, overwrite = TRUE)
        dbWriteTable(con, "cep_details", cep_details_arrow, overwrite = TRUE)
        dbWriteTable(con, "riv_disc", riv_details_arrow, overwrite = TRUE)
        dbWriteTable(con, "riv_authors", riv_authors_arrow, overwrite = TRUE)
        dbWriteTable(con, "riv_text", text_arrow, overwrite = TRUE)
    }, error = function(e) {
        # Print error message and rollback transaction
        cat("An error occurred:", e$message, "\n")
        DBI::dbRollback(con)
    })
    
    # Return the path, which is the output of this target
    path
}
