#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ids
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
api_import_fc <- function(ids) {
  result_df <- tibble()

  for (i in seq_along(ids$vedidk_core_researcher)[1:3]) { # tady bych mel idealne dat vsechny ids do jednoho vektoru a vyhodit z nich duplikovane

    vedidk <- ids$vedidk_core_researcher[i]
    body1 <- list(token = keyring::key_get("riv"), 
                  oblast = "riv", 
                  rezim = "filtr-detail", 
                  "tv-identifikator" = vedidk)
    result <- httr::POST(url = "https://api.isvavai.cz/api.php", 
                         body = body1, 
                         encode = c("form"))

    dataAsRawjson <- httr::content(result, as = "text")
    dataAsjson <- jsonlite::fromJSON(dataAsRawjson, flatten = TRUE)
    # as_tibble(dataAsjson[[1]][1])
    # dataAsjson[["hlavicka"]][["zprava"]] #najit sloupce ktere chci z toho vytahat a pouzit
    # [1] "IP adresa požadavku není uvedena v seznamu povolených adres" --> tohle je problem, zkontrolovat doma

    temp_df <- as_tibble(dataAsjson[[1]])
    temp_df$my_id <- vedidk
    result_df <- bind_rows(result_df, temp_df)
    # result_df[i] <- as_tibble(dataAsjson[[1]][1]) - v pripade ze bych to chtel jako list (pak musim zmenit typ toho result_df)
    Sys.sleep(0.5)
  }
}
