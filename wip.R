trackdown::update_file(file = "article.Rmd", gfile = "david_ecr_funding", hide_code = TRUE, rich_text = TRUE,
rich_text_par = list(rgb_color = list(red = 170/255,
                                                  green = 170/255,
                                                  blue = 170/255)))

trackdown::download_file(file = "article.Rmd", gfile = "david_ecr_funding",  rm_gcomments = TRUE)
