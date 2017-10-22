## visit.R: visit a submitted Rmd file

#' Visit a submitted Rmd file in the script editor
#' 
#' @param x : tibble with submission information
#' @param id : \code{sub_id} number
#' @export 
visit <- function(x, id) {
  x %>% filter(sub_id == id) %>% pull(fullpath) %>% file.edit()
}
