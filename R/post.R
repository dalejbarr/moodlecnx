#' Generate directory structure for reports
#'
#' Copy the 'submitted' directory structure to 'reports' subdirectory.
#'
#' @export
make_report_dir <- function(path, overwrite = FALSE) {
  report_path <- paste(sub("/$", "", path),
		       "reports", sep = "/")
  ## prevent accidental overwriting
  if (dir.exists(report_path)) {
    if (overwrite) {
      unlink(report_path, recursive = TRUE)
    } else {
      stop("target directory ", report_path, " exists and overwrite = FALSE")
    }
  }

  subdirs <- list.files(paste0(path, "/submitted"))
  dir.create(report_path)
  purrr::walk(subdirs, ~ dir.create(paste0(report_path, "/", .x)))
}

#' Generate report based on feedback
#'
#' Generate PDF report based on assessment feedback
#'
#' @param feedback character string with feedback in RMarkdown format
#' @return path to the report
#' @export
report <- function(feedback, sub_id, path, subdir) {
  path2 <- sprintf("%s/reports/%s/report.pdf", sub("/$", "", path), subdir)
  t <- tempfile(fileext = ".Rmd")
  t2 <- tempfile(fileext = ".pdf")
  writeLines(feedback, t)
  rmarkdown::render(t, rmarkdown::pdf_document(), t2)
  file.copy(t2, path2, overwrite = TRUE)
  return(invisible(path2))
}

#' Post grades and feedback
#'
post <- function() {
stop("function not implemented yet")
}
