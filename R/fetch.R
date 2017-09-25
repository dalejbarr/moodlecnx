#' Download assignments from moodle
#'
#' \code{fetch} downloads student submissions for a given assignment from a moodle server.
#' 
#' @param moodle_url web address of moodle server (must already be logged in)
#' @param assign_id assignment id number; digits only
#' @param targ_dir subdirectory in which to store downloaded submissions; if \code{NULL} will be downloaded to a subdirectory of the working directory named \code{assign_id}
#' @param overwrite overwrite the subdirectory if it exists (default \code{FALSE})
#' @return a tibble containing information about contents of the downloaded directory (see \code{\link{describe}})
#' @seealso \code{\link{login}}, \code{\link{describe}}
#' @export
fetch <- function(moodle_url, 
									assign_id, 
									targ_dir = NULL,
									overwrite = FALSE) {
	## make sure assign_id is correctly specified
	if (grepl("[^[:digit:]]", as.character(assign_id))) {
		stop("argument 'assign_id' must contain only digits")
	}

	if (is.null(targ_dir)) {
		targ_dir <- paste("assign", assign_id, sep = "_")
		cat("Writing to", targ_dir, "\n")
	}

	## prevent accidental overwriting
	if (dir.exists(targ_dir)) {
		if (overwrite) {
			unlink(targ_dir)
		} else {
			stop("target directory ", targ_dir, " exists and overwrite = FALSE")
		}
	}

	targ_url <- sprintf("%s/mod/assign/view.php?id=%s&action=downloadall",
											moodle_url, as.character(assign_id))
	tfile <- paste0(tempfile(), ".zip")
	r <- httr::GET(targ_url)

	## TODO: check whether it worked
	bin <- httr::content(r, "raw")
	writeBin(bin, tfile)
	unzip(tfile, exdir = targ_dir)

	## TODO: process the submissions and return a table with info
	describe(targ_dir)
}

#' Describe assignments in a directory
#'
#' @importFrom magrittr %>%
#' @param dir Path to directory containing moodle assignments
#' @return tibble with information about submissions
#' @export
describe <- function(path) {
	tibble::tibble(subdir = list.files(path),
								 sub_id = sub(".+_([0-9]+)_.+", "\\1", subdir) %>% as.integer(),
								 file = purrr::map(subdir,
																	 ~ list.files(paste(path, .x, sep = "/")))) %>%
		tidyr::unnest() %>%
		dplyr::mutate(ext = dplyr::case_when(!grepl("\\.", file) ~ "",
																	TRUE                ~ sub(".+\\.([^\\.]*)$", "\\1", file))) %>%
		dplyr::select(sub_id, file, ext, subdir)
}
