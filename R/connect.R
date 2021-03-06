print.moodle_con <- function(x) {
	cat("Moodle connection to", sub("^https?://", "", x$url), "\n")
}

#' Log into moodle server
#'
#' Log into moodle server
#'
#' Do this once before using any of the other functions in this package that require access to the server.
#' 
#' @param moodle_url web address of moodle server
#' @param user moodle username for logging in
#' @param pass password for logging in
#' @export
login <- function(moodle_url, user = NULL, pass = NULL) {
	if (is.null(user) || is.null(pass)) {
		## TODO prompt for username and password
		stop("arguments 'user' and 'pass' must be supplied")
	}

	## TODO: check moodle_url for sanity
	r <- httr::POST(paste0(moodle_url, "/login/index.php"), 
									body = list(username = user,
															password = pass),
									encode = "form")

	if (httr::status_code(r) != 200L) {
		stop("login attempt returned HTTP status code ", httr::status_code(r))
	} else {
		if (grepl("login/index.php$", r$url)) {
			stop("login to ", moodle_url, " failed")
		} else {
			cat("login to", moodle_url, "was successful\n")
		}
	}

 return(invisible(NULL))
}
