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
    targ_dir <- paste0("assign_", assign_id)
  }

  ## prevent accidental overwriting
  if (dir.exists(targ_dir)) {
    if (overwrite) {
      unlink(targ_dir, recursive = TRUE)
    } else {
      stop("target directory ", targ_dir, " exists and overwrite = FALSE")
    }
  }

  targ_dir2 <- paste0(sub("/$", "", targ_dir), "/", "submitted")
  cat("Writing to", targ_dir2, "\n")

  targ_url <- sprintf("%s/mod/assign/view.php?id=%s&action=downloadall",
		      moodle_url, as.character(assign_id))
  tfile <- paste0(tempfile(), ".zip")
  r <- httr::GET(targ_url)

  ## TODO: check whether it worked
  bin <- httr::content(r, "raw")
  writeBin(bin, tfile)
  unzip(tfile, exdir = targ_dir2)

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
  tibble::tibble(subdir = list.files(paste0(path, "/submitted")),
                 sub_id = sub(".+_([0-9]+)_.+", "\\1", subdir) %>% as.integer(),
                 fullpath = purrr::map(subdir,
				       ~ list.files(paste0(path, "/submitted/", .x),
                                                    full.names = TRUE))) %>%
    tidyr::unnest() %>%
      dplyr::mutate(base = sub("([^\\.]+)\\.*.+", "\\1",
			       sub(".+/([^/]+)", "\\1", fullpath)),
                    ext = dplyr::case_when(!grepl("\\.", fullpath) ~ "",
                                           TRUE                ~ sub(".+\\.([^\\.]*)$", "\\1", fullpath))) %>%
      dplyr::select(sub_id, base, ext, fullpath, subdir)
}

#' Is file an Rmd?
#'
#' Perform some basic tests to see whether the file is an RMarkdown file.
#'
#' @param file name of file you want to check
#' @param check_yaml check whether it has a yaml header?
#' @param check_blocks check whether it has at least one code block?
#' @return logical value
#' @export
is_rmd <- function(file) {
  res <- vector(length = 2L)
  names(res) <- c("yaml", "blocks")
  fdata <- readLines(file, warn = FALSE)
  res["yaml"] <- sum(grepl("^---", fdata)) == 2L
  res["blocks"] <- any(grepl("^`{3}\\{r", fdata))
  return(res)
}

#' Tangle code from Rmd file
#'
#' Extract code blocks from RMarkdown file
#'
#' @param file filename
#' @return A list containing file blocks
#' @export
tangle <- function(file) {
  ofname <- tempfile(fileext = ".R")
  knitr:::knit_code$restore()
  knitr::opts_knit$set(documentation = 1L)
  knitr::knit(file, ofname, tangle = TRUE, quiet = TRUE)
  knitr::read_chunk(ofname)
  res <- knitr:::knit_code$get()
  knitr:::knit_code$restore()
  res
}

#' Assess Rmd assignment
#'
#' Examine the submission and provide basic feedback
#'
#' @param block a list whose elements are code blocks (see \code{\link{tangle}})
#' @param ext the file extension
#' @param is_rmd whether the file is actually an Rmd
#' @return A list including feedback and other info
#' @export
assess <- function(block, ext, is_rmd) {
  ## the filename where we will store our feedback
  fname <- tempfile(fileext = ".Rmd")
  forbid <- FALSE
  fmt <- TRUE
  extn <- TRUE

  ## YAML header for Rmd file
  cat("---\ntitle: \"Lab 1 Feedback\"\n---\n\n", file = fname, append = FALSE)
  cat("## RMarkdown Format\n", file = fname, append = TRUE)
  if (!is_rmd) {
    fmt <- FALSE
    cat("The file you submitted was *not* recognized as a valid RMarkdown (.Rmd) file.
           It was either missing a header like this:\n", file = fname, append = TRUE)
    cat("````\n---\ntitle: \"your title goes here\"\noutput: html_document\n---\n````\n",
        file = fname, append = TRUE)
    cat("\nOR it didn't contain a single R code block demarcated with backticks, like this:\n",
        file = fname, append = TRUE)    
    cat("````\n`r ''````{r my_block}\nsay(\"FEED ME\")\n```\n````\n\n", file = fname, append = TRUE)
    cat("**Had this been assessed, your submission would have received a failing grade**.",
        "None of the code you supplied would execute. ",
        "Please review RMarkdown format.\n\n", file = fname, append = TRUE)

    if (toupper(ext) != "RMD") {
      extn <- FALSE
      cat("Additionally, you saved your file with an extension of .", ext,
          "; the correct file extension for RMarkdown is .Rmd.\n", sep = "", file = fname, append = TRUE)
    }
  } else {
    cat("\nThe file you submitted was a well-formed RMarkdown file.  Well done!\n\n", file = fname, append = TRUE)
    if (toupper(ext) != "RMD") {
      cat("However, you saved your file with an extension of .", ext,
          "; the correct file extension for RMarkdown is .Rmd.  You would have lost points for this.\n",
          sep = "", file = fname, append = TRUE)
      extn <- FALSE
    }

    ## were any "forbidden" functions called?
    all_fns <- unlist(block, use.names = FALSE)
    forbidden <- c("install.packages", "update.packages", "tidyverse_update",
                   "setwd", "help", "vignette", "download.file", "system", "help.start",
                   "file.remove", "curl", "View", "browseURL")
    fb_regx <- fn_regex(forbidden)
    is_forbid <- map_lgl(fb_regx, ~ any(grepl(.x, all_fns)))
    if (any(is_forbid)) {
      forbid <- TRUE
      cat("## Forbidden function calls\n", file = fname, append = TRUE)
      cat("Certain functions should *never* be called from an RMarkdown script.",
          "These functions can be unsafe, require user input, or are just unnecessary.",
          "If we find these functions we will not attempt to execute your code.",
	  "Including these functions could result in a failing grade.\n\n",
          "We found the following 'forbidden' function(s) in your RMarkdown script: ",
          paste(paste0("`", forbidden[is_forbid], "()`", collapse = ", ")),
          "\n\n", file = fname, append = TRUE)
    }
  }
  data_frame(feedback = paste(readLines(fname), collapse = "\n"),
             fmt = fmt,
             extn = extn,
             forbid = forbid)
}

#' Generate regular expression to search for R function call
#'
#' Generate a regular expression to use in a search function, to find a function call in R code.
#'
#' @param fname name of the function
#' @return a string containing a regular expression
#' @export
fn_regex <- function(fname) {
  paste0("(^|[^[:alnum:_]])*", fname, "[[:space:]]*\\(")
}
