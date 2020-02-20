#' Download EDI Package Files
#'
#' Download the latest revision of files in an EDI package
#'
#' @param pkg_id The EDI Package ID
#' @param fnames A vector of file names in the package to download.
#'   Supports regex.
#' @param parse_fun Function to parse datasets. Default assumes that
#'   all files in fnames can be parsed using `readr::read_csv()`.
#' @param ... Additional arguments to pass to `parse_fun`.
#' @param verbose If TRUE, display descriptive messages.
#' @return a list of dataframes
#'
#' @importFrom utils tail
#' @importFrom glue glue
#' @importFrom purrr map_chr map
#' @importFrom stringr str_c str_detect
#' @export
get_edi_data = function(pkg_id, fnames, parse_fun, ..., verbose = TRUE) {
  # check parsing function and arguments
  if (missing(parse_fun)) {
    if (!requireNamespace("readr")) {
      stop("no parse function provided, package \"readr\" is not available.")
    } else {
      message("using \"readr::read_csv()\" to parse files.")
      parse_fun = readr::read_csv
    }
  } else if (!is.function(parse_fun)) {
    stop("argument \"parse_fun\" must be a function.")
  }
  # get revision
  revision_url = glue("https://pasta.lternet.edu/package/eml/edi/{pkg_id}")
  all_revisions = readLines(revision_url, warn = FALSE)
  latest_revision = tail(all_revisions, 1)
  if (verbose) {
    message("Latest revision: ", latest_revision)
  }
  # get entities 
  pkg_url = glue("https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}")
  all_entities = readLines(pkg_url, warn = FALSE)
  name_urls = glue("https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}")
  names(all_entities) = map_chr(name_urls, readLines, warn = FALSE)
  if (verbose) {
    message("Package contains files:\n",
    str_c("    ", names(all_entities), sep = "", collapse = "\n"))
  }
  # select entities that match fnames
  fname_regex = str_c(glue("({fnames})"), collapse = "|")
  included_entities = all_entities[str_detect(names(all_entities), fname_regex)]
  if (length(included_entities) != length(fnames)) {
    stop("Not all specified filenames are included in package")
  }
  # download data
  if (verbose) {
    message("Downloading files:\n",
    str_c("    ", names(included_entities), sep = "", collapse = "\n"))
  }
  dfs = map(glue("https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}"),
    parse_fun, ...)
  names(dfs) = names(included_entities)
  dfs
}
