#' Default Parse Function
#'
#' Return the default file parsing function, and throw error
#'   if it is not available.
#'
#' @param verbose If `TRUE`, display descriptive message.
#' @return Reference to function `readr::read_csv()`
#'
#' @keywords internal
default_parse_fun = function(verbose) {
  if (!requireNamespace("readr")) {
    stop("no parse function provided, package \"readr\" is not available.")
  } else {
    if (verbose) {
      message("using \"readr::read_csv()\" to parse files.")
    }
    return(readr::read_csv)
  }
}


#############################
###      file parsing     ###
#############################

#' Extract Files From Zip
#'
#' Extract files from a zip archive.
#'
#' @param path The path to the zipfile.
#' @param fnames A vector of file names in the archive to extract.
#'   Supports regex.
#' @param ... Other arguments passed to `utils::unzip()`.
#' @param verbose If `TRUE`, display descriptive message.
#' @return A vector of extracted file paths.
#'
#' @examples
#' \dontrun{
#' f = download_file("ftp://ftp.wildlife.ca.gov/Delta%20Smelt/SKT.zip")
#' extract_files(f, "SKT", exdir = tempdir())
#' }
#'
#' @importFrom utils unzip
#' @export
extract_files = function(path, fnames = ".*", ..., verbose = TRUE) {
  all_files = unzip(path, list = TRUE)$Name
  included_files = choose_files(all_files, fnames, verbose)
  if (verbose) {
    message("Extracting files...")
  }
  res = unzip(zipfile = path, files = included_files, ...)
  res
}

#' Download File
#'
#' Helper function to download files using curl::curl_download().
#' This function can be supplied in place of argument `parse_fun`
#' to download files without attempting to read their contents.
#'
#' @param url The url of the file to download.
#' @param fname The name of the file to save to. By default, the
#'   filename at the end of the url is used.
#' @param download_dir The directory to place the downloaded file in.
#'   By default, the session temporary directory is used.
#' @return The path of the downloaded file.
#'
#' @examples
#' \dontrun{
#' download_file(paste0("https://raw.githubusercontent.com/",
#'   "InteragencyEcologicalProgram/Status-and-Trends/master/data/",
#'   "Grandtab_adultsalmon.csv"))
#' }
#'
#' @importFrom curl curl_download
#' @export
download_file = function(url, fname = basename(url), download_dir = tempdir()) {
  out_path = file.path(download_dir, fname)
  res = curl_download(url = url, destfile = out_path)
  res
}


#' Parse Remotely-Hosted Excel File
#'
#' Helper function for parsing an Excel file hosted on a website
#' or FTP server.
#'
#' @note This function will be defunct once `readxl::read_excel()`
#'   [supports reading from more general inputs](https://github.com/tidyverse/readxl/issues/278).
#'
#' @param path The URL or FTP directory of the Excel file.
#' @param ... Other arguments to pass to [readxl::read_excel()]
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#' parse_remote_excel(paste0("https://github.com/",
#'     "InteragencyEcologicalProgram/Status-and-Trends/blob/",
#'     "9d1ba8ec3f475e96dbdd7788b45c26fb0fc55b0b/data/",
#'     "EMPMysidBPUEMatrixAug2019.xlsx?raw=true"),
#'   sheet = "MysidBPUEMatrix1972-2018", guess_max = 100000)
#' }
#'
#' @importFrom curl curl_download
#' @importFrom stringr str_detect
#' @export
parse_remote_excel = function(path, ...) {
  if (!requireNamespace("readxl")) {
    stop("package \"readxl\" is not available.")
  }
  # added "?.*" to regex to support urls with query arguments
  if (str_detect(path, "(xlsx)?.*$")) {
    type = ".xlsx"
  } else if (str_detect(path, "(xls)?.*$")) {
    type = ".xls"
  } else {
    stop(path, " does not appear to be a valid Excel file.")
  }
  tf = tempfile(fileext = type)
  curl::curl_download(path, tf)
  readxl::read_excel(path = tf, ...)
}


#############################
###    webpage parsing    ###
#############################


#' Parse HTML Index
#'
#' Parse an HTML index page.
#'
#' @param url The url of the page to parse.
#' @param node The HTML node to extract. Default is link anchor,
#'   i.e. `<a>`.
#' @param attribute if not `NULL`, the attribute of the node to
#'   extract.
#' @return A character vector of nodes or attributes.
#'
#' @examples
#' \dontrun{
#' parse_html_index(paste0("https://emp.baydeltalive.com/assets/",
#'   "00c870b6fdc0e30d0f92d719984cfb44/application/vnd.ms-excel"))
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_attrs
#' @keywords internal
parse_html_index = function(url, node = "//a", attribute = "href") {
  index.page = read_xml(url, as_html = TRUE)
  nodes = xml_find_all(index.page, "//a")
  if (!is.null(attribute)) {
    node.attr = xml_attrs(nodes, "href")
    unlist(node.attr, use.names = FALSE)
  } else {
    paste(nodes)
  }
}


#' Parse FTP Index
#'
#' Parse an FTP index page.
#'
#' @param url The url of the FTP directory to parse.
#' @return A character vector of file names.
#'
#' @examples
#' \dontrun{
#' parse_ftp_index("ftp://ftp.dfg.ca.gov/IEP_Zooplankton")
#' }
#'
#' @importFrom curl curl new_handle
#' @importFrom stringr str_detect str_c
#' @keywords internal
parse_ftp_index = function(url) {
  if (!str_detect(url, "/$")) {
    url = str_c(url, "/")
  }
  con = curl(url = url, "r",
    handle = new_handle(dirlistonly = TRUE))
  on.exit(close(con))
  readLines(con)
}


#############################
###    main interfaces    ###
#############################


#' Choose Files
#'
#' Subset a list of files based on filenames or regex strings.
#'
#' @param files A vector of file names.
#' @param selections A vector of file names to choose from `files`.
#'   Supports regex.
#' @param verbose If `TRUE`, display descriptive message.
#' @return A vector of file names.
#'
#' @examples
#' \dontrun{
#' file.list = parse_ftp_index("ftp://ftp.dfg.ca.gov/IEP_Zooplankton")
#' choose_files(file.list, c("CBMatrix", "MysidMatrix"))
#'
#' # can also be used to select sheets in an excel file
#' f = download_file(paste0("https://github.com/",
#'     "InteragencyEcologicalProgram/Status-and-Trends/blob/",
#'     "9d1ba8ec3f475e96dbdd7788b45c26fb0fc55b0b/data/",
#'     "EMPMysidBPUEMatrixAug2019.xlsx?raw=true"), "temp.xlsx")
#' sheets = readxl::excel_sheets(f)
#' choose_files(sheets, "Mysid")
#' }
#'
#' @importFrom stringr str_c str_detect
#' @importFrom glue glue
#' @export
choose_files = function(files, selections = ".*", verbose = FALSE) {
  if (verbose) {
    message("Found files:\n",
      str_c("    ", files, sep = "", collapse = "\n"))
  }
  # select entities that match fnames
  fname_regex = str_c(glue("({selections})"), collapse = "|")
  selected_files = files[str_detect(files, fname_regex)]
  if (length(selected_files) < length(selections)) {
    stop("Not all specified selections were found.")
  }
  if (verbose) {
    message("Selected files:\n",
      str_c("    ", selected_files, sep = "", collapse = "\n"))
  }
  selected_files
}


#' Download EDI Package Files
#'
#' Download the latest revision of files in an EDI package
#'
#' @param pkg_id The package identifier.
#' @param fnames A vector of file names in the package to download.
#'   Supports regex.
#' @param parse_fun A function to parse datasets. Default assumes that
#'   all files in fnames can be parsed using `readr::read_csv()`.
#' @param ... Additional arguments to pass to `parse_fun`.
#' @param verbose If `TRUE`, display descriptive messages.
#' @return A named list of dataframes.
#'
#' @examples
#' \dontrun{
#' get_edi_data(233, "YBFMP_Fish", guess_max = 1000000)
#' }
#'
#' @importFrom utils tail
#' @importFrom glue glue
#' @importFrom purrr map_chr map
#' @importFrom stringr str_c str_detect
#' @export
get_edi_data = function(pkg_id, fnames, parse_fun, ..., verbose = TRUE) {
  # check parsing function and arguments
  if (missing(parse_fun)) {
    parse_fun = default_parse_fun(verbose)
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
  included_entities = all_entities[choose_files(names(all_entities), fnames, verbose)]
  # download data
  if (verbose) {
    message("Downloading files...")
  }
  dfs = map(glue("https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}"),
    parse_fun, ...)
  names(dfs) = names(included_entities)
  dfs
}


#' Download Open Data Portal Package Files
#'
#' Download files from an Open Data Portal, assuming it uses the CKAN
#'   API.
#'
#' @param portal_url The base URL of the portal, e.g. `"data.cnra.ca.gov"`.
#' @inheritParams get_edi_data
#' @return a named list of dataframes.
#'
#' @examples
#' \dontrun{
#' get_odp_data(pkg_id = "dayflow", "Dayflow Results")
#' }
#'
#' @importFrom glue glue
#' @importFrom stringr str_detect str_c
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @export
get_odp_data = function(portal_url = "https://data.cnra.ca.gov", pkg_id, fnames, parse_fun, ..., verbose = TRUE) {
  if (missing(parse_fun)) {
    parse_fun = default_parse_fun(verbose)
  } else if (!is.function(parse_fun)) {
    stop("argument \"parse_fun\" must be a function.")
  }
  opd_url = glue("{portal_url}/api/3/action/package_show?id={pkg_id}")
  entity_list = fromJSON(opd_url)[[c("result", "resources")]][c("name", "url")]
  all_entities = as.list(entity_list$url)
  # use paste() to handle NA names
  names(all_entities) = paste(entity_list$name)
  included_entities = all_entities[choose_files(names(all_entities), fnames, verbose)]
  # download data
  if (verbose) {
    message("Downloading files...")
  }
  map(included_entities, parse_fun, ...)
}


#' Download Redbluff Data
#'
#' Download Redbluff data from [cbr.washington.edu](https://cbr.washington.edu).
#'
#' @param report_year The report year.
#' @param start_year The initial year to retrieve data for.
#'   Default is 2004.
#' @inheritParams get_edi_data
#' @return a named list of dataframes. The list also includes an
#'   attribute "Notes" of same length containing the notes section
#'   extracted from the report files.
#'
#' @examples
#' \dontrun{
#' get_redbluff_data(2018, 2016, na = "--")
#' }
#'
#' @importFrom utils head tail
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @importFrom purrr map map2
#' @export
get_redbluff_data = function(report_year, start_year = 2004, parse_fun, ..., verbose = TRUE) {
  if (missing(parse_fun)) {
    parse_fun = default_parse_fun(verbose)
  } else if (!is.function(parse_fun)) {
    stop("argument \"parse_fun\" must be a function.")
  }
  years = seq.int(start_year, report_year, by = 1)
  # download data
  if (verbose) {
    message("Downloading period of record ", start_year, " to ",
      report_year, ".")
  }
  urls = glue("http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year={years}&biweekly=other&wtemp=default")
  redbluff.raw = map(urls, parse_fun, ...)
  # extract notes
  notes.index = map(redbluff.raw,
    ~ which(str_detect(.x[["Project"]], "Notes:")))
  redbluff = map2(redbluff.raw, notes.index,
    ~ head(.x, .y - 1))
  # attach notes as attribute of list
  redbluff.notes = map2(redbluff.raw, notes.index,
    ~ str_c(tail(.x[["Project"]],  -.y), collapse = "\n"))
  attr(redbluff, "Notes") = redbluff.notes
  redbluff
}


#' Download Bay Delta Live Data
#'
#' Download data from [Bay Delta Live](https://cbr.washington.edu).
#'
#' @param asset_id The asset identifier.
#' @param path_suffix Path suffix(es), specifying subfolders of the
#'   asset to search
#' @inheritParams get_edi_data
#' @return a named list of dataframes. The list also includes an
#'   attribute "Notes" of same length containing the notes section
#'   extracted from the report files.
#'
#' @examples
#' \dontrun{
#' get_baydeltalive_data("00c870b6fdc0e30d0f92d719984cfb44",
#'   "application/vnd.ms-excel", "Field_Data_[0-9]{4}-[0-9]{4}x*",
#'   parse_remote_excel, guess_max = 100000L)
#'}
#'
#' @importFrom utils head tail
#' @importFrom stringr str_subset
#' @importFrom glue glue
#' @importFrom purrr map map2
#' @export
get_baydeltalive_data = function(asset_id, path_suffix, fnames, parse_fun, ..., verbose = TRUE) {
  if (missing(parse_fun)) {
    parse_fun = default_parse_fun(verbose)
  } else if (!is.function(parse_fun)) {
    stop("argument \"parse_fun\" must be a function.")
  }
  bdl_url = glue("https://emp.baydeltalive.com/assets/{asset_id}/{path_suffix}")
  all_files = parse_html_index(bdl_url, "//a", "href")
  # select entities that match fnames
  included_files = choose_files(all_files, fnames, verbose)
  # download data
  if (verbose) {
    message("Downloading files...")
  }
  dfs = map(glue("{bdl_url}/{included_files}"), parse_fun, ...)
  names(dfs) = included_files
  dfs
}


#' Download FTP Data
#'
#' Download data from an FTP server.
#'
#' @param ftp_address The FTP server address.
#' @param dir_path FTP directory to search.
#' @inheritParams get_edi_data
#' @return a named list of dataframes. The list also includes an
#'   attribute "Notes" of same length containing the notes section
#'   extracted from the report files.
#'
#' @examples
#' \dontrun{
#' get_ftp_data("ftp://ftp.dfg.ca.gov", "IEP_Zooplankton", "MysidMatrix",
#'   parse_remote_excel, sheet = "Mysid CPUE Matrix 1972-2018 ",
#'   guess_max = 100000L)
#' }
#'
#' @importFrom utils head tail
#' @importFrom stringr str_subset
#' @importFrom glue glue
#' @importFrom purrr map map2
#' @export
get_ftp_data = function(ftp_address, dir_path, fnames, parse_fun, ..., verbose = TRUE) {
  if (missing(parse_fun)) {
    parse_fun = default_parse_fun(verbose)
  } else if (!is.function(parse_fun)) {
    stop("argument \"parse_fun\" must be a function.")
  }
  if (!str_detect(ftp_address, "^ftp://")) {
    ftp_address = str_c("ftp://", ftp_address)
  }
  ftp_path = glue("{ftp_address}/{dir_path}")
  all_files = parse_ftp_index(ftp_path)
  included_files = choose_files(all_files, fnames, verbose)
  # download data
  if (verbose) {
    message("Downloading files...")
  }
  dfs = map(glue("{ftp_path}/{included_files}"), parse_fun, ...)
  names(dfs) = included_files
  dfs
}
