library(glue)
library(purrr)
library(readr)
library(stringr)

#' Download EDI Package Files
#'
#' Download the latest revision of files in an EDI package
#'
#' @param pkg_id The EDI Package ID
#' @param fnames A vector of file names in the package to download. Supports regex.
#' @param verbose If TRUE, display descriptive messages.
#' @return a list of dataframes
#'
#' @details Currently, this function assumes that all files in fnames can be 
#'   parsed using readr::read_csv().
#'
get_edi_file = function(pkg_id, fnames, verbose = TRUE){
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
  if(length(included_entities) != length(fnames)){
    stop("Not all specified filenames are included in package")
  }
  # download data
  if (verbose) {
    message("Downloading files:\n",
	  str_c("    ", names(included_entities), sep = "", collapse = "\n"))
  }
  dfs = map(glue("https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}"),
    read_csv, guess_max = 1000000)
  names(dfs) = names(included_entities)
  dfs
}



# Download the YBFMP Files
ybfmp = get_edi_file(233, c("YBFMP_Fish_Catch_and_Water_Quality.csv",
  "YBFMP_Fish_Taxonomy.csv", "YBFMP_Trap_Effort.csv",
  "YBFMP_Site_locations_latitude_and_longitude.csv"))

# Path to sharepoint site
sharepoint_path = normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - Seasonal monitoring report - Documents/Data"
  )
)  

# save data  
save(ybfmp, file = file.path(sharepoint_path, "ybfmp.RData"))
