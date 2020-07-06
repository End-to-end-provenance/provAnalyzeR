# Copyright (C) President and Fellows of Harvard College and
# Trustees of Mount Holyoke College, 2020.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

###############################################################################

#' get.tool determines whether to use rdt or rdtLite to get the provenance
#' 
#' If rdtLite is loaded, "rdtLite" is returned.  If rdtLite is not loaded, but rdt
#' is, "rdt" is returned.  If neither is loaded, it then checks to see if either
#' is installed, favoring "rdtLite" over "rdt".
#' 
#' Stops if neither rdt or rdtLite is available.
#' 
#' @return "rdtLite" or "rdt"
#' @noRd 
get.tool <- function () {
  # Determine which provenance collector to use
  loaded <- loadedNamespaces()
  if ("rdtLite" %in% loaded) {
    return("rdtLite")
  } 
  if ("rdt" %in% loaded) {
    return("rdt")
  } 
  
  installed <- utils::installed.packages ()
  if ("rdtLite" %in% installed) {
    return("rdtLite")
  } 
  if ("rdt" %in% installed) {
    return("rdt")
  }
  
  stop ("One of rdtLite or rdt must be installed.")
}

#' Prints the table or list of possible options to standard output.
#'
#' @param pos.args The table or list of possible options.
#'
#' @return N/A
#' @noRd
.print.pos.options <- function(pos.args)
{
  cat("Possible options:\n")
  
  if(!is.data.frame(pos.args)) {
    pos.args <- as.data.frame(pos.args)
    colnames(pos.args) <- NULL
  }
  
  print(pos.args)
}

#' Extract all possible variables from the given data nodes table.
#' Data nodes must have type = "Data" or "Snapshot" to be considered a variable.
#'
#' @param data.nodes The data nodes table.
#'
#' @return Table of variables extracted from the given data nodes table.
#' @noRd
.extract.vars <- function(data.nodes)
{
  if(nrow(data.nodes) == 0)
    return(data.nodes)
  
  data.nodes <- data.nodes[data.nodes$type == "Data" | data.nodes$type == "Snapshot", ]
  data.nodes <- .remove.na.rows(data.nodes)
  
  return(data.nodes)
}

#' From the given data frame, remove rows that are all NA, if any.
#' This is needed because sometimes, rows of NA values will be inserted
#' into the data frame.
#'
#' @param df The data frame. May have no rows.
#'
#' @return The data frame with NA rows, if any, removed. Could have no rows.
#' @noRd
.remove.na.rows <- function(df)
{
  if(nrow(df) == 0)
    return(df)
  
  valid.rows <- sapply(c(1:nrow(df)), function(i)
  {
    row <- as.list(df[i, ])
    return(!all(is.na(row)))
  })
  
  return(df[valid.rows, ])
}

#' Finds the associated proc node id from the given data node id.
#'
#' @param d.id The data node id.
#'
#' @return The associated procedure node id, or a list of id if there are multiple.
#' @noRd
.get.p.id <- function(d.id)
{
  # Search output edges first, where the data node is produced.
  # There should only ever be 1, if any
  p.id <- .analyze.env$proc.data$activity[.analyze.env$proc.data$entity == d.id]
  
  # if no output edges found, then search input edges
  # it is possible there are multiple
  if(length(p.id) == 0)
    p.id <- .analyze.env$data.proc$activity[.analyze.env$data.proc$entity == d.id]
  
  return(p.id)
}

#' Combine a list of data frames into a single data frame.
#'
#' @param list The list of data frames to be combined.
#'			   All data frames in this list must have the same columns.
#'			   Guarenteed to have at least one element.
#'
#' @return The combined data frame.
#' @noRd
.form.df <- function(list)
{	
  # get column names
  col.names <- names(list[[1]])
  
  # form data frame
  col.length <- 1:length(col.names)
  
  cols <- lapply(col.length, function(i) 
  {
    return(.flatten.args(mapply(`[[`, list, i)))
  })
  
  names(cols) <- col.names
  df <- data.frame(cols, stringsAsFactors = FALSE)
  
  rownames(df) <- 1:nrow(df)
  return(df)
}

#' Collapses the given parameters into a single list.
#'
#' @param ... The parameteres to be collapsed.
#'
#' @return The given parameters, collapsed into a single list.
#' @noRd
.flatten.args <- function(...)
{
  return(unlist(list(...)))
}

#' save.to.text.file saves the summary to a text file
#' @param prov the parsed provenance
#' @param environment a data frame containing the environment information
#' @noRd
save.to.text.file <- function(environment) {
  prov.path <- environment[environment$label == "provDirectory", ]$value
  prov.file <- paste(prov.path, "/prov-summary.txt", sep="")
  sink(prov.file, split=TRUE)
  generate.summaries(environment)
  sink()
  cat(paste("Saving provenance analysis in", prov.file))
}

#' save.to.zip.file creates a zip file of the provenance directory
#' @param environment the environemnt data frame extracted from the provenance
#' @noRd
save.to.zip.file <- function (environment) {
  # Determine where the provenance is
  cur.dir <- getwd()
  prov.path <- environment[environment$label == "provDirectory", ]$value
  setwd(prov.path)
  
  # Determine the name for the zip file
  prov.dir <- sub (".*/", "", prov.path)
  zipfile <- paste0 (prov.dir, "_",
                     environment[environment$label == "provTimestamp", ]$value, ".zip")
  zippath <- paste0 (cur.dir, "/", zipfile)
  
  if (file.exists (zippath)) {
    warning (zippath, " already exists.")
  }
  
  else {
    # Zip it up
    zip.program <- Sys.getenv("R_ZIPCMD", "zip")
    if (.Platform$OS.type == "windows" && endsWith (zip.program, "7z.exe")) {
      # 7z.exe a prov.zip . -r -x!debug
      zip.result <- utils::zip (zippath, ".", flags="a", extras="-r -x!debug")
    }
    else {
      # zip -r prov.zip . -x debug/
      zip.result <- utils::zip (zippath, ".", flags="-r", extras="-x debug/")
    }
    
    # Check for errors
    if (zip.result == 0) {
      cat (paste ("Provenance saved in", zipfile))
    }
    else if (zip.result == 127) {
      warning ("Unable to create a zip file.  Please check that you have a zip program, such as 7-zip, on your path, and have the R_ZIPCMD environment variable set.")
    }
    else if (zip.result == 124) {
      warning ("Unable to create a zip file.  The zip program timed out.")
    }
    else {
      warning ("Unable to create a zip file.  The zip program ", zip.program, " returned error ", zip.result)
    }
  }
  
  # Return to the directory where the user executed the command from.
  setwd(cur.dir)
}
