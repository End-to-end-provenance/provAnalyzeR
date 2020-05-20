# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2018.

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


#' prov.analyze.run
#'
#' prov.analyze.run executes a script, collects provenance, and outputs a
#' text summary of any coding anomalies to the console.
#'
#' @param r.script the name of a file containing an R script
#' @param ... extra parameters are passed to the provenance collector.  See rdt's prov.run function
#'    or rdtLites's prov.run function for details.

#' @export 
#' @examples 
#' \dontrun{
#' ADD EXAMPLE}
#' @rdname summarize
prov.analyze.run <- function(r.script, save=FALSE, create.zip=FALSE, ...) {
  # Determine which provenance collector to use
  tool <- get.tool()
  if (tool == "rdtLite") {
    prov.run <- rdtLite::prov.run
    prov.json <- rdtLite::prov.json
  } else {
    prov.run <- rdt::prov.run
    prov.json <- rdt:: prov.json
  }
  
  # Run the script, collecting provenance, if a script was provided.
  tryCatch (prov.run(r.script, ...), error = function(x) {print (x)})
  
  # Create the provenance summary
  prov <- provParseR::prov.parse(prov.json(), isFile=FALSE)
  analyze.prov.summary (prov, save, create.zip)
}

#' analyze.prov.summary summarizes course-grained coding anomolies found in the provenance.
#' 
#' @param prov the json in a string
#' @param save if true saves the summary to the file prov-summary.txt in the 
#' provenance directory
#' @param create.zip if true all of the provenance data will be packaged up
#'   into a zip file stored in the current working directory.
#' @noRd
analyze.prov.summary <- function (prov, save, create.zip) {
  environment <- provParseR::get.environment(prov)
  
  if (save) {
    save.to.text.file(prov, environment)
  }
  else {
    generate.summaries(prov, environment)
  }
  
  if (create.zip) {
    save.to.zip.file (environment)
  }
  
}

#' generate.summaries creates the text summary, writing it to the
#' current output sink(s)
#' @param prov the parsed provenance
#' @param environment the environemnt data frame extracted from the provenance
#' @noRd
generate.summaries <- function(prov, environment) {
  script.path <- environment[environment$label == "script", ]$value
  script.file <- sub(".*/", "", script.path)
  
  if (script.file != "") {
    cat (paste ("POTENTIAL ANOMOLIES DETECTED for", script.file, "\n\n"))
  } else {
    # NOT CURRENTLY IMPLEMENTED
    cat (paste ("POTENTIAL ANOMOLIES DETECTED for Console Session\n\n"))
  }
  
  generate.preexisting.summary(provParseR::get.preexisting(prov))
  
}

#' generate.preexisting.summary lists variables in the global environment that are 
#' used but not set by a script or a console session.
#' @param vars a data frame of preexisting variables
#' @noRd
generate.preexisting.summary <- function(vars) {
  cat (paste ("PRE-EXISTING:\n"))
  if (is.null(vars) || nrow(vars) == 0) {
    cat("None\n")
  } else {
    for (i in 1:nrow(vars)) {
      cat(vars[i, "name"], "\n")
    }
  }
  cat("\n")
}

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

#' save.to.text.file saves the summary to a text file
#' @param prov the parsed provenance
#' @param environment a data frame containing the environment information
#' @noRd 
save.to.text.file <- function(prov, environment) {
  prov.path <- environment[environment$label == "provDirectory", ]$value
  prov.file <- paste(prov.path, "/prov-summary.txt", sep="")
  sink(prov.file, split=TRUE)
  generate.summaries(prov, environment)
  sink()
  cat(paste("Saving provenance summmary in", prov.file))
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
