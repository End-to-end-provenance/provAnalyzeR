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

# The analysis environment
.analyze.env <- new.env(parent = emptyenv())

# parsed provenance and adjacency graph
.analyze.env$prov <- NULL
.analyze.env$graph <- NULL
.analyze.env$has.graph <- FALSE

# procedure nodes, data nodes
.analyze.env$proc.nodes <- NULL
.analyze.env$data.nodes <- NULL

# data-to-procedure edges, procedure-to-data edges
.analyze.env$data.proc <- NULL
.analyze.env$proc.data <- NULL

# environment for loaded variables
# .analyze.env$var.env <- NULL

# path to provenance directory
.analyze.env$prov.dir <- NULL

# === HELPER FUNCTIONS ======================================================= #

#' Initialises the analyzer.
#'
#' @param json Path to a PROV-JSON file, or a PROV-JSON string.
#' @param is.file If TRUE, this indicates that the value given in the parameter
#'                'json' is a file.
#'
#' @return N/A
#' @noRd
.analyze.init <- function(json, is.file)
{
  # get parsed provenance, adjacency graph
  .analyze.env$prov <- provParseR::prov.parse(json, isFile = is.file)
  .analyze.env$graph <- provGraphR::create.graph(json, isFile = is.file)
  
  # procedure nodes, data nodes
  .analyze.env$proc.nodes <- provParseR::get.proc.nodes(.analyze.env$prov)
  .analyze.env$data.nodes <- provParseR::get.data.nodes(.analyze.env$prov)
  
  # data-to-procedure edges, procedure-to-data edges
  .analyze.env$data.proc <- provParseR::get.data.proc(.analyze.env$prov)
  .analyze.env$proc.data <- provParseR::get.proc.data(.analyze.env$prov)
  
  # # var.env (for loading variables for viewing)
  # .analyze.env$var.env <- new.env(parent = .analyze.env)
  
  # path to provenance directory
  environment <- provParseR::get.environment(.analyze.env$prov)
  .analyze.env$prov.dir <- environment$value[environment$label == "provDirectory"]
  
  
  # empty case
  if(is.null(.analyze.env$graph)) {
    .analyze.env$has.graph <- FALSE
    stop("There is no provenance.")
  }
  
  .analyze.env$has.graph <- TRUE
  
  # get full code for each procedure node
  .analyze.env$proc.nodes$name <- .get.full.code()
}

#' Returns the full code for each Operation procedure node. A vector of strings.
#' Uses a helper function for testing purposes.
#'
#' @return The full code for each Operation procedure node. A vector of strings.
#' @noRd
.get.full.code <- function()
{
  # get list of saved scripts
  scripts <- provParseR::get.saved.scripts(.analyze.env$prov)$script
  
  # call helper function
  return(.get.full.code.helper(.analyze.env$proc.nodes, scripts))
}

#' Helper function for .get.full.code
#' Given a table of procedure nodes and saved scripts, returns the full code 
#' for each Operation procedure node as a vector of strings.
#' Separated from .get.full.code for testing purposes.
#'
#' @param proc.nodes Table of procedure nodes.
#' @param scripts List of paths to saved scripts.
#'
#' @return The full code for each Operation procedure node. A vector of strings.
#' @noRd
.get.full.code.helper <- function(proc.nodes, scripts)
{	
  # vector to store script paths that could not be found
  inaccessible <- c()
  
  lines <- lapply(scripts, function(script) 
  {
    # check if file exists
    if(!file.exists(script)) {
      inaccessible <<- append(inaccessible, script)
      return(NA)
    }
    
    # read file
    file <- file(script, "r", encoding = getOption("encoding"))
    line.list <- readLines(file, warn = FALSE)
    close(file)
    
    return(line.list)
  })
  
  # case: throw warning if there are inaccessible files
  if(length(inaccessible) > 0) 
  {
    warn.msg <- paste(inaccessible, collapse="\n")
    warn.msg <- paste("Unable to access the following files:",
                      warn.msg, sep="\n")
    warning(warn.msg, call. = FALSE)
    
    # case: if all files are inaccessible
    # return the name column of proc nodes as is
    if(length(inaccessible) == length(scripts)) {
      return(proc.nodes$name)
    }
  }
  
  # get full code for each proc node
  codes <- sapply(1:nrow(proc.nodes), function(i)
  {
    node <- proc.nodes[i, ]
    script.num <- node$scriptNum
    
    # case: not an operation
    # case: script file was not found
    script.lines <- lines[[script.num]]
    
    if(length(script.lines) == 1 && is.na(script.lines)) {
      return(node$name)
    }
    
    if(node$type != "Operation") {
      return(node$name)
    }
    
    # get full code
    # if procedure has more than 1 line, 
    # collapse the lines into 1 before returning
    if(node$endLine - node$startLine == 0) {
      return(script.lines[node$startLine])
    }
    
    code <- script.lines[node$startLine:node$endLine]
    return(paste(code, sep="", collapse = "\n"))
  })
  
  return(unname(codes))
}

# === FOR TESTING ONLY ======================================================= #

# This function returns provAnalzyeR's environment to its initial state. 
#
#' @noRd
.clear <- function()
{
  # parsed provenance and adjacency graph
  .analyze.env$prov <- NULL
  .analyze.env$graph <- NULL
  .analyze.env$has.graph <- FALSE
  
  # procedure nodes, data nodes
  .analyze.env$proc.nodes <- NULL
  .analyze.env$data.nodes <- NULL
  
  # data-to-procedure edges, procedure-to-data edges
  .analyze.env$data.proc <- NULL
  .analyze.env$proc.data <- NULL
  
  # environment for loaded variables
  # .analyze.env$var.env <- NULL
  
  # path to provenance directory
  .analyze.env$prov.dir <- NULL
}