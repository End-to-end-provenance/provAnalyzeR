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

# === FUNCTION REASSIGNMENTS =========================================================== #

#' Tracking Function Reassignments
#' 
#' Returns a data frame for each variable in the execution containing the 
#' instances where the data type changed.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item value: The value of the variable.
#'		\item code: The line of code associated with the variable.
#'		\item scriptNum: The script number associated with the variable.
#'		\item startLine: The line number associated with the variable.
#' }
#'
#' analyze.function.reassignments belongs to provAnalyzeR, an analyzer which utilises provenance 
#' collected during-execution to perform dynamic analysis and identify coding anomalies.
#'
#' This function may be used independently after the analyzer has been initialised using
#' one of its initialisation functions (listed below). Alternatively, it is run by the prov.analyze
#' functions in order to summarize analysis.
#'
#' @param var Optional. Variable name(s) to be queried. If not NA, the results will
#'            be filtered to show only those with the given variable name.
#'
#' @return A list of data frames for each variable with at least 2 function assignments.
#'
#' @seealso provAnalyzeR Initialisation Functions: 
#' @seealso \code{\link{prov.analyze}}
#' @seealso \code{\link{prov.analyze.file}} 
#' @seealso \code{\link{prov.analyze.run}}
#'
#'
#' @examples
#' \dontrun{
#' prov.analyze.run("test.R")
#' analyze.function.reassignments()
#' }
#'
#' @export
#' @rdname analyze.function.reassignments
analyze.function.reassignments <- function(var = NA)
{
  # case: no provenance
  if(!.analyze.env$has.graph)
    stop("There is no provenance.")
  
  # Get all data nodes with type "Data" or "Snapshot"
  data.nodes <- .extract.vars(.analyze.env$data.nodes)
  
  # case: no variables
  if(nrow(data.nodes) == 0) {
    cat("There are no variables.\n")
    return(invisible(NULL))
  }
  
  vars.names <- data.nodes$name
  vars.names <- unique(vars.names)
  
  # Find all variables with function reassignments
  # This functions differs from others that accept queries in that
  # checking the validity of user's query is done after all variables
  # with type changes is found.
  
  remove.indices <- c()
  
  vars <- lapply(c(1:length(vars.names)), function(i)
  {
    # get data nodes with that name that hold functions
    nodes <- data.nodes[data.nodes$name == vars.names[i], ]
    function.nodes <- nodes[nodes$valType == "function", ]
    non.function.nodes <- nodes[nodes$valType != "function", ]
    
    # if true, this variable is defined as another type
    # and will be counted in type changes code
    if (nrow(non.function.nodes) >= 1) {
      remove.indices <<- append(remove.indices, i)
      return(NULL)
    }
    
    # If true, this variable is not defined multiple times as a function
    if(nrow(function.nodes) <= 1) {
      remove.indices <<- append(remove.indices, i)
      return(NULL)
    }
    
    # prepare for printing
    return(.get.output.function.reassignments(function.nodes))
  })
  
  # remove variables without function reassignments
  if(length(remove.indices) > 0) {
    vars.names <- vars.names[-remove.indices]
    vars <- vars[-remove.indices]
  }
  
  # no function reassignments occured
  if(length(vars) == 0) {
    return(0)
  }
  
  names(vars) <- vars.names
  
  return(vars)
}

#' Forms user output.
#' columns: value, container, dimension, type, code, scriptNum, startLine
#'
#' @param data.nodes The data nodes to be displayed to the user.
#'
#' @return The data frame of type changes to be returned to the user.
#'         columns: value, code, scriptNum, startLine
#' @noRd
.get.output.function.reassignments <- function(data.nodes)
{
  # script num, line num, full code, value, valType
  # for each data node (row), get required fields for output
  rows <- lapply(c(1:nrow(data.nodes)), function(i)
  {
    # from data nodes (parameter), extract id, value
    data.id <- data.nodes$id[i]
    data.value <- data.nodes$value[i]
    
    # get valType columns (remove id column)
    # val.type <- provParseR::get.val.type(.analyze.env$prov, node.id = data.id)
    # val.type <- val.type[ , c("container", "dimension", "type")]
    
    # get proc node which either set or first used the data node
    proc.id <- .get.p.id(data.id)[1]
    
    # extract script num, line num, code from proc nodes
    proc.fields <- .analyze.env$proc.nodes[.analyze.env$proc.nodes$id == proc.id, 
                                           c("name", "scriptNum", "startLine")]
    
    # combine fields
    fields <- cbind(data.value, proc.fields, stringsAsFactors = FALSE)
    names(fields) <- c("value", "code", "scriptNum", "startLine")
    return(fields)
  })
  
  return(.form.df(rows))
}

