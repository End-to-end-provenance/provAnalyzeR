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

# === INVALID NAMES =========================================================== #

#' Finding Invalid Names
#' 
#' Returns a data frame for each variable in the execution containing the 
#' instances where the variable name is invalid (c, t, T, F).
#' Each data frame contains the following columns:
#' \itemize{
#'		\item value: The value of the variable.
#'		\item container: The type of the container of the variable.
#'		\item dimension: The size of the container.
#'		\item type: The data type(s) contained within the container.
#'		\item code: The line of code associated with the variable.
#'		\item scriptNum: The script number associated with the variable.
#'		\item startLine: The line number associated with the variable.
#' }
#'
#' analyze.invalid.names belongs to provAnalyzeR, an analyzer which utilises provenance 
#' collected during-execution to perform dynamic analysis and identify coding anomalies.
#'
#' This function may be used independently after the analyzer has been initialised using
#' one of its initialisation functions (listed below). Alternatively, it is run by the prov.analyze
#' functions in order to summarize analysis.
#'
#' @param var Optional. Variable name(s) to be queried. If not NA, the results will
#'            be filtered to show only those with the given variable name.
#'
#' @return A list of data frames for each variable with an invalid name.
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
#' analyze.invalid.names()
#' }
#'
#' @export
#' @rdname analyze.invalid.names
analyze.invalid.names <- function(var = NA)
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
  
  # list of invalid names
  invalid.names <- c("c", "t", "T", "F")
  
  remove.indices <- c()
  
  vars <- lapply(c(1:length(invalid.names)), function(i)
  {
    
    if (invalid.names[i] %in% vars.names) {
      # get all data nodes with that name
      invalid.nodes <- data.nodes[data.nodes$name == invalid.names[i], ]
      
      return(.get.output.invalid.names(invalid.nodes))
    }
    else {
      remove.indices <<- append(remove.indices, i)
      return(NULL)
    }
  })
  
  # remove invalid names that are not present in file
  if(length(remove.indices) > 0) {
    invalid.names.present <- invalid.names[-remove.indices]
    vars <- vars[-remove.indices]
  }
  else {
    invalid.names.present <- invalid.names
  }
  
  # no naming issues occured, return
  if(length(vars) == 0) {
    return(0)
  }
  
  names(vars) <- invalid.names.present
  
  # CURRENTLY UNIMPLEMENTED
  # # if the user has specified variable(s) to be queried, get the valid ones
  # # for this function, this process is much simpler than get.valid.var
  # # first, remove repeated user queries
  # var <- unique(var)
  # 
  # if(!(is.na(var[1]) && length(var) == 1)) 
  # {
  #   valid.queries <- var[var %in% vars.names]
  #   
  #   # no valid variables
  #   if(length(valid.queries) == 0) {
  #     cat("No valid variables.\n\n")
  #     .print.pos.options(vars.names)
  #     return(invisible(NULL))
  #   }
  #   
  #   # extract queried results from list of all possible type changes
  #   vars <- lapply(valid.queries, function(query) {
  #     return(vars[[grep(query, vars.names)]])
  #   })
  #   
  #   names(vars) <- valid.queries
  # }
  # 
  return(vars)
}

#' Forms user output.
#' columns: value, container, dimension, type, code, scriptNum, startLine
#'
#' @param data.nodes The data nodes to be displayed to the user.
#'
#' @return The data frame of type changes to be returned to the user.
#'         columns: value, container, dimension, type, code, scriptNum, startLine
#' @noRd
.get.output.invalid.names <- function(data.nodes)
{
  # script num, line num, full code, value, valType
  # for each data node (row), get required fields for output
  rows <- lapply(c(1:nrow(data.nodes)), function(i)
  {
    # from data nodes (parameter), extract id, value
    data.id <- data.nodes$id[i]
    data.value <- data.nodes$value[i]
    
    # get valType columns (remove id column)
    val.type <- provParseR::get.val.type(.analyze.env$prov, node.id = data.id)
    val.type <- val.type[ , c("container", "dimension", "type")]
    
    # get proc node which either set or first used the data node
    proc.id <- .get.p.id(data.id)[1]
    
    # extract script num, line num, code from proc nodes
    proc.fields <- .analyze.env$proc.nodes[.analyze.env$proc.nodes$id == proc.id, 
                                           c("name", "scriptNum", "startLine")]
    
    # combine fields
    fields <- cbind(data.value, val.type, proc.fields, stringsAsFactors = FALSE)
    names(fields) <- c("value", 
                       "container", "dimension", "type", 
                       "code", "scriptNum", "startLine")
    return(fields)
  })
  
  return(.form.df(rows))
}

