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

# === TYPE CHANGES =========================================================== #

#' Tracking Type Changes
#' 
#' Returns a data frame for each variable in the execution containing the 
#' instances where the data type changed.
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
#' analyze.type.changes belongs to provAnalyzeR, an analyzer which utilises provenance 
#' collected during-execution to perform dynamic analysis and identify coding anomalies.
#'
#' This function may be used independently after the analyzer has been initialised using
#' one of its initialisation functions (listed below). Alternatively, it is run by the prov.analyze
#' functions in order to summarize analysis.
#'
#' @param var Optional. Variable name(s) to be queried. If not NA, the results will
#'            be filtered to show only those with the given variable name.
#'
#' @return A list of data frames for each variable with at least 1 data type change.
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
#' analyze.type.changes()
#' }
#'
#' @export
#' @rdname analyze.type.changes
analyze.type.changes <- function(var = NA)
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
  
  # Find all variables with type changes
  # This functions differs from others that accept queries in that
  # checking the validity of user's query is done after all variables
  # with type changes is found.
  
  remove.indices <- c()
  
  vars <- lapply(c(1:length(vars.names)), function(i)
  {
    # get data nodes with that name
    nodes <- data.nodes[data.nodes$name == vars.names[i], ]
    
    if (nrow(nodes) == 1) {
      remove.indices <<- append (remove.indices, i)
      return (NULL)
    }
    
    # # remove any rows included by coercion functions
    # nodes <- .remove.coercion.functions(nodes)
    # 
    # # check again if there are no type changes
    # if (nrow(nodes) == 1) {
    #   remove.indices <<- append (remove.indices, i)
    #   return (NULL)
    # }
    
    # number of nodes > 1 (can compare valTypes)
    # keep indices of nodes with type change
    type.changes <- c()
    
    val.type.changes <- lapply(c(2:nrow(nodes)), function(i) {
      # check if the type changed in any way
      if (nodes$valType[i] != nodes$valType[i-1]) {
        type.changes <<- append(type.changes, c(i-1, i))
        
        val.type.changes <- .get.val.type.changes(nodes, i)
        
        return(val.type.changes)
      }
    })
    val.type.changes <- unlist(val.type.changes)
    changes <- "NA"
    changes <- append(changes, val.type.changes)
   
    
    type.changes <- unique(type.changes)
    
    if(length(type.changes) == 0) {
      remove.indices <<- append(remove.indices, i)
      return(NULL)
    }
    
    # extract specified nodes with type changes
    nodes <- nodes[type.changes, ]
    
    # prepare for printing
    return(.get.output.type.changes(nodes, changes))
  })
  
  # remove vars without type changes
  if(length(remove.indices) > 0) {
    vars.names <- vars.names[-remove.indices]
    vars <- vars[-remove.indices]
  }
  
  # break if no type changes occured
  if(length(vars) == 0) {
    return(0)
  }
  
  names(vars) <- vars.names
  
  #print(vars)
  
  # CURRENTLY UNIMPLEMENTED
  # if the user has specified variable(s) to be queried, get the valid ones
  # for this function, this process is much simpler than get.valid.var
  # first, remove repeated user queries
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
  
  print(vars)
  
  return(vars)
}

#' Forms user output.
#' columns: value, container, dimension, type, code, scriptNum, startLine
#'
#' @param data.nodes The data nodes to be displayed to the user.
#' @param val.type.changes Specifies what part of the type changed.
#'
#' @return The data frame of type changes to be returned to the user.
#'         columns: value, container, dimension, type, code, scriptNum, startLine
#' @noRd
.get.output.type.changes <- function(data.nodes, changes)
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
    fields <- cbind(data.value, proc.fields, val.type, changes[i], stringsAsFactors = FALSE)
    names(fields) <- c("value", 
                       "code", "scriptNum", "startLine",
                       "container", "dimension", "type", "changes")
    return(fields)
  })
  
  return(.form.df(rows))
}


.get.val.type.changes <- function(nodes, i) {
  # initialize values to false (no changes occurred)
  val.type.changes <- c()

  # split the valType into its components
  val.type.current <- provParseR::get.val.type(.analyze.env$prov, node.id = nodes$id[i])
  val.type.prev <- provParseR::get.val.type(.analyze.env$prov, node.id = nodes$id[i - 1]) 
  # more efficient way to do this?

  # check if the change was from container
  if (.get.val.type.changes.helper(val.type.current, val.type.prev, "container"))
    val.type.changes <- paste(val.type.changes, "c", sep = "")

  # check if change was from dimension
  if (.get.val.type.changes.helper(val.type.current, val.type.prev, "dimension"))
    val.type.changes <- paste(val.type.changes, "d", sep = "")
  
  # check if change was from type
  if (.get.val.type.changes.helper(val.type.current, val.type.prev, "type"))
    val.type.changes <- paste(val.type.changes, "t", sep = "")

  return(val.type.changes)
}

.get.val.type.changes.helper <- function(val.type.current, val.type.prev, component) {
  changed <- FALSE
  
  if (is.na(val.type.current[[component]]) || is.na(val.type.prev[[component]])) {
    # if they are not both NA, then a component change occurred
    if (!(is.na(val.type.current[[component]]) && is.na(val.type.prev[[component]]))) {
      changed <- TRUE
    }
  }
  else if (val.type.current[[component]] != val.type.prev[[component]]) {
    # a change occurred and neither is NA
    changed <- TRUE
  }
  
  return(changed)
}


# .remove.coercion.functions <- function(nodes) 
# {
#   # functions not included, as type changed are expected
#   coercion.functions <- c("as.string", "as.integer", "as.Date", "as.list", "as.POSIXct")
#   
#   remove.indices <- c()
#   
#   # for each node, check if the line it changed on has a coercion function in it
#   lapply(c(1:nrow(nodes)), function(i)
#   {
#     # from data nodes (parameter), extract id, value
#     data.id <- nodes$id[i]
#     
#     # print(nodes$id[i])
#     
#     # get proc node which either set or first used the data node
#     proc.id <- .get.p.id(data.id)[1]
#     # cat("THE PROC ID IS:\n")
#     # print(proc.id)
#     
#     # extract code from the line
#     code <- .analyze.env$proc.nodes[.analyze.env$proc.nodes$id == proc.id, "name"]
#     
#     # check if any of the functions are on this line
#     if (any(sapply(coercion.functions, grepl, code))) {
#       # remove this node at the end
#       remove.indices <<- append (remove.indices, i)
#     }
#   })
#   
#   # cat("we are outside\n")
#   # print(length(remove.indices))
#   # cat("all nodes\n")
#   # print(nodes)
#   # cat("remove nodes\n")
#   # print(nodes[remove.indices, ])
#   
#   # remove vars whose type changes occurred by coercion functions
#   if (length(remove.indices) > 0) {
#     nodes <- nodes[-remove.indices, ]
#   }
# 
#   return (nodes)
# }
