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

# === ELAPSED TIMES =========================================================== #

#' Tracking Execution Time
#' 
#' EDIT THIS
#' Returns a data frame for each line in the execution containing the 
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
#' @rdname analyze.elapsed.time

analyze.elapsed.time <- function()
{
  # get all elapsed times
  elapsed.nodes <- .analyze.env$proc.nodes[, c("startLine", "name", "elapsedTime")]
  
  # produce a bar chart of elapsed time results
  .get.output.elapsed.time(elapsed.nodes)
}

#' Forms user output.
#' columns: value, container, dimension, type, code, scriptNum, startLine
#'
#' @param elapsed.nodes The data nodes to be displayed to the user.
#'
#' @return The data frame of type changes to be returned to the user.
#'         columns: value, container, dimension, type, code, scriptNum, startLine
#' @noRd
.get.output.elapsed.time <- function(elapsed.nodes)
{
  # create bar chart
  cat("bar plot 1\n\n")
  barplot(t(as.matrix(elapsed.nodes)))
  
  cat("bar plot 2\n\n")
  barplot(elapsed.nodes$elapsedTime,
          main = "Elapsed Time by Line",
          xlab = "Script Line",
          ylab = "Time",
          names.arg = elapsed.nodes$startLine,
          col = "darkred",
          horiz = TRUE)
  
  cat("bar plot 3\n\n")
  barplot(elapsed.nodes$elapsedTime,
          main = "Elapsed Time by Line",
          xlab = "Script Line",
          ylab = "Time",
          names.arg = elapsed.nodes$startLine,
          horiz = TRUE)
}


