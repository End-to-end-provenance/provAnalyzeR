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
#' @export
#' @rdname analyze.elapsed.time

analyze.elapsed.time <- function()
{
  # get all elapsed times
  elapsed.nodes <- .analyze.env$proc.nodes[, c("startLine", "name", "elapsedTime")]
  
  # remove nodes that do not have a start line
  # particularly, this is used to remove nodes that store the name of the script
  # at the beginning and end of activity
  elapsed.nodes <- elapsed.nodes[!is.na(elapsed.nodes$startLine), ]

  # produce a bar chart of elapsed time results
  .get.output.elapsed.time(elapsed.nodes)
}

#' Forms user output.
#' columns: value, container, dimension, type, code, scriptNum, startLine
#'
#' @param elapsed.nodes The data nodes to be displayed to the user.
#' @importFrom plotly plot_ly
#' @importFrom graphics layout
#'
#' @return The data frame of type changes to be returned to the user.
#'         columns: value, container, dimension, type, code, scriptNum, startLine
#' @noRd
.get.output.elapsed.time <- function(elapsed.nodes)
{
  # create bar chart
  fig <- plotly::plot_ly(
    data = elapsed.nodes,
    x = ~elapsedTime,
    y = ~startLine,
    text = ~name,
    color = I("light blue"),
    type = "bar",
    orientation = 'h'
  )
  
  # fig <- layout(
  #   fig,
  #   title = 'Elapsed Time by Line',
    # xaxis = list(
    #   type = 'category',
    #   title = 'Elapsed Time (s)'
    # ),
    # yaxis = list(
    #   title = 'Start Line',
    #   tickmode = "linear"
    # )
  # )
  
  print(fig)

}

