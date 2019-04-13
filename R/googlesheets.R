#' Read / access a google sheet from a Google team drive
#'
#' @description \code{read_team_sheet} reads (and finds) a google sheet on a
#'   team drive
#' @details Google sheet objects on team drives are not simply accessible by the
#'   googlesheets package.
#' @param path File name or other path specifier
#' @param id File id (quicker than using the name)
#' @param team_drive Anything that specifies the team drive
#' @param ws positive integer or character string specifying index or title,
#'   respectively, of the worksheet. The special value of \code{Inf} means all
#'   sheets.
#' @param ... Additional arguments passed to
#'   \code{\link[googlesheets]{gs_read_csv}}
#'
#' @return A \code{tibble} or, when \code{ws} specifies more that one sheet, a
#'   list of tibbles nammed by the sheet name. For \code{get_team_sheet} a
#'   \code{\link[googlesheets]{googlesheet}} object.
#' @export
#'
#' @examples
#' \dontrun{
#' pns<-read_team_sheet('AL_fragments_spreadsheet', team_drive='flyconnectome',ws=1:2)
#' gs <- get_team_sheet(id='1QMCbGXmJqxIm2K-IvNivN3od9QKzZCb5kA_Dqn9xjNs')
#' }
read_team_sheet <- function(path=NULL, id=NULL, team_drive = NULL, ws=1, ...) {
  gs <- get_team_sheet(path=path, id=id, team_drive = team_drive)
  allsheets <- is.numeric(ws) && !is.finite(ws)
  if(length(ws)>1 || allsheets) {
    if(is.numeric(ws)) {
      nn=googlesheets::gs_ws_ls(gs)
      ws <- if(allsheets) nn else nn[ws]
    }
    sapply(ws, function(y, ...) googlesheets::gs_read_csv(gs, ws=y, ...), ..., simplify = FALSE)
  } else googlesheets::gs_read_csv(gs, ws=ws, ...)
}

#' @rdname read_team_sheet
#' @description \code{get_team_sheet} finds a google sheet on a team drive. You
#'   can use the returned
#' @export
get_team_sheet <- function(path=NULL, id=NULL, team_drive = NULL) {
  if(is.null(id)) {
    f <- googledrive::drive_get(path = path, id=id, team_drive = team_drive)
    stopifnot(isTRUE(nrow(f)==1))
    id=f[['id']]
  }
  gs <- googlesheets::gs_key(id, lookup = FALSE, visibility = "private")
  gs
}
