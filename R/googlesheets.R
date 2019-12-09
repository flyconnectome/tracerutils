#' Read / access a google sheet from a Google team drive
#'
#' @description \code{read_team_sheet} reads (and finds) a google sheet on a
#'   team drive
#' @details Spreadsheet objects on team drives are not simply accessible by the
#'   googlesheets4 package.
#' @param path File name or other path specifier
#' @param id File id (quicker than using the name)
#' @param team_drive Anything that specifies the team drive
#' @param ws positive integer or character string specifying index or title,
#'   respectively, of the worksheet. The special value of \code{Inf} means all
#'   sheets.
#' @param ... Additional arguments passed to
#'   \code{\link[googlesheets4]{read_sheet}}
#'
#' @return A \code{tibble} or, when \code{ws} specifies more that one sheet, a
#'   list of tibbles nammed by the sheet name.
#' @export
#'
#' @examples
#' \dontrun{
#' fib_pns<-read_team_sheet(path = 'AL_fragments_PN_search', team_drive='flyconnectome',ws=1:2)
#' ss <- get_team_sheet(id='1P-ITJs9JdexIahOaU5teKaQ7fi0t5G0QOtTrAfRGwGM')
#' }
read_team_sheet <- function(path=NULL, id=NULL, team_drive = NULL, ws=1, ...) {
  ss <- get_team_sheet(path=path, id=id, team_drive = team_drive)
  allsheets <- is.numeric(ws) && !is.finite(ws)
  if(length(ws)>1 || allsheets) {
    if(is.numeric(ws)) {
      nn=googlesheets4::sheets_sheets(ss)
      ws <- if(allsheets) nn else nn[ws]
    }
    sapply(ws, function(y, ...) googlesheets4::read_sheet(ss, sheet=y, ...), ..., simplify = FALSE)
  } else googlesheets4::read_sheet(ss, sheet=ws, ...)
}

#' @rdname read_team_sheet
#' @description \code{get_team_sheet} finds a google sheet on a team drive.
#' @return  object of class `sheets_Spreadsheet` returned by \link[googlesheets4]{sheets_get}.
#' @export
get_team_sheet <- function(path=NULL, id=NULL, team_drive = NULL) {
  if(is.null(id)) {
    f <- googledrive::drive_get(path = path, id=id, team_drive = team_drive)
    if(nrow(f)==0) {
      if(is.null(id))
        stop("I'm sorry, but I cannot find a sheet matching that path: ",
             path,"\nYou may have more luck using the sheet id!")
      else
        stop("I'm sorry, but I cannot find a sheet matching that id!")
    } else if (nrow(f)>1) {
      stop(
        "I'm sorry, but there are multiple matching sheets:\n",
        format(f),
        "\nYou may have more luck using the sheet id!"
      )
    }
    id=f[['id']]
  }
  ss <- googlesheets4::sheets_get(id)
  ss
}
