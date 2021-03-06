% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/googlesheets.R
\name{read_team_sheet}
\alias{read_team_sheet}
\alias{get_team_sheet}
\title{Read / access a google sheet from a Google team drive}
\usage{
read_team_sheet(path = NULL, id = NULL, team_drive = NULL, ws = 1,
  ...)

get_team_sheet(path = NULL, id = NULL, team_drive = NULL)
}
\arguments{
\item{path}{File name or other path specifier}

\item{id}{File id (quicker than using the name)}

\item{team_drive}{Anything that specifies the team drive}

\item{ws}{positive integer or character string specifying index or title,
respectively, of the worksheet. The special value of \code{Inf} means all
sheets.}

\item{...}{Additional arguments passed to
\code{\link[googlesheets4]{read_sheet}}}
}
\value{
A \code{tibble} or, when \code{ws} specifies more that one sheet, a
  list of tibbles nammed by the sheet name.

object of class `sheets_Spreadsheet` returned by \link[googlesheets4]{sheets_get}.
}
\description{
\code{read_team_sheet} reads (and finds) a google sheet on a
  team drive

\code{get_team_sheet} finds a google sheet on a team drive.
}
\details{
Spreadsheet objects on team drives are not simply accessible by the
  googlesheets4 package.
}
\examples{
\dontrun{
fib_pns<-read_team_sheet(path = 'AL_fragments_PN_search', team_drive='flyconnectome',ws=1:2)
ss <- get_team_sheet(id='1P-ITJs9JdexIahOaU5teKaQ7fi0t5G0QOtTrAfRGwGM')
}
}
