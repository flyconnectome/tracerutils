#character methods
toParagraph <- function(c){
  paste0("<p>", c, "</p>")
}

toLink <- function(href, text){
  paste0("<a href=", href, ">", text, "</a>")
}

toHeader <- function(c, level){
  if (level < 1) level = 1
  if (level > 6) level = 6
  paste0("<h", level, ">", c, "<h", level, ">")
}

