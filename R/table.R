##' @export
td <- function(x, style = NULL){
    stopifnot(is.character(x),
              length(x) == 1)
    structure(list(content = x,
                   style = style),
              class = "html_td")
}
##' @export
html <- function(x, ...) UseMethod("html")
##' @export
html.html_td <- function(x) {
    if(!is.null(x$style)) x$style <- paste0(" ", x$style)
    paste0("<td", x$style, "> ", x$content, " </td>")
}
##' @export
print.html_td <- function(x, indent = "")
{
    cat(indent, html(x), sep = "")
}
##' @export
tr <- function(x, ...) UseMethod("tr")
##' @export
tr.character <- function(x, style = NULL) {
    structure(list(content = lapply(x, td),
                   style = style),
              class = "html_tr")
}
##' @export
tr.list <- function(x, style = NULL) {
    lapply(x, function(y){
        stopifnot(class(y) == "html_td")
    })
    structure(list(content = x,
                   style = style),
              class = "html_tr")
}
##' @export
html.html_tr <- function(x, ...)
{
    paste0("<tr>",
           sapply(x$content, function(y) html(y)),
           "</tr>")
}
##' @export
print.html_tr <- function(x, indent = "")
{
    cat(indent, html(x), sep = "")
}
##' @export
html_table <- function(x, ...) UseMethod("html_table")
##' @export
html_table.list <- function(x, style = NULL) {
    structure(list(content = lapply(x, tr),
                   style = style),
              class = "html_table")
}

##' @export
html_table.data.frame <- function(x, names = NULL) {
    lapply(x, function(y){
        stopifnot(class(y) %in% c("character", "numeric"))
    })
    if(is.null(names)) {
        x <- rbind(names(x), x)
    } else {
        x <- rbind(names, x)
    }
    x <- as.list(as.data.frame(t(as.matrix(x)), stringsAsFactors = FALSE))
    html_table(x)
}
##' @export
html_table.matrix <- function(x, names = NULL) {
    stopifnot(length(names) == ncol(x))
    stopifnot(class(names) %in% c("character", "numeric"))
    stopifnot(typeof(x) %in% c("character", "numeric"))
    x <- rbind(names, x)
    x <- as.list(as.data.frame(t(x), stringsAsFactors = FALSE))
    html_table(x)
}

## ##' @export
## html.html_table <- function(x){
##     if(!is.null(x$style)) x$style <- paste0(" ", x$style)
##     table <- paste(lapply(x$content, function(y) {
##                html(y)
##            }), collapse = "\n")
##     paste0("<table",
##            x$style,
##            ">\n",
##            table,
##            "\n</table>")
## }

## ##' @export
## print.html_table <- function(x, indent = ""){
##     cat(indent, html(x), sep = "")
## }

## html_body <- function(x, ...) UseMethod("html_body")

## html_body.list <- function(x) {
##     lapply(x, function(y){
##         stopifnot(class(y) %in% c("html_table"))
##     })
##     class(x) <- "html_body"
##     return(x)
## }

## html_body.html_table<- function(x) {
##     stopifnot(class(x) %in% c("html_table"))
##     x <- list(x)
##     class(x) <- "html_body"
##     return(x)
## }

## print.html_body <- function(x) {
##     body <- paste(lapply(x, function(y){
##         print(y)
##     }), collapse = "\n")
##     paste0("<body>\n",
##            body, "\n</body>")
## }

## '[.html_table' <- function(x, i, j) {
##     x$content[[1]]
## }

## '[.tr' <- function(x, j) {
##     tr(x$content[j], x$style)
## }

## '[[.tr' <- function(x, j) {
##     x$content[j]
## }

## class(tr(x$content[1:2]))

## a <- list(c(1,2,3,4,5), c("a", "b", "c", "d", "e"))

## cat(print(html_table(a)))

## x <- (tr(c(1,2,3)))
## show(x)
## b <- html_table(a)

## cat(print(b))
## str(b)

## show.tr <- function(){"foo"}


## b2 <- html_body(list(b, b, b, b))

## cat(print(b2))

## writeLines(print(html_body(html_table(cars))), "foo.html")
