html_object <- function(tag, content)
{
    structure(list(tag = tag, content = content),
              class = c(paste0("html_", tag), "html_object"))
}

##' @export
print.html_object <- function(x, ...)
{
    cat(html(x))
}

##' @export
html <- function(x, ...) UseMethod("html")

##' @export
html.default <- function(x, ...)
{
    if (is.list(x$content)) {
        content <- paste0(sapply(x$content, html), collapse = "")
    } else {
        content <- x$content
    }

    paste0("<", x$tag, ">", content, "</", x$tag, ">")
}

##' Create a cell in an \sQuote{HTML} table
##'
##' @param x the content to display.
##' @return an \code{html_object}.
##' @export
html_td <- function(x)
{
    content <- as.character(x)
    stopifnot(length(content) == 1)
    html_object("td", content)
}

##' Create a header cell in an \sQuote{HTML} table
##'
##' @param x the content to display.
##' @return an \code{html_object}.
##' @export
html_th <- function(x)
{
    content <- as.character(x)
    stopifnot(length(content) == 1)
    html_object("th", content)
}

##' Create a row in an \sQuote{HTML} table
##'
##' @param x one row \code{data.frame} with the content of the row.
##' @return an \code{html_object}.
##' @export
html_tr <- function(x)
{
    stopifnot(is.data.frame(x), nrow(x) == 1)
    content <- lapply(seq_len(ncol(x)), function(j) {
        html_td(x[, j])
    })
    html_object("tr", content)
}

##' Create a \sQuote{<thead>} tag in an \sQuote{HTML} table
##'
##' @param x one row \code{data.frame} with the content of the row.
##' @return an \code{html_object}.
##' @export
html_thead <- function(x)
{
    content <- list(html_object("tr", lapply(as.character(x), html_th)))
    html_object("thead", content)
}

##' Create an \sQuote{HTML} table
##'
##' @param x \code{data.frame} with the content of the table.
##' @return an \code{html_object}.
##' @export
html_table <- function(x)
{
    stopifnot(is.data.frame(x))
    header <- html_thead(colnames(x))
    content <- lapply(seq_len(nrow(x)), function(i) {
        html_tr(x[i, ])
    })
    html_object("table", c(list(header), content))
}

##' @export
as.data.frame.html_table <- function(x, row.names, optional, ...)
{
    ## Combine all td cells to a data.frame. Skip first list item
    ## since it's the thead.
    df <- as.data.frame(do.call("rbind", lapply(x$content[-1], function(tr) {
        matrix(sapply(tr$content, function(td) {td$content}), nrow = 1)
    })))

    ## Extract the th content and use for colnames.
    thead <- x$content[[1]]
    colnames(df) <- sapply(thead$content[[1]]$content, function(th) {
        th$content
    })

    df
}
