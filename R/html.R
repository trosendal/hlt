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

##' Create a \sQuote{<tbody>} tag in an \sQuote{HTML} table
##'
##' @param x \code{data.frame} with the content of the table body.
##' @return an \code{html_object}.
##' @export
html_tbody <- function(x)
{
    stopifnot(is.data.frame(x))
    content <- lapply(seq_len(nrow(x)), function(i) {
        html_tr(x[i, ])
    })
    html_object("tbody", content)
}

##' Create a \sQuote{<tfoot>} tag in an \sQuote{HTML} table
##'
##' @param x one row \code{data.frame} with the content of the foot.
##' @return an \code{html_object}.
##' @export
html_foot <- function(x)
{
    content <- list(html_object("tr", lapply(as.character(x), html_th)))
    html_object("tfoot", content)
}

##' Create an \sQuote{HTML} table
##'
##' @param x \code{data.frame} with the content of the table.
##' @param tfoot use a tfoot tag.
##' @return an \code{html_object}.
##' @export
html_table <- function(x, tfoot = FALSE)
{
    stopifnot(is.data.frame(x))
    header <- list(html_thead(colnames(x)))
    if (isTRUE(tfoot)) {
        i <- seq_len(nrow(x))
        content <- list(html_tbody(x[i[-length(i)], ]))
        foot <- list(html_foot(x[i[length(i)], ]))
    } else {
        content <- list(html_tbody(x))
        foot <- NULL
    }

    html_object("table", c(header, foot, content))
}

colnames_html_thead <- function(x)
{
    ## Extract the th content and use for colnames.
    sapply(x$content[[1]]$content, function(th) {
        th$content
    })
}

##' @export
as.data.frame.html_tr <- function(x, row.names, optional, ...)
{
    ## Combine all td cells.
    m <- matrix(sapply(x$content, function(td) {td$content}), nrow = 1)

    ## Coerce to a data.frame.
    as.data.frame(m, stringsAsFactors = FALSE)
}

##' @export
as.data.frame.html_tbody <- function(x, row.names, optional, ...)
{
    do.call("rbind", lapply(x$content, as.data.frame))
}

##' @export
as.data.frame.html_tfoot <- function(x, row.names, optional, ...)
{
    do.call("rbind", lapply(x$content, as.data.frame))
}

##' @export
as.data.frame.html_table <- function(x, row.names, optional, ...)
{
    df <- as.data.frame(x$content[[2]])
    if (length(x$content) > 2)
        df <- rbind(as.data.frame(x$content[[3]]), df)
    colnames(df) <- colnames_html_thead(x$content[[1]])
    df
}
