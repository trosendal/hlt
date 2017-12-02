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

##' Create an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @return an \code{html_object}.
##' @export
html_html <- function(x)
{
    html_object("html", x)
}

##' Create a cell in an \sQuote{HTML} table
##'
##' @param x the content to display.
##' @return an \code{html_object}.
##' @export
html_td <- function(x)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("td", x)
}

##' Create a header cell in an \sQuote{HTML} table
##'
##' @param x the content to display.
##' @return an \code{html_object}.
##' @export
html_th <- function(x)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("th", x)
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
    content <- list(html_object("tr", lapply(x, html_th)))
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
html_tfoot <- function(x)
{
    content <- list(html_object("tr", lapply(x, html_th)))
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
    header <- html_thead(colnames(x))
    if (isTRUE(tfoot)) {
        i <- seq_len(nrow(x))
        content <- html_tbody(x[i[-length(i)], ])
        foot <- html_tfoot(x[i[length(i)], ])
    } else {
        content <- html_tbody(x)
        foot <- NULL
    }

    html_object("table", header + foot + content)
}

##' @export
as.character.html_thead <- function(x)
{
    ## Conbine the content of the th cells to a character vector.
    sapply(x$content[[1]]$content, "[", "content")
}

##' @export
as.data.frame.html_tr <- function(x, row.names, optional, ...)
{
    ## Combine all td cells and coerce to a data.frame
    as.data.frame(lapply(x$content, "[", "content"), stringsAsFactors = FALSE)
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
    ## content index 1: thead
    ## content index 2: tfoot if tfoot exists, else tbody
    ## content index 3: tbody if tfoot exists, else unused
    df <- as.data.frame(x$content[[2]])
    if (length(x$content) > 2)
        df <- rbind(as.data.frame(x$content[[3]]), df)
    colnames(df) <- as.character(x$content[[1]])
    df
}

##' @export
Ops.html_object <- function(e1, e2)
{
    if (!identical(.Generic , "+"))
        stop(paste0("'", .Generic, "' not defined for 'html_object'"))
    if (is.null(e1))
        return(e2)
    if (is.null(e2))
        return(e1)
    stopifnot(is.list(e1), is.list(e2))
    if (inherits(e1, "html_object")) {
        if (inherits(e2, "html_object"))
            return(c(list(e1), list(e2)))
        stop("Not implemented")
    }

    e1[[length(e1)+1]] <- e2
    e1
}
