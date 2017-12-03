html_object <- function(tag, content = NULL, ...)
{
    object <- list(tag = tag, content = content)
    class(object) <- c(paste0("html_", tag), "html_object")
    tag_attr(object, ...)
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
        if (inherits(x$content, "html_object")) {
            content <- html(x$content)
        } else {
            content <- paste0(sapply(x$content, html), collapse = "")
        }
    } else {
        content <- x$content
    }

    ## Check for attributes, for example, 'style'
    a <- setdiff(names(attributes(x)), c("names", "class"))
    if (length(a)) {
        a <- sapply(a, function(i) {paste0(i, "=\"", attr(x, i), "\"")})
        a <- paste0(a, collapse = " ")
        a <- paste0(" ", a)
    } else {
        a <- ""
    }

    if (is.null(content))
        return(paste0("<", x$tag, a, " />"))
    paste0("<", x$tag, a, ">", content, "</", x$tag, ">")
}

##' Create a \sQuote{<html>} tag in an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_html <- function(x, ...)
{
    html_object("html", x, ...)
}

##' Create a \sQuote{<body>} tag in an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_body <- function(x, ...)
{
    html_object("body", x, ...)
}

##' Create a \sQuote{<hr />} tag in an \sQuote{HTML} page
##'
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_hr <- function(...)
{
    html_object("hr", ...)
}

##' Create a \sQuote{<p>} tag in an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_p <- function(x, ...)
{
    html_object("p", x, ...)
}

##' Create a cell in an \sQuote{HTML} table
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_td <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("td", x, ...)
}

##' Create a header cell in an \sQuote{HTML} table
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_th <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("th", x, ...)
}

##' Create a row in an \sQuote{HTML} table
##'
##' @param x one row \code{data.frame} with the content of the row.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_tr <- function(x, ...)
{
    stopifnot(is.data.frame(x), nrow(x) == 1)
    content <- lapply(seq_len(ncol(x)), function(j) {
        html_td(x[, j])
    })
    html_object("tr", content, ...)
}

##' Create a \sQuote{<thead>} tag in an \sQuote{HTML} table
##'
##' @param x one row \code{data.frame} with the content of the row.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_thead <- function(x, ...)
{
    content <- html_object("tr", lapply(x, html_th))
    html_object("thead", content, ...)
}

##' Create a \sQuote{<tbody>} tag in an \sQuote{HTML} table
##'
##' @param x \code{data.frame} with the content of the table body.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_tbody <- function(x, ...)
{
    stopifnot(is.data.frame(x))
    content <- lapply(seq_len(nrow(x)), function(i) {
        html_tr(x[i, ])
    })
    html_object("tbody", content, ...)
}

##' Create a \sQuote{<tfoot>} tag in an \sQuote{HTML} table
##'
##' @param x one row \code{data.frame} with the content of the foot.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_tfoot <- function(x, ...)
{
    content <- html_object("tr", lapply(x, html_th))
    html_object("tfoot", content, ...)
}

##' Create an \sQuote{HTML} table
##'
##' @param x \code{data.frame} with the content of the table.
##' @param tfoot use a tfoot tag.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_table <- function(x, tfoot = FALSE, ...)
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

    html_object("table", header + foot + content, ...)
}

##' @export
as.character.html_thead <- function(x, ...)
{
    ## Conbine the content of the th cells to a character vector.
    as.character(sapply(x$content$content, "[", "content"))
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
    as.data.frame(x$content)
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

##' Add attributes to a tag
##'
##' @param tag add attributes to tag.
##' @param ... named attributes to add e.g. \code{style =
##'     "background-color: lightblue;"}.
##' @return tag
##' @export
##' @examples
##' \dontrun{
##' h <- html_html(html_body(
##'     html_p("Display the 'cars' dataset as a table") +
##'     html_table(cars) +
##'     tag_attr(html_p("Display the 'cars' dataset again, but now with column sums in a 'tfoot' tag"),
##'              style = "background-color: lightblue;") +
##'     html_table(rbind(cars, colSums(cars)), tfoot = TRUE) +
##'     html_p("Display the 'mtcars' dataset as a table") +
##'     html_table(mtcars) +
##'     html_p("Nice &#9786;")))
##'
##' filename <- tempfile(fileext = ".html")
##' capture.output(h, file = filename)
##' browseURL(filename)
##' }
tag_attr <- function(tag, ...)
{
    ## Check arguments
    stopifnot(inherits(tag, "html_object"))
    a <- list(...)
    if (any(nchar(names(a)) == 0))
        stop("Missing attribute name(s)")
    if (any(duplicated(names(a))))
        stop("Duplicated attribute name(s)")

    ## Assign attributes to tag
    for (i in seq_len(length(a))) {
        attr(tag, names(a)[i]) <- a[[i]]
    }

    tag
}
