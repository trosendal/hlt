##' @export
html_object <- function(.tag, .content = NULL, ...)
{
    if (missing(.tag))
        stop("'.tag' is missing")
    object <- list(tag = .tag, content = .content, attributes = list())
    class(object) <- c(paste0("html_", .tag), "html_object")
    tag_attr(object) <- list(...)
    object
}

##' @export
print.html_object <- function(x, pretty = TRUE, level = 0, indent = "    ", ...)
{
    cat(html(x, pretty = pretty, level = level, indent = indent, ...))
}

##' @export
html <- function(x, pretty = FALSE, level = 0, indent = "    ", ...) UseMethod("html")

##' @export
html.default <- function(x, pretty, level, indent, ...)
{
    ## Check for attributes, for example, 'style'
    a <- ""
    for (i in seq_len(length(x$attributes))) {
        a <- paste0(a, " ", names(x$attributes)[i], "=\"", x$attributes[i], "\"")
    }

    ## Handle start tag and attributes.
    result <- ""
    if (isTRUE(pretty))
        result <- paste0(paste0(rep(indent, level), collapse = ""), result)
    result <- paste0(result, "<", x$tag, a)

    ## Handle content
    if (is.null(x$content)) {
        result <- paste0(result, " />")
    } else if (is.list(x$content)) {
        result <- paste0(result, ">")
        if (isTRUE(pretty))
            result <- paste0(result, "\n")

        if (inherits(x$content, "html_object")) {
            content <- html(x$content,
                            pretty = pretty,
                            level  = level + 1,
                            indent = indent,
                            ...)
        } else {
            content <- sapply(x$content, function(xx) {
                html(xx,
                     pretty = pretty,
                     level  = level + 1,
                     indent = indent,
                     ...)
            })
            content <- paste0(content, collapse = "")
        }

        result <- paste0(result, content)
        if (isTRUE(pretty))
            result <- paste0(result, paste0(rep(indent, level), collapse = ""))
        result <- paste0(result, "</", x$tag, ">")
    } else {
        result <- paste0(result, ">", x$content, "</", x$tag, ">")
    }

    if (isTRUE(pretty))
        result <- paste0(result, "\n")

    result
}

##' @export
html.html_comment <- function(x, pretty, level, indent, ...)
{
    result <- paste0("<!-- ", x$content, " -->")
    if (isTRUE(pretty))
        return(paste0(c(rep(indent, level), result, "\n"), collapse = ""))
    result
}

##' @export
html.html_html <- function(x, pretty, level, indent, ...)
{
    paste0("<!DOCTYPE html>\n", NextMethod())
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

##' Create a \sQuote{<head>} tag in an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_head <- function(x, ...)
{
    html_object("head", x, ...)
}

##' Create a \sQuote{<title>} tag
##'
##' @param title the title of the page.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_title <- function(title, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(title), is.numeric(title), is.character(title)))
        title <- as.character(title)
    stopifnot(length(title) == 1)
    html_object("title", title, ...)
}

##' Create a \sQuote{<meta>} tag
##'
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_meta <- function(...)
{
    html_object("meta", ...)
}

##' Create a \sQuote{<link>} tag
##'
##' @param rel relationship between document and linked document.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_link <- function(rel, ...)
{
    if (missing(rel))
        stop("Missing 'rel' argument")
    object <- html_object("link", rel = rel)
    tag_attr(object) <- list(...)
    object
}

##' Create a \sQuote{<style>} tag
##'
##' @param content style information for an HTML document.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_style <- function(content, ...)
{
    if (missing(content))
        stop("Missing 'content' argument")
    content <- paste0(as.character(content), collapse = "\n")
    html_object("style", content, ...)
}

##' Create a \sQuote{<div>} tag
##'
##' @param content of the div tag.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_div <- function(content, ...)
{
    if (missing(content))
        stop("Missing 'content' argument")
    html_object("div", content, ...)
}

##' Create a \sQuote{<script>} tag
##'
##' @param content of the script tag.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_script <- function(content, ...)
{
    if (missing(content))
        stop("Missing 'content' argument")
    content <- paste0(as.character(content), collapse = "\n")
    html_object("script", content, ...)
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

##' Create a \sQuote{img} tag in an \sQuote{HTML} page
##'
##' @param src URL of the image.
##' @param alt alternate text for the image.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_img <- function(src, alt, ...)
{
    if (missing(src))
        stop("Missing 'src' argument")
    if (missing(alt))
        stop("Missing 'alt' argument")
    object <- html_object("img", src = src, alt = alt)
    tag_attr(object) <- list(...)
    object
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

##' Create a \sQuote{<ul>} tag in an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_ul <- function(x, ...)
{
    html_object("ul", x, ...)
}

##' Create a \sQuote{<li>} tag in an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_li <- function(x, ...)
{
    html_object("li", x, ...)
}

##' Create a \sQuote{<a>} tag in an \sQuote{HTML} page
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_a <- function(x, ...)
{
    html_object("a", x, ...)
}

##' Create a \sQuote{<comment>} tag
##'
##' @param content the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_comment <- function(content, ...)
{
    if (missing(content))
        stop("Missing 'content' argument")
    html_object("comment", content, ...)
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
    content <- html_object("tr", lapply(x, html_td))
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

##' Create a \sQuote{H1} tag
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_H1 <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("H1", x, ...)
}

##' Create a \sQuote{H2} tag
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_H2 <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("H2", x, ...)
}

##' Create a \sQuote{H3} tag
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_H3 <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("H3", x, ...)
}

##' Create a \sQuote{H4} tag
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_H4 <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("H4", x, ...)
}

##' Create a \sQuote{H5} tag
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_H5 <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("H5", x, ...)
}

##' Create a \sQuote{H6} tag
##'
##' @param x the content to display.
##' @param ... tag attributes.
##' @return an \code{html_object}.
##' @export
html_H6 <- function(x, ...)
{
    ## Keep object type if logical, numeric, or character.
    if (!any(is.logical(x), is.numeric(x), is.character(x)))
        x <- as.character(x)
    stopifnot(length(x) == 1)
    html_object("H6", x, ...)
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
##' @param value list with named attributes to add e.g. \code{list(style =
##'     "background-color: lightblue;")}.
##' @return tag
##' @export
##' @examples
##' \dontrun{
##' img <- tempfile(fileext = ".png")
##' png(img)
##' plot(speed ~ dist, cars)
##' dev.off()
##'
##' h <- html_html(html_body(
##'     html_p("Display the 'cars' dataset as a table") +
##'     html_table(cars) +
##'     html_img(src = img, alt = "'cars' dataset") +
##'     html_p("Display the 'cars' dataset again, but now with column sums in a 'tfoot' tag",
##'            style = "background-color: lightblue;") +
##'     html_table(rbind(cars, colSums(cars)), tfoot = TRUE) +
##'     html_p("Display the 'mtcars' dataset as a table",
##'            style = "background-color: lightgreen;") +
##'     html_table(mtcars) +
##'     html_hr() +
##'     html_p("Nice &#9786;")))
##'
##' filename <- tempfile(fileext = ".html")
##' capture.output(h, file = filename)
##' browseURL(filename)
##' }
"tag_attr<-" <- function(tag, value)
{
    ## Check arguments
    stopifnot(inherits(tag, "html_object"),
              is.list(value))
    if (any(nchar(names(value)) == 0))
        stop("Missing attribute name(s)")
    if (any(duplicated(names(value))))
        stop("Duplicated attribute name(s)")

    ## Assign attributes to tag
    for (i in seq_len(length(value))) {
        tag$attributes[names(value)[i]] <- value[[i]]
    }

    tag
}
