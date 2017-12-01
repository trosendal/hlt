library(hlt)

stopifnot(identical(td("a"),
                    structure(list(
                        content = "a", style = NULL),
                        .Names = c("content", "style"),
                        class = "html_td")
                    )
          )

stopifnot(identical(tr(c("a", "b", "c")),
                    structure(list(content = list(
                                       structure(list(
                                           content = "a", style = NULL),
                                           .Names = c("content", "style"),
                                           class = "html_td"),
                                       structure(list(
                                           content = "b", style = NULL),
                                           .Names = c("content", "style"),
                                           class = "html_td"),
                                       structure(list(
                                           content = "c", style = NULL),
                                           .Names = c("content", "style"),
                                           class = "html_td")), style = NULL),
                              .Names = c("content", "style"),
                              class = "html_tr")
                    )
          )
