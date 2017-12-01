library("hlt")

stopifnot(identical(html_td("a"),
                    structure(list(
                        tag = "td", content = "a"),
                        .Names = c("tag", "content"),
                        class = c("html_td", "html_object"))
                    )
          )

stopifnot(identical(html_tr(data.frame(a = 1, b = 2, c = 3)),
                    structure(list(
                        tag = "tr",
                        content = list(structure(
                            list(tag = "td", content = "1"),
                            .Names = c("tag", "content"),
                            class = c("html_td", "html_object")),
                            structure(list(tag = "td", content = "2"),
                                      .Names = c("tag", "content"),
                                      class = c("html_td", "html_object")),
                            structure(list(tag = "td", content = "3"),
                                      .Names = c("tag", "content"),
                                      class = c("html_td", "html_object")))),
                        .Names = c("tag", "content"),
                        class = c("html_tr", "html_object"))))
