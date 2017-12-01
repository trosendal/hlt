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

h <-as.data.frame( html_table(cars))
h$speed <- as.numeric(h$speed)
h$dist <- as.numeric(h$dist)
stopifnot(identical(h, cars))
