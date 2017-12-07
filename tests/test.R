library("hlt")

stopifnot(identical(html_td("a"),
                    structure(list(
                        tag = "td", content = "a", attributes = list()),
                        .Names = c("tag", "content", "attributes"),
                        class = c("html_td", "html_object"))
                    )
          )

stopifnot(identical(html_tr(data.frame(a = 1, b = 2, c = 3)),
                    structure(list(
                        tag = "tr",
                        content = list(structure(
                            list(tag = "td", content = 1, attributes = list()),
                            .Names = c("tag", "content", "attributes"),
                            class = c("html_td", "html_object")),
                            structure(list(tag = "td", content = 2, attributes = list()),
                                      .Names = c("tag", "content", "attributes"),
                                      class = c("html_td", "html_object")),
                            structure(list(tag = "td", content = 3, attributes = list()),
                                      .Names = c("tag", "content", "attributes"),
                                      class = c("html_td", "html_object"))),
                        attributes = list()),
                        .Names = c("tag", "content", "attributes"),
                        class = c("html_tr", "html_object"))))

h <-as.data.frame(html_table(cars))
stopifnot(identical(h, cars))

h <-as.data.frame(html_table(cars, tfoot = TRUE))
stopifnot(identical(h, cars))

h <- html_table(cars[1:2, ], tfoot = TRUE)
h_expected <- "<table><thead><tr><th>speed</th><th>dist</th></tr></thead><tfoot><tr><td>4</td><td>10</td></tr></tfoot><tbody><tr><td>4</td><td>2</td></tr></tbody></table>"
h_observed <- html(h)
stopifnot(identical(h_observed, h_expected))

h <- html_table(cars[1:2, ])
h_expected <- "<table><thead><tr><th>speed</th><th>dist</th></tr></thead><tbody><tr><td>4</td><td>2</td></tr><tr><td>4</td><td>10</td></tr></tbody></table>"
h_observed <- html(h)
stopifnot(identical(h_observed, h_expected))
