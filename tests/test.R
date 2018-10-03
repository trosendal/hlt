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
h_observed <- html(h, pretty = FALSE)
stopifnot(identical(h_observed, h_expected))

h <- html_table(cars[1:2, ])
h_expected <- "<table><thead><tr><th>speed</th><th>dist</th></tr></thead><tbody><tr><td>4</td><td>2</td></tr><tr><td>4</td><td>10</td></tr></tbody></table>"
h_observed <- html(h, pretty = FALSE)
stopifnot(identical(h_observed, h_expected))

## HTML headings
h1_expected <- structure(list(tag = "H1",
                              content = "Hello world!",
                              attributes = list()),
                         class = c("html_H1", "html_object"))
h1_observed <- html_H1("Hello world!")
stopifnot(identical(h1_observed, h1_expected))

h2_expected <- structure(list(tag = "H2",
                              content = "Hello world!",
                              attributes = list()),
                         class = c("html_H2", "html_object"))
h2_observed <- html_H2("Hello world!")
stopifnot(identical(h2_observed, h2_expected))

h3_expected <- structure(list(tag = "H3",
                              content = "Hello world!",
                              attributes = list()),
                         class = c("html_H3", "html_object"))
h3_observed <- html_H3("Hello world!")
stopifnot(identical(h3_observed, h3_expected))

h4_expected <- structure(list(tag = "H4",
                              content = "Hello world!",
                              attributes = list()),
                         class = c("html_H4", "html_object"))
h4_observed <- html_H4("Hello world!")
stopifnot(identical(h4_observed, h4_expected))

h5_expected <- structure(list(tag = "H5",
                              content = "Hello world!",
                              attributes = list()),
                         class = c("html_H5", "html_object"))
h5_observed <- html_H5("Hello world!")
stopifnot(identical(h5_observed, h5_expected))

h6_expected <- structure(list(tag = "H6",
                              content = "Hello world!",
                              attributes = list()),
                         class = c("html_H6", "html_object"))
h6_observed <- html_H6("Hello world!")
stopifnot(identical(h6_observed, h6_expected))
