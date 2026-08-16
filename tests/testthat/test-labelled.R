# Phase k: labelled-data (haven/labelled) interop. Fixtures are built with base structure() -- NO haven
# dependency -- because the converter keys only off the `labels` / `label` attributes.

mklab <- function(codes, labels, label = NULL) {
  x <- structure(codes, labels = labels)
  if (!is.null(label)) attr(x, "label") <- label
  x
}

test_that("val_labels_to_factor: complete labels -> factor in labels order", {
  x <- mklab(c(2, 1, 1, 2, 1), c(No = 1, Yes = 2), "Agreement")
  f <- val_labels_to_factor(x)
  expect_s3_class(f, "factor")
  expect_identical(levels(f), c("No", "Yes"))
  expect_identical(as.character(f), c("Yes", "No", "No", "Yes", "No"))
  expect_null(attr(f, "labels"))
})

test_that("val_labels_to_factor: incomplete labels -> underlying numeric, labels dropped", {
  y <- mklab(c(10, 20, 98), c(refused = 98), "Income")
  z <- val_labels_to_factor(y)
  expect_true(is.numeric(z))
  expect_identical(unname(z), c(10, 20, 98))
  expect_null(attr(z, "labels"))
})

test_that("val_labels_to_factor: no `labels` attr -> unchanged (byte-identity)", {
  p <- factor(c("a", "b"))
  expect_identical(val_labels_to_factor(p), p)
  expect_identical(val_labels_to_factor(1:3), 1:3)
  expect_identical(val_labels_to_factor(c("x", "y")), c("x", "y"))
})

test_that("val_labels_to_factor: an unobserved labelled level is dropped, duplicate labels merge", {
  x <- mklab(c(1, 1, 2), c(No = 1, Yes = 2, Maybe = 3))   # 3 never observed
  f <- val_labels_to_factor(x)
  expect_identical(levels(f), c("No", "Yes"))
  d <- mklab(c(1, 2, 3), c(Lo = 1, Hi = 2, Hi = 3))       # 2 and 3 share the "Hi" label
  fd <- val_labels_to_factor(d)
  expect_identical(levels(fd), c("Lo", "Hi"))
  expect_identical(as.character(fd), c("Lo", "Hi", "Hi"))
})

test_that("tab(): a labelled row/col var uses value labels as levels", {
  set.seed(1)
  n <- 200
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe"),
    avis = mklab(sample(c(1, 2, 3), n, TRUE),
                 c("1-Pour" = 1, "2-Contre" = 2, "3-NSP" = 3), "Avis")
  )
  t1 <- tab(df, sexe, avis, pct = "row")
  expect_true(all(c("Homme", "Femme") %in% levels(t1[[1]])))
  expect_true(all(c("1-Pour", "2-Contre", "3-NSP") %in% names(t1)))
})

test_that("cleannames strips a value-label prefix turned into a factor level", {
  set.seed(2)
  n <- 120
  df <- tibble::tibble(
    avis = mklab(sample(c(1, 2, 3), n, TRUE),
                 c("1-Pour" = 1, "2-Contre" = 2, "3-NSP" = 3), "Avis"),
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe")
  )
  t2 <- tab(df, avis, sexe, pct = "row", cleannames = TRUE)
  expect_true("Pour" %in% levels(t2[[1]]))
  expect_false(any(grepl("^[0-9]-", levels(t2[[1]]))))
})

test_that("variable labels are stored in meta$vars$var_labels (absent when none)", {
  set.seed(3)
  n <- 80
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe de l'enquete"),
    plain = factor(sample(c("x", "y"), n, TRUE))
  )
  t <- tab(df, sexe, plain, pct = "row")
  va <- get_vars_attr(t)
  expect_identical(va$var_labels[["sexe"]], "Sexe de l'enquete")

  g <- tab(forcats::gss_cat, race, marital, pct = "row")   # no labels anywhere
  expect_null(get_vars_attr(g)$var_labels)
})

test_that("tabxplor.var_labels swaps names for labels in exports only, structure unchanged", {
  set.seed(4)
  n <- 120
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe de l'enquete"),
    avis = mklab(sample(c(1, 2), n, TRUE), c(Oui = 1, Non = 2), "Avis exprime")
  )
  t <- tab(df, sexe, avis, pct = "row")

  md_off <- withr::with_options(list(tabxplor.var_labels = FALSE), tab_md(t, css = FALSE))
  md_on  <- withr::with_options(list(tabxplor.var_labels = TRUE),  tab_md(t, css = FALSE))
  expect_true(any(grepl("sexe", md_off)))
  expect_false(any(grepl("Sexe de l'enquete", md_off)))
  expect_true(any(grepl("Sexe de l'enquete", md_on)))
  expect_true(any(grepl("Avis exprime", md_on)))

  # structure keeps canonical names -> select() by the real name still works with the option on
  withr::with_options(list(tabxplor.var_labels = TRUE), {
    expect_identical(names(dplyr::select(t, sexe)), "sexe")
  })
})

test_that("merged (>=2 row_vars) name column swaps to labels under the option", {
  set.seed(5)
  n <- 160
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe de l'enquete"),
    zone = mklab(sample(c(1, 2), n, TRUE), c(Ville = 1, Campagne = 2), "Zone d'habitat"),
    avis = mklab(sample(c(1, 2), n, TRUE), c(Oui = 1, Non = 2), "Avis")
  )
  t <- tab(df, c(sexe, zone), avis, pct = "row")
  md_on <- withr::with_options(list(tabxplor.var_labels = TRUE), tab_md(t, css = FALSE))
  expect_true(any(grepl("Sexe de l'enquete", md_on)))
  expect_true(any(grepl("Zone d'habitat", md_on)))
})

test_that("tab_num() with a labelled grouping var uses value labels", {
  set.seed(6)
  n <- 150
  df <- tibble::tibble(
    sexe = mklab(sample(c(1, 2), n, TRUE), c(Homme = 1, Femme = 2), "Sexe"),
    age  = rnorm(n, 45, 12)
  )
  t3 <- tab_num(df, sexe, age)
  expect_true(all(c("Homme", "Femme") %in% levels(t3[[1]])))
})

test_that("tab_counts() with a labelled key uses value labels", {
  cnts <- tibble::tibble(
    g = mklab(c(1, 1, 2, 2), c(A = 1, B = 2), "Groupe"),
    h = factor(c("p", "q", "p", "q")),
    n = c(10, 20, 30, 40)
  )
  tc <- tab_counts(cnts, g, h, counts = n)
  expect_true(all(c("A", "B") %in% levels(tc[[1]])))
})

test_that("tab_reg(): a labelled predictor shows value-label levels; labels stored", {
  set.seed(7)
  n <- 250
  df <- tibble::tibble(
    bin  = mklab(sample(c(0, 1), n, TRUE), c(Non = 0, Oui = 1), "Reponse binaire"),
    avis = mklab(sample(c(1, 2, 3), n, TRUE), c(Pour = 1, Contre = 2, NSP = 3), "Avis")
  )
  tr <- tab_reg(df, outcome = "bin", predictors = "avis", family = "binomial")
  expect_true(any(c("Contre", "NSP") %in% as.character(tr$levels)))
  expect_identical(get_vars_attr(tr)$var_labels[["avis"]], "Avis")
})
