#!/usr/bin/env Rscript
# FORGE live-demo R runner.
# Usage: Rscript demo/live/run_stage.R <job.json>   (cwd must be the FORGE repo root)
# Reads a JSON job, prints a single JSON result on stdout. All other output goes to stderr.
#
# Modes:
#   intake — build the network, compute Stage-0 diagnostics, build the valid term library
#   screen — guardrail-check + MPLE-fit candidate term sets, rank by pseudo-BIC, GOF the winner

sink(stderr())

suppressPackageStartupMessages({
  library(jsonlite)
  library(network)
  library(ergm)
})

quiet_source <- function(path) {
  invisible(capture.output(suppressMessages(source(path))))
}
quiet_source("consolidated_guardrails.R")
quiet_source("stage1_candidate_library.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("usage: run_stage.R <job.json>")
job <- fromJSON(args[[1]], simplifyVector = FALSE)

finite_or_na <- function(x) ifelse(is.finite(x), x, NA)

build_network <- function(spec) {
  directed <- isTRUE(spec$directed)
  nodes <- spec$nodes
  ids <- vapply(nodes, function(nd) as.character(nd$id), character(1))
  if (anyDuplicated(ids)) stop("duplicate node ids")
  n <- length(ids)
  if (n < 4) stop("need at least 4 nodes")
  idx <- setNames(seq_len(n), ids)
  adj <- matrix(0L, n, n)
  for (edge in spec$edges) {
    s <- unname(idx[as.character(edge[[1]])])
    t <- unname(idx[as.character(edge[[2]])])
    if (is.na(s) || is.na(t)) stop(sprintf("edge references unknown node: %s--%s", edge[[1]], edge[[2]]))
    if (s == t) next
    adj[s, t] <- 1L
    if (!directed) adj[t, s] <- 1L
  }
  net <- network::network(adj, directed = directed)
  network::set.vertex.attribute(net, "vertex.names", ids)

  attr_names <- unique(unlist(lapply(nodes, function(nd) names(nd$attrs))))
  for (name in attr_names) {
    raw <- lapply(nodes, function(nd) {
      v <- nd$attrs[[name]]
      if (is.null(v)) NA else v
    })
    numeric_ok <- all(vapply(raw, function(v) is.na(v) || is.numeric(v), logical(1)))
    values <- if (numeric_ok) as.numeric(unlist(raw)) else as.character(unlist(raw))
    network::set.vertex.attribute(net, name, values)
  }
  net
}

compute_diagnostics <- function(net) {
  directed <- network::is.directed(net)
  A <- as.matrix.network(net, matrix.type = "adjacency")
  A[is.na(A)] <- 0
  n <- nrow(A)
  edges <- network::network.edgecount(net)
  S <- if (directed) pmin(A + t(A), 1) else A
  deg <- if (directed) rowSums(A) + colSums(A) else rowSums(A)
  tri <- sum(diag(S %*% S %*% S)) / 6
  und_deg <- rowSums(S)
  triples <- sum(und_deg * (und_deg - 1) / 2)
  reciprocity <- if (directed && sum(A) > 0) sum(A * t(A)) / sum(A) else NA

  list(
    nodes = n,
    edges = edges,
    directed = directed,
    density = round(network::network.density(net), 4),
    triangles = as.integer(round(tri)),
    transitivity = round(ifelse(triples > 0, 3 * tri / triples, 0), 4),
    degree_max = max(deg),
    degree_quantiles = unname(round(quantile(deg, c(0, 0.25, 0.5, 0.75, 1)), 1)),
    isolates = sum(deg == 0),
    reciprocity = if (is.na(reciprocity)) NA else round(reciprocity, 4)
  )
}

attribute_summary <- function(net) {
  keep <- setdiff(network::list.vertex.attributes(net), c("na", "vertex.names"))
  lapply(keep, function(name) {
    values <- network::get.vertex.attribute(net, name)
    cls <- classify_attribute(values)
    list(
      attribute = name,
      classification = cls$classification,
      unique_values = length(unique(values[!is.na(values)])),
      missing = sum(is.na(values))
    )
  })
}

build_library <- function(net, job) {
  opts <- job$library_options
  build_admissible_library(
    net,
    min_expected_cell = opts$min_expected_cell %||% 5,
    min_expected_dyads_mix = opts$min_expected_dyads_mix %||% 30
  )
}

guardrail_report <- function(terms, net, library_terms) {
  res <- check_all_guardrails(terms, net, library_terms)
  list(
    g1_edges_and_size = isTRUE(res$guardrail_1),
    g2_single_closure_family = isTRUE(res$guardrail_2),
    g3_categorical_support = isTRUE(res$guardrail_3),
    g4_no_match_factor_overlap = isTRUE(res$guardrail_4),
    g5_no_triangle = isTRUE(res$guardrail_5$pass),
    g6_library_only = if (is.null(res$guardrail_6)) NA else isTRUE(res$guardrail_6$pass),
    all_passed = isTRUE(res$all_passed)
  )
}

fit_mple <- function(net, terms) {
  started <- Sys.time()
  rhs <- paste(unique(terms), collapse = " + ")
  f <- as.formula(paste("g ~", rhs))
  env <- new.env(parent = globalenv())
  assign("g", net, envir = env)
  environment(f) <- env
  fit <- suppressMessages(suppressWarnings(
    ergm(f, estimate = "MPLE", control = control.ergm(seed = 42))
  ))
  coefs <- summary(fit)$coefficients
  list(
    fit = fit,
    pseudo_bic = round(finite_or_na(BIC(fit)), 2),
    pseudo_aic = round(finite_or_na(AIC(fit)), 2),
    coefficients = lapply(rownames(coefs), function(term) list(
      term = term,
      estimate = round(finite_or_na(coefs[term, "Estimate"]), 3),
      std_error = round(finite_or_na(coefs[term, "Std. Error"]), 3)
    )),
    wald_max = round(finite_or_na(max(abs(coefs[, "Estimate"] / coefs[, "Std. Error"]), na.rm = TRUE)), 2),
    runtime = round(as.numeric(Sys.time() - started, units = "secs"), 2)
  )
}

quick_gof <- function(fit, directed) {
  tryCatch({
    gof_formula <- if (directed) ~ idegree + odegree + distance else ~ degree + espartners + distance
    g <- suppressMessages(suppressWarnings(gof(fit, GOF = gof_formula)))
    pairs <- list(
      degree = c("obs.deg", "sim.deg"),
      idegree = c("obs.ideg", "sim.ideg"),
      odegree = c("obs.odeg", "sim.odeg"),
      espartners = c("obs.espart", "sim.espart"),
      distance = c("obs.dist", "sim.dist")
    )
    zs <- c()
    worst <- list(stat = NA, z = 0)
    details <- list()
    for (stat_name in names(pairs)) {
      obs <- g[[pairs[[stat_name]][1]]]
      sims <- g[[pairs[[stat_name]][2]]]
      if (is.null(obs) || is.null(sims)) next
      mean_sim <- colMeans(sims)
      sd_sim <- apply(sims, 2, sd)
      keep <- is.finite(obs) & is.finite(mean_sim) & sd_sim > 0
      if (!any(keep)) next
      z_signed <- (obs[keep] - mean_sim[keep]) / sd_sim[keep]
      bins <- names(obs)[keep]
      for (j in seq_along(z_signed)) {
        details[[length(details) + 1]] <- list(
          stat = stat_name,
          bin = if (is.null(bins)) as.character(j) else bins[j],
          z = round(z_signed[j], 2)
        )
      }
      z <- abs(z_signed)
      zs <- c(zs, z)
      if (max(z) > worst$z) worst <- list(stat = stat_name, z = max(z))
    }
    if (!length(zs)) return(NULL)
    details <- details[order(-vapply(details, function(d) abs(d$z), numeric(1)))]
    max_z <- round(max(zs), 2)
    list(max_abs_z = max_z, pass = max_z <= 2.5, worst_stat = worst$stat,
         details = details[seq_len(min(8, length(details)))])
  }, error = function(e) NULL)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

run_intake <- function(job) {
  net <- build_network(job$network)
  lib <- build_library(net, job)
  list(
    ok = TRUE,
    diagnostics = compute_diagnostics(net),
    library = list(
      terms = lib$terms,
      base_terms = lib$base_terms,
      attribute_terms = lib$attribute_terms,
      directed = lib$directed,
      lambda = lib$lambda,
      guardrail_config = lib$guardrails
    ),
    attribute_details = attribute_summary(net)
  )
}

run_screen <- function(job) {
  net <- build_network(job$network)
  lib <- build_library(net, job)
  directed <- network::is.directed(net)
  want_gof <- job$gof %||% "winner"

  fits <- lapply(job$candidates, function(cand) {
    terms <- unlist(cand$terms)
    guard <- tryCatch(
      guardrail_report(terms, net, lib$terms),
      error = function(e) list(all_passed = FALSE, error = conditionMessage(e))
    )
    fitted <- tryCatch(fit_mple(net, terms), error = function(e) e)
    if (inherits(fitted, "error")) {
      return(list(label = cand$label, terms = as.list(terms), success = FALSE,
                  guardrails = guard, error = conditionMessage(fitted)))
    }
    entry <- list(
      label = cand$label,
      terms = as.list(terms),
      success = TRUE,
      guardrails = guard,
      pseudo_bic = fitted$pseudo_bic,
      pseudo_aic = fitted$pseudo_aic,
      coefficients = fitted$coefficients,
      wald_max = fitted$wald_max,
      runtime = fitted$runtime
    )
    attr(entry, "fit") <- fitted$fit
    entry
  })

  ok <- vapply(fits, function(f) isTRUE(f$success) && is.finite(f$pseudo_bic %||% NA), logical(1))
  if (!any(ok)) {
    fits <- lapply(fits, function(f) { attributes(f) <- list(names = names(f)); f })
    return(list(ok = FALSE,
                error = "no candidate specification could be fitted to this network (all MPLE fits failed or had non-finite pseudo-BIC); the network may be empty or otherwise degenerate",
                fits = fits))
  }

  # Winner must pass the guardrails, not just win on pseudo-BIC. g3 is exempt:
  # the demo library is built with min_expected_cell=3 (disclosed in demo/README.md)
  # while check_guardrail_3 hardcodes 5, so g3 reads as a warning on small networks.
  core_guardrails_pass <- function(f) {
    g <- f$guardrails
    isTRUE(g$g1_edges_and_size) && isTRUE(g$g2_single_closure_family) &&
      isTRUE(g$g4_no_match_factor_overlap) && isTRUE(g$g5_no_triangle) &&
      !isFALSE(g$g6_library_only)
  }
  eligible <- ok & vapply(fits, function(f) !identical(f$label, "Edge-only null") && core_guardrails_pass(f), logical(1))
  pool <- if (any(eligible)) eligible else ok
  bics <- vapply(seq_along(fits), function(i) if (pool[i]) fits[[i]]$pseudo_bic else Inf, numeric(1))
  winner_idx <- which.min(bics)
  winner <- fits[[winner_idx]]$label
  gof_targets <- switch(want_gof,
    all = which(ok),
    none = integer(0),
    winner_idx
  )
  for (i in gof_targets) {
    g <- quick_gof(attr(fits[[i]], "fit"), directed)
    if (!is.null(g)) fits[[i]]$gof <- g
  }
  fits <- lapply(fits, function(f) { attributes(f) <- list(names = names(f)); f })

  list(ok = TRUE, winner = winner, fits = fits)
}

result <- tryCatch({
  switch(job$mode,
    intake = run_intake(job),
    screen = run_screen(job),
    stop(sprintf("unknown mode: %s", job$mode))
  )
}, error = function(e) list(ok = FALSE, error = conditionMessage(e)))

sink()
cat(toJSON(result, auto_unbox = TRUE, na = "null", digits = NA), "\n")
