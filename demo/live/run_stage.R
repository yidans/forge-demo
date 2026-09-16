#!/usr/bin/env Rscript
# FORGE live-demo R runner.
# Usage: Rscript demo/live/run_stage.R <job.json>   (cwd must be the FORGE repo root)
# Reads a JSON job, prints a single JSON result on stdout. All other output goes to stderr.
#
# Modes:
#   intake — build the network, compute Stage-0 diagnostics, build the valid term library
#   evaluate — guardrail-check each candidate term set, fit it with stochastic approximation (SA),
#              run the density check (30 simulations, |relative error| <= 25%), compute the
#              simulation-based GOF discrepancy q(M) = max_k |z_k| from 100 simulated networks,
#              record MPLE pseudo-BIC as a secondary diagnostic, and select the eligible
#              candidate with the lowest q(M)

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

make_formula <- function(net, terms) {
  rhs <- paste(unique(terms), collapse = " + ")
  f <- as.formula(paste("g ~", rhs))
  env <- new.env(parent = globalenv())
  assign("g", net, envir = env)
  environment(f) <- env
  f
}

coef_table <- function(fit) {
  coefs <- summary(fit)$coefficients
  lapply(rownames(coefs), function(term) list(
    term = term,
    estimate = round(finite_or_na(coefs[term, "Estimate"]), 3),
    std_error = round(finite_or_na(coefs[term, "Std. Error"]), 3)
  ))
}

fit_mple <- function(net, terms) {
  fit <- suppressMessages(suppressWarnings(
    ergm(make_formula(net, terms), estimate = "MPLE", control = control.ergm(seed = 42))
  ))
  list(fit = fit, pseudo_bic = round(finite_or_na(BIC(fit)), 2))
}

fit_with <- function(net, terms, seed, estimator = "sa") {
  f <- make_formula(net, terms)
  suppressMessages(suppressWarnings(switch(estimator,
    mcmle = ergm(f, control = control.ergm(main.method = "MCMLE", seed = seed)),
    mple = ergm(f, estimate = "MPLE", control = control.ergm(seed = seed)),
    ergm(f, control = control.ergm(main.method = "Stochastic-Approximation", seed = seed))
  )))
}

density_check <- function(fit, net, seed, nsim = 30, tol = 0.25) {
  sims <- suppressMessages(suppressWarnings(
    simulate(fit, nsim = nsim, seed = seed, control = control.simulate.ergm(MCMC.burnin = 20000))
  ))
  obs <- network::network.density(net)
  sim <- mean(vapply(sims, network::network.density, numeric(1)))
  rel <- if (obs > 0) abs(sim - obs) / obs else NA
  list(observed = round(obs, 4), simulated_mean = round(sim, 4),
       rel_error = round(finite_or_na(rel), 4), nsim = nsim, tolerance = tol,
       pass = is.finite(rel) && rel <= tol)
}

simulation_gof <- function(fit, directed, seed, nsim = 100) {
  gof_formula <- if (directed) ~ idegree + odegree + espartners + distance else ~ degree + espartners + distance
  g <- suppressMessages(suppressWarnings(
    gof(fit, GOF = gof_formula, control = control.gof.ergm(nsim = nsim, seed = seed))
  ))
  pairs <- list(
    degree = c("obs.deg", "sim.deg"),
    idegree = c("obs.ideg", "sim.ideg"),
    odegree = c("obs.odeg", "sim.odeg"),
    espartners = c("obs.espart", "sim.espart"),
    distance = c("obs.dist", "sim.dist")
  )
  zs <- c()
  details <- list()
  families <- c()
  for (stat_name in names(pairs)) {
    obs <- g[[pairs[[stat_name]][1]]]
    sims <- g[[pairs[[stat_name]][2]]]
    if (is.null(obs) || is.null(sims)) next
    families <- c(families, stat_name)
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
        observed = unname(obs[keep][j]),
        simulated_mean = round(unname(mean_sim[keep][j]), 2),
        z = round(unname(z_signed[j]), 2)
      )
    }
    zs <- c(zs, abs(z_signed))
  }
  if (!length(zs)) return(NULL)
  details <- details[order(-vapply(details, function(d) abs(d$z), numeric(1)))]
  list(
    q = round(max(zs), 2),
    rmse = round(sqrt(mean(zs^2)), 2),
    bins = length(zs),
    nsim = nsim,
    families = families,
    details = details[seq_len(min(8, length(details)))]
  )
}

core_guardrails_pass <- function(f) {
  # g3 is exempt: the demo library is built with min_expected_cell=3 while
  # check_guardrail_3 hardcodes 5, so g3 reads as a warning on small networks.
  g <- f$guardrails
  isTRUE(g$g1_edges_and_size) && isTRUE(g$g2_single_closure_family) &&
    isTRUE(g$g4_no_match_factor_overlap) && isTRUE(g$g5_no_triangle) &&
    !isFALSE(g$g6_library_only)
}

guardrail_failure <- function(g) {
  if (!isTRUE(g$g1_edges_and_size)) return("model must keep edges and 3-8 terms")
  if (!isTRUE(g$g2_single_closure_family)) return("at most one gwesp/gwdsp closure term")
  if (!isTRUE(g$g4_no_match_factor_overlap)) return("nodematch and nodefactor clash on an attribute")
  if (!isTRUE(g$g5_no_triangle)) return("unstable triangle term")
  if (isFALSE(g$g6_library_only)) return("a term is outside L*")
  "guardrail check failed"
}

evaluate_candidate <- function(net, lib, cand, seed, estimator = "sa") {
  started <- Sys.time()
  terms <- unlist(cand$terms)
  directed <- network::is.directed(net)
  entry <- list(label = cand$label, terms = as.list(terms))
  entry$guardrails <- tryCatch(
    guardrail_report(terms, net, lib$terms),
    error = function(e) list(all_passed = FALSE, error = conditionMessage(e))
  )
  finish <- function(entry, eligible, reason) {
    entry$eligible <- eligible
    entry$reason <- reason
    entry$runtime <- round(as.numeric(Sys.time() - started, units = "secs"), 1)
    entry
  }
  # Specifications outside the compatible space are rejected before any fitting.
  if (!identical(cand$label, "Edge-only baseline") && !core_guardrails_pass(entry)) {
    entry$success <- FALSE
    return(finish(entry, FALSE, paste("incompatible specification:", guardrail_failure(entry$guardrails))))
  }
  fit <- tryCatch(fit_with(net, terms, seed, estimator), error = function(e) e)
  if (inherits(fit, "error")) {
    entry$success <- FALSE
    entry$error <- conditionMessage(fit)
    return(finish(entry, FALSE, paste(toupper(estimator), "fit failed:", conditionMessage(fit))))
  }
  entry$success <- TRUE
  entry$estimator <- toupper(estimator)
  entry$coefficients <- coef_table(fit)
  entry$finite <- all(is.finite(coef(fit)))
  mp <- tryCatch(fit_mple(net, terms), error = function(e) NULL)
  entry$pseudo_bic <- if (is.null(mp)) NA else mp$pseudo_bic
  if (!isTRUE(entry$finite)) return(finish(entry, FALSE, "non-finite coefficients"))
  dens <- tryCatch(density_check(fit, net, seed), error = function(e) NULL)
  if (is.null(dens)) return(finish(entry, FALSE, "network simulation failed"))
  entry$density <- dens
  if (!isTRUE(dens$pass)) {
    return(finish(entry, FALSE, sprintf("density check failed (%.0f%% relative error)", 100 * dens$rel_error)))
  }
  gf <- tryCatch(simulation_gof(fit, directed, seed), error = function(e) NULL)
  if (is.null(gf)) return(finish(entry, FALSE, "GOF diagnostics could not be computed"))
  entry$gof <- gf
  entry$q <- gf$q
  finish(entry, TRUE, "eligible")
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

run_evaluate <- function(job) {
  net <- build_network(job$network)
  lib <- build_library(net, job)
  seed <- as.integer(job$seed %||% 42)
  estimator <- tolower(job$estimator %||% "sa")
  fits <- lapply(job$candidates, function(cand) evaluate_candidate(net, lib, cand, seed, estimator))
  # The edge-only baseline is exempt from the size guardrail; every other winner must pass.
  selectable <- vapply(fits, function(f) {
    isTRUE(f$eligible) && (identical(f$label, "Edge-only baseline") || core_guardrails_pass(f))
  }, logical(1))
  qs <- vapply(seq_along(fits), function(i) if (selectable[i]) fits[[i]]$q else Inf, numeric(1))
  winner <- if (any(is.finite(qs))) fits[[which.min(qs)]]$label else NULL
  list(ok = TRUE, seed = seed, estimator = estimator, winner = winner, n_eligible = sum(selectable), fits = fits)
}

result <- tryCatch({
  switch(job$mode,
    intake = run_intake(job),
    evaluate = run_evaluate(job),
    stop(sprintf("unknown mode: %s", job$mode))
  )
}, error = function(e) list(ok = FALSE, error = conditionMessage(e)))

sink()
cat(toJSON(result, auto_unbox = TRUE, na = "null", digits = NA), "\n")
