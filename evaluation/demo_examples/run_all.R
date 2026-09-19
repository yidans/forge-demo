# Recorded example runs for the static walkthrough.
# For each of the five packaged networks and each estimator (SA, MCMLE, MPLE):
# fit the three LLM-style candidate formulas plus an edge-only baseline, apply the
# eligibility checks (finite estimates, simulation, 30-simulation density check,
# computable GOF), compute q(M) = max_k |z_k| from 100 simulated networks, select
# the eligible model with the lowest q(M), then run up to four revision rounds
# (one add / remove / replace each, kept only if eligible and q(M) falls).
# The candidate formulas and the edit sequence are fixed here (no LLM call), so
# the run is reproducible; the demo's app.js embeds the resulting run_records.json.
#
# Usage (from anywhere):  Rscript evaluation/demo_examples/run_all.R
#   FORGE_ONLY=office Rscript evaluation/demo_examples/run_all.R   # one network
# Needs the R packages network, ergm, jsonlite.  Runtime: about 25 minutes for all 15 runs.

suppressMessages({library(network); library(ergm); library(jsonlite)})
HERE <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
setwd(file.path(HERE, "..", ".."))   # repository root: the R sources below use root-relative paths
invisible(capture.output(suppressMessages({source("consolidated_guardrails.R"); source("stage1_candidate_library.R")})))
nets <- fromJSON(file.path(HERE, "demo_networks.json"), simplifyVector = FALSE)
libs <- fromJSON(file.path(HERE, "libraries.json"), simplifyVector = TRUE)
SEED <- 42
`%||%` <- function(a, b) if (is.null(a)) b else a

build_net <- function(spec) {
  ids <- sapply(spec$nodes, `[[`, "id"); n <- length(ids)
  g <- network.initialize(n, directed = isTRUE(spec$directed)); network.vertex.names(g) <- ids
  for (e in spec$edges) g[match(e[[1]], ids), match(e[[2]], ids)] <- 1
  a <- spec$attrs
  set.vertex.attribute(g, a$group, sapply(spec$nodes, `[[`, "group"))
  set.vertex.attribute(g, a$cohort, sapply(spec$nodes, `[[`, "cohort"))
  set.vertex.attribute(g, a$score, as.numeric(sapply(spec$nodes, `[[`, "score")))
  g
}
make_formula <- function(g, terms) { f <- as.formula(paste("g ~", paste(terms, collapse = " + "))); environment(f) <- environment(); f }
fit_with <- function(g, terms, estimator) {
  f <- make_formula(g, terms)
  suppressMessages(suppressWarnings(switch(estimator,
    sa    = ergm(f, control = control.ergm(main.method = "Stochastic-Approximation", seed = SEED)),
    mcmle = ergm(f, control = control.ergm(main.method = "MCMLE", seed = SEED)),
    mple  = ergm(f, estimate = "MPLE", control = control.ergm(seed = SEED)))))
}
pbic_of <- function(g, terms) tryCatch(round(BIC(suppressMessages(suppressWarnings(ergm(make_formula(g, terms), estimate = "MPLE", control = control.ergm(seed = SEED))))), 2), error = function(e) NA)

evaluate <- function(g, terms, label, estimator) {
  started <- Sys.time(); directed <- is.directed(g)
  out <- list(label = label, terms = as.list(terms))
  finish <- function(out, eligible, reason) { out$eligible <- eligible; out$reason <- reason; out$runtime <- round(as.numeric(Sys.time() - started, units = "secs"), 1); out }
  fit <- tryCatch(fit_with(g, terms, estimator), error = function(e) e)
  if (inherits(fit, "error")) return(finish(out, FALSE, paste("fit failed:", conditionMessage(fit))))
  co <- coef(fit)
  out$coefficients <- lapply(names(co), function(t) list(term = t, estimate = round(unname(co[t]), 3)))
  out$pbic <- pbic_of(g, terms)
  if (!all(is.finite(co))) return(finish(out, FALSE, "non-finite coefficients"))
  sims <- tryCatch(simulate(fit, nsim = 30, seed = SEED, control = control.simulate.ergm(MCMC.burnin = 20000)), error = function(e) NULL)
  if (is.null(sims)) return(finish(out, FALSE, "network simulation failed"))
  d_obs <- network.density(g); d_sim <- mean(sapply(sims, network.density))
  out$density_obs <- round(d_obs, 3); out$density_sim <- round(d_sim, 3); out$density_rel_error <- round(abs(d_sim - d_obs) / d_obs, 3)
  if (out$density_rel_error > 0.25) return(finish(out, FALSE, sprintf("density check failed (%.0f%% relative error)", 100 * out$density_rel_error)))
  gf <- tryCatch(suppressMessages(suppressWarnings(gof(fit, GOF = if (directed) ~ idegree + odegree + espartners + distance else ~ degree + espartners + distance,
                                                        control = control.gof.ergm(nsim = 100, seed = SEED)))), error = function(e) NULL)
  if (is.null(gf)) return(finish(out, FALSE, "GOF could not be computed"))
  pairs <- list(degree = c("obs.deg","sim.deg"), idegree = c("obs.ideg","sim.ideg"), odegree = c("obs.odeg","sim.odeg"), esp = c("obs.espart","sim.espart"), distance = c("obs.dist","sim.dist"))
  details <- list(); zs <- c()
  for (s in names(pairs)) {
    obs <- gf[[pairs[[s]][1]]]; sm <- gf[[pairs[[s]][2]]]
    if (is.null(obs) || is.null(sm)) next
    m <- colMeans(sm); sd <- apply(sm, 2, sd); keep <- is.finite(obs) & is.finite(m) & sd > 0
    if (!any(keep)) next
    z <- (obs[keep] - m[keep]) / sd[keep]; bins <- names(obs)[keep]
    for (j in seq_along(z)) details[[length(details)+1]] <- list(s, bins[j], unname(obs[keep][j]), round(unname(m[keep][j]),2), round(unname(z[j]),2))
    zs <- c(zs, abs(z))
  }
  if (!length(zs)) return(finish(out, FALSE, "no finite GOF residual"))
  details <- details[order(-sapply(details, function(d) abs(d[[5]])))]
  out$q <- round(max(zs), 2); out$gof_rmse <- round(sqrt(mean(zs^2)), 2); out$gof_bins <- length(zs)
  out$residuals <- details[seq_len(min(3, length(details)))]
  finish(out, TRUE, "eligible")
}

GW <- function(t, d = "0.5") sprintf("%s(decay=%s, fixed=TRUE)", t, d)
NM <- function(a) sprintf('nodematch("%s")', a); NF <- function(a) sprintf('nodefactor("%s")', a)
AD <- function(a) sprintf('absdiff("%s")', a); NC <- function(a) sprintf('nodecov("%s")', a)
plans <- list(
  school = list(cands = list(c("edges", GW("gwesp"), NM("club"), GW("gwdegree")), c("edges", GW("gwesp"), NM("grade"), AD("activity")), c("edges", GW("gwesp"), GW("gwdsp"), NM("club"), GW("gwdegree"))),
                edits = list(list(list("add", NM("grade")), list("add", NF("grade"))), list(list("add", AD("activity")), list("add", NC("activity"))),
                             list(list("replace", GW("gwdegree"), GW("gwdegree","0.25")), list("replace", GW("gwesp"), GW("gwesp","0.25"))), list(list("remove", GW("gwesp")), list("remove", GW("gwdegree"))))),
  lab = list(cands = list(c("edges", GW("gwesp"), NM("area"), GW("gwdegree")), c("edges", GW("gwesp"), NM("role"), AD("seniority")), c("edges", GW("gwesp"), GW("gwdsp"), NM("area"), GW("gwdegree"))),
             edits = list(list(list("add", NM("role")), list("add", NF("role"))), list(list("add", AD("seniority")), list("add", NC("seniority"))),
                          list(list("replace", GW("gwesp"), GW("gwesp","0.25")), list("replace", GW("gwdegree"), GW("gwdegree","0.25"))), list(list("remove", GW("gwdegree")), list("remove", GW("gwesp"))))),
  neighborhood = list(cands = list(c("edges", GW("gwesp"), NM("block"), GW("gwdegree")), c("edges", GW("gwesp"), NM("tenure_group"), AD("tenure_years")), c("edges", GW("gwesp"), GW("gwdsp"), NM("block"), GW("gwdegree"))),
             edits = list(list(list("add", NM("tenure_group")), list("add", NF("tenure_group"))), list(list("add", AD("tenure_years")), list("add", NC("tenure_years"))),
                          list(list("replace", GW("gwesp"), GW("gwesp","0.25")), list("replace", GW("gwdegree"), GW("gwdegree","0.25"))), list(list("remove", GW("gwdegree")), list("remove", GW("gwesp"))))),
  office = list(cands = list(c("edges", "mutual", GW("gwesp"), NM("department"), GW("gwidegree")), c("edges", "mutual", NM("level"), 'nodeicov("tenure")'), c("edges", "mutual", GW("gwesp"), NM("department"), 'nodeifactor("level")')),
             edits = list(list(list("add", 'nodeifactor("level")'), list("add", 'nodeicov("tenure")')), list(list("add", AD("tenure")), list("add", 'nodeocov("tenure")')),
                          list(list("replace", GW("gwesp"), GW("gwesp","0.25")), list("add", GW("gwesp","0.25"))), list(list("remove", GW("gwidegree")), list("remove", "mutual")))),
  opensource = list(cands = list(c("edges", GW("gwesp"), NM("module"), GW("gwdegree")), c("edges", GW("gwesp"), NM("role"), NC("commits")), c("edges", GW("gwesp"), GW("gwdsp"), NM("module"), GW("gwdegree"))),
             edits = list(list(list("add", NC("commits")), list("add", AD("commits"))), list(list("add", NM("role")), list("add", NF("role"))),
                          list(list("replace", GW("gwesp"), GW("gwesp","0.25")), list("replace", GW("gwdegree"), GW("gwdegree","0.25"))), list(list("remove", GW("gwdegree")), list("remove", GW("gwesp")))))
)
apply_edit <- function(terms, ed) {
  out <- NULL
  if (ed[[1]] == "add") { if (ed[[2]] %in% terms) return(NULL); out <- c(terms, ed[[2]]) }
  if (ed[[1]] == "remove") { if (!(ed[[2]] %in% terms)) return(NULL); out <- setdiff(terms, ed[[2]]) }
  if (ed[[1]] == "replace") { if (!(ed[[2]] %in% terms) || ed[[3]] %in% terms) return(NULL); terms[terms == ed[[2]]] <- ed[[3]]; out <- terms }
  if (is.null(out) || length(out) < 3 || length(out) > 8) return(NULL)   # compatibility rule: 3-8 terms
  out
}
ONLY <- Sys.getenv("FORGE_ONLY", "")
OUT <- if (nzchar(ONLY)) file.path(HERE, sprintf("run_records_%s.json", ONLY)) else file.path(HERE, "run_records.json")
results <- list()
for (nm in if (nzchar(ONLY)) ONLY else names(nets)) {
  g <- build_net(nets[[nm]]); plan <- plans[[nm]]
  for (est in c("sa", "mcmle", "mple")) {
    cat("=== ", nm, est, "\n")
    cands <- c(plan$cands, list("edges")); labels <- c("Candidate 1", "Candidate 2", "Candidate 3", "Edge-only baseline")
    stage2 <- lapply(seq_along(cands), function(i) { cat("  fit", labels[i], "\n"); evaluate(g, cands[[i]], labels[i], est) })
    elig <- Filter(function(r) isTRUE(r$eligible), stage2)
    rec <- list(estimator = est, candidates = stage2)
    if (!length(elig)) { rec$selected <- NULL; results[[nm]][[est]] <- rec; next }
    sel <- elig[[which.min(sapply(elig, `[[`, "q"))]]; rec$selected <- sel$label; rec$initial_q <- sel$q
    cur <- sel; rounds <- list()
    for (r in seq_along(plan$edits)) {
      newterms <- NULL; ed <- NULL
      for (alt in plan$edits[[r]]) { newterms <- apply_edit(unlist(cur$terms), alt); if (!is.null(newterms)) { ed <- alt; break } }
      if (is.null(newterms)) { cat("  round", r, "no applicable edit\n"); next }
      cat("  round", r, ":", ed[[1]], ed[[2]], if (length(ed) > 2) ed[[3]] else "", "\n")
      ev <- evaluate(g, newterms, sprintf("Round %d", r), est)
      acc <- isTRUE(ev$eligible) && ev$q < cur$q
      rounds[[length(rounds)+1]] <- list(round = r, action = ed[[1]], target = if (ed[[1]] == "replace") ed[[2]] else NULL,
        term = if (ed[[1]] == "replace") ed[[3]] else ed[[2]], terms = as.list(newterms), eligible = isTRUE(ev$eligible),
        q_before = cur$q, q_after = if (isTRUE(ev$eligible)) ev$q else NULL, pbic = ev$pbic, density_rel_error = ev$density_rel_error,
        residual = if (!is.null(ev$residuals)) ev$residuals[[1]] else NULL, accepted = acc,
        reason = if (acc) "eligible and lower q(M)" else if (!isTRUE(ev$eligible)) ev$reason else "eligible, but q(M) did not decrease",
        coefficients = ev$coefficients)
      if (acc) cur <- ev
    }
    rec$rounds <- rounds; final <- cur; final$label <- "Final model"; rec$final <- final
    results[[nm]][[est]] <- rec
    write_json(results, OUT, auto_unbox = TRUE, pretty = TRUE, na = "null")
  }
}
cat("DONE\n")
