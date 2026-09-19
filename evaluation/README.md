# Evaluation data

Files behind the numbers reported in the paper, plus the recorded example runs
that the static walkthrough replays.

## Paper benchmarks

- `forge_16_network_aggregate.csv` — the 16-network benchmark: for each network,
  the best FORGE proposal, the best eligible baseline, the final model after
  revision, and the relative reduction in the GOF discrepancy q(M). Estimator:
  stochastic approximation (SA); selection and revision by q(M) as described in
  the paper. FORGE improves on the best baseline on 13 networks and matches it
  on 3 (median reduction 24.8%).
- `enron_stage3_trajectory.json` — the Enron revision example: the first three
  edits fail the density check, the fourth (`nodeofactor("role")`) is accepted
  and lowers q(M) from 73.92 to 29.41.
- `synthetic_mechanism_recovery_summary.csv` — the controlled benchmark
  (6 generating settings × 10 networks): exact recovery and term-level metrics
  for FORGE, few-shot, and one-shot prompting, with bootstrap intervals.

These are aggregate artifacts. Per-network fit objects and logs are not part of
the public release.

## Recorded example runs (`demo_examples/`)

The static demo at <https://yidans.github.io/forge-demo/> replays recorded
runs on the five packaged networks. `run_all.R` reproduces them with a fixed
candidate set and edit sequence (no LLM call), for every network and every
estimator (SA, MCMLE, MPLE):

```bash
Rscript evaluation/demo_examples/run_all.R
```

- `demo_networks.json` — the five networks (nodes, attributes, edges, layout).
- `libraries.json` — the valid-term list L* the R builder returns for each
  network with the live server's options.
- `run_records.json` — the output: candidates, eligibility, q(M), coefficients,
  and the four revision rounds per network and estimator. `demo/app.js` embeds
  this file; the LLM prompts and rationales shown in the static walkthrough are
  template text, the fit, GOF, and decision values are these records.
