# Paper Evaluation Snapshot

`paper_benchmark_summary.csv` is the locked aggregate input used by the v3
paper figure. It records the ten benchmark rows for which non-null MCMLE refits
converged and identifies the two largest networks that did not converge under
the paper's fixed controls.

The CSV is a publication artifact, not a substitute for raw fit objects or
run-level logs. Local pipeline output belongs in `results/` and may differ when
models, prompts, random seeds, package versions, or MCMC controls change.

Run `python3 make_baseline_improvement.py` from this directory, or the command
shown in the repository README, to reproduce the aggregate PDF.
