# main_v3.tex — changes from v2 (per advisor meeting, 2026-07)

1. **Abstract shortened** 232 → ~160 words. Cut R-pipeline/run-bundle implementation
   details; restored the applications clause ("from friendships in schools ... to trade
   between organizations"); added the headline evaluation number (lowest BIC in 9/10
   converged comparisons).

2. **New Figure 1** (`fig1_pipeline.pdf`, TikZ source in `fig1_pipeline.tex`):
   input (network + truncated natural-language description) → FORGE architecture
   (LLM proposes / checks decide / one checked revision) → output (selected model +
   plain-language explanation). All numbers match the school-friendship walkthrough
   in the paper. Old `ergm_problem.pdf` moved to OBSOLETE/.

3. **Introduction**: citations added to the opening "networks are ubiquitous" sentence
   (Newman 2003; Borgatti et al. 2009 — new entries in references.bib); the long first
   paragraph split in two ("The hard part is writing that equation." now opens ¶2);
   ERGM-popularity sentence reordered per meeting (single equation first, statistics
   as "in addition"); figure references updated to fig:forge-overview.

4. **"demo" → "system/interface"** throughout the body (~14 replacements). "demo" kept
   only where appropriate: abstract/conclusion resource links, the demonstration-script
   paragraph, and the Availability section. Section label sec:demo → sec:interface.

5. Removed unused `soul` package (was only for v1 inline comments).

6. Still TODO (needs your data, cannot be done by editing the tex):
   - Appendix A: replace the compact prompt display with the exact full request
     (expanded L*, JSON schema, model ID, decoding settings) — see the TODO comment.

`main_v3_preview.pdf` was compiled in a sandbox with `courier` substituted for
`inconsolata` (not installed there); compile `main_v3.tex` unmodified on Overleaf /
your machine for the true layout.
