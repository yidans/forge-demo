#!/usr/bin/env python3
"""Reproduce the paper's aggregate BIC-improvement figure from locked CSV data."""

from __future__ import annotations

import csv
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

T_975_DF9 = 2.262157162854099
METHODS = ("Random-K", "Best LLM-only", "FORGE")
BEST_COUNTS = ("1/10", "0/10", "9/10")


def load_values(path: Path) -> np.ndarray:
    rows = []
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            if row["non_null_converged"].upper() != "TRUE":
                continue
            null = float(row["bic_edge_null"])
            random_k = float(row["bic_random_k"])
            llm_only = min(float(row["bic_m3"]), float(row["bic_m4"]), float(row["bic_m5"]))
            forge = float(row["bic_forge"])
            rows.append([100 * (null - value) / null for value in (random_k, llm_only, forge)])
    values = np.asarray(rows)
    if values.shape != (10, 3):
        raise ValueError(f"Expected 10 converged rows, found shape {values.shape}")
    return values


def make_figure(csv_path: Path, output: Path) -> None:
    values = load_values(csv_path)
    means = values.mean(axis=0)
    half_width = T_975_DF9 * values.std(axis=0, ddof=1) / np.sqrt(values.shape[0])
    if not np.allclose(means, [6.3, 23.1, 26.4], atol=0.051):
        raise ValueError(f"Locked aggregate check failed: {means}")

    mpl.rcParams.update({"font.family": "DejaVu Sans", "font.size": 7.8, "pdf.fonttype": 42})
    fig, ax = plt.subplots(figsize=(3.4, 2.35))
    x = np.arange(3, dtype=float)
    colors = ("#9BBDE7", "#5C91D8", "#2157A6")
    ink = "#25354D"

    ax.bar(x, means, width=0.60, color=colors, edgecolor=ink, linewidth=0.7, zorder=3)
    ax.errorbar(x, means, yerr=half_width, fmt="none", ecolor=ink, capsize=2.6, zorder=5)
    rng = np.random.default_rng(7)
    for index in range(3):
        ax.scatter(x[index] + rng.uniform(-0.15, 0.15, size=10), values[:, index],
                   s=10.5, facecolor="#3D5675", edgecolor="white", linewidth=0.6, zorder=6)
        ax.text(x[index] - 0.28, means[index] + half_width[index] + 0.9,
                f"{means[index]:.1f}%", ha="center", va="bottom", fontweight="semibold")

    ax.set_ylim(0, 43)
    ax.set_yticks([0, 10, 20, 30, 40])
    ax.set_ylabel("Mean BIC improvement over\nedge-only null (%)")
    ax.set_xticks(x, [f"{name}\nbest in {count}" for name, count in zip(METHODS, BEST_COUNTS)])
    ax.yaxis.grid(True, color="#E4E9F0", linewidth=0.55)
    ax.set_axisbelow(True)
    ax.spines[["top", "right"]].set_visible(False)
    fig.subplots_adjust(left=0.20, right=0.985, bottom=0.155, top=0.965)
    fig.savefig(output, format="pdf", facecolor="white")
    plt.close(fig)


if __name__ == "__main__":
    directory = Path(__file__).resolve().parent
    make_figure(directory / "paper_benchmark_summary.csv", directory / "baseline_improvement.pdf")
