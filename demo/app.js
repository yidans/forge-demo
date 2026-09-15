/* FORGE demo — static walkthrough of the pipeline.
 * Stages 0–1b use hand-written example prompts and outputs. Stages 2–4 are
 * generated from `runRecords`: real stochastic-approximation (SA) fits,
 * 30-simulation density checks, 100-simulation goodness-of-fit (GOF)
 * discrepancies q(M) = max_k |z_k|, and up-to-four-round checked revision,
 * computed with the ergm package on the three example networks. LLM prompts
 * and rationales in Stages 1b, 3 and 4 are illustrative text; numbers are not. */

function makeEdges(pairs) {
  return pairs.map(([source, target]) => ({ source, target }));
}

function makeKey(source, target) {
  return [source, target].sort().join("--");
}

function makeKeySet(pairs) {
  return new Set(pairs.map(([source, target]) => makeKey(source, target)));
}

function makeAdjacency(nodes, edges) {
  const adjacency = Object.fromEntries(nodes.map((node) => [node.id, new Set()]));
  edges.forEach(({ source, target }) => {
    adjacency[source].add(target);
    adjacency[target].add(source);
  });
  return adjacency;
}

function countTriangles(nodes, adjacency) {
  let triangles = 0;
  for (let i = 0; i < nodes.length; i += 1) {
    for (let j = i + 1; j < nodes.length; j += 1) {
      for (let k = j + 1; k < nodes.length; k += 1) {
        const a = nodes[i].id;
        const b = nodes[j].id;
        const c = nodes[k].id;
        if (adjacency[a].has(b) && adjacency[a].has(c) && adjacency[b].has(c)) {
          triangles += 1;
        }
      }
    }
  }
  return triangles;
}

function roundMetric(value, digits) {
  return Number(value.toFixed(digits));
}

function formatMetric(value, digits) {
  return value.toFixed(digits);
}

function graphDiagnostics(demo) {
  const adjacency = makeAdjacency(demo.nodes, demo.edges);
  const degreeById = Object.fromEntries(demo.nodes.map((node) => [node.id, adjacency[node.id].size]));
  const nodeCount = demo.nodes.length;
  const edgeCount = demo.edges.length;
  const triangles = countTriangles(demo.nodes, adjacency);
  const connectedTriples = demo.nodes.reduce((sum, node) => {
    const degree = degreeById[node.id];
    return sum + (degree * (degree - 1)) / 2;
  }, 0);
  const density = nodeCount > 1 ? (2 * edgeCount) / (nodeCount * (nodeCount - 1)) : 0;
  const transitivity = connectedTriples > 0 ? (3 * triangles) / connectedTriples : 0;
  const degreeMax = Math.max(...Object.values(degreeById));

  return {
    adjacency,
    degreeById,
    diagnostics: {
      nodes: nodeCount,
      edges: edgeCount,
      density,
      triangles,
      transitivity,
      degreeMax
    }
  };
}

function hydrateIntakeStage(demo) {
  const intake = demo.stages.find((stage) => stage.id === "intake");
  if (!intake) return;

  let visiblePatterns = [];
  try {
    visiblePatterns = JSON.parse(intake.output).visible_patterns || [];
  } catch (error) {
    visiblePatterns = [];
  }

  intake.metrics = [
    [String(demo.diagnostics.nodes), demo.nodeKind],
    [String(demo.diagnostics.edges), demo.tieKind],
    [formatMetric(demo.diagnostics.density, 2), "density"],
    [formatMetric(demo.diagnostics.transitivity, 2), "transitivity"]
  ];
  intake.output = JSON.stringify({
    nodes: demo.diagnostics.nodes,
    edges: demo.diagnostics.edges,
    directed: false,
    density: roundMetric(demo.diagnostics.density, 3),
    triangles: demo.diagnostics.triangles,
    transitivity: roundMetric(demo.diagnostics.transitivity, 3),
    degree_max: demo.diagnostics.degreeMax,
    visible_patterns: visiblePatterns
  }, null, 2);
}

// ---------------------------------------------------------------- term display

function shortTerm(term) {
  return term
    .replace(/,\s*fixed\s*=\s*TRUE/, "")
    .replace(/decay\s*=\s*/, "")
    .replace(/\("([^"]+)"\)/, "($1)");
}

const termMeanings = {
  edges: "baseline tie rate"
};

function termGloss(term) {
  if (termMeanings[term]) return termMeanings[term];
  const base = term.split("(")[0].trim();
  const attr = (term.match(/\(\s*"?([A-Za-z_]+)"?\s*\)/) || [])[1];
  const map = {
    edges: "baseline tie rate",
    mutual: "reciprocated ties",
    gwesp: "shared partners / closure",
    gwdsp: "open two-path pressure",
    gwdegree: "degree spread / hub structure",
    gwidegree: "incoming-tie concentration",
    gwodegree: "outgoing-tie concentration",
    nodematch: `same-${attr || "group"} ties`,
    nodemix: `${attr || "group"} pairing mix`,
    nodefactor: `${attr || "group"}-level activity`,
    nodeifactor: `${attr || "group"} incoming activity`,
    nodeofactor: `${attr || "group"} outgoing activity`,
    nodecov: `ties scale with ${attr || "attribute"}`,
    absdiff: `${attr || "attribute"} difference between the pair`
  };
  return map[base] || "model mechanism";
}

// ---------------------------------------------------------------- real run records

const runRecords = {
  "school": {
    "candidates": [
      {
        "label": "Candidate 1",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"club\")",
          "gwdegree(0.5, fixed=TRUE)"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -3.228
          ],
          [
            "gwesp.fixed.0.5",
            0.338
          ],
          [
            "nodematch.club",
            2.648
          ],
          [
            "gwdeg.fixed.0.5",
            2.41
          ]
        ],
        "density_obs": 0.273,
        "density_sim": 0.283,
        "density_rel_error": 0.037,
        "q": 1.3,
        "gof_rmse": 0.64,
        "pbic": 64.32,
        "gof_bins": 21,
        "residuals": [
          [
            "degree",
            "degree4",
            5,
            2.88,
            1.3
          ],
          [
            "esp",
            "esp1",
            12,
            8.14,
            1.23
          ],
          [
            "distance",
            "3",
            20,
            13.94,
            1.12
          ]
        ]
      },
      {
        "label": "Candidate 2",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"grade\")",
          "absdiff(\"activity\")"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -1.477
          ],
          [
            "gwesp.fixed.0.5",
            0.405
          ],
          [
            "nodematch.grade",
            0.391
          ],
          [
            "absdiff.activity",
            -0.159
          ]
        ],
        "density_obs": 0.273,
        "density_sim": 0.272,
        "density_rel_error": 0.004,
        "q": 1.92,
        "gof_rmse": 0.82,
        "pbic": 78.38,
        "gof_bins": 24,
        "residuals": [
          [
            "distance",
            "3",
            20,
            10.33,
            1.92
          ],
          [
            "degree",
            "degree4",
            5,
            2.31,
            1.83
          ],
          [
            "esp",
            "esp1",
            12,
            7.07,
            1.57
          ]
        ]
      },
      {
        "label": "Candidate 3",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "nodematch(\"club\")",
          "gwdegree(0.5, fixed=TRUE)"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -5.867
          ],
          [
            "gwesp.fixed.0.5",
            0.283
          ],
          [
            "gwdsp.fixed.0.5",
            0.524
          ],
          [
            "nodematch.club",
            3.454
          ],
          [
            "gwdeg.fixed.0.5",
            3.541
          ]
        ],
        "density_obs": 0.273,
        "density_sim": 0.266,
        "density_rel_error": 0.024,
        "q": 2.68,
        "gof_rmse": 0.76,
        "pbic": 62.96,
        "gof_bins": 22,
        "residuals": [
          [
            "degree",
            "degree4",
            5,
            1.83,
            2.68
          ],
          [
            "degree",
            "degree5",
            0,
            0.82,
            -1
          ],
          [
            "esp",
            "esp1",
            12,
            9.33,
            0.96
          ]
        ]
      },
      {
        "label": "Edge-only baseline",
        "terms": [
          "edges"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -0.981
          ]
        ],
        "density_obs": 0.273,
        "density_sim": 0.274,
        "density_rel_error": 0.006,
        "q": 1.74,
        "gof_rmse": 0.83,
        "pbic": 81.54,
        "gof_bins": 22,
        "residuals": [
          [
            "distance",
            "3",
            20,
            12.83,
            1.74
          ],
          [
            "esp",
            "esp0",
            3,
            8.31,
            -1.7
          ],
          [
            "degree",
            "degree4",
            5,
            2.33,
            1.67
          ]
        ]
      }
    ],
    "selected": "Candidate 1",
    "initial_q": 1.3,
    "rounds": [
      {
        "round": 1,
        "action": "add",
        "target": null,
        "term": "nodematch(\"grade\")",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"club\")",
          "gwdegree(0.5, fixed=TRUE)",
          "nodematch(\"grade\")"
        ],
        "eligible": true,
        "q_before": 1.3,
        "q_after": 1.74,
        "pbic": 68.07,
        "density_rel_error": 0.026,
        "residual": [
          "degree",
          "degree4",
          5,
          2.45,
          1.74
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      },
      {
        "round": 2,
        "action": "add",
        "target": null,
        "term": "absdiff(\"activity\")",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"club\")",
          "gwdegree(0.5, fixed=TRUE)",
          "absdiff(\"activity\")"
        ],
        "eligible": true,
        "q_before": 1.3,
        "q_after": 1.72,
        "pbic": 67.7,
        "density_rel_error": 0.026,
        "residual": [
          "degree",
          "degree4",
          5,
          2.37,
          1.72
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      },
      {
        "round": 3,
        "action": "replace",
        "target": "gwdegree(0.5, fixed=TRUE)",
        "term": "gwdegree(1.0, fixed=TRUE)",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"club\")",
          "gwdegree(1.0, fixed=TRUE)"
        ],
        "eligible": true,
        "q_before": 1.3,
        "q_after": 1.78,
        "pbic": 63.83,
        "density_rel_error": 0.015,
        "residual": [
          "degree",
          "degree4",
          5,
          2.43,
          1.78
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      },
      {
        "round": 4,
        "action": "remove",
        "target": null,
        "term": "gwesp(0.5, fixed=TRUE)",
        "terms": [
          "edges",
          "nodematch(\"club\")",
          "gwdegree(0.5, fixed=TRUE)"
        ],
        "eligible": true,
        "q_before": 1.3,
        "q_after": 1.55,
        "pbic": 61.7,
        "density_rel_error": 0.05,
        "residual": [
          "degree",
          "degree4",
          5,
          2.7,
          1.55
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      }
    ],
    "final": {
      "label": "Final model",
      "terms": [
        "edges",
        "gwesp(0.5, fixed=TRUE)",
        "nodematch(\"club\")",
        "gwdegree(0.5, fixed=TRUE)"
      ],
      "eligible": true,
      "reason": "eligible",
      "coefficients": [
        [
          "edges",
          -3.228
        ],
        [
          "gwesp.fixed.0.5",
          0.338
        ],
        [
          "nodematch.club",
          2.648
        ],
        [
          "gwdeg.fixed.0.5",
          2.41
        ]
      ],
      "density_obs": 0.273,
      "density_sim": 0.283,
      "density_rel_error": 0.037,
      "q": 1.3,
      "gof_rmse": 0.64,
      "pbic": 64.32,
      "gof_bins": 21,
      "residuals": [
        [
          "degree",
          "degree4",
          5,
          2.88,
          1.3
        ],
        [
          "esp",
          "esp1",
          12,
          8.14,
          1.23
        ],
        [
          "distance",
          "3",
          20,
          13.94,
          1.12
        ]
      ]
    }
  },
  "lab": {
    "candidates": [
      {
        "label": "Candidate 1",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"area\")",
          "gwdegree(0.5, fixed=TRUE)"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -3.925
          ],
          [
            "gwesp.fixed.0.5",
            -0.39
          ],
          [
            "nodematch.area",
            4.077
          ],
          [
            "gwdeg.fixed.0.5",
            14.076
          ]
        ],
        "density_obs": 0.303,
        "density_sim": 0.305,
        "density_rel_error": 0.005,
        "q": 1.66,
        "gof_rmse": 0.61,
        "pbic": 53.04,
        "gof_bins": 16,
        "residuals": [
          [
            "esp",
            "esp1",
            12,
            7.12,
            1.66
          ],
          [
            "esp",
            "esp2",
            3,
            6.97,
            -0.95
          ],
          [
            "degree",
            "degree5",
            1,
            0.49,
            0.81
          ]
        ]
      },
      {
        "label": "Candidate 2",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"role\")",
          "absdiff(\"seniority\")"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -0.197
          ],
          [
            "gwesp.fixed.0.5",
            0.129
          ],
          [
            "nodematch.role",
            -0.507
          ],
          [
            "absdiff.seniority",
            -0.249
          ]
        ],
        "density_obs": 0.303,
        "density_sim": 0.307,
        "density_rel_error": 0.012,
        "q": 2.52,
        "gof_rmse": 0.78,
        "pbic": 93.4,
        "gof_bins": 24,
        "residuals": [
          [
            "degree",
            "degree3",
            7,
            3.09,
            2.52
          ],
          [
            "distance",
            "3",
            18,
            11.61,
            1.38
          ],
          [
            "esp",
            "esp1",
            12,
            8.01,
            1.34
          ]
        ]
      },
      {
        "label": "Candidate 3",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "nodematch(\"area\")",
          "gwdegree(0.5, fixed=TRUE)"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -10.622
          ],
          [
            "gwesp.fixed.0.5",
            -0.792
          ],
          [
            "gwdsp.fixed.0.5",
            1.182
          ],
          [
            "nodematch.area",
            6.286
          ],
          [
            "gwdeg.fixed.0.5",
            22.064
          ]
        ],
        "density_obs": 0.303,
        "density_sim": 0.295,
        "density_rel_error": 0.027,
        "q": 1.08,
        "gof_rmse": 0.44,
        "pbic": 52.97,
        "gof_bins": 15,
        "residuals": [
          [
            "esp",
            "esp1",
            12,
            8.7,
            1.08
          ],
          [
            "esp",
            "esp2",
            3,
            5.25,
            -0.65
          ],
          [
            "degree",
            "degree4",
            3,
            3.76,
            -0.55
          ]
        ]
      },
      {
        "label": "Edge-only baseline",
        "terms": [
          "edges"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -0.833
          ]
        ],
        "density_obs": 0.303,
        "density_sim": 0.292,
        "density_rel_error": 0.037,
        "q": 2.61,
        "gof_rmse": 0.83,
        "pbic": 85.16,
        "gof_bins": 23,
        "residuals": [
          [
            "degree",
            "degree3",
            7,
            2.96,
            2.61
          ],
          [
            "distance",
            "3",
            18,
            11.35,
            1.52
          ],
          [
            "esp",
            "esp1",
            12,
            8.04,
            1.31
          ]
        ]
      }
    ],
    "selected": "Candidate 3",
    "initial_q": 1.08,
    "rounds": [
      {
        "round": 1,
        "action": "add",
        "target": null,
        "term": "nodematch(\"role\")",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "nodematch(\"area\")",
          "gwdegree(0.5, fixed=TRUE)",
          "nodematch(\"role\")"
        ],
        "eligible": true,
        "q_before": 1.08,
        "q_after": 1.12,
        "pbic": 48.39,
        "density_rel_error": 0.018,
        "residual": [
          "esp",
          "esp1",
          12,
          8.1,
          1.12
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      },
      {
        "round": 2,
        "action": "add",
        "target": null,
        "term": "absdiff(\"seniority\")",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "nodematch(\"area\")",
          "gwdegree(0.5, fixed=TRUE)",
          "absdiff(\"seniority\")"
        ],
        "eligible": true,
        "q_before": 1.08,
        "q_after": 1.03,
        "pbic": 45.43,
        "density_rel_error": 0.007,
        "residual": [
          "esp",
          "esp1",
          12,
          9.05,
          1.03
        ],
        "accepted": true,
        "reason": "eligible and lower q(M)"
      },
      {
        "round": 3,
        "action": "replace",
        "target": "gwesp(0.5, fixed=TRUE)",
        "term": "gwesp(0.25, fixed=TRUE)",
        "terms": [
          "edges",
          "gwesp(0.25, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "nodematch(\"area\")",
          "gwdegree(0.5, fixed=TRUE)",
          "absdiff(\"seniority\")"
        ],
        "eligible": true,
        "q_before": 1.03,
        "q_after": 1.21,
        "pbic": 46.99,
        "density_rel_error": 0.018,
        "residual": [
          "esp",
          "esp1",
          12,
          8.74,
          1.21
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      },
      {
        "round": 4,
        "action": "remove",
        "target": null,
        "term": "gwdegree(0.5, fixed=TRUE)",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "nodematch(\"area\")",
          "absdiff(\"seniority\")"
        ],
        "eligible": true,
        "q_before": 1.03,
        "q_after": 1.65,
        "pbic": 52.98,
        "density_rel_error": 0.015,
        "residual": [
          "esp",
          "esp1",
          12,
          7.14,
          1.65
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      }
    ],
    "final": {
      "label": "Final model",
      "terms": [
        "edges",
        "gwesp(0.5, fixed=TRUE)",
        "gwdsp(0.5, fixed=TRUE)",
        "nodematch(\"area\")",
        "gwdegree(0.5, fixed=TRUE)",
        "absdiff(\"seniority\")"
      ],
      "eligible": true,
      "reason": "eligible",
      "coefficients": [
        [
          "edges",
          -7.819
        ],
        [
          "gwesp.fixed.0.5",
          -0.677
        ],
        [
          "gwdsp.fixed.0.5",
          0.988
        ],
        [
          "nodematch.area",
          7.454
        ],
        [
          "gwdeg.fixed.0.5",
          18.8
        ],
        [
          "absdiff.seniority",
          -0.767
        ]
      ],
      "density_obs": 0.303,
      "density_sim": 0.305,
      "density_rel_error": 0.007,
      "q": 1.03,
      "gof_rmse": 0.41,
      "pbic": 45.43,
      "gof_bins": 16,
      "residuals": [
        [
          "esp",
          "esp1",
          12,
          9.05,
          1.03
        ],
        [
          "degree",
          "degree3",
          7,
          6.03,
          0.56
        ],
        [
          "esp",
          "esp2",
          3,
          5.03,
          -0.56
        ]
      ]
    }
  },
  "neighborhood": {
    "candidates": [
      {
        "label": "Candidate 1",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"block\")",
          "gwdegree(0.5, fixed=TRUE)"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -4.222
          ],
          [
            "gwesp.fixed.0.5",
            -0.131
          ],
          [
            "nodematch.block",
            3.835
          ],
          [
            "gwdeg.fixed.0.5",
            8.522
          ]
        ],
        "density_obs": 0.273,
        "density_sim": 0.272,
        "density_rel_error": 0.002,
        "q": 1.46,
        "gof_rmse": 0.63,
        "pbic": 49.18,
        "gof_bins": 19,
        "residuals": [
          [
            "esp",
            "esp1",
            12,
            7.31,
            1.46
          ],
          [
            "distance",
            "3",
            21,
            13.75,
            1.24
          ],
          [
            "distance",
            "5",
            0,
            2.51,
            -0.81
          ]
        ]
      },
      {
        "label": "Candidate 2",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"tenure_group\")",
          "absdiff(\"tenure_years\")"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -2.192
          ],
          [
            "gwesp.fixed.0.5",
            0.387
          ],
          [
            "nodematch.tenure_group",
            0.146
          ],
          [
            "absdiff.tenure_years",
            0.086
          ]
        ],
        "density_obs": 0.273,
        "density_sim": 0.283,
        "density_rel_error": 0.039,
        "q": 2.75,
        "gof_rmse": 1.03,
        "pbic": 78.67,
        "gof_bins": 25,
        "residuals": [
          [
            "degree",
            "degree3",
            6,
            2.25,
            2.75
          ],
          [
            "distance",
            "3",
            21,
            10.42,
            2.43
          ],
          [
            "distance",
            "4",
            9,
            3.14,
            1.77
          ]
        ]
      },
      {
        "label": "Candidate 3",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "nodematch(\"block\")",
          "gwdegree(0.5, fixed=TRUE)"
        ],
        "eligible": false,
        "reason": "fit failed: Matrix 'x' has negative elements on the diagonal."
      },
      {
        "label": "Edge-only baseline",
        "terms": [
          "edges"
        ],
        "eligible": true,
        "reason": "eligible",
        "coefficients": [
          [
            "edges",
            -0.981
          ]
        ],
        "density_obs": 0.273,
        "density_sim": 0.265,
        "density_rel_error": 0.028,
        "q": 1.9,
        "gof_rmse": 0.91,
        "pbic": 81.54,
        "gof_bins": 25,
        "residuals": [
          [
            "degree",
            "degree3",
            6,
            3.21,
            1.9
          ],
          [
            "distance",
            "3",
            21,
            13.14,
            1.78
          ],
          [
            "distance",
            "4",
            9,
            3.41,
            1.75
          ]
        ]
      }
    ],
    "selected": "Candidate 1",
    "initial_q": 1.46,
    "rounds": [
      {
        "round": 1,
        "action": "add",
        "target": null,
        "term": "nodematch(\"tenure_group\")",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"block\")",
          "gwdegree(0.5, fixed=TRUE)",
          "nodematch(\"tenure_group\")"
        ],
        "eligible": true,
        "q_before": 1.46,
        "q_after": 1.44,
        "pbic": 53.32,
        "density_rel_error": 0.006,
        "residual": [
          "esp",
          "esp1",
          12,
          6.85,
          1.44
        ],
        "accepted": true,
        "reason": "eligible and lower q(M)"
      },
      {
        "round": 2,
        "action": "add",
        "target": null,
        "term": "absdiff(\"tenure_years\")",
        "terms": [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "nodematch(\"block\")",
          "gwdegree(0.5, fixed=TRUE)",
          "nodematch(\"tenure_group\")",
          "absdiff(\"tenure_years\")"
        ],
        "eligible": true,
        "q_before": 1.44,
        "q_after": 1.35,
        "pbic": 53.94,
        "density_rel_error": 0.019,
        "residual": [
          "esp",
          "esp1",
          12,
          7.08,
          1.35
        ],
        "accepted": true,
        "reason": "eligible and lower q(M)"
      },
      {
        "round": 3,
        "action": "replace",
        "target": "gwesp(0.5, fixed=TRUE)",
        "term": "gwesp(0.25, fixed=TRUE)",
        "terms": [
          "edges",
          "gwesp(0.25, fixed=TRUE)",
          "nodematch(\"block\")",
          "gwdegree(0.5, fixed=TRUE)",
          "nodematch(\"tenure_group\")",
          "absdiff(\"tenure_years\")"
        ],
        "eligible": true,
        "q_before": 1.35,
        "q_after": 1.25,
        "pbic": 53.3,
        "density_rel_error": 0.002,
        "residual": [
          "distance",
          "3",
          21,
          13.65,
          1.25
        ],
        "accepted": true,
        "reason": "eligible and lower q(M)"
      },
      {
        "round": 4,
        "action": "remove",
        "target": null,
        "term": "gwdegree(0.5, fixed=TRUE)",
        "terms": [
          "edges",
          "gwesp(0.25, fixed=TRUE)",
          "nodematch(\"block\")",
          "nodematch(\"tenure_group\")",
          "absdiff(\"tenure_years\")"
        ],
        "eligible": true,
        "q_before": 1.25,
        "q_after": 1.39,
        "pbic": 57.65,
        "density_rel_error": 0.011,
        "residual": [
          "esp",
          "esp1",
          12,
          7.47,
          1.39
        ],
        "accepted": false,
        "reason": "eligible, but q(M) did not decrease"
      }
    ],
    "final": {
      "label": "Final model",
      "terms": [
        "edges",
        "gwesp(0.25, fixed=TRUE)",
        "nodematch(\"block\")",
        "gwdegree(0.5, fixed=TRUE)",
        "nodematch(\"tenure_group\")",
        "absdiff(\"tenure_years\")"
      ],
      "eligible": true,
      "reason": "eligible",
      "coefficients": [
        [
          "edges",
          -6.291
        ],
        [
          "gwesp.fixed.0.25",
          0.016
        ],
        [
          "nodematch.block",
          3.735
        ],
        [
          "gwdeg.fixed.0.5",
          8.546
        ],
        [
          "nodematch.tenure_group",
          1.971
        ],
        [
          "absdiff.tenure_years",
          0.216
        ]
      ],
      "density_obs": 0.273,
      "density_sim": 0.273,
      "density_rel_error": 0.002,
      "q": 1.25,
      "gof_rmse": 0.58,
      "pbic": 53.3,
      "gof_bins": 19,
      "residuals": [
        [
          "distance",
          "3",
          21,
          13.65,
          1.25
        ],
        [
          "esp",
          "esp1",
          12,
          8.27,
          1.14
        ],
        [
          "distance",
          "5",
          0,
          3.08,
          -0.93
        ]
      ]
    }
  }
};

// ---------------------------------------------------------------- shared text

const guardrailSets = {
  intake: [
    ["pass", "No missing node attributes in the network"],
    ["pass", "Undirected ties have no self-loops"],
    ["pass", "Binary, static network: supported by the term list"]
  ],
  library: [
    ["pass", "Every term is available in ergm syntax"],
    ["pass", "Reciprocity terms excluded: network is undirected"],
    ["pass", "Raw triangle excluded; closure via gwesp/gwdsp"]
  ],
  spec: [
    ["pass", "All proposed terms come from L*"],
    ["pass", "Every specification includes edges"],
    ["pass", "At most three ranked candidates, valid JSON"]
  ],
  interpret: [
    ["pass", "Every claim maps to a fitted term and its sign"],
    ["pass", "Limitations kept separate from findings"],
    ["pass", "Conditional associations, no causal claims"]
  ]
};

function joinTerms(terms) {
  return terms.map(shortTerm).join(" + ");
}

function pct(value) {
  return `${(100 * value).toFixed(1)}%`;
}

function signed(value) {
  return `${value >= 0 ? "+" : ""}${value.toFixed(2)}`;
}

function residualLine([stat, bin, obs, simMean, z]) {
  const label = stat === "esp" ? `shared partners = ${String(bin).replace(/^esp/, "")}`
    : stat === "degree" ? `degree = ${String(bin).replace(/^degree/, "")}`
    : `geodesic distance = ${bin}`;
  return `${label}: observed ${obs}, simulated mean ${simMean}, z = ${signed(z)}`;
}

function editLabel(round) {
  if (round.action === "replace") {
    return `replace ${shortTerm(round.target)} with ${shortTerm(round.term)}`;
  }
  return `${round.action} ${shortTerm(round.term)}`;
}

function compactTerm(term) {
  return shortTerm(term).replace(/^nodematch\(/, "match(").replace(/^nodefactor\(/, "factor(");
}

function editShort(round) {
  if (round.action === "replace") {
    const base = round.term.split("(")[0];
    const from = (round.target.match(/\(([0-9.]+)/) || [])[1];
    const to = (round.term.match(/\(([0-9.]+)/) || [])[1];
    return from && to ? `${base} ${from}→${to}` : `${compactTerm(round.target)}→${compactTerm(round.term)}`;
  }
  return `${round.action === "add" ? "+" : "−"}${compactTerm(round.term)}`;
}

// ---------------------------------------------------------------- stage builders (2–4)

function buildFitStage(demo, run, text) {
  const cands = run.candidates;
  const eligible = cands.filter((c) => c.eligible);
  const selected = cands.find((c) => c.label === run.selected);
  const ineligible = cands.filter((c) => !c.eligible);
  const finiteCount = cands.filter((c) => c.coefficients).length;
  const densityPass = cands.filter((c) => c.density_rel_error !== undefined && c.density_rel_error <= 0.25).length;

  const bic = cands.map((c) => [
    c.label.replace("Edge-only baseline", "Edge-only"),
    c.eligible ? c.q : null,
    !c.eligible ? "ineligible" : c.label === run.selected ? "selected" : "eligible"
  ]);

  const prompt = [
    "Stage 2 — statistical fitting and selection (R backend, no LLM call)",
    "",
    "candidate pool:",
    ...cands.map((c) => `  ${c.label} = ${joinTerms(c.terms)}`),
    "",
    "procedure:",
    "  1. fit each candidate with stochastic approximation (SA)",
    "  2. eligibility A(M) = 1 requires: finite coefficients,",
    "     successful simulation, density check (30 simulated networks,",
    "     |relative density error| <= 25%), computable GOF diagnostics",
    "  3. GOF from 100 simulated networks (same seed for every candidate):",
    "     degree, edgewise shared partners, geodesic distance",
    "     z_k = (obs_k - mean_sim_k) / sd_sim_k;   q(M) = max_k |z_k|",
    "  4. select M0 = argmin q(M) over eligible candidates",
    "     (MPLE pseudo-BIC is recorded as a secondary diagnostic only)"
  ].join("\n");

  const output = JSON.stringify(cands.map((c) => {
    const row = { candidate: c.label, formula: joinTerms(c.terms), eligible: c.eligible };
    if (c.eligible) {
      row.density_rel_error = c.density_rel_error;
      row.q = c.q;
      row.largest_residual = c.residuals ? residualLine(c.residuals[0]) : null;
      row.pbic_secondary = c.pbic;
      row.decision = c.label === run.selected ? "selected: lowest q(M) among eligible" : "eligible";
    } else {
      row.decision = `ineligible: ${c.reason}`;
    }
    return row;
  }), null, 2);

  const guardrails = [
    [finiteCount === cands.length ? "pass" : "warn", `SA returned finite coefficients for ${finiteCount}/${cands.length} candidates`],
    [densityPass === finiteCount ? "pass" : "warn", `Density check passed for ${densityPass}/${finiteCount} fitted candidates (≤25% error)`],
    ["pass", `GOF computed from 100 simulations for ${eligible.length} eligible candidates`],
    ["pass", `Selected ${run.selected}: lowest q(M) = ${selected.q.toFixed(2)}`]
  ];

  return {
    id: "fit",
    number: "2",
    rail: "Fit & select",
    subtitle: "SA fit + GOF",
    kicker: "Stage 2",
    title: "Fit Candidates and Select by GOF",
    status: "Stage 2: SA fit and selection",
    lens: "lowest q(M)",
    mechanismTitle: `${run.selected} wins with the smallest GOF discrepancy`,
    mechanismCopy: text.fitCopy(run, selected, eligible, ineligible),
    metrics: [
      [selected.q.toFixed(2), "lowest q(M)"],
      [`${eligible.length}/${cands.length}`, "eligible candidates"],
      [run.selected.replace("Candidate ", "C"), "selected M0"],
      [String(selected.pbic), "PBIC (secondary)"]
    ],
    terms: selected.terms,
    guardrails,
    chartTitle: "GOF discrepancy q(M) · lower is better",
    chartLabel: `${run.selected} selected`,
    bic,
    prompt,
    output,
    outputBadge: "fit + GOF table",
    highlight: "winner",
    theory: text.fitTheory(run, selected)
  };
}

function buildReviseStage(demo, run, text) {
  const rounds = run.rounds;
  const accepted = rounds.filter((r) => r.accepted);
  const selected = run.candidates.find((c) => c.label === run.selected);
  const final = run.final;
  const reduction = (selected.q - final.q) / selected.q;
  const lastRound = rounds[rounds.length - 1];
  const priorRounds = rounds.slice(0, -1);
  const currentTerms = lastRound.accepted
    ? rounds.filter((r) => r.round < lastRound.round && r.accepted).slice(-1)[0]?.terms || selected.terms
    : (accepted.length ? accepted[accepted.length - 1].terms : selected.terms);
  const currentBeforeLast = priorRounds.filter((r) => r.accepted).slice(-1)[0] || null;
  const qCurrent = currentBeforeLast ? currentBeforeLast.q_after : selected.q;
  const currentResidual = currentBeforeLast
    ? currentBeforeLast.residual
    : (selected.residuals ? selected.residuals[0] : null);
  const historyLines = priorRounds.map((r) => {
    const outcome = r.accepted
      ? `accepted, q = ${r.q_after.toFixed(2)}`
      : (r.eligible ? `rejected, q = ${r.q_after.toFixed(2)} (no decrease)` : `rejected, ${r.reason}`);
    return `  round ${r.round}: ${editLabel(r)} -> ${outcome}`;
  });
  const rejectedSoFar = priorRounds.filter((r) => !r.accepted);

  const prompt = [
    "system:",
    "You are an ERGM expert. Return JSON only.",
    "",
    "user:",
    `Round ${lastRound.round} of 4.`,
    `Current specification M_${lastRound.round - 1} = ${joinTerms(currentBeforeLast ? currentBeforeLast.terms : selected.terms)}`,
    `q(M_${lastRound.round - 1}) = ${qCurrent.toFixed(2)}   (largest |z| over GOF bins; lower is better)`,
    currentResidual ? `largest GOF residual: ${residualLine(currentResidual)}` : "",
    `valid terms L*: ${demo.libraryTerms.map(shortTerm).join(", ")}`,
    "edit history:",
    ...historyLines,
    rejectedSoFar.length ? `Do not repeat the ${rejectedSoFar.length} rejected edit${rejectedSoFar.length > 1 ? "s" : ""} above.` : "",
    "",
    "Propose exactly ONE edit — add, remove, or replace a single term —",
    "with a reason tied to the residual above. The edit is kept only if",
    "the refitted model is eligible and strictly lowers q(M)."
  ].filter((line) => line !== "").join("\n");

  const output = JSON.stringify({
    M0: joinTerms(selected.terms),
    q_M0: selected.q,
    rounds: rounds.map((r) => ({
      round: r.round,
      action: r.action,
      ...(r.target ? { target: shortTerm(r.target) } : {}),
      term: shortTerm(r.term),
      rationale: text.rationales[r.round - 1],
      eligible: r.eligible,
      q_before: r.q_before,
      q_after: r.eligible ? r.q_after : null,
      accepted: r.accepted,
      decision: r.accepted
        ? `accepted: eligible and q(M) fell ${r.q_before.toFixed(2)} → ${r.q_after.toFixed(2)}`
        : (r.eligible
          ? `rejected: q(M) did not decrease (${r.q_after.toFixed(2)} vs ${r.q_before.toFixed(2)})`
          : `rejected: ${r.reason}`)
    })),
    final: { formula: joinTerms(final.terms), q: final.q, pbic_secondary: final.pbic }
  }, null, 2);

  const bic = [
    ["M0 (Stage 2)", selected.q, "selected"],
    ...rounds.map((r) => [
      `R${r.round} ${editShort(r)}`,
      r.eligible ? r.q_after : null,
      r.accepted ? "accepted" : (r.eligible ? "rejected" : "ineligible")
    ])
  ];

  const guardrails = [
    ["pass", "Each round proposes exactly one add / remove / replace"],
    ["pass", "Every proposed term comes from L*; rejected edits are not repeated"],
    [rounds.every((r) => r.eligible) ? "pass" : "warn", `${rounds.filter((r) => r.eligible).length}/4 revised models passed the eligibility checks`],
    ["pass", `Kept ${accepted.length} edit${accepted.length === 1 ? "" : "s"}: q(M_T) = ${final.q.toFixed(2)} ≤ q(M_0) = ${selected.q.toFixed(2)}`]
  ];

  return {
    id: "refine",
    number: "3",
    rail: "Revise",
    subtitle: "≤4 checked rounds",
    kicker: "Stage 3",
    title: "Diagnostic-Guided Revision",
    status: "Stage 3: checked revision",
    lens: accepted.length ? "accepted edits" : "no accepted edit",
    mechanismTitle: accepted.length
      ? `${accepted.length} of 4 proposed edits lowered q(M) and were kept`
      : "Four edits proposed, none lowered q(M): Stage 2 model retained",
    mechanismCopy: text.reviseCopy(run, selected, accepted, final),
    metrics: [
      [final.q.toFixed(2), "final q(M)"],
      [`${accepted.length}/4`, "accepted / rounds"],
      [pct(reduction), "q(M) reduction"],
      [String(final.terms.length), "terms in final model"]
    ],
    terms: final.terms,
    rounds: rounds.map((r) => ({
      round: r.round,
      edit: editLabel(r),
      accepted: r.accepted,
      eligible: r.eligible,
      qBefore: r.q_before,
      qAfter: r.eligible ? r.q_after : null,
      reason: r.accepted ? "eligible, q(M) decreased" : (r.eligible ? "eligible, q(M) did not decrease" : r.reason)
    })),
    guardrails,
    chartTitle: "GOF discrepancy q(M) · lower is better",
    chartLabel: `${accepted.length}/4 accepted`,
    bic,
    prompt,
    output,
    outputBadge: "revision record",
    highlight: "refined",
    theory: text.reviseTheory(run, selected, accepted, final)
  };
}

function buildInterpretStage(demo, run, text) {
  const final = run.final;
  const baseline = run.candidates.find((c) => c.label === "Edge-only baseline");
  const selected = run.candidates.find((c) => c.label === run.selected);
  const accepted = run.rounds.filter((r) => r.accepted);
  const attrCount = new Set(final.terms.map((t) => (t.match(/\("([^"]+)"\)/) || [])[1]).filter(Boolean)).size;
  const coefLines = final.coefficients.map(([term, est], i) =>
    `  ${shortTerm(final.terms[i] || term).padEnd(24)} coef = ${signed(est)}`);

  const prompt = [
    "system:",
    "You are an ERGM expert writing for a non-specialist. Return JSON only.",
    "",
    "user:",
    "Final specification (fixed after Stage 3; do not change it):",
    ...coefLines,
    `GOF: q(M) = ${final.q.toFixed(2)} over 100 simulated networks; density check passed.`,
    `Revision history: 4 edits proposed, ${accepted.length} accepted.`,
    "",
    "Map each retained term to its tie-formation mechanism and explain the",
    "fitted association conditional on the other terms, using the sign of",
    "each coefficient. List limitations separately. Make no causal claims."
  ].join("\n");

  const output = JSON.stringify({
    headline: text.headline,
    term_interpretations: text.termReadings,
    summary: text.summary,
    limitations: text.limitations
  }, null, 2);

  const bic = [
    ["Edge-only", baseline ? baseline.q : null, baseline && baseline.eligible ? "eligible" : "ineligible"],
    [`${run.selected} (M0)`, selected.q, "eligible"],
    ["Final (M_T)", final.q, "selected"]
  ];

  return {
    id: "interpret",
    number: "4",
    rail: "Interpret",
    subtitle: "Term-linked summary",
    kicker: "Stage 4",
    title: "Plain-Language Explanation",
    status: "Stage 4: interpretation",
    lens: "final interpretation",
    mechanismTitle: "What the fitted model supports",
    mechanismCopy: text.summary,
    metrics: [
      [String(final.terms.length), "terms"],
      [String(attrCount), "attributes"],
      [String(text.limitations.length), "limitations"],
      ["0", "causal claims"]
    ],
    terms: final.terms,
    rounds: run.rounds.map((r) => ({
      round: r.round,
      edit: editLabel(r),
      accepted: r.accepted,
      eligible: r.eligible,
      qBefore: r.q_before,
      qAfter: r.eligible ? r.q_after : null,
      reason: r.accepted ? "eligible, q(M) decreased" : (r.eligible ? "eligible, q(M) did not decrease" : r.reason)
    })),
    guardrails: guardrailSets.interpret,
    chartTitle: "GOF discrepancy q(M) · lower is better",
    chartLabel: "final",
    bic,
    prompt,
    output,
    outputBadge: "interpretation JSON",
    highlight: "final",
    theoryHeadline: text.headline,
    theory: text.summary
  };
}

// ---------------------------------------------------------------- example networks

const networkDemos = [
  {
    id: "school",
    shortLabel: "School",
    title: "School Friendship Network",
    nodeKind: "students",
    tieKind: "friendship ties",
    cohortPrefix: "G",
    hubThreshold: 4,
    palette: {
      Robotics: "--green",
      Drama: "--rose",
      Studio: "--blue"
    },
    nodes: [
      { id: "ada", name: "Ada", group: "Robotics", cohort: "11", score: 9, x: 132, y: 84 },
      { id: "ben", name: "Ben", group: "Robotics", cohort: "11", score: 8, x: 244, y: 74 },
      { id: "cal", name: "Cal", group: "Robotics", cohort: "10", score: 7, x: 188, y: 178 },
      { id: "dia", name: "Dia", group: "Robotics", cohort: "10", score: 5, x: 312, y: 178 },
      { id: "eli", name: "Eli", group: "Drama", cohort: "11", score: 6, x: 462, y: 92 },
      { id: "fay", name: "Fay", group: "Drama", cohort: "11", score: 7, x: 590, y: 102 },
      { id: "gia", name: "Gia", group: "Drama", cohort: "10", score: 6, x: 520, y: 206 },
      { id: "hal", name: "Hal", group: "Drama", cohort: "10", score: 5, x: 632, y: 244 },
      { id: "ivy", name: "Ivy", group: "Studio", cohort: "12", score: 9, x: 172, y: 332 },
      { id: "jay", name: "Jay", group: "Studio", cohort: "12", score: 8, x: 304, y: 332 },
      { id: "kim", name: "Kim", group: "Studio", cohort: "11", score: 4, x: 438, y: 342 },
      { id: "leo", name: "Leo", group: "Robotics", cohort: "12", score: 6, x: 356, y: 258 }
    ],
    edges: makeEdges([
      ["ada", "ben"], ["ada", "cal"], ["ben", "cal"], ["ben", "dia"], ["cal", "dia"],
      ["eli", "fay"], ["eli", "gia"], ["fay", "gia"], ["gia", "hal"],
      ["ivy", "jay"], ["ivy", "kim"], ["jay", "kim"], ["jay", "leo"],
      ["cal", "leo"], ["dia", "leo"], ["leo", "kim"], ["dia", "eli"], ["kim", "gia"]
    ]),
    closureEdges: [
      ["ada", "ben"], ["ada", "cal"], ["ben", "cal"],
      ["eli", "fay"], ["eli", "gia"], ["fay", "gia"],
      ["ivy", "jay"], ["ivy", "kim"], ["jay", "kim"]
    ],
    bridgeEdges: [
      ["dia", "eli"], ["kim", "gia"], ["jay", "leo"],
      ["cal", "leo"], ["dia", "leo"], ["kim", "leo"]
    ],
    libraryTerms: [
      "edges",
      "gwesp(0.5, fixed=TRUE)",
      "gwdsp(0.5, fixed=TRUE)",
      "gwdegree(0.5, fixed=TRUE)",
      'nodematch("club")',
      'nodematch("grade")',
      'nodefactor("grade")',
      'absdiff("activity")'
    ],
    run: "school",
    text: {
      fitCopy: (run, selected) =>
        `All four candidates fit with SA and passed the density check. ${selected.label} reproduces the observed degree, shared-partner, and distance distributions best: its largest standardized mismatch is the count of degree-4 students (${selected.residuals[0][2]} observed vs ${selected.residuals[0][3]} simulated). Pseudo-BIC would have preferred Candidate 3, but selection uses simulation-based GOF.`,
      fitTheory: (run, selected) =>
        `The evidence favors the compact specification: shared club, shared friends, and the degree spread explain the friendship network better than the edge-only baseline (q(M) ${selected.q.toFixed(2)} vs ${run.candidates[3].q.toFixed(2)}).`,
      rationales: [
        "Same-grade friendships may account for the extra degree-4 students; grade homophily is in L* and not yet in the model.",
        "Students with similar activity levels may be more connected; absdiff(activity) is available and untested.",
        "The degree-4 bin is still under-simulated; a larger gwdegree decay weights higher degrees more directly.",
        "Removing gwesp tests whether closure is redundant once club homophily and the degree term are present."
      ],
      reviseCopy: (run, selected, accepted, final) =>
        `The LLM proposed grade homophily first, then an activity-similarity term, a different gwdegree decay, and dropping gwesp. Every refit stayed eligible, but none lowered q(M) below ${selected.q.toFixed(2)}, so each edit was rejected and the Stage 2 model is retained as the final specification.`,
      reviseTheory: (run, selected, accepted, final) =>
        `Grade cohort does not improve the model once club homophily and closure are included: adding nodematch(grade) raised q(M) from ${selected.q.toFixed(2)} to ${run.rounds[0].q_after.toFixed(2)}. The final model keeps four terms.`,
      headline: "Students befriend classmates in the same club, and friends of friends",
      termReadings: [
        { term: "edges", mechanism: "baseline tie rate", direction: "negative", reading: "Friendship ties are sparse overall; most pairs are not friends." },
        { term: "nodematch(club)", mechanism: "club homophily", direction: "positive", reading: "Two students in the same activity club are much more likely to be friends, holding the other terms fixed." },
        { term: "gwesp(0.5)", mechanism: "triadic closure", direction: "positive", reading: "Students who already share a friend are somewhat more likely to be friends themselves." },
        { term: "gwdegree(0.5)", mechanism: "degree spread", direction: "positive", reading: "Friendships are spread fairly evenly across students rather than concentrated on a few highly connected ones." }
      ],
      summary: "Students are much more likely to be friends when they belong to the same activity club, and somewhat more likely when they already have a friend in common. Friendships are spread fairly evenly across students rather than concentrated on a few. Grade cohort and activity level were tested in Stage 3 and did not improve the model.",
      limitations: [
        "These are conditional associations in a fitted ERGM, not causal effects.",
        "With 12 students and 18 ties, coefficients are imprecise; q(M) is computed from 100 simulated networks.",
        "Grade homophily was proposed and rejected because q(M) rose; this does not show that grade is irrelevant."
      ]
    },
    stages: [
      {
        id: "intake",
        number: "0",
        rail: "Intake",
        subtitle: "Network + description",
        kicker: "Stage 0",
        title: "Network Intake",
        status: "Stage 0: diagnostics",
        lens: "raw network",
        mechanismTitle: "Observed ties cluster around shared activities",
        mechanismCopy:
          "The demo starts from a 12-student friendship network with club, grade, and activity attributes. Node colors are activity clubs; ties are observed friendships. The graph already hints at homophily and closure.",
        metrics: [
          ["–", "students"],
          ["–", "friendship ties"],
          ["–", "density"],
          ["–", "transitivity"]
        ],
        terms: ["edges"],
        guardrails: guardrailSets.intake,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "nothing fitted yet",
        bic: [],
        prompt: `dataset: school_friendship
actors: students in one school year
tie: undirected mutual friendship
node attributes: club (categorical), grade (categorical), activity (numeric)

description:
"The network consists of students from the same school year. A tie
indicates a mutual friendship. Friendships tend to form within the
same activity clubs and grade cohorts."

task:
validate the input and summarize network diagnostics.`,
        output: `{
  "visible_patterns": [
    "same-club ties",
    "local closure",
    "one bridging student"
  ]
}`,
        outputBadge: "diagnostics",
        highlight: "raw",
        theory:
          "At intake, FORGE has not produced an interpretation yet. It only records that friendships are not random: students appear to cluster by club, close triangles with mutual friends, and rely on a few bridge students."
      },
      {
        id: "library",
        number: "1a",
        rail: "Valid terms",
        subtitle: "Build L*",
        kicker: "Stage 1a",
        title: "Build the Valid Term List L*",
        status: "Stage 1a: valid terms",
        lens: "candidate mechanisms",
        mechanismTitle: "Only terms compatible with this network enter L*",
        mechanismCopy:
          "FORGE lists the ERGM terms that are valid for an undirected network with these attributes. Reciprocity terms are excluded, the raw triangle term is excluded, and attribute terms appear only for attributes that are present.",
        metrics: [
          ["8", "valid terms"],
          ["3", "structural terms"],
          ["4", "attribute terms"],
          ["0", "off-menu terms"]
        ],
        terms: [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "gwdegree(0.5, fixed=TRUE)",
          'nodematch("club")',
          'nodematch("grade")',
          'nodefactor("grade")',
          'absdiff("activity")'
        ],
        guardrails: guardrailSets.library,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "nothing fitted yet",
        bic: [],
        prompt: `input:
network type: undirected, 12 nodes, 18 ties
attributes:
  club: categorical, 3 levels
  grade: categorical, 3 levels
  activity: numeric, range 4-9

rules:
  include edges; exclude mutual (undirected);
  exclude raw triangle (degeneracy); one decay per gw family;
  attribute terms only for present attributes.

task:
construct the valid ERGM term list L*.`,
        output: `{
  "L_star": [
    "edges",
    "gwesp(0.5, fixed=TRUE)",
    "gwdsp(0.5, fixed=TRUE)",
    "gwdegree(0.5, fixed=TRUE)",
    "nodematch(\\"club\\")",
    "nodematch(\\"grade\\")",
    "nodefactor(\\"grade\\")",
    "absdiff(\\"activity\\")"
  ],
  "excluded": ["mutual (undirected)", "triangle (degeneracy risk)"]
}`,
        outputBadge: "term list",
        highlight: "homophily",
        theory:
          "The valid term space contains four mechanism families: baseline tie propensity, shared-friend closure, degree spread, and attribute-based similarity."
      },
      {
        id: "spec",
        number: "1b",
        rail: "Propose",
        subtitle: "LLM formulas",
        kicker: "Stage 1b",
        title: "LLM Proposes Formulas from L*",
        status: "Stage 1b: LLM proposals",
        lens: "LLM-selected terms",
        mechanismTitle: "Mechanisms first, then exact terms, then ranked formulas",
        mechanismCopy:
          "The LLM sees the description, diagnostics, attributes, and L*. It names plausible formation mechanisms, ties each to evidence, maps it to an admissible term, and returns up to three ranked specifications as JSON. Terms outside L* are rejected.",
        metrics: [
          ["3", "candidate specs"],
          ["100%", "terms inside L*"],
          ["4", "terms in Candidate 1"],
          ["3", "mechanisms named"]
        ],
        terms: ["edges", "gwesp(0.5, fixed=TRUE)", 'nodematch("club")', "gwdegree(0.5, fixed=TRUE)"],
        guardrails: guardrailSets.spec,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "3 candidates await fitting",
        bic: [],
        prompt: `system:
You are an ERGM expert. Return JSON only.

user:
Network: 12 students, 18 undirected friendship ties,
density 0.27, transitivity 0.43. Attributes: club, grade, activity.
Description: friendships tend to form within activity clubs and
grade cohorts.

Valid terms L*: edges, gwesp(0.5), gwdsp(0.5), gwdegree(0.5),
nodematch("club"), nodematch("grade"), nodefactor("grade"),
absdiff("activity").

1. List plausible tie-formation mechanisms with the evidence for each.
2. Map each mechanism to exactly one term in L*.
3. Return up to 3 ranked specifications. Use only L*. Include edges.`,
        output: `{
  "mechanisms": [
    {"mechanism": "club homophily", "evidence": "most ties are within clubs", "term": "nodematch(\\"club\\")"},
    {"mechanism": "triadic closure", "evidence": "transitivity 0.43", "term": "gwesp(0.5, fixed=TRUE)"},
    {"mechanism": "degree heterogeneity", "evidence": "degrees range 2-5", "term": "gwdegree(0.5, fixed=TRUE)"}
  ],
  "specifications": [
    {"label": "Candidate 1", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "nodematch(\\"club\\")", "gwdegree(0.5, fixed=TRUE)"]},
    {"label": "Candidate 2", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "nodematch(\\"grade\\")", "absdiff(\\"activity\\")"]},
    {"label": "Candidate 3", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "gwdsp(0.5, fixed=TRUE)", "nodematch(\\"club\\")", "gwdegree(0.5, fixed=TRUE)"]}
  ]
}`,
        outputBadge: "llm json",
        highlight: "closure",
        theory:
          "The LLM's first proposal is that friendship is mostly explained by shared club, shared friends, and the spread of friendships across students. Stage 2 decides which proposal the data supports."
      }
    ]
  },
  {
    id: "lab",
    shortLabel: "Lab",
    title: "Research Collaboration Network",
    nodeKind: "researchers",
    tieKind: "collaboration ties",
    cohortPrefix: "",
    hubThreshold: 4,
    palette: {
      NLP: "--green",
      Vision: "--rose",
      Systems: "--blue"
    },
    nodes: [
      { id: "noor", name: "Noor", group: "NLP", cohort: "PI", score: 9, x: 132, y: 84 },
      { id: "omar", name: "Omar", group: "NLP", cohort: "Postdoc", score: 7, x: 244, y: 74 },
      { id: "pia", name: "Pia", group: "NLP", cohort: "Student", score: 4, x: 188, y: 178 },
      { id: "qin", name: "Qin", group: "NLP", cohort: "Student", score: 3, x: 312, y: 178 },
      { id: "rui", name: "Rui", group: "Vision", cohort: "PI", score: 9, x: 462, y: 92 },
      { id: "sol", name: "Sol", group: "Vision", cohort: "Postdoc", score: 6, x: 590, y: 102 },
      { id: "tao", name: "Tao", group: "Vision", cohort: "Student", score: 3, x: 520, y: 206 },
      { id: "uma", name: "Uma", group: "Vision", cohort: "Student", score: 2, x: 632, y: 244 },
      { id: "val", name: "Val", group: "Systems", cohort: "PI", score: 8, x: 172, y: 332 },
      { id: "wes", name: "Wes", group: "Systems", cohort: "Postdoc", score: 6, x: 304, y: 332 },
      { id: "xia", name: "Xia", group: "Systems", cohort: "Student", score: 3, x: 438, y: 342 },
      { id: "yan", name: "Yan", group: "Systems", cohort: "Student", score: 4, x: 356, y: 258 }
    ],
    edges: makeEdges([
      ["noor", "omar"], ["noor", "pia"], ["omar", "pia"], ["omar", "qin"], ["pia", "qin"],
      ["rui", "sol"], ["rui", "tao"], ["sol", "uma"], ["tao", "uma"], ["sol", "tao"],
      ["val", "wes"], ["val", "yan"], ["wes", "xia"], ["xia", "yan"], ["wes", "yan"],
      ["pia", "rui"], ["qin", "tao"], ["yan", "qin"], ["xia", "tao"], ["noor", "val"]
    ]),
    closureEdges: [
      ["noor", "omar"], ["noor", "pia"], ["omar", "pia"], ["omar", "qin"], ["pia", "qin"],
      ["rui", "sol"], ["rui", "tao"], ["sol", "tao"], ["sol", "uma"], ["tao", "uma"],
      ["val", "yan"], ["wes", "xia"], ["xia", "yan"], ["wes", "yan"]
    ],
    bridgeEdges: [
      ["pia", "rui"], ["qin", "tao"], ["yan", "qin"],
      ["xia", "tao"], ["noor", "val"]
    ],
    libraryTerms: [
      "edges",
      "gwesp(0.5, fixed=TRUE)",
      "gwdsp(0.5, fixed=TRUE)",
      "gwdegree(0.5, fixed=TRUE)",
      'nodematch("area")',
      'nodematch("role")',
      'nodefactor("role")',
      'absdiff("seniority")'
    ],
    run: "lab",
    text: {
      fitCopy: (run, selected) =>
        `All four candidates fit with SA and passed the density check. ${selected.label}, which adds gwdsp to the area-homophily model, has the smallest largest-mismatch: ties with exactly one shared collaborator (${selected.residuals[0][2]} observed vs ${selected.residuals[0][3]} simulated). Pseudo-BIC agrees here, but only q(M) decides.`,
      fitTheory: (run, selected) =>
        `The evidence favors ${selected.label}: research-area homophily, open two-paths, closure, and the degree spread reproduce the collaboration network better than the edge-only baseline (q(M) ${selected.q.toFixed(2)} vs ${run.candidates[3].q.toFixed(2)}).`,
      rationales: [
        "Same-role pairs (student–student, PI–PI) may be under-represented; nodematch(role) is in L* and untested.",
        "Collaboration may be more likely between researchers of similar seniority; absdiff(seniority) targets the one-shared-partner residual.",
        "A smaller gwesp decay weights the first shared partner more heavily, matching the residual bin.",
        "Removing gwdegree tests whether the degree term is redundant once area and seniority are in the model."
      ],
      reviseCopy: (run, selected, accepted, final) =>
        `Round 1 (add role homophily) refit cleanly but nudged q(M) up, so it was rejected. Round 2 added seniority difference and lowered q(M) from ${selected.q.toFixed(2)} to ${final.q.toFixed(2)}, so it was kept. Rounds 3 and 4, which changed the gwesp decay and dropped gwdegree, were rejected because q(M) rose again.`,
      reviseTheory: (run, selected, accepted, final) =>
        `One of four edits survived the checks: collaboration is less likely between researchers with very different seniority. The final model keeps ${final.terms.length} terms with q(M) = ${final.q.toFixed(2)}.`,
      headline: "Researchers collaborate within their area and with peers of similar seniority",
      termReadings: [
        { term: "edges", mechanism: "baseline tie rate", direction: "negative", reading: "Collaboration ties are sparse overall." },
        { term: "nodematch(area)", mechanism: "area homophily", direction: "positive", reading: "Two researchers in the same research area are far more likely to collaborate, holding the other terms fixed." },
        { term: "absdiff(seniority)", mechanism: "seniority similarity", direction: "negative", reading: "The larger the seniority gap between two researchers, the less likely they collaborate." },
        { term: "gwdsp(0.5)", mechanism: "open two-paths", direction: "positive", reading: "Pairs that share collaborators are common, consistent with area clusters." },
        { term: "gwesp(0.5)", mechanism: "triadic closure", direction: "negative", reading: "Given area homophily and shared collaborators, additional closure adds no further tie propensity." },
        { term: "gwdegree(0.5)", mechanism: "degree spread", direction: "positive", reading: "Collaboration counts are spread across researchers rather than concentrated on one hub." }
      ],
      summary: "Researchers are far more likely to collaborate within their own research area, and less likely the wider the seniority gap between them. Shared collaborators are common inside areas, but once area is accounted for, closure by itself does not add to tie likelihood. Collaboration counts are spread across the group rather than concentrated on a single hub.",
      limitations: [
        "These are conditional associations in a fitted ERGM, not causal effects.",
        "With 12 researchers and 20 ties, some coefficients (notably gwdegree) are large and imprecise.",
        "Role homophily was proposed and rejected because q(M) rose; this does not show that role is irrelevant."
      ]
    },
    stages: [
      {
        id: "intake",
        number: "0",
        rail: "Intake",
        subtitle: "Network + description",
        kicker: "Stage 0",
        title: "Network Intake",
        status: "Stage 0: diagnostics",
        lens: "raw network",
        mechanismTitle: "Observed collaborations cluster by research area",
        mechanismCopy:
          "This network tracks 12 researchers. Colors are research areas; ties are coauthorship or project collaboration. The graph shows area clusters, shared-collaborator closure, and a few cross-area connectors.",
        metrics: [
          ["–", "researchers"],
          ["–", "collaboration ties"],
          ["–", "density"],
          ["–", "transitivity"]
        ],
        terms: ["edges"],
        guardrails: guardrailSets.intake,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "nothing fitted yet",
        bic: [],
        prompt: `dataset: research_collab
actors: researchers in one department
tie: undirected active collaboration
node attributes: area (categorical), role (categorical), seniority (numeric)

description:
"Researchers in one department. A tie means an active co-authorship
collaboration. Collaboration follows research areas and lab roles."

task:
validate the input and summarize network diagnostics.`,
        output: `{
  "visible_patterns": [
    "same-area collaboration",
    "shared-collaborator closure",
    "cross-area bridge researchers"
  ]
}`,
        outputBadge: "diagnostics",
        highlight: "raw",
        theory:
          "At intake, FORGE has not produced an interpretation yet. It records that collaborations concentrate inside research areas, close around shared collaborators, and depend on a few researchers who bridge areas."
      },
      {
        id: "library",
        number: "1a",
        rail: "Valid terms",
        subtitle: "Build L*",
        kicker: "Stage 1a",
        title: "Build the Valid Term List L*",
        status: "Stage 1a: valid terms",
        lens: "candidate mechanisms",
        mechanismTitle: "Only terms compatible with this network enter L*",
        mechanismCopy:
          "FORGE restricts the LLM to terms valid for an undirected collaboration graph with area, role, and seniority attributes. Reciprocity and raw triangle terms are excluded.",
        metrics: [
          ["8", "valid terms"],
          ["3", "structural terms"],
          ["4", "attribute terms"],
          ["0", "off-menu terms"]
        ],
        terms: [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "gwdegree(0.5, fixed=TRUE)",
          'nodematch("area")',
          'nodematch("role")',
          'nodefactor("role")',
          'absdiff("seniority")'
        ],
        guardrails: guardrailSets.library,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "nothing fitted yet",
        bic: [],
        prompt: `input:
network type: undirected, 12 nodes, 20 ties
attributes:
  area: categorical, 3 levels
  role: categorical, 3 levels
  seniority: numeric, range 2-9

rules:
  include edges; exclude mutual (undirected);
  exclude raw triangle (degeneracy); one decay per gw family;
  attribute terms only for present attributes.

task:
construct the valid ERGM term list L*.`,
        output: `{
  "L_star": [
    "edges",
    "gwesp(0.5, fixed=TRUE)",
    "gwdsp(0.5, fixed=TRUE)",
    "gwdegree(0.5, fixed=TRUE)",
    "nodematch(\\"area\\")",
    "nodematch(\\"role\\")",
    "nodefactor(\\"role\\")",
    "absdiff(\\"seniority\\")"
  ],
  "excluded": ["mutual (undirected)", "triangle (degeneracy risk)"]
}`,
        outputBadge: "term list",
        highlight: "homophily",
        theory:
          "The valid term space contains baseline collaboration rate, shared-collaborator closure, degree spread, same-area effects, and role or seniority effects."
      },
      {
        id: "spec",
        number: "1b",
        rail: "Propose",
        subtitle: "LLM formulas",
        kicker: "Stage 1b",
        title: "LLM Proposes Formulas from L*",
        status: "Stage 1b: LLM proposals",
        lens: "LLM-selected terms",
        mechanismTitle: "Mechanisms first, then exact terms, then ranked formulas",
        mechanismCopy:
          "The LLM names plausible mechanisms for collaboration, maps each to a term in L*, and returns three ranked specifications. Its first proposal explains collaboration through area homophily, shared collaborators, and the degree spread.",
        metrics: [
          ["3", "candidate specs"],
          ["100%", "terms inside L*"],
          ["4", "terms in Candidate 1"],
          ["3", "mechanisms named"]
        ],
        terms: ["edges", "gwesp(0.5, fixed=TRUE)", 'nodematch("area")', "gwdegree(0.5, fixed=TRUE)"],
        guardrails: guardrailSets.spec,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "3 candidates await fitting",
        bic: [],
        prompt: `system:
You are an ERGM expert. Return JSON only.

user:
Network: 12 researchers, 20 undirected collaboration ties,
density 0.30, transitivity 0.47. Attributes: area, role, seniority.
Description: collaboration follows research areas and lab roles.

Valid terms L*: edges, gwesp(0.5), gwdsp(0.5), gwdegree(0.5),
nodematch("area"), nodematch("role"), nodefactor("role"),
absdiff("seniority").

1. List plausible tie-formation mechanisms with the evidence for each.
2. Map each mechanism to exactly one term in L*.
3. Return up to 3 ranked specifications. Use only L*. Include edges.`,
        output: `{
  "mechanisms": [
    {"mechanism": "area homophily", "evidence": "most ties are within areas", "term": "nodematch(\\"area\\")"},
    {"mechanism": "triadic closure", "evidence": "transitivity 0.47", "term": "gwesp(0.5, fixed=TRUE)"},
    {"mechanism": "degree heterogeneity", "evidence": "PIs have the most ties", "term": "gwdegree(0.5, fixed=TRUE)"}
  ],
  "specifications": [
    {"label": "Candidate 1", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "nodematch(\\"area\\")", "gwdegree(0.5, fixed=TRUE)"]},
    {"label": "Candidate 2", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "nodematch(\\"role\\")", "absdiff(\\"seniority\\")"]},
    {"label": "Candidate 3", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "gwdsp(0.5, fixed=TRUE)", "nodematch(\\"area\\")", "gwdegree(0.5, fixed=TRUE)"]}
  ]
}`,
        outputBadge: "llm json",
        highlight: "closure",
        theory:
          "The LLM's first proposal is that researchers collaborate mostly through shared collaborators, shared research area, and an uneven spread of ties. Stage 2 decides which proposal the data supports."
      }
    ]
  },
  {
    id: "neighborhood",
    shortLabel: "Neighborhood",
    title: "Neighborhood Mutual Aid Network",
    nodeKind: "households",
    tieKind: "mutual-aid ties",
    cohortPrefix: "",
    hubThreshold: 4,
    palette: {
      North: "--green",
      Market: "--rose",
      Riverside: "--blue"
    },
    nodes: [
      { id: "nora", name: "Nora", group: "North", cohort: "Long", score: 12, x: 132, y: 84 },
      { id: "theo", name: "Theo", group: "North", cohort: "Mid", score: 6, x: 244, y: 74 },
      { id: "mina", name: "Mina", group: "North", cohort: "New", score: 2, x: 188, y: 178 },
      { id: "otis", name: "Otis", group: "North", cohort: "Mid", score: 5, x: 312, y: 178 },
      { id: "park", name: "Park", group: "Market", cohort: "Long", score: 15, x: 462, y: 92 },
      { id: "raya", name: "Raya", group: "Market", cohort: "Mid", score: 7, x: 590, y: 102 },
      { id: "sam", name: "Sam", group: "Market", cohort: "New", score: 1, x: 520, y: 206 },
      { id: "tess", name: "Tess", group: "Market", cohort: "New", score: 2, x: 632, y: 244 },
      { id: "uma", name: "Uma", group: "Riverside", cohort: "Long", score: 11, x: 172, y: 332 },
      { id: "vic", name: "Vic", group: "Riverside", cohort: "Mid", score: 6, x: 304, y: 332 },
      { id: "wren", name: "Wren", group: "Riverside", cohort: "New", score: 1, x: 438, y: 342 },
      { id: "zed", name: "Zed", group: "Riverside", cohort: "Long", score: 14, x: 356, y: 258 }
    ],
    edges: makeEdges([
      ["nora", "theo"], ["nora", "mina"], ["theo", "mina"], ["theo", "otis"], ["mina", "otis"],
      ["park", "raya"], ["park", "sam"], ["raya", "tess"], ["sam", "tess"], ["park", "tess"],
      ["uma", "vic"], ["vic", "wren"], ["wren", "zed"], ["uma", "zed"], ["uma", "wren"],
      ["otis", "park"], ["mina", "zed"], ["sam", "wren"]
    ]),
    closureEdges: [
      ["nora", "theo"], ["nora", "mina"], ["theo", "mina"], ["theo", "otis"], ["mina", "otis"],
      ["park", "raya"], ["raya", "tess"], ["park", "tess"], ["park", "sam"], ["sam", "tess"],
      ["uma", "vic"], ["vic", "wren"], ["wren", "zed"], ["uma", "zed"], ["uma", "wren"]
    ],
    bridgeEdges: [
      ["otis", "park"], ["mina", "zed"], ["sam", "wren"]
    ],
    libraryTerms: [
      "edges",
      "gwesp(0.5, fixed=TRUE)",
      "gwdsp(0.5, fixed=TRUE)",
      "gwdegree(0.5, fixed=TRUE)",
      'nodematch("block")',
      'nodematch("tenure_group")',
      'nodefactor("tenure_group")',
      'absdiff("tenure_years")'
    ],
    run: "neighborhood",
    text: {
      fitCopy: (run, selected, eligible, ineligible) =>
        `Candidate 3 is ineligible: its SA fit did not return finite estimates on this 12-household network. Of the three eligible models, ${selected.label} has the smallest largest-mismatch: ties with exactly one shared neighbor (${selected.residuals[0][2]} observed vs ${selected.residuals[0][3]} simulated). Selection uses q(M) only; pseudo-BIC is shown for reference.`,
      fitTheory: (run, selected) =>
        `The evidence favors ${selected.label}: same-block proximity, shared neighbors, and the degree spread reproduce the mutual-aid network better than the edge-only baseline (q(M) ${selected.q.toFixed(2)} vs ${run.candidates[3].q.toFixed(2)}).`,
      rationales: [
        "Households in the same tenure group may exchange help more often; nodematch(tenure_group) is in L* and untested.",
        "Residence-length differences may matter beyond the group label; absdiff(tenure_years) targets the remaining one-shared-neighbor residual.",
        "A smaller gwesp decay weights the first shared neighbor more heavily, matching the residual bin.",
        "Removing gwdegree tests whether the degree term is redundant once block and tenure terms are present."
      ],
      reviseCopy: (run, selected, accepted, final) =>
        `Three edits in a row lowered q(M) and were kept: tenure-group homophily (${run.rounds[0].q_before.toFixed(2)} → ${run.rounds[0].q_after.toFixed(2)}), tenure-year difference (→ ${run.rounds[1].q_after.toFixed(2)}), and a smaller gwesp decay (→ ${run.rounds[2].q_after.toFixed(2)}). Round 4 proposed dropping gwdegree; the refit was eligible but q(M) rose to ${run.rounds[3].q_after.toFixed(2)}, so the edit was rejected.`,
      reviseTheory: (run, selected, accepted, final) =>
        `The revised model adds residence tenure in two forms: support is more likely within the same tenure group, and, across groups, more likely the larger the tenure gap. q(M) fell from ${selected.q.toFixed(2)} to ${final.q.toFixed(2)} over three accepted rounds.`,
      headline: "Neighbors help within their block, and long-established households help newcomers",
      termReadings: [
        { term: "edges", mechanism: "baseline tie rate", direction: "negative", reading: "Help ties are sparse overall." },
        { term: "nodematch(block)", mechanism: "block homophily", direction: "positive", reading: "Two households on the same block are much more likely to exchange help, holding the other terms fixed." },
        { term: "nodematch(tenure_group)", mechanism: "tenure-group homophily", direction: "positive", reading: "Households in the same tenure group (new, mid, long) are more likely to help each other." },
        { term: "absdiff(tenure_years)", mechanism: "tenure gap", direction: "positive", reading: "Across tenure groups, help is more likely the larger the gap in years of residence, e.g. long-established households helping newcomers." },
        { term: "gwesp(0.25)", mechanism: "triadic closure", direction: "near zero", reading: "Sharing a neighbor adds little once block and tenure are accounted for." },
        { term: "gwdegree(0.5)", mechanism: "degree spread", direction: "positive", reading: "Helping is spread across households rather than concentrated on a few hubs." }
      ],
      summary: "Households are much more likely to help each other when they live on the same block. Help also follows residence history in two ways: households in the same tenure group help each other more, and across groups the largest tenure gaps, such as long-established households helping newcomers, are the most likely ties. Sharing a neighbor adds little once block and tenure are accounted for, and helping is spread across households rather than concentrated on a few.",
      limitations: [
        "These are conditional associations in a fitted ERGM, not causal effects.",
        "With 12 households and 18 ties, coefficients are imprecise; q(M) is computed from 100 simulated networks.",
        "Candidate 3 could not be fitted on this network, so the gwdsp mechanism was never compared."
      ]
    },
    stages: [
      {
        id: "intake",
        number: "0",
        rail: "Intake",
        subtitle: "Network + description",
        kicker: "Stage 0",
        title: "Network Intake",
        status: "Stage 0: diagnostics",
        lens: "raw network",
        mechanismTitle: "Observed support ties cluster by block",
        mechanismCopy:
          "This network represents 12 households exchanging mutual aid. Colors are neighborhood blocks. Ties show observed support exchanges, with block clusters and a few households connecting blocks.",
        metrics: [
          ["–", "households"],
          ["–", "mutual-aid ties"],
          ["–", "density"],
          ["–", "transitivity"]
        ],
        terms: ["edges"],
        guardrails: guardrailSets.intake,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "nothing fitted yet",
        bic: [],
        prompt: `dataset: neighborhood_aid
actors: households in one neighborhood
tie: undirected mutual-aid exchange
node attributes: block (categorical), tenure_group (categorical), tenure_years (numeric)

description:
"Households in one neighborhood. A tie means the households exchange
practical help. Help flows within blocks and among long-tenured
residents."

task:
validate the input and summarize network diagnostics.`,
        output: `{
  "visible_patterns": [
    "same-block support",
    "local closure",
    "block-bridging households"
  ]
}`,
        outputBadge: "diagnostics",
        highlight: "raw",
        theory:
          "At intake, FORGE has not produced an interpretation yet. It records that support ties cluster by block, close around shared neighbors, and rely on a few households that bridge local areas."
      },
      {
        id: "library",
        number: "1a",
        rail: "Valid terms",
        subtitle: "Build L*",
        kicker: "Stage 1a",
        title: "Build the Valid Term List L*",
        status: "Stage 1a: valid terms",
        lens: "candidate mechanisms",
        mechanismTitle: "Only terms compatible with this network enter L*",
        mechanismCopy:
          "FORGE lists the terms valid for an undirected household support network with block and residence-tenure attributes. Reciprocity and raw triangle terms are excluded.",
        metrics: [
          ["8", "valid terms"],
          ["3", "structural terms"],
          ["4", "attribute terms"],
          ["0", "off-menu terms"]
        ],
        terms: [
          "edges",
          "gwesp(0.5, fixed=TRUE)",
          "gwdsp(0.5, fixed=TRUE)",
          "gwdegree(0.5, fixed=TRUE)",
          'nodematch("block")',
          'nodematch("tenure_group")',
          'nodefactor("tenure_group")',
          'absdiff("tenure_years")'
        ],
        guardrails: guardrailSets.library,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "nothing fitted yet",
        bic: [],
        prompt: `input:
network type: undirected, 12 nodes, 18 ties
attributes:
  block: categorical, 3 levels
  tenure_group: categorical, 3 levels
  tenure_years: numeric, range 1-15

rules:
  include edges; exclude mutual (undirected);
  exclude raw triangle (degeneracy); one decay per gw family;
  attribute terms only for present attributes.

task:
construct the valid ERGM term list L*.`,
        output: `{
  "L_star": [
    "edges",
    "gwesp(0.5, fixed=TRUE)",
    "gwdsp(0.5, fixed=TRUE)",
    "gwdegree(0.5, fixed=TRUE)",
    "nodematch(\\"block\\")",
    "nodematch(\\"tenure_group\\")",
    "nodefactor(\\"tenure_group\\")",
    "absdiff(\\"tenure_years\\")"
  ],
  "excluded": ["mutual (undirected)", "triangle (degeneracy risk)"]
}`,
        outputBadge: "term list",
        highlight: "homophily",
        theory:
          "The valid term space contains baseline support rate, shared-neighbor closure, degree spread, same-block clustering, and residence-tenure effects."
      },
      {
        id: "spec",
        number: "1b",
        rail: "Propose",
        subtitle: "LLM formulas",
        kicker: "Stage 1b",
        title: "LLM Proposes Formulas from L*",
        status: "Stage 1b: LLM proposals",
        lens: "LLM-selected terms",
        mechanismTitle: "Mechanisms first, then exact terms, then ranked formulas",
        mechanismCopy:
          "The LLM names plausible mechanisms for mutual aid, maps each to a term in L*, and returns three ranked specifications. Its first proposal explains support through same-block proximity, shared neighbors, and the degree spread.",
        metrics: [
          ["3", "candidate specs"],
          ["100%", "terms inside L*"],
          ["4", "terms in Candidate 1"],
          ["3", "mechanisms named"]
        ],
        terms: ["edges", "gwesp(0.5, fixed=TRUE)", 'nodematch("block")', "gwdegree(0.5, fixed=TRUE)"],
        guardrails: guardrailSets.spec,
        chartTitle: "GOF discrepancy q(M) · lower is better",
        chartLabel: "3 candidates await fitting",
        bic: [],
        prompt: `system:
You are an ERGM expert. Return JSON only.

user:
Network: 12 households, 18 undirected mutual-aid ties,
density 0.27, transitivity 0.53. Attributes: block, tenure_group,
tenure_years. Description: help flows within blocks and among
long-tenured residents.

Valid terms L*: edges, gwesp(0.5), gwdsp(0.5), gwdegree(0.5),
nodematch("block"), nodematch("tenure_group"),
nodefactor("tenure_group"), absdiff("tenure_years").

1. List plausible tie-formation mechanisms with the evidence for each.
2. Map each mechanism to exactly one term in L*.
3. Return up to 3 ranked specifications. Use only L*. Include edges.`,
        output: `{
  "mechanisms": [
    {"mechanism": "block homophily", "evidence": "most ties are within blocks", "term": "nodematch(\\"block\\")"},
    {"mechanism": "triadic closure", "evidence": "transitivity 0.53", "term": "gwesp(0.5, fixed=TRUE)"},
    {"mechanism": "degree heterogeneity", "evidence": "long-tenured households have the most ties", "term": "gwdegree(0.5, fixed=TRUE)"}
  ],
  "specifications": [
    {"label": "Candidate 1", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "nodematch(\\"block\\")", "gwdegree(0.5, fixed=TRUE)"]},
    {"label": "Candidate 2", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "nodematch(\\"tenure_group\\")", "absdiff(\\"tenure_years\\")"]},
    {"label": "Candidate 3", "formula": ["edges", "gwesp(0.5, fixed=TRUE)", "gwdsp(0.5, fixed=TRUE)", "nodematch(\\"block\\")", "gwdegree(0.5, fixed=TRUE)"]}
  ]
}`,
        outputBadge: "llm json",
        highlight: "closure",
        theory:
          "The LLM's first proposal is that mutual aid is explained by same-block proximity, shared neighbors, and an uneven spread of helping. Stage 2 decides which proposal the data supports."
      }
    ]
  }
];

networkDemos.forEach((demo) => {
  demo.nodeById = Object.fromEntries(demo.nodes.map((node) => [node.id, node]));
  const computed = graphDiagnostics(demo);
  demo.adjacency = computed.adjacency;
  demo.degreeById = computed.degreeById;
  demo.diagnostics = computed.diagnostics;
  demo.closureSet = makeKeySet(demo.closureEdges);
  demo.bridgeSet = makeKeySet(demo.bridgeEdges);
  hydrateIntakeStage(demo);
  if (demo.run && runRecords[demo.run]) {
    const run = runRecords[demo.run];
    demo.stages.push(
      buildFitStage(demo, run, demo.text),
      buildReviseStage(demo, run, demo.text),
      buildInterpretStage(demo, run, demo.text)
    );
  }
});

let activeNetwork = 0;
let activeStage = 0;
let replayTimer = null;

const svg = document.getElementById("network-svg");
const networkPicker = document.getElementById("network-picker");
const networkTitle = document.getElementById("network-title");
const networkLegend = document.getElementById("network-legend");
const stageList = document.getElementById("stage-list");
const statusText = document.getElementById("stage-status");
const kicker = document.getElementById("current-stage-kicker");
const title = document.getElementById("current-stage-title");
const lensLabel = document.getElementById("lens-label");
const mechanismTitle = document.getElementById("mechanism-title");
const mechanismCopy = document.getElementById("mechanism-copy");
const metricGrid = document.getElementById("metric-grid");
const promptView = document.getElementById("prompt-view");
const replayNote = document.getElementById("replay-note");
const outputView = document.getElementById("output-view");
const outputBadge = document.getElementById("output-badge");
const summaryLabel = document.getElementById("summary-label");
const theoryHeadline = document.getElementById("theory-headline");
const theoryCopy = document.getElementById("theory-copy");
const termList = document.getElementById("term-list");
const termCount = document.getElementById("term-count");
const bicChart = document.getElementById("bic-chart");
const chartTitle = document.getElementById("chart-title");
const bestModelLabel = document.getElementById("best-model-label");
const roundsSection = document.getElementById("rounds-section");
const roundsList = document.getElementById("rounds-list");
const roundsScore = document.getElementById("rounds-score");
const checksTitle = document.getElementById("checks-title");
const guardrailList = document.getElementById("guardrail-list");
const guardrailScore = document.getElementById("guardrail-score");

function currentDemo() {
  return networkDemos[activeNetwork];
}

function currentStages() {
  return currentDemo().stages;
}

function svgEl(name, attrs = {}) {
  const element = document.createElementNS("http://www.w3.org/2000/svg", name);
  Object.entries(attrs).forEach(([key, value]) => element.setAttribute(key, value));
  return element;
}

function colorForGroup(demo, group) {
  const colorVariable = demo.palette[group] || "--green";
  return getComputedStyle(document.documentElement).getPropertyValue(colorVariable).trim() || "#2563eb";
}

function edgeKey(edge) {
  return makeKey(edge.source, edge.target);
}

function isSameGroup(edge, demo) {
  return demo.nodeById[edge.source].group === demo.nodeById[edge.target].group;
}

function isSameCohort(edge, demo) {
  const a = demo.nodeById[edge.source].cohort;
  const b = demo.nodeById[edge.target].cohort;
  return Boolean(a) && a === b;
}

function isClosureEdge(edge, demo) {
  return demo.closureSet.has(edgeKey(edge));
}

function isBridgeEdge(edge, demo) {
  return demo.bridgeSet.has(edgeKey(edge));
}

function shouldHighlightEdge(edge, lens, demo) {
  if (lens === "raw") return false;
  if (lens === "homophily") return isSameGroup(edge, demo);
  if (lens === "closure") return isClosureEdge(edge, demo);
  if (lens === "winner") return isSameGroup(edge, demo) || isClosureEdge(edge, demo);
  if (lens === "refined") return isSameGroup(edge, demo) || isSameCohort(edge, demo) || isBridgeEdge(edge, demo);
  if (lens === "final") return isSameGroup(edge, demo) || isSameCohort(edge, demo) || isClosureEdge(edge, demo);
  return false;
}

function touchesHighlightedEdge(node, lens, demo) {
  return demo.edges.some((edge) => {
    const touchesNode = edge.source === node.id || edge.target === node.id;
    return touchesNode && shouldHighlightEdge(edge, lens, demo);
  });
}

function shouldHighlightNode(node, lens, demo) {
  if (lens === "raw") return false;
  if (lens === "winner") return demo.degreeById[node.id] >= demo.hubThreshold;
  if (lens === "final") return demo.degreeById[node.id] >= demo.hubThreshold || touchesHighlightedEdge(node, lens, demo);
  return touchesHighlightedEdge(node, lens, demo);
}

function renderNetworkPicker() {
  networkPicker.replaceChildren();
  networkDemos.forEach((demo, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.className = `network-button${index === activeNetwork ? " active" : ""}`;
    button.textContent = demo.shortLabel;
    button.setAttribute("aria-pressed", index === activeNetwork ? "true" : "false");
    button.addEventListener("click", () => setNetwork(index));
    networkPicker.appendChild(button);
  });
}

function renderLegend(demo) {
  networkLegend.replaceChildren();
  Object.keys(demo.palette).forEach((group) => {
    const item = document.createElement("span");
    const swatch = document.createElement("i");
    swatch.className = "swatch";
    swatch.style.background = colorForGroup(demo, group);
    item.appendChild(swatch);
    item.appendChild(document.createTextNode(group));
    networkLegend.appendChild(item);
  });
}

function renderNetwork(stage) {
  const demo = currentDemo();
  svg.replaceChildren();

  const edgeLayer = svgEl("g", { class: "edge-layer" });
  const nodeLayer = svgEl("g", { class: "node-layer" });

  demo.edges.forEach((edge) => {
    const source = demo.nodeById[edge.source];
    const target = demo.nodeById[edge.target];
    edgeLayer.appendChild(svgEl("line", {
      x1: source.x,
      y1: source.y,
      x2: target.x,
      y2: target.y,
      class: "edge"
    }));
  });

  demo.nodes.forEach((node) => {
    const group = svgEl("g", { transform: `translate(${node.x}, ${node.y})` });
    const color = colorForGroup(demo, node.group);
    group.appendChild(svgEl("circle", {
      r: 23 + Math.min(demo.degreeById[node.id], 5),
      class: "node-ring",
      fill: color
    }));
    group.appendChild(svgEl("circle", {
      r: 16,
      fill: color
    }));
    const label = svgEl("text", { class: "node-label", x: 0, y: 43, "text-anchor": "middle" });
    label.textContent = node.name;
    const sub = svgEl("text", { class: "node-sub", x: 0, y: 57, "text-anchor": "middle" });
    sub.textContent = `${demo.cohortPrefix}${node.cohort} / d${demo.degreeById[node.id]}`;
    group.appendChild(label);
    group.appendChild(sub);
    nodeLayer.appendChild(group);
  });

  svg.appendChild(edgeLayer);
  svg.appendChild(nodeLayer);
}

function renderStageList() {
  stageList.replaceChildren();
  currentStages().forEach((stage, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.className = `stage-button${index === activeStage ? " active" : ""}`;

    const indexEl = document.createElement("span");
    indexEl.className = "stage-index";
    indexEl.textContent = stage.number;

    const textWrap = document.createElement("span");
    const name = document.createElement("span");
    name.className = "stage-name";
    name.textContent = stage.rail;
    const subtitle = document.createElement("span");
    subtitle.className = "stage-subtitle";
    subtitle.textContent = stage.subtitle;
    textWrap.appendChild(name);
    textWrap.appendChild(subtitle);

    button.appendChild(indexEl);
    button.appendChild(textWrap);
    button.addEventListener("click", () => setStage(index));
    stageList.appendChild(button);
  });
}

function renderMetrics(metrics) {
  metricGrid.replaceChildren();
  metrics.forEach(([value, label]) => {
    const item = document.createElement("div");
    item.className = "metric";
    const valueEl = document.createElement("span");
    valueEl.className = "metric-value";
    valueEl.textContent = value;
    const labelEl = document.createElement("span");
    labelEl.className = "metric-label";
    labelEl.textContent = label;
    item.appendChild(valueEl);
    item.appendChild(labelEl);
    metricGrid.appendChild(item);
  });
}

function renderTerms(terms) {
  termList.replaceChildren();
  termCount.textContent = `${terms.length}`;
  terms.forEach((term) => {
    const item = document.createElement("div");
    item.className = "term-chip";

    const dot = document.createElement("span");
    dot.className = "term-dot";

    const textWrap = document.createElement("span");
    const name = document.createElement("span");
    name.className = "term-name";
    name.textContent = shortTerm(term);
    const meaning = document.createElement("span");
    meaning.className = "term-meaning";
    meaning.textContent = termGloss(term);

    textWrap.appendChild(name);
    textWrap.appendChild(meaning);
    item.appendChild(dot);
    item.appendChild(textWrap);
    termList.appendChild(item);
  });
}

function renderGuardrails(items) {
  guardrailList.replaceChildren();
  const passCount = items.filter(([status]) => status === "pass").length;
  guardrailScore.textContent = `${passCount}/${items.length}`;
  items.forEach(([status, copy]) => {
    const item = document.createElement("div");
    item.className = "guardrail-item";
    const dot = document.createElement("span");
    dot.className = "guardrail-dot";
    dot.dataset.status = status;
    dot.style.background = status === "pass" ? "var(--green)" : status === "fail" ? "#b91c1c" : "var(--amber)";
    const copyEl = document.createElement("span");
    copyEl.className = "guardrail-copy";
    copyEl.textContent = copy;
    item.appendChild(dot);
    item.appendChild(copyEl);
    guardrailList.appendChild(item);
  });
}

function renderChart(rows, label) {
  bicChart.replaceChildren();
  if (!rows || rows.length === 0) {
    const empty = document.createElement("p");
    empty.className = "chart-empty";
    empty.textContent = label || "No candidate has been fitted yet.";
    bicChart.appendChild(empty);
    return;
  }
  const values = rows.map((row) => row[1]).filter((value) => typeof value === "number" && Number.isFinite(value));
  const min = values.length ? Math.min(...values) : 0;
  const max = values.length ? Math.max(...values) : 1;
  rows.forEach(([rowLabel, value, status]) => {
    const row = document.createElement("div");
    row.className = `bar-row${status ? ` ${status}` : ""}`;

    const labelEl = document.createElement("span");
    labelEl.className = "bar-label";
    labelEl.textContent = rowLabel;

    const track = document.createElement("span");
    track.className = "bar-track";
    const fill = document.createElement("span");
    fill.className = "bar-fill";
    const numeric = typeof value === "number" && Number.isFinite(value);
    const width = !numeric ? 100 : max === min ? 80 : 20 + ((max - value) / (max - min)) * 78;
    fill.style.width = `${width}%`;
    track.appendChild(fill);

    const valueEl = document.createElement("span");
    valueEl.className = "bar-value";
    valueEl.textContent = numeric ? value.toFixed(2) : "n/a";

    row.appendChild(labelEl);
    row.appendChild(track);
    row.appendChild(valueEl);
    bicChart.appendChild(row);
  });
}

function renderRounds(rounds) {
  if (!rounds || rounds.length === 0) {
    roundsSection.hidden = true;
    roundsList.replaceChildren();
    return;
  }
  roundsSection.hidden = false;
  roundsList.replaceChildren();
  const accepted = rounds.filter((round) => round.accepted).length;
  roundsScore.textContent = `${accepted}/${rounds.length} accepted`;
  rounds.forEach((round) => {
    const item = document.createElement("div");
    item.className = `round-item ${round.accepted ? "accepted" : "rejected"}`;

    const badge = document.createElement("span");
    badge.className = "round-badge";
    badge.textContent = round.accepted ? "✓" : "✗";
    badge.title = round.accepted ? "accepted" : "rejected";

    const textWrap = document.createElement("span");
    const edit = document.createElement("span");
    edit.className = "round-edit";
    edit.textContent = `R${round.round}: ${round.edit}`;
    const reason = document.createElement("span");
    reason.className = "round-reason";
    reason.textContent = round.reason;
    textWrap.appendChild(edit);
    textWrap.appendChild(reason);

    const q = document.createElement("span");
    q.className = "round-q";
    q.textContent = round.qAfter === null || round.qAfter === undefined ? "n/a" : round.qAfter.toFixed(2);
    const small = document.createElement("small");
    small.textContent = `from ${round.qBefore.toFixed(2)}`;
    q.appendChild(small);

    item.appendChild(badge);
    item.appendChild(textWrap);
    item.appendChild(q);
    roundsList.appendChild(item);
  });
}

function setNetwork(index) {
  activeNetwork = Math.max(0, Math.min(index, networkDemos.length - 1));
  activeStage = 0;
  stopReplay();
  setStage(0);
}

function setStage(index) {
  const demo = currentDemo();
  const stages = currentStages();
  activeStage = Math.max(0, Math.min(index, stages.length - 1));
  const stage = stages[activeStage];

  networkTitle.textContent = demo.title;
  statusText.textContent = `${demo.shortLabel}: ${stage.status}`;
  kicker.textContent = stage.kicker;
  title.textContent = stage.title;
  lensLabel.textContent = stage.lens;
  mechanismTitle.textContent = stage.mechanismTitle;
  mechanismCopy.textContent = stage.mechanismCopy;
  promptView.textContent = stage.prompt;
  outputView.textContent = stage.output;
  outputBadge.textContent = stage.outputBadge;
  if (summaryLabel) {
    summaryLabel.textContent = stage.id === "interpret" ? "Final Interpretation" : "In progress";
  }
  theoryHeadline.textContent = stage.theoryHeadline || stage.mechanismTitle;
  theoryCopy.textContent = stage.theory;
  bestModelLabel.textContent = stage.chartLabel;
  if (chartTitle) chartTitle.textContent = stage.chartTitle || "GOF discrepancy q(M) · lower is better";
  if (replayNote) replayNote.hidden = demo.id === "live";
  if (checksTitle) {
    checksTitle.textContent = stage.id === "fit" || stage.id === "refine" ? "Eligibility & acceptance checks" : "Checks";
  }

  renderNetworkPicker();
  renderLegend(demo);
  renderStageList();
  renderNetwork(stage);
  renderMetrics(stage.metrics);
  renderTerms(stage.terms);
  renderGuardrails(stage.guardrails);
  renderChart(stage.bic, stage.chartLabel);
  renderRounds(stage.rounds);

  document.getElementById("prev-step").disabled = activeStage === 0;
  document.getElementById("next-step").disabled = activeStage === stages.length - 1;
}

function nextStage() {
  setStage(activeStage + 1);
}

function previousStage() {
  setStage(activeStage - 1);
}

function stopReplay() {
  if (replayTimer) {
    clearInterval(replayTimer);
    replayTimer = null;
  }
}

function replay() {
  stopReplay();
  setStage(0);
  replayTimer = setInterval(() => {
    if (activeStage >= currentStages().length - 1) {
      stopReplay();
      return;
    }
    nextStage();
  }, 1900);
}

function setCopyButtonLabel(label) {
  const button = document.getElementById("copy-prompt");
  button.textContent = label;
  window.setTimeout(() => {
    button.textContent = "Copy";
  }, 2200);
}

function fallbackCopy(text) {
  const textArea = document.createElement("textarea");
  textArea.value = text;
  textArea.setAttribute("readonly", "");
  textArea.style.position = "fixed";
  textArea.style.left = "-9999px";
  document.body.appendChild(textArea);
  textArea.select();

  let copied = false;
  try {
    copied = document.execCommand("copy");
  } catch (error) {
    copied = false;
  }
  document.body.removeChild(textArea);
  setCopyButtonLabel(copied ? "Copied" : "Copy unavailable");
}

function copyPrompt() {
  const text = promptView.textContent;
  if (navigator.clipboard && navigator.clipboard.writeText) {
    const writeAttempt = navigator.clipboard.writeText(text);
    const timeout = new Promise((resolve, reject) => {
      window.setTimeout(() => reject(new Error("Clipboard timeout")), 500);
    });
    Promise.race([writeAttempt, timeout])
      .then(() => setCopyButtonLabel("Copied"))
      .catch(() => fallbackCopy(text));
    return;
  }
  fallbackCopy(text);
}

document.getElementById("prev-step").addEventListener("click", previousStage);
document.getElementById("next-step").addEventListener("click", nextStage);
document.getElementById("play-step").addEventListener("click", replay);
document.getElementById("copy-prompt").addEventListener("click", copyPrompt);

document.addEventListener("keydown", (event) => {
  const target = event.target;
  if (target && (target.tagName === "INPUT" || target.tagName === "TEXTAREA" ||
      target.tagName === "SELECT" || target.isContentEditable)) {
    return;
  }
  if (event.key === "ArrowRight") nextStage();
  if (event.key === "ArrowLeft") previousStage();
});

setStage(0);
