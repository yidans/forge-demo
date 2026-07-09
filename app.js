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
    density: roundMetric(demo.diagnostics.density, 3),
    triangles: demo.diagnostics.triangles,
    transitivity: roundMetric(demo.diagnostics.transitivity, 3),
    degree_max: demo.diagnostics.degreeMax,
    visible_patterns: visiblePatterns
  }, null, 2);
}

const termMeanings = {
  edges: "baseline tie rate",
  'gwesp(0.5, fixed=TRUE)': "triadic closure among connected actors",
  'gwdsp(0.5, fixed=TRUE)': "open two-path pressure",
  'gwdegree(0.5, fixed=TRUE)': "hub and degree heterogeneity",
  'nodematch("club")': "students in the same activity group connect more",
  'nodematch("grade")': "same-grade friendship tendency",
  'nodefactor("grade")': "grade-level activity differences",
  'absdiff("activity")': "similar activity level",
  'nodematch("area")': "same research area collaboration",
  'nodematch("role")': "same role or status pattern",
  'nodefactor("role")': "role-specific collaboration propensity",
  'absdiff("seniority")': "similar seniority levels",
  'nodematch("block")': "same neighborhood block support",
  'nodematch("tenure_group")': "same tenure group support",
  'nodefactor("tenure_group")': "tenure-specific support propensity",
  'absdiff("tenure_years")': "similar length of residence"
};

const guardrailSets = {
  intake: [
    ["pass", "No missing node attributes in the network"],
    ["pass", "Undirected ties have no self-loops"],
    ["pass", "Small enough for live demo fitting"]
  ],
  library: [
    ["pass", "Every term is available in ergm syntax"],
    ["pass", "Categorical terms have enough observations per level"],
    ["pass", "Triangle is excluded; curved closure terms are preferred"]
  ],
  spec: [
    ["pass", "All selected terms are inside the admissible library"],
    ["pass", "Specification includes edges"],
    ["pass", "Term count stays within the demo guardrail"]
  ],
  fit: [
    ["pass", "MPLE succeeds for all candidate specs"],
    ["pass", "Best pseudo-BIC improves over null"],
    ["warn", "GOF still shows mild residual structure"]
  ],
  refine: [
    ["pass", "Single edit is admissible"],
    ["pass", "Refined BIC improves over the selected model"],
    ["pass", "GOF max |z| drops below 2.0"]
  ],
  interpret: [
    ["pass", "Mechanism claims are tied to fitted terms"],
    ["pass", "The interpretation separates evidence from caveats"],
    ["pass", "No causal language is used"]
  ]
};

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
    stages: [
      {
        id: "intake",
        number: "0",
        rail: "Network Intake",
        subtitle: "Graph and diagnostics",
        kicker: "Stage 0",
        title: "Network Intake",
        status: "Stage 0: diagnostics",
        lens: "raw network",
        mechanismTitle: "Observed ties cluster around shared activities",
        mechanismCopy:
          "The demo starts from a 12-student friendship network. Node colors are activity groups; ties show observed friendships. The graph already hints at homophily, closure, and hub structure.",
        metrics: [
          ["–", "students"],
          ["–", "friendship ties"],
          ["–", "density"],
          ["–", "transitivity"]
        ],
        terms: ["edges"],
        guardrails: guardrailSets.intake,
        chartLabel: "baseline",
        bic: [
          ["Null", 96.4],
          ["Observed", 81.2]
        ],
        prompt: `dataset: school_friendship
actors: students
tie: undirected friendship
node attributes: club, grade, activity

task:
summarize network diagnostics for ERGM specification.`,
        output: `{
  "visible_patterns": [
    "same-club ties",
    "local closure",
    "one bridging hub"
  ]
}`,
        outputBadge: "diagnostics",
        highlight: "raw",
        theory:
          "At intake, FORGE has not produced an interpretation yet. It only records that friendships are not random: students appear to cluster by activity, close triangles with mutual friends, and rely on a few bridge students."
      },
      {
        id: "library",
        number: "1",
        rail: "Candidate Library",
        subtitle: "Safe ERGM terms",
        kicker: "Stage 1a",
        title: "Build an Admissible Term Library",
        status: "Stage 1a: library",
        lens: "candidate mechanisms",
        mechanismTitle: "The library turns observations into safe model ingredients",
        mechanismCopy:
          "FORGE proposes only terms that match the network type and attributes. This keeps the LLM from inventing invalid ERGM syntax during specification generation.",
        metrics: [
          ["8", "admissible terms"],
          ["3", "structural terms"],
          ["3", "attribute terms"],
          ["0", "off-menu terms"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'gwdsp(0.5, fixed=TRUE)',
          'gwdegree(0.5, fixed=TRUE)',
          'nodematch("club")',
          'nodematch("grade")',
          'nodefactor("grade")',
          'absdiff("activity")'
        ],
        guardrails: guardrailSets.library,
        chartLabel: "library",
        bic: [
          ["structural", 3],
          ["attribute", 3],
          ["baseline", 1]
        ],
        prompt: `input:
network type: undirected
attributes:
  club: categorical, 3 levels
  grade: categorical, 3 levels
  activity: numeric, range 4-9

task:
construct admissible ERGM term library L*.`,
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
  "guardrails": "pass"
}`,
        outputBadge: "library",
        highlight: "homophily",
        theory:
          "The admissible mechanism space contains four mechanism families: baseline tie propensity, shared-friend closure, degree inequality, and attribute-based similarity."
      },
      {
        id: "spec",
        number: "1b",
        rail: "LLM Specs",
        subtitle: "Structured JSON proposals",
        kicker: "Stage 1b",
        title: "Generate LLM Specifications",
        status: "Stage 1b: LLM proposals",
        lens: "LLM-selected terms",
        mechanismTitle: "The LLM chooses a compact explanation from the library",
        mechanismCopy:
          "The model sees diagnostics and the admissible library, then returns JSON. The interface makes off-menu behavior visible and auditable.",
        metrics: [
          ["3", "candidate specs"],
          ["100%", "library compliance"],
          ["4", "terms in best spec"],
          ["0.2", "temperature"]
        ],
        terms: ["edges", 'gwesp(0.5, fixed=TRUE)', 'nodematch("club")', 'gwdegree(0.5, fixed=TRUE)'],
        guardrails: guardrailSets.spec,
        chartLabel: "M4 selected",
        bic: [
          ["M4", 70.2],
          ["M5", 72.5],
          ["M6", 74.8]
        ],
        prompt: `system:
You are an ERGM expert. Return JSON only.

user:
Use only L*. Include edges. Explain expected signs.
Network diagnostics suggest clustering, homophily, and degree skew.`,
        output: `{
  "strategy": "M4",
  "formula": [
    "edges",
    "gwesp(0.5, fixed=TRUE)",
    "nodematch(\\"club\\")",
    "gwdegree(0.5, fixed=TRUE)"
  ],
  "expected_effects": {
    "gwesp(0.5, fixed=TRUE)": "+",
    "nodematch(\\"club\\")": "+",
    "gwdegree(0.5, fixed=TRUE)": "+"
  }
}`,
        outputBadge: "llm json",
        highlight: "closure",
        theory:
          "The LLM's first proposal is that friendship is mostly explained by shared friends, shared activity group, and unequal popularity."
      },
      {
        id: "fit",
        number: "2",
        rail: "Fit and Select",
        subtitle: "MPLE screen",
        kicker: "Stage 2",
        title: "Fit Candidate Specifications",
        status: "Stage 2: model screen",
        lens: "best pseudo-BIC",
        mechanismTitle: "Fitting turns plausible stories into comparable evidence",
        mechanismCopy:
          "Stage 2 screens candidate specifications with fast MPLE. Lower pseudo-BIC and acceptable diagnostics decide which model moves forward.",
        metrics: [
          ["70.2", "best pseudo-BIC"],
          ["0.71", "AUPRC"],
          ["2.8", "max Wald |z|"],
          ["M4", "winner"]
        ],
        terms: ["edges", 'gwesp(0.5, fixed=TRUE)', 'nodematch("club")', 'gwdegree(0.5, fixed=TRUE)'],
        guardrails: guardrailSets.fit,
        chartLabel: "M4 winner",
        bic: [
          ["Null", 96.4],
          ["M4", 70.2],
          ["M5", 72.5],
          ["M6", 74.8]
        ],
        prompt: `candidate catalog:
M3_null = edges
M4 = edges + gwesp + nodematch(club) + gwdegree
M5 = edges + gwesp + nodematch(grade) + absdiff(activity)
M6 = edges + gwesp + gwdsp + nodematch(club) + gwdegree

task:
fit MPLE and rank by pseudo-BIC, AUPRC, diagnostics.`,
        output: `[
  {"spec": "M3_null", "pseudo_bic": 96.4, "auprc": 0.39},
  {"spec": "M4", "pseudo_bic": 70.2, "auprc": 0.71},
  {"spec": "M5", "pseudo_bic": 72.5, "auprc": 0.68},
  {"spec": "M6", "pseudo_bic": 74.8, "auprc": 0.70}
]`,
        outputBadge: "fit table",
        highlight: "winner",
        theory:
          "The evidence favors the compact specification: shared friends, shared club, and degree inequality explain more than the edge-only baseline."
      },
      {
        id: "refine",
        number: "3",
        rail: "Refinement",
        subtitle: "One-edit loop",
        kicker: "Stage 3",
        title: "LLM-Guided Refinement",
        status: "Stage 3: refinement",
        lens: "accepted edit",
        mechanismTitle: "The refinement loop fixes residual misfit with one auditable edit",
        mechanismCopy:
          "GOF indicates that same-grade ties remain under-explained. The LLM proposes one edit from L*, the guardrails validate it, and the fitter accepts it only if diagnostics improve.",
        metrics: [
          ["66.8", "refined BIC"],
          ["1.7", "GOF max |z|"],
          ["1", "accepted edit"],
          ["pass", "GOF status"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'nodematch("club")',
          'gwdegree(0.5, fixed=TRUE)',
          'nodematch("grade")'
        ],
        guardrails: guardrailSets.refine,
        chartLabel: "refined",
        bic: [
          ["M4", 70.2],
          ["+grade", 66.8]
        ],
        prompt: `current model:
edges + gwesp(0.5) + nodematch("club") + gwdegree(0.5)

diagnostics:
max |z| = 2.6
largest residual: same-grade dyads underfit

task:
return one JSON edit from L*.`,
        output: `{
  "action": "add",
  "term": "nodematch(\\"grade\\")",
  "rationale": "Same-grade friendships remain underfit after controlling for club and closure.",
  "accepted": true,
  "bic_before": 70.2,
  "bic_after": 66.8,
  "max_abs_z_after": 1.7
}`,
        outputBadge: "edit record",
        highlight: "refined",
        theory:
          "The refined model adds grade cohorts: students form friendships through shared activity, shared grade, shared friends, and uneven centrality."
      },
      {
        id: "interpret",
        number: "4",
        rail: "Interpretation",
        subtitle: "Model-grounded explanation",
        kicker: "Stage 4",
        title: "Interpret the Mechanism",
        status: "Stage 4: interpretation",
        lens: "model-grounded interpretation",
        mechanismTitle: "The interpretation LLM converts coefficients into a model-grounded explanation",
        mechanismCopy:
          "Stage 4 does not change the model. It explains what the accepted terms mean, which evidence supports them, and where the claims should stay cautious.",
        metrics: [
          ["4", "mechanisms"],
          ["2", "attribute effects"],
          ["1", "fit caveat"],
          ["0", "causal claims"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'nodematch("club")',
          'gwdegree(0.5, fixed=TRUE)',
          'nodematch("grade")'
        ],
        guardrails: guardrailSets.interpret,
        chartLabel: "final",
        bic: [
          ["Null", 96.4],
          ["LLM", 70.2],
          ["Final", 66.8]
        ],
        prompt: `input:
final formula, coefficients, BIC, GOF, dataset brief, refinement history

task:
explain mechanisms and produce a human-readable, model-grounded interpretation.
Do not infer causality.`,
        output: `{
  "headline": "Friendships are shaped by shared contexts and shared friends.",
  "model_grounded_interpretation": "Students are more likely to be friends when they move through the same social settings. Activity groups and grade cohorts create repeated contact, shared friends close triangles, and a few well-connected students bridge otherwise separate clusters. The model supports this as a conditional network pattern, not as proof that any single attribute causes friendship.",
  "limitations": [
    "Small illustrative network",
    "Interpretation depends on GOF and coefficient stability"
  ]
}`,
        outputBadge: "interpretation json",
        highlight: "final",
        theory:
          "Students are more likely to be friends when they move through the same social settings. Activity groups and grade cohorts create repeated contact, shared friends close triangles, and a few well-connected students bridge otherwise separate clusters. The model supports this as a conditional network pattern, not as proof that any single attribute causes friendship."
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
    stages: [
      {
        id: "intake",
        number: "0",
        rail: "Network Intake",
        subtitle: "Graph and diagnostics",
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
        chartLabel: "baseline",
        bic: [
          ["Null", 104.8],
          ["Observed", 89.6]
        ],
        prompt: `dataset: research_collab
actors: researchers
tie: undirected collaboration
node attributes: area, role, seniority

task:
summarize network diagnostics for ERGM specification.`,
        output: `{
  "visible_patterns": [
    "same-area collaboration",
    "shared collaborator closure",
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
        number: "1",
        rail: "Candidate Library",
        subtitle: "Safe ERGM terms",
        kicker: "Stage 1a",
        title: "Build an Admissible Term Library",
        status: "Stage 1a: library",
        lens: "candidate mechanisms",
        mechanismTitle: "The library maps lab structure into valid ERGM terms",
        mechanismCopy:
          "FORGE restricts the LLM to terms that match an undirected collaboration graph and the available area, role, and seniority attributes.",
        metrics: [
          ["8", "admissible terms"],
          ["3", "structural terms"],
          ["3", "attribute terms"],
          ["0", "off-menu terms"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'gwdsp(0.5, fixed=TRUE)',
          'gwdegree(0.5, fixed=TRUE)',
          'nodematch("area")',
          'nodematch("role")',
          'nodefactor("role")',
          'absdiff("seniority")'
        ],
        guardrails: guardrailSets.library,
        chartLabel: "library",
        bic: [
          ["structural", 3],
          ["attribute", 3],
          ["baseline", 1]
        ],
        prompt: `input:
network type: undirected
attributes:
  area: categorical, 3 levels
  role: categorical, 3 levels
  seniority: numeric, range 2-9

task:
construct admissible ERGM term library L*.`,
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
  "guardrails": "pass"
}`,
        outputBadge: "library",
        highlight: "homophily",
        theory:
          "The admissible mechanism space contains baseline collaboration rate, shared-collaborator closure, collaboration inequality, same-area effects, and role or seniority effects."
      },
      {
        id: "spec",
        number: "1b",
        rail: "LLM Specs",
        subtitle: "Structured JSON proposals",
        kicker: "Stage 1b",
        title: "Generate LLM Specifications",
        status: "Stage 1b: LLM proposals",
        lens: "LLM-selected terms",
        mechanismTitle: "The LLM chooses a compact collaboration story",
        mechanismCopy:
          "The LLM proposes candidate formulas using only L*. The strongest first proposal explains collaboration through area homophily, shared collaborators, and degree heterogeneity.",
        metrics: [
          ["3", "candidate specs"],
          ["100%", "library compliance"],
          ["4", "terms in best spec"],
          ["0.2", "temperature"]
        ],
        terms: ["edges", 'gwesp(0.5, fixed=TRUE)', 'nodematch("area")', 'gwdegree(0.5, fixed=TRUE)'],
        guardrails: guardrailSets.spec,
        chartLabel: "L4 selected",
        bic: [
          ["L4", 82.7],
          ["L5", 85.1],
          ["L6", 86.9]
        ],
        prompt: `system:
You are an ERGM expert. Return JSON only.

user:
Use only L*. Include edges. Explain expected signs.
Diagnostics suggest same-area collaboration, closure, and bridge researchers.`,
        output: `{
  "strategy": "L4",
  "formula": [
    "edges",
    "gwesp(0.5, fixed=TRUE)",
    "nodematch(\\"area\\")",
    "gwdegree(0.5, fixed=TRUE)"
  ],
  "expected_effects": {
    "gwesp(0.5, fixed=TRUE)": "+",
    "nodematch(\\"area\\")": "+",
    "gwdegree(0.5, fixed=TRUE)": "+"
  }
}`,
        outputBadge: "llm json",
        highlight: "closure",
        theory:
          "The LLM's first proposal is that researchers collaborate mostly through shared collaborators, shared research area, and unequal centrality."
      },
      {
        id: "fit",
        number: "2",
        rail: "Fit and Select",
        subtitle: "MPLE screen",
        kicker: "Stage 2",
        title: "Fit Candidate Specifications",
        status: "Stage 2: model screen",
        lens: "best pseudo-BIC",
        mechanismTitle: "Fitting compares collaboration explanations",
        mechanismCopy:
          "Stage 2 screens candidate lab models. The best model improves over the null while keeping the formula small enough to interpret live.",
        metrics: [
          ["82.7", "best pseudo-BIC"],
          ["0.74", "AUPRC"],
          ["2.5", "max Wald |z|"],
          ["L4", "winner"]
        ],
        terms: ["edges", 'gwesp(0.5, fixed=TRUE)', 'nodematch("area")', 'gwdegree(0.5, fixed=TRUE)'],
        guardrails: guardrailSets.fit,
        chartLabel: "L4 winner",
        bic: [
          ["Null", 104.8],
          ["L4", 82.7],
          ["L5", 85.1],
          ["L6", 86.9]
        ],
        prompt: `candidate catalog:
L3_null = edges
L4 = edges + gwesp + nodematch(area) + gwdegree
L5 = edges + gwesp + nodematch(role) + absdiff(seniority)
L6 = edges + gwesp + gwdsp + nodematch(area) + gwdegree

task:
fit MPLE and rank by pseudo-BIC, AUPRC, diagnostics.`,
        output: `[
  {"spec": "L3_null", "pseudo_bic": 104.8, "auprc": 0.41},
  {"spec": "L4", "pseudo_bic": 82.7, "auprc": 0.74},
  {"spec": "L5", "pseudo_bic": 85.1, "auprc": 0.69},
  {"spec": "L6", "pseudo_bic": 86.9, "auprc": 0.72}
]`,
        outputBadge: "fit table",
        highlight: "winner",
        theory:
          "The evidence favors a compact specification: collaboration follows research-area boundaries, shared collaborators, and uneven centrality."
      },
      {
        id: "refine",
        number: "3",
        rail: "Refinement",
        subtitle: "One-edit loop",
        kicker: "Stage 3",
        title: "LLM-Guided Refinement",
        status: "Stage 3: refinement",
        lens: "accepted edit",
        mechanismTitle: "The refinement loop adds a role effect",
        mechanismCopy:
          "Residual diagnostics show underfit among same-role dyads. The LLM proposes one admissible edit, and the fitter accepts it because BIC and GOF improve.",
        metrics: [
          ["78.9", "refined BIC"],
          ["1.6", "GOF max |z|"],
          ["1", "accepted edit"],
          ["pass", "GOF status"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'nodematch("area")',
          'gwdegree(0.5, fixed=TRUE)',
          'nodematch("role")'
        ],
        guardrails: guardrailSets.refine,
        chartLabel: "refined",
        bic: [
          ["L4", 82.7],
          ["+role", 78.9]
        ],
        prompt: `current model:
edges + gwesp(0.5) + nodematch("area") + gwdegree(0.5)

diagnostics:
max |z| = 2.4
largest residual: same-role collaboration underfit

task:
return one JSON edit from L*.`,
        output: `{
  "action": "add",
  "term": "nodematch(\\"role\\")",
  "rationale": "Same-role collaborations remain underfit after controlling for area and closure.",
  "accepted": true,
  "bic_before": 82.7,
  "bic_after": 78.9,
  "max_abs_z_after": 1.6
}`,
        outputBadge: "edit record",
        highlight: "refined",
        theory:
          "The refined model adds role structure: collaborations form through shared research area, same-role ties, shared collaborators, and a few central connectors."
      },
      {
        id: "interpret",
        number: "4",
        rail: "Interpretation",
        subtitle: "Model-grounded explanation",
        kicker: "Stage 4",
        title: "Interpret the Mechanism",
        status: "Stage 4: interpretation",
        lens: "model-grounded interpretation",
        mechanismTitle: "The interpretation LLM turns lab coefficients into a model-grounded explanation",
        mechanismCopy:
          "Stage 4 freezes the selected model and explains why each term matters: area boundaries, shared collaborators, role structure, and central bridge researchers.",
        metrics: [
          ["4", "mechanisms"],
          ["2", "attribute effects"],
          ["1", "fit caveat"],
          ["0", "causal claims"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'nodematch("area")',
          'gwdegree(0.5, fixed=TRUE)',
          'nodematch("role")'
        ],
        guardrails: guardrailSets.interpret,
        chartLabel: "final",
        bic: [
          ["Null", 104.8],
          ["LLM", 82.7],
          ["Final", 78.9]
        ],
        prompt: `input:
final formula, coefficients, BIC, GOF, dataset brief, refinement history

task:
explain mechanisms and produce a human-readable, model-grounded interpretation.
Do not infer causality.`,
        output: `{
  "headline": "Collaborations are shaped by research area, shared collaborators, and role structure.",
  "model_grounded_interpretation": "Researchers are more likely to collaborate when they work in the same area and already share collaborators. Role similarity adds another layer: peers with similar positions tend to appear together in projects. A few highly connected researchers link otherwise separate areas. The model supports this as a conditional collaboration pattern, not proof that area or role causes collaboration.",
  "limitations": [
    "Small illustrative network",
    "Interpretation depends on GOF and coefficient stability"
  ]
}`,
        outputBadge: "interpretation json",
        highlight: "final",
        theory:
          "Researchers are more likely to collaborate when they work in the same area and already share collaborators. Role similarity adds another layer: peers with similar positions tend to appear together in projects. A few highly connected researchers link otherwise separate areas. The model supports this as a conditional collaboration pattern, not proof that area or role causes collaboration."
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
    stages: [
      {
        id: "intake",
        number: "0",
        rail: "Network Intake",
        subtitle: "Graph and diagnostics",
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
        chartLabel: "baseline",
        bic: [
          ["Null", 99.3],
          ["Observed", 84.7]
        ],
        prompt: `dataset: neighborhood_aid
actors: households
tie: undirected mutual-aid exchange
node attributes: block, tenure_group, tenure_years

task:
summarize network diagnostics for ERGM specification.`,
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
        number: "1",
        rail: "Candidate Library",
        subtitle: "Safe ERGM terms",
        kicker: "Stage 1a",
        title: "Build an Admissible Term Library",
        status: "Stage 1a: library",
        lens: "candidate mechanisms",
        mechanismTitle: "The library turns block structure into safe model terms",
        mechanismCopy:
          "FORGE proposes terms that fit an undirected household support network and the available block and residence-tenure attributes.",
        metrics: [
          ["8", "admissible terms"],
          ["3", "structural terms"],
          ["3", "attribute terms"],
          ["0", "off-menu terms"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'gwdsp(0.5, fixed=TRUE)',
          'gwdegree(0.5, fixed=TRUE)',
          'nodematch("block")',
          'nodematch("tenure_group")',
          'nodefactor("tenure_group")',
          'absdiff("tenure_years")'
        ],
        guardrails: guardrailSets.library,
        chartLabel: "library",
        bic: [
          ["structural", 3],
          ["attribute", 3],
          ["baseline", 1]
        ],
        prompt: `input:
network type: undirected
attributes:
  block: categorical, 3 levels
  tenure_group: categorical, 3 levels
  tenure_years: numeric, range 1-15

task:
construct admissible ERGM term library L*.`,
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
  "guardrails": "pass"
}`,
        outputBadge: "library",
        highlight: "homophily",
        theory:
          "The admissible mechanism space contains baseline support rate, shared-neighbor closure, support hubs, same-block clustering, and residence-tenure similarity."
      },
      {
        id: "spec",
        number: "1b",
        rail: "LLM Specs",
        subtitle: "Structured JSON proposals",
        kicker: "Stage 1b",
        title: "Generate LLM Specifications",
        status: "Stage 1b: LLM proposals",
        lens: "LLM-selected terms",
        mechanismTitle: "The LLM chooses a compact mutual-aid explanation",
        mechanismCopy:
          "The first LLM proposal explains support through same-block proximity, shared-neighbor closure, and a few high-degree helper households.",
        metrics: [
          ["3", "candidate specs"],
          ["100%", "library compliance"],
          ["4", "terms in best spec"],
          ["0.2", "temperature"]
        ],
        terms: ["edges", 'gwesp(0.5, fixed=TRUE)', 'nodematch("block")', 'gwdegree(0.5, fixed=TRUE)'],
        guardrails: guardrailSets.spec,
        chartLabel: "N4 selected",
        bic: [
          ["N4", 76.4],
          ["N5", 79.0],
          ["N6", 80.6]
        ],
        prompt: `system:
You are an ERGM expert. Return JSON only.

user:
Use only L*. Include edges. Explain expected signs.
Diagnostics suggest same-block support, closure, and helper hubs.`,
        output: `{
  "strategy": "N4",
  "formula": [
    "edges",
    "gwesp(0.5, fixed=TRUE)",
    "nodematch(\\"block\\")",
    "gwdegree(0.5, fixed=TRUE)"
  ],
  "expected_effects": {
    "gwesp(0.5, fixed=TRUE)": "+",
    "nodematch(\\"block\\")": "+",
    "gwdegree(0.5, fixed=TRUE)": "+"
  }
}`,
        outputBadge: "llm json",
        highlight: "closure",
        theory:
          "The LLM's first proposal is that mutual aid is explained by same-block proximity, shared neighbors, and uneven helper centrality."
      },
      {
        id: "fit",
        number: "2",
        rail: "Fit and Select",
        subtitle: "MPLE screen",
        kicker: "Stage 2",
        title: "Fit Candidate Specifications",
        status: "Stage 2: model screen",
        lens: "best pseudo-BIC",
        mechanismTitle: "Fitting compares support-network stories",
        mechanismCopy:
          "Stage 2 ranks the candidate household-support models. The selected formula improves fit over the baseline while remaining interpretable.",
        metrics: [
          ["76.4", "best pseudo-BIC"],
          ["0.73", "AUPRC"],
          ["2.7", "max Wald |z|"],
          ["N4", "winner"]
        ],
        terms: ["edges", 'gwesp(0.5, fixed=TRUE)', 'nodematch("block")', 'gwdegree(0.5, fixed=TRUE)'],
        guardrails: guardrailSets.fit,
        chartLabel: "N4 winner",
        bic: [
          ["Null", 99.3],
          ["N4", 76.4],
          ["N5", 79.0],
          ["N6", 80.6]
        ],
        prompt: `candidate catalog:
N3_null = edges
N4 = edges + gwesp + nodematch(block) + gwdegree
N5 = edges + gwesp + nodematch(tenure_group) + absdiff(tenure_years)
N6 = edges + gwesp + gwdsp + nodematch(block) + gwdegree

task:
fit MPLE and rank by pseudo-BIC, AUPRC, diagnostics.`,
        output: `[
  {"spec": "N3_null", "pseudo_bic": 99.3, "auprc": 0.38},
  {"spec": "N4", "pseudo_bic": 76.4, "auprc": 0.73},
  {"spec": "N5", "pseudo_bic": 79.0, "auprc": 0.67},
  {"spec": "N6", "pseudo_bic": 80.6, "auprc": 0.70}
]`,
        outputBadge: "fit table",
        highlight: "winner",
        theory:
          "The evidence favors a compact specification: aid flows through block proximity, shared neighbors, and a few highly connected helper households."
      },
      {
        id: "refine",
        number: "3",
        rail: "Refinement",
        subtitle: "One-edit loop",
        kicker: "Stage 3",
        title: "LLM-Guided Refinement",
        status: "Stage 3: refinement",
        lens: "accepted edit",
        mechanismTitle: "The refinement loop adds residence-tenure similarity",
        mechanismCopy:
          "GOF shows residual underfit among households with similar residence duration. The LLM adds one admissible tenure term, and fit improves.",
        metrics: [
          ["72.9", "refined BIC"],
          ["1.8", "GOF max |z|"],
          ["1", "accepted edit"],
          ["pass", "GOF status"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'nodematch("block")',
          'gwdegree(0.5, fixed=TRUE)',
          'absdiff("tenure_years")'
        ],
        guardrails: guardrailSets.refine,
        chartLabel: "refined",
        bic: [
          ["N4", 76.4],
          ["+tenure", 72.9]
        ],
        prompt: `current model:
edges + gwesp(0.5) + nodematch("block") + gwdegree(0.5)

diagnostics:
max |z| = 2.5
largest residual: similar-tenure households underfit

task:
return one JSON edit from L*.`,
        output: `{
  "action": "add",
  "term": "absdiff(\\"tenure_years\\")",
  "rationale": "Households with similar residence duration remain underfit after controlling for block and closure.",
  "accepted": true,
  "bic_before": 76.4,
  "bic_after": 72.9,
  "max_abs_z_after": 1.8
}`,
        outputBadge: "edit record",
        highlight: "refined",
        theory:
          "The refined model adds residence tenure: support is structured by block, shared neighbors, helper hubs, and similarity in how long households have lived there."
      },
      {
        id: "interpret",
        number: "4",
        rail: "Interpretation",
        subtitle: "Model-grounded explanation",
        kicker: "Stage 4",
        title: "Interpret the Mechanism",
        status: "Stage 4: interpretation",
        lens: "model-grounded interpretation",
        mechanismTitle: "The interpretation LLM turns support-network terms into a model-grounded explanation",
        mechanismCopy:
          "Stage 4 explains the final model in ordinary language: local proximity, shared neighbors, helper hubs, and tenure similarity shape the observed aid network.",
        metrics: [
          ["4", "mechanisms"],
          ["2", "attribute effects"],
          ["1", "fit caveat"],
          ["0", "causal claims"]
        ],
        terms: [
          "edges",
          'gwesp(0.5, fixed=TRUE)',
          'nodematch("block")',
          'gwdegree(0.5, fixed=TRUE)',
          'absdiff("tenure_years")'
        ],
        guardrails: guardrailSets.interpret,
        chartLabel: "final",
        bic: [
          ["Null", 99.3],
          ["LLM", 76.4],
          ["Final", 72.9]
        ],
        prompt: `input:
final formula, coefficients, BIC, GOF, dataset brief, refinement history

task:
explain mechanisms and produce a human-readable, model-grounded interpretation.
Do not infer causality.`,
        output: `{
  "headline": "Mutual aid is shaped by local blocks, shared neighbors, and residence history.",
  "model_grounded_interpretation": "Households are more likely to exchange help when they live in the same block and share nearby support contacts. A small number of well-connected households bridge blocks, while similar residence history adds another layer of connection. The model supports this as a conditional pattern in the aid network, not proof that block or tenure causes support.",
  "limitations": [
    "Small illustrative network",
    "Interpretation depends on GOF and coefficient stability"
  ]
}`,
        outputBadge: "interpretation json",
        highlight: "final",
        theory:
          "Households are more likely to exchange help when they live in the same block and share nearby support contacts. A small number of well-connected households bridge blocks, while similar residence history adds another layer of connection. The model supports this as a conditional pattern in the aid network, not proof that block or tenure causes support."
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
const outputView = document.getElementById("output-view");
const outputBadge = document.getElementById("output-badge");
const theoryHeadline = document.getElementById("theory-headline");
const theoryCopy = document.getElementById("theory-copy");
const termList = document.getElementById("term-list");
const termCount = document.getElementById("term-count");
const bicChart = document.getElementById("bic-chart");
const bestModelLabel = document.getElementById("best-model-label");
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
  return demo.nodeById[edge.source].cohort === demo.nodeById[edge.target].cohort;
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
    const highlight = shouldHighlightEdge(edge, stage.highlight, demo);
    const muted = stage.highlight !== "raw" && !highlight;
    edgeLayer.appendChild(svgEl("line", {
      x1: source.x,
      y1: source.y,
      x2: target.x,
      y2: target.y,
      class: `edge${highlight ? " highlight" : ""}${muted ? " muted" : ""}`
    }));
  });

  demo.nodes.forEach((node) => {
    const group = svgEl("g", { transform: `translate(${node.x}, ${node.y})` });
    const highlight = shouldHighlightNode(node, stage.highlight, demo);
    const color = colorForGroup(demo, node.group);
    group.appendChild(svgEl("circle", {
      r: 23 + Math.min(demo.degreeById[node.id], 5),
      class: `node-ring ${highlight ? "highlight" : ""}`,
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
    name.textContent = term;
    const meaning = document.createElement("span");
    meaning.className = "term-meaning";
    meaning.textContent = termMeanings[term] || "model mechanism";

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
    dot.style.background = status === "pass" ? "var(--green)" : "var(--amber)";
    const copyEl = document.createElement("span");
    copyEl.className = "guardrail-copy";
    copyEl.textContent = copy;
    item.appendChild(dot);
    item.appendChild(copyEl);
    guardrailList.appendChild(item);
  });
}

function renderChart(rows) {
  bicChart.replaceChildren();
  const values = rows.map(([, value]) => value);
  const min = Math.min(...values);
  const max = Math.max(...values);
  rows.forEach(([label, value]) => {
    const row = document.createElement("div");
    row.className = "bar-row";

    const labelEl = document.createElement("span");
    labelEl.className = "bar-label";
    labelEl.textContent = label;

    const track = document.createElement("span");
    track.className = "bar-track";
    const fill = document.createElement("span");
    fill.className = "bar-fill";
    const width = max === min ? 80 : 20 + ((max - value) / (max - min)) * 78;
    fill.style.width = `${width}%`;
    track.appendChild(fill);

    const valueEl = document.createElement("span");
    valueEl.className = "bar-value";
    valueEl.textContent = value;

    row.appendChild(labelEl);
    row.appendChild(track);
    row.appendChild(valueEl);
    bicChart.appendChild(row);
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
  theoryHeadline.textContent = stage.mechanismTitle;
  theoryCopy.textContent = stage.theory;
  bestModelLabel.textContent = stage.chartLabel;

  renderNetworkPicker();
  renderLegend(demo);
  renderStageList();
  renderNetwork(stage);
  renderMetrics(stage.metrics);
  renderTerms(stage.terms);
  renderGuardrails(stage.guardrails);
  renderChart(stage.bic);

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
  if (event.key === "ArrowRight") nextStage();
  if (event.key === "ArrowLeft") previousStage();
});

setStage(0);
