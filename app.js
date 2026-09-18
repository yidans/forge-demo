/* FORGE demo — static walkthrough of the pipeline on five packaged networks.
 * Stages 0–1b are built from the network data, the R-generated valid-term list
 * L* (same builder and options as the live server), and hand-written example
 * prompts. Stages 2–4 are generated from `runRecords`: real fits with the
 * selected estimator (SA, MCMLE, or MPLE), 30-simulation density checks,
 * 100-simulation GOF discrepancies q(M) = max_k |z_k|, and up to four checked
 * revision rounds, computed with the ergm package on each network. LLM prompts,
 * rationales, and the plain-language readings are template text; the fit, GOF,
 * coefficient, and decision values are not. */

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
  const dyads = demo.directed ? nodeCount * (nodeCount - 1) : (nodeCount * (nodeCount - 1)) / 2;
  const density = nodeCount > 1 ? edgeCount / dyads : 0;
  const transitivity = connectedTriples > 0 ? (3 * triangles) / connectedTriples : 0;
  let reciprocity = null;
  if (demo.directed) {
    const arcs = new Set(demo.edges.map((e) => `${e.source}>${e.target}`));
    const mutualArcs = demo.edges.filter((e) => arcs.has(`${e.target}>${e.source}`)).length;
    reciprocity = edgeCount ? mutualArcs / edgeCount : 0;
  }
  const degreeMax = Math.max(...Object.values(degreeById));

  return {
    adjacency,
    degreeById,
    diagnostics: {
      nodes: nodeCount,
      edges: edgeCount,
      directed: Boolean(demo.directed),
      density,
      triangles,
      transitivity,
      reciprocity,
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

  const d = demo.diagnostics;
  intake.metrics = [
    [String(d.nodes), demo.nodeKind],
    [String(d.edges), demo.tieKind],
    [formatMetric(d.density, 2), "density"],
    d.directed ? [formatMetric(d.reciprocity, 2), "reciprocity"] : [formatMetric(d.transitivity, 2), "transitivity"]
  ];
  intake.output = JSON.stringify({
    nodes: d.nodes,
    edges: d.edges,
    directed: d.directed,
    density: roundMetric(d.density, 3),
    triangles: d.triangles,
    transitivity: roundMetric(d.transitivity, 3),
    ...(d.directed ? { reciprocity: roundMetric(d.reciprocity, 3) } : {}),
    degree_max: d.degreeMax,
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

function termBase(term) {
  return term.split("(")[0].trim();
}

function termAttr(term) {
  return (term.match(/\(\s*"?([A-Za-z_]+)"?\s*\)/) || [])[1] || null;
}

const termMeanings = {
  edges: "baseline tie rate"
};

function termGloss(term) {
  if (termMeanings[term]) return termMeanings[term];
  const base = termBase(term);
  const attr = termAttr(term);
  const map = {
    edges: "baseline tie rate",
    mutual: "reciprocated ties",
    gwesp: "shared partners / closure",
    gwdsp: "open two-path pressure",
    gwdegree: "degree spread / hub structure",
    gwidegree: "in-degree spread (who receives)",
    gwodegree: "out-degree spread (who sends)",
    twopath: "two-path connectivity",
    ttriple: "transitive triads",
    ctriple: "cyclic triads",
    nodematch: `same-${attr || "group"} ties`,
    nodemix: `${attr || "group"} pairing mix`,
    nodefactor: `${attr || "group"}-level activity`,
    nodeifactor: `${attr || "group"}: incoming activity`,
    nodeofactor: `${attr || "group"}: outgoing activity`,
    nodecov: `ties scale with ${attr || "attribute"}`,
    nodeicov: `incoming ties scale with ${attr || "attribute"}`,
    nodeocov: `outgoing ties scale with ${attr || "attribute"}`,
    absdiff: `${attr || "attribute"} difference between the pair`
  };
  return map[base] || "model mechanism";
}

const STRUCTURAL_BASES = new Set(["edges", "mutual", "gwesp", "gwdsp", "gwdegree", "gwidegree", "gwodegree", "twopath", "ttriple", "ctriple"]);

// ---------------------------------------------------------------- data

const ESTIMATORS = [
  { id: "sa", label: "SA", long: "stochastic approximation (SA)", note: "Statnet's ergm, main.method = \"Stochastic-Approximation\"" },
  { id: "mcmle", label: "MCMLE", long: "Monte Carlo maximum likelihood (MCMLE)", note: "Statnet's ergm, main.method = \"MCMLE\"" },
  { id: "mple", label: "MPLE", long: "maximum pseudolikelihood (MPLE)", note: "Statnet's ergm, estimate = \"MPLE\"" }
];

const networkData = {"school": {"directed": false, "attrs": {"group": "club", "cohort": "grade", "score": "activity"}, "nodes": [{"id": "ada", "name": "Ada", "group": "Robotics", "cohort": "11", "score": 9, "x": 132, "y": 84}, {"id": "ben", "name": "Ben", "group": "Robotics", "cohort": "11", "score": 8, "x": 244, "y": 74}, {"id": "cal", "name": "Cal", "group": "Robotics", "cohort": "10", "score": 7, "x": 188, "y": 178}, {"id": "dia", "name": "Dia", "group": "Robotics", "cohort": "10", "score": 5, "x": 312, "y": 178}, {"id": "eli", "name": "Eli", "group": "Drama", "cohort": "11", "score": 6, "x": 462, "y": 92}, {"id": "fay", "name": "Fay", "group": "Drama", "cohort": "11", "score": 7, "x": 590, "y": 102}, {"id": "gia", "name": "Gia", "group": "Drama", "cohort": "10", "score": 6, "x": 520, "y": 206}, {"id": "hal", "name": "Hal", "group": "Drama", "cohort": "10", "score": 5, "x": 632, "y": 244}, {"id": "ivy", "name": "Ivy", "group": "Studio", "cohort": "12", "score": 9, "x": 172, "y": 332}, {"id": "jay", "name": "Jay", "group": "Studio", "cohort": "12", "score": 8, "x": 304, "y": 332}, {"id": "kim", "name": "Kim", "group": "Studio", "cohort": "11", "score": 4, "x": 438, "y": 342}, {"id": "leo", "name": "Leo", "group": "Robotics", "cohort": "12", "score": 6, "x": 356, "y": 258}], "edges": [["ada", "ben"], ["ada", "cal"], ["ben", "cal"], ["ben", "dia"], ["cal", "dia"], ["eli", "fay"], ["eli", "gia"], ["fay", "gia"], ["gia", "hal"], ["ivy", "jay"], ["ivy", "kim"], ["jay", "kim"], ["jay", "leo"], ["cal", "leo"], ["dia", "leo"], ["leo", "kim"], ["dia", "eli"], ["kim", "gia"]]}, "lab": {"directed": false, "attrs": {"group": "area", "cohort": "role", "score": "seniority"}, "nodes": [{"id": "noor", "name": "Noor", "group": "NLP", "cohort": "PI", "score": 9, "x": 132, "y": 84}, {"id": "omar", "name": "Omar", "group": "NLP", "cohort": "Postdoc", "score": 7, "x": 244, "y": 74}, {"id": "pia", "name": "Pia", "group": "NLP", "cohort": "Student", "score": 4, "x": 188, "y": 178}, {"id": "qin", "name": "Qin", "group": "NLP", "cohort": "Student", "score": 3, "x": 312, "y": 178}, {"id": "rui", "name": "Rui", "group": "Vision", "cohort": "PI", "score": 9, "x": 462, "y": 92}, {"id": "sol", "name": "Sol", "group": "Vision", "cohort": "Postdoc", "score": 6, "x": 590, "y": 102}, {"id": "tao", "name": "Tao", "group": "Vision", "cohort": "Student", "score": 3, "x": 520, "y": 206}, {"id": "uma", "name": "Uma", "group": "Vision", "cohort": "Student", "score": 2, "x": 172, "y": 332}, {"id": "val", "name": "Val", "group": "Systems", "cohort": "PI", "score": 8, "x": 172, "y": 332}, {"id": "wes", "name": "Wes", "group": "Systems", "cohort": "Postdoc", "score": 6, "x": 304, "y": 332}, {"id": "xia", "name": "Xia", "group": "Systems", "cohort": "Student", "score": 3, "x": 438, "y": 342}, {"id": "yan", "name": "Yan", "group": "Systems", "cohort": "Student", "score": 4, "x": 356, "y": 258}], "edges": [["noor", "omar"], ["noor", "pia"], ["omar", "pia"], ["omar", "qin"], ["pia", "qin"], ["rui", "sol"], ["rui", "tao"], ["sol", "uma"], ["tao", "uma"], ["sol", "tao"], ["val", "wes"], ["val", "yan"], ["wes", "xia"], ["xia", "yan"], ["wes", "yan"], ["pia", "rui"], ["qin", "tao"], ["yan", "qin"], ["xia", "tao"], ["noor", "val"]]}, "neighborhood": {"directed": false, "attrs": {"group": "block", "cohort": "tenure_group", "score": "tenure_years"}, "nodes": [{"id": "nora", "name": "Nora", "group": "North", "cohort": "Long", "score": 12, "x": 132, "y": 84}, {"id": "theo", "name": "Theo", "group": "North", "cohort": "Mid", "score": 6, "x": 244, "y": 74}, {"id": "mina", "name": "Mina", "group": "North", "cohort": "New", "score": 2, "x": 188, "y": 178}, {"id": "otis", "name": "Otis", "group": "North", "cohort": "Mid", "score": 5, "x": 312, "y": 178}, {"id": "park", "name": "Park", "group": "Market", "cohort": "Long", "score": 15, "x": 462, "y": 92}, {"id": "raya", "name": "Raya", "group": "Market", "cohort": "Mid", "score": 7, "x": 590, "y": 102}, {"id": "sam", "name": "Sam", "group": "Market", "cohort": "New", "score": 1, "x": 520, "y": 206}, {"id": "tess", "name": "Tess", "group": "Market", "cohort": "New", "score": 2, "x": 632, "y": 244}, {"id": "uma", "name": "Uma", "group": "Riverside", "cohort": "Long", "score": 11, "x": 172, "y": 332}, {"id": "vic", "name": "Vic", "group": "Riverside", "cohort": "Mid", "score": 6, "x": 304, "y": 332}, {"id": "wren", "name": "Wren", "group": "Riverside", "cohort": "New", "score": 1, "x": 438, "y": 342}, {"id": "zed", "name": "Zed", "group": "Riverside", "cohort": "Long", "score": 14, "x": 356, "y": 258}], "edges": [["nora", "theo"], ["nora", "mina"], ["theo", "mina"], ["theo", "otis"], ["mina", "otis"], ["park", "raya"], ["park", "sam"], ["raya", "tess"], ["sam", "tess"], ["park", "tess"], ["uma", "vic"], ["vic", "wren"], ["wren", "zed"], ["uma", "zed"], ["uma", "wren"], ["otis", "park"], ["mina", "zed"], ["sam", "wren"]]}, "office": {"directed": true, "attrs": {"group": "department", "cohort": "level", "score": "tenure"}, "nodes": [{"id": "amy", "name": "Amy", "group": "Sales", "cohort": "Senior", "score": 9, "x": 175, "y": 65}, {"id": "bo", "name": "Bo", "group": "Sales", "cohort": "Mid", "score": 4, "x": 275, "y": 117}, {"id": "cy", "name": "Cy", "group": "Sales", "cohort": "Junior", "score": 1, "x": 237, "y": 201}, {"id": "dee", "name": "Dee", "group": "Sales", "cohort": "Mid", "score": 5, "x": 113, "y": 201}, {"id": "eve", "name": "Eve", "group": "Sales", "cohort": "Junior", "score": 2, "x": 75, "y": 117}, {"id": "finn", "name": "Finn", "group": "Engineering", "cohort": "Senior", "score": 11, "x": 545, "y": 55}, {"id": "gus", "name": "Gus", "group": "Engineering", "cohort": "Mid", "score": 6, "x": 645, "y": 107}, {"id": "hana", "name": "Hana", "group": "Engineering", "cohort": "Junior", "score": 1, "x": 607, "y": 191}, {"id": "ivan", "name": "Ivan", "group": "Engineering", "cohort": "Mid", "score": 3, "x": 483, "y": 191}, {"id": "jo", "name": "Jo", "group": "Engineering", "cohort": "Junior", "score": 2, "x": 445, "y": 107}, {"id": "kai", "name": "Kai", "group": "Support", "cohort": "Senior", "score": 8, "x": 360, "y": 247}, {"id": "lena", "name": "Lena", "group": "Support", "cohort": "Mid", "score": 4, "x": 465, "y": 305}, {"id": "mo", "name": "Mo", "group": "Support", "cohort": "Junior", "score": 1, "x": 360, "y": 363}, {"id": "nia", "name": "Nia", "group": "Support", "cohort": "Junior", "score": 2, "x": 255, "y": 305}], "edges": [["bo", "amy"], ["cy", "amy"], ["dee", "amy"], ["eve", "amy"], ["cy", "bo"], ["eve", "dee"], ["amy", "bo"], ["bo", "dee"], ["dee", "bo"], ["eve", "cy"], ["gus", "finn"], ["hana", "finn"], ["ivan", "finn"], ["jo", "finn"], ["hana", "gus"], ["jo", "ivan"], ["ivan", "gus"], ["gus", "ivan"], ["finn", "gus"], ["hana", "ivan"], ["lena", "kai"], ["mo", "kai"], ["nia", "kai"], ["mo", "lena"], ["nia", "lena"], ["kai", "lena"], ["nia", "mo"], ["dee", "finn"], ["gus", "amy"], ["lena", "gus"], ["kai", "amy"], ["ivan", "kai"], ["mo", "cy"]]}, "opensource": {"directed": false, "attrs": {"group": "module", "cohort": "role", "score": "commits"}, "nodes": [{"id": "ana", "name": "Ana", "group": "Core", "cohort": "Maintainer", "score": 320, "x": 170, "y": 58}, {"id": "ben", "name": "Ben", "group": "Core", "cohort": "Contributor", "score": 140, "x": 272, "y": 99}, {"id": "cai", "name": "Cai", "group": "Core", "cohort": "Contributor", "score": 90, "x": 272, "y": 181}, {"id": "dan", "name": "Dan", "group": "Core", "cohort": "Newcomer", "score": 15, "x": 170, "y": 222}, {"id": "eli", "name": "Eli", "group": "Core", "cohort": "Contributor", "score": 60, "x": 68, "y": 181}, {"id": "fen", "name": "Fen", "group": "Core", "cohort": "Newcomer", "score": 8, "x": 68, "y": 99}, {"id": "gil", "name": "Gil", "group": "UI", "cohort": "Maintainer", "score": 280, "x": 550, "y": 52}, {"id": "hal", "name": "Hal", "group": "UI", "cohort": "Contributor", "score": 110, "x": 653, "y": 106}, {"id": "ira", "name": "Ira", "group": "UI", "cohort": "Contributor", "score": 70, "x": 613, "y": 193}, {"id": "jun", "name": "Jun", "group": "UI", "cohort": "Newcomer", "score": 12, "x": 487, "y": 193}, {"id": "kat", "name": "Kat", "group": "UI", "cohort": "Newcomer", "score": 20, "x": 447, "y": 106}, {"id": "lou", "name": "Lou", "group": "Docs", "cohort": "Maintainer", "score": 150, "x": 360, "y": 256}, {"id": "mia", "name": "Mia", "group": "Docs", "cohort": "Contributor", "score": 55, "x": 484, "y": 299}, {"id": "ned", "name": "Ned", "group": "Docs", "cohort": "Newcomer", "score": 10, "x": 436, "y": 368}, {"id": "oli", "name": "Oli", "group": "Docs", "cohort": "Contributor", "score": 40, "x": 284, "y": 368}, {"id": "pat", "name": "Pat", "group": "Docs", "cohort": "Newcomer", "score": 6, "x": 236, "y": 299}], "edges": [["ana", "ben"], ["ana", "cai"], ["ana", "dan"], ["ana", "eli"], ["ana", "fen"], ["ben", "cai"], ["ben", "eli"], ["cai", "dan"], ["dan", "eli"], ["eli", "fen"], ["cai", "fen"], ["gil", "hal"], ["gil", "ira"], ["gil", "jun"], ["gil", "kat"], ["hal", "ira"], ["hal", "jun"], ["ira", "kat"], ["jun", "kat"], ["hal", "kat"], ["lou", "mia"], ["lou", "ned"], ["lou", "oli"], ["lou", "pat"], ["mia", "oli"], ["mia", "ned"], ["ned", "pat"], ["oli", "pat"], ["ana", "gil"], ["gil", "lou"], ["ana", "lou"], ["ben", "hal"]]}};

const libraries = {"school": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.25, fixed=TRUE)", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "gwdegree(decay=0.25, fixed=TRUE)", "gwdegree(decay=0.5, fixed=TRUE)", "twopath", "nodecov(\"activity\")", "absdiff(\"activity\")", "nodematch(\"club\")", "nodefactor(\"club\")", "nodematch(\"grade\")", "nodefactor(\"grade\")"], "lab": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.25, fixed=TRUE)", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "gwdegree(decay=0.25, fixed=TRUE)", "gwdegree(decay=0.5, fixed=TRUE)", "twopath", "nodematch(\"area\")", "nodefactor(\"area\")", "nodematch(\"role\")", "nodefactor(\"role\")", "nodecov(\"seniority\")", "absdiff(\"seniority\")"], "neighborhood": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.25, fixed=TRUE)", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "gwdegree(decay=0.25, fixed=TRUE)", "gwdegree(decay=0.5, fixed=TRUE)", "twopath", "nodematch(\"block\")", "nodefactor(\"block\")", "nodematch(\"tenure_group\")", "nodefactor(\"tenure_group\")", "nodecov(\"tenure_years\")", "absdiff(\"tenure_years\")"], "office": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.25, fixed=TRUE)", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "mutual", "gwidegree(decay=0.25, fixed=TRUE)", "gwodegree(decay=0.25, fixed=TRUE)", "gwidegree(decay=0.5, fixed=TRUE)", "gwodegree(decay=0.5, fixed=TRUE)", "ttriple", "ctriple", "nodematch(\"department\")", "nodeifactor(\"department\")", "nodeofactor(\"department\")", "nodematch(\"level\")", "nodeifactor(\"level\")", "nodeofactor(\"level\")", "nodecov(\"tenure\")", "nodeicov(\"tenure\")", "nodeocov(\"tenure\")", "absdiff(\"tenure\")"], "opensource": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.25, fixed=TRUE)", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "gwdegree(decay=0.25, fixed=TRUE)", "gwdegree(decay=0.5, fixed=TRUE)", "twopath", "nodecov(\"commits\")", "absdiff(\"commits\")", "nodematch(\"module\")", "nodefactor(\"module\")", "nodematch(\"role\")", "nodefactor(\"role\")"]};

const runRecords = {"school": {"sa": {"estimator": "sa", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.228}, {"term": "gwesp.fixed.0.5", "estimate": 0.338}, {"term": "nodematch.club", "estimate": 2.648}, {"term": "gwdeg.fixed.0.5", "estimate": 2.41}], "pbic": 64.32, "density_obs": 0.273, "density_sim": 0.283, "density_rel_error": 0.037, "q": 1.3, "gof_rmse": 0.64, "gof_bins": 21, "residuals": [["degree", "degree4", 5, 2.88, 1.3], ["esp", "esp1", 12, 8.14, 1.23], ["distance", "3", 20, 13.94, 1.12]], "eligible": true, "reason": "eligible", "runtime": 2}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"grade\")", "absdiff(\"activity\")"], "coefficients": [{"term": "edges", "estimate": -1.477}, {"term": "gwesp.fixed.0.5", "estimate": 0.405}, {"term": "nodematch.grade", "estimate": 0.391}, {"term": "absdiff.activity", "estimate": -0.159}], "pbic": 78.38, "density_obs": 0.273, "density_sim": 0.272, "density_rel_error": 0.004, "q": 1.92, "gof_rmse": 0.82, "gof_bins": 24, "residuals": [["distance", "3", 20, 10.33, 1.92], ["degree", "degree4", 5, 2.31, 1.83], ["esp", "esp1", 12, 7.07, 1.57]], "eligible": true, "reason": "eligible", "runtime": 2.1}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -5.867}, {"term": "gwesp.fixed.0.5", "estimate": 0.283}, {"term": "gwdsp.fixed.0.5", "estimate": 0.524}, {"term": "nodematch.club", "estimate": 3.454}, {"term": "gwdeg.fixed.0.5", "estimate": 3.541}], "pbic": 62.96, "density_obs": 0.273, "density_sim": 0.266, "density_rel_error": 0.024, "q": 2.68, "gof_rmse": 0.76, "gof_bins": 22, "residuals": [["degree", "degree4", 5, 1.83, 2.68], ["degree", "degree5", 0, 0.82, -1], ["esp", "esp1", 12, 9.33, 0.96]], "eligible": true, "reason": "eligible", "runtime": 2.4}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.981}], "pbic": 81.54, "density_obs": 0.273, "density_sim": 0.274, "density_rel_error": 0.006, "q": 1.74, "gof_rmse": 0.83, "gof_bins": 22, "residuals": [["distance", "3", 20, 12.83, 1.74], ["esp", "esp0", 3, 8.31, -1.7], ["degree", "degree4", 5, 2.33, 1.67]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 1", "initial_q": 1.3, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"grade\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"grade\")"], "eligible": true, "q_before": 1.3, "q_after": 1.74, "pbic": 68.07, "density_rel_error": 0.026, "residual": ["degree", "degree4", 5, 2.45, 1.74], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.602}, {"term": "gwesp.fixed.0.5", "estimate": 0.358}, {"term": "nodematch.club", "estimate": 2.769}, {"term": "gwdeg.fixed.0.5", "estimate": 2.501}, {"term": "nodematch.grade", "estimate": 0.731}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"activity\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"activity\")"], "eligible": true, "q_before": 1.3, "q_after": 1.72, "pbic": 67.7, "density_rel_error": 0.026, "residual": ["degree", "degree4", 5, 2.37, 1.72], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.873}, {"term": "gwesp.fixed.0.5", "estimate": 0.345}, {"term": "nodematch.club", "estimate": 2.714}, {"term": "gwdeg.fixed.0.5", "estimate": 3.096}, {"term": "absdiff.activity", "estimate": -0.325}]}, {"round": 3, "action": "replace", "target": "gwdegree(decay=0.5, fixed=TRUE)", "term": "gwdegree(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.25, fixed=TRUE)"], "eligible": true, "q_before": 1.3, "q_after": 1.82, "pbic": 64.29, "density_rel_error": 0.039, "residual": ["degree", "degree4", 5, 2.28, 1.82], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.946}, {"term": "gwesp.fixed.0.5", "estimate": 0.299}, {"term": "nodematch.club", "estimate": 2.795}, {"term": "gwdeg.fixed.0.25", "estimate": 2.895}]}, {"round": 4, "action": "remove", "target": null, "term": "gwesp(decay=0.5, fixed=TRUE)", "terms": ["edges", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.3, "q_after": 1.55, "pbic": 61.7, "density_rel_error": 0.05, "residual": ["degree", "degree4", 5, 2.7, 1.55], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.721}, {"term": "nodematch.club", "estimate": 3.25}, {"term": "gwdeg.fixed.0.5", "estimate": 1.618}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.228}, {"term": "gwesp.fixed.0.5", "estimate": 0.338}, {"term": "nodematch.club", "estimate": 2.648}, {"term": "gwdeg.fixed.0.5", "estimate": 2.41}], "pbic": 64.32, "density_obs": 0.273, "density_sim": 0.283, "density_rel_error": 0.037, "q": 1.3, "gof_rmse": 0.64, "gof_bins": 21, "residuals": [["degree", "degree4", 5, 2.88, 1.3], ["esp", "esp1", 12, 8.14, 1.23], ["distance", "3", 20, 13.94, 1.12]], "eligible": true, "reason": "eligible", "runtime": 2}}, "mcmle": {"estimator": "mcmle", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.25}, {"term": "gwesp.fixed.0.5", "estimate": 0.31}, {"term": "nodematch.club", "estimate": 2.716}, {"term": "gwdeg.fixed.0.5", "estimate": 2.571}], "pbic": 64.32, "density_obs": 0.273, "density_sim": 0.271, "density_rel_error": 0.007, "q": 1.63, "gof_rmse": 0.66, "gof_bins": 23, "residuals": [["degree", "degree4", 5, 2.66, 1.63], ["distance", "3", 20, 12.56, 1.36], ["esp", "esp1", 12, 7.93, 1.32]], "eligible": true, "reason": "eligible", "runtime": 1}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"grade\")", "absdiff(\"activity\")"], "coefficients": [{"term": "edges", "estimate": -1.665}, {"term": "gwesp.fixed.0.5", "estimate": 0.502}, {"term": "nodematch.grade", "estimate": 0.37}, {"term": "absdiff.activity", "estimate": -0.144}], "pbic": 78.38, "density_obs": 0.273, "density_sim": 0.264, "density_rel_error": 0.033, "q": 2.6, "gof_rmse": 0.96, "gof_bins": 23, "residuals": [["degree", "degree4", 5, 1.93, 2.6], ["distance", "3", 20, 9.57, 2.15], ["esp", "esp1", 12, 7.54, 1.38]], "eligible": true, "reason": "eligible", "runtime": 1}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": false, "reason": "fit failed: Unconstrained MCMC sampling did not mix at all. Optimization cannot continue.", "runtime": 232.3}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.981}], "pbic": 81.54, "density_obs": 0.273, "density_sim": 0.274, "density_rel_error": 0.006, "q": 1.74, "gof_rmse": 0.83, "gof_bins": 22, "residuals": [["distance", "3", 20, 12.83, 1.74], ["esp", "esp0", 3, 8.31, -1.7], ["degree", "degree4", 5, 2.33, 1.67]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 1", "initial_q": 1.63, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"grade\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"grade\")"], "eligible": true, "q_before": 1.63, "q_after": 1.79, "pbic": 68.07, "density_rel_error": 0.022, "residual": ["degree", "degree4", 5, 2.52, 1.79], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.508}, {"term": "gwesp.fixed.0.5", "estimate": 0.316}, {"term": "nodematch.club", "estimate": 2.851}, {"term": "gwdeg.fixed.0.5", "estimate": 2.324}, {"term": "nodematch.grade", "estimate": 0.761}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"activity\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"activity\")"], "eligible": true, "q_before": 1.63, "q_after": 1.86, "pbic": 67.7, "density_rel_error": 0.031, "residual": ["degree", "degree4", 5, 2.42, 1.86], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.801}, {"term": "gwesp.fixed.0.5", "estimate": 0.338}, {"term": "nodematch.club", "estimate": 2.778}, {"term": "gwdeg.fixed.0.5", "estimate": 2.898}, {"term": "absdiff.activity", "estimate": -0.366}]}, {"round": 3, "action": "replace", "target": "gwdegree(decay=0.5, fixed=TRUE)", "term": "gwdegree(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.25, fixed=TRUE)"], "eligible": true, "q_before": 1.63, "q_after": 1.7, "pbic": 64.29, "density_rel_error": 0.044, "residual": ["degree", "degree4", 5, 2.64, 1.7], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.803}, {"term": "gwesp.fixed.0.5", "estimate": 0.253}, {"term": "nodematch.club", "estimate": 2.8}, {"term": "gwdeg.fixed.0.25", "estimate": 2.831}]}, {"round": 4, "action": "remove", "target": null, "term": "gwesp(decay=0.5, fixed=TRUE)", "terms": ["edges", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.63, "q_after": 1.76, "pbic": 61.7, "density_rel_error": 0.022, "residual": ["degree", "degree4", 5, 2.43, 1.76], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.739}, {"term": "nodematch.club", "estimate": 3.299}, {"term": "gwdeg.fixed.0.5", "estimate": 1.422}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.25}, {"term": "gwesp.fixed.0.5", "estimate": 0.31}, {"term": "nodematch.club", "estimate": 2.716}, {"term": "gwdeg.fixed.0.5", "estimate": 2.571}], "pbic": 64.32, "density_obs": 0.273, "density_sim": 0.271, "density_rel_error": 0.007, "q": 1.63, "gof_rmse": 0.66, "gof_bins": 23, "residuals": [["degree", "degree4", 5, 2.66, 1.63], ["distance", "3", 20, 12.56, 1.36], ["esp", "esp1", 12, 7.93, 1.32]], "eligible": true, "reason": "eligible", "runtime": 1}}, "mple": {"estimator": "mple", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.262}, {"term": "gwesp.fixed.0.5", "estimate": 0.456}, {"term": "nodematch.club", "estimate": 2.422}, {"term": "gwdeg.fixed.0.5", "estimate": 1.888}], "pbic": 64.32, "density_obs": 0.273, "density_sim": 0.275, "density_rel_error": 0.007, "q": 1.6, "gof_rmse": 0.67, "gof_bins": 22, "residuals": [["degree", "degree4", 5, 2.46, 1.6], ["esp", "esp1", 12, 7.69, 1.37], ["distance", "3", 20, 11.99, 1.36]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"grade\")", "absdiff(\"activity\")"], "coefficients": [{"term": "edges", "estimate": -2.182}, {"term": "gwesp.fixed.0.5", "estimate": 0.855}, {"term": "nodematch.grade", "estimate": 0.305}, {"term": "absdiff.activity", "estimate": -0.208}], "pbic": 78.38, "density_obs": 0.273, "density_sim": 0.222, "density_rel_error": 0.187, "q": 2.72, "gof_rmse": 1.16, "gof_bins": 22, "residuals": [["distance", "3", 20, 6.97, 2.72], ["degree", "degree4", 5, 1.76, 2.68], ["esp", "esp1", 12, 5.38, 1.99]], "eligible": true, "reason": "eligible", "runtime": 0.5}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -9.679}, {"term": "gwesp.fixed.0.5", "estimate": 0.308}, {"term": "gwdsp.fixed.0.5", "estimate": 1.267}, {"term": "nodematch.club", "estimate": 4.029}, {"term": "gwdeg.fixed.0.5", "estimate": 5.422}], "pbic": 62.96, "density_obs": 0.273, "density_sim": 0.268, "density_rel_error": 0.019, "q": 18.19, "gof_rmse": 6.84, "gof_bins": 15, "residuals": [["distance", "3", 20, 0.19, 18.19], ["distance", "2", 21, 48.21, -16.11], ["degree", "degree4", 5, 0.31, 8.33]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.981}], "pbic": 81.54, "density_obs": 0.273, "density_sim": 0.274, "density_rel_error": 0.006, "q": 1.74, "gof_rmse": 0.83, "gof_bins": 22, "residuals": [["distance", "3", 20, 12.83, 1.74], ["esp", "esp0", 3, 8.31, -1.7], ["degree", "degree4", 5, 2.33, 1.67]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 1", "initial_q": 1.6, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"grade\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"grade\")"], "eligible": true, "q_before": 1.6, "q_after": 2.04, "pbic": 68.07, "density_rel_error": 0.048, "residual": ["degree", "degree4", 5, 2.48, 2.04], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.326}, {"term": "gwesp.fixed.0.5", "estimate": 0.414}, {"term": "nodematch.club", "estimate": 2.634}, {"term": "gwdeg.fixed.0.5", "estimate": 1.408}, {"term": "nodematch.grade", "estimate": 0.589}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"activity\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"activity\")"], "eligible": true, "q_before": 1.6, "q_after": 1.77, "pbic": 67.7, "density_rel_error": 0.083, "residual": ["degree", "degree4", 5, 2.43, 1.77], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.749}, {"term": "gwesp.fixed.0.5", "estimate": 0.449}, {"term": "nodematch.club", "estimate": 2.51}, {"term": "gwdeg.fixed.0.5", "estimate": 1.783}, {"term": "absdiff.activity", "estimate": -0.273}]}, {"round": 3, "action": "replace", "target": "gwdegree(decay=0.5, fixed=TRUE)", "term": "gwdegree(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"club\")", "gwdegree(decay=0.25, fixed=TRUE)"], "eligible": true, "q_before": 1.6, "q_after": 1.86, "pbic": 64.29, "density_rel_error": 0.07, "residual": ["degree", "degree4", 5, 2.22, 1.86], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.058}, {"term": "gwesp.fixed.0.5", "estimate": 0.462}, {"term": "nodematch.club", "estimate": 2.448}, {"term": "gwdeg.fixed.0.25", "estimate": 2.341}]}, {"round": 4, "action": "remove", "target": null, "term": "gwesp(decay=0.5, fixed=TRUE)", "terms": ["edges", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.6, "q_after": 1.42, "pbic": 61.7, "density_rel_error": 0.065, "residual": ["esp", "esp1", 12, 7.54, 1.42], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -2.521}, {"term": "nodematch.club", "estimate": 3.311}, {"term": "gwdeg.fixed.0.5", "estimate": 0.694}]}], "final": {"label": "Final model", "terms": ["edges", "nodematch(\"club\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -2.521}, {"term": "nodematch.club", "estimate": 3.311}, {"term": "gwdeg.fixed.0.5", "estimate": 0.694}], "pbic": 61.7, "density_obs": 0.273, "density_sim": 0.29, "density_rel_error": 0.065, "q": 1.42, "gof_rmse": 0.6, "gof_bins": 23, "residuals": [["esp", "esp1", 12, 7.54, 1.42], ["degree", "degree4", 5, 2.76, 1.3], ["distance", "3", 20, 14.38, 1.17]], "eligible": true, "reason": "eligible", "runtime": 0.5}}}, "lab": {"sa": {"estimator": "sa", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.925}, {"term": "gwesp.fixed.0.5", "estimate": -0.39}, {"term": "nodematch.area", "estimate": 4.077}, {"term": "gwdeg.fixed.0.5", "estimate": 14.076}], "pbic": 53.04, "density_obs": 0.303, "density_sim": 0.305, "density_rel_error": 0.005, "q": 1.66, "gof_rmse": 0.61, "gof_bins": 16, "residuals": [["esp", "esp1", 12, 7.12, 1.66], ["esp", "esp2", 3, 6.97, -0.95], ["degree", "degree5", 1, 0.49, 0.81]], "eligible": true, "reason": "eligible", "runtime": 1.9}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"role\")", "absdiff(\"seniority\")"], "coefficients": [{"term": "edges", "estimate": -0.197}, {"term": "gwesp.fixed.0.5", "estimate": 0.129}, {"term": "nodematch.role", "estimate": -0.507}, {"term": "absdiff.seniority", "estimate": -0.249}], "pbic": 93.4, "density_obs": 0.303, "density_sim": 0.307, "density_rel_error": 0.012, "q": 2.52, "gof_rmse": 0.78, "gof_bins": 24, "residuals": [["degree", "degree3", 7, 3.09, 2.52], ["distance", "3", 18, 11.61, 1.38], ["esp", "esp1", 12, 8.01, 1.34]], "eligible": true, "reason": "eligible", "runtime": 2.3}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -10.622}, {"term": "gwesp.fixed.0.5", "estimate": -0.792}, {"term": "gwdsp.fixed.0.5", "estimate": 1.182}, {"term": "nodematch.area", "estimate": 6.286}, {"term": "gwdeg.fixed.0.5", "estimate": 22.064}], "pbic": 52.97, "density_obs": 0.303, "density_sim": 0.295, "density_rel_error": 0.027, "q": 1.08, "gof_rmse": 0.44, "gof_bins": 15, "residuals": [["esp", "esp1", 12, 8.7, 1.08], ["esp", "esp2", 3, 5.25, -0.65], ["degree", "degree4", 3, 3.76, -0.55]], "eligible": true, "reason": "eligible", "runtime": 2.3}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.833}], "pbic": 85.16, "density_obs": 0.303, "density_sim": 0.292, "density_rel_error": 0.037, "q": 2.61, "gof_rmse": 0.83, "gof_bins": 23, "residuals": [["degree", "degree3", 7, 2.96, 2.61], ["distance", "3", 18, 11.35, 1.52], ["esp", "esp1", 12, 8.04, 1.31]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 3", "initial_q": 1.08, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"role\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"role\")"], "eligible": true, "q_before": 1.08, "q_after": 1.12, "pbic": 48.39, "density_rel_error": 0.018, "residual": ["esp", "esp1", 12, 8.1, 1.12], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -9.75}, {"term": "gwesp.fixed.0.5", "estimate": -0.575}, {"term": "gwdsp.fixed.0.5", "estimate": 0.886}, {"term": "nodematch.area", "estimate": 6.068}, {"term": "gwdeg.fixed.0.5", "estimate": 18.802}, {"term": "nodematch.role", "estimate": 1.947}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"seniority\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"seniority\")"], "eligible": true, "q_before": 1.08, "q_after": 1.03, "pbic": 45.43, "density_rel_error": 0.007, "residual": ["esp", "esp1", 12, 9.05, 1.03], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -7.819}, {"term": "gwesp.fixed.0.5", "estimate": -0.677}, {"term": "gwdsp.fixed.0.5", "estimate": 0.988}, {"term": "nodematch.area", "estimate": 7.454}, {"term": "gwdeg.fixed.0.5", "estimate": 18.8}, {"term": "absdiff.seniority", "estimate": -0.767}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"seniority\")"], "eligible": true, "q_before": 1.03, "q_after": 1.21, "pbic": 46.99, "density_rel_error": 0.018, "residual": ["esp", "esp1", 12, 8.74, 1.21], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -8.642}, {"term": "gwesp.fixed.0.25", "estimate": -0.575}, {"term": "gwdsp.fixed.0.5", "estimate": 1.057}, {"term": "nodematch.area", "estimate": 7.144}, {"term": "gwdeg.fixed.0.5", "estimate": 20.881}, {"term": "absdiff.seniority", "estimate": -0.768}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "absdiff(\"seniority\")"], "eligible": true, "q_before": 1.03, "q_after": 1.65, "pbic": 52.98, "density_rel_error": 0.015, "residual": ["esp", "esp1", 12, 7.14, 1.65], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -1.239}, {"term": "gwesp.fixed.0.5", "estimate": -0.735}, {"term": "gwdsp.fixed.0.5", "estimate": 0.237}, {"term": "nodematch.area", "estimate": 7.704}, {"term": "absdiff.seniority", "estimate": -0.831}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"seniority\")"], "coefficients": [{"term": "edges", "estimate": -7.819}, {"term": "gwesp.fixed.0.5", "estimate": -0.677}, {"term": "gwdsp.fixed.0.5", "estimate": 0.988}, {"term": "nodematch.area", "estimate": 7.454}, {"term": "gwdeg.fixed.0.5", "estimate": 18.8}, {"term": "absdiff.seniority", "estimate": -0.767}], "pbic": 45.43, "density_obs": 0.303, "density_sim": 0.305, "density_rel_error": 0.007, "q": 1.03, "gof_rmse": 0.41, "gof_bins": 16, "residuals": [["esp", "esp1", 12, 9.05, 1.03], ["degree", "degree3", 7, 6.03, 0.56], ["esp", "esp2", 3, 5.03, -0.56]], "eligible": true, "reason": "eligible", "runtime": 2.2}}, "mcmle": {"estimator": "mcmle", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.741}, {"term": "gwesp.fixed.0.5", "estimate": -0.409}, {"term": "nodematch.area", "estimate": 4.204}, {"term": "gwdeg.fixed.0.5", "estimate": 12.775}], "pbic": 53.04, "density_obs": 0.303, "density_sim": 0.299, "density_rel_error": 0.012, "q": 1.52, "gof_rmse": 0.56, "gof_bins": 17, "residuals": [["esp", "esp1", 12, 6.81, 1.52], ["esp", "esp2", 3, 6.84, -0.91], ["degree", "degree5", 1, 0.51, 0.71]], "eligible": true, "reason": "eligible", "runtime": 1}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"role\")", "absdiff(\"seniority\")"], "coefficients": [{"term": "edges", "estimate": -0.173}, {"term": "gwesp.fixed.0.5", "estimate": 0.099}, {"term": "nodematch.role", "estimate": -0.492}, {"term": "absdiff.seniority", "estimate": -0.24}], "pbic": 93.4, "density_obs": 0.303, "density_sim": 0.292, "density_rel_error": 0.037, "q": 2.46, "gof_rmse": 0.83, "gof_bins": 24, "residuals": [["degree", "degree3", 7, 2.9, 2.46], ["esp", "esp1", 12, 7.56, 1.59], ["distance", "3", 18, 11.74, 1.54]], "eligible": true, "reason": "eligible", "runtime": 1.1}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -10.864}, {"term": "gwesp.fixed.0.5", "estimate": -0.829}, {"term": "gwdsp.fixed.0.5", "estimate": 1.234}, {"term": "nodematch.area", "estimate": 6.439}, {"term": "gwdeg.fixed.0.5", "estimate": 21.488}], "pbic": 52.97, "density_obs": 0.303, "density_sim": 0.305, "density_rel_error": 0.005, "q": 1.08, "gof_rmse": 0.38, "gof_bins": 17, "residuals": [["esp", "esp1", 12, 9.04, 1.08], ["esp", "esp2", 3, 4.78, -0.61], ["degree", "degree2", 1, 1.43, -0.4]], "eligible": true, "reason": "eligible", "runtime": 1.4}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.833}], "pbic": 85.16, "density_obs": 0.303, "density_sim": 0.292, "density_rel_error": 0.037, "q": 2.61, "gof_rmse": 0.83, "gof_bins": 23, "residuals": [["degree", "degree3", 7, 2.96, 2.61], ["distance", "3", 18, 11.35, 1.52], ["esp", "esp1", 12, 8.04, 1.31]], "eligible": true, "reason": "eligible", "runtime": 0.4}], "selected": "Candidate 3", "initial_q": 1.08, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"role\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"role\")"], "eligible": true, "q_before": 1.08, "q_after": 0.96, "pbic": 48.39, "density_rel_error": 0.018, "residual": ["esp", "esp1", 12, 8.91, 0.96], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -11.483}, {"term": "gwesp.fixed.0.5", "estimate": -0.714}, {"term": "gwdsp.fixed.0.5", "estimate": 1.13}, {"term": "nodematch.area", "estimate": 6.992}, {"term": "gwdeg.fixed.0.5", "estimate": 21.297}, {"term": "nodematch.role", "estimate": 1.932}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"seniority\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"role\")", "absdiff(\"seniority\")"], "eligible": false, "q_before": 0.96, "q_after": null, "pbic": null, "density_rel_error": null, "residual": null, "accepted": false, "reason": "fit failed: Unconstrained MCMC sampling did not mix at all. Optimization cannot continue.", "coefficients": null}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"role\")"], "eligible": true, "q_before": 0.96, "q_after": 1, "pbic": 50.18, "density_rel_error": 0.015, "residual": ["esp", "esp1", 12, 8.93, 1], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -11.408}, {"term": "gwesp.fixed.0.25", "estimate": -0.505}, {"term": "gwdsp.fixed.0.5", "estimate": 1.076}, {"term": "nodematch.area", "estimate": 6.1}, {"term": "gwdeg.fixed.0.5", "estimate": 22.052}, {"term": "nodematch.role", "estimate": 1.913}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "nodematch(\"role\")"], "eligible": true, "q_before": 0.96, "q_after": 1.67, "pbic": 58.55, "density_rel_error": 0.022, "residual": ["degree", "degree3", 7, 4.13, 1.67], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -4.352}, {"term": "gwesp.fixed.0.5", "estimate": -0.745}, {"term": "gwdsp.fixed.0.5", "estimate": 0.334}, {"term": "nodematch.area", "estimate": 6.997}, {"term": "nodematch.role", "estimate": 2.115}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"role\")"], "coefficients": [{"term": "edges", "estimate": -11.483}, {"term": "gwesp.fixed.0.5", "estimate": -0.714}, {"term": "gwdsp.fixed.0.5", "estimate": 1.13}, {"term": "nodematch.area", "estimate": 6.992}, {"term": "gwdeg.fixed.0.5", "estimate": 21.297}, {"term": "nodematch.role", "estimate": 1.932}], "pbic": 48.39, "density_obs": 0.303, "density_sim": 0.297, "density_rel_error": 0.018, "q": 0.96, "gof_rmse": 0.4, "gof_bins": 14, "residuals": [["esp", "esp1", 12, 8.91, 0.96], ["esp", "esp2", 3, 5.13, -0.61], ["degree", "degree5", 1, 0.61, 0.55]], "eligible": true, "reason": "eligible", "runtime": 1.7}}, "mple": {"estimator": "mple", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -4.766}, {"term": "gwesp.fixed.0.5", "estimate": -0.398}, {"term": "nodematch.area", "estimate": 3.851}, {"term": "gwdeg.fixed.0.5", "estimate": 19.444}], "pbic": 53.04, "density_obs": 0.303, "density_sim": 0.297, "density_rel_error": 0.018, "q": 1.51, "gof_rmse": 0.51, "gof_bins": 16, "residuals": [["esp", "esp1", 12, 6.76, 1.51], ["esp", "esp2", 3, 5.92, -0.61], ["esp", "esp0", 5, 7.29, -0.6]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"role\")", "absdiff(\"seniority\")"], "coefficients": [{"term": "edges", "estimate": -0.674}, {"term": "gwesp.fixed.0.5", "estimate": 0.289}, {"term": "nodematch.role", "estimate": -0.308}, {"term": "absdiff.seniority", "estimate": -0.211}], "pbic": 93.4, "density_obs": 0.303, "density_sim": 0.288, "density_rel_error": 0.048, "q": 3.23, "gof_rmse": 0.92, "gof_bins": 24, "residuals": [["degree", "degree3", 7, 2.71, 3.23], ["distance", "3", 18, 10.75, 1.67], ["esp", "esp1", 12, 7.85, 1.23]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -11.332}, {"term": "gwesp.fixed.0.5", "estimate": -1.016}, {"term": "gwdsp.fixed.0.5", "estimate": 1.386}, {"term": "nodematch.area", "estimate": 6.594}, {"term": "gwdeg.fixed.0.5", "estimate": 22.698}], "pbic": 52.97, "density_obs": 0.303, "density_sim": 0.309, "density_rel_error": 0.018, "q": 1.04, "gof_rmse": 0.47, "gof_bins": 14, "residuals": [["esp", "esp1", 12, 9.19, 1.04], ["distance", "2", 25, 27.87, -0.72], ["esp", "esp0", 5, 7.08, -0.71]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.833}], "pbic": 85.16, "density_obs": 0.303, "density_sim": 0.292, "density_rel_error": 0.037, "q": 2.61, "gof_rmse": 0.83, "gof_bins": 23, "residuals": [["degree", "degree3", 7, 2.96, 2.61], ["distance", "3", 18, 11.35, 1.52], ["esp", "esp1", 12, 8.04, 1.31]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 3", "initial_q": 1.04, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"role\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"role\")"], "eligible": true, "q_before": 1.04, "q_after": 5.04, "pbic": 48.39, "density_rel_error": 0.027, "residual": ["distance", "4", 3, 0.16, 5.04], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -19.6}, {"term": "gwesp.fixed.0.5", "estimate": -1.492}, {"term": "gwdsp.fixed.0.5", "estimate": 2.344}, {"term": "nodematch.area", "estimate": 10.5}, {"term": "gwdeg.fixed.0.5", "estimate": 33.152}, {"term": "nodematch.role", "estimate": 4.026}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"seniority\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"seniority\")"], "eligible": true, "q_before": 1.04, "q_after": 2.33, "pbic": 45.43, "density_rel_error": 0.013, "residual": ["distance", "4", 3, 0.36, 2.33], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -11.861}, {"term": "gwesp.fixed.0.5", "estimate": -1.894}, {"term": "gwdsp.fixed.0.5", "estimate": 1.409}, {"term": "nodematch.area", "estimate": 11.405}, {"term": "gwdeg.fixed.0.5", "estimate": 44.087}, {"term": "absdiff.seniority", "estimate": -1.142}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.04, "q_after": 0.97, "pbic": 54.79, "density_rel_error": 0.003, "residual": ["esp", "esp1", 12, 9.18, 0.97], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -10.699}, {"term": "gwesp.fixed.0.25", "estimate": -0.658}, {"term": "gwdsp.fixed.0.5", "estimate": 1.209}, {"term": "nodematch.area", "estimate": 5.116}, {"term": "gwdeg.fixed.0.5", "estimate": 23.18}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")"], "eligible": true, "q_before": 0.97, "q_after": 1.59, "pbic": 62.91, "density_rel_error": 0.03, "residual": ["esp", "esp1", 12, 6.69, 1.59], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.655}, {"term": "gwesp.fixed.0.25", "estimate": -0.561}, {"term": "gwdsp.fixed.0.5", "estimate": 0.43}, {"term": "nodematch.area", "estimate": 5.316}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"area\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -10.699}, {"term": "gwesp.fixed.0.25", "estimate": -0.658}, {"term": "gwdsp.fixed.0.5", "estimate": 1.209}, {"term": "nodematch.area", "estimate": 5.116}, {"term": "gwdeg.fixed.0.5", "estimate": 23.18}], "pbic": 54.79, "density_obs": 0.303, "density_sim": 0.302, "density_rel_error": 0.003, "q": 0.97, "gof_rmse": 0.4, "gof_bins": 16, "residuals": [["esp", "esp1", 12, 9.18, 0.97], ["esp", "esp0", 5, 6.75, -0.67], ["distance", "2", 25, 27.47, -0.54]], "eligible": true, "reason": "eligible", "runtime": 0.6}}}, "neighborhood": {"sa": {"estimator": "sa", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -4.222}, {"term": "gwesp.fixed.0.5", "estimate": -0.131}, {"term": "nodematch.block", "estimate": 3.835}, {"term": "gwdeg.fixed.0.5", "estimate": 8.522}], "pbic": 49.18, "density_obs": 0.273, "density_sim": 0.272, "density_rel_error": 0.002, "q": 1.46, "gof_rmse": 0.63, "gof_bins": 19, "residuals": [["esp", "esp1", 12, 7.31, 1.46], ["distance", "3", 21, 13.75, 1.24], ["distance", "5", 0, 2.51, -0.81]], "eligible": true, "reason": "eligible", "runtime": 1.8}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")", "absdiff(\"tenure_years\")"], "coefficients": [{"term": "edges", "estimate": -2.192}, {"term": "gwesp.fixed.0.5", "estimate": 0.387}, {"term": "nodematch.tenure_group", "estimate": 0.146}, {"term": "absdiff.tenure_years", "estimate": 0.086}], "pbic": 78.67, "density_obs": 0.273, "density_sim": 0.283, "density_rel_error": 0.039, "q": 2.75, "gof_rmse": 1.03, "gof_bins": 25, "residuals": [["degree", "degree3", 6, 2.25, 2.75], ["distance", "3", 21, 10.42, 2.43], ["distance", "4", 9, 3.14, 1.77]], "eligible": true, "reason": "eligible", "runtime": 2.3}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": false, "reason": "fit failed: Matrix 'x' has negative elements on the diagonal.", "runtime": 0.6}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.981}], "pbic": 81.54, "density_obs": 0.273, "density_sim": 0.265, "density_rel_error": 0.028, "q": 1.9, "gof_rmse": 0.91, "gof_bins": 25, "residuals": [["degree", "degree3", 6, 3.21, 1.9], ["distance", "3", 21, 13.14, 1.78], ["distance", "4", 9, 3.41, 1.75]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 1", "initial_q": 1.46, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"tenure_group\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")"], "eligible": true, "q_before": 1.46, "q_after": 1.44, "pbic": 53.32, "density_rel_error": 0.006, "residual": ["esp", "esp1", 12, 6.85, 1.44], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -4.361}, {"term": "gwesp.fixed.0.5", "estimate": -0.184}, {"term": "nodematch.block", "estimate": 4.166}, {"term": "gwdeg.fixed.0.5", "estimate": 8.441}, {"term": "nodematch.tenure_group", "estimate": 0.561}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"tenure_years\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")", "absdiff(\"tenure_years\")"], "eligible": true, "q_before": 1.44, "q_after": 1.35, "pbic": 53.94, "density_rel_error": 0.019, "residual": ["esp", "esp1", 12, 7.08, 1.35], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -6.423}, {"term": "gwesp.fixed.0.5", "estimate": -0.193}, {"term": "nodematch.block", "estimate": 4.383}, {"term": "gwdeg.fixed.0.5", "estimate": 8.864}, {"term": "nodematch.tenure_group", "estimate": 2.113}, {"term": "absdiff.tenure_years", "estimate": 0.238}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")", "absdiff(\"tenure_years\")"], "eligible": true, "q_before": 1.35, "q_after": 1.25, "pbic": 53.3, "density_rel_error": 0.002, "residual": ["distance", "3", 21, 13.65, 1.25], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -6.291}, {"term": "gwesp.fixed.0.25", "estimate": 0.016}, {"term": "nodematch.block", "estimate": 3.735}, {"term": "gwdeg.fixed.0.5", "estimate": 8.546}, {"term": "nodematch.tenure_group", "estimate": 1.971}, {"term": "absdiff.tenure_years", "estimate": 0.216}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"block\")", "nodematch(\"tenure_group\")", "absdiff(\"tenure_years\")"], "eligible": true, "q_before": 1.25, "q_after": 1.39, "pbic": 57.65, "density_rel_error": 0.011, "residual": ["esp", "esp1", 12, 7.47, 1.39], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -4.593}, {"term": "gwesp.fixed.0.25", "estimate": -0.225}, {"term": "nodematch.block", "estimate": 5.125}, {"term": "nodematch.tenure_group", "estimate": 2.011}, {"term": "absdiff.tenure_years", "estimate": 0.214}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")", "absdiff(\"tenure_years\")"], "coefficients": [{"term": "edges", "estimate": -6.291}, {"term": "gwesp.fixed.0.25", "estimate": 0.016}, {"term": "nodematch.block", "estimate": 3.735}, {"term": "gwdeg.fixed.0.5", "estimate": 8.546}, {"term": "nodematch.tenure_group", "estimate": 1.971}, {"term": "absdiff.tenure_years", "estimate": 0.216}], "pbic": 53.3, "density_obs": 0.273, "density_sim": 0.273, "density_rel_error": 0.002, "q": 1.25, "gof_rmse": 0.58, "gof_bins": 19, "residuals": [["distance", "3", 21, 13.65, 1.25], ["esp", "esp1", 12, 8.27, 1.14], ["distance", "5", 0, 3.08, -0.93]], "eligible": true, "reason": "eligible", "runtime": 2.1}}, "mcmle": {"estimator": "mcmle", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -4.082}, {"term": "gwesp.fixed.0.5", "estimate": -0.192}, {"term": "nodematch.block", "estimate": 4.063}, {"term": "gwdeg.fixed.0.5", "estimate": 8.189}], "pbic": 49.18, "density_obs": 0.273, "density_sim": 0.271, "density_rel_error": 0.006, "q": 1.39, "gof_rmse": 0.61, "gof_bins": 18, "residuals": [["esp", "esp1", 12, 7.24, 1.39], ["distance", "3", 21, 14.05, 1.1], ["esp", "esp2", 3, 6.61, -0.85]], "eligible": true, "reason": "eligible", "runtime": 1.3}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")", "absdiff(\"tenure_years\")"], "coefficients": [{"term": "edges", "estimate": -2.307}, {"term": "gwesp.fixed.0.5", "estimate": 0.42}, {"term": "nodematch.tenure_group", "estimate": 0.227}, {"term": "absdiff.tenure_years", "estimate": 0.092}], "pbic": 78.67, "density_obs": 0.273, "density_sim": 0.258, "density_rel_error": 0.056, "q": 2.3, "gof_rmse": 1.02, "gof_bins": 24, "residuals": [["distance", "3", 21, 10.88, 2.3], ["degree", "degree3", 6, 2.58, 2.25], ["distance", "4", 9, 2.88, 2.04]], "eligible": true, "reason": "eligible", "runtime": 1}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": false, "reason": "fit failed: Unconstrained MCMC sampling did not mix at all. Optimization cannot continue.", "runtime": 0.4}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.981}], "pbic": 81.54, "density_obs": 0.273, "density_sim": 0.265, "density_rel_error": 0.028, "q": 1.9, "gof_rmse": 0.91, "gof_bins": 25, "residuals": [["degree", "degree3", 6, 3.21, 1.9], ["distance", "3", 21, 13.14, 1.78], ["distance", "4", 9, 3.41, 1.75]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 1", "initial_q": 1.39, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"tenure_group\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")"], "eligible": true, "q_before": 1.39, "q_after": 1.66, "pbic": 53.32, "density_rel_error": 0.009, "residual": ["esp", "esp1", 12, 6.33, 1.66], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -4.467}, {"term": "gwesp.fixed.0.5", "estimate": -0.211}, {"term": "nodematch.block", "estimate": 4.231}, {"term": "gwdeg.fixed.0.5", "estimate": 8.791}, {"term": "nodematch.tenure_group", "estimate": 0.538}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"tenure_years\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"tenure_years\")"], "eligible": true, "q_before": 1.39, "q_after": 1.34, "pbic": 51.7, "density_rel_error": 0.013, "residual": ["esp", "esp1", 12, 7.2, 1.34], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -4.51}, {"term": "gwesp.fixed.0.5", "estimate": -0.141}, {"term": "nodematch.block", "estimate": 3.837}, {"term": "gwdeg.fixed.0.5", "estimate": 8.516}, {"term": "absdiff.tenure_years", "estimate": 0.055}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"tenure_years\")"], "eligible": true, "q_before": 1.34, "q_after": 1.52, "pbic": 50.99, "density_rel_error": 0.007, "residual": ["distance", "3", 21, 12.38, 1.52], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -5.125}, {"term": "gwesp.fixed.0.25", "estimate": 0.185}, {"term": "nodematch.block", "estimate": 3}, {"term": "gwdeg.fixed.0.5", "estimate": 10.362}, {"term": "absdiff.tenure_years", "estimate": 0.066}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "absdiff(\"tenure_years\")"], "eligible": true, "q_before": 1.34, "q_after": 1.56, "pbic": 54.17, "density_rel_error": 0.046, "residual": ["esp", "esp1", 12, 6.31, 1.56], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.812}, {"term": "gwesp.fixed.0.5", "estimate": -0.433}, {"term": "nodematch.block", "estimate": 5.382}, {"term": "absdiff.tenure_years", "estimate": 0.057}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"tenure_years\")"], "coefficients": [{"term": "edges", "estimate": -4.51}, {"term": "gwesp.fixed.0.5", "estimate": -0.141}, {"term": "nodematch.block", "estimate": 3.837}, {"term": "gwdeg.fixed.0.5", "estimate": 8.516}, {"term": "absdiff.tenure_years", "estimate": 0.055}], "pbic": 51.7, "density_obs": 0.273, "density_sim": 0.276, "density_rel_error": 0.013, "q": 1.34, "gof_rmse": 0.62, "gof_bins": 19, "residuals": [["esp", "esp1", 12, 7.2, 1.34], ["distance", "3", 21, 13.4, 1.21], ["esp", "esp2", 3, 6.81, -0.81]], "eligible": true, "reason": "eligible", "runtime": 1.2}}, "mple": {"estimator": "mple", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -5.89}, {"term": "gwesp.fixed.0.5", "estimate": 0.081}, {"term": "nodematch.block", "estimate": 2.9}, {"term": "gwdeg.fixed.0.5", "estimate": 16.463}], "pbic": 49.18, "density_obs": 0.273, "density_sim": 0.27, "density_rel_error": 0.009, "q": 1.21, "gof_rmse": 0.64, "gof_bins": 18, "residuals": [["esp", "esp1", 12, 8.31, 1.21], ["distance", "3", 21, 14.11, 1.12], ["degree", "degree4", 3, 1.65, 1.06]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")", "absdiff(\"tenure_years\")"], "coefficients": [{"term": "edges", "estimate": -2.674}, {"term": "gwesp.fixed.0.5", "estimate": 0.822}, {"term": "nodematch.tenure_group", "estimate": 0.347}, {"term": "absdiff.tenure_years", "estimate": 0.039}], "pbic": 78.67, "density_obs": 0.273, "density_sim": 0.259, "density_rel_error": 0.05, "q": 3.09, "gof_rmse": 1.14, "gof_bins": 28, "residuals": [["distance", "3", 21, 6.91, 3.09], ["distance", "4", 9, 1.68, 3], ["degree", "degree3", 6, 2.06, 2.75]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -494.87}, {"term": "gwesp.fixed.0.5", "estimate": -22.595}, {"term": "gwdsp.fixed.0.5", "estimate": 73.562}, {"term": "nodematch.block", "estimate": 185.005}, {"term": "gwdeg.fixed.0.5", "estimate": 585.741}], "pbic": 32.41, "density_obs": 0.273, "density_sim": 0.301, "density_rel_error": 0.102, "eligible": false, "reason": "no finite GOF residual", "runtime": 0.6}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -0.981}], "pbic": 81.54, "density_obs": 0.273, "density_sim": 0.265, "density_rel_error": 0.028, "q": 1.9, "gof_rmse": 0.91, "gof_bins": 25, "residuals": [["degree", "degree3", 6, 3.21, 1.9], ["distance", "3", 21, 13.14, 1.78], ["distance", "4", 9, 3.41, 1.75]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 1", "initial_q": 1.21, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodematch(\"tenure_group\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"tenure_group\")"], "eligible": true, "q_before": 1.21, "q_after": 1.27, "pbic": 53.32, "density_rel_error": 0.007, "residual": ["esp", "esp1", 12, 7.92, 1.27], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -5.893}, {"term": "gwesp.fixed.0.5", "estimate": 0.081}, {"term": "nodematch.block", "estimate": 2.976}, {"term": "gwdeg.fixed.0.5", "estimate": 16.035}, {"term": "nodematch.tenure_group", "estimate": 0.256}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"tenure_years\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)", "absdiff(\"tenure_years\")"], "eligible": true, "q_before": 1.21, "q_after": 1.33, "pbic": 51.7, "density_rel_error": 0.007, "residual": ["esp", "esp1", 12, 7.53, 1.33], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -7.754}, {"term": "gwesp.fixed.0.5", "estimate": 0.091}, {"term": "nodematch.block", "estimate": 2.588}, {"term": "gwdeg.fixed.0.5", "estimate": 20.637}, {"term": "absdiff.tenure_years", "estimate": 0.163}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.21, "q_after": 1.22, "pbic": 48.52, "density_rel_error": 0.006, "residual": ["distance", "3", 21, 13.34, 1.22], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -6.358}, {"term": "gwesp.fixed.0.25", "estimate": 0.366}, {"term": "nodematch.block", "estimate": 2.231}, {"term": "gwdeg.fixed.0.5", "estimate": 17.972}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")"], "eligible": true, "q_before": 1.21, "q_after": 1.71, "pbic": 50.52, "density_rel_error": 0.007, "residual": ["esp", "esp1", 12, 6.66, 1.71], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -2.516}, {"term": "gwesp.fixed.0.5", "estimate": -0.335}, {"term": "nodematch.block", "estimate": 5.173}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"block\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -5.89}, {"term": "gwesp.fixed.0.5", "estimate": 0.081}, {"term": "nodematch.block", "estimate": 2.9}, {"term": "gwdeg.fixed.0.5", "estimate": 16.463}], "pbic": 49.18, "density_obs": 0.273, "density_sim": 0.27, "density_rel_error": 0.009, "q": 1.21, "gof_rmse": 0.64, "gof_bins": 18, "residuals": [["esp", "esp1", 12, 8.31, 1.21], ["distance", "3", 21, 14.11, 1.12], ["degree", "degree4", 3, 1.65, 1.06]], "eligible": true, "reason": "eligible", "runtime": 0.6}}}, "office": {"sa": {"estimator": "sa", "candidates": [{"label": "Candidate 1", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -1.682}, {"term": "mutual", "estimate": -1.493}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.305}, {"term": "nodematch.department", "estimate": 7.162}, {"term": "gwideg.fixed.0.5", "estimate": -6.205}], "pbic": 106.69, "density_obs": 0.181, "density_sim": 0.178, "density_rel_error": 0.018, "q": 1.89, "gof_rmse": 0.66, "gof_bins": 30, "residuals": [["odegree", "odegree3", 7, 3.92, 1.89], ["odegree", "odegree4", 0, 1.5, -1.35], ["idegree", "idegree6", 1, 0.31, 1.27]], "eligible": true, "reason": "eligible", "runtime": 2.2}, {"label": "Candidate 2", "terms": ["edges", "mutual", "nodematch(\"level\")", "nodeicov(\"tenure\")"], "coefficients": [{"term": "edges", "estimate": -3.098}, {"term": "mutual", "estimate": 0.972}, {"term": "nodematch.level", "estimate": 0.14}, {"term": "nodeicov.tenure", "estimate": 0.269}], "pbic": 167.71, "density_obs": 0.181, "density_sim": 0.182, "density_rel_error": 0.003, "q": 2.1, "gof_rmse": 0.8, "gof_bins": 36, "residuals": [["odegree", "odegree3", 7, 3.29, 2.1], ["distance", "7", 2, 0.34, 1.68], ["idegree", "idegree1", 1, 3.61, -1.6]], "eligible": true, "reason": "eligible", "runtime": 1.7}, {"label": "Candidate 3", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "nodeifactor(\"level\")"], "coefficients": [{"term": "edges", "estimate": -6.752}, {"term": "mutual", "estimate": -1.101}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.101}, {"term": "nodematch.department", "estimate": 6.805}, {"term": "nodeifactor.level.Mid", "estimate": 3.751}, {"term": "nodeifactor.level.Senior", "estimate": 5.795}], "pbic": 97.02, "density_obs": 0.181, "density_sim": 0.18, "density_rel_error": 0.008, "q": 2, "gof_rmse": 0.67, "gof_bins": 32, "residuals": [["odegree", "odegree3", 7, 4.07, 2], ["odegree", "odegree4", 0, 1.95, -1.91], ["idegree", "idegree1", 1, 2.69, -1.33]], "eligible": true, "reason": "eligible", "runtime": 2.1}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -1.507}], "pbic": 177.52, "density_obs": 0.181, "density_sim": 0.178, "density_rel_error": 0.018, "q": 3.13, "gof_rmse": 1.28, "gof_bins": 33, "residuals": [["idegree", "idegree0", 4, 0.9, 3.13], ["esp", "esp.OTP0", 16, 22.32, -2.68], ["odegree", "odegree3", 7, 3.11, 2.56]], "eligible": true, "reason": "eligible", "runtime": 0.6}], "selected": "Candidate 1", "initial_q": 1.89, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodeifactor(\"level\")", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "nodeifactor(\"level\")"], "eligible": true, "q_before": 1.89, "q_after": 2.05, "pbic": 100.54, "density_rel_error": 0.001, "residual": ["odegree", "odegree3", 7, 3.75, 2.05], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -4.739}, {"term": "mutual", "estimate": -1.224}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.336}, {"term": "nodematch.department", "estimate": 7.682}, {"term": "gwideg.fixed.0.5", "estimate": -3.101}, {"term": "nodeifactor.level.Mid", "estimate": 1.992}, {"term": "nodeifactor.level.Senior", "estimate": 3.892}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"tenure\")", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "absdiff(\"tenure\")"], "eligible": true, "q_before": 1.89, "q_after": 2.1, "pbic": 110.97, "density_rel_error": 0.016, "residual": ["odegree", "odegree3", 7, 3.5, 2.1], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -1.674}, {"term": "mutual", "estimate": -1.511}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.308}, {"term": "nodematch.department", "estimate": 7.171}, {"term": "gwideg.fixed.0.5", "estimate": -6.182}, {"term": "absdiff.tenure", "estimate": -0.003}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "mutual", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.89, "q_after": 2.29, "pbic": 108.55, "density_rel_error": 0.007, "residual": ["odegree", "odegree3", 7, 3.57, 2.29], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -1.679}, {"term": "mutual", "estimate": -1.403}, {"term": "gwesp.OTP.fixed.0.25", "estimate": -1.245}, {"term": "nodematch.department", "estimate": 6.618}, {"term": "gwideg.fixed.0.5", "estimate": -5.836}]}, {"round": 4, "action": "remove", "target": null, "term": "gwidegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")"], "eligible": true, "q_before": 1.89, "q_after": 4.87, "pbic": 135.94, "density_rel_error": 0.022, "residual": ["idegree", "idegree6", 1, 0.04, 4.87], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.029}, {"term": "mutual", "estimate": -1.346}, {"term": "gwesp.OTP.fixed.0.5", "estimate": 0.044}, {"term": "nodematch.department", "estimate": 3.749}]}], "final": {"label": "Final model", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -1.682}, {"term": "mutual", "estimate": -1.493}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.305}, {"term": "nodematch.department", "estimate": 7.162}, {"term": "gwideg.fixed.0.5", "estimate": -6.205}], "pbic": 106.69, "density_obs": 0.181, "density_sim": 0.178, "density_rel_error": 0.018, "q": 1.89, "gof_rmse": 0.66, "gof_bins": 30, "residuals": [["odegree", "odegree3", 7, 3.92, 1.89], ["odegree", "odegree4", 0, 1.5, -1.35], ["idegree", "idegree6", 1, 0.31, 1.27]], "eligible": true, "reason": "eligible", "runtime": 2.2}}, "mcmle": {"estimator": "mcmle", "candidates": [{"label": "Candidate 1", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -1.706}, {"term": "mutual", "estimate": -1.621}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.306}, {"term": "nodematch.department", "estimate": 7.229}, {"term": "gwideg.fixed.0.5", "estimate": -6.138}], "pbic": 106.69, "density_obs": 0.181, "density_sim": 0.177, "density_rel_error": 0.022, "q": 2.09, "gof_rmse": 0.77, "gof_bins": 29, "residuals": [["odegree", "odegree3", 7, 3.67, 2.09], ["distance", "7", 2, 0.37, 1.68], ["odegree", "odegree4", 0, 1.73, -1.36]], "eligible": true, "reason": "eligible", "runtime": 1.2}, {"label": "Candidate 2", "terms": ["edges", "mutual", "nodematch(\"level\")", "nodeicov(\"tenure\")"], "coefficients": [{"term": "edges", "estimate": -3.04}, {"term": "mutual", "estimate": 0.905}, {"term": "nodematch.level", "estimate": 0.152}, {"term": "nodeicov.tenure", "estimate": 0.26}], "pbic": 167.71, "density_obs": 0.181, "density_sim": 0.183, "density_rel_error": 0.01, "q": 2.19, "gof_rmse": 0.91, "gof_bins": 34, "residuals": [["odegree", "odegree3", 7, 3.38, 2.19], ["distance", "7", 2, 0.27, 1.88], ["idegree", "idegree1", 1, 3.44, -1.67]], "eligible": true, "reason": "eligible", "runtime": 1}, {"label": "Candidate 3", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "nodeifactor(\"level\")"], "coefficients": [{"term": "edges", "estimate": -7.048}, {"term": "mutual", "estimate": -1.117}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.17}, {"term": "nodematch.department", "estimate": 7.2}, {"term": "nodeifactor.level.Mid", "estimate": 4.012}, {"term": "nodeifactor.level.Senior", "estimate": 6.128}], "pbic": 97.02, "density_obs": 0.181, "density_sim": 0.176, "density_rel_error": 0.027, "q": 2.28, "gof_rmse": 0.7, "gof_bins": 32, "residuals": [["odegree", "odegree3", 7, 3.75, 2.28], ["odegree", "odegree4", 0, 1.99, -1.73], ["idegree", "idegree1", 1, 2.78, -1.47]], "eligible": true, "reason": "eligible", "runtime": 1.3}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -1.507}], "pbic": 177.52, "density_obs": 0.181, "density_sim": 0.178, "density_rel_error": 0.018, "q": 3.13, "gof_rmse": 1.28, "gof_bins": 33, "residuals": [["idegree", "idegree0", 4, 0.9, 3.13], ["esp", "esp.OTP0", 16, 22.32, -2.68], ["odegree", "odegree3", 7, 3.11, 2.56]], "eligible": true, "reason": "eligible", "runtime": 0.6}], "selected": "Candidate 1", "initial_q": 2.09, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodeifactor(\"level\")", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "nodeifactor(\"level\")"], "eligible": true, "q_before": 2.09, "q_after": 1.9, "pbic": 100.54, "density_rel_error": 0.038, "residual": ["odegree", "odegree3", 7, 3.6, 1.9], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -5.131}, {"term": "mutual", "estimate": -1.263}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.439}, {"term": "nodematch.department", "estimate": 7.907}, {"term": "gwideg.fixed.0.5", "estimate": -2.801}, {"term": "nodeifactor.level.Mid", "estimate": 2.394}, {"term": "nodeifactor.level.Senior", "estimate": 4.377}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"tenure\")", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "nodeifactor(\"level\")", "absdiff(\"tenure\")"], "eligible": true, "q_before": 1.9, "q_after": 1.74, "pbic": 100.8, "density_rel_error": 0.007, "residual": ["odegree", "odegree3", 7, 4.35, 1.74], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -4.169}, {"term": "mutual", "estimate": -1.215}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.599}, {"term": "nodematch.department", "estimate": 8.936}, {"term": "gwideg.fixed.0.5", "estimate": -3.582}, {"term": "nodeifactor.level.Mid", "estimate": 2.489}, {"term": "nodeifactor.level.Senior", "estimate": 6.124}, {"term": "absdiff.tenure", "estimate": -0.47}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "mutual", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "nodeifactor(\"level\")", "absdiff(\"tenure\")"], "eligible": true, "q_before": 1.74, "q_after": 1.97, "pbic": 103.49, "density_rel_error": 0.007, "residual": ["odegree", "odegree3", 7, 4.12, 1.97], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -4.407}, {"term": "mutual", "estimate": -0.892}, {"term": "gwesp.OTP.fixed.0.25", "estimate": -1.435}, {"term": "nodematch.department", "estimate": 8.117}, {"term": "gwideg.fixed.0.5", "estimate": -3.052}, {"term": "nodeifactor.level.Mid", "estimate": 2.527}, {"term": "nodeifactor.level.Senior", "estimate": 6.01}, {"term": "absdiff.tenure", "estimate": -0.452}]}, {"round": 4, "action": "remove", "target": null, "term": "gwidegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "nodeifactor(\"level\")", "absdiff(\"tenure\")"], "eligible": true, "q_before": 1.74, "q_after": 1.75, "pbic": 97.46, "density_rel_error": 0.009, "residual": ["odegree", "odegree3", 7, 4.44, 1.75], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -6.752}, {"term": "mutual", "estimate": -0.989}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.417}, {"term": "nodematch.department", "estimate": 8.164}, {"term": "nodeifactor.level.Mid", "estimate": 4.665}, {"term": "nodeifactor.level.Senior", "estimate": 8.432}, {"term": "absdiff.tenure", "estimate": -0.444}]}], "final": {"label": "Final model", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "nodeifactor(\"level\")", "absdiff(\"tenure\")"], "coefficients": [{"term": "edges", "estimate": -4.169}, {"term": "mutual", "estimate": -1.215}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.599}, {"term": "nodematch.department", "estimate": 8.936}, {"term": "gwideg.fixed.0.5", "estimate": -3.582}, {"term": "nodeifactor.level.Mid", "estimate": 2.489}, {"term": "nodeifactor.level.Senior", "estimate": 6.124}, {"term": "absdiff.tenure", "estimate": -0.47}], "pbic": 100.8, "density_obs": 0.181, "density_sim": 0.183, "density_rel_error": 0.007, "q": 1.74, "gof_rmse": 0.61, "gof_bins": 31, "residuals": [["odegree", "odegree3", 7, 4.35, 1.74], ["distance", "7", 2, 0.47, 1.38], ["odegree", "odegree4", 0, 1.61, -1.35]], "eligible": true, "reason": "eligible", "runtime": 3.5}}, "mple": {"estimator": "mple", "candidates": [{"label": "Candidate 1", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -1.57}, {"term": "mutual", "estimate": -1.207}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.298}, {"term": "nodematch.department", "estimate": 6.872}, {"term": "gwideg.fixed.0.5", "estimate": -6.285}], "pbic": 106.69, "density_obs": 0.181, "density_sim": 0.172, "density_rel_error": 0.052, "q": 1.78, "gof_rmse": 0.68, "gof_bins": 30, "residuals": [["odegree", "odegree3", 7, 4.02, 1.78], ["distance", "7", 2, 0.31, 1.39], ["idegree", "idegree6", 1, 0.32, 1.33]], "eligible": true, "reason": "eligible", "runtime": 0.7}, {"label": "Candidate 2", "terms": ["edges", "mutual", "nodematch(\"level\")", "nodeicov(\"tenure\")"], "coefficients": [{"term": "edges", "estimate": -3.342}, {"term": "mutual", "estimate": 1.312}, {"term": "nodematch.level", "estimate": 0.187}, {"term": "nodeicov.tenure", "estimate": 0.296}], "pbic": 167.71, "density_obs": 0.181, "density_sim": 0.188, "density_rel_error": 0.039, "q": 2.01, "gof_rmse": 0.83, "gof_bins": 37, "residuals": [["odegree", "odegree3", 7, 3.55, 2.01], ["distance", "7", 2, 0.2, 1.89], ["distance", "2", 27, 47.5, -1.6]], "eligible": true, "reason": "eligible", "runtime": 0.7}, {"label": "Candidate 3", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "nodeifactor(\"level\")"], "coefficients": [{"term": "edges", "estimate": -8.604}, {"term": "mutual", "estimate": -1.36}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.872}, {"term": "nodematch.department", "estimate": 9.034}, {"term": "nodeifactor.level.Mid", "estimate": 5.581}, {"term": "nodeifactor.level.Senior", "estimate": 8.082}], "pbic": 97.02, "density_obs": 0.181, "density_sim": 0.181, "density_rel_error": 0.003, "q": 1.97, "gof_rmse": 0.74, "gof_bins": 30, "residuals": [["odegree", "odegree3", 7, 3.89, 1.97], ["odegree", "odegree4", 0, 1.86, -1.72], ["idegree", "idegree1", 1, 2.27, -1.14]], "eligible": true, "reason": "eligible", "runtime": 0.7}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -1.507}], "pbic": 177.52, "density_obs": 0.181, "density_sim": 0.178, "density_rel_error": 0.018, "q": 3.13, "gof_rmse": 1.28, "gof_bins": 33, "residuals": [["idegree", "idegree0", 4, 0.9, 3.13], ["esp", "esp.OTP0", 16, 22.32, -2.68], ["odegree", "odegree3", 7, 3.11, 2.56]], "eligible": true, "reason": "eligible", "runtime": 0.6}], "selected": "Candidate 1", "initial_q": 1.78, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodeifactor(\"level\")", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "nodeifactor(\"level\")"], "eligible": true, "q_before": 1.78, "q_after": 1.9, "pbic": 100.54, "density_rel_error": 0.026, "residual": ["odegree", "odegree3", 7, 3.76, 1.9], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -6.865}, {"term": "mutual", "estimate": -1.296}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.971}, {"term": "nodematch.department", "estimate": 9.337}, {"term": "gwideg.fixed.0.5", "estimate": -2.525}, {"term": "nodeifactor.level.Mid", "estimate": 4.041}, {"term": "nodeifactor.level.Senior", "estimate": 6.412}]}, {"round": 2, "action": "add", "target": null, "term": "absdiff(\"tenure\")", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)", "absdiff(\"tenure\")"], "eligible": true, "q_before": 1.78, "q_after": 2.59, "pbic": 110.97, "density_rel_error": 0.043, "residual": ["distance", "7", 2, 0.18, 2.59], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -1.171}, {"term": "mutual", "estimate": -1.171}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.212}, {"term": "nodematch.department", "estimate": 6.817}, {"term": "gwideg.fixed.0.5", "estimate": -6.429}, {"term": "absdiff.tenure", "estimate": -0.121}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "mutual", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.78, "q_after": 2.08, "pbic": 108.55, "density_rel_error": 0.02, "residual": ["odegree", "odegree3", 7, 3.49, 2.08], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -1.694}, {"term": "mutual", "estimate": -0.988}, {"term": "gwesp.OTP.fixed.0.25", "estimate": -1.136}, {"term": "nodematch.department", "estimate": 6.297}, {"term": "gwideg.fixed.0.5", "estimate": -5.964}]}, {"round": 4, "action": "remove", "target": null, "term": "gwidegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")"], "eligible": true, "q_before": 1.78, "q_after": 4.61, "pbic": 135.94, "density_rel_error": 0.012, "residual": ["idegree", "idegree0", 4, 0.72, 4.61], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.017}, {"term": "mutual", "estimate": -1.297}, {"term": "gwesp.OTP.fixed.0.5", "estimate": 0.043}, {"term": "nodematch.department", "estimate": 3.706}]}], "final": {"label": "Final model", "terms": ["edges", "mutual", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"department\")", "gwidegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -1.57}, {"term": "mutual", "estimate": -1.207}, {"term": "gwesp.OTP.fixed.0.5", "estimate": -1.298}, {"term": "nodematch.department", "estimate": 6.872}, {"term": "gwideg.fixed.0.5", "estimate": -6.285}], "pbic": 106.69, "density_obs": 0.181, "density_sim": 0.172, "density_rel_error": 0.052, "q": 1.78, "gof_rmse": 0.68, "gof_bins": 30, "residuals": [["odegree", "odegree3", 7, 4.02, 1.78], ["distance", "7", 2, 0.31, 1.39], ["idegree", "idegree6", 1, 0.32, 1.33]], "eligible": true, "reason": "eligible", "runtime": 0.7}}}, "opensource": {"sa": {"estimator": "sa", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.221}, {"term": "gwesp.fixed.0.5", "estimate": 0.128}, {"term": "nodematch.module", "estimate": 3.905}, {"term": "gwdeg.fixed.0.5", "estimate": 1.957}], "pbic": 79.41, "density_obs": 0.267, "density_sim": 0.264, "density_rel_error": 0.008, "q": 2.34, "gof_rmse": 1.05, "gof_bins": 23, "residuals": [["degree", "degree3", 8, 4.2, 2.34], ["esp", "esp1", 13, 6.96, 1.83], ["distance", "3", 50, 29.62, 1.83]], "eligible": true, "reason": "eligible", "runtime": 2.2}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"role\")", "nodecov(\"commits\")"], "coefficients": [{"term": "edges", "estimate": -3.169}, {"term": "gwesp.fixed.0.5", "estimate": 0.866}, {"term": "nodematch.role", "estimate": 0.18}, {"term": "nodecov.commits", "estimate": 0.003}], "pbic": 111.43, "density_obs": 0.267, "density_sim": 0.288, "density_rel_error": 0.081, "q": 4.55, "gof_rmse": 1.23, "gof_bins": 33, "residuals": [["distance", "3", 50, 16.51, 4.55], ["degree", "degree3", 8, 2.53, 3.55], ["esp", "esp1", 13, 8.17, 1.48]], "eligible": true, "reason": "eligible", "runtime": 2.4}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": 81.454}, {"term": "gwesp.fixed.0.5", "estimate": 8.184}, {"term": "gwdsp.fixed.0.5", "estimate": -10.492}, {"term": "nodematch.module", "estimate": -88.622}, {"term": "gwdeg.fixed.0.5", "estimate": 0.519}], "pbic": 43.17, "density_obs": 0.267, "density_sim": 0.999, "density_rel_error": 2.748, "eligible": false, "reason": "density check failed (275% relative error)", "runtime": 1.8}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -1.012}], "pbic": 143.97, "density_obs": 0.267, "density_sim": 0.261, "density_rel_error": 0.02, "q": 2.92, "gof_rmse": 1.09, "gof_bins": 29, "residuals": [["distance", "3", 50, 24.69, 2.92], ["degree", "degree3", 8, 3.24, 2.65], ["distance", "2", 38, 56.72, -2.22]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Candidate 1", "initial_q": 2.34, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodecov(\"commits\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodecov(\"commits\")"], "eligible": true, "q_before": 2.34, "q_after": 2.02, "pbic": 47.72, "density_rel_error": 0.003, "residual": ["esp", "esp1", 13, 6.65, 2.02], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -11.949}, {"term": "gwesp.fixed.0.5", "estimate": -0.573}, {"term": "nodematch.module", "estimate": 7.732}, {"term": "gwdeg.fixed.0.5", "estimate": 28.82}, {"term": "nodecov.commits", "estimate": 0.025}]}, {"round": 2, "action": "add", "target": null, "term": "nodematch(\"role\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodecov(\"commits\")", "nodematch(\"role\")"], "eligible": false, "q_before": 2.02, "q_after": null, "pbic": 40.71, "density_rel_error": 1.812, "residual": null, "accepted": false, "reason": "density check failed (181% relative error)", "coefficients": [{"term": "edges", "estimate": 12.726}, {"term": "gwesp.fixed.0.5", "estimate": -0.022}, {"term": "nodematch.module", "estimate": -1.812}, {"term": "gwdeg.fixed.0.5", "estimate": -77.6}, {"term": "nodecov.commits", "estimate": -0.026}, {"term": "nodematch.role", "estimate": -0.06}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodecov(\"commits\")"], "eligible": true, "q_before": 2.02, "q_after": 1.78, "pbic": 47.74, "density_rel_error": 0.009, "residual": ["esp", "esp1", 13, 7.16, 1.78], "accepted": true, "reason": "eligible and lower q(M)", "coefficients": [{"term": "edges", "estimate": -10.928}, {"term": "gwesp.fixed.0.25", "estimate": -0.306}, {"term": "nodematch.module", "estimate": 6.704}, {"term": "gwdeg.fixed.0.5", "estimate": 22.553}, {"term": "nodecov.commits", "estimate": 0.022}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"module\")", "nodecov(\"commits\")"], "eligible": true, "q_before": 1.78, "q_after": 2.39, "pbic": 65.16, "density_rel_error": 0.004, "residual": ["degree", "degree3", 8, 4.09, 2.39], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -7.112}, {"term": "gwesp.fixed.0.25", "estimate": -0.246}, {"term": "nodematch.module", "estimate": 7.061}, {"term": "nodecov.commits", "estimate": 0.015}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodecov(\"commits\")"], "coefficients": [{"term": "edges", "estimate": -10.928}, {"term": "gwesp.fixed.0.25", "estimate": -0.306}, {"term": "nodematch.module", "estimate": 6.704}, {"term": "gwdeg.fixed.0.5", "estimate": 22.553}, {"term": "nodecov.commits", "estimate": 0.022}], "pbic": 47.74, "density_obs": 0.267, "density_sim": 0.264, "density_rel_error": 0.009, "q": 1.78, "gof_rmse": 0.86, "gof_bins": 23, "residuals": [["esp", "esp1", 13, 7.16, 1.78], ["degree", "degree6", 2, 0.76, 1.65], ["distance", "3", 50, 37.01, 1.54]], "eligible": true, "reason": "eligible", "runtime": 1.7}}, "mcmle": {"estimator": "mcmle", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.386}, {"term": "gwesp.fixed.0.5", "estimate": 0.115}, {"term": "nodematch.module", "estimate": 3.896}, {"term": "gwdeg.fixed.0.5", "estimate": 4.014}], "pbic": 79.41, "density_obs": 0.267, "density_sim": 0.27, "density_rel_error": 0.013, "q": 1.95, "gof_rmse": 1.1, "gof_bins": 22, "residuals": [["degree", "degree3", 8, 4.01, 1.95], ["distance", "3", 50, 31.41, 1.9], ["degree", "degree7", 1, 0.2, 1.88]], "eligible": true, "reason": "eligible", "runtime": 1.8}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"role\")", "nodecov(\"commits\")"], "coefficients": [{"term": "edges", "estimate": -3.121}, {"term": "gwesp.fixed.0.5", "estimate": 0.858}, {"term": "nodematch.role", "estimate": 0.193}, {"term": "nodecov.commits", "estimate": 0.003}], "pbic": 111.43, "density_obs": 0.267, "density_sim": 0.258, "density_rel_error": 0.031, "q": 4.51, "gof_rmse": 1.31, "gof_bins": 30, "residuals": [["distance", "3", 50, 17.13, 4.51], ["degree", "degree3", 8, 2.57, 4.11], ["degree", "degree2", 0, 1.83, -1.36]], "eligible": true, "reason": "eligible", "runtime": 1.8}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": false, "reason": "fit failed: Unconstrained MCMC sampling did not mix at all. Optimization cannot continue.", "runtime": 0.3}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -1.012}], "pbic": 143.97, "density_obs": 0.267, "density_sim": 0.261, "density_rel_error": 0.02, "q": 2.92, "gof_rmse": 1.09, "gof_bins": 29, "residuals": [["distance", "3", 50, 24.69, 2.92], ["degree", "degree3", 8, 3.24, 2.65], ["distance", "2", 38, 56.72, -2.22]], "eligible": true, "reason": "eligible", "runtime": 0.6}], "selected": "Candidate 1", "initial_q": 1.95, "rounds": [{"round": 1, "action": "add", "target": null, "term": "nodecov(\"commits\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodecov(\"commits\")"], "eligible": true, "q_before": 1.95, "q_after": 2, "pbic": 47.72, "density_rel_error": 0.004, "residual": ["esp", "esp1", 13, 6.53, 2], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -12.77}, {"term": "gwesp.fixed.0.5", "estimate": -0.515}, {"term": "nodematch.module", "estimate": 7.688}, {"term": "gwdeg.fixed.0.5", "estimate": 33.974}, {"term": "nodecov.commits", "estimate": 0.026}]}, {"round": 2, "action": "add", "target": null, "term": "nodematch(\"role\")", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)", "nodematch(\"role\")"], "eligible": true, "q_before": 1.95, "q_after": 2.13, "pbic": 80.02, "density_rel_error": 0.039, "residual": ["degree", "degree7", 1, 0.16, 2.13], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -4.043}, {"term": "gwesp.fixed.0.5", "estimate": 0.165}, {"term": "nodematch.module", "estimate": 4.34}, {"term": "gwdeg.fixed.0.5", "estimate": 2.972}, {"term": "nodematch.role", "estimate": 1.293}]}, {"round": 3, "action": "replace", "target": "gwesp(decay=0.5, fixed=TRUE)", "term": "gwesp(decay=0.25, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.25, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "eligible": true, "q_before": 1.95, "q_after": 2.22, "pbic": 70.39, "density_rel_error": 0.004, "residual": ["degree", "degree3", 8, 3.87, 2.22], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.853}, {"term": "gwesp.fixed.0.25", "estimate": 0.627}, {"term": "nodematch.module", "estimate": 3.411}, {"term": "gwdeg.fixed.0.5", "estimate": 4.638}]}, {"round": 4, "action": "remove", "target": null, "term": "gwdegree(decay=0.5, fixed=TRUE)", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")"], "eligible": true, "q_before": 1.95, "q_after": 2.18, "pbic": 80.66, "density_rel_error": 0.024, "residual": ["degree", "degree3", 8, 3.92, 2.18], "accepted": false, "reason": "eligible, but q(M) did not decrease", "coefficients": [{"term": "edges", "estimate": -3.061}, {"term": "gwesp.fixed.0.5", "estimate": 0.074}, {"term": "nodematch.module", "estimate": 4.246}]}], "final": {"label": "Final model", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -3.386}, {"term": "gwesp.fixed.0.5", "estimate": 0.115}, {"term": "nodematch.module", "estimate": 3.896}, {"term": "gwdeg.fixed.0.5", "estimate": 4.014}], "pbic": 79.41, "density_obs": 0.267, "density_sim": 0.27, "density_rel_error": 0.013, "q": 1.95, "gof_rmse": 1.1, "gof_bins": 22, "residuals": [["degree", "degree3", 8, 4.01, 1.95], ["distance", "3", 50, 31.41, 1.9], ["degree", "degree7", 1, 0.2, 1.88]], "eligible": true, "reason": "eligible", "runtime": 1.8}}, "mple": {"estimator": "mple", "candidates": [{"label": "Candidate 1", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -5.392}, {"term": "gwesp.fixed.0.5", "estimate": 0.99}, {"term": "nodematch.module", "estimate": 1.123}, {"term": "gwdeg.fixed.0.5", "estimate": 19.219}], "pbic": 79.41, "density_obs": 0.267, "density_sim": 0.262, "density_rel_error": 0.019, "q": 3.42, "gof_rmse": 1.14, "gof_bins": 21, "residuals": [["distance", "3", 50, 31.47, 3.42], ["distance", "4", 0, 10.98, -1.6], ["degree", "degree7", 1, 0.25, 1.56]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Candidate 2", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "nodematch(\"role\")", "nodecov(\"commits\")"], "coefficients": [{"term": "edges", "estimate": -3.388}, {"term": "gwesp.fixed.0.5", "estimate": 1.237}, {"term": "nodematch.role", "estimate": 0.824}, {"term": "nodecov.commits", "estimate": 0}], "pbic": 111.43, "density_obs": 0.267, "density_sim": 0.324, "density_rel_error": 0.215, "q": 4.66, "gof_rmse": 1.42, "gof_bins": 29, "residuals": [["degree", "degree3", 8, 1.62, 4.66], ["distance", "3", 50, 12.28, 4.54], ["esp", "esp1", 13, 6.91, 1.66]], "eligible": true, "reason": "eligible", "runtime": 0.6}, {"label": "Candidate 3", "terms": ["edges", "gwesp(decay=0.5, fixed=TRUE)", "gwdsp(decay=0.5, fixed=TRUE)", "nodematch(\"module\")", "gwdegree(decay=0.5, fixed=TRUE)"], "coefficients": [{"term": "edges", "estimate": -35.821}, {"term": "gwesp.fixed.0.5", "estimate": -2.377}, {"term": "gwdsp.fixed.0.5", "estimate": 4.48}, {"term": "nodematch.module", "estimate": 25.862}, {"term": "gwdeg.fixed.0.5", "estimate": 61.259}], "pbic": 43.17, "density_obs": 0.267, "density_sim": 0.258, "density_rel_error": 0.032, "q": 57.23, "gof_rmse": 18.19, "gof_bins": 10, "residuals": [["distance", "2", 38, 88.8, -57.23], ["degree", "degree5", 1, 0.05, 4.34], ["degree", "degree3", 8, 12.61, -2.62]], "eligible": true, "reason": "eligible", "runtime": 0.7}, {"label": "Edge-only baseline", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -1.012}], "pbic": 143.97, "density_obs": 0.267, "density_sim": 0.261, "density_rel_error": 0.02, "q": 2.92, "gof_rmse": 1.09, "gof_bins": 29, "residuals": [["distance", "3", 50, 24.69, 2.92], ["degree", "degree3", 8, 3.24, 2.65], ["distance", "2", 38, 56.72, -2.22]], "eligible": true, "reason": "eligible", "runtime": 0.5}], "selected": "Edge-only baseline", "initial_q": 2.92, "rounds": [], "final": {"label": "Final model", "terms": ["edges"], "coefficients": [{"term": "edges", "estimate": -1.012}], "pbic": 143.97, "density_obs": 0.267, "density_sim": 0.261, "density_rel_error": 0.02, "q": 2.92, "gof_rmse": 1.09, "gof_bins": 29, "residuals": [["distance", "3", 50, 24.69, 2.92], ["degree", "degree3", 8, 3.24, 2.65], ["distance", "2", 38, 56.72, -2.22]], "eligible": true, "reason": "eligible", "runtime": 0.5}}}};

// Per-network vocabulary and hand-written Stage 0 / 1b example text.
const networkText = {
  school: {
    shortLabel: "School",
    title: "School Friendship Network",
    nodeKind: "students", tieKind: "friendship ties", actor: "student", actors: "students", headlineVerb: "are friends", tieVerb: "be friends", tieNoun: "friendship",
    partner: "friend", partners: "friends", cohortPrefix: "G", hubThreshold: 4,
    palette: { Robotics: "--green", Drama: "--rose", Studio: "--blue" },
    attrLabels: { club: "activity club", grade: "grade", activity: "activity level" },
    dataset: "school_friendship",
    tieLine: "undirected mutual friendship",
    description: "The network consists of students from the same school year. A tie indicates a mutual friendship. Friendships tend to form within the same activity clubs and grade cohorts.",
    visiblePatterns: ["same-club ties", "local closure", "one bridging student"],
    intakeCopy: "The demo starts from a 12-student friendship network with club, grade, and activity attributes. Node colors are activity clubs; ties are observed friendships. The graph already hints at homophily and closure.",
    intakeTheory: "At intake, FORGE has not produced an interpretation yet. It only records that friendships are not random: students appear to cluster by club, close triangles with mutual friends, and rely on a few bridge students.",
    mechanisms: [
      ["club homophily", "most ties are within clubs", 'nodematch("club")'],
      ["triadic closure", "transitivity 0.43", "gwesp(decay=0.5, fixed=TRUE)"],
      ["degree heterogeneity", "degrees range 2-5", "gwdegree(decay=0.5, fixed=TRUE)"]
    ],
    specTheory: "The LLM's first proposal is that friendship is mostly explained by shared club, shared friends, and the spread of friendships across students. Stage 2 fits the candidates and decides which proposal the data supports."
  },
  lab: {
    shortLabel: "Lab",
    title: "Research Collaboration Network",
    nodeKind: "researchers", tieKind: "collaboration ties", actor: "researcher", actors: "researchers", headlineVerb: "collaborate", tieVerb: "collaborate", tieNoun: "collaboration",
    partner: "collaborator", partners: "collaborators", cohortPrefix: "", hubThreshold: 4,
    palette: { NLP: "--green", Vision: "--rose", Systems: "--blue" },
    attrLabels: { area: "research area", role: "role", seniority: "seniority" },
    dataset: "research_collab",
    tieLine: "undirected active collaboration",
    description: "Researchers in one department. A tie means an active co-authorship collaboration. Collaboration follows research areas and lab roles.",
    visiblePatterns: ["same-area collaboration", "shared-collaborator closure", "cross-area bridge researchers"],
    intakeCopy: "This network tracks 12 researchers. Colors are research areas; ties are coauthorship or project collaboration. The graph shows area clusters, shared-collaborator closure, and a few cross-area connectors.",
    intakeTheory: "At intake, FORGE has not produced an interpretation yet. It records that collaborations concentrate inside research areas, close around shared collaborators, and depend on a few researchers who bridge areas.",
    mechanisms: [
      ["area homophily", "most ties are within areas", 'nodematch("area")'],
      ["triadic closure", "transitivity 0.36", "gwesp(decay=0.5, fixed=TRUE)"],
      ["degree heterogeneity", "PIs have the most ties", "gwdegree(decay=0.5, fixed=TRUE)"]
    ],
    specTheory: "The LLM's first proposal is that researchers collaborate mostly through shared collaborators, shared research area, and an uneven spread of ties. Stage 2 fits the candidates and decides which proposal the data supports."
  },
  neighborhood: {
    shortLabel: "Neighborhood",
    title: "Neighborhood Mutual Aid Network",
    nodeKind: "households", tieKind: "mutual-aid ties", actor: "household", actors: "households", headlineVerb: "help each other", tieVerb: "exchange help", tieNoun: "help",
    partner: "neighbor", partners: "neighbors", cohortPrefix: "", hubThreshold: 4,
    palette: { North: "--green", Market: "--rose", Riverside: "--blue" },
    attrLabels: { block: "block", tenure_group: "tenure group", tenure_years: "years of residence" },
    dataset: "neighborhood_aid",
    tieLine: "undirected mutual-aid exchange",
    description: "Households in one neighborhood. A tie means the households exchange practical help. Help flows within blocks and among long-tenured residents.",
    visiblePatterns: ["same-block support", "local closure", "block-bridging households"],
    intakeCopy: "This network represents 12 households exchanging mutual aid. Colors are neighborhood blocks. Ties show observed support exchanges, with block clusters and a few households connecting blocks.",
    intakeTheory: "At intake, FORGE has not produced an interpretation yet. It records that support ties cluster by block, close around shared neighbors, and rely on a few households that bridge local areas.",
    mechanisms: [
      ["block homophily", "most ties are within blocks", 'nodematch("block")'],
      ["triadic closure", "transitivity 0.46", "gwesp(decay=0.5, fixed=TRUE)"],
      ["degree heterogeneity", "long-tenured households have the most ties", "gwdegree(decay=0.5, fixed=TRUE)"]
    ],
    specTheory: "The LLM's first proposal is that mutual aid is explained by same-block proximity, shared neighbors, and an uneven spread of helping. Stage 2 fits the candidates and decides which proposal the data supports."
  },
  office: {
    shortLabel: "Office",
    title: "Office Advice Network (directed)",
    nodeKind: "employees", tieKind: "advice ties", actor: "employee", actors: "employees", headlineVerb: "seek advice", tieVerb: "seek advice", tieNoun: "advice",
    partner: "contact", partners: "contacts", cohortPrefix: "", hubThreshold: 5,
    palette: { Sales: "--green", Engineering: "--rose", Support: "--blue" },
    attrLabels: { department: "department", level: "seniority level", tenure: "tenure" },
    dataset: "office_advice",
    tieLine: "directed: A -> B means A seeks advice from B",
    description: "Employees in a small company. A directed tie from A to B means A regularly asks B for work advice. Advice flows mostly within departments and toward senior staff, and is sometimes returned.",
    visiblePatterns: ["within-department advice", "senior staff receive many requests", "some reciprocated pairs"],
    intakeCopy: "This directed network has 14 employees in three departments. Colors are departments; an arrow from A to B means A seeks advice from B. Senior employees receive many requests, and a few pairs advise each other.",
    intakeTheory: "At intake, FORGE has not produced an interpretation yet. It records that advice requests stay mostly within departments, concentrate on senior staff, and are reciprocated in a minority of pairs.",
    mechanisms: [
      ["reciprocity", "reciprocity 0.30: some pairs advise each other", "mutual"],
      ["department homophily", "most requests stay within departments", 'nodematch("department")'],
      ["in-degree concentration", "senior staff receive many requests", "gwidegree(decay=0.5, fixed=TRUE)"]
    ],
    specTheory: "The LLM's first proposal is that advice ties are explained by reciprocation, shared department, closure among colleagues, and a concentration of requests on a few advisers. Stage 2 fits the candidates and decides which proposal the data supports."
  },
  opensource: {
    shortLabel: "Open source",
    title: "Open-Source Project Network",
    nodeKind: "developers", tieKind: "co-editing ties", actor: "developer", actors: "developers", headlineVerb: "work together", tieVerb: "work together", tieNoun: "collaboration",
    partner: "collaborator", partners: "collaborators", cohortPrefix: "", hubThreshold: 5,
    palette: { Core: "--green", UI: "--rose", Docs: "--blue" },
    attrLabels: { module: "module", role: "role", commits: "commit count" },
    dataset: "opensource_project",
    tieLine: "undirected: the two developers edited the same files",
    description: "Developers of one open-source project. A tie means the two developers edited the same files in the last release. Maintainers coordinate their module and link modules together.",
    visiblePatterns: ["same-module collaboration", "maintainers as hubs", "closure inside modules"],
    intakeCopy: "This network has 16 developers across three modules. Colors are modules; ties mean two developers edited the same files. Maintainers sit at the center of their module and connect to other maintainers.",
    intakeTheory: "At intake, FORGE has not produced an interpretation yet. It records that co-editing concentrates inside modules, that maintainers have the most ties, and that modules are linked mainly through maintainers.",
    mechanisms: [
      ["module homophily", "most ties are within modules", 'nodematch("module")'],
      ["triadic closure", "transitivity 0.5", "gwesp(decay=0.5, fixed=TRUE)"],
      ["degree heterogeneity", "maintainers have the most ties", "gwdegree(decay=0.5, fixed=TRUE)"]
    ],
    specTheory: "The LLM's first proposal is that co-editing is explained by shared module, shared collaborators, and a concentration of ties on maintainers. Stage 2 fits the candidates and decides which proposal the data supports."
  }
};

// ---------------------------------------------------------------- shared text

const guardrailSets = {
  intake: (directed) => [
    ["pass", "No missing node attributes in the network"],
    ["pass", directed ? "No self-loops; reciprocal arcs allowed" : "Undirected ties have no self-loops"],
    ["pass", "Binary, static network: supported by the term list"]
  ],
  library: (directed) => [
    ["pass", "Every term is available in ergm syntax"],
    ["pass", directed ? "Reciprocity and in/out-degree terms included: network is directed" : "Reciprocity terms excluded: network is undirected"],
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

function capitalize(text) {
  return text.charAt(0).toUpperCase() + text.slice(1);
}

function residualLabel([stat, bin]) {
  if (stat === "esp") return `shared partners = ${String(bin).replace(/^esp/, "")}`;
  if (stat === "degree") return `degree = ${String(bin).replace(/^degree/, "")}`;
  if (stat === "idegree") return `in-degree = ${String(bin).replace(/^idegree/, "")}`;
  if (stat === "odegree") return `out-degree = ${String(bin).replace(/^odegree/, "")}`;
  return `geodesic distance = ${bin}`;
}

function residualLine(residual) {
  const [stat, bin, obs, simMean, z] = residual;
  return `${residualLabel([stat, bin])}: observed ${obs}, simulated mean ${simMean}, z = ${signed(z)}`;
}

function editLabel(round) {
  if (round.action === "replace") {
    return `replace ${shortTerm(round.target)} with ${shortTerm(round.term)}`;
  }
  return `${round.action} ${shortTerm(round.term)}`;
}

function compactTerm(term) {
  return shortTerm(term).replace(/^nodematch\(/, "match(").replace(/^nodefactor\(/, "factor(").replace(/^nodeifactor\(/, "ifactor(").replace(/^nodeofactor\(/, "ofactor(");
}

function editShort(round) {
  if (round.action === "replace") {
    const base = termBase(round.term);
    const from = (round.target.match(/decay=([0-9.]+)/) || [])[1];
    const to = (round.term.match(/decay=([0-9.]+)/) || [])[1];
    return from && to ? `${base} ${from}→${to}` : `${compactTerm(round.target)}→${compactTerm(round.term)}`;
  }
  return `${round.action === "add" ? "+" : "−"}${compactTerm(round.term)}`;
}

function estimatorInfo(id) {
  return ESTIMATORS.find((e) => e.id === id) || ESTIMATORS[0];
}

// ---------------------------------------------------------------- template text for Stages 3–4

function rationaleFor(round, voc, residual) {
  const base = termBase(round.term);
  const attr = termAttr(round.term);
  const label = attr ? (voc.attrLabels[attr] || attr) : null;
  const target = residual ? residualLabel(residual) : "the largest residual";
  if (round.action === "remove") {
    return `Removing ${shortTerm(round.term)} tests whether ${termGloss(round.term)} is redundant once the other terms are in the model.`;
  }
  if (round.action === "replace") {
    return `A ${termBase(round.term)} decay of ${(round.term.match(/decay=([0-9.]+)/) || [])[1]} weights the first ${voc.partners} more heavily, targeting the ${target} mismatch.`;
  }
  const reasons = {
    nodematch: `Same-${label} ${voc.actors} may be under-represented; ${label} homophily is in L* and untested.`,
    absdiff: `Ties may depend on how far apart two ${voc.actors} are in ${label}; absdiff targets the ${target} mismatch.`,
    nodecov: `${capitalize(voc.actors)} with higher ${label} may form more ties; nodecov(${attr}) is in L* and untested.`,
    nodeicov: `${capitalize(voc.actors)} with higher ${label} may receive more ties; nodeicov(${attr}) is in L* and untested.`,
    nodeocov: `${capitalize(voc.actors)} with higher ${label} may send more ties; nodeocov(${attr}) is in L* and untested.`,
    nodefactor: `Tie counts may differ across ${label} levels; nodefactor(${attr}) targets the ${target} mismatch.`,
    nodeifactor: `Incoming ties may differ across ${label} levels; nodeifactor(${attr}) targets the ${target} mismatch.`,
    nodeofactor: `Outgoing ties may differ across ${label} levels; nodeofactor(${attr}) targets the ${target} mismatch.`,
    gwesp: `Closure may be under-modeled; gwesp targets the ${target} mismatch.`,
    gwdsp: `Open two-paths may be under-modeled; gwdsp targets the ${target} mismatch.`,
    mutual: `Reciprocated ties may be under-modeled; mutual is in L* and untested.`
  };
  return reasons[base] || `${shortTerm(round.term)} is in L* and untested; it targets the ${target} mismatch.`;
}

function magnitudeWord(value) {
  const size = Math.abs(value);
  if (size >= 1.5) return "much more";
  if (size >= 0.3) return "more";
  return "somewhat more";
}

function readingsFor(final, voc, directed) {
  const readings = [];
  const seen = new Set();
  const coefs = final.coefficients || [];
  final.terms.forEach((term) => {
    const base = termBase(term);
    const attr = termAttr(term);
    const label = attr ? (voc.attrLabels[attr] || attr) : null;
    const prefix = base === "edges" ? "edges" : attr ? `${base}.${attr}` : base;
    const matching = coefs.filter((c) => c.term === prefix || c.term.startsWith(`${prefix}.`) || (base !== "edges" && c.term.startsWith(base) && !attr));
    const est = matching.length ? matching[0].estimate : 0;
    const pos = est > 0;
    const tiny = Math.abs(est) < 0.05;
    const A = capitalize(voc.actors);
    let mechanism = termGloss(term);
    let reading;
    switch (base) {
      case "edges":
        mechanism = "baseline tie rate";
        reading = est < 0 ? `${capitalize(voc.tieKind)} are sparse overall; most pairs are not connected.` : `${capitalize(voc.tieKind)} are dense overall.`;
        break;
      case "mutual":
        mechanism = "reciprocity";
        reading = pos ? `When one ${voc.actor} seeks ${voc.tieNoun} from another, the tie is likely to be returned.` : `Ties are rarely returned.`;
        break;
      case "gwesp":
        mechanism = "triadic closure";
        reading = tiny ? `Sharing a ${voc.partner} adds little once the other terms are accounted for.`
          : pos ? `${A} who already share a ${voc.partner} are ${magnitudeWord(est)} likely to ${voc.tieVerb}.`
          : `Given the other terms, additional shared ${voc.partners} do not add tie propensity.`;
        break;
      case "gwdsp":
        mechanism = "open two-paths";
        reading = pos ? `Pairs that share ${voc.partners} without a direct tie are common, consistent with clustered groups.` : `Shared ${voc.partners} tend to be closed into direct ties.`;
        break;
      case "gwdegree":
        mechanism = "degree spread";
        reading = pos ? `${capitalize(voc.tieKind)} are spread fairly evenly across ${voc.actors} rather than concentrated on a few.` : `${capitalize(voc.tieKind)} are concentrated on a few highly connected ${voc.actors}.`;
        break;
      case "gwidegree":
        mechanism = "in-degree spread";
        reading = pos ? `Incoming ${voc.tieKind} are spread across ${voc.actors} rather than concentrated on a few.` : `Incoming ${voc.tieKind} are concentrated on a few ${voc.actors} who receive many requests.`;
        break;
      case "gwodegree":
        mechanism = "out-degree spread";
        reading = pos ? `Outgoing ${voc.tieKind} are spread across ${voc.actors}.` : `A few ${voc.actors} send most of the ${voc.tieKind}.`;
        break;
      case "nodematch":
        mechanism = `${label} homophily`;
        reading = pos ? `Two ${voc.actors} with the same ${label} are ${magnitudeWord(est)} likely to ${voc.tieVerb}, holding the other terms fixed.` : `Two ${voc.actors} with the same ${label} are less likely to ${voc.tieVerb}.`;
        break;
      case "absdiff":
        mechanism = `${label} gap`;
        reading = pos ? `The larger the gap in ${label} between two ${voc.actors}, the more likely a tie.` : `The larger the gap in ${label} between two ${voc.actors}, the less likely a tie.`;
        break;
      case "nodecov":
        mechanism = `${label} effect`;
        reading = pos ? `${A} with higher ${label} form more ties.` : `${A} with higher ${label} form fewer ties.`;
        break;
      case "nodeicov":
        mechanism = `${label} effect (incoming)`;
        reading = pos ? `${A} with higher ${label} receive more ${voc.tieKind}.` : `${A} with higher ${label} receive fewer ${voc.tieKind}.`;
        break;
      case "nodeocov":
        mechanism = `${label} effect (outgoing)`;
        reading = pos ? `${A} with higher ${label} send more ${voc.tieKind}.` : `${A} with higher ${label} send fewer ${voc.tieKind}.`;
        break;
      case "nodefactor":
      case "nodeifactor":
      case "nodeofactor": {
        const verb = base === "nodeifactor" ? "receive" : base === "nodeofactor" ? "send" : "form";
        mechanism = `${label} activity`;
        const parts = matching.map((c) => {
          const level = c.term.split(".").slice(2).join(".");
          return `${level}: ${c.estimate >= 0 ? "more" : "fewer"}`;
        });
        reading = `How many ${voc.tieKind} ${voc.actors} ${verb} depends on ${label} (${parts.join("; ")} than the reference level).`;
        break;
      }
      default:
        reading = `${termGloss(term)} is associated with tie formation (coefficient ${signed(est)}).`;
    }
    if (!seen.has(term)) {
      seen.add(term);
      readings.push({ term: shortTerm(term), mechanism, direction: tiny ? "near zero" : pos ? "positive" : "negative", coefficient: roundMetric(est, 2), reading });
    }
  });
  return readings;
}

function headlineFor(final, voc, readings) {
  const byBase = (base) => final.terms.find((t) => termBase(t) === base);
  const coefOf = (term) => {
    const r = readings.find((x) => x.term === shortTerm(term));
    return r ? r.coefficient : 0;
  };
  const clauses = [];
  const matches = final.terms.filter((t) => termBase(t) === "nodematch" && coefOf(t) > 0);
  if (matches.length) {
    clauses.push(`${capitalize(voc.actors)} ${voc.headlineVerb || voc.tieVerb} mostly within the same ${matches.map((t) => voc.attrLabels[termAttr(t)] || termAttr(t)).join(" and ")}`);
  } else if (final.terms.length <= 1) {
    return `No proposed mechanism improved on the baseline tie rate for ${voc.tieKind}`;
  } else {
    clauses.push(`${capitalize(voc.actors)} ${voc.headlineVerb || voc.tieVerb} in a structured way`);
  }
  const abs = final.terms.filter((t) => termBase(t) === "absdiff");
  abs.forEach((t) => {
    const c = coefOf(t);
    const label = voc.attrLabels[termAttr(t)] || termAttr(t);
    clauses.push(c < 0 ? `with peers of similar ${label}` : `across different ${label}`);
  });
  const mutual = byBase("mutual");
  if (mutual && coefOf(mutual) > 0) clauses.push("and often return the favor");
  const gwesp = byBase("gwesp");
  if (gwesp && coefOf(gwesp) > 0.05) clauses.push(`and ${voc.partners} of ${voc.partners} connect`);
  return clauses.join(", ");
}

function limitationsFor(demo, run, voc) {
  const rejected = (run.rounds || []).filter((r) => !r.accepted);
  const items = [
    "These are conditional associations in a fitted ERGM, not causal effects.",
    `With ${demo.diagnostics.nodes} ${voc.actors} and ${demo.diagnostics.edges} ${voc.tieKind}, coefficients are imprecise; q(M) is computed from 100 simulated networks.`
  ];
  if (rejected.length) {
    const names = rejected.slice(0, 2).map((r) => shortTerm(r.term)).join(" and ");
    items.push(`${names} ${rejected.length > 2 ? "and other edits were" : rejected.length === 1 ? "was" : "were"} proposed and rejected because q(M) did not fall; this does not show the mechanism is irrelevant.`);
  }
  const ineligible = run.candidates.filter((c) => !c.eligible);
  if (ineligible.length) {
    items.push(`${ineligible.map((c) => c.label).join(", ")} could not be fitted or failed the checks with this estimator, so its mechanisms were never compared.`);
  }
  return items;
}

// ---------------------------------------------------------------- stage builders

function buildIntakeStage(demo, voc) {
  const d = demo.diagnostics;
  const attrs = demo.attrs;
  return {
    id: "intake", number: "0", rail: "Intake", subtitle: "Network + description", kicker: "Stage 0", title: "Network Intake",
    status: "Stage 0: diagnostics", lens: "raw network",
    mechanismTitle: voc.intakeCopy.split(".")[0] ? (demo.directed ? "Observed advice ties stay within departments" : "Observed ties cluster within groups") : "",
    mechanismCopy: voc.intakeCopy,
    metrics: [["–", voc.nodeKind], ["–", voc.tieKind], ["–", "density"], ["–", d.directed ? "reciprocity" : "transitivity"]],
    terms: ["edges"],
    guardrails: guardrailSets.intake(demo.directed),
    chartTitle: "GOF discrepancy q(M) · lower is better", chartLabel: "nothing fitted yet", bic: [],
    prompt: `dataset: ${voc.dataset}
actors: ${voc.actors}
tie: ${voc.tieLine}
node attributes: ${attrs.group} (categorical), ${attrs.cohort} (categorical), ${attrs.score} (numeric)

description:
"${voc.description}"

task:
validate the input and summarize network diagnostics.`,
    output: JSON.stringify({ visible_patterns: voc.visiblePatterns }, null, 2),
    outputBadge: "diagnostics", highlight: "raw",
    theory: voc.intakeTheory
  };
}

function buildLibraryStage(demo, voc) {
  const terms = libraries[demo.id];
  const structural = terms.filter((t) => STRUCTURAL_BASES.has(termBase(t)));
  const attribute = terms.filter((t) => !STRUCTURAL_BASES.has(termBase(t)));
  const d = demo.diagnostics;
  const attrs = demo.attrs;
  const scoreValues = demo.nodes.map((n) => n.score);
  return {
    id: "library", number: "1a", rail: "Valid terms", subtitle: "Build L*", kicker: "Stage 1a", title: "Build the Valid Term List L*",
    status: "Stage 1a: valid terms", lens: "candidate mechanisms",
    mechanismTitle: "Only terms compatible with this network enter L*",
    mechanismCopy: `FORGE lists the ERGM terms that are valid for ${demo.directed ? "a directed" : "an undirected"} network with these attributes: ${structural.length} structural terms (${demo.directed ? "including mutual and in/out-degree terms" : "reciprocity terms excluded"}) and ${attribute.length} attribute terms. The raw triangle term is excluded, and each geometrically weighted family offers two fixed decays. This is the same list the live server builds in R.`,
    metrics: [[String(terms.length), "valid terms"], [String(structural.length), "structural terms"], [String(attribute.length), "attribute terms"], ["0", "off-menu terms"]],
    terms,
    guardrails: guardrailSets.library(demo.directed),
    chartTitle: "GOF discrepancy q(M) · lower is better", chartLabel: "nothing fitted yet", bic: [],
    prompt: `input:
network type: ${demo.directed ? "directed" : "undirected"}, ${d.nodes} nodes, ${d.edges} ties
attributes:
  ${attrs.group}: categorical, ${new Set(demo.nodes.map((n) => n.group)).size} levels
  ${attrs.cohort}: categorical, ${new Set(demo.nodes.map((n) => n.cohort)).size} levels
  ${attrs.score}: numeric, range ${Math.min(...scoreValues)}-${Math.max(...scoreValues)}

rules (R, build_admissible_library):
  include edges; ${demo.directed ? "include mutual and in/out-degree terms (directed)" : "exclude mutual (undirected)"};
  exclude raw triangle (degeneracy); fixed decays 0.25 and 0.5 per gw family;
  attribute terms only for present attributes with enough support per level.

task:
construct the valid ERGM term list L*.`,
    output: JSON.stringify({ L_star: terms, excluded: [demo.directed ? "triangle (degeneracy risk)" : "mutual (undirected)", ...(demo.directed ? [] : ["triangle (degeneracy risk)"])] }, null, 2),
    outputBadge: "term list", highlight: "homophily",
    theory: `The valid term space contains ${structural.length} structural mechanisms (baseline rate, ${demo.directed ? "reciprocity, " : ""}closure, degree spread) and ${attribute.length} attribute-based mechanisms for ${Object.values(voc.attrLabels).join(", ")}.`
  };
}

function buildSpecStage(demo, voc, run) {
  const cands = run.candidates.filter((c) => c.label !== "Edge-only baseline");
  const first = cands[0];
  const d = demo.diagnostics;
  const terms = libraries[demo.id];
  return {
    id: "spec", number: "1b", rail: "Propose", subtitle: "LLM formulas", kicker: "Stage 1b", title: "LLM Proposes Formulas from L*",
    status: "Stage 1b: LLM proposals", lens: "LLM-selected terms",
    mechanismTitle: "Mechanisms first, then exact terms, then ranked formulas",
    mechanismCopy: "The LLM sees the description, diagnostics, attributes, and L*. It names plausible formation mechanisms, ties each to evidence, maps it to an admissible term, and returns up to three ranked specifications as JSON. Terms outside L* are rejected.",
    metrics: [[String(cands.length), "candidate specs"], ["100%", "terms inside L*"], [String(first.terms.length), "terms in Candidate 1"], [String(voc.mechanisms.length), "mechanisms named"]],
    terms: first.terms,
    guardrails: guardrailSets.spec,
    chartTitle: "GOF discrepancy q(M) · lower is better", chartLabel: `${cands.length} candidates await fitting`, bic: [],
    prompt: `system:
You are an ERGM expert. Return JSON only.

user:
Network: ${d.nodes} ${voc.actors}, ${d.edges} ${demo.directed ? "directed" : "undirected"} ${voc.tieKind},
density ${d.density.toFixed(2)}, ${demo.directed ? `reciprocity ${d.reciprocity.toFixed(2)}` : `transitivity ${d.transitivity.toFixed(2)}`}.
Attributes: ${demo.attrs.group}, ${demo.attrs.cohort}, ${demo.attrs.score}.
Description: ${voc.description}

Valid terms L*: ${terms.map(shortTerm).join(", ")}.

1. List plausible tie-formation mechanisms with the evidence for each.
2. Map each mechanism to exactly one term in L*.
3. Return up to 3 ranked specifications. Use only L*. Include edges.`,
    output: JSON.stringify({
      mechanisms: voc.mechanisms.map(([mechanism, evidence, term]) => ({ mechanism, evidence, term })),
      specifications: cands.map((c) => ({ label: c.label, formula: c.terms }))
    }, null, 2),
    outputBadge: "llm json", highlight: "closure",
    theory: voc.specTheory
  };
}

function buildFitStage(demo, voc, run, estimator) {
  const cands = run.candidates;
  const est = estimatorInfo(estimator);
  const eligible = cands.filter((c) => c.eligible);
  const selected = cands.find((c) => c.label === run.selected);
  const ineligible = cands.filter((c) => !c.eligible);
  const finiteCount = cands.filter((c) => c.coefficients).length;
  const densityPass = cands.filter((c) => c.density_rel_error !== undefined && c.density_rel_error !== null && c.density_rel_error <= 0.25).length;
  const baseline = cands.find((c) => c.label === "Edge-only baseline");
  const pbicBest = eligible.slice().sort((a, b) => a.pbic - b.pbic)[0];

  const bic = cands.map((c) => [
    c.label.replace("Edge-only baseline", "Edge-only"),
    c.eligible ? c.q : null,
    !c.eligible ? "ineligible" : c.label === run.selected ? "selected" : "eligible"
  ]);

  const prompt = [
    `Stage 2 — statistical fitting and selection (R backend, no LLM call)`,
    ``,
    `estimator: ${est.long}  [${est.note}]`,
    ``,
    `candidate pool:`,
    ...cands.map((c) => `  ${c.label} = ${joinTerms(c.terms)}`),
    ``,
    `procedure:`,
    `  1. fit each candidate with ${est.label}`,
    `  2. eligibility A(M) = 1 requires: finite coefficients,`,
    `     successful simulation, density check (30 simulated networks,`,
    `     |relative density error| <= 25%), computable GOF diagnostics`,
    `  3. GOF from 100 simulated networks (same seed for every candidate):`,
    `     ${demo.directed ? "in-degree, out-degree" : "degree"}, edgewise shared partners, geodesic distance`,
    `     z_k = (obs_k - mean_sim_k) / sd_sim_k;   q(M) = max_k |z_k|`,
    `  4. select M0 = argmin q(M) over eligible candidates`,
    `     (MPLE pseudo-BIC is recorded as a secondary diagnostic only)`
  ].join("\n");

  const output = JSON.stringify(cands.map((c) => {
    const row = { candidate: c.label, formula: joinTerms(c.terms), estimator: est.label, eligible: c.eligible };
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

  const copyParts = [];
  copyParts.push(ineligible.length
    ? `${ineligible.map((c) => c.label).join(" and ")} ${ineligible.length > 1 ? "are" : "is"} ineligible with ${est.label} (${ineligible[0].reason}).`
    : `All four candidates were fitted with ${est.label} and passed the density check.`);
  copyParts.push(`${selected.label} reproduces the observed ${demo.directed ? "in/out-degree" : "degree"}, shared-partner, and distance distributions best (q(M) = ${selected.q.toFixed(2)}).`);
  if (selected.residuals) copyParts.push(`Its largest remaining mismatch: ${residualLine(selected.residuals[0])}.`);
  if (pbicBest && pbicBest.label !== selected.label) copyParts.push(`Pseudo-BIC would have preferred ${pbicBest.label}, but selection uses simulation-based GOF.`);
  else copyParts.push("Pseudo-BIC agrees here, but only q(M) decides.");

  return {
    id: "fit", number: "2", rail: "Fit & select", subtitle: `${est.label} fit + GOF`, kicker: "Stage 2", title: "Fit Candidates and Select by GOF",
    status: `Stage 2: ${est.label} fit and selection`, lens: "lowest q(M)",
    mechanismTitle: `${selected.label} wins with the smallest GOF discrepancy`,
    mechanismCopy: copyParts.join(" "),
    metrics: [[selected.q.toFixed(2), "lowest q(M)"], [`${eligible.length}/${cands.length}`, "eligible candidates"], [selected.label.replace("Candidate ", "C").replace("Edge-only baseline", "Edge-only"), "selected M0"], [selected.pbic == null ? "–" : String(selected.pbic), "PBIC (secondary)"]],
    terms: selected.terms,
    guardrails: [
      [finiteCount === cands.length ? "pass" : "warn", `${est.label} returned finite coefficients for ${finiteCount}/${cands.length} candidates`],
      [densityPass === finiteCount ? "pass" : "warn", `Density check passed for ${densityPass}/${finiteCount} fitted candidates (≤25% error)`],
      ["pass", `GOF computed from 100 simulations for ${eligible.length} eligible candidates`],
      ["pass", `Selected ${selected.label}: lowest q(M) = ${selected.q.toFixed(2)}`]
    ],
    chartTitle: "GOF discrepancy q(M) · lower is better", chartLabel: `${selected.label} selected`, bic,
    prompt, output, outputBadge: "fit + GOF table", highlight: "winner",
    theory: `With ${est.long}, the evidence favors ${selected.label}: ${selected.terms.filter((t) => t !== "edges").map((t) => termGloss(t)).join(", ") || "the baseline tie rate alone"} reproduce${selected.terms.length > 2 ? "" : "s"} the observed network best (q(M) ${selected.q.toFixed(2)}${baseline && baseline.eligible && baseline.label !== selected.label ? ` vs ${baseline.q.toFixed(2)} for the edge-only baseline` : ""}).`
  };
}

function roundRecord(r) {
  return {
    round: r.round, edit: editLabel(r), accepted: r.accepted, eligible: r.eligible,
    qBefore: r.q_before, qAfter: r.eligible ? r.q_after : null,
    reason: r.accepted ? "eligible, q(M) decreased" : (r.eligible ? "eligible, q(M) did not decrease" : r.reason)
  };
}

function buildEmptyReviseStage(demo, voc, run, estimator) {
  const est = estimatorInfo(estimator);
  const selected = run.candidates.find((c) => c.label === run.selected);
  const copy = `With ${est.label}, Stage 2 selected ${selected.label} (${joinTerms(selected.terms)}). A single add, remove, or replace cannot turn a ${selected.terms.length}-term model into a compatible specification (3 to 8 terms), so no revision round was run and the Stage 2 model is the final specification.`;
  return {
    id: "refine", number: "3", rail: "Revise", subtitle: "≤4 checked rounds", kicker: "Stage 3", title: "Diagnostic-Guided Revision",
    status: "Stage 3: checked revision", lens: "no compatible edit",
    mechanismTitle: "No compatible single-term edit: Stage 2 model retained",
    mechanismCopy: copy,
    metrics: [[selected.q.toFixed(2), "final q(M)"], ["0/0", "accepted / rounds"], ["0.0%", "q(M) reduction"], [String(selected.terms.length), "terms in final model"]],
    terms: selected.terms, rounds: [],
    guardrails: [["pass", "Each round proposes exactly one add / remove / replace"], ["pass", "Edited models must keep 3 to 8 terms (compatibility rule)"], ["warn", "No compatible edit available from the selected model"], ["pass", `q(M_T) = q(M_0) = ${selected.q.toFixed(2)}`]],
    chartTitle: "GOF discrepancy q(M) · lower is better", chartLabel: "0/0 accepted",
    bic: [["M0 (Stage 2)", selected.q, "selected"]],
    prompt: `Stage 3 — revision skipped\n\nselected model M0 = ${joinTerms(selected.terms)}\nq(M0) = ${selected.q.toFixed(2)}\n\nA compatible specification needs 3 to 8 terms; no single edit of M0 satisfies this, so no LLM edit request was issued.`,
    output: JSON.stringify({ estimator: est.label, M0: joinTerms(selected.terms), q_M0: selected.q, rounds: [], final: { formula: joinTerms(selected.terms), q: selected.q, pbic_secondary: selected.pbic } }, null, 2),
    outputBadge: "revision record", highlight: "refined",
    theory: `No compatible single-term edit exists for the selected ${selected.terms.length}-term model, so it stays the final specification (q(M) = ${selected.q.toFixed(2)}).`
  };
}

function buildReviseStage(demo, voc, run, estimator) {
  if (!run.rounds || run.rounds.length === 0) return buildEmptyReviseStage(demo, voc, run, estimator);
  const est = estimatorInfo(estimator);
  const rounds = run.rounds;
  const accepted = rounds.filter((r) => r.accepted);
  const selected = run.candidates.find((c) => c.label === run.selected);
  const final = run.final;
  const reduction = (selected.q - final.q) / selected.q;
  const lastRound = rounds[rounds.length - 1];
  const priorRounds = rounds.slice(0, -1);
  const currentBeforeLast = priorRounds.filter((r) => r.accepted).slice(-1)[0] || null;
  const currentTerms = currentBeforeLast ? currentBeforeLast.terms : selected.terms;
  const qCurrent = currentBeforeLast ? currentBeforeLast.q_after : selected.q;
  const currentResidual = currentBeforeLast ? currentBeforeLast.residual : (selected.residuals ? selected.residuals[0] : null);
  const residualBefore = (r) => {
    const prev = rounds.filter((x) => x.round < r.round && x.accepted).slice(-1)[0];
    return prev ? prev.residual : (selected.residuals ? selected.residuals[0] : null);
  };
  const historyLines = priorRounds.map((r) => {
    const outcome = r.accepted ? `accepted, q = ${r.q_after.toFixed(2)}`
      : (r.eligible ? `rejected, q = ${r.q_after.toFixed(2)} (no decrease)` : `rejected, ${r.reason}`);
    return `  round ${r.round}: ${editLabel(r)} -> ${outcome}`;
  });
  const rejectedSoFar = priorRounds.filter((r) => !r.accepted);

  const prompt = [
    "system:", "You are an ERGM expert. Return JSON only.", "", "user:",
    `Round ${lastRound.round} of 4.  Estimator: ${est.label}.`,
    `Current specification M_${lastRound.round - 1} = ${joinTerms(currentTerms)}`,
    `q(M_${lastRound.round - 1}) = ${qCurrent.toFixed(2)}   (largest |z| over GOF bins; lower is better)`,
    currentResidual ? `largest GOF residual: ${residualLine(currentResidual)}` : "",
    `valid terms L*: ${libraries[demo.id].map(shortTerm).join(", ")}`,
    "edit history:", ...historyLines,
    rejectedSoFar.length ? `Do not repeat the ${rejectedSoFar.length} rejected edit${rejectedSoFar.length > 1 ? "s" : ""} above.` : "",
    "", "Propose exactly ONE edit — add, remove, or replace a single term —",
    "with a reason tied to the residual above. The edit is kept only if",
    "the refitted model is eligible and strictly lowers q(M)."
  ].filter((line) => line !== "").join("\n");

  const output = JSON.stringify({
    estimator: est.label,
    M0: joinTerms(selected.terms), q_M0: selected.q,
    rounds: rounds.map((r) => ({
      round: r.round, action: r.action, ...(r.target ? { target: shortTerm(r.target) } : {}), term: shortTerm(r.term),
      rationale: rationaleFor(r, voc, residualBefore(r)),
      eligible: r.eligible, q_before: r.q_before, q_after: r.eligible ? r.q_after : null, accepted: r.accepted,
      decision: r.accepted ? `accepted: eligible and q(M) fell ${r.q_before.toFixed(2)} → ${r.q_after.toFixed(2)}`
        : (r.eligible ? `rejected: q(M) did not decrease (${r.q_after.toFixed(2)} vs ${r.q_before.toFixed(2)})` : `rejected: ${r.reason}`)
    })),
    final: { formula: joinTerms(final.terms), q: final.q, pbic_secondary: final.pbic }
  }, null, 2);

  const copy = rounds.map((r) => `Round ${r.round}: ${editLabel(r)} → ${r.accepted ? `accepted (q(M) ${r.q_before.toFixed(2)} → ${r.q_after.toFixed(2)})` : r.eligible ? `rejected (q(M) ${r.q_after.toFixed(2)} did not fall below ${r.q_before.toFixed(2)})` : `rejected (${r.reason})`}.`).join(" ");

  return {
    id: "refine", number: "3", rail: "Revise", subtitle: "≤4 checked rounds", kicker: "Stage 3", title: "Diagnostic-Guided Revision",
    status: "Stage 3: checked revision", lens: accepted.length ? "accepted edits" : "no accepted edit",
    mechanismTitle: accepted.length ? `${accepted.length} of ${rounds.length} proposed edits lowered q(M) and were kept` : `${rounds.length} edits proposed, none lowered q(M): Stage 2 model retained`,
    mechanismCopy: copy + (accepted.length ? "" : " The Stage 2 model is retained as the final specification."),
    metrics: [[final.q.toFixed(2), "final q(M)"], [`${accepted.length}/${rounds.length}`, "accepted / rounds"], [pct(reduction), "q(M) reduction"], [String(final.terms.length), "terms in final model"]],
    terms: final.terms,
    rounds: rounds.map(roundRecord),
    guardrails: [
      ["pass", "Each round proposes exactly one add / remove / replace"],
      ["pass", "Every proposed term comes from L*; rejected edits are not repeated"],
      [rounds.every((r) => r.eligible) ? "pass" : "warn", `${rounds.filter((r) => r.eligible).length}/${rounds.length} revised models passed the eligibility checks`],
      ["pass", `Kept ${accepted.length} edit${accepted.length === 1 ? "" : "s"}: q(M_T) = ${final.q.toFixed(2)} ≤ q(M_0) = ${selected.q.toFixed(2)}`]
    ],
    chartTitle: "GOF discrepancy q(M) · lower is better", chartLabel: `${accepted.length}/${rounds.length} accepted`,
    bic: [["M0 (Stage 2)", selected.q, "selected"], ...rounds.map((r) => [`R${r.round} ${editShort(r)}`, r.eligible ? r.q_after : null, r.accepted ? "accepted" : (r.eligible ? "rejected" : "ineligible")])],
    prompt, output, outputBadge: "revision record", highlight: "refined",
    theory: accepted.length
      ? `${accepted.length} edit${accepted.length === 1 ? "" : "s"} survived the checks: ${accepted.map((r) => termGloss(r.term)).join("; ")}. q(M) fell from ${selected.q.toFixed(2)} to ${final.q.toFixed(2)}.`
      : `No proposed edit lowered q(M) with ${est.label}, so the Stage 2 model stays the final specification (q(M) = ${selected.q.toFixed(2)}).`
  };
}

function buildInterpretStage(demo, voc, run, estimator) {
  const est = estimatorInfo(estimator);
  const final = run.final;
  const baseline = run.candidates.find((c) => c.label === "Edge-only baseline");
  const selected = run.candidates.find((c) => c.label === run.selected);
  const accepted = (run.rounds || []).filter((r) => r.accepted);
  const attrCount = new Set(final.terms.map((t) => termAttr(t)).filter(Boolean)).size;
  const readings = readingsFor(final, voc, demo.directed);
  const headline = headlineFor(final, voc, readings);
  const summary = final.terms.length <= 1
    ? `With ${est.label}, no proposed mechanism produced an eligible model with a lower q(M) than the edge-only baseline, so the final specification contains only the baseline tie rate and supports no mechanism-specific claim.`
    : readings.filter((r) => r.term !== "edges").map((r) => r.reading).join(" ");
  const limitations = limitationsFor(demo, run, voc);
  const coefLines = (final.coefficients || []).map((c) => `  ${c.term.padEnd(28)} coef = ${signed(c.estimate)}`);

  const prompt = [
    "system:", "You are an ERGM expert writing for a non-specialist. Return JSON only.", "", "user:",
    `Final specification (fixed after Stage 3; ${est.label} estimates; do not change it):`,
    ...coefLines,
    `GOF: q(M) = ${final.q.toFixed(2)} over 100 simulated networks; density check passed.`,
    `Revision history: ${(run.rounds || []).length} edits proposed, ${accepted.length} accepted.`,
    "", "Map each retained term to its tie-formation mechanism and explain the",
    "fitted association conditional on the other terms, using the sign of",
    "each coefficient. List limitations separately. Make no causal claims."
  ].join("\n");

  return {
    id: "interpret", number: "4", rail: "Interpret", subtitle: "Term-linked summary", kicker: "Stage 4", title: "Plain-Language Explanation",
    status: "Stage 4: interpretation", lens: "final interpretation",
    mechanismTitle: "What the fitted model supports", mechanismCopy: summary,
    metrics: [[String(final.terms.length), "terms"], [String(attrCount), "attributes"], [String(limitations.length), "limitations"], ["0", "causal claims"]],
    terms: final.terms,
    rounds: (run.rounds || []).map(roundRecord),
    guardrails: guardrailSets.interpret,
    chartTitle: "GOF discrepancy q(M) · lower is better", chartLabel: "final",
    bic: [["Edge-only", baseline && baseline.eligible ? baseline.q : null, baseline && baseline.eligible ? "eligible" : "ineligible"], [`${run.selected} (M0)`, selected.q, "eligible"], ["Final (M_T)", final.q, "selected"]],
    prompt,
    output: JSON.stringify({ headline, term_interpretations: readings, summary, limitations }, null, 2),
    outputBadge: "interpretation JSON", highlight: "final",
    theoryHeadline: headline, theory: summary
  };
}

function buildStages(demo, voc, estimator) {
  const run = runRecords[demo.id][estimator];
  const stages = [buildIntakeStage(demo, voc), buildLibraryStage(demo, voc), buildSpecStage(demo, voc, run)];
  if (run && run.selected) {
    stages.push(buildFitStage(demo, voc, run, estimator), buildReviseStage(demo, voc, run, estimator), buildInterpretStage(demo, voc, run, estimator));
  }
  return stages;
}

// ---------------------------------------------------------------- assemble demos

const networkDemos = Object.keys(networkData).map((id) => {
  const data = networkData[id];
  const voc = networkText[id];
  const demo = {
    id,
    shortLabel: voc.shortLabel,
    title: voc.title,
    directed: Boolean(data.directed),
    attrs: data.attrs,
    nodeKind: voc.nodeKind,
    tieKind: voc.tieKind,
    cohortPrefix: voc.cohortPrefix,
    hubThreshold: voc.hubThreshold,
    palette: voc.palette,
    nodes: data.nodes.map((n) => ({ ...n })),
    edges: makeEdges(data.edges),
    closureEdges: [],
    bridgeEdges: [],
    stages: []
  };
  demo.nodeById = Object.fromEntries(demo.nodes.map((node) => [node.id, node]));
  const computed = graphDiagnostics(demo);
  demo.adjacency = computed.adjacency;
  demo.degreeById = computed.degreeById;
  demo.diagnostics = computed.diagnostics;
  demo.closureSet = makeKeySet([]);
  demo.bridgeSet = makeKeySet([]);
  demo.stagesByEstimator = {};
  ESTIMATORS.forEach((est) => {
    demo.stages = buildStages(demo, voc, est.id);
    hydrateIntakeStage(demo);
    demo.stagesByEstimator[est.id] = demo.stages;
  });
  demo.stages = demo.stagesByEstimator[ESTIMATORS[0].id];
  return demo;
});

let activeNetwork = 0;
let activeStage = 0;
let activeEstimator = ESTIMATORS[0].id;
let replayTimer = null;

const svg = document.getElementById("network-svg");
const networkPicker = document.getElementById("network-picker");
const estimatorPicker = document.getElementById("estimator-picker");
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
  const demo = currentDemo();
  if (demo.stagesByEstimator && demo.stagesByEstimator[activeEstimator]) return demo.stagesByEstimator[activeEstimator];
  return demo.stages;
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
  if (estimatorPicker) {
    estimatorPicker.replaceChildren();
    const demo = currentDemo();
    ESTIMATORS.forEach((est) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = `network-button${est.id === activeEstimator ? " active" : ""}`;
      button.textContent = est.label;
      button.title = est.long;
      button.disabled = !(demo.stagesByEstimator && demo.stagesByEstimator[est.id]);
      button.setAttribute("aria-pressed", est.id === activeEstimator ? "true" : "false");
      button.addEventListener("click", () => setEstimator(est.id));
      estimatorPicker.appendChild(button);
    });
  }
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
  if (demo.directed) {
    const item = document.createElement("span");
    item.textContent = "arrow: A seeks advice from B";
    networkLegend.appendChild(item);
  }
}

function renderNetwork(stage) {
  const demo = currentDemo();
  svg.replaceChildren();

  if (demo.directed) {
    const defs = svgEl("defs");
    const marker = svgEl("marker", { id: "arrow-head", markerWidth: "9", markerHeight: "9", refX: "8", refY: "4.5", orient: "auto", markerUnits: "userSpaceOnUse" });
    marker.appendChild(svgEl("path", { d: "M0,0 L9,4.5 L0,9 z", fill: "#7f96b8" }));
    defs.appendChild(marker);
    svg.appendChild(defs);
  }

  const edgeLayer = svgEl("g", { class: "edge-layer" });
  const nodeLayer = svgEl("g", { class: "node-layer" });
  const arcSet = new Set(demo.edges.map((e) => `${e.source}>${e.target}`));

  demo.edges.forEach((edge) => {
    const source = demo.nodeById[edge.source];
    const target = demo.nodeById[edge.target];
    if (!demo.directed) {
      edgeLayer.appendChild(svgEl("line", { x1: source.x, y1: source.y, x2: target.x, y2: target.y, class: "edge" }));
      return;
    }
    // directed: shorten the arc so the arrowhead ends at the node ring; offset reciprocal pairs
    const dx = target.x - source.x;
    const dy = target.y - source.y;
    const len = Math.hypot(dx, dy) || 1;
    const ux = dx / len;
    const uy = dy / len;
    const reciprocal = arcSet.has(`${edge.target}>${edge.source}`);
    const off = reciprocal ? 5 : 0;
    const px = -uy * off;
    const py = ux * off;
    const r = 18;
    edgeLayer.appendChild(svgEl("line", {
      x1: source.x + ux * r + px, y1: source.y + uy * r + py,
      x2: target.x - ux * (r + 7) + px, y2: target.y - uy * (r + 7) + py,
      class: "edge directed", "marker-end": "url(#arrow-head)"
    }));
  });

  demo.nodes.forEach((node) => {
    const group = svgEl("g", { transform: `translate(${node.x}, ${node.y})` });
    const color = colorForGroup(demo, node.group);
    group.appendChild(svgEl("circle", { r: 23 + Math.min(demo.degreeById[node.id], 5), class: "node-ring", fill: color }));
    group.appendChild(svgEl("circle", { r: 16, fill: color }));
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
    // Bar length is proportional to q(M): lower is better, so the selected model has the shortest bar.
    const width = !numeric ? 100 : Math.max(6, (100 * value) / max);
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

function setEstimator(id) {
  if (!ESTIMATORS.some((e) => e.id === id)) return;
  activeEstimator = id;
  stopReplay();
  setStage(activeStage);
}

function setStage(index) {
  const demo = currentDemo();
  const stages = currentStages();
  activeStage = Math.max(0, Math.min(index, stages.length - 1));
  const stage = stages[activeStage];
  const est = estimatorInfo(activeEstimator);

  networkTitle.textContent = demo.title;
  statusText.textContent = `${demo.shortLabel}: ${stage.status}`;
  const estLabel = stage.estimatorLabel || est.label;
  kicker.textContent = stage.id === "fit" || stage.id === "refine" || stage.id === "interpret" ? `${stage.kicker} · ${estLabel}` : stage.kicker;
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
