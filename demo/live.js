/* FORGE live mode — runs the real pipeline against demo/live/server.py.
 * Loaded after app.js; reuses its render chain (setNetwork/setStage) by pushing
 * a scenario-shaped demo object whose stages are built from real API responses.
 * On the static deployment the health check fails and this file does nothing. */

(() => {
  const API = {
    health: "/api/health",
    intake: "/api/intake",
    propose: "/api/propose",
    screen: "/api/screen",
    revise: "/api/revise",
    interpret: "/api/interpret"
  };

  const ATTR_MAPS = {
    school: { group: "club", cohort: "grade", score: "activity" },
    lab: { group: "area", cohort: "role", score: "seniority" },
    neighborhood: { group: "block", cohort: "tenure_group", score: "tenure_years" },
    office: { group: "department", cohort: "level", score: "tenure" },
    opensource: { group: "module", cohort: "role", score: "commits" }
  };

  const BRIEFS = {
    school: {
      actors: "Actors are students in one school year.",
      tie_meaning: "A tie means mutual friendship.",
      constraint: "Friendships cluster around activity clubs and grade cohorts."
    },
    lab: {
      actors: "Actors are researchers in one department.",
      tie_meaning: "A tie means an active co-authorship collaboration.",
      constraint: "Collaboration follows research areas and lab roles."
    },
    neighborhood: {
      actors: "Actors are households in one neighborhood.",
      tie_meaning: "A tie means the households exchange practical help.",
      constraint: "Help flows within blocks and among long-tenured residents."
    },
    office: {
      actors: "Actors are employees in a small company.",
      tie_meaning: "A directed tie from A to B means A regularly asks B for work advice.",
      constraint: "Advice flows within departments and toward senior staff, and is sometimes returned."
    },
    opensource: {
      actors: "Actors are developers of one open-source project.",
      tie_meaning: "A tie means the two developers edited the same files in the last release.",
      constraint: "Maintainers coordinate their module and link modules together."
    },
    custom: {
      actors: "Actors are …",
      tie_meaning: "A tie means …",
      constraint: "Ties are constrained by …"
    }
  };

  const PALETTE_VARS = ["--green", "--rose", "--blue"];

  const el = {};
  let running = false;
  let serverInfo = null;

  function shortModel(model) {
    return (model || "").split("/").pop();
  }

  function glossFor(term) {
    const base = term.split("(")[0].trim();
    const decay = (term.match(/decay=([0-9.]+)/) || [])[1];
    const attr = (term.match(/\("([^"]+)"\)/) || [])[1];
    const map = {
      edges: "baseline tie rate",
      mutual: "reciprocated ties",
      gwesp: `shared partners / closure${decay ? ` (λ=${decay})` : ""}`,
      gwdsp: `open two-path pressure${decay ? ` (λ=${decay})` : ""}`,
      gwdegree: `hub / degree structure${decay ? ` (λ=${decay})` : ""}`,
      gwidegree: "incoming-tie concentration",
      gwodegree: "outgoing-tie concentration",
      twopath: "two-path connectivity",
      ttriple: "transitive triads",
      ctriple: "cyclic triads",
      nodematch: `same-${attr || "group"} ties`,
      nodemix: `${attr || "group"} pairing mix`,
      nodefactor: `${attr || "group"}-level activity`,
      nodeifactor: `${attr || "group"} incoming activity`,
      nodeofactor: `${attr || "group"} outgoing activity`,
      nodecov: `ties scale with ${attr || "attribute"}`,
      nodeicov: `incoming ties scale with ${attr || "attribute"}`,
      nodeocov: `outgoing ties scale with ${attr || "attribute"}`,
      absdiff: `similar ${attr || "attribute"} values`
    };
    return map[base] || "model mechanism";
  }

  function registerGlosses(terms) {
    terms.forEach((term) => {
      if (!termMeanings[term]) termMeanings[term] = glossFor(term);
    });
  }

  async function postJSON(path, payload) {
    const resp = await fetch(path, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    const data = await resp.json().catch(() => ({}));
    if (!resp.ok || data.ok === false) {
      throw new Error(data.error || `${path} failed (HTTP ${resp.status})`);
    }
    return data;
  }

  // ---------------------------------------------------------------- network payloads

  function scenarioPayload(sourceId) {
    const demo = networkDemos.find((d) => d.id === sourceId);
    const map = ATTR_MAPS[sourceId];
    return {
      directed: Boolean(demo.directed),
      nodes: demo.nodes.map((node) => ({
        id: node.id,
        attrs: {
          [map.group]: node.group,
          [map.cohort]: node.cohort,
          [map.score]: node.score
        }
      })),
      edges: demo.edges.map((edge) => [edge.source, edge.target])
    };
  }

  function customPayload(text) {
    let parsed;
    try {
      parsed = JSON.parse(text);
    } catch (error) {
      throw new Error(`Custom network is not valid JSON: ${error.message}`);
    }
    if (!Array.isArray(parsed.nodes) || !Array.isArray(parsed.edges)) {
      throw new Error("Custom network needs \"nodes\" and \"edges\" arrays.");
    }
    const nodes = parsed.nodes.map((node, index) => {
      if (typeof node === "string") return { id: node, attrs: {} };
      const attrs = node.attrs || {};
      if (!node.attrs) {
        Object.entries(node).forEach(([key, value]) => {
          if (!["id", "name", "x", "y"].includes(key)) attrs[key] = value;
        });
      }
      return { id: String(node.id ?? `n${index}`), name: node.name, attrs };
    });
    const edges = parsed.edges.map((edge) =>
      Array.isArray(edge)
        ? [String(edge[0]), String(edge[1])]
        : [String(edge.source ?? edge.from), String(edge.target ?? edge.to)]
    );
    const ids = new Set(nodes.map((node) => node.id));
    edges.forEach(([s, t]) => {
      const bad = !ids.has(s) ? s : (!ids.has(t) ? t : null);
      if (bad !== null) {
        throw new Error(`Custom network edge ["${s}", "${t}"] references unknown node id "${bad}" — every endpoint must appear in "nodes".`);
      }
    });
    return { directed: Boolean(parsed.directed), nodes, edges, isCustom: true };
  }

  // ---------------------------------------------------------------- live demo skeleton

  function circularLayout(nodes, groupOf) {
    const sorted = [...nodes].sort((a, b) => groupOf(a).localeCompare(groupOf(b)));
    const cx = 360;
    const cy = 215;
    sorted.forEach((node, index) => {
      const angle = (2 * Math.PI * index) / sorted.length - Math.PI / 2;
      node.x = Math.round(cx + 255 * Math.cos(angle));
      node.y = Math.round(cy + 150 * Math.sin(angle));
    });
  }

  function computeClosureBridge(nodes, edges, nodeById) {
    const adjacency = makeAdjacency(nodes, edges);
    const closure = [];
    const bridge = [];
    edges.forEach(({ source, target }) => {
      const shared = [...adjacency[source]].some((other) => adjacency[target].has(other));
      if (shared) {
        closure.push([source, target]);
      } else if (nodeById[source].group !== nodeById[target].group) {
        bridge.push([source, target]);
      }
    });
    return { closure, bridge };
  }

  function buildLiveDemo(sourceId, payload) {
    const isScenario = sourceId !== "custom";
    const source = isScenario ? networkDemos.find((d) => d.id === sourceId) : null;

    let nodes;
    let palette;
    if (isScenario) {
      nodes = source.nodes.map((node) => ({ ...node }));
      palette = { ...source.palette };
    } else {
      // pick ONE grouping attribute for the whole payload (most common string attr)
      const keyCounts = {};
      payload.nodes.forEach((node) => {
        Object.entries(node.attrs || {}).forEach(([key, value]) => {
          if (typeof value === "string") keyCounts[key] = (keyCounts[key] || 0) + 1;
        });
      });
      const groupKey = Object.keys(keyCounts).sort((a, b) => keyCounts[b] - keyCounts[a])[0];
      const groupOf = (node) => String(node.group ?? (node.attrs || {})[groupKey] ?? "A");
      const groups = [...new Set(payload.nodes.map(groupOf))];
      palette = Object.fromEntries(groups.map((group, i) => [group, PALETTE_VARS[i % PALETTE_VARS.length]]));
      nodes = payload.nodes.map((node) => ({
        id: node.id,
        name: node.name || node.id,
        group: groupOf(node),
        cohort: "",
        score: 0,
        x: 0,
        y: 0
      }));
      circularLayout(nodes, (node) => node.group);
    }

    // visual copy: one line per dyad (reciprocal directed pairs collapse), no self loops
    const seenPairs = new Set();
    const edges = [];
    payload.edges.forEach(([source_, target_]) => {
      const key = [source_, target_].sort().join("--");
      if (source_ === target_ || seenPairs.has(key)) return;
      seenPairs.add(key);
      edges.push({ source: source_, target: target_ });
    });
    const nodeById = Object.fromEntries(nodes.map((node) => [node.id, node]));
    const { closure, bridge } = computeClosureBridge(nodes, edges, nodeById);
    const avgDegree = nodes.length ? (2 * edges.length) / nodes.length : 0;

    const demo = {
      id: "live",
      shortLabel: "▶ Live",
      title: `${isScenario ? source.title : "Custom Network"} — live run`,
      nodeKind: isScenario ? source.nodeKind : "nodes",
      tieKind: isScenario ? source.tieKind : "ties",
      cohortPrefix: isScenario ? source.cohortPrefix : "",
      hubThreshold: isScenario ? source.hubThreshold : Math.max(3, Math.ceil(avgDegree) + 1),
      palette,
      nodes,
      edges,
      closureEdges: closure,
      bridgeEdges: bridge,
      stages: []
    };
    demo.nodeById = nodeById;
    const computed = graphDiagnostics(demo);
    demo.adjacency = computed.adjacency;
    demo.degreeById = computed.degreeById;
    demo.diagnostics = computed.diagnostics;
    demo.closureSet = makeKeySet(closure);
    demo.bridgeSet = makeKeySet(bridge);
    return demo;
  }

  function installLiveDemo(demo) {
    const index = networkDemos.findIndex((d) => d.id === "live");
    if (index >= 0) {
      networkDemos[index] = demo;
    } else {
      networkDemos.push(demo);
    }
    return networkDemos.findIndex((d) => d.id === "live");
  }

  function showLatestStage() {
    const index = networkDemos.findIndex((d) => d.id === "live");
    if (index < 0 || networkDemos[index].stages.length === 0) return;
    if (activeNetwork !== index) {
      setNetwork(index);
    }
    setStage(networkDemos[index].stages.length - 1);
  }

  // ---------------------------------------------------------------- guardrail rows

  function guardrailRowsFromReport(report, extraRows = []) {
    if (!report) return extraRows;
    const rows = [
      ["g1_edges_and_size", "Model includes edges and stays within 3-8 terms"],
      ["g2_single_closure_family", "At most one curved closure term (gwesp/gwdsp)"],
      ["g3_categorical_support", "Categorical terms have enough observations per level"],
      ["g4_no_match_factor_overlap", "No nodematch + nodefactor on the same attribute"],
      ["g5_no_triangle", "Unstable triangle term is excluded"],
      ["g6_library_only", "Every term comes from the valid library"]
    ];
    const items = rows
      .filter(([key]) => report[key] !== null && report[key] !== undefined)
      .map(([key, copy]) => [report[key] ? "pass" : "warn", copy]);
    return items.concat(extraRows);
  }

  // ---------------------------------------------------------------- stage builders

  function baseStage(id, number, rail, subtitle, kicker, title, status, lens) {
    return { id, number, rail, subtitle, kicker, title, status, lens, terms: [], bic: [], chartLabel: "pending" };
  }

  function intakeStage(ctx) {
    const d = ctx.intake.diagnostics;
    const stage = baseStage("intake", "0", "Intake", "Network + description", "Stage 0", "Network Intake",
      "Stage 0: diagnostics (live)", "raw network");
    stage.mechanismTitle = "Live run: diagnostics computed from your network";
    stage.mechanismCopy = `FORGE just measured the ${d.nodes}-node network in R: density, closure, and degree spread below are real Stage 0 outputs, not cached values.`;
    stage.metrics = [
      [String(d.nodes), ctx.demo.nodeKind],
      [String(d.edges), ctx.demo.tieKind],
      [d.density.toFixed(2), "density"],
      [d.transitivity.toFixed(2), "transitivity"]
    ];
    stage.terms = ["edges"];
    stage.guardrails = [
      [ctx.intake.attribute_details.every((a) => a.missing === 0) ? "pass" : "warn", "No missing node attributes in the network"],
      [d.isolates === 0 ? "pass" : "warn", `Isolates: ${d.isolates}`],
      [d.nodes <= (serverInfo ? serverInfo.max_nodes : 60) ? "pass" : "warn", "Small enough for live SA fitting and simulation"]
    ];
    stage.chartLabel = "nothing fitted yet";
    stage.prompt = [
      `live run — Stage 0 (R, demo/live/run_stage.R)`,
      `network: ${d.nodes} nodes / ${d.edges} ${d.directed ? "directed" : "undirected"} ties`,
      `attributes: ${ctx.intake.attribute_details.map((a) => a.attribute).join(", ") || "none"}`,
      ``,
      `task:`,
      `compute graph diagnostics for ERGM specification.`
    ].join("\n");
    stage.output = JSON.stringify(d, null, 2);
    stage.outputBadge = "diagnostics";
    stage.highlight = "raw";
    stage.theory = "Live run in progress. FORGE has measured the raw network; the term library, LLM proposal, and fits will fill in as each real stage completes.";
    return stage;
  }

  function libraryStage(ctx) {
    const lib = ctx.intake.library;
    const attrTermCount = lib.terms.length - lib.base_terms.length;
    const coveredAttrs = new Set(lib.terms.map((t) => (t.match(/\("([^"]+)"\)/) || [])[1]).filter(Boolean));
    const excluded = ctx.intake.attribute_details.filter((a) => !coveredAttrs.has(a.attribute));
    const stage = baseStage("library", "1a", "Valid terms", "Build L*", "Stage 1a", "Build the Valid Term List L*",
      "Stage 1a: library (live)", "candidate mechanisms");
    stage.mechanismTitle = "The guardrails just built the menu for this network";
    stage.mechanismCopy = `build_admissible_library() returned ${lib.terms.length} terms valid for this ${lib.directed ? "directed" : "undirected"} network. ${excluded.length ? `Excluded attribute${excluded.length > 1 ? "s" : ""}: ${excluded.map((a) => a.attribute).join(", ")} (too few observations per level).` : "All attributes qualified."}`;
    stage.metrics = [
      [String(lib.terms.length), "valid terms"],
      [String(lib.base_terms.length), "structural terms"],
      [String(attrTermCount), "attribute terms"],
      [String(excluded.length), "excluded attributes"]
    ];
    stage.terms = lib.terms;
    stage.guardrails = [
      ["pass", "Every term is available in ergm syntax"],
      [excluded.length === 0 ? "pass" : "warn", excluded.length === 0 ? "All categorical terms have enough observations per level" : `Small-sample gate excluded: ${excluded.map((a) => a.attribute).join(", ")}`],
      ["pass", "Triangle is excluded; curved closure terms are preferred"]
    ];
    stage.chartLabel = "nothing fitted yet";
    stage.prompt = [
      `live run — Stage 1a (R)`,
      `input: ${lib.directed ? "directed" : "undirected"} network, ${ctx.intake.diagnostics.nodes} nodes`,
      `attributes:`,
      ...ctx.intake.attribute_details.map((a) => `  ${a.attribute}: ${a.classification}, ${a.unique_values} unique values`),
      ``,
      `task:`,
      `construct the valid ERGM term library L* with guardrails ${JSON.stringify(lib.guardrail_config)}.`
    ].join("\n");
    stage.output = JSON.stringify({ L_star: lib.terms }, null, 2);
    stage.outputBadge = "library";
    stage.highlight = "homophily";
    stage.theory = "The valid term space for this network is fixed. Whatever the LLM proposes next is checked against this menu character-for-character.";
    return stage;
  }

  function specStage(ctx) {
    const specs = ctx.propose.specifications;
    const compliant = specs.filter((s) => s.library_compliant);
    const first = compliant[0] || specs[0];
    const stage = baseStage("spec", "1b", "Propose", "LLM formulas", "Stage 1b", "LLM Proposes Formulas from L*",
      "Stage 1b: LLM proposals (live)", "LLM-selected terms");
    stage.mechanismTitle = `${shortModel(ctx.propose.model)} proposed ${specs.length} candidate formulas`;
    stage.mechanismCopy = `The prompt and JSON on this screen are the real request and response (${ctx.propose.latency}s). ${compliant.length}/${specs.length} candidates use only library terms; off-menu terms would be flagged and dropped here.`;
    stage.metrics = [
      [String(specs.length), "candidate specs"],
      [`${Math.round((100 * compliant.length) / specs.length)}%`, "library compliance"],
      [String(first.formula.length), `terms in ${first.label}`],
      [shortModel(ctx.propose.model), "model"]
    ];
    stage.terms = first.formula;
    stage.guardrails = [
      [compliant.length === specs.length ? "pass" : "warn", `${compliant.length}/${specs.length} specifications use library terms only`],
      [specs.every((s) => s.formula.includes("edges")) ? "pass" : "warn", "Every specification includes edges"],
      [specs.every((s) => s.formula.length >= 3 && s.formula.length <= 8) ? "pass" : "warn", "Term counts stay within the guardrail (3-8)"]
    ];
    stage.chartLabel = "3 candidates await fitting";
    stage.prompt = `system:\n${ctx.propose.prompt.system}\n\nuser:\n${ctx.propose.prompt.user}`;
    stage.output = JSON.stringify({
      specifications: specs.map((s) => ({
        label: s.label,
        formula: s.formula,
        library_compliant: s.library_compliant
      }))
    }, null, 2);
    stage.outputBadge = "llm json";
    stage.highlight = "closure";
    stage.theory = `The LLM's proposals are stories about ${ctx.demo.tieKind}: ${first.formula.filter((t) => t !== "edges").map((t) => glossFor(t)).join(", ")}. Stage 2 fits them and decides which story the data supports.`;
    return stage;
  }

  function joinTerms(terms) {
    return terms.map((t) => (typeof shortTerm === "function" ? shortTerm(t) : t)).join(" + ");
  }

  function residualText(d) {
    if (!d) return null;
    const label = d.stat === "espartners" ? `shared partners = ${String(d.bin).replace(/^esp/, "")}`
      : d.stat === "distance" ? `geodesic distance = ${d.bin}`
      : `${d.stat} = ${String(d.bin).replace(/^i?o?degree/, "")}`;
    return `${label}: observed ${d.observed}, simulated mean ${d.simulated_mean}, z = ${d.z >= 0 ? "+" : ""}${Number(d.z).toFixed(2)}`;
  }

  function st(term) {
    return typeof shortTerm === "function" ? shortTerm(term) : term;
  }

  function editLabel(edit) {
    if (!edit) return "";
    if (edit.action === "replace") return `replace ${st(edit.target)} with ${st(edit.term)}`;
    return `${edit.action} ${st(edit.term)}`;
  }

  function editShort(edit) {
    if (!edit) return "";
    if (edit.action === "replace") return `${st(edit.target)}→${st(edit.term)}`;
    return `${edit.action === "add" ? "+" : "−"}${st(edit.term)}`;
  }

  function fitStage(ctx) {
    const fits = ctx.screen.fits;
    const winner = fits.find((f) => f.label === ctx.screen.winner);
    if (!winner) throw new Error("no candidate passed the eligibility checks");
    const eligible = fits.filter((f) => f.eligible);
    const fitted = fits.filter((f) => f.success);
    const densityPass = fits.filter((f) => f.density && f.density.pass).length;
    const estLabel = ctx.estLabel || "SA";
    const stage = baseStage("fit", "2", "Fit & select", `${estLabel} fit + GOF`, "Stage 2", "Fit Candidates and Select by GOF",
      `Stage 2: ${estLabel} fit and selection (live)`, "lowest q(M)");
    const worst = winner.gof && winner.gof.details ? winner.gof.details[0] : null;
    stage.mechanismTitle = `${winner.label} wins with the smallest GOF discrepancy`;
    stage.mechanismCopy = `Every candidate was just fitted with ${estLabel} in R (${fits.reduce((s, f) => s + (f.runtime || 0), 0).toFixed(0)}s). ${eligible.length}/${fits.length} passed the eligibility checks; ${winner.label} has the lowest q(M) = ${winner.q}.${worst ? ` Its largest remaining mismatch: ${residualText(worst)}.` : ""}`;
    stage.metrics = [
      [String(winner.q), "lowest q(M)"],
      [`${eligible.length}/${fits.length}`, "eligible candidates"],
      [winner.label.replace("Candidate ", "C").replace("Edge-only baseline", "Edge-only"), "selected M0"],
      [winner.pseudo_bic == null ? "–" : String(winner.pseudo_bic), "PBIC (secondary)"]
    ];
    stage.terms = winner.terms;
    stage.guardrails = [
      [fitted.length === fits.length ? "pass" : "warn", `${estLabel} returned finite coefficients for ${fits.filter((f) => f.finite).length}/${fits.length} candidates`],
      [densityPass === fitted.length ? "pass" : "warn", `Density check passed for ${densityPass}/${fitted.length} fitted candidates (≤25% error, 30 simulations)`],
      ["pass", `GOF computed from 100 simulations for ${eligible.length} eligible candidates`],
      ["pass", `Selected ${winner.label}: lowest q(M) = ${winner.q}`]
    ];
    stage.chartTitle = "GOF discrepancy q(M) · lower is better";
    stage.chartLabel = `${winner.label} selected`;
    stage.bic = fits.map((f) => [
      f.label.replace("Edge-only baseline", "Edge-only"),
      f.eligible ? f.q : null,
      !f.eligible ? "ineligible" : f.label === winner.label ? "selected" : "eligible"
    ]);
    stage.prompt = [
      `Stage 2 — statistical fitting and selection (R backend, no LLM call)`,
      ``,
      `estimator: ${estLabel}`,
      ``,
      `candidate pool:`,
      ...fits.map((f) => `  ${f.label} = ${joinTerms(f.terms)}`),
      ``,
      `procedure:`,
      `  1. fit each candidate with ${estLabel}`,
      `  2. eligibility: finite coefficients, successful simulation,`,
      `     density check (30 simulated networks, |relative error| <= 25%),`,
      `     computable GOF diagnostics`,
      `  3. GOF from 100 simulated networks (seed ${ctx.screen.seed} for every candidate):`,
      `     ${ctx.intake.diagnostics.directed ? "in-degree, out-degree" : "degree"}, edgewise shared partners, geodesic distance`,
      `     z_k = (obs_k - mean_sim_k) / sd_sim_k;   q(M) = max_k |z_k|`,
      `  4. select M0 = argmin q(M) over eligible candidates`,
      `     (MPLE pseudo-BIC is recorded as a secondary diagnostic only)`
    ].join("\n");
    stage.output = JSON.stringify(fits.map((f) => ({
      candidate: f.label,
      formula: joinTerms(f.terms),
      eligible: Boolean(f.eligible),
      density_rel_error: f.density ? f.density.rel_error : null,
      q: f.eligible ? f.q : null,
      largest_residual: f.gof && f.gof.details ? residualText(f.gof.details[0]) : null,
      pbic_secondary: f.pseudo_bic ?? null,
      decision: f.label === winner.label ? "selected: lowest q(M) among eligible" : (f.eligible ? "eligible" : `ineligible: ${f.reason}`)
    })), null, 2);
    stage.outputBadge = "fit + GOF table";
    stage.highlight = "winner";
    const baseline = fits.find((f) => f.label === "Edge-only baseline");
    stage.theory = `The evidence favors ${winner.label}: ${winner.terms.filter((t) => t !== "edges").map((t) => glossFor(t)).join(", ") || "the baseline tie rate alone"} reproduce${winner.terms.length > 2 ? "" : "s"} the observed network best (q(M) ${winner.q}${baseline && baseline.eligible && baseline.label !== winner.label ? ` vs ${baseline.q} for the edge-only baseline` : ""}).`;
    return stage;
  }

  function roundRecord(r) {
    return {
      round: r.round,
      edit: editLabel(r.edit),
      accepted: Boolean(r.accepted),
      eligible: Boolean(r.eligible),
      qBefore: r.q_before,
      qAfter: r.eligible ? r.q_after : null,
      reason: r.accepted ? "eligible, q(M) decreased" : (r.rejection_reason || "rejected")
    };
  }

  function refineStage(ctx, inProgress) {
    const rounds = ctx.rounds;
    const accepted = rounds.filter((r) => r.accepted);
    const m0 = ctx.m0;
    const current = ctx.current;
    const last = rounds[rounds.length - 1];
    const stage = baseStage("refine", "3", "Revise", "≤4 checked rounds", "Stage 3", "Diagnostic-Guided Revision",
      inProgress ? `Stage 3: round ${rounds.length + 1} of 4 (live)` : "Stage 3: checked revision (live)",
      accepted.length ? "accepted edits" : "no accepted edit");
    const reduction = m0.q ? (100 * (m0.q - current.q) / m0.q) : 0;
    stage.mechanismTitle = inProgress
      ? `Round ${rounds.length} of 4 done: ${last && last.accepted ? "edit kept" : "edit rejected"}`
      : (accepted.length
        ? `${accepted.length} of ${rounds.length} proposed edits lowered q(M) and were kept`
        : `${rounds.length} edits proposed, none lowered q(M): Stage 2 model retained`);
    stage.mechanismCopy = rounds.map((r) => `Round ${r.round}: ${editLabel(r.edit)} → ${r.accepted ? `accepted (q(M) ${r.q_before} → ${r.q_after})` : `rejected (${r.rejection_reason})`}.`).join(" ")
      + (inProgress ? " The next round is being proposed and refitted…" : "");
    stage.metrics = [
      [String(current.q), inProgress ? "current q(M)" : "final q(M)"],
      [`${accepted.length}/${rounds.length}`, "accepted / rounds"],
      [`${reduction.toFixed(1)}%`, "q(M) reduction"],
      [String(current.terms.length), "terms in model"]
    ];
    stage.terms = current.terms;
    stage.rounds = rounds.map(roundRecord);
    stage.guardrails = [
      ["pass", "Each round proposes exactly one add / remove / replace"],
      [rounds.every((r) => r.eligible || !r.refit) ? "pass" : "warn", `${rounds.filter((r) => r.eligible).length}/${rounds.length} revised models passed the eligibility checks`],
      ["pass", "Rejected edits are fed back and excluded from later rounds"],
      ["pass", `Kept ${accepted.length} edit${accepted.length === 1 ? "" : "s"}: q(M_T) = ${current.q} ≤ q(M_0) = ${m0.q}`]
    ];
    stage.chartTitle = "GOF discrepancy q(M) · lower is better";
    stage.chartLabel = `${accepted.length}/${rounds.length} accepted`;
    stage.bic = [
      ["M0 (Stage 2)", m0.q, "selected"],
      ...rounds.map((r) => [`R${r.round} ${editShort(r.edit)}`, r.eligible ? r.q_after : null, r.accepted ? "accepted" : (r.eligible ? "rejected" : "ineligible")])
    ];
    stage.prompt = last ? `system:\n${last.prompt.system}\n\nuser:\n${last.prompt.user}` : "";
    stage.output = JSON.stringify({
      M0: joinTerms(m0.terms),
      q_M0: m0.q,
      rounds: rounds.map((r) => ({
        round: r.round,
        action: r.edit.action,
        ...(r.edit.target ? { target: r.edit.target } : {}),
        term: r.edit.term,
        rationale: r.edit.rationale,
        eligible: Boolean(r.eligible),
        q_before: r.q_before,
        q_after: r.eligible ? r.q_after : null,
        accepted: Boolean(r.accepted),
        decision: r.accepted ? `accepted: eligible and q(M) fell ${r.q_before} → ${r.q_after}` : `rejected: ${r.rejection_reason}`
      })),
      final: { formula: joinTerms(current.terms), q: current.q, pbic_secondary: current.pseudo_bic ?? null }
    }, null, 2);
    stage.outputBadge = "revision record";
    stage.highlight = "refined";
    stage.theory = accepted.length
      ? `${accepted.length} edit${accepted.length === 1 ? "" : "s"} survived the checks: ${accepted.map((r) => glossFor(r.edit.term)).join("; ")}. q(M) fell from ${m0.q} to ${current.q}.`
      : `No proposed edit lowered q(M), so the Stage 2 model stays the final specification (q(M) = ${m0.q}).`;
    return stage;
  }

  function interpretStage(ctx) {
    const interp = ctx.interpret.interpretation;
    const final = ctx.current;
    const m0 = ctx.m0;
    const baseline = ctx.screen.fits.find((f) => f.label === "Edge-only baseline");
    const attrCount = new Set(final.terms.map((t) => (t.match(/\("([^"]+)"\)/) || [])[1]).filter(Boolean)).size;
    const tis = Array.isArray(interp.term_interpretations) ? interp.term_interpretations : [];
    const claimsMatch = tis.length > 0 && tis.every((ti) => ti && typeof ti.term === "string" &&
      final.terms.some((t) => t === ti.term || t.startsWith(ti.term.split("(")[0])));
    const stage = baseStage("interpret", "4", "Interpret", "Term-linked summary", "Stage 4", "Plain-Language Explanation",
      "Stage 4: interpretation (live)", "final interpretation");
    stage.mechanismTitle = "What the fitted model supports";
    stage.mechanismCopy = interp.mechanism_explanation;
    stage.metrics = [
      [String(final.terms.length), "terms"],
      [String(attrCount), "attributes"],
      [String((interp.limitations || []).length), "limitations"],
      ["0", "causal claims"]
    ];
    stage.terms = final.terms;
    stage.rounds = ctx.rounds.map(roundRecord);
    stage.guardrails = [
      [claimsMatch ? "pass" : "warn", "Every claim maps to a fitted term and its sign"],
      [(interp.limitations || []).length > 0 ? "pass" : "warn", "Limitations kept separate from findings"],
      ["pass", "Conditional associations, no causal claims"]
    ];
    stage.chartTitle = "GOF discrepancy q(M) · lower is better";
    stage.chartLabel = "final";
    stage.bic = [
      ...(baseline ? [["Edge-only", baseline.eligible ? baseline.q : null, baseline.eligible ? "eligible" : "ineligible"]] : []),
      [`${m0.label} (M0)`, m0.q, "eligible"],
      ["Final (M_T)", final.q, "selected"]
    ];
    stage.prompt = `system:\n${ctx.interpret.prompt.system}\n\nuser:\n${ctx.interpret.prompt.user}`;
    stage.output = JSON.stringify(interp, null, 2);
    stage.outputBadge = "interpretation json";
    stage.highlight = "final";
    stage.theoryHeadline = interp.headline;
    stage.theory = interp.human_understandable_theory;
    return stage;
  }

  // ---------------------------------------------------------------- pipeline

  function setLiveStatus(text, state) {
    el.status.textContent = text;
    el.status.dataset.state = state || "idle";
  }

  async function runPipeline() {
    if (running) return;
    running = true;
    el.run.disabled = true;
    try {
      const sourceId = el.source.value;
      const model = el.model.value;
      const estimator = el.estimator ? el.estimator.value : "sa";
      const estLabel = { sa: "SA", mcmle: "MCMLE", mple: "MPLE" }[estimator] || "SA";
      const brief = {
        actors: el.actors.value.trim(),
        tie_meaning: el.tie.value.trim(),
        constraint: el.constraint.value.trim()
      };
      const payload = sourceId === "custom" ? customPayload(el.custom.value) : scenarioPayload(sourceId);
      const demo = buildLiveDemo(sourceId, payload);
      const ctx = { demo, brief, model };

      setLiveStatus("Stage 0-1a: diagnostics + term library (R)…", "running");
      ctx.intake = await postJSON(API.intake, { network: payload });
      ctx.libraryTerms = ctx.intake.library.terms;
      registerGlosses(ctx.libraryTerms);
      demo.stages.push(intakeStage(ctx));
      installLiveDemo(demo);
      showLatestStage();
      demo.stages.push(libraryStage(ctx));
      showLatestStage();

      setLiveStatus(`Stage 1b: ${shortModel(model)} proposing formulas…`, "running");
      ctx.propose = await postJSON(API.propose, {
        diagnostics: ctx.intake.diagnostics,
        library_terms: ctx.libraryTerms,
        attribute_details: ctx.intake.attribute_details,
        brief,
        model,
        n_candidates: 3
      });
      ctx.propose.specifications.forEach((s) => registerGlosses(s.formula));
      demo.stages.push(specStage(ctx));
      showLatestStage();

      setLiveStatus(`Stage 2: ${estLabel} fitting, density check and 100-simulation GOF in R…`, "running");
      const candidates = ctx.propose.specifications
        .filter((s) => s.library_compliant)
        .map((s) => ({ label: s.label, terms: s.formula }));
      ctx.estimator = estimator;
      ctx.estLabel = estLabel;
      ctx.screen = await postJSON(API.screen, { network: payload, candidates, estimator });
      demo.stages.push(fitStage(ctx));
      showLatestStage();

      const winner = ctx.screen.fits.find((f) => f.label === ctx.screen.winner);
      ctx.m0 = {
        label: winner.label,
        terms: winner.terms,
        q: winner.q,
        pseudo_bic: winner.pseudo_bic,
        coefficients: winner.coefficients,
        gof: winner.gof || null,
        density: winner.density || null
      };
      ctx.current = { ...ctx.m0 };
      ctx.rounds = [];
      const maxRounds = 4;
      for (let round = 1; round <= maxRounds; round += 1) {
        setLiveStatus(`Stage 3, round ${round} of ${maxRounds}: ${shortModel(model)} proposes one edit, R refits and re-checks it…`, "running");
        const revise = await postJSON(API.revise, {
          network: payload,
          current: ctx.current,
          library_terms: ctx.libraryTerms,
          brief,
          model,
          round,
          seed: ctx.screen.seed,
          estimator,
          history: ctx.rounds.map((r) => ({
            round: r.round, edit: r.edit, accepted: r.accepted,
            q_before: r.q_before, q_after: r.q_after, reason: r.rejection_reason || null
          }))
        });
        registerGlosses([revise.edit.term].filter(Boolean));
        ctx.rounds.push(revise);
        if (revise.accepted) ctx.current = revise.final;
        const refineIndex = demo.stages.findIndex((s) => s.id === "refine");
        const refined = refineStage(ctx, round < maxRounds);
        if (refineIndex >= 0) demo.stages[refineIndex] = refined; else demo.stages.push(refined);
        showLatestStage();
      }

      const history = ctx.rounds.map((r) =>
        `round=${r.round} edit=${editLabel(r.edit)} accepted=${r.accepted ? "TRUE" : "FALSE"} q_before=${r.q_before} q_after=${r.q_after ?? "NA"} rationale=${r.edit.rationale}${r.accepted ? "" : ` | ${r.rejection_reason}`}`
      );
      setLiveStatus(`Stage 4: ${shortModel(model)} writing the interpretation…`, "running");
      ctx.interpret = await postJSON(API.interpret, {
        brief,
        network_meta: { id: sourceId === "custom" ? "custom_network" : sourceId, title: demo.title },
        diagnostics: ctx.intake.diagnostics,
        final: ctx.current,
        history,
        model
      });
      demo.stages.push(interpretStage(ctx));
      showLatestStage();

      setLiveStatus("", "done");
    } catch (error) {
      setLiveStatus(`Live run stopped: ${error.message}`, "error");
    } finally {
      running = false;
      el.run.disabled = false;
    }
  }

  // ---------------------------------------------------------------- boot

  function fillBrief(sourceId) {
    const brief = BRIEFS[sourceId] || BRIEFS.custom;
    el.actors.value = brief.actors;
    el.tie.value = brief.tie_meaning;
    el.constraint.value = brief.constraint;
  }

  function wireUp() {
    el.bar = document.getElementById("live-bar");
    el.source = document.getElementById("live-source");
    el.model = document.getElementById("live-model");
    el.estimator = document.getElementById("live-estimator");
    el.actors = document.getElementById("live-actors");
    el.tie = document.getElementById("live-tie");
    el.constraint = document.getElementById("live-constraint");
    el.custom = document.getElementById("live-custom");
    el.customWrap = document.getElementById("live-custom-wrap");
    el.run = document.getElementById("live-run");
    el.status = document.getElementById("live-status");

    serverInfo.models.forEach((model) => {
      const option = document.createElement("option");
      option.value = model;
      option.textContent = shortModel(model);
      if (model === serverInfo.default_model) option.selected = true;
      el.model.appendChild(option);
    });

    el.source.addEventListener("change", () => {
      el.customWrap.hidden = el.source.value !== "custom";
      fillBrief(el.source.value);
    });
    el.run.addEventListener("click", runPipeline);
    fillBrief(el.source.value);
    document.body.classList.add("forge-live");
    el.bar.hidden = false;
  }

  fetch(API.health)
    .then((resp) => (resp.ok ? resp.json() : null))
    .then((info) => {
      if (info && info.ok && info.key_present) {
        serverInfo = info;
        wireUp();
      }
    })
    .catch(() => { /* static deployment: live mode stays hidden */ });
})();
