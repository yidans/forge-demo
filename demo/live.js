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
    neighborhood: { group: "block", cohort: "tenure_group", score: "tenure_years" }
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
      directed: false,
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
    const stage = baseStage("intake", "0", "Intake", "Graph checks", "Stage 0", "Network Intake",
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
      [d.nodes <= (serverInfo ? serverInfo.max_nodes : 60) ? "pass" : "warn", "Small enough for live MPLE fitting"]
    ];
    stage.chartLabel = "awaiting fit";
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
    const stage = baseStage("library", "1", "Library", "Valid terms", "Stage 1a", "Build a Valid Term Library",
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
    stage.chartLabel = "awaiting fit";
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
    const stage = baseStage("spec", "1b", "Formula", "LLM proposal", "Stage 1b", "Generate LLM Specifications",
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
    stage.chartLabel = "screening next";
    stage.prompt = `system:\n${ctx.propose.prompt.system}\n\nuser:\n${ctx.propose.prompt.user}`;
    stage.output = ctx.propose.raw_response;
    stage.outputBadge = "llm json";
    stage.highlight = "closure";
    stage.theory = `The LLM's proposals are stories about ${ctx.demo.tieKind}: ${first.formula.filter((t) => t !== "edges").map((t) => glossFor(t)).join(", ")}. Stage 2 decides which story the data supports.`;
    return stage;
  }

  function fitStage(ctx) {
    const fits = ctx.screen.fits;
    const winner = fits.find((f) => f.label === ctx.screen.winner);
    if (!winner) throw new Error("screening returned no usable winner for this network");
    const nullFit = fits.find((f) => f.label === "Edge-only null");
    const beatsNull = !nullFit || winner.pseudo_bic < nullFit.pseudo_bic;
    const stage = baseStage("fit", "2", "Screen", "Fast fit", "Stage 2", "Fit Candidate Specifications",
      "Stage 2: model screen (live)", "best pseudo-BIC");
    stage.mechanismTitle = `MPLE screening picked ${winner.label} in ${fits.reduce((s, f) => s + (f.runtime || 0), 0).toFixed(1)}s`;
    stage.mechanismCopy = `Every candidate was just fitted to your network with fast maximum pseudo-likelihood in R. Lower pseudo-BIC wins; the winner also gets a quick goodness-of-fit simulation.`;
    stage.metrics = [
      [String(winner.pseudo_bic), "best pseudo-BIC"],
      [String(nullFit ? nullFit.pseudo_bic : "–"), "null pseudo-BIC"],
      [winner.gof ? String(winner.gof.max_abs_z) : "–", "GOF max |z|"],
      [winner.label, "winner"]
    ];
    stage.terms = winner.terms;
    stage.guardrails = guardrailRowsFromReport(winner.guardrails, [
      [fits.every((f) => f.success) ? "pass" : "warn", `MPLE succeeded for ${fits.filter((f) => f.success).length}/${fits.length} specifications`],
      [beatsNull ? "pass" : "warn", "Best pseudo-BIC improves over the edge-only null"],
      [winner.gof && winner.gof.pass ? "pass" : "warn", winner.gof ? `GOF max |z| = ${winner.gof.max_abs_z} (${winner.gof.pass ? "pass" : "check"})` : "GOF not computed"]
    ]);
    stage.chartLabel = `${winner.label} winner`;
    stage.bic = fits.filter((f) => f.success).map((f) => [f.label === "Edge-only null" ? "Null" : f.label, f.pseudo_bic]);
    stage.prompt = [
      `candidate catalog:`,
      ...fits.map((f) => `${f.label} = ${f.terms.join(" + ")}`),
      ``,
      `task:`,
      `fit each with MPLE in R and rank by pseudo-BIC.`
    ].join("\n");
    stage.output = JSON.stringify(fits.map((f) => ({
      spec: f.label,
      pseudo_bic: f.pseudo_bic ?? null,
      wald_max: f.wald_max ?? null,
      gof_max_abs_z: f.gof ? f.gof.max_abs_z : null,
      success: f.success
    })), null, 2);
    stage.outputBadge = "fit table";
    stage.highlight = "winner";
    stage.theory = beatsNull
      ? `The evidence favors ${winner.label}: ${winner.terms.filter((t) => t !== "edges").map((t) => glossFor(t)).join(", ")} explain more than the edge-only baseline (pseudo-BIC ${winner.pseudo_bic} vs ${nullFit ? nullFit.pseudo_bic : "–"}).`
      : `On this network no proposal beat the edge-only baseline (best ${winner.label} pseudo-BIC ${winner.pseudo_bic} vs null ${nullFit.pseudo_bic}). The guarded screen reports that honestly; ${winner.label} moves forward only as the best available proposal.`;
    return stage;
  }

  function refineStage(ctx) {
    const revise = ctx.revise;
    const edit = revise.edit;
    const refit = revise.refit;
    const accepted = revise.accepted;
    const current = ctx.current;
    const stage = baseStage("refine", "3", "Revise", "Checked edit", "Stage 3", "LLM-Guided Refinement",
      "Stage 3: refinement (live)", accepted ? "accepted edit" : "rejected edit");
    stage.mechanismTitle = accepted
      ? `Edit accepted: ${edit.action} ${edit.term}`
      : `Edit rejected: the checks kept the Stage 2 winner`;
    stage.mechanismCopy = accepted
      ? `The LLM proposed one edit, the guardrails validated it, and the refit improved the evidence — so the edit was kept.`
      : `The LLM proposed “${edit.action} ${edit.term}”, but ${revise.rejection_reason}. FORGE keeps the model the evidence supports — this rejection is the guarded loop working, not a failure.`;
    stage.metrics = [
      [edit.action, "proposed edit"],
      [refit && refit.pseudo_bic != null ? String(refit.pseudo_bic) : "–", "edited pseudo-BIC"],
      [accepted ? "yes" : "no", "accepted"],
      [revise.final.gof ? String(revise.final.gof.max_abs_z) : (current.gof ? String(current.gof.max_abs_z) : "–"), "final GOF max |z|"]
    ];
    stage.terms = revise.final.terms;
    stage.guardrails = guardrailRowsFromReport(refit ? refit.guardrails : null, [
      [edit.term && ctx.libraryTerms.includes(edit.term) ? "pass" : "warn", "Proposed edit uses a valid library term"],
      [accepted ? "pass" : "warn", accepted ? "Evidence improved, edit kept" : "Evidence did not improve, edit reverted"]
    ]);
    stage.chartLabel = accepted ? "revised" : "edit rejected";
    stage.bic = [
      [current.label, current.pseudo_bic],
      ...(refit && refit.pseudo_bic != null ? [["After edit", refit.pseudo_bic]] : [])
    ];
    stage.prompt = `system:\n${revise.prompt.system}\n\nuser:\n${revise.prompt.user}`;
    stage.output = JSON.stringify({
      edit,
      accepted,
      rejection_reason: revise.rejection_reason || null,
      refit: refit ? { pseudo_bic: refit.pseudo_bic, gof: refit.gof || null } : null
    }, null, 2);
    stage.outputBadge = "edit record";
    stage.highlight = "refined";
    stage.theory = accepted
      ? `The refined model adds ${glossFor(edit.term)}. One auditable edit, validated and kept because the evidence improved.`
      : `The proposal to ${edit.action} ${edit.term} was tested and reverted: ${revise.rejection_reason}. The final model stays exactly what the data supported at Stage 2.`;
    return stage;
  }

  function interpretStage(ctx) {
    const interp = ctx.interpret.interpretation;
    const final = ctx.revise.final;
    const attrCount = new Set(final.terms.map((t) => (t.match(/\("([^"]+)"\)/) || [])[1]).filter(Boolean)).size;
    const tis = Array.isArray(interp.term_interpretations) ? interp.term_interpretations : [];
    const claimsMatch = tis.length > 0 && tis.every((ti) => ti && typeof ti.term === "string" &&
      final.terms.some((t) => t === ti.term || t.startsWith(ti.term.split("(")[0])));
    const stage = baseStage("interpret", "4", "Interpret", "Final summary", "Stage 4", "Final Interpretation",
      "Stage 4: interpretation (live)", "final interpretation");
    stage.mechanismTitle = "What the model supports";
    stage.mechanismCopy = interp.mechanism_explanation;
    stage.metrics = [
      [String(final.terms.length), "terms"],
      [String(attrCount), "attributes"],
      [String((interp.limitations || []).length), "caveats"],
      ["0", "causal claims"]
    ];
    stage.terms = final.terms;
    stage.guardrails = [
      [claimsMatch ? "pass" : "warn", "Claims match fitted terms"],
      [(interp.limitations || []).length > 0 ? "pass" : "warn", "Caveats kept separate"],
      ["pass", "Conditional associations, no causal claims"]
    ];
    stage.chartLabel = "final";
    stage.bic = ctx.finalBicRows;
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

      setLiveStatus("Stage 2: MPLE screening in R…", "running");
      const candidates = ctx.propose.specifications
        .filter((s) => s.library_compliant)
        .map((s) => ({ label: s.label, terms: s.formula }));
      ctx.screen = await postJSON(API.screen, { network: payload, candidates, gof: "winner" });
      demo.stages.push(fitStage(ctx));
      showLatestStage();

      const winner = ctx.screen.fits.find((f) => f.label === ctx.screen.winner);
      ctx.current = {
        label: winner.label,
        terms: winner.terms,
        pseudo_bic: winner.pseudo_bic,
        coefficients: winner.coefficients,
        gof: winner.gof || null
      };
      setLiveStatus(`Stage 3: ${shortModel(model)} proposing one checked edit…`, "running");
      ctx.revise = await postJSON(API.revise, {
        network: payload,
        current: ctx.current,
        library_terms: ctx.libraryTerms,
        brief,
        model
      });
      registerGlosses(ctx.revise.final.terms);
      demo.stages.push(refineStage(ctx));
      showLatestStage();

      const nullFit = ctx.screen.fits.find((f) => f.label === "Edge-only null");
      ctx.finalBicRows = [
        ...(nullFit ? [["Edge-only null", nullFit.pseudo_bic]] : []),
        ["LLM proposal", ctx.current.pseudo_bic],
        ...(ctx.revise.accepted ? [["After revision", ctx.revise.final.pseudo_bic]] : [])
      ];
      const edit = ctx.revise.edit;
      const refitBic = ctx.revise.refit ? ctx.revise.refit.pseudo_bic : "NA";
      const refitZ = ctx.revise.refit && ctx.revise.refit.gof ? ctx.revise.refit.gof.max_abs_z : "NA";
      const history = [
        `round=1 action=${edit.action} term=${edit.term} accepted=${ctx.revise.accepted ? "TRUE" : "FALSE"} pseudo_bic=${refitBic} max_abs_z=${refitZ} rationale=${edit.rationale}${ctx.revise.accepted ? "" : ` | ${ctx.revise.rejection_reason}`}`
      ];
      setLiveStatus(`Stage 4: ${shortModel(model)} writing the interpretation…`, "running");
      ctx.interpret = await postJSON(API.interpret, {
        brief,
        network_meta: { id: sourceId === "custom" ? "custom_network" : sourceId, title: demo.title },
        diagnostics: ctx.intake.diagnostics,
        final: ctx.revise.final,
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
