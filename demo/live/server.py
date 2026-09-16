#!/usr/bin/env python3
"""FORGE live-demo server.

Serves the static demo plus a small pipeline API that runs the real FORGE
stages on a user-supplied network: R builds the valid term library, fits
candidates with stochastic approximation (SA), applies the density check and
the simulation-based GOF discrepancy q(M), and selects the eligible candidate
with the lowest q(M); the LLM (via OpenRouter) proposes specifications,
proposes one edit per revision round (at most four), and writes the final
interpretation. An edit is kept only if the refitted model is eligible and
strictly lowers q(M).

Usage:  python3 demo/live/server.py [--port 8765]
Requires: Rscript on PATH, OPENROUTER_API_KEY in <repo>/.env or the environment.
"""

import argparse
import json
import re
import subprocess
import threading
import time
import urllib.error
import urllib.request
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

LIVE_DIR = Path(__file__).resolve().parent
DEMO_DIR = LIVE_DIR.parent
FORGE_ROOT = DEMO_DIR.parent

ALLOWED_MODELS = [
    "anthropic/claude-haiku-4.5",
    "openai/gpt-4o-mini",
    "google/gemini-2.5-flash",
    "anthropic/claude-sonnet-4.5",
]
DEFAULT_MODEL = ALLOWED_MODELS[0]
LLM_TIMEOUT = 120
R_TIMEOUT = 900
MAX_ROUNDS = 4
DEFAULT_SEED = 42
MAX_NODES = 60
ESTIMATOR_LABELS = {"sa": "SA", "mcmle": "MCMLE", "mple": "MPLE"}
MAX_EDGES = 400
LIBRARY_OPTIONS = {"min_expected_cell": 3}

_r_lock = threading.Lock()


def load_api_key():
    import os
    key = os.environ.get("OPENROUTER_API_KEY", "").strip()
    if key:
        return key
    env_file = FORGE_ROOT / ".env"
    if env_file.exists():
        for line in env_file.read_text().splitlines():
            line = line.strip()
            if line.startswith("OPENROUTER_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")
    return ""


API_KEY = load_api_key()


class ApiError(Exception):
    def __init__(self, message, status=400):
        super().__init__(message)
        self.status = status


# ---------------------------------------------------------------- R bridge

def run_r(job):
    with _r_lock:
        proc = subprocess.run(
            ["Rscript", str(LIVE_DIR / "run_stage.R"), json.dumps(job)],
            cwd=FORGE_ROOT, capture_output=True, text=True, timeout=R_TIMEOUT,
        )
    if proc.returncode != 0:
        raise ApiError(f"R runner failed: {proc.stderr.strip()[-800:]}", 500)
    try:
        result = json.loads(proc.stdout)
    except json.JSONDecodeError:
        raise ApiError(f"R runner returned invalid JSON: {proc.stdout[:400]}", 500)
    if not result.get("ok"):
        raise ApiError(f"R error: {result.get('error', 'unknown')}", 400)
    return result


def validate_network(network):
    if not isinstance(network, dict):
        raise ApiError("network must be an object")
    nodes = network.get("nodes")
    edges = network.get("edges")
    if not isinstance(nodes, list) or not isinstance(edges, list):
        raise ApiError("network.nodes and network.edges must be arrays")
    if len(nodes) < 4:
        raise ApiError("network needs at least 4 nodes")
    if len(nodes) > MAX_NODES:
        raise ApiError(f"live demo caps networks at {MAX_NODES} nodes")
    if len(edges) > MAX_EDGES:
        raise ApiError(f"live demo caps networks at {MAX_EDGES} edges")
    return {
        "directed": bool(network.get("directed", False)),
        "nodes": nodes,
        "edges": edges,
    }


# ---------------------------------------------------------------- LLM bridge

def strip_json_fences(text):
    text = text.strip()
    text = re.sub(r"^```(?:json)?\s*", "", text)
    text = re.sub(r"\s*```$", "", text)
    start = text.find("{")
    if start >= 0:
        try:
            _, end = json.JSONDecoder().raw_decode(text[start:])
            return text[start:start + end]
        except json.JSONDecodeError:
            pass
    return text


def call_llm(system_prompt, user_prompt, model, temperature=0.0):
    if not API_KEY:
        raise ApiError("OPENROUTER_API_KEY not configured on the server", 503)
    body = json.dumps({
        "model": model,
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt},
        ],
        "temperature": temperature,
    }).encode()
    req = urllib.request.Request(
        "https://openrouter.ai/api/v1/chat/completions",
        data=body,
        headers={
            "Authorization": f"Bearer {API_KEY}",
            "Content-Type": "application/json",
            "X-Title": "FORGE live demo",
        },
    )
    started = time.time()
    try:
        with urllib.request.urlopen(req, timeout=LLM_TIMEOUT) as resp:
            payload = json.loads(resp.read().decode())
    except urllib.error.HTTPError as err:
        detail = err.read().decode(errors="replace")[:300]
        raise ApiError(f"OpenRouter HTTP {err.code}: {detail}", 502)
    except (urllib.error.URLError, TimeoutError) as err:
        raise ApiError(f"OpenRouter unreachable: {err}", 502)
    latency = round(time.time() - started, 1)
    try:
        content = payload["choices"][0]["message"]["content"]
    except (KeyError, IndexError):
        raise ApiError(f"unexpected OpenRouter payload: {json.dumps(payload)[:300]}", 502)
    return content, latency


def call_llm_json(system_prompt, user_prompt, model, temperature=0.0):
    content, latency = call_llm(system_prompt, user_prompt, model, temperature)
    try:
        return json.loads(strip_json_fences(content)), content, latency
    except json.JSONDecodeError as err:
        retry_user = (user_prompt +
                      f"\n\nYour previous reply was not valid JSON ({err}). "
                      "Reply again with the JSON object only.")
        content2, latency2 = call_llm(system_prompt, retry_user, model, temperature)
        try:
            return json.loads(strip_json_fences(content2)), content2, latency + latency2
        except json.JSONDecodeError:
            raise ApiError(f"LLM did not return valid JSON: {content2[:300]}", 502)


def pick_model(payload):
    model = payload.get("model") or DEFAULT_MODEL
    if model not in ALLOWED_MODELS:
        raise ApiError(f"model must be one of {ALLOWED_MODELS}")
    return model


# ---------------------------------------------------------------- prompts

def format_diagnostics_line(diag):
    recip = diag.get("reciprocity")
    return (f"edges={diag['edges']}, density={diag['density']}, "
            f"degree_quantiles={diag['degree_quantiles']}, isolates={diag['isolates']}, "
            f"reciprocity={'NA' if recip is None else recip}, clustering={diag['transitivity']}")


def format_attribute_lines(attribute_details):
    return "; ".join(
        f"{a['attribute']}: classification={a['classification']}, unique={a['unique_values']}"
        for a in attribute_details
    )


def format_brief(brief):
    return (f"• Actors: {brief.get('actors', 'Not provided.')}\n"
            f"• Tie meaning: {brief.get('tie_meaning', 'Not provided.')}\n"
            f"• Constraint: {brief.get('constraint', 'Not provided.')}")


PROPOSE_SYSTEM = ("You are an expert network scientist for ERGM specification. "
                  "You have a valid term library and diagnostics. Produce valid JSON only.")


def build_propose_prompt(diag, library_terms, attribute_details, brief, n_candidates):
    user = f"""**Inputs**
• Network: directed = {'true' if diag.get('directed') else 'false'}, bipartite = false, |V| = {diag['nodes']}
• Diagnostics: {format_diagnostics_line(diag)}
• Valid term library (use these names exactly): {', '.join(library_terms)}
• Attribute types: {format_attribute_lines(attribute_details)}

**System brief (concise)**
{format_brief(brief)}

**Task**
- Propose exactly {n_candidates} candidate ERGM specifications that best explain the network formation. Label them "Candidate 1".."Candidate {n_candidates}". Each specification has 3 to 5 terms, and every term must come from the valid term library, copied character-for-character.

**Rules**
- Include edges in every specification.
- Prefer GW families; do not use triangle.
- Use at most one gwesp/gwdsp closure term per specification.
- Do not pair nodematch and nodefactor on the same attribute in one specification.
- Make the candidates meaningfully different from each other.
- Provide expected sign (+/-) for each non-edge term.

**Output JSON ONLY**
{{
  "specifications": [{{
    "label": "Candidate 1",
    "formula": ["edges", "..."],
    "expected_effects": {{"term": "+"}},
    "rationale": {{"term": "one short sentence"}}
  }}]
}}"""
    return PROPOSE_SYSTEM, user


REVISE_SYSTEM = ("You are Stage 3 of FORGE: a diagnostic-guided refinement assistant for fitted ERGMs. "
                 "You propose exactly one edit to the current model; statistical checks decide "
                 "whether it is kept. Return valid JSON only.")


def format_history(history):
    if not history:
        return "- none yet"
    lines = []
    for h in history:
        edit = h.get("edit", {})
        desc = describe_edit(edit)
        if h.get("accepted"):
            lines.append(f"- round {h.get('round')}: {desc} -> ACCEPTED, q(M) {h.get('q_before')} -> {h.get('q_after')}")
        elif h.get("q_after") is not None:
            lines.append(f"- round {h.get('round')}: {desc} -> REJECTED, q(M) {h.get('q_after')} did not fall below {h.get('q_before')}")
        else:
            lines.append(f"- round {h.get('round')}: {desc} -> REJECTED, {h.get('reason')}")
    return "\n".join(lines)


def describe_edit(edit):
    action = edit.get("action", "?")
    if action == "replace":
        return f"replace {edit.get('target')} with {edit.get('term')}"
    return f"{action} {edit.get('term')}"


def build_revise_prompt(current, library_terms, brief, history, round_no, estimator="sa"):
    coef_lines = "\n".join(
        f"- {c['term']}: estimate={c['estimate']}, SE={c['std_error']}"
        for c in current.get("coefficients", [])
    )
    gof = current.get("gof") or {}
    details = gof.get("details") or []
    residual_lines = "\n".join(
        f"  - {d['stat']} / {d['bin']}: observed {d.get('observed')}, simulated mean {d.get('simulated_mean')}, "
        f"z = {d['z']:+.2f} ({'under-simulated' if d['z'] > 0 else 'over-simulated'})"
        for d in details if isinstance(d, dict) and d.get("z") is not None
    ) or "  - none"
    numbered = "\n".join(f"{i + 1}. {t}" for i, t in enumerate(library_terms))
    rejected = [h for h in history if not h.get("accepted")]
    user = f"""Revision round {round_no} of {MAX_ROUNDS}.

Current model M_{round_no - 1}: {' + '.join(current['terms'])}
Estimator: {ESTIMATOR_LABELS.get(estimator, 'SA')}
Current GOF discrepancy q(M_{round_no - 1}) = {current.get('q')}   (largest |z| over the GOF bins; lower is better)
Largest GOF residuals (positive z = the observed count exceeds the simulated mean):
{residual_lines}
Coefficient table (SA estimates):
{coef_lines}
Secondary diagnostic: MPLE pseudo-BIC = {current.get('pseudo_bic')} (not used for acceptance)

**System brief (concise)**
{format_brief(brief)}

Valid term list L* (use these names exactly):
{numbered}

Edit history so far:
{format_history(history)}
{('Do not repeat any of the ' + str(len(rejected)) + ' rejected edit(s) above.') if rejected else ''}

**Task**
Propose exactly ONE edit to the current model: add one term, remove one term, or replace one term with another.
Tie the reason to the residuals above. The edited model is refitted with SA and re-checked; it is kept only if it
remains eligible (finite estimates, density check, computable GOF) and its q(M) is strictly lower than the current value.

**Rules**
- Never remove edges.
- The edited model must keep 3 to 8 terms.
- Any added term must be copied character-for-character from L*.

**Output JSON ONLY**
{{"action": "add" | "remove" | "replace", "term": "<term from L*>", "target": "<replace only: existing term to replace>", "rationale": "<one or two sentences tied to the residuals>"}}"""
    return REVISE_SYSTEM, user


INTERPRET_SYSTEM = ("You are Stage 4 of FORGE: an interpretation LLM for fitted ERGMs.\n"
                    "Your job is to explain social/network mechanisms, not to select terms or improve the model.\n"
                    "Use cautious language: ERGM coefficients support conditional association mechanisms, not causal proof.\n"
                    "If GOF does not pass or diagnostics are weak, say so clearly.\n"
                    "Return valid JSON only.")


def term_mechanism(term):
    base = term.split("(")[0].strip()
    glossary = {
        "edges": "baseline tie propensity after accounting for all other terms",
        "mutual": "reciprocity: actors tend to return directed ties",
        "gwesp": "triadic closure and local clustering among connected pairs",
        "gwdsp": "open two-path structure; pressure toward or against shared partners",
        "gwdegree": "degree spread: how evenly ties are distributed across actors",
        "gwidegree": "in-degree spread: concentration of incoming ties",
        "gwodegree": "out-degree spread: concentration of outgoing ties",
        "twopath": "two-path connectivity",
        "ttriple": "transitive triads in directed ties",
        "ctriple": "cyclic triads in directed ties",
        "nodematch": "homophily: ties are more likely within the same category",
        "nodemix": "mixing pattern: tie rates differ across category pairings",
        "nodefactor": "activity differences across categories",
        "nodeifactor": "incoming-tie activity differences across categories",
        "nodeofactor": "outgoing-tie activity differences across categories",
        "nodecov": "tie propensity rising or falling with a numeric attribute",
        "nodeicov": "incoming ties rising or falling with a numeric attribute",
        "nodeocov": "outgoing ties rising or falling with a numeric attribute",
        "absdiff": "similarity on a numeric attribute: closer values, likelier ties",
    }
    return glossary.get(base, "model mechanism")


def build_interpret_prompt(payload):
    brief = payload.get("brief", {})
    meta = payload.get("network_meta", {})
    final = payload["final"]
    diag = payload.get("diagnostics", {})
    terms = final["terms"]
    term_lines = "\n".join(f"- {t}" for t in terms)
    glossary_lines = "\n".join(f"- {t}: {term_mechanism(t)}" for t in terms)
    coef_lines = "\n".join(
        f"- {c['term']}: estimate={c['estimate']}, SE={c['std_error']}"
        for c in final.get("coefficients", [])
    )
    gof = final.get("gof") or {}
    history_lines = "\n".join(f"- {line}" for line in payload.get("history", [])) or "- none"
    recip = diag.get("reciprocity")
    user = f"""Dataset: {meta.get('id', 'live_network')}
Input source: live_pipeline
Network: name={meta.get('title', 'User network')}, nodes={diag.get('nodes')}, directed={str(bool(diag.get('directed'))).lower()}
Actors: {brief.get('actors', 'Not provided.')}
Tie meaning: {brief.get('tie_meaning', 'Not provided.')}
Context constraint: {brief.get('constraint', 'Not provided.')}

Final ERGM terms:
{term_lines}

Term mechanism glossary:
{glossary_lines}

Coefficient table:
{coef_lines}

Fit and diagnostic evidence:
- Estimator: {ESTIMATOR_LABELS.get(str(final.get('estimator', 'sa')).lower(), 'SA')}
- GOF discrepancy q(M) = {gof.get('q', 'NA')} (largest |z| over {gof.get('bins', 'NA')} GOF bins from {gof.get('nsim', 100)} simulated networks; lower is better)
- Density check: {'passed' if (final.get('density') or {}).get('pass') else 'not recorded'}
- Secondary diagnostic: MPLE pseudo-BIC = {final.get('pseudo_bic')}
- Initial diagnostics: density={diag.get('density')}, isolates={diag.get('isolates')}, reciprocity={'NA' if recip is None else recip}, clustering={diag.get('transitivity')}, triangles={diag.get('triangles')}

Refinement evidence:
{history_lines}

Task:
Explain the mechanisms represented by this final ERGM. Tie each mechanism to specific terms and, where available, coefficient signs/magnitudes and refinement evidence. Then synthesize those mechanisms into one human-understandable theory of how ties form in this network. Separate supported interpretation from limitations. Do not overclaim causality. Interpret each coefficient's sign conditional on the other terms in the model.
The "headline" is shown to a general audience as the one-line summary, so write it in plain everyday words with NO statistical or ERGM jargon: do not use phrases like "triadic closure", "homophily", "baseline tie rate/rarity", "degree heterogeneity", "reciprocity", "conditional association", or term names. Say things people understand, e.g. "friends of friends tend to become friends", "students mostly befriend others in the same club and grade", "a few students have far more friends than the rest".
Output language: English.

Output JSON schema:
{{
  "headline": "one plain-language sentence a non-expert can read, describing how ties form in everyday words, with no statistical/ERGM jargon or term names",
  "human_understandable_theory": "plain theory of the network in 1-2 short paragraphs, using everyday language and no ERGM jargon unless briefly defined",
  "mechanism_explanation": "concise paragraph (at most 4 short sentences) explaining the main network mechanisms",
  "term_interpretations": [
    {{"term": "term name", "mechanism": "what it means", "evidence": "coefficient/diagnostic evidence", "caution": "interpretive limit"}}
  ],
  "evidence_assessment": "how strong the fitted evidence is, including the q(M) value and remaining GOF discrepancies",
  "limitations": ["specific limitation 1", "specific limitation 2"],
  "plain_language": "nontechnical explanation for a domain audience",
  "recommended_followups": ["diagnostic or modeling follow-up 1", "follow-up 2"]
}}"""
    return INTERPRET_SYSTEM, user


# ---------------------------------------------------------------- pipeline steps

def validate_spec_terms(formula, library_terms):
    lib = set(library_terms)
    valid = [t for t in formula if t in lib]
    invalid = [t for t in formula if t not in lib]
    return valid, invalid


def api_intake(payload):
    network = validate_network(payload.get("network"))
    result = run_r({"mode": "intake", "network": network,
                    "library_options": LIBRARY_OPTIONS})
    return result


def api_propose(payload):
    model = pick_model(payload)
    diag = payload["diagnostics"]
    library_terms = payload["library_terms"]
    attribute_details = payload.get("attribute_details", [])
    brief = payload.get("brief", {})
    n_candidates = min(int(payload.get("n_candidates", 3)), 4)

    system, user = build_propose_prompt(diag, library_terms, attribute_details, brief, n_candidates)
    parsed, raw, latency = call_llm_json(system, user, model, temperature=0.0)

    specs = parsed.get("specifications")
    if not isinstance(specs, list) or not specs:
        raise ApiError(f"LLM returned no specifications: {raw[:300]}", 502)

    cleaned = []
    seen_labels = set()
    for i, spec in enumerate(specs[:n_candidates]):
        raw_formula = spec.get("formula")
        if isinstance(raw_formula, str):
            raw_formula = raw_formula.split("+")
        if not isinstance(raw_formula, list):
            raise ApiError(f"LLM returned a malformed formula for spec {i + 1}: {raw_formula!r}", 502)
        formula = list(dict.fromkeys(t for t in (str(t).strip() for t in raw_formula) if t))
        if "edges" not in formula:
            formula = ["edges"] + formula
        valid, invalid = validate_spec_terms(formula, library_terms)
        label = str(spec.get("label") or "").strip()
        if not label or label in seen_labels or label == "Edge-only null":
            label = f"Candidate {i + 1}"
        while label in seen_labels:
            label += "b"
        seen_labels.add(label)
        cleaned.append({
            "label": label,
            "formula": formula,
            "valid_terms": valid,
            "invalid_terms": invalid,
            "library_compliant": not invalid,
            "expected_effects": spec.get("expected_effects", {}),
            "rationale": spec.get("rationale", {}),
        })
    if not any(s["library_compliant"] for s in cleaned):
        raise ApiError("no library-compliant specification returned; try re-running", 502)
    return {
        "ok": True,
        "model": model,
        "latency": latency,
        "prompt": {"system": system, "user": user},
        "raw_response": raw,
        "specifications": cleaned,
    }


ESTIMATORS = ("sa", "mcmle", "mple")


def pick_estimator(payload):
    est = str(payload.get("estimator", "sa")).strip().lower()
    if est not in ESTIMATORS:
        raise ApiError(f"estimator must be one of {ESTIMATORS}")
    return est


def api_screen(payload):
    """Stage 2: SA fit every candidate plus the edge-only baseline, apply the
    eligibility checks, and select the eligible model with the lowest q(M)."""
    network = validate_network(payload.get("network"))
    candidates = payload.get("candidates")
    if not isinstance(candidates, list) or not candidates:
        raise ApiError("candidates must be a non-empty array")
    labels = [c.get("label") for c in candidates]
    if "Edge-only baseline" not in labels:
        candidates = candidates + [{"label": "Edge-only baseline", "terms": ["edges"]}]
    seed = int(payload.get("seed", DEFAULT_SEED))
    estimator = pick_estimator(payload)
    result = run_r({"mode": "evaluate", "network": network, "candidates": candidates,
                    "seed": seed, "estimator": estimator, "library_options": LIBRARY_OPTIONS})
    result["estimator"] = estimator
    if not result.get("winner"):
        raise ApiError("no candidate passed the eligibility checks (finite SA estimates, density check, computable GOF)", 422)
    return result


def apply_edit(action, term, target, current_terms):
    terms = list(current_terms)
    if action == "add":
        if term in terms:
            return None, f"term already in model: {term}"
        terms.append(term)
    elif action == "remove":
        if term == "edges":
            return None, "cannot remove edges"
        if term not in terms:
            return None, f"term not in model: {term}"
        terms.remove(term)
    elif action == "replace":
        if not target or target not in terms:
            return None, f"replace target not in model: {target}"
        if target == "edges":
            return None, "cannot replace edges"
        if term in terms and term != target:
            return None, f"term already in model: {term}"
        terms[terms.index(target)] = term
    else:
        return None, f"unknown action: {action}"
    if not 3 <= len(terms) <= 8:
        return None, f"edited model has {len(terms)} terms (allowed 3-8)"
    return terms, None


def current_from_fit(fit, label=None):
    return {
        "label": label or fit.get("label"),
        "estimator": fit.get("estimator"),
        "terms": fit["terms"],
        "q": fit.get("q"),
        "pseudo_bic": fit.get("pseudo_bic"),
        "coefficients": fit.get("coefficients", []),
        "gof": fit.get("gof"),
        "density": fit.get("density"),
        "guardrails": fit.get("guardrails"),
    }


def api_revise(payload):
    """Stage 3, one round: the LLM proposes one edit; the edited model is
    refitted with SA and re-checked; it is kept only if eligible and q(M)
    strictly decreases. The browser calls this up to MAX_ROUNDS times."""
    model = pick_model(payload)
    network = validate_network(payload.get("network"))
    current = payload["current"]
    library_terms = payload["library_terms"]
    brief = payload.get("brief", {})
    history = payload.get("history", []) or []
    round_no = int(payload.get("round", len(history) + 1))
    if round_no > MAX_ROUNDS:
        raise ApiError(f"revision budget is {MAX_ROUNDS} rounds")
    seed = int(payload.get("seed", DEFAULT_SEED))

    estimator = pick_estimator(payload)
    system, user = build_revise_prompt(current, library_terms, brief, history, round_no, estimator)
    parsed, raw, latency = call_llm_json(system, user, model, temperature=0.0)

    action = str(parsed.get("action", "")).strip().lower()
    if action == "substitute":
        action = "replace"
    term = str(parsed.get("term", "")).strip()
    target = str(parsed.get("target", "")).strip() or None
    rationale = str(parsed.get("rationale", "")).strip()

    edit = {"action": action, "term": term, "target": target, "rationale": rationale}
    response = {
        "ok": True,
        "model": model,
        "latency": latency,
        "round": round_no,
        "max_rounds": MAX_ROUNDS,
        "prompt": {"system": system, "user": user},
        "raw_response": raw,
        "edit": edit,
        "q_before": current.get("q"),
        "q_after": None,
        "eligible": False,
    }

    def reject(reason):
        response.update(accepted=False, rejection_reason=reason, final=current)
        return response

    if action in ("add", "replace") and term not in library_terms:
        return reject(f"proposed term is not in L*: {term}")
    new_terms, err = apply_edit(action, term, target, current["terms"])
    if err:
        return reject(err)

    evaluated = run_r({"mode": "evaluate", "network": network,
                       "candidates": [{"label": f"Round {round_no}", "terms": new_terms}],
                       "seed": seed, "estimator": estimator, "library_options": LIBRARY_OPTIONS})
    revised = evaluated["fits"][0]
    response["refit"] = revised
    response["eligible"] = bool(revised.get("eligible"))
    response["q_after"] = revised.get("q")

    guard = revised.get("guardrails") or {}
    required = {
        "g1_edges_and_size": "model must keep edges and 3-8 terms",
        "g2_single_closure_family": "at most one gwesp/gwdsp closure term",
        "g4_no_match_factor_overlap": "nodematch and nodefactor clash on an attribute",
        "g5_no_triangle": "unstable triangle term",
        "g6_library_only": "edited model leaves L*",
    }
    failed = [reason for key, reason in required.items() if not guard.get(key)]
    if failed:
        return reject(f"incompatible specification: {failed[0]}")
    if not revised.get("eligible"):
        return reject(f"ineligible: {revised.get('reason', 'failed the post-fit checks')}")
    q_before = current.get("q")
    q_after = revised.get("q")
    if q_before is not None and q_after is not None and q_after < q_before:
        response["accepted"] = True
        response["final"] = current_from_fit(revised, label=f"Revised (round {round_no})")
        return response
    return reject(f"q(M) did not decrease ({q_after} vs {q_before})")


def strip_markdown(value):
    """The demo renders interpretation strings as plain text; drop md emphasis."""
    if isinstance(value, str):
        return re.sub(r"\*\*(.+?)\*\*", r"\1", value).replace("`", "")
    if isinstance(value, list):
        return [strip_markdown(v) for v in value]
    if isinstance(value, dict):
        return {k: strip_markdown(v) for k, v in value.items()}
    return value


def api_interpret(payload):
    model = pick_model(payload)
    system, user = build_interpret_prompt(payload)
    parsed, raw, latency = call_llm_json(system, user, model, temperature=0.2)
    parsed = strip_markdown(parsed)
    required = ["headline", "human_understandable_theory", "mechanism_explanation",
                "term_interpretations", "evidence_assessment", "limitations",
                "plain_language", "recommended_followups"]
    missing = [k for k in required if k not in parsed]
    if missing:
        raise ApiError(f"interpretation missing keys: {missing}", 502)
    return {
        "ok": True,
        "model": model,
        "latency": latency,
        "prompt": {"system": system, "user": user},
        "raw_response": raw,
        "interpretation": parsed,
    }


ROUTES = {
    "/api/intake": api_intake,
    "/api/propose": api_propose,
    "/api/screen": api_screen,
    "/api/revise": api_revise,
    "/api/interpret": api_interpret,
}


class LiveHandler(SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=str(DEMO_DIR), **kwargs)

    def log_message(self, fmt, *args):
        print(f"[{time.strftime('%H:%M:%S')}] {fmt % args}")

    def _send_json(self, obj, status=200):
        body = json.dumps(obj).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == "/api/health":
            self._send_json({
                "ok": True,
                "key_present": bool(API_KEY),
                "models": ALLOWED_MODELS,
                "default_model": DEFAULT_MODEL,
                "max_nodes": MAX_NODES,
            })
            return
        super().do_GET()

    def do_POST(self):
        handler = ROUTES.get(self.path)
        if handler is None:
            self._send_json({"ok": False, "error": "unknown endpoint"}, 404)
            return
        try:
            length = int(self.headers.get("Content-Length", 0))
            payload = json.loads(self.rfile.read(length) or b"{}")
            result = handler(payload)
            self._send_json(result)
        except ApiError as err:
            self._send_json({"ok": False, "error": str(err)}, err.status)
        except subprocess.TimeoutExpired:
            self._send_json({"ok": False, "error": "R fitting timed out"}, 504)
        except (KeyError, TypeError, ValueError) as err:
            self._send_json({"ok": False, "error": f"bad request: {err!r}"}, 400)
        except Exception as err:  # keep the demo server alive
            self._send_json({"ok": False, "error": f"server error: {err!r}"}, 500)


def main():
    parser = argparse.ArgumentParser(description="FORGE live demo server")
    parser.add_argument("--port", type=int, default=8765)
    args = parser.parse_args()
    server = ThreadingHTTPServer(("127.0.0.1", args.port), LiveHandler)
    print(f"FORGE live demo: http://127.0.0.1:{args.port}/")
    print(f"  serving {DEMO_DIR}")
    print(f"  OpenRouter key: {'found' if API_KEY else 'MISSING (live mode disabled)'}")
    server.serve_forever()


if __name__ == "__main__":
    main()
