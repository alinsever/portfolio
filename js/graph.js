/* ==========================================================================
   graph.js — interactive hero graph, in the spirit of Zachary's Karate Club.

   Two communities, like the two factions of the club:
     - profile  (cyan):   resume, skills, education, experience, awards, contact
     - projects (orange): the projects page, every project, and their domains
   Bridges between them are real relationships taken from the data:
     - HSLU  ->  every academic project
     - a skill  ->  every project whose tech stack uses it
   Click a node to jump to that section or page. Hover or focus a node to light up
   its neighbours. Drag a node (mouse) to rearrange the graph.

   Projects and domains are read from js/projects-data.js, so new projects appear
   in the graph automatically. Only the profile side is configured below.
   No libraries: a small force simulation drives plain SVG.
   ========================================================================== */
(() => {
  "use strict";

  const host = document.getElementById("hero-graph");
  if (!host || !window.PROJECTS) return;
  const { PROJECTS, DOMAINS } = window;

  const NS = "http://www.w3.org/2000/svg";
  const reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  const touchOnly = window.matchMedia("(hover: none)").matches;

  /* ---- edit me: the profile side ---------------------------------------- */

  // [id, label, size (hub | mid | leaf), where a click goes]
  const PROFILE_NODES = [
    ["resume",  "Resume",          "hub",  "#resume"],
    ["about",   "About",           "mid",  "#about"],
    ["contact", "Contact",         "mid",  "#contact"],
    ["skills",  "Skills",          "mid",  "#skills"],
    ["python",  "Python",          "leaf", "#skills"],
    ["sql",     "SQL",             "leaf", "#skills"],
    ["r",       "R",               "leaf", "#skills"],
    ["ml",      "ML & AI",         "leaf", "#skills"],
    ["bi",      "BI & Dashboards", "leaf", "#skills"],
    ["dev",     "Dev & Workflow",  "leaf", "#skills"],
    ["hslu",    "HSLU",            "mid",  "#resume"],
    ["bsc",     "BSc Food Eng.",   "leaf", "#resume"],
    ["ircc",    "IRCC",            "mid",  "#resume"],
    ["pspc",    "PSPC",            "leaf", "#resume"],
    ["m1",      "Millennium 1",    "leaf", "#resume"],
    ["awards",  "Awards",          "leaf", "#resume"],
  ];

  const PROFILE_LINKS = [
    // the resume hub
    ["resume", "about"], ["resume", "skills"], ["resume", "hslu"], ["resume", "bsc"], ["resume", "ircc"],
    ["resume", "pspc"], ["resume", "m1"], ["resume", "awards"], ["resume", "contact"], ["about", "contact"],
    // the skills hub
    ["skills", "python"], ["skills", "sql"], ["skills", "r"], ["skills", "ml"], ["skills", "bi"], ["skills", "dev"],
    // where each skill was used (from the resume)
    ["ircc", "sql"], ["ircc", "r"], ["ircc", "python"], ["ircc", "bi"],
    ["pspc", "sql"], ["pspc", "bi"], ["m1", "bi"],
    ["awards", "ircc"], ["awards", "pspc"],
    ["hslu", "ml"], ["hslu", "python"], ["hslu", "dev"],
    // the two hubs
    ["resume", "projects"],
  ];

  // project domains that are really skills of yours. They move to the profile side, hang off the
  // Skills hub (a click jumps to #skills) and keep their links to every project in that domain.
  const SKILL_DOMAINS = ["NLP & Language"];

  // a project connects to a skill when any item of its `tech` list matches
  const TECH_TO_SKILL = [
    ["python", /python|scikit/i],
    ["ml",     /rag|llm|agentic|rerank|knowledge|nlp|statistical|scikit|chroma|watson/i],
    ["dev",    /git|jira|streamlit|quarto|oop|next\.js|docker/i],
    ["sql",    /\bsql\b/i],
    ["r",      /^r$/i],
    ["bi",     /dashboard|tableau|power ?bi|cognos/i],
  ];

  /* ---- tuning ----------------------------------------------------------- */

  const RADIUS = { hub: 15, mid: 10, leaf: 6.5 };
  const CHARGE = { hub: 2.4, mid: 1.9, leaf: 1 };
  const FONT   = { hub: 13, mid: 11.5, leaf: 10.5 };
  const REPEL = 7200, LINK_LEN = 78, BRIDGE_LEN = 240, DAMPING = 0.8;
  const HOP_MS = 340, PULSE_EVERY_MS = 5200;
  const ALPHA_MIN = reduceMotion ? 0.004 : 0.02;

  /* ---- build the graph from the data ------------------------------------ */

  const nodes = [], byId = new Map(), links = [], seen = new Set();

  const addNode = (id, label, size, href, community, aria) => {
    const n = {
      id, label, size, href, community, aria,
      r: RADIUS[size], q: CHARGE[size],
      x: 0, y: 0, vx: 0, vy: 0, fx: null, fy: null, dx: 0, dy: 0,
      adj: new Set(), ph: Math.random() * Math.PI * 2,
    };
    nodes.push(n);
    byId.set(id, n);
    return n;
  };

  const addLink = (a, b) => {
    const A = byId.get(a), B = byId.get(b);
    if (!A || !B || A === B) return;
    const key = A.id < B.id ? A.id + "|" + B.id : B.id + "|" + A.id;
    if (seen.has(key)) return;
    seen.add(key);
    links.push({ a: A, b: B, kind: A.community === B.community ? A.community : "bridge" });
    A.adj.add(B);
    B.adj.add(A);
  };

  PROFILE_NODES.forEach(([id, label, size, href]) =>
    addNode(id, label, size, href, "profile", href.startsWith("#") ? `${label}: jump to this section` : label));

  addNode("projects", "Projects", "hub", "projects.html", "projects", "Projects: open all projects");

  PROJECTS.forEach((p) =>
    addNode("p:" + p.id, p.short || p.title, "mid", "project.html?id=" + encodeURIComponent(p.id), "projects", `${p.title}: open project`));

  const usedDomains = DOMAINS.filter((d) => PROJECTS.some((p) => p.domain === d));
  usedDomains.forEach((d) => {
    const isSkill = SKILL_DOMAINS.includes(d);
    addNode("d:" + d, d, "leaf",
      isSkill ? "#skills" : "projects.html?domain=" + encodeURIComponent(d),
      isSkill ? "profile" : "projects",
      isSkill ? `${d}: jump to this section` : `${d}: show projects in this domain`);
  });

  PROFILE_LINKS.forEach(([a, b]) => addLink(a, b));
  PROJECTS.forEach((p) => {
    const id = "p:" + p.id;
    addLink("projects", id);
    addLink("d:" + p.domain, id);
    if (p.type === "Academic") addLink("hslu", id);
    (p.tech || []).forEach((t) => TECH_TO_SKILL.forEach(([skill, re]) => re.test(t) && addLink(skill, id)));
  });
  usedDomains.forEach((d) => addLink(SKILL_DOMAINS.includes(d) ? "skills" : "projects", "d:" + d));

  /* ---- DOM -------------------------------------------------------------- */

  const el = (tag, attrs = {}) => {
    const e = document.createElementNS(NS, tag);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    return e;
  };

  const root = document.createElement("div");
  root.className = "graph";
  const svg = el("svg", { class: "graph__svg" });
  const gEdges = el("g"), gNodes = el("g");
  svg.append(gEdges, gNodes);

  const skip = document.createElement("a");
  skip.className = "skip-link";
  skip.href = "#about";
  skip.textContent = "Skip graph";

  const legend = document.createElement("ul");
  legend.className = "graph__legend";
  legend.innerHTML =
    `<li class="graph__key graph__key--profile">Profile</li>` +
    `<li class="graph__key graph__key--projects">Projects</li>` +
    `<li class="graph__hint">${touchOnly ? "Tap a node to open it" : "Click a node to open it · drag to rearrange"}</li>`;

  root.append(skip, svg, legend);
  host.append(root);

  links.forEach((l) => {
    l.el = el("line", { class: `graph__edge graph__edge--${l.kind}` });
    gEdges.append(l.el);
  });

  nodes.forEach((n, i) => {
    const a = el("a", {
      class: `graph__node graph__node--${n.community} graph__node--${n.size}`,
      href: n.href, "aria-label": n.aria, draggable: "false",
    });
    a.style.setProperty("--i", i);
    n.halo = el("circle", { class: "graph__halo" });
    n.hit = el("circle", { class: "graph__hit" });
    n.dot = el("circle", { class: "graph__dot" });
    n.text = el("text", { class: "graph__label" });
    n.text.textContent = n.label;
    a.append(n.halo, n.hit, n.dot, n.text);
    n.el = a;
    gNodes.append(a);
    wireNode(n);
  });

  /* ---- state ------------------------------------------------------------ */

  let W = 0, H = 0, s = 1, alpha = 1;
  let raf = 0, lastT = 0, visible = true, focusNode = null, drag = null, suppressClickUntil = 0;
  const centre = { profile: [0, 0], projects: [0, 0] };

  // small seeded generator so the starting layout is the same on every load
  let seedState = 11;
  const rand = () => {
    seedState = (seedState + 0x6d2b79f5) | 0;
    let t = Math.imul(seedState ^ (seedState >>> 15), 1 | seedState);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));

  /* ---- layout ----------------------------------------------------------- */

  function layout() {
    s = clamp(Math.min(W, H * 1.5) / 640, 0.62, 1.3);
    const wide = W / H >= 0.95;           // communities left / right; only stack them on tall, narrow screens
    centre.profile  = wide ? [W * 0.27, H * 0.50] : [W * 0.50, H * 0.29];
    centre.projects = wide ? [W * 0.78, H * 0.48] : [W * 0.50, H * 0.73];

    const fs = clamp(s, 0.85, 1.1);
    nodes.forEach((n) => {
      n.r = RADIUS[n.size] * s;
      n.halo.setAttribute("r", n.r + (n.size === "hub" ? 8 : 6));
      n.hit.setAttribute("r", n.r + 9);
      n.dot.setAttribute("r", n.r);
      n.text.setAttribute("y", n.r + 16 * fs);
      n.text.style.fontSize = FONT[n.size] * fs + "px";
    });
  }

  function seed() {
    nodes.forEach((n) => {
      const [cx, cy] = centre[n.community];
      const ang = rand() * Math.PI * 2;
      const rad = (n.size === "hub" ? 8 : 40 + rand() * 90) * s;
      n.x = cx + Math.cos(ang) * rad;
      n.y = cy + Math.sin(ang) * rad;
    });
  }

  /* ---- physics ---------------------------------------------------------- */

  function tick() {
    const a = alpha;

    for (let i = 0; i < nodes.length; i++) {
      const A = nodes[i];
      for (let j = i + 1; j < nodes.length; j++) {
        const B = nodes[j];
        let dx = B.x - A.x, dy = B.y - A.y, d2 = dx * dx + dy * dy;
        if (d2 < 0.01) { dx = rand() - 0.5; dy = rand() - 0.5; d2 = dx * dx + dy * dy + 0.01; }
        const d = Math.sqrt(d2), ux = dx / d, uy = dy / d;

        const f = (REPEL * s * s * A.q * B.q * a) / (d2 + 400);          // repulsion
        A.vx -= f * ux; A.vy -= f * uy; B.vx += f * ux; B.vy += f * uy;

        const min = A.r + B.r + 14 * s;                                   // never overlap
        if (d < min) {
          const p = (min - d) * 0.18;
          A.vx -= p * ux; A.vy -= p * uy; B.vx += p * ux; B.vy += p * uy;
        }
      }
    }

    for (const l of links) {                                              // springs
      const { a: A, b: B } = l;
      const dx = B.x - A.x, dy = B.y - A.y, d = Math.sqrt(dx * dx + dy * dy) || 1;
      const bridge = l.kind === "bridge";
      const rest = (bridge ? BRIDGE_LEN : LINK_LEN) * s + (A.r + B.r) * 0.5;
      const f = (d - rest) * (bridge ? 0.009 : 0.07) * a;
      const fx = (f * dx) / d, fy = (f * dy) / d;
      A.vx += fx; A.vy += fy; B.vx -= fx; B.vy -= fy;
    }

    const padX = 30 * s, padTop = 26 * s, padBottom = 62 * s;
    for (const n of nodes) {
      const [cx, cy] = centre[n.community];                               // pull towards own community
      const g = (n.size === "hub" ? 0.09 : n.size === "mid" ? 0.022 : 0.014) * a;
      n.vx += (cx - n.x) * g;
      n.vy += (cy - n.y) * g;

      if (n.fx != null) { n.x = n.fx; n.y = n.fy; n.vx = n.vy = 0; continue; }
      n.vx *= DAMPING; n.vy *= DAMPING;
      n.x = clamp(n.x + n.vx, padX + n.r, W - padX - n.r);
      n.y = clamp(n.y + n.vy, padTop + n.r, H - padBottom - n.r);
    }

    alpha = Math.max(ALPHA_MIN, alpha * 0.985);
  }

  /* ---- drawing ---------------------------------------------------------- */

  function draw(t) {
    const amp = reduceMotion ? 0 : 2.4 * s;                               // gentle idle float
    for (const n of nodes) {
      const still = n.fx != null || amp === 0;
      n.dx = n.x + (still ? 0 : Math.sin(t / 1400 + n.ph) * amp);
      n.dy = n.y + (still ? 0 : Math.cos(t / 1800 + n.ph * 1.7) * amp);
      n.el.setAttribute("transform", `translate(${n.dx.toFixed(1)} ${n.dy.toFixed(1)})`);
    }
    for (const l of links) {
      l.el.setAttribute("x1", l.a.dx.toFixed(1));
      l.el.setAttribute("y1", l.a.dy.toFixed(1));
      l.el.setAttribute("x2", l.b.dx.toFixed(1));
      l.el.setAttribute("y2", l.b.dy.toFixed(1));
    }
  }

  function frame(t) {
    raf = 0;
    if (!visible) return;
    const steps = clamp(Math.round((t - lastT) / 16.7), 1, 3);
    lastT = t;
    for (let i = 0; i < steps; i++) tick();
    draw(t);
    if (!reduceMotion || alpha > ALPHA_MIN * 1.5 || drag) raf = requestAnimationFrame(frame);
  }

  function kick() {
    if (raf || !visible) return;
    lastT = performance.now();
    raf = requestAnimationFrame(frame);
  }

  /* ---- interaction ------------------------------------------------------ */

  function setFocus(n) {
    focusNode = n;
    root.classList.toggle("is-focus", !!n);
    for (const m of nodes) m.el.classList.toggle("is-on", !!n && (m === n || n.adj.has(m)));
    for (const l of links) l.el.classList.toggle("is-on", !!n && (l.a === n || l.b === n));
  }

  function toLocal(e) {
    const r = svg.getBoundingClientRect();
    return [((e.clientX - r.left) * W) / r.width, ((e.clientY - r.top) * H) / r.height];
  }

  function wireNode(n) {
    const a = n.el;
    a.addEventListener("pointerenter", () => { if (!drag) setFocus(n); });
    a.addEventListener("pointerleave", () => { if (!drag) setFocus(null); });
    a.addEventListener("focus", () => setFocus(n));
    a.addEventListener("blur", () => setFocus(null));
    a.addEventListener("dragstart", (e) => e.preventDefault());

    // a drag must not count as a click
    a.addEventListener("click", (e) => { if (performance.now() < suppressClickUntil) e.preventDefault(); });

    a.addEventListener("pointerdown", (e) => {
      if (e.pointerType === "touch" || e.button !== 0) return;           // touch keeps scrolling the page
      drag = { n, id: e.pointerId, x0: e.clientX, y0: e.clientY, active: false };
      try { a.setPointerCapture(e.pointerId); } catch (_) { /* pointer already gone; drag still works without capture */ }
    });

    a.addEventListener("pointermove", (e) => {
      if (!drag || drag.n !== n || e.pointerId !== drag.id) return;
      if (!drag.active && Math.hypot(e.clientX - drag.x0, e.clientY - drag.y0) < 5) return;
      drag.active = true;
      svg.classList.add("is-dragging");
      const [x, y] = toLocal(e);
      n.fx = clamp(x, n.r, W - n.r);
      n.fy = clamp(y, n.r, H - n.r);
      alpha = Math.max(alpha, 0.3);
      kick();
    });

    const end = (e) => {
      if (!drag || drag.n !== n) return;
      if (drag.active) suppressClickUntil = performance.now() + 80;
      n.fx = n.fy = null;
      drag = null;
      svg.classList.remove("is-dragging");
      try { a.releasePointerCapture(e.pointerId); } catch (_) { /* already released */ }
      kick();
    };
    a.addEventListener("pointerup", end);
    a.addEventListener("pointercancel", end);
  }

  /* ---- "message passing" pulse: a wave that spreads hop by hop ---------- */

  function ping(e, delay) {
    setTimeout(() => {
      e.classList.add("is-ping");
      setTimeout(() => e.classList.remove("is-ping"), 900);
    }, delay);
  }

  function pulse() {
    if (!visible || document.hidden || focusNode || drag) return;
    const src = nodes[Math.floor(Math.random() * nodes.length)];
    const depth = new Map([[src, 0]]);
    const queue = [src];
    while (queue.length) {
      const n = queue.shift();
      n.adj.forEach((m) => { if (!depth.has(m)) { depth.set(m, depth.get(n) + 1); queue.push(m); } });
    }
    depth.forEach((d, n) => ping(n.el, d * HOP_MS));
    for (const l of links) {
      const da = depth.get(l.a), db = depth.get(l.b);
      if (da !== db) ping(l.el, (Math.min(da, db) + 0.5) * HOP_MS);
    }
  }

  /* ---- sizing, visibility, start ---------------------------------------- */

  function resize() {
    const w = host.clientWidth, h = host.clientHeight;
    if (w < 40 || h < 40) return;
    const first = !W;
    if (!first) nodes.forEach((n) => { n.x *= w / W; n.y *= h / H; });
    W = w; H = h;
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    layout();

    if (first) {
      seed();
      const warmup = reduceMotion ? 700 : 110;                            // start partly settled, finish on screen
      for (let i = 0; i < warmup && (!reduceMotion || alpha > ALPHA_MIN * 1.2); i++) tick();
      draw(0);
    } else {
      alpha = Math.max(alpha, 0.3);
    }
    kick();
  }

  new ResizeObserver(resize).observe(host);

  if ("IntersectionObserver" in window) {
    new IntersectionObserver(([entry]) => {
      visible = entry.isIntersecting;
      if (visible) kick();
    }).observe(host);
  }

  if (!reduceMotion) {
    setTimeout(pulse, 2600);
    setInterval(pulse, PULSE_EVERY_MS);
  }
})();
