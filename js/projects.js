/* ==========================================================================
   projects.js — shared card renderer + the projects.html grid
   (also renders the "featured" strip on the home page)
   ========================================================================== */
(() => {
  "use strict";

  const PAGE_SIZE = 8;
  const Portfolio = (window.Portfolio = window.Portfolio || {});
  const { PROJECTS, DOMAINS, TYPES } = window;

  const esc = (s) =>
    String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));

  /* ---- generated placeholder art ------------------------------------- */
  const hash = (str) => {
    let h = 2166136261;
    for (let i = 0; i < str.length; i++) { h ^= str.charCodeAt(i); h = Math.imul(h, 16777619); }
    return h >>> 0;
  };
  const prng = (a) => () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };

  // deterministic "data-viz" artwork per project id; replaced by project.image when set
  function projectImage(p, w = 640, h = 400) {
    if (p.image) return esc(p.image);
    const seed = hash(p.id);
    const rnd = prng(seed);
    const h1 = seed % 360;
    const h2 = (h1 + 35 + Math.floor(rnd() * 55)) % 360;
    let s = `<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 ${w} ${h}'>`;
    s += `<defs><linearGradient id='g' x1='0' y1='0' x2='1' y2='1'><stop offset='0' stop-color='hsl(${h1} 62% 18%)'/><stop offset='1' stop-color='hsl(${h2} 70% 38%)'/></linearGradient></defs>`;
    s += `<rect width='${w}' height='${h}' fill='url(#g)'/>`;
    for (let i = 0; i < 4; i++) {
      s += `<circle cx='${(rnd() * w) | 0}' cy='${(rnd() * h) | 0}' r='${(50 + rnd() * 110) | 0}' fill='white' fill-opacity='${(0.05 + rnd() * 0.08).toFixed(2)}'/>`;
    }
    for (let y = 22; y < h; y += 30) {
      for (let x = 22; x < w; x += 30) {
        if (rnd() > 0.6) s += `<circle cx='${x}' cy='${y}' r='1.6' fill='white' fill-opacity='${(0.2 + rnd() * 0.35).toFixed(2)}'/>`;
      }
    }
    const pts = [];
    const n = 8;
    for (let i = 0; i < n; i++) pts.push([Math.round((i / (n - 1)) * (w - 120) + 60), Math.round(h * 0.28 + rnd() * h * 0.46)]);
    s += `<polyline points='${pts.map((q) => q.join(",")).join(" ")}' fill='none' stroke='white' stroke-opacity='.8' stroke-width='3' stroke-linejoin='round' stroke-linecap='round'/>`;
    pts.forEach((q) => { s += `<circle cx='${q[0]}' cy='${q[1]}' r='5' fill='white'/>`; });
    s += "</svg>";
    return "data:image/svg+xml;utf8," + encodeURIComponent(s);
  }

  /* ---- card ----------------------------------------------------------- */
  // cards stay short: two names, then "+N" (the project page lists everyone)
  const withNames = (c) => (c.length <= 2 ? c.join(", ") : `${c.slice(0, 2).join(", ")} +${c.length - 2}`);
  const statusClass = (status) => "status" + (status === "Completed" ? " status--completed" : "");

  function renderCard(p, i = 0, extra = "project-card--enter") {
    return `
      <li class="project-card ${extra}" style="--i:${i}">
        <a class="project-card__link" href="project.html?id=${encodeURIComponent(p.id)}">
          <div class="project-card__media">
            <img src="${projectImage(p)}" width="640" height="400" alt="" loading="lazy"${p.imagePosition ? ` style="object-position:${esc(p.imagePosition)}"` : ""}>
          </div>
          <div class="project-card__body">
            <h3 class="project-card__title">${esc(p.title)}</h3>
            <p class="${statusClass(p.status)}">${esc(p.status)}</p>
            <p class="project-card__summary">${esc(p.subtitle)}</p>
            ${p.collaborators && p.collaborators.length ? `<p class="project-card__with">With ${esc(withNames(p.collaborators))}</p>` : ""}
            <ul class="tags"><li class="tag">${esc(p.domain)}</li><li class="tag">${esc(p.type)}</li></ul>
          </div>
        </a>
      </li>`;
  }

  Object.assign(Portfolio, { esc, projectImage, renderCard, statusClass });

  /* ---- home: featured strip ------------------------------------------ */
  const featured = document.getElementById("featured-grid");
  if (featured) {
    featured.innerHTML = PROJECTS.filter((p) => p.featured)
      .slice(0, 4)
      .map((p, i) => renderCard(p, i, "reveal"))
      .join("");
    if (Portfolio.reveal) Portfolio.reveal(featured);
  }

  /* ---- projects.html: filters + grid + pagination --------------------- */
  const grid = document.getElementById("project-grid");
  if (!grid) return;

  const form = document.getElementById("filters");
  const selDomain = document.getElementById("f-domain");
  const selType = document.getElementById("f-type");
  const inpSearch = document.getElementById("f-search");
  const btnClear = document.getElementById("f-clear");
  const count = document.getElementById("result-count");
  const pager = document.getElementById("pagination");

  const fill = (select, values) =>
    values.forEach((v) => select.insertAdjacentHTML("beforeend", `<option value="${esc(v)}">${esc(v)}</option>`));

  // offer only the values projects actually use, and hide a filter with nothing to choose between
  const used = (key, order) => order.filter((v) => PROJECTS.some((p) => p[key] === v));
  const domains = used("domain", DOMAINS);
  const types = used("type", TYPES);
  fill(selDomain, domains);
  fill(selType, types);
  selDomain.closest(".field").hidden = domains.length < 2;
  selType.closest(".field").hidden = types.length < 2;

  const state = { domain: "", type: "", q: "", page: 1 };

  const readUrl = () => {
    const sp = new URLSearchParams(location.search);
    state.domain = domains.includes(sp.get("domain")) ? sp.get("domain") : "";
    state.type = types.includes(sp.get("type")) ? sp.get("type") : "";
    state.q = (sp.get("q") || "").slice(0, 80);
    state.page = Math.max(1, parseInt(sp.get("page"), 10) || 1);
    selDomain.value = state.domain;
    selType.value = state.type;
    inpSearch.value = state.q;
  };

  const writeUrl = () => {
    const sp = new URLSearchParams();
    if (state.domain) sp.set("domain", state.domain);
    if (state.type) sp.set("type", state.type);
    if (state.q) sp.set("q", state.q);
    if (state.page > 1) sp.set("page", state.page);
    const qs = sp.toString();
    history.replaceState(null, "", qs ? `?${qs}` : location.pathname);
  };

  const matches = (p) => {
    if (state.domain && p.domain !== state.domain) return false;
    if (state.type && p.type !== state.type) return false;
    if (state.q) {
      const hay = [p.title, p.subtitle, p.domain, p.type, p.status, ...(p.tech || [])].join(" ").toLowerCase();
      return state.q.toLowerCase().split(/\s+/).every((t) => hay.includes(t));
    }
    return true;
  };

  // 1 … 4 5 6 … 9 style window
  const pageWindow = (cur, total) => {
    const keep = new Set([1, total, cur - 1, cur, cur + 1]);
    const out = [];
    for (let n = 1; n <= total; n++) {
      if (keep.has(n)) out.push(n);
      else if (out[out.length - 1] !== "…") out.push("…");
    }
    return out;
  };

  const renderPager = (pages) => {
    pager.hidden = pages <= 1;
    if (pages <= 1) return;
    const btn = (label, page, attrs = "") => `<button type="button" data-page="${page}" ${attrs}>${label}</button>`;
    pager.innerHTML =
      btn('<i class="bi bi-chevron-left" aria-hidden="true"></i><span class="visually-hidden">Previous page</span>', state.page - 1, state.page === 1 ? "disabled" : "") +
      pageWindow(state.page, pages)
        .map((n) => (n === "…" ? `<span aria-hidden="true">…</span>` : btn(n, n, `aria-label="Page ${n}"${n === state.page ? ' aria-current="page"' : ""}`)))
        .join("") +
      btn('<span class="visually-hidden">Next page</span><i class="bi bi-chevron-right" aria-hidden="true"></i>', state.page + 1, state.page === pages ? "disabled" : "") +
      `<span class="pagination__count">${state.page} / ${pages}</span>`;
  };

  const render = () => {
    const list = PROJECTS.filter(matches);
    const pages = Math.max(1, Math.ceil(list.length / PAGE_SIZE));
    state.page = Math.min(state.page, pages);
    const start = (state.page - 1) * PAGE_SIZE;
    const slice = list.slice(start, start + PAGE_SIZE);

    grid.innerHTML = slice.length
      ? slice.map((p, i) => renderCard(p, i)).join("")
      : `<li class="empty"><strong>No results found</strong>Try changing your filters or search term.</li>`;

    count.textContent = list.length
      ? `Showing ${start + 1}–${start + slice.length} of ${list.length} project${list.length === 1 ? "" : "s"}`
      : "No projects match your filters.";
    btnClear.hidden = !(state.domain || state.type || state.q);
    renderPager(pages);
    writeUrl();
  };

  let timer;
  selDomain.addEventListener("change", () => { state.domain = selDomain.value; state.page = 1; render(); });
  selType.addEventListener("change", () => { state.type = selType.value; state.page = 1; render(); });
  inpSearch.addEventListener("input", () => {
    clearTimeout(timer);
    timer = setTimeout(() => { state.q = inpSearch.value.trim(); state.page = 1; render(); }, 140);
  });
  form.addEventListener("submit", (e) => e.preventDefault());
  btnClear.addEventListener("click", () => {
    Object.assign(state, { domain: "", type: "", q: "", page: 1 });
    selDomain.value = selType.value = inpSearch.value = "";
    render();
    selDomain.focus();
  });
  pager.addEventListener("click", (e) => {
    const b = e.target.closest("button[data-page]");
    if (!b || b.disabled) return;
    state.page = Number(b.dataset.page);
    render();
    const smooth = !window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    form.scrollIntoView({ behavior: smooth ? "smooth" : "auto", block: "start" });
  });

  readUrl();
  render();
})();
