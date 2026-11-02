/* ==========================================================================
   project-detail.js — renders project.html from ?id=<slug>
   ========================================================================== */
(() => {
  "use strict";

  const root = document.getElementById("project-root");
  if (!root) return;

  const { esc, projectImage, statusClass, siteName } = window.Portfolio;
  const { PROJECTS } = window;

  const id = new URLSearchParams(location.search).get("id");
  const idx = PROJECTS.findIndex((p) => p.id === id);

  if (idx < 0) {
    document.title = `Project not found — ${siteName}`;
    root.innerHTML = `
      <nav class="breadcrumb" aria-label="Breadcrumb"><ol>
        <li><a href="index.html">Home</a></li>
        <li><a href="projects.html">Projects</a></li>
        <li aria-current="page">Not found</li>
      </ol></nav>
      <h1 class="detail__title">Project not found</h1>
      <p class="detail__subtitle">That project doesn't exist (any more). It may have been renamed.</p>
      <p style="margin-top:28px"><a class="btn btn--primary" href="projects.html"><i class="bi bi-arrow-left" aria-hidden="true"></i> Back to all projects</a></p>`;
    return;
  }

  const p = PROJECTS[idx];
  const prev = PROJECTS[idx - 1];
  const next = PROJECTS[idx + 1];

  document.title = `${p.title} — ${siteName}`;
  const meta = document.querySelector('meta[name="description"]');
  if (meta) meta.content = `${p.title}: ${p.subtitle}`;

  const para = (heading, text) => (text ? `<section class="detail__section"><h2>${heading}</h2><p>${esc(text)}</p></section>` : "");
  const list = (heading, items) =>
    items && items.length ? `<section class="detail__section"><h2>${heading}</h2><ul>${items.map((r) => `<li>${esc(r)}</li>`).join("")}</ul></section>` : "";

  const LINKS = [
    ["github", "bi-github", "Source code"],
    ["demo", "bi-box-arrow-up-right", "Live demo"],
    ["paper", "bi-file-earmark-text", "Paper"],
  ];
  const links = LINKS.filter(([k]) => p.links && p.links[k])
    .map(([k, icon, label]) => `<a href="${esc(p.links[k])}" target="_blank" rel="noopener"><i class="bi ${icon}" aria-hidden="true"></i>${label}</a>`)
    .join("");

  // "My role": what you did yourself on a team project (an array of points, or { summary, points })
  const role = p.role ? (Array.isArray(p.role) ? { points: p.role } : p.role) : null;
  const roleSection =
    role && (role.summary || (role.points || []).length)
      ? `<section class="detail__section detail__role"><h2>My role</h2>${role.summary ? `<p>${esc(role.summary)}</p>` : ""}${
          (role.points || []).length ? `<ul>${role.points.map((r) => `<li>${esc(r)}</li>`).join("")}</ul>` : ""
        }</section>`
      : "";

  // "Motivation": why the project exists (a paragraph, or { summary, points })
  const motivation = p.motivation ? (typeof p.motivation === "string" ? { summary: p.motivation } : p.motivation) : null;
  const motivationSection =
    motivation && (motivation.summary || (motivation.points || []).length)
      ? `<section class="detail__section"><h2>Motivation</h2>${motivation.summary ? `<p>${esc(motivation.summary)}</p>` : ""}${
          (motivation.points || []).length ? `<ul>${motivation.points.map((r) => `<li>${esc(r)}</li>`).join("")}</ul>` : ""
        }</section>`
      : "";

  // "Lessons learned": what went wrong or surprised you, and what you would do differently
  // (an array of points, or { summary, points }). Shown after the results.
  const lessons = p.lessons ? (Array.isArray(p.lessons) ? { points: p.lessons } : p.lessons) : null;
  const lessonsSection =
    lessons && (lessons.summary || (lessons.points || []).length)
      ? `<section class="detail__section detail__lessons"><h2><i class="bi bi-lightbulb" aria-hidden="true"></i> Lessons learned</h2>${lessons.summary ? `<p>${esc(lessons.summary)}</p>` : ""}${
          (lessons.points || []).length ? `<ul>${lessons.points.map((r) => `<li>${esc(r)}</li>`).join("")}</ul>` : ""
        }</section>`
      : "";

  // credit line for images that are not your own
  const imageCredit = p.imageCredit
    ? `<figcaption class="detail__credit">Image: ${
        p.imageCredit.href
          ? `<a href="${esc(p.imageCredit.href)}" target="_blank" rel="noopener">${esc(p.imageCredit.text)}</a>`
          : esc(p.imageCredit.text)
      }</figcaption>`
    : "";

  // write-ups (e.g. rendered Quarto reports) open in a new tab: they bring their own layout and table of contents
  const reports = (p.reports || []).length
    ? `<section class="detail__section"><h2>Reports</h2><ul class="reports">${p.reports
        .map(
          (r) => `<li><a class="report" href="${esc(r.href)}" target="_blank" rel="noopener">
            <i class="bi bi-file-earmark-bar-graph" aria-hidden="true"></i>
            <span class="report__body">
              <strong>${esc(r.title)}</strong>
              ${r.description ? `<span>${esc(r.description)}</span>` : ""}
              ${r.format ? `<small>${esc(r.format)}</small>` : ""}
            </span>
            <i class="bi bi-box-arrow-up-right report__go" aria-hidden="true"></i>
            <span class="visually-hidden">(opens in a new tab)</span>
          </a></li>`
        )
        .join("")}</ul></section>`
    : "";

  // wide figure (e.g. a system architecture): full width between the text blocks, click opens it full size
  const d = p.diagram;
  const diagram = d && d.src
    ? `<section class="detail__section detail__diagram" aria-labelledby="diagram-title">
        <h2 id="diagram-title">${esc(d.title || "Diagram")}</h2>
        <figure>
          <a href="${esc(d.src)}" target="_blank" rel="noopener">
            <img src="${esc(d.src)}" alt="${esc(d.alt || "")}"${d.width && d.height ? ` width="${esc(d.width)}" height="${esc(d.height)}"` : ""} loading="lazy">
            <span class="visually-hidden">(opens full size in a new tab)</span>
          </a>
          <figcaption>${d.caption ? `${esc(d.caption)} ` : ""}Click to open it full size.</figcaption>
        </figure>
      </section>`
    : "";

  const textTop = `${para("Overview", p.overview)}${roleSection}${reports}${motivationSection}${para("Problem", p.problem)}${para("Approach", p.approach)}`;
  const textRest = `${list("Highlights", p.highlights)}${lessonsSection}`;
  const aside = `<aside class="detail__aside" aria-label="Project facts">
        <h2>Tech stack</h2>
        <ul class="tags">${(p.tech || []).map((t) => `<li class="tag">${esc(t)}</li>`).join("")}</ul>
        ${links ? `<h2>Links</h2><div class="detail__links">${links}</div>` : ""}
      </aside>`;

  // with a diagram the text splits around it; without one the page is exactly as before
  const body = diagram
    ? `<div class="detail__layout"><div>${textTop}</div>${aside}</div>
    ${diagram}
    <div class="detail__layout detail__layout--rest"><div>${textRest}</div></div>`
    : `<div class="detail__layout"><div>${textTop}${textRest}</div>${aside}</div>`;

  const pagerLink = (q, dir) =>
    q ? `<a class="pager__${dir}" href="project.html?id=${encodeURIComponent(q.id)}"><small>${dir === "prev" ? "← Previous" : "Next →"}</small><strong>${esc(q.title)}</strong></a>` : "";

  root.innerHTML = `
    <nav class="breadcrumb" aria-label="Breadcrumb"><ol>
      <li><a href="index.html">Home</a></li>
      <li><a href="projects.html">Projects</a></li>
      <li aria-current="page">${esc(p.title)}</li>
    </ol></nav>

    <h1 class="detail__title">${esc(p.title)}</h1>
    <p class="detail__subtitle">${esc(p.subtitle)}</p>
    <div class="detail__meta">
      <span class="${statusClass(p.status)}">${esc(p.status)}</span>
      <ul class="tags"><li class="tag">${esc(p.domain)}</li><li class="tag">${esc(p.type)}</li></ul>
      ${p.year ? `<span class="detail__year">${esc(p.year)}</span>` : ""}
      ${p.collaborators && p.collaborators.length ? `<span class="detail__with"><i class="bi bi-people" aria-hidden="true"></i> With <strong>${esc(p.collaborators.join(", "))}</strong></span>` : ""}
    </div>

    <figure class="detail__figure">
      <div class="detail__hero"><img src="${projectImage(p, 1260, 630)}" alt="${esc(p.imageAlt || "")}" width="1260" height="630"${p.imagePosition ? ` style="object-position:${esc(p.imagePosition)}"` : ""}></div>
      ${imageCredit}
    </figure>

    ${[].concat(p.note || []).map((n) => `<p class="notice notice--info"><i class="bi bi-info-circle" aria-hidden="true"></i><span>${esc(n)}</span></p>`).join("")}

    ${p.placeholder ? `<p class="notice"><i class="bi bi-info-circle" aria-hidden="true"></i><span>Placeholder content. Replace this project in <code>js/projects-data.js</code>.</span></p>` : ""}

    ${body}

    <nav class="pager" aria-label="More projects">${pagerLink(prev, "prev")}${pagerLink(next, "next")}</nav>`;
})();
