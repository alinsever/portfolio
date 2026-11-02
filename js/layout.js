/* ==========================================================================
   layout.js — shared chrome for every page
   - injects the sidebar (single source of truth, edit PROFILE / NAV below)
   - mobile drawer, scroll-spy, back-to-top, typed role line, scroll reveal
   ========================================================================== */
(() => {
  "use strict";

  /* ---- edit me -------------------------------------------------------- */
  const PROFILE = {
    name: "Alin Sever",
    role: "Data Science & AI",
    initials: "AS", // monogram shown at the top of the sidebar (the photo is in the home page hero)
    social: [
      { icon: "bi-github", label: "GitHub", href: "https://github.com/alinsever" },
      { icon: "bi-linkedin", label: "LinkedIn", href: "https://www.linkedin.com/in/alin-sever" },
      { icon: "bi-envelope-fill", label: "Email", href: "mailto:alinsever0@gmail.com" },
    ],
  };

  // `section` = id of the home-page section that should light this item up while scrolling
  const NAV = [
    { id: "about", label: "About", icon: "bi-person", href: "#about" },
    { id: "skills", label: "Skills", icon: "bi-stack", href: "#skills" },
    { id: "resume", label: "Resume", icon: "bi-file-earmark-text", href: "#resume" },
    { id: "projects", label: "Projects", icon: "bi-grid-3x3-gap", href: "projects.html", section: "featured" },
    { id: "recommendations", label: "Recommendations", icon: "bi-chat-quote", href: "#recommendations" },
    { id: "contact", label: "Contact", icon: "bi-envelope", href: "#contact" },
  ];
  /* --------------------------------------------------------------------- */

  const reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  const page = document.body.dataset.page || "home"; // home | projects | project
  const onHome = page === "home";
  const el = (html) => {
    const t = document.createElement("template");
    t.innerHTML = html.trim();
    return t.content.firstElementChild;
  };

  const Portfolio = (window.Portfolio = window.Portfolio || {});
  Portfolio.siteName = PROFILE.name;

  /* ---- sidebar -------------------------------------------------------- */
  const navHref = (item) => (onHome || !item.href.startsWith("#") ? item.href : "index.html" + item.href);
  const isCurrentPage = (item) => item.id === "projects" && (page === "projects" || page === "project");

  const sidebar = el(`
    <header class="sidebar" id="sidebar">
      <a class="sidebar__mark" href="index.html" aria-label="${PROFILE.name} — home">
        <svg viewBox="0 0 96 96" aria-hidden="true" focusable="false">
          <defs>
            <linearGradient id="mark-disc" x1="0" y1="0" x2="1" y2="1">
              <stop offset="0" stop-color="#17396d"/>
              <stop offset="1" stop-color="#0a1c39"/>
            </linearGradient>
          </defs>
          <circle cx="48" cy="48" r="45" fill="rgba(255,255,255,.08)"/>
          <circle cx="48" cy="48" r="39" fill="url(#mark-disc)"/>
          <line x1="21" y1="75" x2="75" y2="21" stroke="#fff" stroke-opacity=".28" stroke-width="1.4" stroke-dasharray="2 4" stroke-linecap="round"/>
          <text x="48" y="59" text-anchor="middle" font-size="29">${PROFILE.initials}</text>
          <circle cx="75" cy="21" r="5.5" fill="#4cc9f0" stroke="#fff" stroke-opacity=".9" stroke-width="1.5"/>
          <circle cx="21" cy="75" r="5.5" fill="#ff9f6e" stroke="#fff" stroke-opacity=".9" stroke-width="1.5"/>
        </svg>
      </a>
      <p class="sidebar__name"><a href="index.html">${PROFILE.name}</a></p>
      <p class="sidebar__role">${PROFILE.role}</p>
      <div class="social">
        ${PROFILE.social
          .map((s) => `<a href="${s.href}" aria-label="${s.label}" ${s.href.startsWith("mailto") ? "" : 'target="_blank" rel="noopener"'}><i class="bi ${s.icon}" aria-hidden="true"></i></a>`)
          .join("")}
      </div>
      <nav class="sidebar__nav" aria-label="Main">
        <ul>
          ${NAV.map(
            (n) => `<li><a href="${navHref(n)}" data-nav="${n.id}"${isCurrentPage(n) ? ' class="is-active" aria-current="page"' : ""}>
              <i class="bi ${n.icon}" aria-hidden="true"></i><span>${n.label}</span></a></li>`
          ).join("")}
        </ul>
      </nav>
      <p class="sidebar__foot">© ${new Date().getFullYear()} ${PROFILE.name}</p>
    </header>`);

  const toggle = el(`<button class="nav-toggle" type="button" aria-controls="sidebar" aria-expanded="false" aria-label="Open menu"><i class="bi bi-list" aria-hidden="true"></i></button>`);
  const scrim = el(`<div class="scrim" aria-hidden="true"></div>`);
  const toTop = el(`<button class="to-top" type="button" aria-label="Back to top"><i class="bi bi-arrow-up-short" aria-hidden="true"></i></button>`);

  const slot = document.getElementById("site-sidebar");
  if (slot) slot.replaceWith(sidebar);
  else document.body.prepend(sidebar);
  document.body.append(toggle, scrim, toTop);

  /* ---- mobile drawer -------------------------------------------------- */
  const setNav = (open) => {
    document.body.classList.toggle("nav-open", open);
    toggle.setAttribute("aria-expanded", String(open));
    toggle.setAttribute("aria-label", open ? "Close menu" : "Open menu");
    toggle.firstElementChild.className = "bi " + (open ? "bi-x-lg" : "bi-list");
    if (open) sidebar.querySelector("a[data-nav]").focus({ preventScroll: true });
  };
  toggle.addEventListener("click", () => setNav(!document.body.classList.contains("nav-open")));
  scrim.addEventListener("click", () => setNav(false));
  sidebar.addEventListener("click", (e) => { if (e.target.closest("a[data-nav]")) setNav(false); });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && document.body.classList.contains("nav-open")) { setNav(false); toggle.focus(); }
  });
  window.matchMedia("(min-width: 1200px)").addEventListener("change", (e) => { if (e.matches) setNav(false); });

  /* ---- back to top ---------------------------------------------------- */
  const onScroll = () => toTop.classList.toggle("is-shown", window.scrollY > 500);
  window.addEventListener("scroll", onScroll, { passive: true });
  onScroll();
  toTop.addEventListener("click", () => window.scrollTo({ top: 0, behavior: reduceMotion ? "auto" : "smooth" }));

  /* ---- scroll-spy (home only) ----------------------------------------- */
  if (onHome && "IntersectionObserver" in window) {
    const links = new Map(NAV.map((n) => [n.section || n.id, sidebar.querySelector(`[data-nav="${n.id}"]`)]));
    const spy = new IntersectionObserver(
      (entries) => {
        entries.forEach((entry) => {
          if (!entry.isIntersecting) return;
          links.forEach((a, key) => {
            const active = key === entry.target.id;
            a.classList.toggle("is-active", active);
            if (active) a.setAttribute("aria-current", "location");
            else a.removeAttribute("aria-current");
          });
        });
      },
      { rootMargin: "-40% 0px -55% 0px" }
    );
    links.forEach((_, id) => { const s = document.getElementById(id); if (s) spy.observe(s); });
  }

  /* ---- scroll reveal -------------------------------------------------- */
  const io = "IntersectionObserver" in window
    ? new IntersectionObserver((entries) => {
        entries.forEach((en) => {
          if (en.isIntersecting) { en.target.classList.add("is-visible"); io.unobserve(en.target); }
        });
      }, { rootMargin: "0px 0px -8% 0px", threshold: 0.08 })
    : null;

  // also used by projects.js for cards it renders later
  Portfolio.reveal = (root = document) => {
    root.querySelectorAll(".reveal:not(.is-visible)").forEach((n) => (io ? io.observe(n) : n.classList.add("is-visible")));
  };
  Portfolio.reveal();

  /* ---- contact fallback ----------------------------------------------- */
  // A mailto: link does nothing on a computer without a mail app, so the address is also shown
  // under the contact buttons with a copy button. It is read from PROFILE, the single source.
  const mailto = (PROFILE.social.find((s) => s.href.startsWith("mailto:")) || {}).href;
  const ctaLinks = document.querySelector(".cta__links");
  if (mailto && ctaLinks) {
    const address = mailto.replace(/^mailto:/, "");
    ctaLinks.after(el(`
      <p class="cta__address">No mail app opening? Write to <strong>${address}</strong>
        <button class="copy-btn" type="button" data-copy="${address}"><i class="bi bi-clipboard" aria-hidden="true"></i><span>Copy</span></button>
        <span class="copy-status" role="status" aria-live="polite"></span>
      </p>`));
  }

  const copyText = async (text) => {
    try {
      // some browser states leave writeText pending forever, so give it a second and then use the fallback
      await Promise.race([navigator.clipboard.writeText(text), new Promise((_, no) => setTimeout(no, 1000))]);
      return true;
    } catch { /* insecure page, blocked or too slow: use the fallback */ }
    try {
      const t = document.createElement("textarea");
      t.value = text;
      t.setAttribute("readonly", "");
      t.style.cssText = "position:fixed;opacity:0";
      document.body.append(t);
      t.select();
      const ok = document.execCommand("copy");
      t.remove();
      return ok;
    } catch { return false; }
  };
  let copyTimer;
  document.addEventListener("click", async (e) => {
    const btn = e.target.closest("[data-copy]");
    if (!btn) return;
    const ok = await copyText(btn.dataset.copy);
    const status = btn.parentElement.querySelector(".copy-status");
    if (!ok) { // select the address so Ctrl+C works
      const strong = btn.parentElement.querySelector("strong");
      if (strong) { const r = document.createRange(); r.selectNodeContents(strong); const s = getSelection(); s.removeAllRanges(); s.addRange(r); }
    }
    if (status) {
      status.textContent = ok ? "Copied" : "Selected: press Ctrl+C";
      clearTimeout(copyTimer);
      copyTimer = setTimeout(() => (status.textContent = ""), 3000);
    }
  });

  /* ---- typed role line ------------------------------------------------ */
  const typed = document.querySelector("[data-typed]");
  if (typed) {
    const roles = typed.dataset.typed.split("|").map((s) => s.trim()).filter(Boolean);
    if (reduceMotion || roles.length < 2) {
      typed.textContent = roles[0] || "";
    } else {
      let r = 0, i = 0, deleting = false;
      const tick = () => {
        const word = roles[r];
        i += deleting ? -1 : 1;
        typed.textContent = word.slice(0, i);
        let wait = deleting ? 38 : 82;
        if (!deleting && i === word.length) { deleting = true; wait = 1700; }
        else if (deleting && i === 0) { deleting = false; r = (r + 1) % roles.length; wait = 380; }
        setTimeout(tick, wait);
      };
      typed.textContent = "";
      setTimeout(tick, 900);
    }
  }
})();
