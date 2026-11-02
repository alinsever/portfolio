# Portfolio

A static portfolio site: plain HTML, CSS and JavaScript. No build step, no dependencies to install.

Layout inspired by [patrickstaehli.ch](https://patrickstaehli.ch/) (sidebar, hero, timeline resume)
and [datascience.ch/projects](https://datascience.ch/projects) (filterable project grid).

## Run it locally

Open `index.html` directly, or serve the folder (needed if you later add fetch calls):

```
python -m http.server 8000
```

then visit http://localhost:8000.

## What to edit

| What | Where |
|---|---|
| Name, role, sidebar monogram (initials), social links, nav items | `js/layout.js` (the `PROFILE` and `NAV` blocks at the top) |
| Hero text, About, Skills, Resume, Awards, Contact | `index.html` |
| Recommendation letters (excerpt cards and PDF links) | `index.html` (the `#recommendations` section) and the PDFs in `assets/letters/`. Quotes are copied word for word from each letter. |
| Projects (cards, filters, detail pages) | `js/projects-data.js` |
| Profile photo | replace `assets/img/profile-round.webp` (used at the top of the home page hero; the sidebar shows a monogram instead). Use a square image around 480px or larger; the file should have transparent corners so it is round everywhere. |
| Colours, fonts, spacing | `:root` variables at the top of `css/base.css` |

Your name also appears in each page's `<title>`, in `meta description` and in the footer. Search for `Alin Sever` to find every spot.

## Add a project

Add an object to `PROJECTS` in `js/projects-data.js` (the file's header comment documents every field).
The card, the filters, the search, the pagination and the detail page at `project.html?id=<id>` all update on their own.

- `domain` and `type` must match an entry in `DOMAINS` / `TYPES` (edit those lists to add categories).
- Set `featured: true` to show it in the home page strip (the first four are used).
- Put a 16:10 image at e.g. `assets/img/projects/my-project.jpg` and set `image: "assets/img/projects/my-project.jpg"`.
  Without an image, a generated placeholder is used. Add `imagePosition: "50% 35%"` to choose which part stays in frame when it is cropped.
- For a team project, add `collaborators: ["Name", ...]` (credited on the card and the page) and `role: { summary, points: [...] }`. The "My role" section appears right after the overview, set apart, so it is clear what you did yourself.
- Add `lessons: { summary, points: [...] }` for a "Lessons learned" section after the results: what went wrong or surprised you, and what you would do differently. It suits projects where the results were disappointing.
- If an image is not your own, add `imageCredit: { text: "...", href: "https://..." }` and an "Image: ..." credit line appears under the banner on the project page.
- Use `highlights` for the bullet points on the detail page, and `overview`, `problem`, `approach` for paragraphs. All are optional.
- The Domain and Type dropdowns only list values your projects use, and a dropdown is hidden while all projects share one value (for example, every project is currently "Academic").
- Pagination appears once there are more than 8 projects.
- Add `placeholder: true` to a project to show a "placeholder content" notice on its detail page.
- Add `short: "Name"` to give the project a shorter label in the hero graph.
- Add `reports: [{ title, description, format, href }]` to list write-ups on the project page. Each opens in a new tab.
- Add `diagram: { src, alt, title, caption, width, height }` for a wide figure such as a system architecture. It is shown full width after "Approach" and opens full size on click; on phones it scrolls sideways so the text stays readable. Use a lossless WebP or PNG for diagrams, and put the image's pixel size in `width` / `height`.

### Quarto / R reports

Render each `.qmd` to a single self-contained HTML file and put it under `reports/<project>/`:

```
quarto render my_report.qmd --to html -M embed-resources:true
```

then point a `reports` entry at it (see the NHANES BMI project in `js/projects-data.js`). Keep the source `.qmd` files, data and
the large `*_cache` / `*_files` folders in `projects/`; only the rendered HTML in `reports/` is needed on the site, so leave `projects/` out when you deploy.

## The hero graph

The graph on the home page is inspired by Zachary's Karate Club: two communities joined by a few bridges.
The cyan side is your profile (resume, skills, education, experience, awards, contact). The orange side is your projects.
Click a node to jump to that section or page, hover or focus to light up its neighbours, drag a node with the mouse to rearrange.

- **Projects and domains** are read from `js/projects-data.js`, so a new project becomes a new node automatically.
  It is linked to the Projects hub, to its domain, to HSLU (when `type` is "Academic") and to every skill its `tech` list matches.
- **The profile side** (nodes, connections, and which tech names map to which skill) is the config block at the top of `js/graph.js`.
- The layout puts the two communities side by side on wide panels and stacks them on tall, narrow ones. With "reduce motion" turned on in the OS, the graph is still and the pulse animation is off.

## If the browser shows an old version

Browsers cache scripts and styles. After editing a `.js` or `.css` file, press Ctrl+F5 to reload without the cache.
The HTML files link them as `js/graph.js?v=33`; bump that number (in `index.html`, `projects.html` and `project.html`) to force every visitor to fetch the new file.

## Deploy (free)

- **GitHub Pages:** push the folder to a repository, then Settings → Pages → deploy from the `main` branch root.
- **Netlify / Cloudflare Pages:** drag the folder in, no build command, publish directory is the root.

## Structure

```
index.html  projects.html  project.html
css/   base · layout · home · projects · graph
js/    layout (sidebar, drawer, scroll-spy, typer) · projects-data · projects (grid + filters) · project-detail · graph (hero graph)
assets/img/   hero-bg.svg · dots-light.svg · profile-round.webp
assets/letters/   recommendation letters (PDF)
```
