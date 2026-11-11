# Alin Sever · Data Science & AI Portfolio

**Live site: [alinsever.github.io/portfolio](https://alinsever.github.io/portfolio/)**

I'm a data scientist with 7+ years of analytics experience in large Canadian federal organisations, now completing an MSc at HSLU in Lucerne, Switzerland. My focus is machine learning, NLP and agentic AI, and this portfolio collects the projects, experience and references behind that work.

I'm open to data science and AI roles, collaborations and interesting problems.

| | |
|---|---|
| **Now** | MSc Applied Information & Data Science, HSLU Lucerne (Feb 2025 – Jun 2027) |
| **Before** | Senior Data Analyst, Immigration, Refugees and Citizenship Canada (2021–2025) |
| **Focus** | Machine learning, deep learning, NLP, RAG systems, agentic AI |
| **Languages** | English (C2), German (B1), Romanian |
| **Tools** | Python, R, SQL, scikit-learn, PyTorch (learning), ChromaDB, Watson Orchestrate, Quarto |

## What's on the site

- **Projects:** nine projects with search and filters. Each page covers the problem, approach and results, and links to the code or a full report where one exists.
- **Resume:** education, experience and certifications as a timeline.
- **Skills:** grouped by area: ML & AI, programming, BI, data engineering and workflow.
- **Recommendations:** five letters from managers and colleagues at PSPC and IRCC, with excerpts and the full PDFs.
- **Interactive graph:** the home page links my profile and my projects in one network. Each node opens the matching section.

## Selected projects

| Project | What it is | Result | Code |
|---|---|---|---|
| **WatsonX IBM Challenge** | Multi-agent platform that tracks 53 Swiss insurance brokers and briefs consultants, built on IBM Watson Orchestrate with a Supabase/pgvector RAG layer. Team of five, around 96% of the code is mine. | The first run on the live app passed 33 of 44 benchmark cases (English and German). After fixing two real bugs and some test-setup problems, all 44 passed. | Under NDA |
| **Reliable Agentic RAG** | Bilingual RAG system over 7,544 ETH Zurich news articles that scores its own evidence and abstains when it is too weak. | 0.0 contradiction rate on the 24 queries it answered. It abstained on the other 26 instead of guessing. | [GitHub](https://github.com/dhub100/advanced-genai-rag) |
| **AI Media Trend Mining** | NLP pipeline on 16,317 AI news articles: topic modelling, LoRA fine-tuning of Qwen2.5, and three RAG setups compared. | Fine-tuning lifted topic accuracy from 34% to 79%. A cross-encoder re-ranker raised the RAG judge score from 1.62 to 1.91. | [GitHub](https://github.com/RobinGirardin/ai-media-trend-mining) |
| **CodeRAG** | RAG assistant that answers questions about course code and PDFs, with source files shown. I built the RAG-Fusion pipeline and the fallback routing. | Evaluated with RAGAS. Runs locally on Llama 3.2 with a Streamlit chat app. | [GitHub](https://github.com/dydy2010/GenAI_RAG_Sys_Code_Navigagor_for_Learning) |
| **Speed Dating Recommender** | Reciprocal recommender that ranks partners before the first date, with leave-one-wave-out evaluation. I built the feature engineering. | Best model reached NDCG@3 of 0.368 but did not significantly beat a popularity baseline. Reported as an open question. | [GitHub](https://github.com/lerenardrouxx/speed_dating_experiment) |
| **LLM Performance & Sustainability** | Scraped and analysed the size, benchmark score and CO₂ cost of popular open-source LLMs. | Size and score correlate at r ≈ 0.75, with gains flattening past 20–30B parameters. | [GitHub](https://github.com/dhub100/CIP_HS2025_205) |
| **NHANES BMI** | R analysis of US health survey data: BMI regression and SVM obesity classification. | Radial SVM reached 76.6% test accuracy. The linear SVM found no obese cases. | [GitHub](https://github.com/alinsever/ml1-nhanes) · [Reports](https://alinsever.github.io/portfolio/project.html?id=nhanes-bmi) |
| **Bitcoin Portfolio** | Backtest of 60/40, All Weather and Cockroach portfolios with Bitcoin added (2016–2025). | A 4–9% Bitcoin allocation improved risk-adjusted returns. It did not behave as a safe haven. | [Report](https://alinsever.github.io/portfolio/project.html?id=bitcoin-portfolio) |
| **Swiss Electricity Transition** | Time-series study of whether solar can replace nuclear in Switzerland's electricity mix. | Annual totals can match, but winter deficits of 5–6 TWh remain without storage and imports. | [Report](https://alinsever.github.io/portfolio/project.html?id=swiss-electricity-transition) |

Team projects say plainly which parts are mine and which are the team's. Several also include a "Lessons learned" section, because what didn't work is often the most useful part.

## Professional background

**Senior Data Analyst, IRCC (2021–2025)**
- Merged 7 data sources into one automated R and Quarto reporting pipeline, cutting delivery from 1 day to under 5 minutes.
- Built an identity-matching algorithm with a 97% match rate across provincial and federal databases.
- Reached a 0% error rate on mission-critical reporting through automated data validation on multi-million-row datasets.
- Designed a processing-time KPI framework that was adopted across the whole department.

**Data Analyst, PSPC (2018–2021)**
- Replaced hours of manual data compiling with SQL and VBA dashboards used by 90+ stakeholders.
- Created a performance deck that was adopted as a template across the organisation.

**Planning & Forecasting Analyst, Millennium 1 Solutions (2015–2018)**
- Cut monthly overtime by about 140 hours with workforce forecasting models, and held billing forecasts to a 2.5% error margin.

**Recognition:** Award of Excellence, PSPC (2019). Award of Excellence, Best Problem Solver, IRCC (2023). IBM SkillsBuild badges in Enterprise Design Thinking and AI solution design.

## How the site is built

The site is plain HTML, CSS and JavaScript, with no framework, no build step and no dependencies to install. It is hosted on GitHub Pages.

- The project cards, filters, search, detail pages and the home page graph are all generated from a single data file, `js/projects-data.js`.
- Written to be accessible: skip link, keyboard-navigable graph, semantic headings, and reduced-motion support.
- Responsive from phone to desktop.
- Long-form analyses are written in R and Quarto and published as self-contained HTML reports under `reports/`.

Layout inspired by [patrickstaehli.ch](https://patrickstaehli.ch/) and [datascience.ch/projects](https://datascience.ch/projects).

To run it locally:

```
python -m http.server 8000
```

then open http://localhost:8000.

## Contact

[LinkedIn](https://www.linkedin.com/in/alin-sever) · [GitHub](https://github.com/alinsever) · or use the contact section on the [site](https://alinsever.github.io/portfolio/#contact)
