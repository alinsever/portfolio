/* ==========================================================================
   projects-data.js — THE file to edit when you add or change projects.

   Each project:
     id         unique slug, used in the URL (project.html?id=coderag)
     title      short name shown on the card
     short      optional shorter label for the hero graph node (falls back to title)
     subtitle   one-line description shown on the card
     status     "In progress" | "Completed"
     domain     one of DOMAINS below (drives the Domain filter)
     type       one of TYPES below   (drives the Type filter, hidden while all projects share one type)
     year       number or text, e.g. 2025 or "2025 – 2026" (optional)
     featured   true -> shown in the "Featured projects" strip on the home page (first four are used)
     image      optional path, e.g. "assets/img/projects/coderag.jpg" (16:10 works best).
                Leave null and a generated placeholder is used.
     imagePosition  optional CSS object-position for which part of the photo stays in frame
                when it is cropped, e.g. "50% 35%" (x y). Default is the centre.
     imageAlt   optional description of the image for screen readers (shown on the detail page)
     imageCredit  optional { text, href } shown as an "Image: ..." credit line under the banner
                on the detail page. Use it whenever the image is not your own.
     collaborators  optional list of names, e.g. ["Cyriel Van Helleputte"]. Shown as
                "With ..." on the project card and the detail page.
     note       optional sentence shown in a note box on the detail page (a disclaimer, data sources, ...).
                Use an array of sentences to get one box each, e.g. a team credit and an availability notice.
     role       optional "My role" section, for team projects: { summary, points: [..] } (or just an array
                of points). Shown right after the overview, so it is clear what you did yourself.
     lessons    optional "Lessons learned" section: { summary, points: [..] } (or just an array of points).
                Shown after the results. Use it for what went wrong or surprised you, and what you would
                do differently. Honest reflections on a project that did not work well are a strength.
     overview / problem / approach   paragraphs (strings, all optional)
     motivation optional "Motivation" section, why the project exists: a paragraph, or { summary, points: [..] }
                for a paragraph followed by a list. Shown after "My role" and the reports, in place of "Problem".
     diagram   optional wide figure (e.g. a system architecture) shown full width after "Approach":
                { src, alt, title, caption, width, height }. Click opens it full size in a new tab.
                width / height (the image's pixel size) stop the page from jumping while it loads.
     highlights array of bullet strings (optional)
     tech       array of strings
     links      { github, demo, paper }  (omit any you don't have)
     reports    optional write-ups listed on the detail page and opened in a new tab:
                [{ title, description, format, href }]. href is a file in this folder
                (e.g. "reports/nhanes-bmi/01_linear_model.html") or a full URL.
     placeholder  true -> shows a "placeholder content" notice. Leave it out for real projects.
   ========================================================================== */

window.DOMAINS = [
  "Agentic AI",
  "NLP & Language",
  "Recommender Systems",
  "Health & Biomedical",
  "Software Engineering",
  "Computer Vision",
  "Finance & Insurance",
  "Climate & Environment",
  "Energy & Sustainability",
  "Other",
];

window.TYPES = ["Academic", "Industry", "Personal", "Open source"];

window.PROJECTS = [
  {
    id: "watsonx-challenge",
    title: "WatsonX IBM Challenge",
    short: "WatsonX",
    subtitle: "A multi-agent platform that tracks Swiss insurance brokers and briefs consultants, built on IBM Watson Orchestrate",
    status: "In progress",
    domain: "Agentic AI",
    type: "Academic",
    year: "Feb 2026 – Present",
    featured: true,
    image: null,
    collaborators: ["Meleknur Ozgu", "Filip Plavsic"],
    note: [
      "Challenge project from my MSc at HSLU, built around a real use case from a large Swiss insurer, in a team of three. Meleknur Ozgu built the alert agent that sends email and Slack alerts, and Filip Plavsic wrote the standalone RAG pipeline and contributed to the agent configurations.",
      "Code and live demo: because of an NDA I cannot share a link yet. The client has agreed that I may share the project as long as their name is removed, so I am cleaning up the repository. The live app ran on IBM Cloud, where my access has ended, so I am moving it to another host such as Render.",
    ],
    role: {
      summary: "I designed and built the platform: about 96% of the code in the repository is mine, counted by line.",
      points: [
        "Designed the data layer in Supabase (Postgres with pgvector): the schema, the embedding pipeline and the search functions that mix 80% semantic similarity with 20% recency.",
        "Built the three scheduled monitors (news, job ads, team pages) and the curator gate behind them: every signal gets an importance score from 0 to 100, uncertain ones go to a human review queue, and saved briefings are de-duplicated.",
        "Designed the Watson Orchestrate setup: one orchestrator, five specialist agents (broker profile, market signal, competitor intel, action playbook and document Q&A) and an internal synthesis composer that merges their answers, all with their API tools.",
        "Built the hybrid runtime: fast helpers grounded in the database answer the common questions, and Watson Orchestrate handles the open-ended ones.",
        "Built the Next.js app (streaming chat, timeline, review queue, document upload, watchlist, login, English and German) and deployed it with the monitors on IBM Code Engine.",
        "Wrote the 44-case English and German evaluation benchmark and the documentation book.",
      ],
    },
    overview: "A broker-intelligence workspace for the business consultants of a large Swiss insurer. It watches 53 Swiss insurance brokers and 11 competing insurers for public signals (news, job ads, team changes, registry updates), stores them in a searchable vector database and lets a consultant ask for a briefing, a meeting plan or a person lookup in English or German. It is built on IBM Watson Orchestrate.",
    motivation: {
      summary: "Switzerland has about 630 insurance brokers. Each business consultant at the insurer looks after 20 to 30 of them and makes 80 to 120 broker visits a year, and preparing a visit means piecing together news, job ads, team changes and registry entries by hand. The team set out to give consultants trusted broker intelligence, proactive monitoring and more time with clients. Four problems stood in the way:",
      points: [
        "Fragmented market signals: news, hiring and registry updates sit on different websites.",
        "Reactive monitoring: consultants hear about a merger or a new hire after the fact.",
        "Slow visit preparation: every visit starts with the same manual research.",
        "Missed risk signals: a warning sign is easy to overlook across dozens of companies.",
      ],
    },
    approach: "Three monitors scan public sources every night and a curator scores each signal. Signals are embedded and stored in Postgres with pgvector, and retrieval mixes semantic similarity with recency. On top sits a team of Watson Orchestrate agents. The Next.js app answers common questions through fast helpers that read straight from the database and only calls the agents for open-ended questions.",
    diagram: {
      src: "assets/img/projects/watsonx-architecture.webp",
      alt: "System architecture in four steps. 1: data collection by a team monitor, a job monitor and a news monitor running as background jobs. 2: a curator agent deduplicates, scores and writes events into a shared Supabase PostgreSQL and pgvector data layer, with embeddings refreshed every 5 minutes. 3: AI orchestration with helper functions, an orchestrator and specialist agents for broker profile, market signals, competitors, playbooks and a synthesis composer. 4: a broker consultant asks a question in natural language and the Next.js app streams the answer. Underneath, source compliance control and human-in-the-loop review reduce the risk of hallucination.",
      title: "System architecture",
      caption: "How the components connect, from data collection to the answer a consultant sees.",
      width: 1229,
      height: 667,
    },
    highlights: [
      "At the last count in May 2026 the system covered 53 brokers and 11 competing insurers, with about 1,050 stored signals, 420+ AI-written briefings and 7 action playbooks with German, French and Italian conversation openers.",
      "The in-app helpers answer in about 0.2 seconds (95th percentile under 0.5 s). When Watson Orchestrate has to reason it through, the median is 12 seconds and the 95th percentile 49 seconds.",
      "In the automated run of the 44-case English and German benchmark on 22 May 2026, the app passed 33 cases (75%) and picked the right route in 95.5% of them (100% for the helpers). All 6 streaming smoke tests passed.",
      "The weak spot in that run was the trap questions (invented brokers, rumoured events, unsupported claims), where only half passed. After I fixed the guardrail, the anti-hallucination checks passed 96.7%.",
    ],
    lessons: {
      summary: "The first full benchmark run on the live app passed only 18 of 44 cases, and the trap questions stayed the weakest group until I fixed the guardrail.",
      points: [
        "Let plain code handle the common questions. A multi-agent setup is slow and flaky for routine requests, so briefings, meeting preparation and lookups moved into deterministic helpers and Watson Orchestrate kept the open-ended ones. A broader Orchestrate prompt made things slower and was rolled back.",
        "Guardrails need their own work. Invented brokers, rumoured events and unsupported claims were the weakest group, with half of them passing, until a dedicated fix raised the anti-hallucination checks to 96.7%.",
        "Test the deployed app, not only the code. The first run exposed missing test documents and answers in the wrong language, which local checks had not shown. After fixing them, 33 of 44 cases passed in the automated run. Reviewing the other 11 showed that six were test-setup artefacts and the rest came from two real bugs; with those fixed, all 44 passed.",
        "Say what you do not know. Office and address data were incomplete, so the helper returns what is stored plus a link to the official website instead of guessing.",
      ],
    },
    tech: ["IBM Watson Orchestrate", "Agentic AI", "RAG", "LLM APIs", "pgvector", "Supabase", "SQL", "Python", "Next.js", "TypeScript", "IBM Code Engine", "Quarto"],
    links: {},
  },
  {
    id: "speed-dating-recommender",
    title: "Speed Dating Recommender",
    short: "Speed Dating",
    subtitle: "Predicting mutual second-date decisions before the first date",
    status: "Completed",
    domain: "Recommender Systems",
    type: "Academic",
    year: 2026,
    featured: false,
    image: null,
    collaborators: ["Alison Barnard", "Andrin Kohler", "Petronela Tabalae"],
    note: "Team project from the HSLU course Recommender Systems. The code lives in a teammate's repository.",
    role: {
      summary: "I did the exploratory data analysis and the feature engineering, and wrote the data and feature sections of the report.",
      points: [
        "Explored the speed-dating data (8,378 rows and 195 columns) and set the constraints the rest of the project followed: the boundary between pre-date and during-date information, the leave-one-wave-out protocol with its complete cold start, and an audit of the survey scales.",
        "Built the leakage-safe feature table (8,368 directed encounters and 102 features, with a data dictionary) that all three methods use, including directional compatibility features that reflect both people's preferences.",
        "Wrote the Data understanding and Feature set and leakage boundary sections of the report.",
      ],
    },
    overview: "A reciprocal recommender for the Columbia speed-dating experiment (Fisman and Iyengar, 2002 to 2004). A match needs both people to say yes, so a good pairing has to satisfy two preference models at once. Using only what is known at signup, the task was to rank each person's candidate partners before the first date so that mutual second-date decisions land in the top three. The team compared a popularity baseline, a gender-asymmetric clustering method and a factorization machine.",
    approach: "The data covers 551 graduate students in 21 speed-dating waves. Evaluation holds out one whole wave at a time (leave-one-wave-out cross-validation), so every held-out participant is a complete cold start. Each candidate list is ranked at k = 3 and scored with Recall, NDCG, Precision, MRR and coverage, all by one shared evaluator, and every method is also tested against a random ranking with a paired t-test and a Wilcoxon test. The baseline ranks people by their predicted desirability, the clustering method scores pairs of participant types, and the factorization machine scores reciprocal pairs with a harmonic mean and is also run without its interaction term.",
    highlights: [
      "The factorization machine ranked best (NDCG@3 of 0.368) and beat a random ranking most clearly (p < 0.001, 17 of 21 folds). The popularity baseline also beat chance (p = 0.011).",
      "Its lead over the popularity baseline (NDCG@3 of 0.348) was small and not statistically significant (p = 0.435), so the lead could not be shown to be real.",
      "The clustering method did not reliably beat a random ranking (9 of 21 folds, p = 0.174): with only four types per gender, most candidates received identical scores.",
      "Switching off the factorization machine's interaction term changed nothing (NDCG@3 of 0.367 against 0.368), so the gain came from its larger feature set, not from modelling pairs of features.",
      "The personalised methods reached far more of the candidate pool than the popularity baseline (coverage of 65% for the factorization machine against 28%), but the factorization machine ranked men's candidates better than women's (NDCG@3 gap of 0.031).",
    ],
    lessons: {
      summary: "The best model only narrowly beat a simple popularity baseline, and not by a statistically significant margin. So what I took away is mostly about how to build and judge a recommender honestly.",
      points: [
        "Know what is available at prediction time. Several of the strongest predictors in the data were collected during the date, so they could not be used for recommendations made before it.",
        "The evaluation protocol shapes the features. Holding out whole waves makes every participant a complete cold start, so participant history could not be used.",
        "A reciprocal problem needs both sides. A pairing only works if both people say yes, so the features have to reflect both people's preferences.",
        "Normalising a scale does not make it comparable. The survey scales differed in meaning, not just in range.",
        "Compare with a simple baseline and test the difference. The factorization machine beat the popularity baseline on every metric, but not significantly over 21 folds, so we reported it as an open question instead of a win.",
        "Agree on one shared evaluator before building the methods. Our separately written ranking metrics quietly drifted apart and had to be reconciled afterwards.",
        "Methods from papers do not always transfer. The clustering approach came from a paper on an online dating recommender, but on our data it could not tell candidates apart.",
      ],
    },
    tech: ["Python", "pandas", "scikit-learn", "PyTorch", "Factorization machines", "Statistical testing", "Jupyter", "Quarto"],
    links: { github: "https://github.com/lerenardrouxx/speed_dating_experiment" },
  },
  {
    id: "reliable-agentic-rag",
    title: "Reliable Agentic RAG",
    short: "Agentic RAG",
    subtitle: "A bilingual RAG system that judges its evidence and abstains when it should",
    status: "Completed",
    domain: "Agentic AI",
    type: "Academic",
    year: 2026,
    featured: false,
    image: "assets/img/projects/reliable-agentic-rag.webp",
    imageAlt: "Diagram of the Reliable Agentic RAG system: a bilingual knowledge base feeds a glowing agentic core protected by a shield, which either passes its answers to an evaluation step marked accurate and bilingual, or abstains with the label insufficient confidence",
    collaborators: ["Robin Girardin", "Daniel Huber"],
    note: "Team project from the HSLU course Advanced Generative AI (spring 2026). The code lives in a teammate's repository.",
    role: {
      summary: "I owned four of the six reliability mechanisms (A, D, E and G) and worked on the architecture, evaluation and conclusions with Robin and Daniel.",
      points: [
        "Evidence sufficiency (A): scores whether the retrieved chunks can answer the question, from semantic coverage, chunk support, aspect coverage and temporal validity, then routes the query to answer, recover, clarify or abstain.",
        "Recovery (G): when the evidence falls short, rewrites the query, switches the retrieval strategy or expands it with pseudo-relevance feedback, with up to two attempts.",
        "Clarification (D) and abstention (E): ambiguous questions get a clarification route, and when recovery is exhausted the system returns a structured abstention instead of guessing.",
        "Worked on the redesigned architecture and the conclusions with Robin and Daniel, and on the evaluation (reliability metrics, ablations, extended benchmark) with Daniel.",
      ],
    },
    overview: "The third-semester capstone project of the HSLU MSc in Applied Information and Data Science, for the course Advanced Generative AI. It builds on a bilingual (English and German) agentic RAG system over the ETH Zurich news corpus (7,544 documents) and asks how such a system can become more reliable: it should judge whether its evidence is sufficient, check that its answer is supported, and abstain instead of giving an unreliable answer.",
    approach: "The baseline retrieves with BM25, dense search (multilingual E5 embeddings in ChromaDB) and a GraphRAG knowledge graph, fuses the results with weighted Reciprocal Rank Fusion, and answers with Mistral-7B-Instruct. The redesign wraps it in a reliability-aware orchestrator with two quality gates. Before answering, an evidence analyst scores whether the retrieved material is sufficient. After answering, a groundedness verifier (NLI entailment) and a trust scorer decide whether the answer is returned or withheld. Every step is traced. The system is compared with the baseline using reliability metrics, ablations and a 15-case extended benchmark.",
    highlights: [
      "On the standard benchmark, all 24 answered queries got a grounded response with a contradiction rate of 0.0, and the system abstained on the other 26 (52%) instead of answering.",
      "Retrieval metrics (MRR, precision, NDCG) are identical with and without the reliability layer, so the gains come from the checks, not from better retrieval.",
      "On the 15-case extended benchmark it handled adversarial and insufficient-evidence queries 3 out of 3 times, ambiguous queries 2 out of 3, and conflicting-evidence queries 1 out of 2.",
      "Limitations: abstention is very conservative, a grounded answer can still be wrong, and German and code-switched queries are weak.",
    ],
    tech: ["Python", "Agentic RAG", "BM25", "GraphRAG", "ChromaDB", "Multilingual E5", "Mistral-7B", "NLI verification", "Quarto"],
    links: { github: "https://github.com/dhub100/advanced-genai-rag" },
  },
  {
    id: "coderag",
    title: "CodeRAG",
    subtitle: "RAG assistant that answers questions about course code and PDFs",
    status: "Completed",
    domain: "NLP & Language",
    type: "Academic",
    year: 2025,
    featured: true,
    image: "assets/img/projects/coderag.webp",
    imageAlt: "Illustration of CodeRAG: a code snippet and the course files feed a search that expands a question into several queries, retrieves and merges the best chunks, and returns an answer with its sources",
    collaborators: ["Cyriel Van Helleputte", "Robin Girardin", "Robin Ramiro Díez-Liébana", "Dongyuan Gao"],
    note: "Team project from the HSLU GenAI course. The code lives in a teammate's repository.",
    role: {
      summary: "My main contribution was the Fusion RAG system and the fallback to a general LLM answer.",
      points: [
        "Designed and implemented the RAG-Fusion pipeline: several query variations combined with Reciprocal Rank Fusion (RRF) to improve retrieval.",
        "Built the fallback routing: when the retrieved context does not contain the answer, the assistant says so and falls back to a general LLM answer.",
        "Tuned the query generation and ranking parameters (RRF-k and top-k retrieval).",
      ],
    },
    overview: "CodeRAG is a retrieval-augmented generation (RAG) assistant for data science students. They ask questions in natural language about their own course code (Python, R, notebooks, Quarto) and PDFs, and get answers grounded in those materials, with the source files listed alongside.",
    approach: "Course code is parsed to JSON and PDFs are split into 1,500-character chunks, then embedded with Qwen3-Embedding-8B and stored in ChromaDB. In the core pipeline, a question is expanded into four search queries, each retrieves the top five chunks, and the lists are merged with Reciprocal Rank Fusion (k = 60). A local Llama 3.2 model, served through Ollama, answers from that context in a Streamlit chat app. Answers are shown with their source files and an optional code preview. Quality is measured with RAGAS: faithfulness, answer relevancy and context precision. The code is organised into indexing, core pipeline and evaluation modules, with documentation and Git version control.",
    tech: ["Python", "LangChain", "ChromaDB", "Ollama", "Llama 3.2", "Qwen3 embeddings", "RAG-Fusion", "RAGAS", "Streamlit", "Git"],
    links: { github: "https://github.com/dydy2010/GenAI_RAG_Sys_Code_Navigagor_for_Learning" },
  },
  {
    id: "ai-media-trend-mining",
    title: "AI Media Trend Mining",
    short: "Media Mining",
    subtitle: "An NLP pipeline on 16,317 AI news articles, from topic modelling to RAG",
    status: "Completed",
    domain: "NLP & Language",
    type: "Academic",
    year: 2026,
    featured: true,
    image: null,
    collaborators: ["Robin Girardin", "Daniel Huber"],
    note: "Team project from the HSLU course Computational Language Technologies. The code lives in a teammate's repository.",
    role: {
      summary: "I fine-tuned the Qwen model in Stage 2, and in Stage 3 I wrote the evaluation of the three RAG setups and the re-ranking enhancement.",
      points: [
        "Stage 1 (data cleaning, exploration, topic modelling and the knowledge graph): worked on it together with Robin and Daniel.",
        "Stage 2: fine-tuned Qwen2.5-1.5B for topic classification with LoRA (4-bit quantisation, two epochs on an A100), and wrote the stage introduction and the first evaluation chapter.",
        "Stage 3: built the evaluation of all three RAG configurations, with a custom LLM judge (answer relevance, faithfulness, context grounding) and RAGAS, and compared the two frameworks.",
        "Stage 3: implemented the cross-encoder re-ranking enhancement of the baseline RAG and measured its effect on the same benchmark.",
      ],
    },
    overview: "A three-stage project for the HSLU course Computational Language Technologies, on a Kaggle dataset of 16,317 news articles about AI published between September 2024 and August 2025. Stage 1 cleans and explores the articles, models topics and sentiment, and builds a knowledge graph. Stage 2 compares classical and fine-tuned embedding models. Stage 3 builds a retrieval-augmented question answering system and tests whether a knowledge graph or a re-ranker improves it.",
    approach: "Stage 1 produced cleaned article chunks, sentiment labels, BERTopic topics and an entity-relation knowledge graph. Stage 2 trained Word2Vec and FastText embeddings and compared them with fine-tuned TinyLlama, Gemma 2 and Qwen2.5 models and a frozen Gemma embedding model on sentiment and topic classification. Stage 3 built a benchmark of 215 reviewed question and answer pairs, and compared a text-only RAG baseline (ChromaDB) with two knowledge-graph variants, scored by a custom LLM judge and by RAGAS.",
    highlights: [
      "Classical Word2Vec embeddings with logistic regression gave the best cost-to-performance ratio: a Macro-F1 of 0.752 on sentiment against 0.761 for the best fine-tuned model, at about a 625th of its carbon footprint.",
      "The fine-tuned Qwen2.5-1.5B jumped from 34.3% to 79.4% accuracy on topic classification, but still trailed the classical models (Macro-F1 of 0.720 against 0.776). Likely cause: articles were truncated to 512 tokens, while the median article is over 1,400.",
      "A frozen, pretrained Gemma embedding model beat every other model on both tasks without any training.",
      "All three RAG setups scored poorly with the LLM judge (averages between 1.2 and 1.8 out of 5). The plain text-only baseline came out best, and the knowledge-graph versions did not help: their extra context added noise.",
      "Adding a cross-encoder re-ranker raised the baseline's mean judge score from 1.62 to 1.91 (faithfulness from 1.60 to 2.40), most clearly on factual and analytical questions, while context grounding fell from 1.60 to 1.27.",
    ],
    tech: ["Python", "BERTopic", "Word2Vec & FastText", "LoRA fine-tuning", "Qwen2.5", "ChromaDB", "RAG", "Knowledge graphs", "Re-ranking", "RAGAS", "Quarto"],
    links: { github: "https://github.com/RobinGirardin/ai-media-trend-mining" },
  },
  {
    id: "nhanes-bmi",
    title: "NHANES BMI",
    subtitle: "BMI regression and obesity classification on a large health dataset",
    status: "Completed",
    domain: "Health & Biomedical",
    type: "Academic",
    year: 2025,
    featured: true,
    image: "assets/img/projects/nhanes-bmi.png",
    imageAlt: "The NHANES logo: a red apple outline with a heartbeat line, and the word nhanes",
    imageCredit: { text: "NHANES logo, CDC National Center for Health Statistics", href: "https://www.cdc.gov/nchs/nhanes-participants" },
    overview: "An R analysis of the National Health and Nutrition Examination Survey (NHANES), a representative sample of the U.S. population. It covers data preprocessing, exploratory analysis, linear regression of BMI and support vector machine (SVM) classification of obesity.",
    highlights: [
      "Estimated BMI from demographic and health variables using regression models, and validated the predictive stability of SVM-based obesity classification across cross-validation folds.",
      "Improved model interpretability through feature engineering and exploratory data analysis, identifying the key predictive variables in a large health dataset.",
      "The linear regression found race/ethnicity, physical activity, smoking status and systolic blood pressure to be relevant predictors of BMI, but explains only about 8.8% of its variation.",
      "A radial SVM reached 76.6% test accuracy (61.3% sensitivity, 84.3% specificity), where the linear SVM identified no obese individuals at all and only matched the 66.7% no-information rate.",
    ],
    tech: ["R", "tidyverse", "caret", "Linear regression", "SVM", "Statistical modelling", "Quarto"],
    links: { github: "https://github.com/alinsever/ml1-nhanes" },
    reports: [
      {
        title: "Linear Regression Analysis: Predicting Body Mass Index",
        description: "Exploring demographic, socioeconomic, lifestyle, and clinical factors associated with BMI using NHANES data.",
        format: "Quarto report · R",
        href: "reports/nhanes-bmi/01_linear_model.html",
      },
      {
        title: "Predicting Obesity with Support Vector Machines",
        description: "Comparing linear and radial SVM models for obesity classification using NHANES data.",
        format: "Quarto report · R",
        href: "reports/nhanes-bmi/02_support_vector_machine.html",
      },
    ],
  },
  {
    id: "bitcoin-portfolio",
    title: "Bitcoin Portfolio",
    short: "Bitcoin",
    subtitle: "Risk or refuge? Bitcoin, the S&P 500 and bonds in diversified portfolios",
    status: "Completed",
    domain: "Finance & Insurance",
    type: "Academic",
    year: 2025,
    featured: false,
    image: "assets/img/projects/bitcoin-portfolio-coin.webp",
    imageAlt: "A glowing Bitcoin coin on a dark blue background",
    collaborators: ["Cyriel Van Helleputte"],
    note: "An empirical observation based on historical data, not investment advice. Findings may not predict future performance.",
    overview: "A study of whether Bitcoin improves a diversified portfolio or remains a speculative risk asset. It compares Bitcoin with the S&P 500, long-term U.S. Treasury bonds and the VIX volatility index from 2016 to 2025, then backtests three portfolio frameworks: 60/40, All Weather and Cockroach.",
    highlights: [
      "Bitcoin remains highly volatile but has low correlation with both equities and bonds. It does not behave as a pure safe haven in market stress.",
      "Across all three frameworks, modest Bitcoin allocations improved annualised returns and risk-adjusted performance, with the strongest results at roughly 4% to 9%, especially when funded from the bond component.",
      "Conclusion: Bitcoin should not replace traditional safe assets, but it may serve as a strategic complement in portfolio construction.",
    ],
    tech: ["R", "Time series", "Portfolio backtesting", "Granger causality", "quantmod", "PerformanceAnalytics", "Quarto"],
    links: {},
    reports: [
      {
        title: "Risk or Refuge? Bitcoin, S&P 500, and Bonds",
        description: "Assessing asset interactions under market stress within a diversified portfolio framework.",
        format: "Quarto report · R",
        href: "reports/bitcoin-portfolio/Bitcoin_portfolio_analysis.html",
      },
    ],
  },
  {
    id: "swiss-electricity-transition",
    title: "Swiss Electricity Transition",
    short: "Swiss Electricity",
    subtitle: "Can solar power replace nuclear in Switzerland's electricity mix?",
    status: "Completed",
    domain: "Energy & Sustainability",
    type: "Academic",
    year: 2025,
    featured: false,
    image: "assets/img/projects/swiss-electricity-transition.webp",
    imageAlt: "Alpine valley with the Matterhorn, a hydropower dam and reservoir, and electricity pylons, with two hikers on a trail in the foreground",
    note: "Data: Swiss Federal Office of Energy (electricity statistics) and MeteoSwiss (OGD-NBCN solar irradiation).",
    overview: "A data-driven assessment of whether Switzerland can keep its electricity supply reliable all year as it phases out nuclear power and expands solar. It uses official Swiss energy and solar-radiation data (2020 to 2024) with exploratory analysis, seasonal decomposition, correlation studies and time-series forecasting.",
    highlights: [
      "Solar can match nuclear in annual production totals, but winter production stays too low to meet demand without additional balancing mechanisms.",
      "In a nuclear-free scenario with scaled solar, Switzerland still faces winter deficits of about 5 to 6 TWh, alongside large summer surpluses.",
      "Hydropower flexibility and imports already stabilise the system, and that reliance will grow as nuclear declines. A successful transition also needs seasonal storage, flexible hydro, smart-grid upgrades and coordinated cross-border flows.",
    ],
    tech: ["R", "tidyverse", "Time series forecasting", "fable", "feasts", "leaflet", "Quarto"],
    links: {},
    reports: [
      {
        title: "Swiss Electricity System Under Transition",
        description: "Assessing whether solar power can replace nuclear energy in Switzerland's future electricity mix.",
        format: "Quarto report · R",
        href: "reports/swiss-electricity-transition/Swiss_Electricity_Transition_Project.html",
      },
    ],
  },
  {
    id: "llm-performance-analysis",
    title: "LLM Performance & Sustainability",
    short: "LLM Analysis",
    subtitle: "Size, benchmark score and CO₂ cost of popular open-source LLMs",
    status: "Completed",
    domain: "NLP & Language",
    type: "Academic",
    year: 2025,
    featured: false,
    image: null,
    collaborators: ["Daniel Huber", "Robin Girardin"],
    note: "Team project from the CIP course, autumn semester 2025. The code lives in a teammate's repository.",
    role: {
      summary: "I prepared and cleaned the data, and developed the analysis and visualisations together with Daniel and Robin.",
      points: [
        "Checked the API and scraped datasets for duplicates and missing values, standardised the model names and reformatted the time variables.",
        "Merged the two datasets on the model name and filled the missing model sizes through targeted web scraping.",
        "Converted model sizes to billions of parameters, cleaned the benchmark percentages and CO₂ indicator, and mapped the model-type symbols to text labels.",
        "Saved the final clean dataset that the whole analysis builds on.",
        "Co-developed the data analysis and visualisation with Daniel and Robin.",
      ],
    },
    overview: "A data analysis of how the size, benchmark performance and carbon cost of popular large language models relate. The team collected metadata for the top 100 most-downloaded text-generation models through the Hugging Face API, scraped benchmark scores from the Hugging Face Open LLM Leaderboard, and analysed three questions: which models give the best size-to-performance ratio, which are the most cost-effective, and how open-source models differ from proprietary ones.",
    approach: "A Python class pulls the model metadata (author, licence, language, size) from the Hugging Face API, and a Selenium and BeautifulSoup scraper collects the leaderboard scores model by model. After cleaning and an inner join on the model name, size against score is analysed with LOESS smoothing and Pearson correlation, CO₂ cost against likes per download with a density plot, and open-source against proprietary models with Welch's t-test and ANCOVA. The write-up is a Quarto report.",
    highlights: [
      "Model size and average benchmark score are strongly correlated (Pearson r of about 0.75), and the gains flatten beyond roughly 20 to 30 billion parameters.",
      "Qwen 2.5-32B-Instruct has the highest average score (about 46.6%) despite being half the size of Meta-Llama 3 70B.",
      "No significant performance difference between open-source and proprietary models (Welch's t-test: t = -1.23, p = 0.24), and access type shows no measurable effect once size is controlled for.",
      "Limitation: the scraper could not handle the leaderboard's scrollable table, so the sample is small and trends above 20 billion parameters stay unclear.",
    ],
    tech: ["Python", "pandas", "Selenium", "BeautifulSoup", "Hugging Face API", "Statistical analysis", "Seaborn", "Plotnine", "Quarto"],
    links: { github: "https://github.com/dhub100/CIP_HS2025_205" },
  },
];
