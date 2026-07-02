---
title: TRISK Climate Transition Risk Stress Test
emoji: 🌍
colorFrom: blue
colorTo: green
sdk: docker
app_port: 7860
pinned: false
license: mit
short_description: Climate transition-risk stress testing demo
---

# TRISK — Climate Transition Risk Stress Testing

Interactive demo of **TRISK Desktop**, a tool for assessing how climate
transition scenarios affect a loan/investment portfolio's **NPV, probability
of default (PD), and Expected Loss**.

⚠️ **Public showcase build.** Runs on **synthetic portfolio data** and has
**no authentication**. It is not the hardened build used for real financial
institutions — that version sits behind TLS + Basic Auth in a locked-down
container (see the project's `docker-compose.yml`).

## What you can do here

1. Configure a transition-risk scenario (carbon price path, shock year, discount rate).
2. Run the stress test against the bundled sample portfolio.
3. Explore results — summary, horizon, sector concentration, attribution.
4. Export results to Excel/CSV.

Built with R / Shiny on top of the `trisk.model` and `trisk.analysis` engines
from [Theia Finance Labs](https://github.com/Theia-Finance-Labs).
