# TRISK Desktop

**Climate transition risk stress testing for banks - locally deployed, fully offline, audit-ready.**

TRISK Desktop allows bank risk teams to run climate transition stress tests on their loan portfolios inside a controlled local environment. Sensitive portfolio data stays on bank infrastructure. Results are transparent, reproducible, and export-ready for ICAAP reports, board presentations, and regulatory submissions.

## How it works

The app guides you through a numbered workflow:

1. **Upload** your portfolio, asset-level data, and financial features
2. **Configure** baseline and target climate scenarios, shock year, and model parameters
3. **Run** the TRISK stress test engine
4. **Review** portfolio results - sector breakdown, concentration risk, attribution, horizon analysis
5. **Compare** outcomes across scenario families (Net Zero, Below 2C, Current Policies)
6. **Integrate** TRISK outputs with your internal PD/EL estimates
7. **Export** everything as Excel, CSV, or JSON

A built-in demo dataset lets you explore the full workflow immediately after installation.

## Key features

- **Local-only deployment** - all data stays on your machine, no cloud transfer
- **114 pre-loaded scenarios** from NGFS, IPR, GECO, and Mission Possible
- **Multi-horizon analysis** - test shock years from 2025 to 2040
- **Risk integration** - combine outputs with internal PD/EL using absolute, relative, or Z-score (Basel IRB) methods
- **Audit trail** - every run gets a unique ID with full configuration metadata
- **Enterprise-hardened** - non-root container, read-only filesystem, column allowlisting, formula injection protection

## Quick links

- [Getting Started](getting-started.md) - install and run your first analysis in 5 minutes
- [Data Preparation](data-preparation.md) - how to format your portfolio CSV files
- [User Guide](workflow/01-upload.md) - step-by-step walkthrough of every tab
- [Security Architecture](security.md) - what bank IT teams need to know

## Built on

- [TRISK Model](https://theia-finance-labs.github.io/trisk.model/) - the stress test engine
- [TRISK Analysis](https://theia-finance-labs.github.io/trisk.analysis/) - portfolio analytics layer
- [1in1000 Initiative](https://1in1000.com) - open-source climate risk methodologies

---

Co-funded by the European Union. Scientific Transition Risk Exercises for Stress Tests & Scenario Analysis has received funding from the European Union's Life programme under Grant No. LIFE21-GIC-DE-Stress.
