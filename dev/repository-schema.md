# Chaotic Dynamical Systems Repository Schema

## 1. Package Architecture Overview

```mermaid
graph TB
    subgraph "📦 chaoticds Package Architecture"

        subgraph "Core Simulation Layer"
            SIM[simulate.R<br/>324 lines]
            SIM --> LOG[simulate_logistic_map<br/>1D Chaos]
            SIM --> HEN[simulate_henon_map<br/>2D Attractor]
            SIM --> TENT[simulate_tent_map<br/>Piecewise Linear]
            SIM --> LOZI[simulate_lozi_map<br/>2D Variant]
            SIM --> CAT[simulate_cat_map<br/>Area Preserving]
            SIM --> BIF[logistic_bifurcation<br/>Period Doubling]
        end

        subgraph "Extreme Value Analysis"
            BM[block-maxima.R<br/>300 lines]
            BM --> BMAX[block_maxima<br/>Extract Maxima]
            BM --> GEV[fit_gev<br/>GEV Distribution]

            POT[peaks-over-threshold.R<br/>100 lines]
            POT --> EXC[exceedances<br/>Above Threshold]
            POT --> GPD[fit_gpd<br/>GP Distribution]

            EI[extremal-index.R<br/>318 lines]
            EI --> RUNS[extremal_index_runs<br/>Cluster Method]
            EI --> INT[extremal_index_intervals<br/>Ferro-Segers]
            EI --> HT[hitting_times<br/>Return Times]
        end

        subgraph "Diagnostic Tools"
            THR[threshold-selection.R<br/>79 lines]
            THR --> MRL[mean_residual_life<br/>MRL Plot]
            THR --> HILL[hill_estimates<br/>Tail Index]

            MIX[mixing-diagnostics.R<br/>100 lines]
            MIX --> ACF[acf_decay<br/>Autocorrelation]
            MIX --> MIXC[mixing_coefficients<br/>α-mixing]
            MIX --> DUN[d_check<br/>Leadbetter D_un]
        end

        subgraph "Advanced Methods"
            ADV[advanced-extremes.R<br/>150 lines]
            ADV --> MV[Multivariate<br/>Extremal Index]
            ADV --> NS[Non-stationary<br/>GEV Models]
            ADV --> TD[Tail Dependence<br/>Coefficients]

            MPP[mppexcesses.R<br/>140 lines]
            MPP --> MARK[marked_point_process<br/>Point Process]
            MPP --> CP[fit_compound_poisson<br/>Compound Model]
        end

        subgraph "Analysis Tools"
            CLUST[cluster-statistics.R<br/>70 lines]
            CLUST --> CSIZE[cluster_sizes<br/>Size Distribution]
            CLUST --> CSUM[cluster_summary<br/>Statistics]

            BOOT[bootstrap-ci.R<br/>63 lines]
            BOOT --> BEI[bootstrap_extremal_index<br/>CI for θ]
            BOOT --> BBLOCK[block_bootstrap<br/>General Bootstrap]

            CHAOS[Chaos Analysis]
            CHAOS --> LYA[lyapunov.R<br/>Lyapunov Exponent]
            CHAOS --> FRAC[fractal-dimension.R<br/>Correlation Dim]
            CHAOS --> REC[recurrence-analysis.R<br/>RQA Metrics]
        end

        subgraph "Performance Layer"
            FAST[fast-functions.R]
            OPT[optimized-wrappers.R]
            CPP1[fast_algorithms.cpp<br/>163 lines]
            CPP2[additional_optimizations.cpp<br/>394 lines]

            FAST --> AUTO{Auto-select<br/>R or C++}
            OPT --> AUTO
            AUTO --> CPP1
            AUTO --> CPP2
        end

        subgraph "User Interface"
            SHINY[launch-explorer.R<br/>Shiny App]
            DEMO[run-demo-chaos.R<br/>Complete Workflow]
            UTILS[utils.R<br/>Helpers]
        end

        subgraph "Data Layer"
            DATA[data.R]
            DATA --> D1[logistic_ts.rda<br/>5000 obs]
            DATA --> D2[henon_ts.rda<br/>3000 obs]
            DATA --> D3[ar1_ts.rda<br/>4000 obs]
        end
    end

    subgraph "📚 Documentation"
        DOC[Documentation]
        DOC --> VIG[6 Vignettes]
        DOC --> ROX[Roxygen2<br/>100+ functions]
        DOC --> PKG[pkgdown<br/>Website]
        DOC --> README[README.md<br/>360 lines]
        DOC --> GUIDES[Development<br/>Guides]
    end

    subgraph "🧪 Testing"
        TEST[tests/testthat<br/>19 test files]
        TEST --> TSIM[test-simulate.R]
        TEST --> TEI[test-extremal-index.R]
        TEST --> TBOOT[test-bootstrap.R]
        TEST --> TCLUST[test-clusters.R]
        TEST --> TEDGE[test-edge-cases.R]
        TEST --> TCOMP[test-comprehensive.R]
    end

    subgraph "🔧 CI/CD Pipeline"
        CI[GitHub Actions]
        CI --> CHECK[R-CMD-check<br/>Multi-OS]
        CI --> COV[test-coverage<br/>Codecov]
        CI --> LINT[lintr<br/>Code Quality]
        CI --> BENCH[benchmark<br/>Performance]
        CI --> SEC[security<br/>Scanning]
    end

    %% Key Dependencies
    BMAX -.->|uses| EVD[evd package]
    GEV -.->|uses| ISMEV[ismev package]
    GPD -.->|uses| EVIR[evir package]
    CP -.->|uses| EXTREMES[extRemes package]

    %% Data Flow
    SIM ==>|generates| BMAX
    SIM ==>|generates| EXC
    BMAX ==>|fits| GEV
    EXC ==>|fits| GPD
    EXC ==>|estimates| EI

    %% Performance Flow
    SIM -->|optimized by| CPP1
    BMAX -->|optimized by| CPP1
    EI -->|optimized by| CPP2

    style SIM fill:#e1f5fe
    style BM fill:#fff3e0
    style POT fill:#fff3e0
    style EI fill:#fff3e0
    style ADV fill:#f3e5f5
    style FAST fill:#c8e6c9
    style CPP1 fill:#c8e6c9
    style CPP2 fill:#c8e6c9
    style TEST fill:#ffebee
    style CI fill:#ffebee
    style SHINY fill:#e8f5e9
```

## 2. Analysis Workflow Pipeline

```mermaid
flowchart LR
    subgraph "Workflow Pipeline"
        Start([User Input])

        Start --> Sim{Choose<br/>Chaotic Map}
        Sim --> |Logistic| LOG[r ∈ 3,4]
        Sim --> |Hénon| HEN[a,b params]
        Sim --> |Tent| TENT[μ param]
        Sim --> |Lozi| LOZI[a,b params]
        Sim --> |Cat| CAT[Torus map]

        LOG --> TS[Time Series<br/>Generation]
        HEN --> TS
        TENT --> TS
        LOZI --> TS
        CAT --> TS

        TS --> EVT{Extreme Value<br/>Method}

        EVT --> |Block Maxima| BM[Extract Blocks]
        BM --> GEVFIT[Fit GEV<br/>ξ, μ, σ]

        EVT --> |POT| THR[Select Threshold]
        THR --> MRL[MRL Plot]
        THR --> HILL[Hill Plot]
        MRL --> THRVAL[u threshold]
        HILL --> THRVAL
        THRVAL --> POTFIT[Fit GPD<br/>ξ, σ]

        TS --> EI[Extremal Index]
        EI --> RUNS[Runs Method]
        EI --> INTS[Intervals Method]
        RUNS --> THETA[θ estimate]
        INTS --> THETA

        TS --> DIAG[Diagnostics]
        DIAG --> LYAP[Lyapunov λ]
        DIAG --> FDIM[Fractal D]
        DIAG --> RQA[Recurrence]
        DIAG --> ACF[Autocorrelation]

        GEVFIT --> RESULTS[Results]
        POTFIT --> RESULTS
        THETA --> RESULTS
        LYAP --> RESULTS

        RESULTS --> VIZ[Visualization]
        VIZ --> PLOTS[Static Plots]
        VIZ --> SHINY[Interactive App]

        RESULTS --> BOOT[Bootstrap CI]
        BOOT --> CI95[95% Intervals]

        CI95 --> REPORT([Final Analysis])
    end

    style Start fill:#e8f5e9
    style REPORT fill:#e8f5e9
    style TS fill:#e3f2fd
    style GEVFIT fill:#fff9c4
    style POTFIT fill:#fff9c4
    style THETA fill:#fce4ec
    style VIZ fill:#f3e5f5
```

## 3. Module Dependencies

```mermaid
classDiagram
    class chaoticds {
        <<R Package>>
        +Version: 0.1.0
        +License: MIT
        +Author: Diogo Ribeiro
    }

    class Simulation {
        +simulate_logistic_map()
        +simulate_henon_map()
        +simulate_tent_map()
        +simulate_lozi_map()
        +simulate_cat_map()
        +logistic_bifurcation()
    }

    class BlockMaxima {
        +block_maxima()
        +fit_gev()
        +calculate_return_levels()
    }

    class PeaksOverThreshold {
        +exceedances()
        +fit_gpd()
        +mean_residual_life()
    }

    class ExtremalIndex {
        +extremal_index_runs()
        +extremal_index_intervals()
        +hitting_times()
        +bootstrap_extremal_index()
    }

    class Clustering {
        +cluster_exceedances()
        +cluster_sizes()
        +cluster_summary()
        +cluster_histogram()
    }

    class Diagnostics {
        +acf_decay()
        +mixing_coefficients()
        +d_check()
        +threshold_diagnostics()
        +hill_estimates()
    }

    class ChaosMetrics {
        +estimate_lyapunov_exponent()
        +estimate_correlation_dimension()
        +recurrence_plot()
        +recurrence_analysis()
    }

    class Advanced {
        +extremal_index_multivariate()
        +tail_dependence_coefficient()
        +fit_nonstationary_gev()
        +adaptive_threshold_selection()
    }

    class Performance {
        +simulate_logistic_map_cpp()
        +block_maxima_cpp()
        +extremal_index_runs_cpp()
        +cluster_sizes_cpp()
    }

    class UI {
        +launch_explorer()
        +run_demo()
    }

    chaoticds --> Simulation
    chaoticds --> BlockMaxima
    chaoticds --> PeaksOverThreshold
    chaoticds --> ExtremalIndex
    chaoticds --> Clustering
    chaoticds --> Diagnostics
    chaoticds --> ChaosMetrics
    chaoticds --> Advanced
    chaoticds --> Performance
    chaoticds --> UI

    BlockMaxima ..> Simulation : uses
    PeaksOverThreshold ..> Simulation : uses
    ExtremalIndex ..> PeaksOverThreshold : depends
    Clustering ..> PeaksOverThreshold : analyzes
    Advanced ..> ExtremalIndex : extends
    Performance --|> Simulation : optimizes
    Performance --|> BlockMaxima : optimizes
    Performance --|> ExtremalIndex : optimizes
    UI ..> Simulation : displays
    UI ..> BlockMaxima : visualizes
    UI ..> ExtremalIndex : reports
```

## 4. File Structure Tree

```mermaid
graph TD
    ROOT[chaotic-dynamical-systems/]

    ROOT --> R[R/<br/>22 source files]
    ROOT --> SRC[src/<br/>3 C++ files]
    ROOT --> TESTS[tests/testthat/<br/>19 test files]
    ROOT --> VIG[vignettes/<br/>6 guides]
    ROOT --> DATA[data/<br/>3 datasets]
    ROOT --> GH[.github/workflows/<br/>6 CI/CD pipelines]
    ROOT --> DOCS[Documentation<br/>10+ files]

    R --> R1[simulate.R - 324 lines]
    R --> R2[extremal-index.R - 318 lines]
    R --> R3[block-maxima.R - 300 lines]
    R --> R4[peaks-over-threshold.R - 100 lines]
    R --> R5[advanced-extremes.R - 150 lines]
    R --> R6[mppexcesses.R - 140 lines]
    R --> RMORE[... 16 more files]

    SRC --> CPP1[fast_algorithms.cpp - 163 lines]
    SRC --> CPP2[additional_optimizations.cpp - 394 lines]
    SRC --> CPP3[RcppExports.cpp]

    TESTS --> T1[test-simulate.R]
    TESTS --> T2[test-extremal-index.R]
    TESTS --> T3[test-bootstrap.R]
    TESTS --> TMORE[... 16 more tests]

    VIG --> V1[getting-started.Rmd]
    VIG --> V2[estimating-theta-logistic.Rmd]
    VIG --> V3[block-maxima-vs-pot-henon.Rmd]
    VIG --> VMORE[... 3 more vignettes]

    DATA --> D1[logistic_ts.rda]
    DATA --> D2[henon_ts.rda]
    DATA --> D3[ar1_ts.rda]

    GH --> W1[R-CMD-check.yaml]
    GH --> W2[test-coverage.yaml]
    GH --> W3[lintr.yaml]
    GH --> WMORE[... 3 more workflows]

    DOCS --> README[README.md]
    DOCS --> DESC[DESCRIPTION]
    DOCS --> ROAD[roadmap.md]
    DOCS --> DMORE[... 7 more docs]

    style ROOT fill:#e3f2fd
    style R fill:#fff9c4
    style SRC fill:#c8e6c9
    style TESTS fill:#ffebee
    style VIG fill:#f3e5f5
    style DATA fill:#fff3e0
    style GH fill:#ffccbc
    style DOCS fill:#e1f5fe
```

## 5. Development Status

```mermaid
gantt
    title chaoticds Package Development Progress
    dateFormat  X
    axisFormat %s

    section Core Features
    Simulation Functions     :done, 0, 100
    Block Maxima            :done, 0, 100
    Peaks Over Threshold    :done, 0, 100
    Extremal Index          :done, 0, 100
    Clustering              :done, 0, 100

    section Advanced
    Multivariate Methods    :done, 0, 90
    Non-stationary Models   :done, 0, 85
    Tail Dependence        :done, 0, 90
    Marked Point Process    :done, 0, 95

    section Diagnostics
    Threshold Selection     :done, 0, 100
    Mixing Diagnostics      :done, 0, 100
    Chaos Metrics          :done, 0, 95
    Recurrence Analysis    :active, 0, 80

    section Performance
    C++ Implementation      :done, 0, 100
    Auto-selection         :done, 0, 100
    Parallel Processing    :done, 0, 90

    section Documentation
    Function Docs          :done, 0, 95
    Vignettes             :done, 0, 100
    pkgdown Website       :active, 0, 85

    section Testing
    Unit Tests            :done, 0, 95
    Integration Tests     :done, 0, 90
    CI/CD Pipeline        :done, 0, 100

    section Polish
    CRAN Ready            :active, 0, 85
    Bug Fixes             :active, 0, 90
    Performance Tuning    :done, 0, 95
```

## How to View These Diagrams

You can visualize these Mermaid diagrams in several ways:

1. **VS Code**: Install the "Markdown Preview Mermaid Support" extension
2. **GitHub**: The diagrams will render automatically when you view this file on GitHub
3. **Mermaid Live Editor**: Copy any diagram to https://mermaid.live/
4. **Obsidian**: Native Mermaid support
5. **R Markdown**: Include in your vignettes with `mermaid` code chunks

## Summary

This schema shows that your `chaoticds` package is a well-architected, comprehensive R package for analyzing extreme events in chaotic dynamical systems. The package features:

- **5 chaotic map simulators** with C++ optimizations
- **Complete EVT implementation** (Block Maxima & POT approaches)
- **Multiple extremal index estimators** with bootstrap CI
- **Advanced methods** for multivariate and non-stationary analysis
- **Comprehensive diagnostics** for threshold selection and mixing
- **95% complete** with minor documentation fixes needed
- **Production-ready** with extensive testing and CI/CD