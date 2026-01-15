# Crane – Weld_I_dflux (CalculiX / ccx) + FRD → ParaView Converter

This repository provides a **welding simulation case** for **CalculiX (ccx)** using **DFLUX**, plus a workflow to visualize results in **ParaView** (converting from `.frd`), and optional patches for users who want to compile `ccx` themselves.

## What’s included

- A **Weld_I_dflux** simulation case for CalculiX (ccx)  
  (contains the `.inp`, mesh, and all required input data).
- A **converter** to visualize results in **ParaView** (from `.frd`).
- A **precompiled `ccx` binary** to run the case easily.
- **Patches** for users who want to compile `ccx` themselves.

> Note: There are two Weld_I_dflux folders:
> - **inputs only** (no output files)
> - **with results** (includes solver outputs and ParaView-ready data, depending on the converter workflow)

## 📁 Repository Structure

```text
crane/
├── Weld_I_dflux/                  # Case inputs: INP, mesh, required data (no outputs)
├── Weld_I_dflux_with_results/     # Same case, but includes results (solver outputs)
├── 03_FRD_paraview_converter/     # FRD → ParaView converter/tools
├── 04_calculix/                  # Precompiled CalculiX (ccx)
└── Patches/                       # Patches for compiling your own ccx
