# Laser Welding Multipath (Only Thermal)

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
Weld_I_dflux_multi-path/
├── 01_Files to Run/                  # Case inputs: INP, mesh, required data (no outputs)
├── 02_Results/     # includes results
├── 03_FRD_paraview_converter/     # FRD → ParaView converter/tools
├── 04_Calculix_2.21/                  # Precompiled CalculiX (ccx)
├── 05_Patches/                       # Patches for compiling your own ccx
└── 06_Images/                       
