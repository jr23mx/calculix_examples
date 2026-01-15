# Weld_I_dflux (CalculiX / ccx) + FRD → ParaView Converter

This repository includes:
- A **Weld_I_dflux** simulation case for **CalculiX (ccx)** (contains the `.inp`, mesh, and all required input data).
- A converter to visualize results in **ParaView** (from **.frd**).
- A **precompiled ccx** binary to run the case easily.
- **Patches** for users who want to compile ccx themselves.

> Note: There are two Weld_I_dflux folders:
> 1) **inputs only** (no output files)  
> 2) **with results** (includes solver output files)

## 📁 Repository Structure
├── Weld_I_dflux_no_results/ # Case inputs: INP, mesh, required data (no outputs)
├── Weld_I_dflux_with_results/ # Same case, but includes results (outputs)
├── 03_FRD_paraview_converter/ # FRD → ParaView converter
├── 04_Calculix/ # Precompiled CalculiX (ccx) 
└── patches/ # Patches

