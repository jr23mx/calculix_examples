import ccx2paraview

c = ccx2paraview.Converter("Analysis-1.frd", ["vtu"])
c.run()