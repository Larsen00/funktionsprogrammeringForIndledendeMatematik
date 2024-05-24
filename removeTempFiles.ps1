
# Remove temporary files
Get-ChildItem -Recurse -Include *.tmp, *.log, *.cache, *.bbl, *.bbl-SAVE-ERROR, *.bcf-SAVE-ERROR, *.blg, *.fdb_latexmk, *.fls, *.out, *.toc, *.dll | Remove-Item -Force -ErrorAction SilentlyContinue
