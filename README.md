# Sensory processing in humans and mice fluctuates between external and internal modes


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10019948.svg)](https://doi.org/10.5281/zenodo.10019948)

This repository accompanies **Sensory processing in humans and mice fluctuates between external and internal modes** (PLOS Biology 2023), authored by Veith Weilnhammer, Heiner Stuke, Kai Standvoß and Philipp Sterzer.

To reproduce our analyses, download this repository and run the file **modes_mouse_rev2.Rmd**. **modes_mouse_rev2.Rmd** contains the text of our manuscript, the supplement and our response to the reviews at PLOS Biology in markdown format. In addition, it contains the R code to perform the statistical analyses and generate the figures associated with our publication. 

The upmost code chunk **setup** controls what analyses to run, and whether to load preprocessed data from disk. The fastest way reproduce our results is with the setting **load_summary_data = TRUE**. In this condition, **modes_mouse_rev2.Rmd** loads the summary data, which were saved as .Rdata files in the folder **./Summary_Data**, and uses them to generate all statistical results and figures. If you want to recapitulate a specific set of analyses (e.g., re-run the simulation of bimodal inference as shown in Figure 4), you can modify the corresponding statement in **setup** (e.g., **run_simulation = TRUE**) and run **modes_mouse_rev2.Rmd**.

