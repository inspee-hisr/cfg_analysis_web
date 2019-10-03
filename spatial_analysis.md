---
title: Spatial Analysis
aside: false
---

In this analysis we used spatial data (e.g shapefiles) to enrich the cave fauna of Greece. We incorporated the shapefiles as detailed below:

* Greece administrative data [geodata.gov.gr](http://geodata.gov.gr/en/dataset?tags=administrative+units)
* [Natura 2000 areas](https://ec.europa.eu/environment/nature/natura2000/access_data/index_en.htm)
* [Wildlife refugees](https://geodata.gov.gr/dataset/kataphugia-agrias-zoes-e-per-gr)

We used these packages for the analysis in R:

* [sp](https://cran.r-project.org/web/packages/sp/index.html)
* [rgdal](https://www.rdocumentation.org/packages/rgdal/versions/1.4-4)
* [maptools](https://www.rdocumentation.org/packages/maptools/versions/0.9-5)

## Administrative data of Greece

![Caves and species abudance in each administravive region of Greece](/assets/Website_plots/caves_species_region.png)

![Regions with caves coordinates in Greece.](/assets/Plots/caves_in_region_no_text_color.png)

![Cave fauna distribution in Greece.](/assets/Plots/species_spatial_dist_per_region_no_text.png)

![Species sampled per municipality in Greece](/assets/Plots/species_spatial_dist_per_municipality_no_text.png)


### Species and caves per region


![Species abundance in Greece](/assets/Plots/map_greece_plot_lines_grid_species.png)

![Endemic species abundance in Greece](/assets/Plots/map_greece_plot_lines_grid_endemic_species.png)

![Species abundance in Greece in respect to their classification](/assets/Plots/map_greece_plot_lines_grid_species_classification.png)

### Conservation

![Summary of cave protections in Greece](/assets/Website_plots/caves_protection_data_type.png)


![Caves and protected areas in Greece (only borders)](/assets/Plots/map_greece_plot_lines.png)


### Altitude


![Most samplings for caves are from 0-400 meter altitude](/assets/Plots/species_per_altitude_classification.jpeg)


