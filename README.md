# shipapp

**Front-end**

For this app I used shiny, shiny.semantic, leaflet and tidyverse.

*shiny.semantic* is very simple, clean and good to use.

I stated to buil a modularizing app, but due to a schedule I did not implemented it.

The app are clean and simple to show functionality, but it is possible with a efort in CSS (or SASS) transform it to a stunning looking app.


**Back-end**

I used a algorithm from geosphere package *distHaversine* to calculate the distance between two points.
I wraped the dataset in a map_df loop function calculating by ship_id. It took 2 hours and 49 minuts to run.
Would be better if I had used a foreach and parallelization appoach to shortend the runtime.

Other appoach possible would be filter the dataset in the application and then perform a calculation only for the ship selected, and return the result to the leaflet map.
I could also use a JuliaCall and use a julia function to speed up this calculations.
