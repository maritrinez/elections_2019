# LOCAL ELECTIONS DOT DENSITY MAP


## Analysis

#### `results2csv.R`
  Reads the elections results (`raw_data/results/municipales2019`) provided by the gorverment. 
  Writes the output file `data/muni2019_results.csv`.
  It also contains a function to read the European Elections results, but it isn't use as the european results are not shown on the dot map.

#### `parties2019.R`
  First step to harmonize the party names.
  Reads the parties with at least one file in the 'muni2019_results.csv' and make join it with the parties from the 2015 elections harmonized by Populate.
  Writes the output file as `data/muni2019_parties.csv`. 

#### `parties2019` [g sheets](https://docs.google.com/spreadsheets/d/15NQVBHTwwptCARTFpL6SiyYclrUZEMS5F_BOwD_K3jU/edit#gid=943723096)

  The parties names were fixed manually in a spreadsheet. Colors also were assigned manually.

  The output file including parties harmonized, seats and colors was saved as `data/muni2019_parties_colors.csv`.
  The unique color palette is saved as `parties_color_palette.csv`

  The files inside `candidaturas` folder were used to fix some parties names. [Raw files found here](https://www.elconfidencial.com/elecciones-municipales-y-autonomicas/2019-05-26/candidaturas-listas-eleciones-municipales-2019_1991950/) 

#### `generate_dots.R`
  Generate coordinates for each party seat, inside the correspondent municipality geography. 
  Assign each pair of coordinates o a party, according to the number of seats that each party got in the elections. 
  As the dots generation takes a while (around one hour and a half), the resultant dataframe is saved as `muni_dots.csv`.
  Add seats info to each municipality polygon. 
  Save both objects, dots and polygons as geojson, so the files can be converted to mbtiles with [tippecanoe](https://docs.mapbox.com/help/troubleshooting/large-data-tippecanoe/#about-tippecanoe).
  The files are `/development/data/municipalities.json` `/development/data/seats.json`

#### Tippecanoe
Convert the geojson to `vector mbtiles` and make the mapbox load faster.


## Development

