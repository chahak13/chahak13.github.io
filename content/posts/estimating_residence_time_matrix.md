+++
title = "Estimating residence-time matrix"
author = ["Chahak Mehta"]
tags = ["monte-carlo"]
draft = false
+++

Extending Notes on: Horne, J. S., Garton, E. O., Krone, S. M., & Lewis, J. S. (2007): Analyzing Animal Movements Using  Brownian Bridges, we now want to calculate the residence-time matrix for various polygons (AGEBs). To do this, we will use the `geopandas` library along with the data from Hermisillo that we have for a set of individuals. We will currently work with the data of just one individual.


## AGEB data {#ageb-data}

The data for the AGEBs was provided by INEGI (National Institute of Statistics and Geography). There are a total of 582 AGEBs of which we might discard some that are not of interest. From the data, we particularly care about `CVE_AGEB` and the geometry - which is a polygon.

```python
import geopandas as gpd

data = gpd.read_file("/home/boticelli/Documents/uta/code/residence-time/bbmm-drive/26a.shp")
data.head()
```

We can confirm that there are 582 AGEBs by checking the number of rows in the dataframe `data`.

```python
  len(data)
```

Each `GeoSeries` has a corresponding _CRS_ (Coordinate Reference System) that defines the system used for the projection. If a `GeoDataFrame` has only one column that corresponds to a `GeoSeries`, then the CRS of that series corresponds to the CRS of the dataframe. A dataframe can also have multiple `GeoSeries`, each with their own corresponding CRS. In such a case, one of the series is designated as the active geometry column and is used by default for any geometrical operations.

```python
data.crs
```

Here, the data we have is in the "MEXICO\_ITRF\_2008\_LCC" system. We will now transform the system to a Pseudomercator system for ease of use. This can be done using the `geopandas.GeoDataFrame.to_crs` function. The EPSG code corresponding to Pseudomercator projection is _EPSG:3857_.

```python
data.to_crs("EPSG:3857", inplace=True)
```

We can also visualize this data using plotting functionality in geopandas and matplotlib.

```python
import matplotlib.pyplot as plt

data.plot(figsize=(5,5))
plt.savefig('/home/boticelli/Documents/uta/code/residence-time/agebs.png')
```
