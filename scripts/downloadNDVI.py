import geemap
import ee
import geopandas as gpd
import time
# ee.Authenticate(auth_mode='notebook')        
ee.Initialize(project='justgreen-450923')

# read in geopandas object 
aois = gpd.read_file("/home/dune/trueNAS/work/justGreen/data/processed/top200/top200Cities.gpkg")





def calculate_ndvi(image):
    """Calculates NDVI from a Sentinel-2 image."""
    ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI')
    return image.addBands(ndvi)

def process_aoi(aoi):
    """Processes each AOI to calculate maximum NDVI."""
    # Load Sentinel-2 surface reflectance data.
    sentinel2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED') \
        .filterBounds(aoi.geometry()) \
        .filterDate('2020-01-01', '2020-12-31') \
        .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20)) \
        .map(calculate_ndvi)

    # Find the maximum NDVI pixel value over the year and clip to the AOI.
    max_ndvi = sentinel2.select('NDVI').max().clip(aoi.geometry())

    # Add the maximum NDVI image as a property to the AOI feature.
    return aoi.set('maxNDVI', max_ndvi)

# error 3 120,130, 136, 176,200  -- character in name 
for index in range(201, 201):
    # Access row data using row['column_name'] or row.geometry
    row = aois.loc[[index]]  # Replace index_value with the index of the row
    # geoid
    geoid = row.loc[index, 'GEOID']
    # name 
    name = row.loc[index, 'NAME']
    # convert to gee object 
    aoiGEE = geemap.gdf_to_ee(row)
    # generate NDVI image 
    ndvi = process_aoi(aoi=aoiGEE)
    # pull the image 
    max_ndvi_image = ee.Image(ndvi.get('maxNDVI'))
    print(name)
    time.sleep(15)
    # export 
    task1 = ee.batch.Export.image.toDrive(
        image = max_ndvi_image,
        folder= "Earth Engine",
        description =  geoid + "_"+ name+ "_2020NDVI",
        region=aoiGEE.geometry(),
        scale=10,
        maxPixels = 1e13
    )
    task1.start()
