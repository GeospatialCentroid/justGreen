import geemap
import ee
import geopandas as gpd
import numpy
import time
import re
# ee.Authenticate(auth_mode='notebook')       
ee.Initialize(project='justgreen-450923')

# read in geopandas object 
aois = gpd.read_file("data/processed/top200/top200Cities.gpkg")


# features to rerun 
errors = [ 33,  56,  58,  59,  63,  64,  70,  72,  78,  79,  80,  81,  82,  86,  87,  88,  89,  93,  94,  95,  96,  99, 102,
103, 104, 105, 106, 108, 110, 111, 113, 115, 117, 118, 120, 123, 124, 125, 127, 128, 129, 132, 133, 134, 137, 138,
140, 143, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 166, 167, 168, 169, 171, 174, 175, 176, 177, 179, 180,
181, 182, 184, 186, 187, 189, 190, 191, 192, 193, 199, 200]
reduced_vector = [x - 1 for x in errors]
# filter aoi
selected_gdf = aois.iloc[reduced_vector]

def calculate_ndvi(image):
    """Calculates NDVI from a Sentinel-2 image."""
    ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI')
    return image.addBands(ndvi)

def process_aoi(aoi):
    """Processes each AOI to calculate maximum NDVI."""
    #buffer object 
    bufferedAOI = aoi.geometry().buffer(10000)


    # Load Sentinel-2 surface reflectance data.
    sentinel2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED') \
        .filterBounds(aoi) \
        .filterDate('2022-01-01', '2024-12-31') \
        .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20)) \
        .map(calculate_ndvi)

    # Find the maximum NDVI pixel value over the year and clip to the AOI.
    max_ndvi = sentinel2.select('NDVI').max().clip(bufferedAOI)

    # Add the maximum NDVI image as a property to the AOI feature.
    return aoi.set('maxNDVI', max_ndvi)

def remove_special_characters(text):
  """Removes all special characters from a string."""
  # Define a regular expression pattern to match special characters.
  # This pattern keeps alphanumeric characters (a-z, A-Z, 0-9) and spaces.
  pattern = r'[^a-zA-Z0-9\s]'
  cleaned_text = re.sub(pattern, '', text)
  return cleaned_text


### full feature run 
# error 3 120,130, 136, 176,200  -- character in name 
renamed = [120,136,176]
# for index in range(199,200):
for index in reduced_vector:
    print(index)
    # Access row data using row['column_name'] or row.geometry
    row = aois.loc[[index]]  # Replace index_value with the index of the row
    # geoid
    geoid = row.loc[index, 'GEOID']
    # name 
    name = remove_special_characters(row.loc[index, 'NAME'])
    print(name)
    # convert to gee object 
    aoiGEE = geemap.gdf_to_ee(row)
    # buffer 
    bufferedAOI = aoiGEE.geometry().buffer(10000)
    # generate NDVI image 
    ndvi = process_aoi(aoi=aoiGEE)
    # pull the image 
    max_ndvi_image = ee.Image(ndvi.get('maxNDVI'))
    # print(name)
    time.sleep(5)
    # export 
    ### something odd going on with the export process.. files are not writen to folder but are listed as duplicated folders of the same name ... 
    task1 = ee.batch.Export.image.toDrive(
        image = max_ndvi_image,
        # folder= "justGreenImages/",
        description = geoid + "_"+ name+ "_2023NDVI_buffered10k_2",
        region=bufferedAOI,
        scale=10,
        maxPixels = 1e13
    )
    task1.start()

