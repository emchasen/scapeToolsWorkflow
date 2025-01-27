import sys
print(sys.version)

import sys
from osgeo import gdal
import os

class ConvertCRS:
    def __init__(self, base_file_path, src_dir):
        self.base_file = base_file_path
        self.src_dir = src_dir

    def convert_crs(self):
        base_image = gdal.Open(self.base_file)
        for file in os.listdir(self.src_dir):
            if '.tif' in file and not file.startswith('._'):
                image = gdal.Open(os.path.join(self.src_dir, file),gdal.GA_Update)
                image.SetGeoTransform(base_image.GetGeoTransform())  ##sets same geotransform as input
                image.SetProjection(base_image.GetProjection())  ##sets same projection as input
                image.FlushCache()
                image = None
        base_image = None
        print("finished converting")

if __name__ == "__main__":
    base_dir = '/Volumes/One Touch/Grassland/scapeWorkflow/rasterOutputs'
    base_file = os.path.join(base_dir, "southEastWI", "grazeScape", "southEastWI_awc_10m.tif") # file that works
    src_dir = os.path.join(base_dir, "southEastWI", "smartScape") # file of folders that don't work
    # Get the current working directory
    # Print the current working directory
    print(base_file)
    print('this is source directory', src_dir)
    conversion = ConvertCRS(base_file, src_dir)
    conversion.convert_crs()
