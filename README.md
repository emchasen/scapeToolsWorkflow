# scapeToolsWorkflow
all of the scripts needed to create a new region in grazescape and smartscape

1. Download SSURGO data 
1.1. scripts/soilScripts/downloadSoil.R 

2. clean soil data to make csvs and shapefiles
2.1 scripts/soilScripts/cleanSoil.R 
2.2 scripts/soilScripts/makeSoilShapes.R # note that the nresponse data property is added after the original shape is created from SSURGO data but is included at the end of the file. You can find documentation regarding the number of soils that do not have nResponses. this makes two soil shapes for each county. 

3. to create rasters 
3.1. Create rasters from shape file - (use remote computing/condor)* see notes for using remote computing/condor
3.1.1. see scripts in scripts/soil scripts/rasterCreationScripts/ for each layer
3.2. To create the DEM, slope and LS rasters (on local machine)
3.2.1. first use the DEM shapefile from DNR to make learning hub shapefiles
3.2.2. use scripts/soilScripts/rasterCreationScripts/cutDEM.R to create a shapefile of learning hubs
3.2.3. follow scripts/soilScripts/rasterCreationScripts/DEM_slope_LS_rasterCreation.R 
3.3. for corn and soy yield rasters
3.3.1. use script in scripts/soil scripts/rasterCreationScripts/yieldRasters.R
3.4. for Wiscland layer (only used in smartscape)
3.4.1. load the WL_reclass_3857.tif into qgis or arcgis and extract learning hub region
3.4.2. follow scripts/soil scripts/wiscLandCreation.R
3.5. make the streams, open water shapefiles and distance to water raster
3.5.1. create a shapefile of streams for each learning hub region which includes the counties in the learning hub and all surrounding counties (scripts/water body scripts/makeWaterShapefile.R)
3.5.2. use the new learninghub shapefile to clip the streamlines and openWater shapefiles in qgis
3.5.3. use scripts/water body scripts/streamDistance.R on the remote computing service*
3.5.4. use scripts/water body scripts/openWaterDistance.R on the remote computing service*
3.5.5. use scripts/water body scripts/combineStreamWater.R to make the distance to waterwaters raster (on local machine)

4. To make the models:
4.1. join the snapplus simulation batch output files with the csvs created in cleanSoil.R (one file per county that is run in the simulation)
4.1.1 joinSnapSSurgo.R. the script has examples from previous soils but each one requires some hand checking - not all soils in the snapplus file have a match in the ssurgo file. This may be due to the way soils are filtered in the cleanSoil.R file, some soils from ssurgo have missing data, or some other reason TBD. We decided to let these soils be instead of going down the rabbit hole. You can find notes for each county that says how many soils don't have a match within the R script each time the which_soils object is created. 
4.2. model creation
4.2.1 in the scripts/model scripts folder, find one file per crop_model outcome (cc_erosion.R, cc_PI.R, cg_erosion, etc.). The only thing you should need to change in the script is the soils loaded and the name of the output files.
4.2.2. the PI models are a two step process. 
4.2.2.1. First, the model scripts (cc_PI.R, cg_PI.R, etc) are run on the remote computing service. Then, the model output from those files is used to create a tidymodel version of the model. 
4.2.2.2. Use the scripts/model scripts/check_PI_out.R to create the final tidymodel version. After running anova on the output of the model created *crop_PI.R script, copy and paste the final model into the fit of the tidyMod, because this (tends to) make the final model file to be smaller and easier to use in the tools. 

5. make the smartscape model rasters
5.1. to reduce computation time of each set of rasters, create one soil data frame of the entire region with scripts/modelRasterScripts/makeSoilDF.R. The csv created in this script is loaded into each of the model raster scripts.
5.2. use the scripts in scripts/modelRasterScripts to create the smartScape model rasters. These all take place on the remote computing machine. You need to check that the names of the rasters are all correct that feed into the dataframe. The names of the soil dataframe need to match the names of the variables in the model. Find the additional csvs needed in the csv folder.
5.1.1. in the above scripts, edit the file names along with the initialP value (found around line 60 in each script). To determine the initialP value for each region, use the scripts/modelRasterScripts/soilP.R

6. check that rasters are read into geoserver, and convert CRS as necessary
6.1. starting and opening geoserver
6.1.1. if you don't already have geoserver installed, download at https://geoserver.org/download/
6.1.2. start geoserver on your local machine by navigating in the terminal to geoserver_version/bin
6.1.3. sh startup.sh
6.1.4. open geoserver and log in (http://localhost:8080/geoserver/web/?0) user: admin; password: geoserver
6.2. confirming if a raster can be used in geoserver
6.2.1. click "Import Data", choose the file or directory, click "next"
6.2.2. if nothing loads, or you can an error here, it means the raster does not have the correct metadata
6.2.3. if it loads, import it and view the layer preview (on make, I had to change the format from JPEG to TIFF by clicking on the 3 dots on the top left to open the options)
6.3. convert CRS for all rasters within a folder
6.3.1. create an environment (ask Matthew for help)
6.3.2. put the file scripts/newConvertCRS.py in the folder with the environment
6.3.3. edit the newConvertCRS.py so that there is a working TIFF for "base_file" and "src_dir" is the file of folders with rasters to be converted.
6.3.3.1. for each region, import one grazescape and one smartscape raster layer into qgis and export them. this creates a working tiff file for each region and resolution. Be sure to use the correct region and resolution as the base_file in the newConvertCRS.py file.
6.4. running the newConvertCRS.py
6.4.1. use terminal to navigate to the folder with the environment and newConvertCRS.py
6.4.2. type "conda activate 'environmentName'"
6.4.3. type "python newConvertCRS.py"
6.4.4. double check that the rasters are not able to be read in geoserver


* notes for using the remote computing services. 
1. you need to be logged onto a wisc VPN
2. You will need IT help (I used WEI help desk) to create an environment on the remote server to complete your jobs.
3. you need some sort of file transferring program (FTP). I used filezilla. 
4. After logging onto the remote server in your FTP, create a folder for each job on the remote server.
5. Each job needs a submitFile.txt and a bashScript.sh. See examples in the scripts folder.
6. When all files for a particular job are located in their folder, log into the remote server on your terminal (ssh)
7. The following commands are used in the terminal to get the jobs done:
cd = change directory, chmod 775 bashScript.sh, chmod 775 scriptName.R, condor_submit submitFile.txt, condor_q = see what jobs are running, condor_rm jobNumber = remove a job, less scriptName = open a script in your terminal to see it

