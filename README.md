LCF
===

IDL procedures for calculating Low Cloud Frequency (LCF) from the MODIS Cloud Product

## Instructions
#### Data preparing
1. Run *check_hdf.pro* to make sure that all downloaded MOD/MYD35 Cloud Mask and MOD/MYD03 Geolocation files are correct. If there exist any broken HDF files, the procedure will write those file names to a log file named *[dir].redownload.txt*. Redownload those files and rerun the procedure until no entry in the log file.
2. Run *gen_prm_bat_file.pro* to generate [MRTSwath](https://lpdaac.usgs.gov/tools/modis_reprojection_tool_swath) parameter files for each MOD/MYD35 file and a batch file named *mrtswath.bat*. Before running *mrtswath.bat* make sure you have installed MRTSwath and added the *bin* directory of MRTSwath to PATH environment variable on your computer.
3. Run *check_mrtswath_georef.pro* to check whether there are any MOD/MYD35 files that MRTSwath failed to georeferece maybe due to Fill Value. The procedure will generate a log file named *MRTSwathFail.txt* and copy all unsuccesfully georeferenced MOD/MYD35 and MOD/MYD03 files to *MRTSwathFail* directory for step 4 processing. If MRTSwath successfully georeferenced all files, skip step 4.
4. Run *georef_mod35_using_mod03.pro* to geoference those unsuccesfully georeferenced files in step 3. Pay attention to that the results of this procedure are a bit different from MRTSwath in pixel size. If anyone knows MRTSwath how to calculate output pixel size, please tell me.

#### Data preprocessing
1. Run *detect_lc_from_mod35.pro* to detect low cloud from MOD/MYD35 Cloud Mask. The cloud classification scheme is: 0 - background, 1 - pure low cloud, 2 - obscure, 3 - no low cloud and 4 - probable low cloud (1 or 2).
2. Run *resize_mod35_lc.pro* to resample all low cloud image to same pixel size. It's an essential step before gettting the following step 3 correctly excution.
3. Run *mosaic_mod35_lc.pro* to combine multiple images into a single seamless composite image that completely covers the study area for day (0) or night (1).
4. Run *subset_mod35_lc.pro* to subset the mosaicking image via the shapefile of study area.

#### LCF calculation
1. Run *calc_mod35_lcf.pro* to calculate *pure low cloud frequency*, *obscure frequency*, *no low cloud frequency* and *probable low cloud frequency*. The corresponding band names of resulting images are *Monthly LCF1*, *Monthly OF*, *Monthly NLCF* and *Monthly LCF2*.

#### LCF extraction
1. Run *get_mod35_lcf_via_evf.pro* to extract the LCF of ROI storing in an ENVI evf file to txt file for further analysis in [R](http://www.r-project.org/).
