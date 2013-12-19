;+
;         Georeferencing MOYD35 cloud mask using MOYD03 geolocation
;                       when MRTSwath doesn't work
;
;                       Version: 1.1.0 (2013-12-18)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO GEOREF_MOD35_USING_MOD03
  COMPILE_OPT IDL2
  
  RESOLVE_ROUTINE, 'GETBOUNDINGRECT', /IS_FUNCTION
  
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ; Customize year and pdir
  year = 2003
  pdir = 'H:\People\ZhangYawen\C6\'
  MOYD35Dir = pdir + 'MRTSwathFail\MYD35\' + STRTRIM(STRING(year), 1) + '\'
  MOYD03Dir = pdir + 'MRTSwathFail\MYD03\' + STRTRIM(STRING(year), 1) + '\'
  MOYD35GeoRefDir = pdir + 'MRTSwathFail\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\'
  IF FILE_TEST(MOYD35GeoRefDir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, MOYD35GeoRefDir
  oMOYD35GeoRefDir = STRJOIN(STRSPLIT(MOYD35GeoRefDir, 'MRTSwathFail\\', /REGEX, /EXTRACT, /PRESERVE_NULL), '')
  
  ; Record hdfs with fill value
  fvhdf = pdir + 'MRTSwathFail\fv.txt'
  IF FILE_TEST(fvhdf) EQ 1 THEN FILE_DELETE, fvhdf
  
  MOYD35list = FILE_SEARCH(MOYD35Dir, '*.hdf', count = MOYD35_count, /FULLY_QUALIFY_PATH)
  IF MOYD35_count EQ 0 THEN RETURN
  
  FOR i = 0, MOYD35_count - 1 DO BEGIN
    MOYD35file = MOYD35list[i]
    PRINT, MOYD35file
    
    ; Read in Cloud Mask image
    ENVI_OPEN_FILE, MOYD35file, r_fid = MOYD35_fid
    ENVI_FILE_QUERY, MOYD35_fid, nb = nb
    
    fbname = FILE_BASENAME(MOYD35file)
    str = STRSPLIT(fbname, '.', /EXTRACT)
    pattern = '*.' + str[1] + '.' + str[2] + '*.hdf'
    MOYD03list = FILE_SEARCH(MOYD03Dir, pattern, count = MOYD03_count, /FULLY_QUALIFY_PATH)
    IF MOYD03_count NE 1 THEN CONTINUE
    
    MOYD03file = MOYD03list[0]
    ; Get Longitude and Latitude from MOYD03
    SDinterface_id = HDF_SD_START(MOYD03file, /READ)
    ; Longitude dataset
    index = HDF_SD_NAMETOINDEX(SDinterface_id, 'Longitude')
    SDdataset_id = HDF_SD_SELECT(SDinterface_id, index)
    HDF_SD_GETDATA, SDdataset_id, Longitude
    HDF_SD_ENDACCESS, SDdataset_id
    ; Latitude dataset
    index = HDF_SD_NAMETOINDEX(SDinterface_id, 'Latitude')
    SDdataset_id = HDF_SD_SELECT(SDinterface_id, index)
    HDF_SD_GETDATA, SDdataset_id, Latitude
    HDF_SD_ENDACCESS, SDdataset_id
    HDF_SD_END, SDinterface_id
    
    ; If Longitude or Latitude exists Fill Value (-999.0) then return
    xy = [[[Longitude]], [[Latitude]]]
    index = WHERE(xy EQ -999.0, count)
    IF count NE 0 THEN BEGIN
      ; Write the HDF file with fill value to fv.txt
      OPENW, lun, fvhdf, /GET_LUN, /APPEND
      PRINTF, lun, FILE_BASENAME(MOYD35file)
      PRINTF, lun, FILE_BASENAME(MOYD03file)
      FREE_LUN, lun
      CONTINUE
    ENDIF
    
    ; Write xy to temporal ENVI image
    out_name = pdir + 'temp.tif'
    bnames = ['Longitude', 'Latitude']
    ENVI_WRITE_ENVI_FILE, xy, out_name = out_name, bnames = bnames, r_fid = r_fid
    IF r_fid EQ -1 THEN RETURN
    
    ; Build GLT
    x_fid = r_fid
    y_fid = r_fid
    ; band1 - Longitude
    ; band2 - Latitude
    x_pos = 0
    y_pos = 1
    rotation = 0.0
    i_proj = ENVI_PROJ_CREATE(/geographic)
    o_proj = ENVI_PROJ_CREATE(/geographic)
    out_name = pdir + 'temp.glt'
    
    ; Pixel size using default value
    ENVI_GLT_DOIT, i_proj = i_proj, o_proj = o_proj, out_name = out_name, $
      r_fid = glt_fid, rotation = rotation, x_fid = x_fid, y_fid = y_fid, x_pos = x_pos, y_pos = y_pos
    IF glt_fid EQ -1 THEN RETURN
    
    ; Georeference image from GLT
    out_name = pdir + 'temp_georef.tif'
    pos = INDGEN(nb)
    ENVI_DOIT, 'ENVI_GEOREF_FROM_GLT_DOIT', fid = MOYD35_fid, glt_fid = glt_fid, out_name = out_name, pos = pos, r_fid = georef_fid
    IF georef_fid EQ -1 THEN RETURN
    
    ; Resample image
    ENVI_FILE_QUERY, georef_fid, dims = dims
    out_name = pdir + 'temp_resample.tif'
    map_info = ENVI_GET_MAP_INFO(fid = georef_fid)
    ps = map_info.ps[0:1]
    BoundingRect = GETBOUNDINGRECT(MOYD35file, 'LAT_LONG')
    lat = DOUBLE(BoundingRect[2:3])
    ; Earth radius in KM
    R = 6371007.181/1000
    ; Parellel circle radius in KM
    r = R*COS(ABS(MEAN(lat)/180*!PI))
    ; Degrees per KM
    out_ps = [360/(2*!PI*r), 360/(2*!PI*r)]
    ; Caculate the rebin factors for x and y
    IF (ps[0] NE out_ps[0]) OR (ps[1] NE out_ps[1]) THEN rfact = out_ps/ps ELSE rfact = [1, 1]
    
    ENVI_DOIT, 'RESIZE_DOIT', fid = georef_fid, dims = dims, interp = 0, out_name = out_name, pos = pos, $
      r_fid = resample_fid, rfact = rfact
      
    ENVI_FILE_QUERY, resample_fid, dims = dims
    map_info = ENVI_GET_MAP_INFO(fid = resample_fid)
    ; Create geokey information for writing GeoTIFF
    g_tags = {MODELPIXELSCALETAG: [map_info.ps[0:1], 0D], $
      MODELTIEPOINTTAG: [0D, map_info.mc, 0D], $
      GTMODELTYPEGEOKEY: 2, $
      GTRASTERTYPEGEOKEY: 2, $
      GTCITATIONGEOKEY: 'Geographic (Longitude, Latitude) WGS 1984', $
      GEOGRAPHICTYPEGEOKEY: 4326, $
      GEOGGEODETICDATUMGEOKEY: 6325, $
      GEOGANGULARUNITSGEOKEY: 9102$
    }
    
    FOR j = 0, nb - 1 DO BEGIN
      out_name = MOYD35GeoRefDir + str[0] + '_' + str[1] + '_' + str[2] + '_Cloud_Mask_b' + STRTRIM(STRING(j), 1) + '.tif'
      PRINT, out_name
      cloudmask = ENVI_GET_DATA(fid = resample_fid, dims = dims, pos = j)
      ; Description is an identifier for different georeferencing methods
      WRITE_TIFF, out_name, cloudmask, geotiff = g_tags, description = 'Georeferenced by IDL procedure'
      ; Copy georeferenced file to oMOYD35GeoRefDir
      FILE_COPY, out_name, oMOYD35GeoRefDir, /OVERWRITE
    ENDFOR
    
    ; Remove files from ENVI_FILE_MNG
    fids = ENVI_GET_FILE_IDS()
    IF (fids[0] EQ -1) THEN RETURN
    FOR k = 0, N_ELEMENTS(fids) - 1 DO ENVI_FILE_MNG, id = fids[k], /REMOVE
  ENDFOR
  ; Delete temporary files
  templist = FILE_SEARCH(pdir, 'temp*', count = count, /FULLY_QUALIFY_PATH)
  IF count NE 0 THEN FILE_DELETE, templist
  
  ENVI_BATCH_EXIT, /NO_CONFIRM
END