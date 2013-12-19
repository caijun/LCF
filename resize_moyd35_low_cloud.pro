;+
;       Resizing all MOYD35 low cloud bands to same spatial resolution
;
;                       Version: 1.1.5 (2013-10-24)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO RESIZE_MOYD35_LOW_CLOUD
  COMPILE_OPT IDL2
  
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ; Customize year and indir
  year = 2003
  indir = 'H:\People\ZhangYawen\C6\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\LowCloud\'
  CD, indir
  
  ; Set output directory
  outdir = indir + 'Resize\'
  IF FILE_TEST(outdir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, outdir
  
  ;  Resizing all MOD35 loud cloud bands to same spatial resolution
  pattern = 'MYD35_L2_A' + STRTRIM(STRING(year), 1) + '*_LowCloud.tif'
  flist = FILE_SEARCH(pattern, count = count, /FULLY_QUALIFY_PATH)
  IF count EQ 0 THEN RETURN
  
  ; Calculate the output pixel size, default method 2
  method = 2
  CASE method OF
    1: BEGIN
      ; Method 1: using minium pixel size of all MOYD low cloud bands
      psarr = DBLARR(2, count)
      FOR i = 0, count - 1 DO BEGIN
        fname = flist[i]
        ENVI_OPEN_DATA_FILE, fname, r_fid = fid
        IF (fid EQ -1) THEN RETURN
        
        map_info = ENVI_GET_MAP_INFO(fid = fid)
        psarr[*, i] = map_info.ps[0:1]
        
        ENVI_FILE_MNG, id = fid, /REMOVE
      ENDFOR
      ; Minium pixel size
      IF count EQ 1 THEN out_ps = psarr ELSE out_ps = MIN(psarr, DIMENSION = 2)
    END
    2: BEGIN
      ; Method 2: using latitude of study area centroid to calculate pixel size
      ; Bounding rectangle of study area
      BoundingRect = ['118.5', '114.5', '36', '40']
      lat = DOUBLE(BoundingRect[2:3])
      ; Earth radius in KM
      R = 6371007.181/1000
      ; Parellel circle radius in KM
      r = R*COS(ABS(MEAN(lat)/180*!PI))
      ; Degrees per KM
      out_ps = [360/(2*!PI*r), 360/(2*!PI*r)]
    END
  ENDCASE
  
  FOR i = 0, count - 1 DO BEGIN
    fname = flist[i]
    ENVI_OPEN_DATA_FILE, fname, r_fid = fid
    IF (fid EQ -1) THEN RETURN
    
    ENVI_FILE_QUERY, fid, dims = dims, nb = nb
    pos = LINDGEN(nb)
    
    map_info = ENVI_GET_MAP_INFO(fid = fid)
    ps = map_info.ps[0:1]
    ; Caculate the rebin factors for x and y
    IF (ps[0] NE out_ps[0]) OR (ps[1] NE out_ps[1]) THEN rfact = out_ps/ps ELSE rfact = [1, 1]
    
    fbname = FILE_BASENAME(fname, '.tif')
    out_name = outdir + fbname + '_Resize.tif'
    PRINT, out_name
    ENVI_DOIT, 'RESIZE_DOIT', fid = fid, pos = pos, dims = dims, interp = 0, rfact = rfact, $
      out_name = out_name, r_fid = r_fid
      
    ENVI_FILE_MNG, id = fid, /REMOVE
    ENVI_FILE_MNG, id = r_fid, /REMOVE
  ENDFOR
  
  ENVI_BATCH_EXIT
END