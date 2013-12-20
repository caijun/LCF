;+
;               Detecting low cloud from MOYD35 cloud mask
;
;                       Version: 1.0.1 (2013-12-9)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO DETECT_LC_FROM_MOD35
  COMPILE_OPT IDL2
  RESOLVE_ROUTINE, 'DETECTLOWCLOUD'
  
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ; Customize year and indir
  year = 2003
  indir = 'H:\People\ZhangYawen\C6\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\'
  
  ; Set output directory
  outdir = indir + 'LowCloud\'
  IF FILE_TEST(outdir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, outdir
  
  pattern = 'MYD35_L2_A*Cloud_Mask_b[0-5].tif'
  flist = FILE_SEARCH(indir, pattern, count = count)
  IF count EQ 0 THEN RETURN
  
  ; Read b0-5 very time
  FOR i = 0, count - 1, 6 DO BEGIN
    FOR j = 0, 6 - 1 DO BEGIN
      fname = flist[i + j]
      
      ENVI_OPEN_DATA_FILE, fname, r_fid = fid
      IF fid EQ -1 THEN RETURN
      
      ENVI_FILE_QUERY, fid, ns = ns, nl = nl, dims = dims
      IF j EQ 0 THEN begin
        cloudmask = MAKE_ARRAY(ns, nl, 6, /BYTE)
        map_info = ENVI_GET_MAP_INFO(fid = fid)
      ENDIF
      
      cloudmask[*, *, j] = ENVI_GET_DATA(fid = fid, dims = dims, pos = 0)
      
      ENVI_FILE_MNG, id = fid, /REMOVE
    ENDFOR
    
    ; Detecting low cloud
    str = STRSPLIT(fname, '_', /EXTRACT)
    IF FIX(str[3]) LT 1000 THEN BEGIN
      DETECTLOWCLOUD, cloudmask, lowcloud, /DAY
      bnames = ['Low Cloud (day)']
    ENDIF ELSE BEGIN
      DETECTLOWCLOUD, cloudmask, lowcloud
      bnames = ['Low Cloud (night)']
    ENDELSE
    
    fbname = FILE_BASENAME(fname, 'Cloud_Mask_b5.tif')
    out_name = outdir + fbname + 'LC.tif'
    PRINT, out_name
    ENVI_WRITE_ENVI_FILE, lowcloud, out_name = out_name, bnames = bnames, map_info = map_info, r_fid = fid
    
    ENVI_FILE_MNG, id = fid, /REMOVE
  ENDFOR
  
  ENVI_BATCH_EXIT
END