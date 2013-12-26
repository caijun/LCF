;+
;            Subsetting MOYD35 low cloud via specific shapefile
;
;                      Version: 1.0.7 (2013-12-26)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO SUBSET_MOD35_LC
  COMPILE_OPT idl2
  
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ; Customize year and pdir
  year = 2003
  pdir = 'I:\TT\People\ZhangYawen\C6\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\LowCloud\'
  indir = pdir + 'Mosaic\'
  CD, indir
  
  ; Set output directory
  outdir = pdir + 'Subset\'
  IF FILE_TEST(outdir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, outdir
  
  pattern = 'MYD35_L2_A' + STRTRIM(STRING(year), 1) + '*_LC_RM.tif'
  flist = FILE_SEARCH(indir, pattern, count = count)
  IF count EQ 0 THEN RETURN
  
  FOR i = 0, count - 1 DO BEGIN
    fname = flist[i]
    ENVI_OPEN_FILE, fname, r_fid = fid
    IF fid EQ -1 THEN RETURN
    
    ENVI_FILE_QUERY, fid, nb = nb, nl = nl, ns = ns
    pos = INDGEN(nb)
    
    ; Open shapefile
    shpfile = 'I:\TT\People\ZhangYawen\Vector\NorthChinaPlain\StudyArea\NCP.shp'
    oshp = OBJ_NEW('IDLffshape', shpfile)
    oshp ->GETPROPERTY, n_entities = n_ent, attribute_info = attr_info, n_attributes = n_attr, $
      entity_type = ent_type
      
    roi_shp = LONARR(n_ent)
    FOR ishp = 0, n_ent - 1 DO BEGIN
      entity = oshp ->GETENTITY(ishp)
      IF entity.shape_type EQ 5 THEN BEGIN
        record = *(entity.vertices)
        ; Convert x, y pixel coordinates to their corresponding map coordinates for given file
        ENVI_CONVERT_FILE_COORDINATES, fid, xmap, ymap, record[0, *], record[1, *]
        ; Create ROI
        roi_shp[ishp] = ENVI_CREATE_ROI(ns = ns, nl = nl)
        ; xmap: column vector, reform(xmap): row vector
        ENVI_DEFINE_ROI, roi_shp[ishp], /POLYGON, xpts = REFORM(xmap), ypts = REFORM(ymap)
        
        ; Bounding box of shape
        IF ishp EQ 0 THEN BEGIN
          xmin = ROUND(MIN(xmap, max = xmax))
          ymin = ROUND(MIN(ymap, max = ymax))
        ENDIF ELSE BEGIN
          xmin = xmin < ROUND(MIN(xmap))
          xmax = xmax > ROUND(MAX(xmap))
          ymin = ymin < ROUND(MIN(ymap))
          ymax = ymax > ROUND(MAX(ymap))
        ENDELSE
      ENDIF
      oshp->DESTROYENTITY, entity
    ENDFOR
    ; Minimum of bounding box of shape and bounding box of image
    xmin = xmin > 0
    xmax = xmax < ns - 1
    ymin = ymin > 0
    ymax = ymax < nl - 1
    
    dims = [-1, xmin, xmax, ymin, ymax]
    ; Set output file name
    fbname = FILE_BASENAME(fname, '.tif')
    out_name = outdir + fbname + 'S.tif'
    PRINT, out_name
    
    ; ENVI_SUBSET_VIA_ROI_DOIT is a hidden function, can't be found in help document
    ENVI_DOIT, 'ENVI_SUBSET_VIA_ROI_DOIT', background = 0, fid = fid, dims = dims, out_name = out_name,$
      ns = ns, nl = nl, pos = pos, roi_ids = roi_shp
      
    ENVI_FILE_MNG, id = fid, /REMOVE
    fids = ENVI_GET_FILE_IDS()
    IF (fids[0] EQ -1) THEN RETURN
    ENVI_FILE_MNG, id = fids[0], /REMOVE
  ENDFOR
  
  ENVI_BATCH_EXIT
END