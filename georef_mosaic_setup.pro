;+
; Routine to estimate values for some of the required keywords for MOSAIC_DOIT
;                    before doing a georeferenced mosaic
;                    
;                       Version: 1.0.0 (2012-12-30)
;                       
;                 Modified based on Exelis Help Article
;  GEOREF_MOSAIC_SETUP: A routine to estimate output parameters for MOSAIC_DOIT 
;    (http://www.exelisvis.com/Support/HelpArticleDetail/ArticleId/3336.aspx)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;                   Contact Email: tony.tsai@whu.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
; 
; Input Keywords:
; FIDS: Long-integer array of file IDs - one for each file to mosaic.
; 
; DIMS: Keyword to specify the spatial dimensions on which to perform the operation. 
; DIMS is a long-integer array with the following definition:
; DIMS[0, *]: Unused for this function, set to -1L.
; DIMS[1, *]: The starting X pixel (the first pixel is number zero).
; DIMS[2, *]: The ending X pixel.
; DIMS[3, *]: The starting Y pixel (the first pixel is number zero).
; DIMS[4, *]: The ending Y pixel.
; 
; OUT_PS: Keyword to specify a two-element floating point or double precision array containing 
; the X and Y pixel sizes, respectively. This is used to estimate the output size of the mosaic.
; 
; Returned Keywords: Set these keywords equal to these named variables that will contain 
; the calculated parameter values necessary to use mosaic_doit.
; XSIZE: The X size of the output image (sample direction). XSIZE will have the same units as the pixel size.
; 
; YSIZE: The Y size of the output image (line direction). YSIZE will have the same units as the pixel size.
; 
; x0: A long-integer array of X starting pixels, one for each file. x0 will be in pixel coordinates, 
; which are calculated by converting map locations for georeferenced images to output pixel locations.
; 
; y0: A long-integer array of Y starting lines, one for each file. y0 will be in pixel coordinates, 
; which are calculated by converting map locations for georeferenced images to output line locations.
; 
; MAP_INFO: The map projection information of the input files.
;-
PRO GEOREF_MOSAIC_SETUP, fids = fids, dims = dims, out_ps = out_ps, $
    xsize = xsize, ysize = ysize, x0 = x0, y0 = y0, map_info = map_info
  COMPILE_OPT strictarr, hidden
  
  ; Some basic error checking
  IF KEYWORD_SET(dims) THEN $
    IF N_ELEMENTS(fids) NE N_ELEMENTS(dims[0, *]) THEN dims = 0
  ;
  IF N_ELEMENTS(fids) LT 2 THEN BEGIN
    xsize = -1
    ysize = -1
    x0 = -1
    y0 = -1
    RETURN
  ENDIF
  
  ; If no DIMS passed in
  nfiles = N_ELEMENTS(fids)
  IF (KEYWORD_SET(dims) EQ 0) THEN BEGIN
    dims = FLTARR(5, nfiles)
    FOR i = 0, nfiles - 1 DO BEGIN
      ENVI_FILE_QUERY, fids[i], dims = tmp_dims
      dims[*, i] = tmp_dims
    ENDFOR
  ENDIF
  
  ; Compute the size of the output mosaic (xsize and ysize)
  ; Store the map coords of the UL corner of each image since you'll need it later
  UL_corners_X = DBLARR(nfiles)
  UL_corners_Y = DBLARR(nfiles)
  east = -1e34
  west = 1e34
  north = -1e34
  south = 1e34
  FOR i = 0, nfiles - 1 DO BEGIN
    pts = [[dims[1, i], dims[3, i]], $ ; UL
      [dims[2, i], dims[3, i]], $	; UR
      [dims[1, i], dims[4, i]], $	; LL
      [dims[2, i], dims[4, i]]]	; LR
    ENVI_CONVERT_FILE_COORDINATES, fids[i], pts[0, *], pts[1, *], xmap, ymap, /to_map
    UL_corners_X[i] = xmap[0]
    UL_corners_Y[i] = ymap[0]
    east  = east > MAX(xmap)
    west = west < MIN(xmap)
    north = north > MAX(ymap)
    south = south < MIN(ymap)
  ENDFOR
  xsize = east - west
  ysize = north - south
  xsize_pix = ROUND(xsize/out_ps[0])
  ysize_pix = ROUND(ysize/out_ps[1])
  
  ; to make things easy, create a temp image that's got a header
  ; that's the same as the output mosaic image
  proj = ENVI_GET_PROJECTION(fid = fids[0])
  map_info = ENVI_MAP_INFO_CREATE(proj = proj, mc = [0, 0, west, north], ps = out_ps)
  temp = BYTARR(10,10)
  ENVI_ENTER_DATA, temp, map_info = map_info, /no_realize, r_fid = tmp_fid
  
  ; find the x and y offsets for the images
  x0 = LONARR(nfiles)
  y0 = LONARR(nfiles)
  FOR i = 0, nfiles - 1 DO BEGIN
    ENVI_CONVERT_FILE_COORDINATES, tmp_fid, xpix, ypix, UL_corners_X[i], UL_corners_Y[i]
    x0[i] = ROUND(xpix)
    y0[i] = ROUND(ypix)
  ENDFOR
  
  ; delete the tmp file
  ENVI_FILE_MNG, id = tmp_fid, /remove, /no_warning
END