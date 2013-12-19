;+
;                Mosaicking the same day's MOD35 low cloud
;                To run the mosaicking procedure correctly,
;            firstly resize input files to same spatial resolution
;
;                       Version: 1.3.7 (2013-12-19)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO MOSAIC_MOD35_LOW_CLOUD
  COMPILE_OPT IDL2
  
  RESOLVE_ROUTINE, 'GEOREF_MOSAIC_SETUP'
  
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ; Customize year and indir
  year = 2003
  indir = 'H:\People\ZhangYawen\C6\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\LowCloud\Resize\'
  CD, indir
  
  ; Set output directory
  outdir = indir + 'Mosaic\'
  IF FILE_TEST(outdir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, outdir
  ; Write unvailable data file to nafile
  nafile = outdir + 'na.txt'
  IF FILE_TEST(nafile) EQ 1 THEN FILE_DELETE, nafile
  
  timespan = TIMEGEN(START = JULDAY(1, 1, year), FINAL = JULDAY(12, 31, year), UNITS = 'Days')
  DOY = LONG(timespan - JULDAY(12, 31, year - 1))
  ndays = N_ELEMENTS(timespan)
  
  ; Coverage option: 0 - day data, 1 - night data
  co = ['0', '1']
  FOR i = 0, ndays - 1 DO BEGIN
    FOR j = 0, N_ELEMENTS(co) - 1 DO BEGIN
      pattern = 'MYD35_L2_A' + STRTRIM(STRING(year), 1) + STRTRIM(STRING(DOY[i], $
        format = '(I03)'), 1) + '_' + co[j] + '*_LC_R.tif'
      flist = FILE_SEARCH(pattern, count = count, /TEST_REGULAR)
      IF count EQ 0 THEN BEGIN
        OPENW, lun, nafile, /GET_LUN, /APPEND
        PRINTF, lun, STRTRIM(STRING(year), 1) + STRTRIM(STRING(DOY[i], format = '(I03)'), 1)
        FREE_LUN, lun
        CONTINUE
      ENDIF
      
      ; Set output file name
      fname = flist[0]
      fbname = FILE_BASENAME(fname, '.tif')
      STRPUT, fbname, '   ', 19
      fbname = STRCOMPRESS(fbname, /REMOVE_ALL)
      out_name = outdir + fbname + 'M.tif'
      PRINT, out_name
      
      ; If only one file, not mosaic and just copy file to output dir
      IF count EQ 1 THEN BEGIN
        ; Copy .tif file
        FILE_COPY, fname, out_name, /OVERWRITE
        ; Copy .hdr file
        FILE_COPY, indir + FILE_BASENAME(fname, '.tif') + '.hdr', $
          outdir + fbname + 'M.hdr', /OVERWRITE
      ENDIF ELSE BEGIN
        fids = LONARR(count)
        dims = FLTARR(5, count)
        see_through_val = BYTARR(count)
        use_see_through = INTARR(count)
        FOR k = 0, count - 1 DO BEGIN
          fname = flist[k]
          ENVI_OPEN_DATA_FILE, fname, r_fid = fid
          IF (fid EQ -1) THEN RETURN
          
          IF k EQ 0 THEN BEGIN
            ENVI_FILE_QUERY, fid, nb = nb, dims = tmp_dims
            dims[*, k] = tmp_dims
            pos = LONARR(nb, count)
            
            map_info = ENVI_GET_MAP_INFO(fid = fid)
            out_ps = map_info.ps[0:1]
          ENDIF
          
          fids[k] = fid
          ENVI_FILE_QUERY, fid, nb = nb, dims = tmp_dims
          dims[*, k] = tmp_dims
          pos[*, k] = LINDGEN(nb)
          see_through_val[k] = 0
          use_see_through[k] = 1
        ENDFOR
        ; Call georef_mosaic_setup to calculate the xsize, ysize, x0, y0, and map_info structure.
        ; Pass the fids of the two input files and output pixel size you want.
        GEOREF_MOSAIC_SETUP, fids = fids, dims = dims, out_ps = out_ps, xsize = xsize, ysize = ysize, $
          x0 = x0, y0 = y0, map_info = map_info
          
        ENVI_DOIT, 'MOSAIC_DOIT', background = 0, fid = fids, dims = dims, georef = 1, map_info = map_info, out_dt = 1, $
          out_name = out_name, pixel_size = out_ps, pos = pos, r_fid = r_fid, see_through_val = see_through_val, $
          use_see_through = use_see_through, xsize = xsize, ysize = ysize, x0 = x0, y0 = y0
          
        FOR k = 0, count - 1 DO ENVI_FILE_MNG, id = fids[k], /REMOVE
        ENVI_FILE_MNG, id = r_fid, /REMOVE
      ENDELSE
    ENDFOR
  ENDFOR
  
  ENVI_BATCH_EXIT
END