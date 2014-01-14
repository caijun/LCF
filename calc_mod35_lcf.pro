;+
;               Calculating monthly LCF from MOYD35 low cloud
;
;                       Version: 1.4.3 (2014-01-14)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO CALC_MOD35_LCF
  COMPILE_OPT IDL2
  
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ; Customize year and indir
  year = 2003
  pdir = 'I:\TT\People\ZhangYawen\C6\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\LowCloud\'
  indir = pdir + 'Subset\'
  CD, indir
  
  ; Set output directory
  outdir = pdir + 'LCF\'
  IF FILE_TEST(outdir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, outdir
  ; Coverage option: 0 - day data, 1 - night data
  co = ['0', '1']
  
  timespan = TIMEGEN(START = JULDAY(1, 1, year), FINAL = JULDAY(12, 31, year), UNITS = 'Days')
  DOY = LONG(timespan - JULDAY(12, 31, year - 1))
  
  ; Days for each month
  ; If leap year
  IF (year MOD 4) EQ 0 AND (year MOD 100) NE 0 THEN $
    mdays = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] $
  ELSE mdays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  
  FOR t = 0, N_ELEMENTS(co) - 1 DO BEGIN
    ; Current Month
    CM = 0
    ; Availble Days of Every Month
    ADEM = PTRARR(12, /ALLOCATE_HEAP)
    FOR i = 0, N_ELEMENTS(DOY) - 1 DO BEGIN
      CALDAT, timespan[i], month
      IF month NE CM THEN BEGIN
        *ADEM[CM] = []
        CM = month
      ENDIF
      
      fname = indir + 'MYD35_L2_A' + STRTRIM(STRING(year), 1) + STRTRIM(STRING(DOY[i], $
        format = '(I03)'), 1) + '_' + co[t] + '_LC_RMS.tif'
      IF FILE_TEST(fname) EQ 1 THEN *ADEM[CM - 1] = [*ADEM[CM - 1], fname] ELSE *ADEM[CM - 1] = [*ADEM[CM - 1], !NULL]
    ENDFOR
    
    FOR i = 0, N_ELEMENTS(ADEM) - 1 DO BEGIN
      ; Call the layer stacking routine to ensure that multiple images have the same samples and lines
      ; Monthly Available Day Count
      madc = N_ELEMENTS(*ADEM[i])
      IF madc EQ 0 THEN CONTINUE
      
      ; Use the only band from all files and all spatial pixels
      ; First build the array of FID, POS and DIMS for all files
      fid = LONARR(madc)
      pos = LONARR(madc)
      dims = LONARR(5, madc)
      FOR j = 0, madc - 1 DO BEGIN
        fname = (*ADEM[i])[j]
        PRINT, fname
        
        ENVI_OPEN_FILE, fname, r_fid = t_fid
        IF (t_fid EQ -1) THEN RETURN
        
        ENVI_FILE_QUERY, t_fid, dims = t_dims
        fid[j] = t_fid
        pos[j] = 0
        dims[*, j] = t_dims
      ENDFOR
      
      ; Set the output projection and pixel size
      out_proj = ENVI_GET_PROJECTION(fid = fid[0], pixel_size = out_ps)
      ; Set the output data type
      ENVI_FILE_QUERY, fid[0], data_type = out_dt
      ; Save the result to temporal image
      out_name = 'temp.img'
      
      ; Set the exclusive keyword
      ; Use nearest neighbor for the interpolation method
      ENVI_DOIT, 'ENVI_LAYER_STACKING_DOIT', fid = fid, pos = pos, dims = dims, $
        /EXCLUSIVE, out_dt = out_dt, out_name = out_name, interp = 0, out_ps = out_ps, $
        out_proj = out_proj, r_fid = r_fid
        
      ENVI_FILE_QUERY, r_fid, dims = dims, nb = nb, ns = ns,  nl = nl, fname = out_name
      map_info = ENVI_GET_MAP_INFO(fid = r_fid)
      
      lowcloud = MAKE_ARRAY(ns, nl, nb)
      FOR j = 0, nb - 1 DO lowcloud[*, *, j] = ENVI_GET_DATA(fid = r_fid, dims = dims, pos = j)
      
      fids = [fid, r_fid]
      FOR j = 0, N_ELEMENTS(fids) - 1 DO ENVI_FILE_MNG, id = fids[j], /REMOVE
      FILE_DELETE, out_name
      
      dims = SIZE(lowcloud, /DIMENSIONS)
      ; Unique pixel value: 1, 2, 3
      pv = lowcloud[UNIQ(lowcloud, SORT(lowcloud))]
      freq = INTARR(dims[0], dims[1], N_ELEMENTS(pv))
      FOR m = 0, dims[0] - 1 DO BEGIN
        FOR n = 0, dims[1] - 1 DO BEGIN
          FOR k = 0, N_ELEMENTS(pv) - 1 DO BEGIN
            result = WHERE(lowcloud[m, n, *] EQ pv[k], count)
            freq[m, n, k] = count
          ENDFOR
        ENDFOR
      ENDFOR
      ; Pure low cloud frequency: #1 / madc
      lcf1 = DOUBLE(freq[*, *, 0]) / dims[2]
      ; Obscure frequency 1: #2 / madc
      obsf = DOUBLE(freq[*, *, 1]) / dims[2]
      ; No low cloud frequency 2: #3 / madc
      nlcf = DOUBLE(freq[*, *, 2]) / dims[2]
      ; Probable low cloud frequency: (#1 + #2) / madc
      lcf2 = DOUBLE(freq[*, *, 0] + freq[*, *, 1]) / dims[2]
      prodat = [[[lcf1]], [[obsf]], [[nlcf]], [[lcf2]]]
      
      ; Set output file name
      fbname = FILE_BASENAME(fname, '.tif')
      fbname = STRMID(fbname, 0, 14)
      out_name = outdir + fbname + STRTRIM(STRING(i + 1, format = '(I02)'), 1) + '_' + co[t]+ '.tif'
      PRINT, out_name
      bnames = [['Monthly LCF1 (' + STRTRIM(STRING(madc), 1) + '/' + STRTRIM(STRING(mdays[i]), 1) + ')'], $
        ['Monthly OF (' + STRTRIM(STRING(madc), 1) + '/' + STRTRIM(STRING(mdays[i]), 1) + ')'], $
        ['Monthly NLCF (' + STRTRIM(STRING(madc), 1) + '/' + STRTRIM(STRING(mdays[i]), 1) + ')'], $
        ['Monthly LCF2 (' + STRTRIM(STRING(madc), 1) + '/' + STRTRIM(STRING(mdays[i]), 1) + ')']]
      ENVI_WRITE_ENVI_FILE, prodat, out_name = out_name, bnames = bnames, map_info = map_info, r_fid = r_fid
      
      ENVI_FILE_MNG, id = r_fid, /REMOVE
      PTR_FREE, ADEM[i]
    ENDFOR
    PTR_FREE, ADEM
  ENDFOR
  
  ENVI_BATCH_EXIT
END