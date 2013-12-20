;+
;               Calculating monthly LCF from MOYD35 low cloud
;
;                       Version: 1.2.2 (2013-12-20)
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
  pdir = 'H:\People\ZhangYawen\C6\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\LowCloud\'
  indir = pdir + 'Resize\Mosaic\Subset\'
  CD, indir
  
  ; Set output directory
  outdir = pdir + 'LCF\'
  IF FILE_TEST(outdir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, outdir
  ; Output product: PPR, LCF1, LCF2
  prod = ['PPR', 'LCF1', 'LCF2']
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
      ; Calculate the output spatial dims
      ; Monthly available day count
      madc = N_ELEMENTS(*ADEM[i])
      IF madc EQ 0 THEN CONTINUE
      
      nsarr = INTARR(madc)
      nlarr = INTARR(madc)
      FOR j = 0, madc - 1 DO BEGIN
        fname = (*ADEM[i])[j]
        PRINT, fname
        
        ENVI_OPEN_DATA_FILE, fname, r_fid = fid
        ENVI_FILE_QUERY, fid, ns = ns, nl = nl
        nsarr[j] = ns
        nlarr[j] = nl
        
        IF j EQ 0 THEN map_info = ENVI_GET_MAP_INFO(fid = fid)
        
        ENVI_FILE_MNG, id = fid, /REMOVE
      ENDFOR
      ; Minimum ns and nl
      nsmin = MIN(nsarr)
      nlmin = MIN(nlarr)
      lowcloud = MAKE_ARRAY(nsmin, nlmin, madc)
      dims = [-1, 0, nsmin - 1, 0, nlmin - 1]
      
      FOR j = 0, madc - 1 DO BEGIN
        fname = (*ADEM[i])[j]
        
        ENVI_OPEN_DATA_FILE, fname, r_fid = fid
        lowcloud[*, *, j] = ENVI_GET_DATA(fid = fid, dims = dims, pos = 0)
        
        ENVI_FILE_MNG, id = fid, /REMOVE
      ENDFOR
      
      dims = SIZE(lowcloud, /DIMENSIONS)
      ; Unique pixel value
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
      ; Pure pixel ratio: (#1 + #2) / madc
      ppr = DOUBLE((freq[*, *, 1] + freq[*, *, 2])) / dims[2]
      ; Low cloud frequency 1: #2 / (#1 + #2)
      lcf1 = DOUBLE(freq[*, *, 2]) / (freq[*, *, 1] + freq[*, *, 2])
      ; Low cloud frequency 2: #2 / madc
      lcf2 = DOUBLE(freq[*, *, 2]) / dims[2]
      prodat = [[[ppr]], [[lcf1]], [[lcf2]]]
      
      ; Set output file name
      fbname = FILE_BASENAME(fname, '.tif')
      fbname = STRMID(fbname, 0, 14)
      FOR j = 0, N_ELEMENTS(prod) - 1 DO BEGIN
        out_name = outdir + fbname + STRTRIM(STRING(i + 1, format = '(I02)'), 1) + '_' + co[t]+ '_' + prod[j] + '.tif'
        PRINT, out_name
        bnames = ['Monthly ' + prod[j] + ' (' + STRTRIM(STRING(madc), 1) + '/' + STRTRIM(STRING(mdays[i]), 1) + ')']
        ENVI_WRITE_ENVI_FILE, prodat[*, *, j], out_name = out_name, bnames = bnames, map_info = map_info, r_fid = r_fid
        
        ENVI_FILE_MNG, id = r_fid, /REMOVE
      ENDFOR
      PTR_FREE, ADEM[i]
    ENDFOR
    PTR_FREE, ADEM
  ENDFOR
  
  ENVI_BATCH_EXIT
END