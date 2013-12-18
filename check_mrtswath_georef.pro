;+
;    Check files that MRTSwath failed to georeference due to unknown error
;
;  Note: those files can be georeferenced by GEOREF_MOYD35_USING_MOYD03.pro
;        in following data processing, which needs to pay attention to the
;        pixel size georeferenced by this procedure is a bit different from
;        that of MRTSwath.
;
;                       Version: 1.4.0 (2013-12-18)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO CHECK_MRTSWATH_GEOREF
  COMPILE_OPT IDL2
  
  ; TODO: Incremental check
  
  ; Customize year and pdir
  year = 2003
  pdir = 'H:\People\ZhangYawen\C6\'
  MOYD35Dir = pdir + 'MYD35\' + STRTRIM(STRING(year), 1) + '\'
  MOYD03Dir = pdir + 'MYD03\' + STRTRIM(STRING(year), 1) + '\'
  MOYD35GeoRefDir = pdir + 'MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\'
  
  ; Set output directory
  oMOYD35Dir = STRJOIN(STRSPLIT(MOYD35Dir, 'MYD35', /REGEX, /EXTRACT, /PRESERVE_NULL), 'MRTSwathFail\MYD35')
  IF FILE_TEST(oMOYD35Dir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, oMOYD35Dir
  oMOYD03Dir = STRJOIN(STRSPLIT(MOYD03Dir, 'MYD03', /REGEX, /EXTRACT, /PRESERVE_NULL), 'MRTSwathFail\MYD03')
  IF FILE_TEST(oMOYD03Dir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, oMOYD03Dir
  
  ; Record HDF files that MRTSwath failed to georeference
  MRTSfile = pdir + 'MRTSwathFail\MRTSwathFail.txt'
  ; First check
  IF FILE_TEST(MRTSfile) EQ 0 THEN BEGIN
    MOYD35list = FILE_SEARCH(MOYD35Dir, '*.hdf', count = MOYD35_count, /FULLY_QUALIFY_PATH)
    IF MOYD35_count EQ 0 THEN RETURN
  ENDIF ELSE BEGIN
    ; Read previous checking results and only check those files to save time
    results = QUERY_ASCII(MRTSfile, info)
    MOYD35_count = info.LINES - 1
    IF MOYD35_count EQ 0 THEN BEGIN
      RETURN
    ENDIF ELSE BEGIN
      header = ''
      ; Records of HDF files that MRTSwath failed to gereferenced
      records = !NULL
      records = STRARR(MOYD35_count)
      OPENR, lun, MRTSfile, /GET_LUN
      ; Skip header
      READF, lun, header
      READF, lun, records
      FREE_LUN, lun
      ; Split records to rows*cols
      ; Note: records is a LIST
      records = STRSPLIT(records, ', ', /EXTRACT)
      MOYD35list = STRARR(MOYD35_count)
      FOR i = 0, MOYD35_count - 1 DO MOYD35list[i] = MOYD35Dir + (records[i])[0]
      
      FILE_DELETE, MRTSfile
    ENDELSE
  ENDELSE
  
  ; Write header to MRTSfile
  header = ['MOYD35 File', 'MOYD03 File']
  OPENW, lun, MRTSfile, /GET_LUN
  PRINTF, lun, header, format = '(2(A, :, ", "))'
  FREE_LUN, lun
  
  FOR j = 0, MOYD35_count - 1 DO BEGIN
    MOYD35fname = MOYD35list[j]
    MOYD35fbname = FILE_BASENAME(MOYD35fname)
    str = STRSPLIT(MOYD35fbname, '.', /EXTRACT)
    pattern = str[0] + '_' + str[1] + '_' + str[2] + '_Cloud_Mask_b*.tif'
    TIFlist = FILE_SEARCH(MOYD35GeoRefDir, pattern, count = TIF_count)
    
    IF TIF_count LT 6 THEN BEGIN
      PRINT, TIFlist
      ; Copy MOYD35 file to oMOYD35Dir
      FILE_COPY, MOYD35fname, oMOYD35Dir, /OVERWRITE
      
      pattern = 'MYD03.' + str[1] + '.' + str[2] + '*.hdf'
      MOYD03list = FILE_SEARCH(MOYD03Dir, pattern, count = MOYD03_count, /FULLY_QUALIFY_PATH)
      IF MOYD03_count EQ 1 THEN BEGIN
        MOYD03fname = MOYD03list[0]
        MOYD03fbname = FILE_BASENAME(MOYD03fname)
        ; Write record to MRTSfile
        record = [MOYD35fbname, MOYD03fbname]
        OPENW, lun, MRTSfile, /GET_LUN, /APPEND
        PRINTF, lun, record, format = '(2(A, :, ", "))'
        FREE_LUN, lun
        
        ; Copy MOYD03 file to oMOYD03Dir
        FILE_COPY, MOYD03fname, oMOYD03Dir, /OVERWRITE
      ENDIF
      FREE_LUN, lun
      
      IF TIF_count NE 0 THEN FILE_DELETE, TIFlist
    ENDIF
  ENDFOR
END