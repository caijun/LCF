;+
;       Extacting monthly LCF via evf and writing results to ASCII file
;
;                       Version: 1.2.3 (2013-12-25)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO GET_MOD35_LCF_VIA_EVF
  COMPILE_OPT IDL2
  
  RESOLVE_ROUTINE, 'GETDATAVIAEVF'
  
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ; Customize year and indir
  year = 2003
  indir = 'I:\TT\People\ZhangYawen\C6\MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\LowCloud\LCF\'
  CD, indir
  
  ; Set output directory
  outdir = indir + 'ROIData\'
  IF FILE_TEST(outdir, /DIRECTORY, /write) EQ 0 THEN FILE_MKDIR, outdir
  ; Coverage option: 0 - day data, 1 - night data
  co = ['0', '1']
  
  evf_fname = 'I:\TT\People\ZhangYawen\Vector\NorthChinaPlain\WeatherStation\Station.evf'
  ; Open evf file
  evf_id = ENVI_EVF_OPEN(evf_fname)
  ; Get the vector information
  ENVI_EVF_INFO, evf_id, num_recs = num_recs
  
  coords = DBLARR(2, num_recs)
  FOR i = 0, num_recs - 1 DO BEGIN
    record = ENVI_EVF_READ_RECORD(evf_id, i)
    coords[*, i] = record
  ENDFOR
  ; Close the EVF file
  ENVI_EVF_CLOSE, evf_id
  
  num_months = 12
  months = INDGEN(num_months) + 1
  ROIData = !NULL
  FOR i = 0, N_ELEMENTS(co) - 1 DO BEGIN
    FOR j = 0, num_months - 1 DO BEGIN
      fname = 'MYD35_L2_A' + STRTRIM(STRING(year), 1) + STRTRIM(STRING(months[j], $
        format = '(I02)'), 1) + '_' + co[i] + '.tif'
        
      GETDATAVIAEVF, fname, evf_fname, roi_data
      ROIData = [[[ROIData]], [[roi_data]]]
    ENDFOR
    
    ; Set output file name
    txtfile = outdir + 'MYD35_L2_A' + STRTRIM(STRING(year), 1) + '_' + co[i] + '.txt'
    PRINT, txtfile
    ; Write ROIData to ASCII file
    OPENW, lun, txtfile, /GET_LUN
    ; Print header
    header = ['ID', 'Lng', 'Lat', 'Month', 'LCF1', 'OF', 'NLCF', 'LCF2']
    PRINTF, lun, header
    ; Print data
    FOR j = 0, num_recs - 1 DO BEGIN
      FOR k = 0, num_months - 1 DO BEGIN
        line = [STRTRIM(STRING(j + 1), 1), STRTRIM(STRING(coords[*, j], format = '(F6.2)'), 1), $
          STRTRIM(STRING(k + 1), 1), STRTRIM(STRING(ROIData[*, j, k], format = '(F6.4)'), 1)]
        PRINTF, lun, line
      ENDFOR
    ENDFOR
    FREE_LUN, lun
  ENDFOR
  
  ENVI_BATCH_EXIT
END