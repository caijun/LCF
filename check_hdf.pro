;+
;    Check HDF integrity and record the invalid HDF files for redowndload
;
;                       Version: 1.2.0 (2013-12-16)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO CHECK_HDF
  COMPILE_OPT IDL2
  
  ; Customize indir
  indir = 'H:\ZhangYawen\2003\'
  dlist = FILE_SEARCH(indir, '*', count = num_dir, /TEST_DIRECTORY)
  IF num_dir EQ 0 THEN RETURN
  
  FOR i = 0, num_dir - 1 DO BEGIN
    subdir = dlist[i]
    ; Record wrong HDF files
    whdf = subdir + '.redownload.txt'
    PRINT, whdf
    
    ; First check
    IF FILE_TEST(whdf) EQ 0 THEN BEGIN
      flist = FILE_SEARCH(subdir, '*.hdf', count = num_hdf, /FULLY_QUALIFY_PATH)
      IF num_hdf EQ 0 THEN CONTINUE
    ENDIF ELSE BEGIN
      ; Read previous checking results and only check those files to save time
      results = QUERY_ASCII(whdf, info)
      num_hdf = info.LINES - 1
      IF num_hdf EQ 0 THEN BEGIN
        CONTINUE
      ENDIF ELSE BEGIN
        header = ''
        ; Records of wrong HDF files
        records = !NULL
        records = STRARR(num_hdf)
        OPENR, lun, whdf, /GET_LUN
        ; Skip header
        READF, lun, header
        READF, lun, records
        FREE_LUN, lun
        ; Split records to rows*cols
        ; Note: records is a LIST
        records = STRSPLIT(records, ', ', /EXTRACT)
        flist = STRARR(num_hdf)
        FOR k = 0, num_hdf - 1 DO flist[k] = subdir + '\' + (records[k])[0]
        
        FILE_DELETE, whdf
      ENDELSE
    ENDELSE
    
    ; Write header to redownload.txt
    header = ['File Name', 'Error Index', 'Error Message']
    OPENW, lun, whdf, /GET_LUN
    PRINTF, lun, header, format = '(3(A, :, ", "))'
    FREE_LUN, lun
    
    FOR j = 0,  num_hdf - 1 DO BEGIN
      fname = flist[j]
      fbname = FILE_BASENAME(fname)
      str = STRSPLIT(fbname, '.', /EXTRACT)
      IF str[0] EQ 'MYD35_L2' THEN SDS_name = 'Cloud_Mask' ELSE SDS_name = ['Longitude', 'Latitude']
      
      CATCH, Error_status
      IF Error_status NE 0 THEN BEGIN
        ; Write record to redownload.txt
        record = [FILE_BASENAME(fname), STRTRIM(STRING(Error_status), 1), !ERROR_STATE.MSG]
        OPENW, lun, whdf, /GET_LUN, /APPEND
        PRINTF, lun, record, format = '(3(A, :, ", "))'
        FREE_LUN, lun
        ; Print on console
        PRINT, fname
        PRINT, 'Error index: ', Error_status
        PRINT, 'Error message: ', !ERROR_STATE.MSG
        CATCH, /CANCEL
        CONTINUE
      ENDIF
      
      ; Try to catch Error index: -1112
      SDinterface_id = HDF_SD_START(fname, /READ)
      FOR k = 0, N_ELEMENTS(SDS_name) - 1 DO BEGIN
        index = HDF_SD_NAMETOINDEX(SDinterface_id, SDS_name[k])
        ; Try to catch Error index: -1113
        SDdataset_id = HDF_SD_SELECT(SDinterface_id, index)
        ; Try to catch Error index: -1119
        HDF_SD_GETDATA, SDdataset_id, data
        HDF_SD_ENDACCESS, SDdataset_id
      ENDFOR
      HDF_SD_END, SDinterface_id
    ENDFOR
  ENDFOR
END