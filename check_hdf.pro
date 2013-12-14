;+
;    Check HDF integrity and record the invalid HDF files for redowndload
;           
;                       Version: 1.0.0 (2013-12-15)
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
    IF FILE_TEST(whdf) EQ 1 THEN FILE_DELETE, whdf
    
    flist = FILE_SEARCH(subdir, '*.hdf', count = num_hdf, /FULLY_QUALIFY_PATH)
    IF num_hdf EQ 0 THEN RETURN
    
    FOR j = 0,  num_hdf - 1 DO BEGIN
      fname = flist[j]
      
      CATCH, Error_status
      IF Error_status NE 0 THEN BEGIN
        PRINT, fname
        ; Write the wrong HDF file name to redownload.txt
        OPENW, lun, whdf, /GET_LUN, /APPEND
        PRINTF, lun, FILE_BASENAME(fname)
        FREE_LUN, lun
        
        PRINT, 'Error index: ', Error_status
        PRINT, 'Error message: ', !ERROR_STATE.MSG
        CATCH, /CANCEL
        CONTINUE
      ENDIF
      
      SDinterface_id = HDF_SD_START(fname, /READ)
      HDF_SD_END, SDinterface_id
    ENDFOR
  ENDFOR
END