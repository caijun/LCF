;+
;  Generating prm file and bat file for MRTSwath to automated batch processing
;
;                       Version: 1.2.6 (2013-12-15)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO GEN_PRM_BAT_FILE
  COMPILE_OPT IDL2
  
  RESOLVE_ROUTINE, 'GETBOUNDINGRECT', /IS_FUNCTION
  
  ; Custimize MOD03Dir and MOD35GeoRef
  year = 2003
  pdir = 'H:\People\ZhangYawen\C6\'
  MOYD35Dir = pdir + 'MYD35\' + STRTRIM(STRING(year), 1) + '\'
  MOYD03Dir = pdir + 'MYD03\' + STRTRIM(STRING(year), 1) + '\'
  MOYD35GeoRefDir = pdir + 'MYD35GeoRef\' + STRTRIM(STRING(year), 1) + '\'
  IF FILE_TEST(MOYD35GeoRefDir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, MOYD35GeoRefDir
  
  outdir = MOYD35Dir + 'prm\'
  IF FILE_TEST(outdir, /DIRECTORY, /WRITE) EQ 0 THEN FILE_MKDIR, outdir
  
  batfile = outdir + 'mrtswath.bat'
  ; Delete the existed mrtswath.bat file
  IF FILE_TEST(batfile) EQ 1 THEN FILE_DELETE, batfile
  
  MOYD35list = FILE_SEARCH(MOYD35Dir, '*.hdf', count = MOYD35_count, /FULLY_QUALIFY_PATH)
  IF count EQ 0 THEN RETURN
  
  FOR i = 0, MOYD35_count - 1 DO BEGIN
    fname = MOYD35list[i]
    ; Write prm file
    fbname = FILE_BASENAME(fname)
    str = STRSPLIT(fbname, '.', /EXTRACT)
    prmfile = outdir + str[0] + '.' + str[1] + '.' + str[2] + '.prm'
    PRINT, prmfile
    
    OPENW, lun, prmfile, /get_lun
    PRINTF, lun, ''
    input_filename = 'INPUT_FILENAME = ' + fname
    PRINTF, lun, input_filename
    PRINTF, lun, ''
    
    pattern = '*.' + str[1] + '.' + str[2] + '*.hdf'
    MOYD03list = FILE_SEARCH(MOYD03Dir, pattern, count = MOYD03_count, /FULLY_QUALIFY_PATH)
    IF MOYD03_count NE 1 THEN RETURN
    
    MOYD03file = MOYD03list[0]
    geolocation_filename = 'GEOLOCATION_FILENAME = ' + MOYD03file
    PRINTF, lun, geolocation_filename
    PRINTF, lun, ''
    
    input_sds_name = 'INPUT_SDS_NAME = Cloud_Mask, 1, 1, 1, 1, 1, 1'
    PRINTF, lun, input_sds_name
    PRINTF, lun, ''
    
    spatial_subset_type = 'LAT_LONG'
    ; spatial_subset_type = 'LINE_SAMPLE'
    output_spatial_subset_type = 'OUTPUT_SPATIAL_SUBSET_TYPE = ' + spatial_subset_type
    PRINTF, lun, output_spatial_subset_type
    BoundingRect = GETBOUNDINGRECT(fname, spatial_subset_type)
    IF BoundingRect EQ !NULL THEN BEGIN
      ; Stop writing prmfile and delete it
      FREE_LUN, lun
      FILE_DELETE, prmfile
      CONTINUE
    ENDIF
    
    CASE spatial_subset_type OF
      'LAT_LONG': BEGIN
        output_space_ul_corner = 'OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) = ' + BoundingRect[1] + ' ' + BoundingRect[3]
        output_space_lr_corner = 'OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) = ' + BoundingRect[0] + ' ' + BoundingRect[2]
      END
      'LINE_SAMPLE': BEGIN
        output_space_ul_corner = 'OUTPUT_SPACE_UPPER_LEFT_CORNER (SAMPLE LINE) = ' + BoundingRect[0] + ' ' + BoundingRect[1]
        output_space_lr_corner = 'OUTPUT_SPACE_LOWER_RIGHT_CORNER (SAMPLE LINE) = ' + BoundingRect[2] + ' ' + BoundingRect[3]
      END
    ENDCASE
    PRINTF, lun, output_space_ul_corner
    PRINTF, lun, output_space_lr_corner
    PRINTF, lun, ''
    
    ; Note: ENVI->Spectral->SPEAR Tools->Google Earth Bridge doesn't support dot in the file name
    output_filename = 'OUTPUT_FILENAME = ' + MOYD35GeoRefDir + str[0] + '_' + str[1] + '_' + str[2]
    PRINTF, lun, output_filename
    output_file_format = 'OUTPUT_FILE_FORMAT = GEOTIFF_FMT'
    PRINTF, lun, output_file_format
    PRINTF, lun, ''
    
    kernel_type = 'KERNEL_TYPE (CC/BI/NN) = NN'
    PRINTF, lun, kernel_type
    PRINTF, lun, ''
    
    output_proj_num = 'OUTPUT_PROJECTION_NUMBER = GEO'
    PRINTF, lun, output_proj_num
    PRINTF, lun, ''
    
    output_proj_param = 'OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0'
    PRINTF, lun, output_proj_param
    PRINTF, lun, ''
    
    output_proj_sphere = 'OUTPUT_PROJECTION_SPHERE = 8'
    PRINTF, lun, output_proj_sphere
    PRINTF, lun, ''
    FREE_LUN, lun
    
    ; Write batch file
    OPENW, lun, batfile, /GET_LUN, /APPEND
    
    cmd = 'swath2grid -pf=' + prmfile
    PRINTF, lun, cmd
    PRINTF, lun, ''
    
    FREE_LUN, lun
  ENDFOR
END