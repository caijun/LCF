;+
;       Function of getting bounding rectangle of a HDF-EOS Swath file
;
;            Usage: BoudingRect = GETBOUNDINGRECT(FileName, /Type)
;     The returned BoundingRect contains bounding rectangle coordinates if
;                             Type = 'LAT_LONG'
;                 BoundingRect[0]: east bounding coordinate
;                 BoundingRect[1]: west bounding coordinate
;                 BoundingRect[2]: south bounding coordinate
;                 BoundingRect[3]: north bounding coordinate
;                             Type = 'LINE_SAMPLE'
;                 BoundingRect[0]: 1
;                 BoundingRect[1]: 1
;                 BoundingRect[2]: sample
;                 BoundingRect[3]: line
;
;                        Version: 2.1.0 (2013-12-7)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
FUNCTION GETBOUNDINGRECT, FileName, Type  
  BoundingRect = STRARR(4)
  
  SDinterface_id = HDF_SD_START(FileName, /READ)
  IF Type EQ 'LAT_LONG' THEN BEGIN
    ; Bounding rectangle information in 'CoreMetadata.0' attribute
    gindex = HDF_SD_ATTRFIND(SDinterface_id, 'CoreMetadata.0')
    IF gindex EQ -1 THEN RETURN, !NULL
    HDF_SD_ATTRINFO, SDinterface_id, gindex, data = data
    
    ; Locate bounding rectangle group
    gstart = 'GROUP                  = BOUNDINGRECTANGLE'
    sind = STRPOS(data, gstart)
    gend = 'END_GROUP              = BOUNDINGRECTANGLE'
    eind = STRPOS(data, gend)
    strrect = STRMID(data, sind, eind - sind + STRLEN(gend))
    
    ; Locate east, west, south, north bounding coordinate
    strbound = ['EASTBOUNDINGCOORDINATE', 'WESTBOUNDINGCOORDINATE', 'SOUTHBOUNDINGCOORDINATE', 'NORTHBOUNDINGCOORDINATE']
    FOR i = 0, N_ELEMENTS(strbound) - 1 DO BEGIN
      objstart = 'OBJECT                 = ' + strbound[i]
      sind = STRPOS(strrect, objstart)
      objend = 'END_OBJECT             = ' + strbound[i]
      eind = STRPOS(strrect, objend)
      strobj = STRMID(strrect, sind, eind - sind + STRLEN(gend))
      
      result = STRSPLIT(strobj, ' ', /EXTRACT)
      ; remove '\n' from the string, e.g:'123\n'->'123'
      BoundingRect[i] = STRSPLIT(result[8], STRING(10B), /EXTRACT)
    ENDFOR
  ENDIF ELSE BEGIN
    BoundingRect[0] = '1'
    BoundingRect[1] = '1'
    ; SAMPLE information in 'Maximum_Number_of_1km_Frames' attribute
    gindex = HDF_SD_ATTRFIND(SDinterface_id, 'Maximum_Number_of_1km_Frames')
    IF gindex EQ -1 THEN RETURN, !NULL
    HDF_SD_ATTRINFO, SDinterface_id, gindex, data = sample
    BoundingRect[2] = STRTRIM(STRING(sample), 1)
    
    ; Line information in 'Number_of_Instrument_Scans' attribute
    gindex = HDF_SD_ATTRFIND(SDinterface_id, 'Number_of_Instrument_Scans')
    IF gindex EQ -1 THEN RETURN, !NULL
    HDF_SD_ATTRINFO, SDinterface_id, gindex, data = line
    BoundingRect[3] = STRTRIM(STRING(line), 1)
  ENDELSE
  HDF_SD_END, SDinterface_id
  
  RETURN, BoundingRect
END