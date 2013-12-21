;+
;                    Algorithm for detecting low cloud
;
;                       Version: 1.2.0 (2013-12-21)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO DETECTLOWCLOUD, cloudmask, lowcloud, DAY = DAY
  COMPILE_OPT IDL2
  
  ; BIT 2: Unobstructed FOV Confidence Flag
  ; 0: cloudy | probably cloudy
  ; BIT 14: High Cloud Flag Test - CO2 Threshold Test
  ; 0: Yes
  ; 1: No
  ; BIT 15: High Cloud Flag Test - 6.7 um Test
  ; 0: Yes
  ; 1: No
  ; BIT 16: High Cloud Flag Test - 1.38 um Test
  ; 0: Yes
  ; 1: No
  ; BIT 17: High Cloud Flag Test - 3.9 -12 um Test (night only)
  ; 0: Yes
  ; 1: No
  ; BIT 18: Cloud Flag - IR Temperture Difference Test
  ; 0: Yes
  ; 1: No
  ; BIT 19: Cloud Flag - 3.9-11 um Test
  ; 0: Yes
  ; 1: No
  ; BIT 20: Cloud Flag - Visible Reflectance Test
  ; 0: Yes
  ; 1: No
  ; BIT 27: Cloud Flag - Surface Temperature Tests (water, night land)
  ; 0: Yes
  ; 1: No
  IF KEYWORD_SET(day) THEN BITS = [2, 14, 15, 16, 18, 19, 20] ELSE BITS = [2, 14, 15, 17, 18, 19, 27]
  
  dims = SIZE(cloudmask, /dimensions)
  bitdata = MAKE_ARRAY(dims[0], dims[1], N_ELEMENTS(BITS))
  FOR i = 0, N_ELEMENTS(BITS) - 1 DO BEGIN
    BIT = BITS[i]
    data = cloudmask[*, *, BIT/8]
    base = 2^(BIT MOD 8)
    bitdata[*, *, i] = (data AND base) EQ base
  ENDFOR
  
  ; Detecting pure low cloud
  ; (cloudy|probably cloudy) AND (no high cloud) AND (probable low cloud)
  ; Algorithm for day
  ; (BIT2 EQ 0) AND ((BIT14 EQ 1) AND (BIT15 EQ 1) AND (BIT16 EQ 1) AND (BIT18 EQ 1)) AND ((BIT19 EQ 0) OR (BIT20 EQ 0))
  ; Algorithm for night
  ; (BIT2 EQ 0) AND ((BIT14 EQ 1) AND (BIT15 EQ 1) AND (BIT17 EQ 1) AND (BIT18 EQ 1)) AND ((BIT19 EQ 0) OR (BIT27 EQ 0))
  ; Classification scheme
  ; 0: background
  ; 1: pure low cloud
  ; 2: obscure
  ; 3: no low cloud
  ; 4: probable low cloud (1 OR 2)
  lowcloud = BYTARR(dims[0], dims[1])
  FOR i = 0, dims[0] - 1 DO BEGIN
    FOR j = 0, dims[1] - 1 DO BEGIN
      IF ARRAY_EQUAL(cloudmask[i, j, *], 0) THEN BEGIN
        ; Background
        lowcloud[i, j] = 0b
      ENDIF ELSE IF (bitdata[i, j, 5] EQ 0) OR (bitdata[i, j, 6] EQ 0) EQ 0 THEN BEGIN
        ; No low cloud
        lowcloud[i, j] = 3b
      ENDIF ELSE IF ((bitdata[i, j, 0] EQ 0) AND $
        ((bitdata[i, j, 1] EQ 1) AND (bitdata[i, j, 2] EQ 1) AND (bitdata[i, j, 3] EQ 1) AND (bitdata[i, j, 4] EQ 1))) EQ 1 THEN BEGIN
        ; Pure low cloud
        lowcloud[i, j] = 1b
      ENDIF ELSE BEGIN
        ; Obscure: pure high cloud and high cloud overlapping low cloud
        lowcloud[i, j] = 2b
      ENDELSE
    ENDFOR
  ENDFOR
END