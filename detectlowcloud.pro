;+
;                    Algorithm for detecting low cloud
;
;                       Version: 1.1.0 (2013-12-19)
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
  
  ; Detecting high cloud including the situation that both low cloud and high cloud exist
  highcloud = (bitdata[*, *, 1] EQ 0) OR (bitdata[*, *, 2] EQ 0) OR (bitdata[*, *, 3] EQ 0) OR (bitdata[*, *, 4] EQ 0)
  hcidx = WHERE(highcloud EQ 1)
  ; Detecting low cloud, which is underestimated as the situation that both low cloud and high cloud exist is excluded
  ; (cloudy|probably cloudy) AND (no high cloud) AND (low cloud)
  ; Algorithm for day
  ; (BIT2 EQ 0) AND ((BIT14 EQ 1) AND (BIT15 EQ 1) AND (BIT16 EQ 1) AND (BIT18 EQ 1)) AND ((BIT19 EQ 0) AND (BIT20 EQ 0))
  ; Algorithm for night
  ; (BIT2 EQ 0) AND ((BIT14 EQ 1) AND (BIT15 EQ 1) AND (BIT17 EQ 1) AND (BIT18 EQ 1)) AND ((BIT19 EQ 0) AND (BIT27 EQ 0))
  ; Value meaning
  ; 0: background
  ; 1: no low cloud
  ; 2: low cloud
  ; 3: high cloud
  lowcloud = (bitdata[*, *, 0] EQ 0) AND ((bitdata[*, *, 1] EQ 1) AND (bitdata[*, *, 2] EQ 1) AND (bitdata[*, *, 3] EQ 1) AND (bitdata[*, *, 4] EQ 1)) $
    AND ((bitdata[*, *, 5] EQ 0) AND (bitdata[*, *, 6] EQ 0))
  lowcloud = lowcloud + 1
  lowcloud[hcidx] = 3
  ; Determine backgroud value (0)
  bgidx = WHERE((cloudmask[*, *, 0] EQ 0) AND (cloudmask[*, *, 1] EQ 0) AND (cloudmask[*, *, 2] EQ 0) AND $
    (cloudmask[*, *, 3] EQ 0) AND (cloudmask[*, *, 4] EQ 0) AND (cloudmask[*, *, 5] EQ 0))
  lowcloud[bgidx] = 0
END