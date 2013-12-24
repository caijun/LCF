;+
;           Getting ROI data from specific img via specific evf
;
;                       Version: 1.1.0 (2013-12-25)
;
;                    Author: Tony Tsai, Ph.D. Student
;          (Center for Earth System Science, Tsinghua University)
;               Contact Email: cai-j12@mails.tsinghua.edu.cn
;
;                    Copyright: belongs to Dr.Xu's Lab
;               (School of Environment, Tsinghua University)
; (College of Global Change and Earth System Science, Beijing Normal University)
;-
PRO GETDATAVIAEVF, img_fname, evf_fname, data
  ; Open evf file
  evf_id = ENVI_EVF_OPEN(evf_fname)
  
  ; Get the vector information
  ENVI_EVF_INFO, evf_id, num_recs = num_recs, data_type = data_type, projection = projection, $
    layer_name = layer_name
    
  lng = DBLARR(num_recs)
  lat = DBLARR(num_recs)
  FOR i = 0, num_recs - 1 DO BEGIN
    record = ENVI_EVF_READ_RECORD(evf_id, i)
    lng[i] = record[0, *]
    lat[i] = record[1, *]
  ENDFOR
  
  ; Close the EVF file
  ENVI_EVF_CLOSE, evf_id
  
  ENVI_OPEN_FILE, img_fname, r_fid = fid
  IF (fid EQ -1) THEN RETURN
  
  ENVI_FILE_QUERY, fid, ns = ns, nl = nl, nb = nb, dims = dims, bnames = bnames
  ; Convert latitude and longitude of the record to file coordinates
  ENVI_CONVERT_FILE_COORDINATES, fid, xf, yf, lng, lat
  
  ; Create ROI
  roi_id = ENVI_CREATE_ROI(color = 4, name = 'Sampling Point', ns = ns, nl = nl)
  ENVI_DEFINE_ROI, roi_id, /point, xpts = REFORM(xf), ypts = REFORM(yf)
  
  pos = INDGEN(nb)
  data = ENVI_GET_ROI_DATA(roi_id, fid = fid, pos = pos)
  
  ENVI_FILE_MNG, id = fid, /REMOVE
END