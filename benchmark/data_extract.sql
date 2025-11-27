SELECT df.[timestamp] as 'ts'
      ,ROUND([value], 3) AS [value]
      ,df.[quality]
      ,qc.[quality_description] as 'desc'
      ,qc.[quality_type] as 'type'
      ,df.[station_id]
      ,df.[node_name]
      ,'TSSeq' AS [parameter]
      ,'mg/L' AS [units]
  FROM [eio_rt_curated].[tsseq] df
  LEFT JOIN [lkp].[quality_codes] qc
    ON df.[quality] = qc.[quality_code]
  WHERE [station_id] = '113006A'