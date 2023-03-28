-- Congregation labels.
CREATE OR REPLACE VIEW och.congregation_labels AS
SELECT CongregationID,
       CONCAT(CongregationName,
			  CASE WHEN City IS NULL OR State IS NULL THEN ''
                   ELSE CONCAT(' (', City, ', ', State, ')')
			  END) AS CongregationLabel
FROM och.congregations
ORDER BY CongregationName, State, City;

-- Song instance labels.  Include title, key/time signatures, ID, and
-- songbook entries to make it easier to identify the one we want when
-- processing songbooks.
CREATE OR REPLACE VIEW och.songinstance_labels AS
WITH keysignatures_concat AS
     (SELECT songinstances.SongInstanceID,
             GROUP_CONCAT(keysignature_labels.KeySignatureLabel
                          SEPARATOR ', ') AS KeySignatures
	  FROM wsdb.songinstances
           JOIN wsdb.songinstances_keysignatures
           ON songinstances.SongInstanceID = songinstances_keysignatures.SongInstanceID
           JOIN wsdb.keysignature_labels
           ON songinstances_keysignatures.KeySignatureID = keysignature_labels.KeySignatureID
	  GROUP BY songinstances.SongInstanceID),
     timesignatures_concat AS
	 (SELECT songinstances.SongInstanceID,
             GROUP_CONCAT(timesignature_labels.TimeSignatureLabel
                          SEPARATOR ', ') AS TimeSignatures
      FROM wsdb.songinstances
           JOIN wsdb.songinstances_timesignatures
           ON songinstances.SongInstanceID = songinstances_timesignatures.SongInstanceID
           JOIN wsdb.timesignature_labels
           ON songinstances_timesignatures.TimeSignatureID = timesignature_labels.TimeSignatureID
	  GROUP BY songinstances.SongInstanceID),
	 songbookentries_concat AS
     (SELECT songinstances.SongInstanceID,
			 GROUP_CONCAT(CONCAT(songbooks.SongbookAbbreviation,
								 CASE WHEN songbookentries.EntryNumber IS NULL THEN ''
                                      ELSE ' '
								 END,
                                 IFNULL(songbookentries.EntryNumber, ''))
                          SEPARATOR ', ') AS SongbookEntries
	  FROM wsdb.songbookentries
           JOIN wsdb.songinstances
           ON songbookentries.SongInstanceID = songinstances.SongInstanceID
		   JOIN wsdb.songbooks
           ON songbookentries.SongBookID = songbooks.SongBookID
	  GROUP BY songinstances.SongInstanceID)
SELECT songinstances.SongInstanceID, songinstances.SongID,
       CONCAT('<b>', songinstances.SongInstance, ' (',
              songinstances.SongInstanceID, ')</b>\n',
              IFNULL(keysignatures_concat.KeySignatures, ''),
			  CASE WHEN keysignatures_concat.KeySignatures IS NOT NULL
                        AND timesignatures_concat.TimeSignatures IS NOT NULL
						THEN '; '
				   ELSE ''
			  END,
              IFNULL(timesignatures_concat.TimeSignatures, ''),
              '\n', IFNULL(songbookentries_concat.SongbookEntries, '')) AS SongInstanceLabel
FROM wsdb.songinstances
     LEFT JOIN keysignatures_concat
     ON songinstances.SongInstanceID = keysignatures_concat.SongInstanceID
	 LEFT JOIN timesignatures_concat
     ON songinstances.SongInstanceID = timesignatures_concat.SongInstanceID
     LEFT JOIN songbookentries_concat
     ON songinstances.SongInstanceID = songbookentries_concat.SongInstanceID
ORDER BY songinstances.SongInstance,
         songinstances.SongInstanceID;