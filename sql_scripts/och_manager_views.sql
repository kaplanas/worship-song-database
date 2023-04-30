-- Congregation labels.
CREATE OR REPLACE VIEW och.congregation_labels AS
SELECT CongregationID,
       CONCAT(CongregationName,
			  CASE WHEN City IS NULL OR State IS NULL THEN ''
                   ELSE CONCAT(' (', City, ', ', State, ')')
			  END) AS CongregationLabel,
	   UseData
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
                                 CASE WHEN songbookvolumes.SongbookVolumeID <> 2
                                           THEN CONCAT(' ', songbookvolumes.SongbookVolume)
									  ELSE ''
								 END,
								 CASE WHEN songbookentries.EntryNumber IS NULL THEN ''
                                      ELSE ' '
								 END,
                                 IFNULL(songbookentries.EntryNumber, ''))
						  ORDER BY songbooks.SongbookAbbreviation,
								   songbookentries.EntryNumber
                          SEPARATOR ', ') AS SongbookEntries
	  FROM wsdb.songbookentries
           JOIN wsdb.songinstances
           ON songbookentries.SongInstanceID = songinstances.SongInstanceID
		   LEFT JOIN wsdb.songbookvolumes
           ON songbookentries.SongbookVolumeID = songbookvolumes.SongbookVolumeID
		   JOIN wsdb.songbooks
           ON songbookentries.SongBookID = songbooks.SongBookID
	  GROUP BY songinstances.SongInstanceID)
SELECT songinstances.SongInstanceID, songinstances.SongID,
       CONCAT('<b>', songinstances.SongInstance, '</b> (',
              songinstances.SongInstanceID, ')\n',
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

-- Songs with restoration-affiliated writers.
CREATE OR REPLACE VIEW och.restoration_songs AS
WITH restoration_lyrics AS
     (SELECT DISTINCT songinstances_lyrics.SongInstanceID
      FROM wsdb.songinstances_lyrics
           JOIN wsdb.lyrics_artists
           ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID
           JOIN wsdb.artists
           ON lyrics_artists.ArtistID = artists.ArtistID
      WHERE artists.Restoration),
	 restoration_tune AS
     (SELECT DISTINCT songinstances_tunes.SongInstanceID
      FROM wsdb.songinstances_tunes
           JOIN wsdb.tunes_artists
           ON songinstances_tunes.TuneID = tunes_artists.TuneID
           JOIN wsdb.artists
           ON tunes_artists.ArtistID = artists.ArtistID
	  WHERE artists.Restoration)
SELECT DISTINCT songs.SongID
FROM wsdb.songs
     LEFT JOIN (SELECT songinstances.SongID
                FROM wsdb.songinstances
                     LEFT JOIN restoration_lyrics
                     ON songinstances.SongInstanceID = restoration_lyrics.SongInstanceID
				GROUP BY songinstances.SongID
                HAVING SUM(CASE WHEN restoration_lyrics.SongInstanceID IS NULL THEN 0
								ELSE 1
						   END) > 0
					   AND SUM(CASE WHEN restoration_lyrics.SongInstanceID IS NULL THEN 1
                                    ELSE 0
							   END) = 0) all_restoration_lyrics
     ON songs.SongID = all_restoration_lyrics.SongID
     LEFT JOIN (SELECT songinstances.SongID
                FROM wsdb.songinstances
                     LEFT JOIN restoration_tune
                     ON songinstances.SongInstanceID = restoration_tune.SongInstanceID
				GROUP BY songinstances.SongID
                HAVING SUM(CASE WHEN restoration_tune.SongInstanceID IS NULL THEN 0
								ELSE 1
						   END) > 0
					   AND SUM(CASE WHEN restoration_tune.SongInstanceID IS NULL THEN 1
                                    ELSE 0
							   END) = 0) all_restoration_tune
     ON songs.SongID = all_restoration_tune.SongID
WHERE all_restoration_lyrics.SongID IS NOT NULL
      OR all_restoration_tune.SongID IS NOT NULL;