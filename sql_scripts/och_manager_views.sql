-- Song labels.
CREATE OR REPLACE VIEW och.song_labels AS
SELECT SongID,
       SongLabel
FROM wsdb.song_labels;

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
SELECT songinstances.SongInstanceID,
       songinstances.SongID,
       CONCAT('<b>', songinstances.SongInstance, '</b> (',
              songinstances.SongInstanceID, ')\n',
              IFNULL(keysignatures_concat.KeySignatures, ''),
              CASE WHEN keysignatures_concat.KeySignatures IS NOT NULL
                        AND timesignatures_concat.TimeSignatures IS NOT NULL
                        THEN '; '
                   ELSE ''
              END,
              IFNULL(timesignatures_concat.TimeSignatures, ''),
              '\n',
              IFNULL(songbookentries_concat.SongbookEntries,
                     '')) AS SongInstanceLabel
FROM wsdb.songinstances
     LEFT JOIN keysignatures_concat
     ON songinstances.SongInstanceID = keysignatures_concat.SongInstanceID
     LEFT JOIN timesignatures_concat
     ON songinstances.SongInstanceID = timesignatures_concat.SongInstanceID
     LEFT JOIN songbookentries_concat
     ON songinstances.SongInstanceID = songbookentries_concat.SongInstanceID
ORDER BY songinstances.SongInstance,
         songinstances.SongInstanceID;

-- All titles associated with each song.  Used by the fuzzy string matcher for
-- bulletin uploads.
CREATE OR REPLACE VIEW och.song_titles AS
WITH all_titles AS
     (SELECT songs.SongID, songs.SongName AS SongTitle
      FROM wsdb.songs
      UNION
      SELECT DISTINCT songinstances.SongID, songinstances.SongInstance AS SongTitle
      FROM wsdb.songinstances)
SELECT SongID,
       GROUP_CONCAT(SongTitle ORDER BY SongTitle SEPARATOR '\n') AS SongTitles
FROM all_titles
GROUP BY SongID;

-- Song info.  Used by the OCH manager for various reports.
CREATE OR REPLACE VIEW och.song_info AS
WITH songinstance_years AS
     (SELECT songinstances_lyrics.SongInstanceID, lyrics.CopyrightYear
      FROM wsdb.songinstances_lyrics
           JOIN wsdb.lyrics
           ON songinstances_lyrics.LyricsID = lyrics.LyricsID
      UNION ALL
      SELECT songinstances_tunes.SongInstanceID, tunes.CopyrightYear
      FROM wsdb.songinstances_tunes
           JOIN wsdb.tunes
           ON songinstances_tunes.TuneID = tunes.TuneID),
     songinstance_max_year AS
     (SELECT songinstance_years.SongInstanceID,
             MAX(songinstance_years.CopyrightYear) AS MaxYear
      FROM songinstance_years
      GROUP BY songinstance_years.SongInstanceID),
     song_min_year AS
     (SELECT songinstances.SongID, MIN(MaxYear) AS Year
      FROM songinstance_max_year
           JOIN wsdb.songinstances
           ON songinstance_max_year.SongInstanceID = songinstances.SongInstanceID
      GROUP BY songinstances.SongID)
SELECT songs.SongID,
       CONCAT(songs.SongName,
              CASE WHEN songs.SongDisambiguator IS NULL THEN ''
                   ELSE CONCAT(' (', songs.SongDisambiguator, ')')
              END) AS SongName,
       song_min_year.Year
FROM wsdb.songs
     LEFT JOIN song_min_year
     ON songs.SongID = song_min_year.SongID;

-- Song-topic pairs.
CREATE OR REPLACE VIEW songs_topics AS
SELECT songs_topics.SongID, topics.TopicName AS Topic
FROM wsdb.songs_topics
     JOIN wsdb.topics
     ON songs_topics.TopicID = topics.TopicID;
