-- Gender labels.
CREATE OR REPLACE VIEW wsdb.gender_labels AS
SELECT GenderID, GenderName AS GenderLabel
FROM wsdb.genders
ORDER BY GenderID;

-- Artist labels.  First name (if present) followed by last name,
-- followed by ID (because there are artists with the same name).
CREATE OR REPLACE VIEW wsdb.artist_labels AS
SELECT ArtistID,
       CONCAT(CASE WHEN FirstName IS NULL THEN ''
                   ELSE CONCAT(FirstName, ' ')
			  END,
              LastName, ' (', ArtistID, ')') AS ArtistLabel
FROM wsdb.artists
ORDER BY LastName, FirstName;

-- Copyright administrator labels.
CREATE OR REPLACE VIEW wsdb.copyrightadministrator_labels AS
SELECT CopyrightAdministratorID,
       CopyrightAdministratorName AS CopyrightAdministratorLabel
FROM wsdb.copyrightadministrators
ORDER BY CopyrightAdministratorName;

-- Copyright holder labels.
CREATE OR REPLACE VIEW wsdb.copyrightholder_labels AS
SELECT CopyrightHolderID,
	   CopyrightHolderName AS CopyrightHolderLabel
FROM wsdb.copyrightholders
ORDER BY CopyrightHolderName;

-- Language labels.
CREATE OR REPLACE VIEW wsdb.language_labels AS
SELECT LanguageID, LanguageName AS LanguageLabel
FROM wsdb.languages
ORDER BY LanguageName;

-- Book of the Bible labels.
CREATE OR REPLACE VIEW wsdb.bookofthebible_labels AS
SELECT BookID, BookName AS BookLabel
FROM wsdb.booksofthebible
ORDER BY BookID;

-- Scripture reference labels.  Standard Book Chapter:Verse format.
CREATE OR REPLACE VIEW wsdb.scripturereference_labels AS
SELECT scripturereferences.ScriptureReferenceID,
       CONCAT(booksofthebible.BookAbbreviation, ' ',
              scripturereferences.Chapter, ':',
              scripturereferences.Verse) AS ScriptureReferenceLabel
FROM wsdb.scripturereferences
     JOIN wsdb.booksofthebible
     ON scripturereferences.BookID = booksofthebible.BookID
ORDER BY booksofthebible.BookID, scripturereferences.Chapter,
         scripturereferences.Verse;

-- Meter labels.  Meter code plus multiplier (if present).  Sort
-- irregular first.
CREATE OR REPLACE VIEW wsdb.meter_labels AS
SELECT MeterID,
       CONCAT(Meter,
              CASE WHEN Multiplier IS NULL THEN ''
                   ELSE CONCAT(' ', Multiplier)
			  END) AS MeterLabel
FROM wsdb.meters
ORDER BY CASE WHEN Meter = 'Irregular' THEN 0
              ELSE 1
	     END,
         SortString, Multiplier;

-- Pitch labels.
CREATE OR REPLACE VIEW wsdb.pitch_labels AS
SELECT PitchID, PitchName AS PitchLabel
FROM wsdb.pitches
ORDER BY PitchName;

-- Accidental labels.  Sort natural first.
CREATE OR REPLACE VIEW wsdb.accidental_labels AS
SELECT AccidentalID, AccidentalSymbol AS AccidentalLabel
FROM wsdb.accidentals
ORDER BY CASE WHEN AccidentalID = 3 THEN 0
			  ELSE AccidentalID
		 END;

-- Mode labels.
CREATE OR REPLACE VIEW wsdb.mode_labels AS
SELECT ModeID, ModeName AS ModeLabel
FROM wsdb.modes
ORDER BY ModeID;

-- Key signature labels.  Abbreviate minor as "m" and use full
-- name of other non-major modes.  Sort by pitch, then accidental,
-- then mode.
CREATE OR REPLACE VIEW wsdb.keysignature_labels AS
SELECT KeySignatureID,
       CONCAT(PitchName,
              CASE WHEN accidentals.AccidentalID = 3 THEN ''
                   ELSE accidentals.AccidentalSymbol
			  END,
			  CASE WHEN modes.ModeID = 1 THEN ''
                   WHEN modes.ModeID = 2 THEN 'm'
                   ELSE CONCAT(' ', modes.ModeName)
			  END) AS KeySignatureLabel
FROM wsdb.keysignatures
     JOIN wsdb.pitches
     ON keysignatures.PitchID = pitches.PitchID
     JOIN wsdb.accidentals
     ON keysignatures.AccidentalID = accidentals.AccidentalID
     JOIN wsdb.modes
     ON keysignatures.ModeID = modes.ModeID
ORDER BY pitches.PitchName,
         CASE WHEN accidentals.AccidentalID = 3 THEN 0
              ELSE accidentals.AccidentalID
		 END,
         modes.ModeID;

-- Time signature labels.  Sort by measure, then beat.
CREATE OR REPLACE VIEW wsdb.timesignature_labels AS
SELECT TimeSignatureID,
       CONCAT(TimeSignatureBeat, '/', TimeSignatureMeasure) AS TimeSignatureLabel
FROM wsdb.timesignatures
ORDER BY TimeSignatureMeasure, TimeSignatureBeat;

-- Lyric labels.  First line and refrain first line, separated by
-- a line break.  Include ID for uniqueness.
CREATE OR REPLACE VIEW wsdb.lyrics_labels AS
SELECT LyricsID,
       CONCAT(FirstLine,
              CASE WHEN RefrainFirstLine IS NULL THEN ''
				   ELSE CONCAT('<br/>', RefrainFirstLine)
			  END,
              '<br/>(', LyricsID, ')') AS LyricsLabel
FROM wsdb.lyrics
ORDER BY FirstLine, RefrainFirstLine;

-- Tune labels.
CREATE OR REPLACE VIEW wsdb.tune_labels AS
SELECT TuneID, TuneName AS TuneLabel
FROM wsdb.tunes
ORDER BY TuneName;

-- Arrangement type labels.
CREATE OR REPLACE VIEW wsdb.arrangementtype_labels AS
SELECT ArrangementTypeID, ArrangementType AS ArrangementTypeLabel
FROM wsdb.arrangementtypes
ORDER BY ArrangementType;

-- Arrangement labels.  Include ID for uniqueness.
CREATE OR REPLACE VIEW wsdb.arrangement_labels AS
SELECT ArrangementID,
       CONCAT(ArrangementName, ' (', ArrangementID, ')') AS ArrangementLabel
FROM wsdb.arrangements
ORDER BY ArrangementName, ArrangementID;

-- Songbook labels.
CREATE OR REPLACE VIEW wsdb.songbook_labels AS
SELECT SongbookID, SongbookName AS SongbookLabel
FROM wsdb.songbooks
ORDER BY SongbookName;

-- Songbook volume labels.
CREATE OR REPLACE VIEW wsdb.songbookvolume_labels AS
SELECT SongbookVolumeID, SongbookVolume AS SongbookVolumeLabel
FROM wsdb.songbookvolumes
ORDER BY SongbookVolume;

-- Song instance labels.  Include title, key/time signatures, arrangement,
-- and ID to make it easier to identify the one we want when processing
-- songbooks.
CREATE OR REPLACE VIEW wsdb.songinstance_labels AS
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
	  GROUP BY songinstances.SongInstanceID)
SELECT songinstances.SongInstanceID,
       CONCAT('<b>', songinstances.SongInstance, ' (',
              songinstances.SongInstanceID, ')</b>',
              CASE WHEN keysignatures_concat.KeySignatures IS NULL
                        AND timesignatures_concat.TimeSignatures IS NULL
                        THEN ''
                   ELSE '\n'
              END,
              IFNULL(keysignatures_concat.KeySignatures, ''),
			  CASE WHEN keysignatures_concat.KeySignatures IS NOT NULL
                        AND timesignatures_concat.TimeSignatures IS NOT NULL
						THEN '; '
				   ELSE ''
			  END,
              IFNULL(timesignatures_concat.TimeSignatures, ''),
              CASE WHEN keysignatures_concat.KeySignatures IS NOT NULL
                        AND timesignatures_concat.TimeSignatures IS NOT NULL
                        AND arrangements.ArrangementName IS NOT NULL
                        THEN '\n'
                   ELSE ''
              END,
              IFNULL(arrangements.ArrangementName, '')) AS SongInstanceLabel
FROM wsdb.songinstances
     LEFT JOIN keysignatures_concat
     ON songinstances.SongInstanceID = keysignatures_concat.SongInstanceID
	 LEFT JOIN timesignatures_concat
     ON songinstances.SongInstanceID = timesignatures_concat.SongInstanceID
     LEFT JOIN wsdb.arrangements
     ON songinstances.ArrangementID = arrangements.ArrangementID
ORDER BY songinstances.SongInstance,
         songinstances.SongInstanceID;

-- Topic labels.
CREATE OR REPLACE VIEW wsdb.topic_labels AS
SELECT TopicID, TopicName as TopicLabel
FROM wsdb.topics
ORDER BY TopicName;

-- Song type labels.
CREATE OR REPLACE VIEW wsdb.songtype_labels AS
SELECT SongTypeID, SongType AS SongTypeLabel
FROM wsdb.songtypes
ORDER BY SongTypeID;

-- Song tempo labels.
CREATE OR REPLACE VIEW wsdb.songtempo_labels AS
SELECT SongTempoID, SongTempo AS SongTempoLabel
FROM wsdb.songtempi
ORDER BY SongTempoID;

-- Song labels.  Title plus disambiguator (if present) plus one line
-- of lyrics.
CREATE OR REPLACE VIEW wsdb.song_labels AS
WITH all_song_lyrics AS
     (SELECT songinstances.SongID,
			 COALESCE(lyrics.RefrainFirstLine,
                      lyrics.FirstLine, '') AS LyricsLine,
             COUNT(*) AS NumInstances,
			 lyrics.LyricsID,
             CASE WHEN lyrics.LanguageID IN (1, 2) THEN 'Y'
                  ELSE 'N'
			 END AS EnglishOrSpanish,
             CASE WHEN lyrics.LanguageID = 1 THEN 'Y'
                  ELSE 'N'
		     END AS English,
             CASE WHEN lyrics_translations.LyricsID IS NULL THEN 'N'
                  ELSE 'Y'
			 END AS Translation
      FROM wsdb.songinstances
           JOIN wsdb.songinstances_lyrics
           ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
		   JOIN wsdb.lyrics
           ON songinstances_lyrics.LyricsID = lyrics.LyricsID
           LEFT JOIN (SELECT DISTINCT LyricsID
                      FROM wsdb.lyrics_translations) lyrics_translations
		   ON lyrics.LyricsID = lyrics_translations.LyricsID
	  GROUP BY songinstances.SongID,
               COALESCE(lyrics.RefrainFirstLine, lyrics.FirstLine, ''),
               lyrics.LyricsID)
SELECT songs.SongID,
       CONCAT('<b>', songs.SongName,
			  CASE WHEN songs.SongDisambiguator IS NULL THEN ''
				   ELSE CONCAT(' (', CONCAT(songs.SongDisambiguator, ')'))
			  END,
              '</b>',
              CASE WHEN song_lyrics.LyricsLine IS NULL THEN ''
                   ELSE CONCAT('\n', song_lyrics.LyricsLine)
			  END) AS SongLabel
FROM wsdb.songs
     LEFT JOIN (SELECT SongID, LyricsLine,
                       ROW_NUMBER()
					   OVER (PARTITION BY SongID
                             ORDER BY EnglishOrSpanish DESC,
                                      Translation,
                                      English DESC,
                                      NumInstances DESC,
                                      LyricsID) AS ROWNUM
                FROM all_song_lyrics) song_lyrics
	 ON songs.SongID = song_lyrics.SongID
        AND song_lyrics.ROWNUM = 1
ORDER BY songs.SongName, songs.SongDisambiguator;

-- Metrical psalm labels.  Psalm number plus lyrics first line.
CREATE OR REPLACE VIEW wsdb.metricalpsalm_labels AS
SELECT metricalpsalms.MetricalPsalmID,
       CONCAT(PsalmNumber, ': ', FirstLine, ' (', Artists, ', ',
              metricalpsalms.MetricalPsalmID, ')') AS MetricalPsalmLabel
FROM wsdb.metricalpsalms
     JOIN (SELECT MetricalPsalmID, MIN(LyricsID) AS LyricsID
           FROM wsdb.metricalpsalms_lyrics
           GROUP BY MetricalPsalmID) metricalpsalms_lyrics
     ON metricalpsalms.MetricalPsalmID = metricalpsalms_lyrics.MetricalPsalmID
     JOIN wsdb.lyrics
     ON metricalpsalms_lyrics.LyricsID = lyrics.LyricsID
     LEFT JOIN (SELECT lyrics_artists.LyricsID,
                       GROUP_CONCAT(LastName SEPARATOR '/') AS Artists
				FROM wsdb.lyrics_artists
                     JOIN wsdb.artists
                     ON lyrics_artists.ArtistID = artists.ArtistID
				GROUP BY lyrics_artists.LyricsID) lyrics_artists
	 ON lyrics.LyricsID = lyrics_artists.LyricsID;

-- Tune labels.  Display name of canonical song.
CREATE OR REPLACE VIEW wsdb.tune_song_labels AS
SELECT tunes.TuneID,
       CONCAT(CASE WHEN tunes.RealTuneName = 1
                        THEN CONCAT(TuneName, ' (')
                   ELSE ''
              END,
              COALESCE(tunes.CanonicalSongName,
                       CONCAT(canonical_song.SongName,
							  CASE WHEN canonical_song.SongDisambiguator IS NULL
                                        THEN ''
								   ELSE CONCAT(' ',
                                               CASE WHEN tunes.RealTuneName = 1
                                                         THEN '['
													ELSE '('
											   END,
                                               canonical_song.SongDisambiguator,
                                               CASE WHEN tunes.RealTuneName = 1
														 THEN ']'
													ELSE ')'
											   END)
							  END)),
              CASE WHEN tunes.RealTuneName = 1
                        THEN ')'
                   ELSE ''
              END) AS TuneLabel
FROM wsdb.tunes
     LEFT JOIN (SELECT TuneID, MAX(SongID) AS SongID
                FROM wsdb.tunes_canonicalsongs
                GROUP BY TuneID
                HAVING COUNT(*) = 1) one_canonical_song
     ON tunes.TuneID = one_canonical_song.TuneID
     LEFT JOIN wsdb.songs canonical_song
     ON one_canonical_song.SongID = canonical_song.SongID;

-- Year of each song.
CREATE OR REPLACE VIEW wsdb.song_year AS
WITH lyrics_year AS
     (SELECT SongID, MAX(CopyrightYear) AS LyricsYear
	  FROM wsdb.songinstances
		   JOIN wsdb.songinstances_lyrics
           ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
           JOIN wsdb.lyrics
           ON songinstances_lyrics.LyricsID = lyrics.LyricsID
		   LEFT JOIN wsdb.lyrics_translations
           ON lyrics.LyricsID = lyrics_translations.LyricsID
	  WHERE lyrics_translations.LyricsID IS NULL
	  GROUP BY SongID),
	 tune_year AS
     (SELECT SongID, MAX(CopyrightYear) AS TuneYear
      FROM wsdb.songinstances
		   JOIN wsdb.songinstances_tunes
           ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
           JOIN wsdb.tunes
           ON songinstances_tunes.TuneID = tunes.TuneID
	  GROUP BY SongID)
SELECT songs.SongID, GREATEST(LyricsYear, TuneYear) AS Year
FROM wsdb.songs
     LEFT JOIN lyrics_year
     ON songs.SongID = lyrics_year.SongID
     LEFT JOIN tune_year
     ON songs.SongID = tune_year.SongID;
