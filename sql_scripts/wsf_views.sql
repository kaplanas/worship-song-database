-- SONGBOOK ENTRY DATA --

-- Table of songbooks
CREATE OR REPLACE VIEW wsf.songbooks AS
SELECT songbooks.SongbookID,
       songbooks.SongbookName,                  
       songbooks.SongbookAbbreviation
FROM wsdb.songbooks
WHERE (songbooks.IncludeInSearch);

-- SONG INSTANCE DATA --

-- Table that connects song instances and songbooks
CREATE OR REPLACE VIEW wsf.songinstances_songbooks AS
WITH entries AS
     (SELECT songbookentries.SongbookEntryID,
             songinstances.SongInstanceID,
             songinstances.SongID,
             songbooks.SongbookID,
             songbooks.SongbookName,
             songbooks.SongbookAbbreviation,
             songbookvolumes.SongbookVolumeID,
             songbookvolumes.SongbookVolume,
             songbookentries.EntryNumber,
             CONCAT(CASE WHEN songbookvolumes.SongbookVolume IS NULL THEN ''
                         WHEN songbooks.SongbookID = 6 THEN ''
                         WHEN songbookvolumes.SongbookVolumeID = 2 THEN ''
                         ELSE CONCAT('(', songbookvolumes.SongbookVolume, ')')
                    END,
                    CASE WHEN songbooks.SongbookID = 12 THEN ''
                         WHEN songbookentries.EntryNumber IS NULL THEN ''
                         WHEN songbookentries.EntryNumber = '' THEN ''
                         ELSE CONCAT(CASE WHEN songbookvolumes.SongbookVolume IS NULL
                                               THEN ''
                                          WHEN songbooks.SongbookID = 6
                                               THEN ''
                                          WHEN songbookvolumes.SongbookVolumeID = 2
                                               THEN ''
                                          ELSE ' '
                                     END,
                                     songbookentries.EntryNumber)
                    END) AS EntryStringNoName,
             CONCAT(songinstances.SongInstanceID, '-',
                    COALESCE(EntryNumber, 'none')) AS SongInstanceIDEntryNumber
      FROM wsdb.songinstances
           JOIN wsdb.songbookentries
           ON songinstances.SongInstanceID = songbookentries.SongInstanceID
           JOIN wsf.songbooks
           ON songbookentries.SongbookID = songbooks.SongbookID
           LEFT JOIN wsdb.songbookvolumes
           ON songbookentries.SongbookVolumeID = songbookvolumes.SongbookVolumeID)
SELECT SongbookEntryID,
       SongInstanceID,
       SongID,
       SongbookID,
       SongbookName,
       SongbookAbbreviation,
       SongbookVolumeID,
       SongbookVolume,
       EntryNumber,
       CONCAT(SongbookName,
              CASE WHEN EntryStringNoName IS NULL THEN ''
                   WHEN EntryStringNoName = '' THEN ''
                   ELSE ' '
              END,
              EntryStringNoName) AS EntryString,
       CASE WHEN SongbookID = 12
                 THEN CONCAT(EntryStringNoName, ' ', EntryNumber)
            ELSE EntryStringNoName
       END AS EntryStringNoName,
       SongInstanceIDEntryNumber
FROM entries;

-- Table that connects song instances and artists
CREATE OR REPLACE VIEW wsf.songinstances_artists AS
WITH RECURSIVE
     translations AS
     (SELECT lyrics.LyricsID, lyrics_translations.TranslatedFromID
      FROM wsdb.lyrics
           LEFT JOIN wsdb.lyrics_translations
           ON lyrics.LyricsID = lyrics_translations.LyricsID
      UNION ALL
      SELECT translations.LyricsID, lyrics_translations.TranslatedFromID
      FROM translations
           JOIN wsdb.lyrics_translations
           ON translations.TranslatedFromID = lyrics_translations.LyricsID)
SELECT DISTINCT songinstances.SongInstanceID,
       songinstances.SongID,
       arrangements_artists.ArtistID,
       'arranger' AS Role,
       CONCAT(songinstances.SongInstanceID, '-arranger') AS SongInstanceIDRole
FROM wsdb.songinstances
     JOIN wsdb.arrangements_artists
     ON songinstances.ArrangementID = arrangements_artists.ArrangementID
UNION ALL
SELECT DISTINCT songinstances.SongInstanceID,
       songinstances.SongID,
       tunes_artists.ArtistID,
       'composer' AS Role,
       CONCAT(songinstances.SongInstanceID, '-composer') AS SongInstanceIDRole
FROM wsdb.songinstances
     JOIN wsdb.songinstances_tunes
     ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
     JOIN wsdb.tunes_artists
     ON songinstances_tunes.TuneID = tunes_artists.TuneID
UNION ALL
SELECT DISTINCT songinstances.SongInstanceID,
       songinstances.SongID,
       lyrics_artists.ArtistID,
       'lyricist' AS Role,
       CONCAT(songinstances.SongInstanceID, '-lyricist') AS SongInstanceIDRole
FROM wsdb.songinstances
     JOIN wsdb.songinstances_lyrics
     ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
     JOIN wsdb.lyrics_artists
     ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID
UNION ALL
SELECT DISTINCT songinstances.SongInstanceID,
       songinstances.SongID,
       lyrics_artists.ArtistID,
       'lyricist' AS Role,
       CONCAT(songinstances.SongInstanceID, '-lyricist') AS SongInstanceIDRole
FROM translations
     JOIN wsdb.songinstances_lyrics
     ON translations.LyricsID = songinstances_lyrics.LyricsID
     JOIN wsdb.lyrics_artists
     ON translations.TranslatedFromID = lyrics_artists.LyricsID
     JOIN wsdb.songinstances
     ON songinstances_lyrics.SongInstanceID = songinstances.SongInstanceID;

-- Table of artists
CREATE OR REPLACE VIEW wsf.artists AS
SELECT DISTINCT artists.ArtistID,
       artists.LastName,
       artists.FirstName,
       CONCAT(CASE WHEN artists.FirstName IS NULL THEN ''
                   ELSE CONCAT(artists.FirstName, ' ')
              END,
              artists.LastName) AS ArtistName
FROM wsdb.artists
     JOIN wsf.songinstances_artists
     ON artists.ArtistID = songinstances_artists.ArtistID;

-- Table that connects song instances and tunes
CREATE OR REPLACE VIEW wsf.songinstances_tunes AS
SELECT songinstances.SongInstanceID,
       songinstances.SongID,
       songinstances_tunes.TuneID
FROM wsdb.songinstances
     JOIN wsdb.songinstances_tunes
     ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID;

-- Table of tunes
CREATE OR REPLACE VIEW wsf.tunes AS
SELECT DISTINCT tunes.TuneID,
       tunes.TuneName,
       tunes.RealTuneName
FROM wsdb.tunes
     JOIN wsf.songinstances_tunes
     ON tunes.TuneID = songinstances_tunes.TuneID;

-- Table that connects song instances and key signatures
CREATE OR REPLACE VIEW wsf.songinstances_keysignatures AS
SELECT songinstances.SongInstanceID,
       songinstances.SongID,
       songinstances_keysignatures.KeySignatureID
FROM wsdb.songinstances
     JOIN wsdb.songinstances_keysignatures
     ON songinstances.SongInstanceID = songinstances_keysignatures.SongInstanceID;

-- Table of key signatures
CREATE OR REPLACE VIEW wsf.keysignatures AS
SELECT DISTINCT keysignatures.KeySignatureID,
       pitches.PitchName,
       accidentals.AccidentalID,
       accidentals.AccidentalSymbol,
       modes.ModeID,
       modes.ModeName,
       CONCAT(pitches.PitchName,
              CASE WHEN accidentals.AccidentalID = 3 THEN ''
                   ELSE accidentals.AccidentalSymbol
              END,
              CASE WHEN modes.ModeID = 1 THEN ''
                   WHEN modes.ModeID = 2 THEN 'm'
                   ELSE CONCAT(' ', modes.ModeName)
              END) AS KeySignatureString
FROM wsdb.keysignatures
     JOIN wsdb.pitches
     ON keysignatures.PitchID = pitches.PitchID
     JOIN wsdb.accidentals
     ON keysignatures.AccidentalID = accidentals.AccidentalID
     JOIN wsdb.modes
     ON keysignatures.ModeID = modes.ModeID
     JOIN wsf.songinstances_keysignatures
     ON keysignatures.KeySignatureID = songinstances_keysignatures.KeySignatureID;

-- Table that connects song instances and time signatures
CREATE OR REPLACE VIEW wsf.songinstances_timesignatures AS
SELECT songinstances.SongInstanceID,
        songinstances.SongID,
       songinstances_timesignatures.TimeSignatureID
FROM wsdb.songinstances
     JOIN wsdb.songinstances_timesignatures
     ON songinstances.SongInstanceID = songinstances_timesignatures.SongInstanceID;

-- Table of time signatures
CREATE OR REPLACE VIEW wsf.timesignatures AS
SELECT DISTINCT timesignatures.TimeSignatureID,
       timesignatures.TimeSignatureBeat,
       timesignatures.TimeSignatureMeasure,
       CONCAT(timesignatures.TimeSignatureBeat, '/',
              timesignatures.TimeSignatureMeasure) AS TimeSignatureString
FROM wsdb.timesignatures
     JOIN wsf.songinstances_timesignatures
     ON timesignatures.TimeSignatureID = songinstances_timesignatures.TimeSignatureID;

-- Table that connects song instances and meters
CREATE OR REPLACE VIEW wsf.songinstances_meters AS
SELECT songinstances.SongInstanceID,
       songinstances.SongID,
       lyrics_meters.MeterID
FROM wsdb.songinstances
     JOIN wsdb.songinstances_lyrics
     ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
     JOIN wsdb.lyrics_meters
     ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
UNION DISTINCT
SELECT songinstances.SongInstanceID,
       songinstances.SongID,
       tunes_meters.MeterID
FROM wsdb.songinstances
     JOIN wsdb.songinstances_tunes
     ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
     JOIN wsdb.tunes_meters
     ON songinstances_tunes.TuneID = tunes_meters.TuneID;

-- Table of meters
CREATE OR REPLACE VIEW wsf.meters AS
SELECT DISTINCT meters.MeterID,
       meters.Meter,
       meters.Multiplier,
       CONCAT(meters.Meter,
              CASE WHEN meters.Multiplier IS NULL THEN ''
                   ELSE CONCAT(' ', meters.Multiplier)
              END) AS MeterString,
       meters.SortString,
       total_songs.TotalSongs
FROM wsdb.meters
     JOIN (SELECT songinstances_meters.MeterID,
                  COUNT(DISTINCT songinstances.SongID) AS TotalSongs
           FROM wsf.songinstances_meters
                JOIN wsdb.songinstances
                ON songinstances_meters.SongInstanceID = songinstances.SongInstanceID
           GROUP BY songinstances_meters.MeterID) total_songs
     ON meters.MeterID = total_songs.MeterID;

-- Table that connects song instances and scripture references
CREATE OR REPLACE VIEW wsf.songinstances_scripturereferences AS
SELECT DISTINCT songinstances.SongInstanceID,
        songinstances.SongID,
       lyrics_scripturereferences.ScriptureReferenceID
FROM wsdb.songinstances
     JOIN wsdb.songinstances_lyrics
     ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
     JOIN wsdb.lyrics_scripturereferences
     ON songinstances_lyrics.LyricsID = lyrics_scripturereferences.LyricsID;

-- Table of books of the Bible
CREATE OR REPLACE VIEW wsf.bible_books AS
SELECT booksofthebible.BookID,
       booksofthebible.BookName,
       booksofthebible.BookAbbreviation
FROM wsdb.booksofthebible;

-- Table of scripture references
CREATE OR REPLACE VIEW wsf.scripturereferences AS
SELECT DISTINCT scripturereferences.ScriptureReferenceID,
       booksofthebible.BookID,
       booksofthebible.BookName,
       booksofthebible.BookAbbreviation,
       scripturereferences.Chapter,
       scripturereferences.Verse
FROM wsdb.scripturereferences
     JOIN wsdb.booksofthebible
     ON scripturereferences.BookID = booksofthebible.BookID
     JOIN wsf.songinstances_scripturereferences
     ON scripturereferences.ScriptureReferenceID = songinstances_scripturereferences.ScriptureReferenceID;

-- Table that connects song instances and languages
CREATE OR REPLACE VIEW wsf.songinstances_languages AS
SELECT DISTINCT songinstances.SongInstanceID,
       songinstances.SongID,
       lyrics.LanguageID
FROM wsdb.songinstances
     JOIN wsdb.songinstances_lyrics
     ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
     JOIN wsdb.lyrics
     ON songinstances_lyrics.LyricsID = lyrics.LyricsID;

-- Table of languages
CREATE OR REPLACE VIEW wsf.languages AS
SELECT DISTINCT languages.LanguageID,
       languages.LanguageName
FROM wsdb.languages
     JOIN wsf.songinstances_languages
     ON languages.LanguageID = songinstances_languages.LanguageID;

-- Table that connects song instances and arrangement types
CREATE OR REPLACE VIEW wsf.songinstances_arrangementtypes AS
SELECT songinstances.SongInstanceID,
       songinstances.SongID,
       arrangements_arrangementtypes.ArrangementTypeID
FROM wsdb.songinstances
     JOIN wsdb.arrangements_arrangementtypes
     ON songinstances.ArrangementID = arrangements_arrangementtypes.ArrangementID;

-- Table of arrangement types
CREATE OR REPLACE VIEW wsf.arrangementtypes AS
SELECT DISTINCT arrangementtypes.ArrangementTypeID,
       arrangementtypes.ArrangementType
FROM wsdb.arrangementtypes
     JOIN wsf.songinstances_arrangementtypes
     ON arrangementtypes.ArrangementTypeID = songinstances_arrangementtypes.ArrangementTypeID;

-- Table of lyrics first lines for all song instances
CREATE OR REPLACE VIEW wsf.lyrics_first_lines AS
SELECT songinstances.SongInstanceID,
       lyrics.FirstLine,
       1 AS FirstLineOrder,
       lyrics.LyricsID,
       CONCAT(lyrics.LyricsID, '-1') AS LyricsIDFirstLineOrder
FROM wsdb.songinstances
     JOIN wsdb.songinstances_lyrics
     ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
     JOIN wsdb.lyrics
     ON songinstances_lyrics.LyricsID = lyrics.LyricsID
UNION ALL
SELECT songinstances.SongInstanceID,
       lyrics.RefrainFirstLine AS FirstLine,
       2 AS FirstLineOrder,
       lyrics.LyricsID,
       CONCAT(lyrics.LyricsID, '-2') AS LyricsIDFirstLineOrder
FROM wsdb.songinstances
     JOIN wsdb.songinstances_lyrics
     ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
     JOIN wsdb.lyrics
     ON songinstances_lyrics.LyricsID = lyrics.LyricsID
WHERE lyrics.RefrainFirstLine IS NOT NULL
      AND lyrics.RefrainFirstLine <> '';

-- Table of song instances
CREATE OR REPLACE VIEW wsf.songinstances AS
WITH RECURSIVE
     lyrics_artists AS
     (SELECT lyrics_artists.LyricsID,
             GROUP_CONCAT(DISTINCT artists.ArtistName
                          ORDER BY artists.LastName, artists.FirstName
                          SEPARATOR ', ') AS Artists
      FROM wsdb.lyrics_artists
           JOIN wsf.artists
           ON lyrics_artists.ArtistID = artists.ArtistID
      GROUP BY lyrics_artists.LyricsID),
     translations AS
     (SELECT lyrics.LyricsID,
             lyrics_translations.TranslatedFromID,
             lyrics_artists.Artists AS Translators
      FROM wsdb.lyrics
           JOIN lyrics_artists
           ON lyrics.LyricsID = lyrics_artists.LyricsID
           LEFT JOIN wsdb.lyrics_translations
           ON lyrics.LyricsID = lyrics_translations.LyricsID
      WHERE lyrics_translations.TranslatedFromID IS NULL
      UNION ALL
      SELECT lyrics.LyricsID, lyrics_translations.TranslatedFromID,
             CONCAT(translations.Translators, ', ',
                    CASE WHEN lyrics.LanguageID = t.LanguageID THEN 'alt'
                         ELSE 'tr'
                    END,
                    '. ', lyrics_artists.Artists) AS Translators
      FROM wsdb.lyrics
           JOIN wsdb.lyrics_translations
           ON lyrics.LyricsID = lyrics_translations.LyricsID
           JOIN translations
           ON lyrics_translations.TranslatedFromID = translations.LyricsID
           JOIN wsdb.lyrics t
           ON lyrics_translations.TranslatedFromID = t.LyricsID
           JOIN lyrics_artists
           ON lyrics_translations.LyricsID = lyrics_artists.LyricsID),
     lyricists AS
     (SELECT songinstances.SongInstanceID,
             songinstances.SongInstance,
             GROUP_CONCAT(translations.Translators
                          SEPARATOR '; ') AS Lyricists
      FROM wsdb.songinstances
           JOIN wsdb.songinstances_lyrics
           ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
           JOIN translations
           ON songinstances_lyrics.LyricsID = translations.LyricsID
      GROUP BY songinstances.SongInstanceID,
               songinstances.SongInstance),
     lyrics_copyright AS
     (SELECT lyrics.LyricsID,
             lyrics.CopyrightYear,
             GROUP_CONCAT(DISTINCT
                          CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                    THEN copyrightholders.CopyrightHolderName
                          END
                          ORDER BY copyrightholders.CopyrightHolderName
                          SEPARATOR ', ') AS CopyrightHolderNames
      FROM wsdb.lyrics
           LEFT JOIN wsdb.lyrics_copyrightholders
           ON lyrics.LyricsID = lyrics_copyrightholders.LyricsID
           LEFT JOIN wsdb.copyrightholders
           ON lyrics_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
      GROUP BY lyrics.LyricsID,
               lyrics.CopyrightYear),
     lyrics AS
     (SELECT SongInstanceID,
             MAX(lyrics_copyright.CopyrightYear) AS LastLyricsYear,
             GROUP_CONCAT(DISTINCT CONCAT('© ',
                                          CASE WHEN lyrics_copyright.CopyrightYear IS NOT NULL
                                                    THEN CONCAT(lyrics_copyright.CopyrightYear,
                                                                ' ')
                                               ELSE ''
                                          END,
                                          lyrics_copyright.CopyrightHolderNames)
                          ORDER BY ISNULL(lyrics_copyright.CopyrightYear),
                                   lyrics_copyright.CopyrightYear
                          SEPARATOR '; ') AS LyricsCopyright
      FROM wsdb.songinstances_lyrics
           JOIN lyrics_copyright
           ON songinstances_lyrics.LyricsID = lyrics_copyright.LyricsID
      GROUP BY songinstances_lyrics.SongInstanceID),
     composers AS
     (SELECT songinstances_artists.SongInstanceID,
             GROUP_CONCAT(DISTINCT artists.ArtistName
                          ORDER BY artists.LastName, artists.FirstName
                          SEPARATOR ', ') AS Composers
      FROM wsf.songinstances_artists
           JOIN wsf.artists
           ON songinstances_artists.ArtistID = artists.ArtistID
      WHERE songinstances_artists.Role = 'composer'
      GROUP BY songinstances_artists.SongInstanceID),
     tunes_copyright AS
     (SELECT tunes.TuneID,
             tunes.TuneName,
             tunes.RealTuneName,
             tunes.CopyrightYear,
             GROUP_CONCAT(DISTINCT
                          CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                    THEN copyrightholders.CopyrightHolderName
                          END
                          ORDER BY copyrightholders.CopyrightHolderName
                          SEPARATOR ', ') AS CopyrightHolderNames
      FROM wsdb.tunes
           LEFT JOIN wsdb.tunes_copyrightholders
           ON tunes.TuneID = tunes_copyrightholders.TuneID
           LEFT JOIN wsdb.copyrightholders
           ON tunes_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
      GROUP BY tunes.TuneID,
               tunes.TuneName,
               tunes.RealTuneName,
               tunes.CopyrightYear),
     tunes AS
     (SELECT songinstances_tunes.SongInstanceID,
             GROUP_CONCAT(DISTINCT CASE WHEN (tunes_copyright.RealTuneName)
	                                     THEN tunes_copyright.TuneName
                                   END
                          ORDER BY tunes_copyright.TuneName
                          SEPARATOR ', ') AS Tunes,
             MAX(tunes_copyright.CopyrightYear) AS LastTuneYear,
             GROUP_CONCAT(DISTINCT CONCAT('© ',
                                          CASE WHEN tunes_copyright.CopyrightYear IS NOT NULL
                                                    THEN CONCAT(tunes_copyright.CopyrightYear,
                                                                ' ')
                                               ELSE ''
                                          END,
                                          tunes_copyright.CopyrightHolderNames)
                          ORDER BY ISNULL(tunes_copyright.CopyrightYear),
                                   tunes_copyright.CopyrightYear
                          SEPARATOR '; ') AS TuneCopyright
      FROM wsdb.songinstances_tunes
           JOIN tunes_copyright
           ON songinstances_tunes.TuneID = tunes_copyright.TuneID
      GROUP BY songinstances_tunes.SongInstanceID),
     arrangers AS
     (SELECT songinstances_artists.SongInstanceID,
             GROUP_CONCAT(DISTINCT artists.ArtistName
                          ORDER BY artists.LastName, artists.FirstName
                          SEPARATOR ', ') AS Arrangers
      FROM wsf.songinstances_artists
           JOIN wsf.artists
           ON songinstances_artists.ArtistID = artists.ArtistID
      WHERE songinstances_artists.Role = 'arranger'
      GROUP BY songinstances_artists.SongInstanceID),
     arrangements_copyright AS
     (SELECT arrangements.ArrangementID,
             arrangements.CopyrightYear,
             GROUP_CONCAT(DISTINCT
                          CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                    THEN copyrightholders.CopyrightHolderName
                          END
                          ORDER BY copyrightholders.CopyrightHolderName
                          SEPARATOR ', ') AS CopyrightHolderNames
      FROM wsdb.arrangements
           LEFT JOIN wsdb.arrangements_copyrightholders
           ON arrangements.ArrangementID = arrangements_copyrightholders.ArrangementID
           LEFT JOIN wsdb.copyrightholders
           ON arrangements_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
      GROUP BY arrangements.ArrangementID,
               arrangements.CopyrightYear),
     arrangements AS
     (SELECT arrangements_copyright.ArrangementID,
             GROUP_CONCAT(DISTINCT CONCAT('© ', arrangements_copyright.CopyrightYear,
                                          ' ',
                                          arrangements_copyright.CopyrightHolderNames)
                          ORDER BY arrangements_copyright.CopyrightYear
                          SEPARATOR '; ') AS ArrangementCopyright,
             GROUP_CONCAT(DISTINCT arrangementtypes.ArrangementType
                          ORDER BY arrangementtypes.ArrangementType
                          SEPARATOR ', ') AS ArrangementTypes
      FROM arrangements_copyright
           LEFT JOIN wsdb.arrangements_arrangementtypes
           ON arrangements_copyright.ArrangementID = arrangements_arrangementtypes.ArrangementID
           LEFT JOIN wsdb.arrangementtypes
           ON arrangements_arrangementtypes.ArrangementTypeID = arrangementtypes.ArrangementTypeID
      GROUP BY arrangements_copyright.ArrangementID),
     keysignatures AS
     (SELECT songinstances_keysignatures.SongInstanceID,
             GROUP_CONCAT(DISTINCT keysignatures.KeySignatureString
                          ORDER BY keysignatures.PitchName,
                                   keysignatures.AccidentalID,
                                   keysignatures.ModeID
                          SEPARATOR ', ') AS KeySignatures
      FROM wsdb.songinstances_keysignatures
           JOIN wsf.keysignatures
           ON songinstances_keysignatures.KeySignatureID = keysignatures.KeySignatureID
      GROUP BY songinstances_keysignatures.SongInstanceID),
     timesignatures AS
     (SELECT songinstances_timesignatures.SongInstanceID,
             GROUP_CONCAT(DISTINCT timesignatures.TimeSignatureString
                          ORDER BY timesignatures.TimeSignatureMeasure,
                                   timesignatures.TimeSignatureBeat
                          SEPARATOR ', ') AS TimeSignatures
      FROM wsdb.songinstances_timesignatures
           JOIN wsf.timesignatures
           ON songinstances_timesignatures.TimeSignatureID = timesignatures.TimeSignatureID
      GROUP BY songinstances_timesignatures.SongInstanceID),
     entries AS
     (SELECT songinstances_songbooks.SongInstanceID,
             GROUP_CONCAT(DISTINCT songinstances_songbooks.EntryString
                          ORDER BY songinstances_songbooks.SongbookName,
                                   songinstances_songbooks.EntryNumber
                          SEPARATOR ', ') AS SongbookEntries,
             COUNT(*) AS NumEntries
      FROM wsf.songinstances_songbooks
      GROUP BY songinstances_songbooks.SongInstanceID),
     first_lines AS
     (SELECT lyrics_first_lines.SongInstanceID,
             GROUP_CONCAT(CONCAT('<i>', lyrics_first_lines.FirstLine, '</i>')
                          ORDER BY lyrics_first_lines.FirstLineOrder,
                                   lyrics_first_lines.LyricsID
                          SEPARATOR '<br/>') AS LyricsFirstLines
      FROM wsf.lyrics_first_lines
      GROUP BY lyrics_first_lines.SongInstanceID),
     ordered_refs AS
     (SELECT songinstances_scripturereferences.SongInstanceID,
             scripturereferences.BookID,
             scripturereferences.BookAbbreviation,
             scripturereferences.Chapter,
             scripturereferences.Verse,
             CASE WHEN COALESCE(LAG(scripturereferences.Verse)
                                OVER(PARTITION BY songinstances_scripturereferences.SongInstanceID,
                                                  scripturereferences.BookID,
                                                  scripturereferences.Chapter
                                     ORDER BY scripturereferences.Verse), -1) <> scripturereferences.Verse - 1
                       THEN scripturereferences.Verse
             END AS FirstInSeq,
             CASE WHEN COALESCE(LEAD(scripturereferences.Verse)
                                OVER(PARTITION BY songinstances_scripturereferences.SongInstanceID,
                                                  scripturereferences.BookID,
                                                  scripturereferences.Chapter
                                     ORDER BY scripturereferences.Verse), -1) <> scripturereferences.Verse + 1
                       THEN scripturereferences.Verse
             END AS LastInSeq
      FROM wsf.songinstances_scripturereferences
           JOIN wsf.scripturereferences
           ON songinstances_scripturereferences.ScriptureReferenceID = scripturereferences.ScriptureReferenceID),
     filled_refs AS
     (SELECT ordered_refs.SongInstanceID,
             ordered_refs.BookID,
             ordered_refs.BookAbbreviation,
             ordered_refs.Chapter,
             ordered_refs.Verse,
             COALESCE(ordered_refs.FirstInSeq,
                      LAG(ordered_refs.FirstInSeq)
                      OVER (PARTITION BY ordered_refs.SongInstanceID,
                                         ordered_refs.BookID,
                                         ordered_refs.Chapter
                            ORDER BY ordered_refs.Verse)) AS FirstInSeq,
             COALESCE(ordered_refs.LastInSeq,
                      LEAD(ordered_refs.LastInSeq)
                      OVER (PARTITION BY ordered_refs.SongInstanceID,
                                         ordered_refs.BookID,
                                         ordered_refs.Chapter
                            ORDER BY ordered_refs.Verse)) AS LastInSeq
      FROM ordered_refs
      WHERE ordered_refs.FirstInSeq IS NOT NULL
            OR ordered_refs.LastInSeq IS NOT NULL),
     collapsed_refs AS
     (SELECT DISTINCT filled_refs.SongInstanceID,
             filled_refs.BookID,
             filled_refs.BookAbbreviation,
             filled_refs.Chapter,
             filled_refs.FirstInSeq AS FirstVerse,
             CONCAT(filled_refs.FirstInSeq,
                    CASE WHEN filled_refs.FirstInSeq = filled_refs.LastInSeq
                              THEN ''
                         ELSE CONCAT('-', filled_refs.LastInSeq)
                    END) AS Verses
      FROM filled_refs),
     refs_by_chapter AS
     (SELECT collapsed_refs.SongInstanceID,
             collapsed_refs.BookID,
             collapsed_refs.BookAbbreviation,
             collapsed_refs.Chapter,
             GROUP_CONCAT(collapsed_refs.Verses
                          ORDER BY collapsed_refs.FirstVerse
                          SEPARATOR ', ') AS Verses
      FROM collapsed_refs
      GROUP BY collapsed_refs.SongInstanceID,
               collapsed_refs.BookID,
               collapsed_refs.BookAbbreviation,
               collapsed_refs.Chapter),
     refs_by_book AS
     (SELECT refs_by_chapter.SongInstanceID,
             refs_by_chapter.BookID,
             refs_by_chapter.BookAbbreviation,
             GROUP_CONCAT(CONCAT(refs_by_chapter.Chapter, ':',
                                 refs_by_chapter.Verses)
                          ORDER BY refs_by_chapter.Chapter
                          SEPARATOR ', ') AS Verses
      FROM refs_by_chapter
      GROUP BY refs_by_chapter.SongInstanceID,
               refs_by_chapter.BookID,
               refs_by_chapter.BookAbbreviation),
     prettyscripturelists AS
     (SELECT refs_by_book.SongInstanceID,
             GROUP_CONCAT(CONCAT(refs_by_book.BookAbbreviation, ' ',
                                 refs_by_book.Verses)
                          ORDER BY refs_by_book.BookID
                          SEPARATOR '; ') AS PrettyScriptureList
      FROM refs_by_book
      GROUP BY refs_by_book.SongInstanceID)
SELECT songinstances.SongInstanceID,
       songinstances.SongInstance,
       LOWER(songinstances.SongInstance) AS SongInstanceLower,
       songinstances.ArrangementID,
       songinstances.SongID,
       lyricists.Lyricists,
       lyrics.LastLyricsYear,
       lyrics.LyricsCopyright,
       composers.Composers,
       tunes.Tunes,
       tunes.LastTuneYear,
       tunes.TuneCopyright,
       arrangers.Arrangers,
       arrangements.ArrangementCopyright,
       arrangements.ArrangementTypes,
       keysignatures.KeySignatures,
       timesignatures.TimeSignatures,
       entries.SongbookEntries,
       entries.NumEntries,
       CONCAT('<hr/> <h3>', songinstances.SongInstance, '</h3>',
              CASE WHEN entries.SongbookEntries IS NULL THEN ''
                   ELSE CONCAT('<p>', entries.SongbookEntries, '</p>')
              END,
              CASE WHEN keysignatures.KeySignatures IS NOT NULL
                        OR timesignatures.TimeSignatures IS NOT NULL
                        THEN CONCAT('<p>',
                                    COALESCE(keysignatures.KeySignatures, ''),
                                    CASE WHEN keysignatures.KeySignatures IS NOT NULL
                                              AND timesignatures.TimeSignatures IS NOT NULL
                                              THEN '; '
                                         ELSE ''
                                    END,
                                    COALESCE(timesignatures.TimeSignatures, ''),
                                    '</p>')
                   ELSE ''
              END,
              CASE WHEN tunes.Tunes IS NULL THEN ''
                        ELSE CONCAT('<p><b>Tune:</b>&nbsp;', tunes.Tunes,
                                    '</p>')
              END,
              CASE WHEN arrangements.ArrangementTypes IS NULL THEN ''
                   ELSE CONCAT('<p><b>Arrangement type:</b>&nbsp;',
                               arrangements.ArrangementTypes, '</p>')
              END,
              CASE WHEN prettyscripturelists.PrettyScriptureList IS NULL THEN ''
                   ELSE CONCAT('<p><b>Scripture references:</b>&nbsp;',
                               prettyscripturelists.PrettyScriptureList, '</p>')
              END,
              CASE WHEN lyricists.Lyricists = composers.Composers
                        THEN CONCAT('<p><b>Lyrics & Music:</b>&nbsp;',
                                    lyricists.Lyricists, '</p>')
                   ELSE CONCAT(CASE WHEN lyricists.Lyricists IS NULL THEN ''
                                    ELSE CONCAT('<p><b>Lyrics:</b>&nbsp;',
                                                lyricists.Lyricists, '</p>')
                               END,
                               CASE WHEN composers.Composers IS NULL THEN ''
                                    ELSE CONCAT('<p><b>Music:</b>&nbsp;', composers.Composers, '</p>')
                               END)
              END,
              CASE WHEN arrangers.Arrangers IS NULL THEN ''
                   ELSE CONCAT('<p><b>Arr.:</b>&nbsp;', arrangers.Arrangers,
                               '</p>')
              END,
              CASE WHEN lyrics.LyricsCopyright = tunes.TuneCopyright
                        THEN CONCAT('<p>', lyrics.LyricsCopyright, '</p>')
                   ELSE CONCAT(CASE WHEN lyrics.LyricsCopyright IS NULL THEN ''
                                    ELSE CONCAT('<p>Lyrics ',
                                                lyrics.LyricsCopyright, '</p>')
                               END,
                               CASE WHEN tunes.TuneCopyright IS NULL THEN ''
                                    ELSE CONCAT('<p>Tune ', tunes.TuneCopyright,
                                                '</p>')
                               END)
              END,
              CASE WHEN arrangements.ArrangementCopyright IS NULL THEN ''
                   ELSE CONCAT('<p>Arrangement ',
                               arrangements.ArrangementCopyright, '</p>')
              END,
              CONCAT('<p>', first_lines.LyricsFirstLines, '</p>')) AS HTML
FROM wsdb.songinstances
     LEFT JOIN lyrics
     ON songinstances.SongInstanceID = lyrics.SongInstanceID
     LEFT JOIN lyricists
     ON songinstances.SongInstanceID = lyricists.SongInstanceID
     LEFT JOIN tunes
     ON songinstances.SongInstanceID = tunes.SongInstanceID
     LEFT JOIN composers
     ON songinstances.SongInstanceID = composers.SongInstanceID
     LEFT JOIN arrangers
     ON songinstances.SongInstanceID = arrangers.SongInstanceID
     LEFT JOIN arrangements
     ON songinstances.ArrangementID = arrangements.ArrangementID
     LEFT JOIN keysignatures
     ON songinstances.SongInstanceID = keysignatures.SongInstanceID
     LEFT JOIN timesignatures
     ON songinstances.SongInstanceID = timesignatures.SongInstanceID
     LEFT JOIN entries
     ON songinstances.SongInstanceID = entries.SongInstanceID
     LEFT JOIN prettyscripturelists
     ON songinstances.SongInstanceID = prettyscripturelists.SongInstanceID
     LEFT JOIN first_lines
     ON songinstances.SongInstanceID = first_lines.SongInstanceID;

-- SONG DATA --

-- Table that connects songs and topics
CREATE OR REPLACE VIEW wsf.songs_topics AS
SELECT songs_topics.SongID,
       songs_topics.TopicID
FROM wsdb.songs_topics;

-- Table of topics
CREATE OR REPLACE VIEW wsf.topics AS
SELECT DISTINCT topics.TopicID,
       topics.TopicName
FROM wsdb.topics
     JOIN wsdb.songs_topics
     ON topics.TopicID = songs_topics.TopicID;

-- Table of songs
CREATE OR REPLACE VIEW wsf.songs AS
WITH song_names AS
     (SELECT songs.SongID,
             songs.SongName,
             CONCAT(songs.SongName,
                    CASE WHEN COUNT(*) OVER (PARTITION BY songs.SongName) > 1
                              AND songs.SongDisambiguator IS NOT NULL
                              THEN CONCAT(' (', songs.SongDisambiguator, ')')
                         ELSE ''
                    END) AS SongNameUnique,
             CASE WHEN COUNT(*) OVER (PARTITION BY songs.SongName) > 1
                       AND songs.SongDisambiguator IS NOT NULL
                       THEN songs.SongDisambiguator
                  ELSE ''
             END AS SongDisambiguator
      FROM wsdb.songs),
     songinstance_names AS
     (SELECT songs.SongID,
             GROUP_CONCAT(DISTINCT CONCAT('<i>', songinstances.SongInstance,
                                          '</i>')
                          ORDER BY songinstances.SongInstance
                          SEPARATOR '<br/>') AS OtherTitles
      FROM wsdb.songinstances
           JOIN wsdb.songs
           ON songinstances.SongID = songs.SongID
      WHERE LOWER(songinstances.SongInstance) <> SUBSTRING(LOWER(songs.SongName), 1, LENGTH(songinstances.SongInstance))
      GROUP BY songs.SongID),
     copyrighted_songinstances AS
     (SELECT songinstances.SongInstanceID, songinstances.SongID,
             CASE WHEN MAX(lyrics_copyrightholders.CopyrightHolderID) = 1
                       THEN 'N' 
                  ELSE 'Y'
             END AS AnyCopyrighted
      FROM wsdb.songinstances
           JOIN wsdb.songinstances_lyrics
           ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
           JOIN wsdb.lyrics_copyrightholders
           ON songinstances_lyrics.LyricsID = lyrics_copyrightholders.LyricsID
      GROUP BY songinstances.SongInstanceID, songinstances.SongID),
     copyrighted_songs AS
     (SELECT copyrighted_songinstances.SongID,
             CASE WHEN MIN(copyrighted_songinstances.AnyCopyrighted) = 'N'
                       THEN 'Y'
                  ELSE 'N'
             END AS AnyPublicDomain
      FROM copyrighted_songinstances
      GROUP BY copyrighted_songinstances.SongID),
     songbook_entries AS
     (SELECT songinstances_songbooks.SongID,
             GROUP_CONCAT(DISTINCT songinstances_songbooks.EntryString
                          ORDER BY songinstances_songbooks.SongbookName,
                                   songinstances_songbooks.EntryNumber
                          SEPARATOR ', ') AS SongbookEntries
      FROM wsf.songinstances_songbooks
      GROUP BY songinstances_songbooks.SongID),
     topics AS
     (SELECT songs_topics.SongID,
             GROUP_CONCAT(DISTINCT topics.TopicName
                          ORDER BY topics.TopicName
                          SEPARATOR ', ') Topics
      FROM wsdb.songs_topics
           JOIN wsdb.topics
           ON songs_topics.TopicID = topics.TopicID
      GROUP BY songs_topics.SongID)
SELECT song_names.SongID,
       song_names.SongName,
       song_names.SongDisambiguator,
       song_names.SongNameUnique,
       REGEXP_REPLACE(CONCAT(song_names.SongName, ' ',
                             song_names.SongDisambiguator),
                      '^[\'\"¡¿]', '') AS SongNameSort,
       CONCAT(song_names.SongNameUnique,
               CASE WHEN songinstance_names.OtherTitles IS NOT NULL
                         THEN CONCAT('<br/>', songinstance_names.OtherTitles)
                    ELSE ''
               END) AS PanelName,
       CASE WHEN copyrighted_songs.AnyPublicDomain = 'Y' THEN 'N'
            ELSE 'Y'
       END AS Copyrighted,
       songbook_entries.SongbookEntries,
       topics.Topics
FROM song_names
     LEFT JOIN songinstance_names
     ON song_names.SongID = songinstance_names.SongID
     LEFT JOIN copyrighted_songs
     ON song_names.SongID = copyrighted_songs.SongID
     LEFT JOIN songbook_entries
     ON song_names.SongID = songbook_entries.SongID
     LEFT JOIN topics
     ON song_names.SongID = topics.SongID;

-- PSALM SONG DATA --

-- Table that connects psalm songs and lyrics; not saved to DynamoDB, but useful
-- for other queries
CREATE OR REPLACE VIEW wsf.psalmsongs_lyrics AS
WITH public_domain AS
     (SELECT lyrics_copyrightholders.LyricsID,
             CASE WHEN MIN(lyrics_copyrightholders.CopyrightHolderID) = 1
                       THEN 'Y'
                  ELSE 'N'
             END AS PublicDomain
      FROM wsdb.lyrics_copyrightholders
      GROUP BY lyrics_copyrightholders.LyricsID),
     avg_verse AS
     (SELECT lyrics_scripturereferences.LyricsID,
             AVG(scripturereferences.Verse) AS AvgVerse
      FROM wsdb.lyrics_scripturereferences
           JOIN wsdb.scripturereferences
           ON lyrics_scripturereferences.ScriptureReferenceID = scripturereferences.ScriptureReferenceID
      GROUP BY lyrics_scripturereferences.LyricsID),
     unique_lyrics AS
     (SELECT DISTINCT CONCAT('PS', psalmsongs.PsalmSongID) AS PsalmSongID,
             lyrics.LyricsID,
             lyrics.FirstLine,
             lyrics.LanguageID,
             COALESCE(public_domain.PublicDomain, 'N') AS PublicDomain
      FROM wsdb.psalmsongs
           JOIN wsdb.songinstances
           ON psalmsongs.SongID = songinstances.SongID
           JOIN wsdb.songinstances_lyrics
           ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
           JOIN wsdb.lyrics
           ON songinstances_lyrics.LyricsID = lyrics.LyricsID
           LEFT JOIN public_domain
           ON songinstances_lyrics.LyricsID = public_domain.LyricsID)
SELECT unique_lyrics.PsalmSongID,
       unique_lyrics.LyricsID,
       unique_lyrics.FirstLine,
       unique_lyrics.LanguageID,
       unique_lyrics.PublicDomain,
       ROW_NUMBER() OVER (PARTITION BY unique_lyrics.PsalmSongID
                          ORDER BY unique_lyrics.LyricsID) AS LyricsOrder
FROM unique_lyrics
UNION ALL
SELECT DISTINCT CONCAT('MP', metricalpsalms.MetricalPsalmID) AS PsalmSongID,
       lyrics.LyricsID,
       lyrics.FirstLine,
       lyrics.LanguageID,
       COALESCE(public_domain.PublicDomain, 'N') AS PublicDomain,
       ROW_NUMBER() OVER (PARTITION BY metricalpsalms.MetricalPsalmID
                          ORDER BY avg_verse.AvgVerse) AS LyricsOrder
FROM wsdb.metricalpsalms
     JOIN wsdb.metricalpsalms_lyrics
     ON metricalpsalms.MetricalPsalmID = metricalpsalms_lyrics.MetricalPsalmID
     JOIN wsdb.lyrics
     ON metricalpsalms_lyrics.LyricsID = lyrics.LyricsID
     LEFT JOIN public_domain
     ON lyrics.LyricsID = public_domain.LyricsID
     LEFT JOIN avg_verse
     ON metricalpsalms_lyrics.LyricsID = avg_verse.LyricsID
WHERE metricalpsalms_lyrics.LyricsID NOT IN
      (SELECT songinstances_lyrics.LyricsID
       FROM wsdb.songinstances_lyrics
            JOIN wsdb.songinstances
            ON songinstances_lyrics.SongInstanceID = songinstances.SongInstanceID
            JOIN wsdb.psalmsongs
            ON songinstances.SongID = psalmsongs.SongID);

-- Table of lyrics tabs for psalm songs
CREATE OR REPLACE VIEW wsf.psalmsongs_lyrics_tabs AS
SELECT psalmsongs_lyrics.PsalmSongID,
       CONCAT('Lyrics',
              CASE WHEN languages.LanguageID = 1 THEN ''
                   ELSE CONCAT(' (', languages.LanguageName, ')')
              END) AS TabName,
       psalmsongs_lyrics.LyricsOrder,
       GROUP_CONCAT(lyrics.LyricsHTML
                    ORDER BY LyricsOrder
                    SEPARATOR ' ') AS LyricsHTML
FROM wsf.psalmsongs_lyrics
     JOIN wsdb.languages
     ON psalmsongs_lyrics.LanguageID = languages.LanguageID
     JOIN wsdb.lyrics
     ON psalmsongs_lyrics.LyricsID = lyrics.LyricsID
WHERE psalmsongs_lyrics.PublicDomain = 'Y'
GROUP BY psalmsongs_lyrics.PsalmSongID, 2;

-- Table of psalm song types
CREATE OR REPLACE VIEW wsf.psalmsongtypes AS
SELECT psalmsongtypes.PsalmSongTypeID,
       psalmsongtypes.PsalmSongType
FROM wsdb.psalmsongtypes;

-- Table that connects psalm songs and alternative tunes
CREATE OR REPLACE VIEW wsf.psalmsongs_alternativetunes AS
WITH one_canonical_song AS
     (SELECT tunes_canonicalsongs.TuneID,
             MAX(tunes_canonicalsongs.SongID) AS SongID
      FROM wsdb.tunes_canonicalsongs
      GROUP BY tunes_canonicalsongs.TuneID
      HAVING COUNT(*) = 1),
     not_psalmsongs AS
     (SELECT psalmsongs_lyrics.PsalmSongID,
             psalmsongs_lyrics.FirstLine
      FROM wsf.psalmsongs_lyrics
      WHERE psalmsongs_lyrics.LyricsOrder = 1),
     psalmsongs AS
     (SELECT CONCAT('PS', psalmsongs.PsalmSongID) AS PsalmSongID,
             CONCAT('ps', psalmsongs.SongID) AS SongOrMetricalPsalmID,
             songs.SongName AS PsalmSongTitle,
             songs.SongID
      FROM wsdb.psalmsongs
           JOIN wsdb.songs
           ON psalmsongs.SongID = songs.SongID
      UNION ALL
      SELECT CONCAT('MP', metricalpsalms.MetricalPsalmID) AS PsalmSongID,
             CONCAT('mp', metricalpsalms.MetricalPsalmID) AS SongOrMetricalPsalmID,
             not_psalmsongs.FirstLine AS PsalmSongTitle,
             NULL AS SongID
      FROM wsdb.metricalpsalms
           JOIN not_psalmsongs
           ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = not_psalmsongs.PsalmSongID)
SELECT alternativetunes.AlternativeTuneID,
       psalmsongs.PsalmSongID,
       tunes.TuneID,
       CONCAT(CASE WHEN tunes.RealTuneName = 1
                        THEN CONCAT(tunes.TuneName, ' (')
                   ELSE ''
              END,
              COALESCE(tunes.CanonicalSongName,
                       canonical_song.SongName),
              CASE WHEN tunes.RealTuneName = 1
                        THEN ')'
                   ELSE ''
              END) AS TuneDisplayName,
       REPLACE(REPLACE(alternativetunes.Notes, '#T#',
                       CONCAT('"',
                              COALESCE(tunes.CanonicalSongName,
                                       canonical_song.SongName),
                              '"')),
               '#L#', CONCAT('"', psalmsongs.PsalmSongTitle, '"')) AS Notes
FROM psalmsongs
     JOIN wsdb.alternativetunes
     ON psalmsongs.SongOrMetricalPsalmID = alternativetunes.SongOrMetricalPsalmID
     JOIN wsdb.tunes
     ON alternativetunes.TuneID = tunes.TuneID
     JOIN wsdb.tunes_copyrightholders
     ON tunes.TuneID = tunes_copyrightholders.TuneID
        AND tunes_copyrightholders.CopyrightHolderID = 1
     LEFT JOIN wsf.songs
     ON psalmsongs.SongID = songs.SongID
        AND songs.Copyrighted = 'Y'
     LEFT JOIN one_canonical_song
     ON tunes.TuneID = one_canonical_song.TuneID
     LEFT JOIN wsdb.songs canonical_song
     ON one_canonical_song.SongID = canonical_song.SongID
WHERE songs.SongID IS NULL;

-- Table of psalm songs
CREATE OR REPLACE VIEW wsf.psalmsongs AS
WITH RECURSIVE
     not_psalmsongs AS
     (SELECT psalmsongs_lyrics.PsalmSongID,
             psalmsongs_lyrics.FirstLine
      FROM wsf.psalmsongs_lyrics
      WHERE psalmsongs_lyrics.LyricsOrder = 1),
     firstlines AS
     (SELECT psalmsongs_lyrics.PsalmSongID,
             GROUP_CONCAT(DISTINCT psalmsongs_lyrics.FirstLine
                          ORDER BY psalmsongs_lyrics.LanguageID,
                                   psalmsongs_lyrics.LyricsOrder
                          SEPARATOR '<br/>') AS FirstLines
      FROM wsf.psalmsongs_lyrics
      GROUP BY psalmsongs_lyrics.PsalmSongID),
     laterfirstlines AS
     (SELECT psalmsongs_lyrics.PsalmSongID,
             GROUP_CONCAT(CONCAT('<i>', psalmsongs_lyrics.FirstLine, '</i>')
                          ORDER BY psalmsongs_lyrics.LyricsOrder
                          SEPARATOR '<br/>') AS LaterFirstLines
      FROM wsf.psalmsongs_lyrics
      WHERE psalmsongs_lyrics.LyricsOrder > 1
      GROUP BY psalmsongs_lyrics.PsalmSongID),
     songinstances AS
     (SELECT songinstances.SongID,
             CASE WHEN songinstances.Lyricists = songinstances.Composers
                       THEN CONCAT('Lyrics & Music: ', songinstances.Lyricists)
                  ELSE CONCAT(CASE WHEN songinstances.Lyricists IS NULL THEN ''
                                   ELSE CONCAT('Lyrics: ',
                                               songinstances.Lyricists)
                              END,
                              CASE WHEN songinstances.Lyricists IS NULL
                                        OR songinstances.Composers IS NULL
                                        THEN ''
                                   ELSE '; '
                              END,
                              CASE WHEN songinstances.Composers IS NULL THEN ''
                                   ELSE CONCAT('Music: ',
                                               songinstances.Composers)
                              END)
             END AS Artists,
             ROW_NUMBER() OVER (PARTITION BY songinstances.SongID
                                ORDER BY GREATEST(COALESCE(songinstances.LastLyricsYear,
                                                           0),
                                                  COALESCE(songinstances.LastTuneYear,
                                                           0))) AS RowNum
      FROM wsf.songinstances),
     lyrics_artists AS
     (SELECT lyrics_artists.LyricsID,
             GROUP_CONCAT(DISTINCT artists.ArtistName
                          ORDER BY artists.LastName, artists.FirstName
                          SEPARATOR ', ') AS Artists
      FROM wsdb.lyrics_artists
           JOIN wsf.artists
           ON lyrics_artists.ArtistID = artists.ArtistID
      GROUP BY lyrics_artists.LyricsID),
     translations AS
     (SELECT lyrics.LyricsID,
             lyrics_translations.TranslatedFromID,
             lyrics_artists.Artists AS Translators
      FROM wsdb.lyrics
           JOIN lyrics_artists
           ON lyrics.LyricsID = lyrics_artists.LyricsID
           LEFT JOIN wsdb.lyrics_translations
           ON lyrics.LyricsID = lyrics_translations.LyricsID
      WHERE lyrics_translations.TranslatedFromID IS NULL
      UNION ALL
      SELECT lyrics.LyricsID, lyrics_translations.TranslatedFromID,
             CONCAT(translations.Translators, ', ',
                    CASE WHEN lyrics.LanguageID = t.LanguageID THEN 'alt'
                         ELSE 'tr'
                    END,
                    '. ', lyrics_artists.Artists) AS Translators
      FROM wsdb.lyrics
           JOIN wsdb.lyrics_translations
           ON lyrics.LyricsID = lyrics_translations.LyricsID
           JOIN translations
           ON lyrics_translations.TranslatedFromID = translations.LyricsID
           JOIN wsdb.lyrics t
           ON lyrics_translations.TranslatedFromID = t.LyricsID
           JOIN lyrics_artists
           ON lyrics_translations.LyricsID = lyrics_artists.LyricsID),
     metricalpsalms_artists AS
     (SELECT metricalpsalms_lyrics.MetricalPsalmID,
             CONCAT('Lyrics: ',
                    GROUP_CONCAT(translations.Translators
                                 SEPARATOR '; ')) AS Lyricists
      FROM wsdb.metricalpsalms_lyrics
           JOIN translations
           ON metricalpsalms_lyrics.LyricsID = translations.LyricsID
      GROUP BY metricalpsalms_lyrics.MetricalPsalmID),
     all_refs AS
     (SELECT songinstances_scripturereferences.SongID,
             NULL AS MetricalPsalmID,
             scripturereferences.Chapter,
             scripturereferences.Verse
      FROM wsf.songinstances_scripturereferences
           JOIN wsf.scripturereferences
           ON songinstances_scripturereferences.ScriptureReferenceID = scripturereferences.ScriptureReferenceID
      WHERE scripturereferences.BookID = 19
      UNION ALL
      SELECT NULL AS SongID,
             metricalpsalms_lyrics.MetricalPsalmID,
             scripturereferences.Chapter,
             scripturereferences.Verse
      FROM wsdb.metricalpsalms_lyrics
           JOIN wsdb.lyrics_scripturereferences
           ON metricalpsalms_lyrics.LyricsID = lyrics_scripturereferences.LyricsID
           JOIN wsf.scripturereferences
           ON lyrics_scripturereferences.ScriptureReferenceID = scripturereferences.ScriptureReferenceID
      WHERE scripturereferences.BookID = 19),
     ordered_refs AS
     (SELECT all_refs.SongID,
             all_refs.MetricalPsalmID,
             all_refs.Chapter,
             all_refs.Verse,
             CASE WHEN COALESCE(LAG(all_refs.Verse)
                                OVER(PARTITION BY all_refs.SongID,
                                                  all_refs.MetricalPsalmID,
                                                  all_refs.Chapter
                                     ORDER BY all_refs.Verse), -1) <> all_refs.Verse - 1
                       THEN all_refs.Verse
             END AS FirstInSeq,
             CASE WHEN COALESCE(LEAD(all_refs.Verse)
                                OVER(PARTITION BY all_refs.SongID,
                                                  all_refs.MetricalPsalmID,
                                                  all_refs.Chapter
                                     ORDER BY all_refs.Verse), -1) <> all_refs.Verse + 1
                       THEN all_refs.Verse
             END AS LastInSeq
      FROM all_refs),
     filled_refs AS
     (SELECT ordered_refs.SongID,
             ordered_refs.MetricalPsalmID,
             ordered_refs.Chapter,
             ordered_refs.Verse,
             COALESCE(ordered_refs.FirstInSeq,
                      LAG(ordered_refs.FirstInSeq)
                      OVER (PARTITION BY ordered_refs.SongID,
                                         ordered_refs.MetricalPsalmID,
                                         ordered_refs.Chapter
                            ORDER BY ordered_refs.Verse)) AS FirstInSeq,
             COALESCE(ordered_refs.LastInSeq,
                      LEAD(ordered_refs.LastInSeq)
                      OVER (PARTITION BY ordered_refs.SongID,
                                         ordered_refs.MetricalPsalmID,
                                         ordered_refs.Chapter
                            ORDER BY ordered_refs.Verse)) AS LastInSeq
      FROM ordered_refs
      WHERE ordered_refs.FirstInSeq IS NOT NULL
            OR ordered_refs.LastInSeq IS NOT NULL),
     collapsed_refs AS
     (SELECT DISTINCT filled_refs.SongID,
             filled_refs.MetricalPsalmID,
             filled_refs.Chapter,
             filled_refs.FirstInSeq AS FirstVerse,
             CONCAT(filled_refs.FirstInSeq,
                    CASE WHEN filled_refs.FirstInSeq = filled_refs.LastInSeq
                              THEN ''
                         ELSE CONCAT('-', filled_refs.LastInSeq)
                    END) AS Verses
      FROM filled_refs),
     prettyscripturelists AS
     (SELECT collapsed_refs.SongID,
             collapsed_refs.MetricalPsalmID,
             collapsed_refs.Chapter,
             CONVERT(GROUP_CONCAT(collapsed_refs.Verses
                                  ORDER BY collapsed_refs.FirstVerse
                                  SEPARATOR ', ') USING utf8mb4) AS PrettyScriptureList
      FROM collapsed_refs
      GROUP BY collapsed_refs.SongID,
               collapsed_refs.MetricalPsalmID,
               collapsed_refs.Chapter),
     ps AS
     (SELECT CONCAT('PS', psalmsongs.PsalmSongID) AS PsalmSongID,
             psalmsongs.PsalmNumber,
             psalmsongs.SongID,
             NULL AS MetricalPsalmID,
             CONCAT('ps', psalmsongs.SongID) AS SongOrMetricalPsalmID,
             psalmsongs.PsalmSongTypeID,
             psalmsongtypes.PsalmSongType,
             songs.SongName AS PsalmSongTitle,
             songs.PanelName,
             prettyscripturelists.PrettyScriptureList,
             songinstances.Artists,
             CONVERT(CONCAT('<br/><h3>', psalmsongtypes.PsalmSongType, '</h3>',
                            CASE WHEN songs.SongbookEntries IS NOT NULL
                                      THEN CONCAT('<p>', songs.SongbookEntries, '</p>')
                                 ELSE ''
                            END,
                            '<p><b>Verses:</b> ',
                            prettyscripturelists.PrettyScriptureList, '</p>',
                            CASE WHEN songinstances.Artists IS NOT NULL
                                      THEN CONCAT('<p>', songinstances.Artists, '</p>')
                                 ELSE ''
                            END,
                            CASE WHEN firstlines.FirstLines IS NOT NULL
                                      THEN CONCAT('<p>', firstlines.FirstLines, '</p>')
                                 ELSE ''
                            END) USING utf8mb4) AS HTMLInfo
      FROM wsdb.psalmsongs
           JOIN wsdb.psalmsongtypes
           ON psalmsongs.PsalmSongTypeID = psalmsongtypes.PsalmSongTypeID
           JOIN wsf.songs
           ON psalmsongs.SongID = songs.SongID
           LEFT JOIN prettyscripturelists
           ON psalmsongs.SongID = prettyscripturelists.SongID
              AND psalmsongs.PsalmNumber = prettyscripturelists.Chapter
           LEFT JOIN songinstances
           ON psalmsongs.SongID = songinstances.SongID
              AND songinstances.RowNum = 1
           LEFT JOIN firstlines
           ON CONCAT('PS', psalmsongs.PsalmSongID) = firstlines.PsalmSongID
      UNION ALL
      SELECT CONCAT('MP', metricalpsalms.MetricalPsalmID) AS PsalmSongID,
             metricalpsalms.PsalmNumber,
             NULL AS SongID,
             metricalpsalms.MetricalPsalmID,
             CONCAT('mp', metricalpsalms.MetricalPsalmID) AS SongOrMetricalPsalmID,
             NULL AS PsalmSongTypeID,
             'Paraphrase' AS PsalmSongType,
             not_psalmsongs.FirstLine AS PsalmSongTitle,
             CONCAT(not_psalmsongs.FirstLine,
                    CASE WHEN laterfirstlines.LaterFirstLines IS NULL
                              THEN ''
                         ELSE CONCAT('<br/>', laterfirstlines.LaterFirstLines)
                    END) AS PanelName,
             prettyscripturelists.PrettyScriptureList,
             metricalpsalms_artists.Lyricists AS Artists,
             CONVERT(CONCAT('<br/><h3>Paraphrase</h3>', '<p><b>Verses:</b> ',
                            prettyscripturelists.PrettyScriptureList, '</p>',
                            CASE WHEN metricalpsalms_artists.Lyricists IS NOT NULL
                                      THEN CONCAT('<p>',
                                                  metricalpsalms_artists.Lyricists,
                                                  '</p>')
                            END,
                            '<p>', firstlines.FirstLines, '</p>') USING utf8mb4) AS HTMLInfo
      FROM wsdb.metricalpsalms
           JOIN not_psalmsongs
           ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = not_psalmsongs.PsalmSongID
           LEFT JOIN laterfirstlines
           ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = laterfirstlines.PsalmSongID
           LEFT JOIN prettyscripturelists
           ON metricalpsalms.MetricalPsalmID = prettyscripturelists.MetricalPsalmID
              AND metricalpsalms.PsalmNumber = prettyscripturelists.Chapter
           LEFT JOIN metricalpsalms_artists
           ON metricalpsalms.MetricalPsalmID = metricalpsalms_artists.MetricalPsalmID
           LEFT JOIN firstlines
           ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = firstlines.PsalmSongID),
     tuneentries AS
     (SELECT songinstances_tunes.TuneID,
             GROUP_CONCAT(DISTINCT songinstances_songbooks.EntryString
                          ORDER BY songinstances_songbooks.SongbookName,
                                   songinstances_songbooks.EntryNumber
                          SEPARATOR ', ') AS TuneEntries
      FROM wsdb.songinstances_tunes
           JOIN wsf.songinstances_songbooks
           ON songinstances_tunes.SongInstanceID = songinstances_songbooks.SongInstanceID
      GROUP BY songinstances_tunes.TuneID),
     alternativetunes AS
     (SELECT psalmsongs_alternativetunes.PsalmSongID,
             GROUP_CONCAT(DISTINCT
                          CONCAT('<div>', '<b>',
                                 psalmsongs_alternativetunes.TuneDisplayName,
                                 '</b>',
                                 CASE WHEN tuneentries.TuneEntries IS NOT NULL
                                           THEN CONCAT('<br/><i>',
                                                       tuneentries.TuneEntries,
                                                       '</i>')
                                      ELSE ''
                                 END,
                                 CASE WHEN psalmsongs_alternativetunes.Notes IS NOT NULL
                                           THEN CONCAT('<br/><p>',
                                                       psalmsongs_alternativetunes.Notes,
                                                       '</p>')
                                      ELSE ''
                                 END,
                                 '</div>')
                          ORDER BY psalmsongs_alternativetunes.TuneDisplayName
                          SEPARATOR '') AS AlternativeTunes
      FROM wsf.psalmsongs_alternativetunes
           LEFT JOIN tuneentries
           ON psalmsongs_alternativetunes.TuneID = tuneentries.TuneID
      GROUP BY psalmsongs_alternativetunes.PsalmSongID)
SELECT ps.PsalmSongID,
       ps.PsalmNumber,
       ps.SongID,
       ps.MetricalPsalmID,
       ps.SongOrMetricalPsalmID,
       ps.PsalmSongTypeID,
       ps.PsalmSongType,
       ps.PsalmSongTitle,
       ps.PanelName,
       ps.PrettyScriptureList,
       ps.Artists,
       ps.HTMLInfo,
       CONCAT('<p></p>', alternativetunes.AlternativeTunes) AS HTMLAlternatives
FROM ps
     LEFT JOIN alternativetunes
     ON ps.PsalmSongID = alternativetunes.PsalmSongID;
