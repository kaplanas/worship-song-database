summary.table.info = list(
  sql = "WITH song_titles AS
              (SELECT SongID,
                      CONCAT(SongName,
                             CASE WHEN SongDisambiguator IS NULL THEN ''
                                  ELSE CONCAT(' (',
                                              CONCAT(SongDisambiguator,
                                                     ')'))
                             END) AS Song
               FROM wsdb.songs),
              entry_numbers AS
              (SELECT songinstances.SongID, songbooks.SongbookID,
                      GROUP_CONCAT(songbookentries.EntryNumber
                                   ORDER BY songbookentries.EntryNumber
                                   SEPARATOR ', ') AS EntryNumbers
               FROM wsdb.songbookentries
                    JOIN wsdb.songinstances
                    ON songbookentries.SongInstanceID = songinstances.SongInstanceID
                    JOIN wsdb.songbooks
                    ON songbookentries.SongbookID = songbooks.SongbookID
               GROUP BY songinstances.SongID, songbooks.SongbookID),
              return_counts AS
              (SELECT SongID,
                      COUNT(DISTINCT hymnologist_returns.HymnologistID) AS Hymnologists
               FROM lhp.hymnologist_returns
               WHERE Processed
               GROUP BY SongID),
              lyrics_year AS
              (SELECT songinstances.SongID,
                      MIN(lyrics.CopyrightYear) AS Year
               FROM wsdb.songinstances
                    JOIN wsdb.songinstances_lyrics
                    ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                    JOIN wsdb.lyrics
                    ON songinstances_lyrics.LyricsID = lyrics.LyricsID
               WHERE lyrics.CopyrightYear <= 1994
               GROUP BY songinstances.SongID),
              tune_year AS
              (SELECT songinstances.SongID,
                      MIN(tunes.CopyrightYear) AS Year
               FROM wsdb.songinstances
                    JOIN wsdb.songinstances_tunes
                    ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
                    JOIN wsdb.tunes
                    ON songinstances_tunes.TuneID = tunes.TuneID
               WHERE tunes.CopyrightYear <= 1994
               GROUP BY songinstances.SongID)
         SELECT song_titles.Song, return_counts.Hymnologists,
                (SELECT COUNT(DISTINCT HymnologistID)
                 FROM lhp.hymnologist_returns
                 WHERE Processed) AS TotalHymnologists,
                sfp.EntryNumbers AS \"Songs of Faith and Praise\",
                pl.EntryNumbers AS \"Praise for the Lord\",
                phss.EntryNumbers AS \"Psalms, Hymns, and Spiritual Songs\",
                hw.EntryNumbers AS \"Hymns for Worship\",
                tph.EntryNumbers AS \"The Paperless Hymnal\",
                CASE WHEN christmas.SongID IS NULL THEN 'N'
                     ELSE 'Y'
                END AS Christmas,
                CASE WHEN nation.SongID IS NULL THEN 'N'
                     ELSE 'Y'
                END AS Nation,
                GREATEST(IFNULL(lyrics_year.Year, 0),
                         IFNULL(tune_year.Year, 0)) AS Year
         FROM return_counts
              JOIN song_titles
              ON return_counts.SongID = song_titles.SongID
              LEFT JOIN entry_numbers sfp
              ON return_counts.SongID = sfp.SongID
                 AND sfp.SongbookID = 1
              LEFT JOIN entry_numbers pl
              ON return_counts.SongID = pl.SongID
                 AND pl.SongbookID = 6
              LEFT JOIN entry_numbers phss
              ON return_counts.SongID = phss.SongID
                 AND phss.SongbookID = 21
              LEFT JOIN entry_numbers hw
              ON return_counts.SongID = hw.SongID
                 AND hw.SongbookID = 22
              LEFT JOIN entry_numbers tph
              ON return_counts.SongID = tph.SongID
                 AND tph.SongbookID = 12
              LEFT JOIN wsdb.songs_topics christmas
              ON return_counts.SongID = christmas.SongID
                 AND christmas.TopicID = 1
              LEFT JOIN wsdb.songs_topics nation
              ON return_counts.SongID = nation.SongID
                 AND nation.TopicID = 2
              LEFT JOIN lyrics_year
              ON return_counts.SongID = lyrics_year.SongID
              LEFT JOIN tune_year
              ON return_counts.SongID = tune_year.SongID
         ORDER BY return_counts.Hymnologists DESC, song_titles.Song"
)

summary.table = tabPanel(
  "Table of songs",
  DTOutput("lhp.summary")
)

summary.histogram = tabPanel(
  "Distribution of votes",
  plotOutput("lhp.histogram")
)

summary.violinplot = tabPanel(
  "Year of composition",
  plotOutput("lhp.violinplot")
)

summary.page = tabPanel("Summary",
                        navlistPanel(
                          summary.table,
                          summary.histogram,
                          summary.violinplot,
                          well = F,
                          widths = c(2, 10)
                        ))